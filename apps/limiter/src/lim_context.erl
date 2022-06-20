-module(lim_context).

-include_lib("limiter_proto/include/lim_limiter_context_thrift.hrl").

-export([create/1]).
-export([woody_context/1]).
-export([get_operation/2]).
-export([get_from_context/3]).
-export([get_from_context/4]).

-export([set_context/2]).
-export([set_clock/2]).

-export([clock/1]).

-type woody_context() :: woody_context:ctx().
-type timestamp() :: binary().
-type thrift_context() :: lim_limiter_thrift:'LimitContext'().
-type clock() :: lim_limiter_thrift:'Clock'().
-type id() :: binary().
-type token() :: binary().
-type exp_date() :: binary().
-type cash() :: lim_body:cash().

-type t() :: #{
    woody_context => woody_context(),
    context => context(),
    clock => clock()
}.

-type context_type() :: payment_processing.
-type context_operation() :: payment_processing_operation().

-type context() :: #{
    payment_processing => payment_processing_context()
}.

-type payment_processing_context() :: #{
    op := payment_processing_operation(),
    invoice => payment_processing_invoice()
}.

-type payment_processing_operation() ::
    invoice
    | invoice_adjustment
    | invoice_payment
    | invoice_payment_adjustment
    | invoice_payment_refund
    | invoice_payment_chargeback.

-type payment_processing_invoice() :: #{
    id => id(),
    owner_id => id(),
    shop_id => id(),
    cost => cash(),
    created_at => timestamp(),
    adjustment => payment_processing_adjustment(),
    payment => payment_processing_payment()
}.

-type payment_processing_adjustment() :: #{
    id => id()
}.

-type payment_processing_payment() :: #{
    id => id(),
    owner_id => id(),
    shop_id => id(),
    cost => cash(),
    capture_cost => cash(),
    created_at => timestamp(),
    flow => instant | hold,
    payer => payment_processing_payer(),
    adjustment => payment_processing_payment_adjustment(),
    refund => payment_processing_payment_refund(),
    chargeback => payment_processing_payment_chargeback()
}.

-type payment_processing_payer() ::
    {payment_resource, payment_processing_payer_data()}
    | {customer, payment_processing_payer_data()}
    | {recurrent, payment_processing_payer_data()}.

-type payment_processing_payer_data() :: #{
    payment_tool => payment_processing_payment_tool()
}.

-type payment_processing_payment_tool() :: {bank_card, payment_processing_bank_card()}.

-type payment_processing_bank_card() :: #{
    token => token(),
    exp_date => exp_date()
}.

-type payment_processing_payment_adjustment() :: #{
    id => id(),
    created_at => timestamp()
}.

-type payment_processing_payment_refund() :: #{
    id => id(),
    cost => cash(),
    created_at => timestamp()
}.

-type payment_processing_payment_chargeback() :: #{
    id => id(),
    levy => cash(),
    body => cash(),
    created_at => timestamp()
}.

-export_type([t/0]).
-export_type([context/0]).
-export_type([context_type/0]).
-export_type([context_operation/0]).

-spec create(woody_context()) -> t().
create(WoodyContext) ->
    #{woody_context => WoodyContext}.

-spec woody_context(t()) -> woody_context().
woody_context(Context) ->
    maps:get(woody_context, Context).

-spec clock(t()) -> {ok, clock()} | {error, notfound}.
clock(#{clock := Clock}) ->
    {ok, Clock};
clock(_) ->
    {error, notfound}.

-spec set_context(thrift_context(), t()) -> t().
set_context(Context, LimContext) ->
    LimContext#{context => unmarshal_context(Context)}.

-spec set_clock(clock(), t()) -> t().
set_clock(Clock, LimContext) ->
    LimContext#{clock => Clock}.

-spec get_operation(context_type(), t()) -> {ok, atom()} | {error, notfound}.
get_operation(Type, #{context := Context}) ->
    case maps:get(Type, Context, undefined) of
        undefined ->
            {error, notfound};
        #{op := Operation} ->
            {ok, Operation}
    end.

-spec get_from_context(context_type(), atom(), t()) -> {ok, term()} | {error, notfound}.
get_from_context(payment_processing, ValueName, LimContext = #{context := Context}) ->
    case maps:get(payment_processing, Context, undefined) of
        undefined ->
            {error, notfound};
        #{op := Operation} ->
            get_from_context(payment_processing, ValueName, Operation, LimContext)
    end;
get_from_context(_, _ValueName, _LimContext) ->
    {error, notfound}.

-spec get_from_context(context_type(), atom(), context_operation(), t()) -> {ok, term()} | {error, notfound}.
get_from_context(payment_processing, ValueName, Op, #{context := #{payment_processing := Context}}) ->
    case get_payment_processing_operation_context(Op, Context) of
        {ok, OperationContext} ->
            case maps:get(ValueName, OperationContext, undefined) of
                undefined ->
                    {error, notfound};
                Value ->
                    {ok, Value}
            end;
        Error ->
            Error
    end;
get_from_context(_, _ValueName, _Op, _LimContext) ->
    {error, notfound}.

get_payment_processing_operation_context(invoice, #{invoice := Invoice}) ->
    {ok, Invoice};
get_payment_processing_operation_context(invoice_adjustment, #{invoice := #{adjustment := Adjustment}}) ->
    {ok, Adjustment};
get_payment_processing_operation_context(invoice_payment, #{invoice := #{payment := Payment}}) ->
    {ok, Payment};
get_payment_processing_operation_context(
    invoice_payment_adjustment,
    #{invoice := #{payment := #{adjustment := Adjustment}}}
) ->
    {ok, Adjustment};
get_payment_processing_operation_context(
    invoice_payment_refund,
    #{invoice := #{payment := #{refund := Refund}}}
) ->
    {ok, Refund};
get_payment_processing_operation_context(
    invoice_payment_chargeback,
    #{invoice := #{payment := #{chargeback := Chargeback}}}
) ->
    {ok, Chargeback};
get_payment_processing_operation_context(_, _) ->
    {error, notfound}.

%%

unmarshal_context(#limiter_context_LimitContext{limiter_payment_processing = PaymentProcessing}) when
    PaymentProcessing =/= undefined
->
    #{payment_processing => lim_limiter_context:unmarshal(PaymentProcessing)};
unmarshal_context(#limiter_context_LimitContext{payment_processing = PaymentProcessing}) when
    PaymentProcessing =/= undefined
->
    #{payment_processing => lim_payproc_context:unmarshal(PaymentProcessing)};
unmarshal_context(_) ->
    #{}.
