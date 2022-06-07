-module(lim_body).

-export([get_body/3]).

-type t() :: {amount, amount()} | {cash, cash()}.

-type amount() :: integer().
-type cash() :: #{
    amount := amount(),
    currency := currency()
}.

-type currency() :: lim_base_thrift:'CurrencySymbolicCode'().
-type config() :: lim_config_machine:config().
-type body_type() :: full | partial.
-type get_body_error() :: notfound | lim_rates:conversion_error().

-export_type([t/0]).
-export_type([amount/0]).
-export_type([currency/0]).
-export_type([cash/0]).
-export_type([get_body_error/0]).

-import(lim_pipeline, [do/1, unwrap/1]).

-spec get_body(body_type(), config(), lim_context:t()) -> {ok, t()} | {error, get_body_error()}.
get_body(BodyType, Config, LimitContext) ->
    do(fun() ->
        Body = unwrap(do_get_body(BodyType, Config, LimitContext)),
        apply_op_behaviour(Body, Config, LimitContext)
    end).

do_get_body(BodyType, Config = #{body_type := {cash, ConfigCurrency}}, LimitContext) ->
    do(fun() ->
        OpBody = unwrap(get_body_for_operation(BodyType, Config, LimitContext)),
        case OpBody of
            {cash, #{currency := ConfigCurrency}} = Result ->
                Result;
            {cash, #{currency := _} = Cash} ->
                ConvertedCash = unwrap(lim_rates:convert(Cash, Config, LimitContext)),
                {cash, ConvertedCash};
            Error ->
                Error
        end
    end);
do_get_body(BodyType, Config = #{body_type := amount}, LimitContext) ->
    do(fun() ->
        OpBody = unwrap(get_body_for_operation(BodyType, Config, LimitContext)),
        case OpBody of
            {cash, #{amount := Amount}} when Amount > 0 ->
                % NOTE
                % Increment limit by one on every positive amount operation.
                {amount, 1};
            {cash, #{amount := 0}} ->
                % NOTE
                % Looks crutchy: zero amount operation means "rollback" in the protocol.
                {amount, 0}
        end
    end).

-spec get_body_for_operation(body_type(), config(), lim_context:t()) ->
    {ok, t()} | {error, notfound}.
get_body_for_operation(BodyType, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    {ok, Operation} = lim_context:get_operation(ContextType, LimitContext),
    get_body_for_operation(BodyType, Operation, Config, LimitContext).

-spec get_body_for_operation(body_type(), lim_context:context_operation(), config(), lim_context:t()) ->
    {ok, t()} | {error, notfound}.
get_body_for_operation(full, invoice = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, Operation, LimitContext);
get_body_for_operation(full, invoice_adjustment, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, invoice, LimitContext);
get_body_for_operation(full, invoice_payment = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, Operation, LimitContext);
get_body_for_operation(full, invoice_payment_adjustment, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, invoice_payment, LimitContext);
get_body_for_operation(full, invoice_payment_refund, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, invoice_payment, LimitContext);
get_body_for_operation(full, invoice_payment_chargeback = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, body, Operation, LimitContext);
get_body_for_operation(partial, invoice, _Config, _LimitContext) ->
    {error, notfound};
get_body_for_operation(partial, invoice_adjustment, _Config, _LimitContext) ->
    {error, notfound};
get_body_for_operation(partial, invoice_payment = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, capture_cost, Operation, LimitContext);
get_body_for_operation(partial, invoice_payment_adjustment, _Config, _LimitContext) ->
    {error, notfound};
get_body_for_operation(partial, invoice_payment_refund = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, Operation, LimitContext);
get_body_for_operation(partial, invoice_payment_chargeback, _Config, _LimitContext) ->
    {error, notfound}.

apply_op_behaviour(Body, #{op_behaviour := ComputationConfig}, LimitContext) ->
    {ok, Operation} = lim_context:get_operation(payment_processing, LimitContext),
    case maps:get(Operation, ComputationConfig, undefined) of
        addition ->
            Body;
        subtraction ->
            invert_body(Body);
        undefined ->
            Body
    end;
apply_op_behaviour(Body, _Config, _LimitContext) ->
    Body.

invert_body({cash, Cash = #{amount := Amount}}) ->
    {cash, Cash#{amount := -Amount}};
invert_body({amount, Amount}) ->
    {amount, -Amount}.
