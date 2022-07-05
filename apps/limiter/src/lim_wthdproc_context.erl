-module(lim_wthdproc_context).

-include_lib("limiter_proto/include/limproto_context_withdrawal_thrift.hrl").
-include_lib("damsel/include/dmsl_wthd_domain_thrift.hrl").

-behaviour(lim_context).
-export([get_operation/1]).
-export([get_value/2]).

-type context() :: limproto_context_withdrawal_thrift:'Context'().

-type operation() ::
    withdrawal.

-export_type([operation/0]).
-export_type([context/0]).

%%

-spec get_operation(context()) -> {ok, operation()} | {error, notfound}.
get_operation(#context_withdrawal_Context{op = {Operation, _}}) ->
    {ok, Operation};
get_operation(#context_withdrawal_Context{op = undefined}) ->
    {error, notfound}.

-spec get_value(atom(), context()) -> {ok, term()} | {error, notfound | {unsupported, _}}.
get_value(ValueName, Context) ->
    case get_operation(Context) of
        {ok, Operation} ->
            get_value(ValueName, Operation, Context);
        {error, _} = Error ->
            Error
    end.

get_value(owner_id, _Operation, Context) ->
    get_owner_id(Context);
get_value(created_at, Operation, Context) ->
    get_created_at(Operation, Context);
get_value(cost, Operation, Context) ->
    get_cost(Operation, Context);
get_value(payment_tool, Operation, Context) ->
    get_payment_tool(Operation, Context);
get_value(ValueName, _Operation, _Context) ->
    {error, {unsupported, ValueName}}.

-define(WITHDRAWAL(V), #context_withdrawal_Context{
    withdrawal = #context_withdrawal_Withdrawal{
        withdrawal = V = #wthd_domain_Withdrawal{}
    }
}).

get_owner_id(?WITHDRAWAL(Wthd)) ->
    Identity = Wthd#wthd_domain_Withdrawal.sender,
    {ok, Identity#wthd_domain_Identity.owner_id};
get_owner_id(_CtxWithdrawal) ->
    {error, notfound}.

get_created_at(withdrawal, ?WITHDRAWAL(Wthd)) ->
    {ok, Wthd#wthd_domain_Withdrawal.created_at};
get_created_at(_, _CtxWithdrawal) ->
    {error, notfound}.

get_cost(withdrawal, ?WITHDRAWAL(Wthd)) ->
    Body = Wthd#wthd_domain_Withdrawal.body,
    lim_payproc_utils:cash(Body);
get_cost(_, _CtxWithdrawal) ->
    {error, notfound}.

get_payment_tool(withdrawal, ?WITHDRAWAL(Wthd)) ->
    Destination = Wthd#wthd_domain_Withdrawal.destination,
    lim_payproc_utils:payment_tool(Destination);
get_payment_tool(_, _CtxWithdrawal) ->
    {error, notfound}.
