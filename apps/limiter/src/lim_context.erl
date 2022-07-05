-module(lim_context).

-include_lib("limiter_proto/include/limproto_limiter_thrift.hrl").

-export([create/1]).
-export([woody_context/1]).
-export([get_operation/2]).
-export([get_value/3]).

-export([set_context/2]).
-export([set_clock/2]).

-export([clock/1]).

-type woody_context() :: woody_context:ctx().
-type limit_context() :: limproto_limiter_thrift:'LimitContext'().
-type clock() :: limproto_limiter_thrift:'Clock'().

-type t() :: #{
    woody_context => woody_context(),
    context => limit_context(),
    clock => clock()
}.

-type context_type() :: payment_processing | withdrawal_processing.
-type context_inner() :: lim_payproc_context:context() | lim_wthdproc_context:context().
-type context_operation() :: lim_payproc_context:operation() | lim_wthdproc_context:operation().

-export_type([t/0]).
-export_type([context_type/0]).
-export_type([context_operation/0]).

-callback get_operation(context_inner()) -> {ok, context_operation()} | {error, notfound}.
-callback get_value(_Name :: atom(), context_inner()) -> {ok, term()} | {error, notfound | {unsupported, _}}.

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

-spec set_context(limit_context(), t()) -> t().
set_context(Context, LimContext) ->
    LimContext#{context => Context}.

-spec set_clock(clock(), t()) -> t().
set_clock(Clock, LimContext) ->
    LimContext#{clock => Clock}.

-spec get_operation(context_type(), t()) ->
    {ok, context_operation()} | {error, notfound}.
get_operation(Type, Context) ->
    {Mod, OperationContext} = get_operation_context(Type, Context),
    Mod:get_operation(OperationContext).

-spec get_value(context_type(), atom(), t()) ->
    {ok, term()} | {error, notfound | {unsupported, _}}.
get_value(Type, ValueName, Context) ->
    {Mod, OperationContext} = get_operation_context(Type, Context),
    Mod:get_value(ValueName, OperationContext).

get_operation_context(
    payment_processing,
    #{context := #limiter_LimitContext{payment_processing = PayprocContext}}
) ->
    {lim_payproc_context, PayprocContext};
get_operation_context(
    withdrawal_processing,
    #{context := #limiter_LimitContext{withdrawal_processing = WithdrawalContext}}
) ->
    {lim_wthdproc_context, WithdrawalContext}.
