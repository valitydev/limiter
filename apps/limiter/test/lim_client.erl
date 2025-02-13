-module(lim_client).

-include_lib("limiter_proto/include/limproto_limiter_thrift.hrl").

-export([new/0]).
-export([get/4]).
-export([hold/3]).
-export([commit/3]).
-export([rollback/3]).

-export([get_values/3]).
-export([get_batch/3]).
-export([hold_batch/3]).
-export([commit_batch/3]).
-export([rollback_batch/3]).

-export([legacy_create_config/2]).
-export([create_config/2]).
-export([get_config/2]).

-type client() :: woody_context:ctx().

-type limit_id() :: limproto_limiter_thrift:'LimitID'().
-type limit_version() :: limproto_limiter_thrift:'Version'() | undefined.
-type limit_change() :: limproto_limiter_thrift:'LimitChange'().
-type limit_request() :: limproto_limiter_thrift:'LimitRequest'().
-type limit_context() :: limproto_limiter_thrift:'LimitContext'().
-type clock() :: limproto_limiter_thrift:'Clock'().
-type legacy_create_params() :: limproto_configurator_thrift:'LimitCreateParams'().
-type limit_config_params() :: limproto_config_thrift:'LimitConfigParams'().

%%% API

-spec new() -> client().
new() ->
    woody_context:new().

-spec get(limit_id(), limit_version(), limit_context(), client()) -> woody:result() | no_return().
get(LimitID, undefined, Context, Client) ->
    call('Get', {LimitID, clock(), Context}, Client);
get(LimitID, Version, Context, Client) ->
    call('GetVersioned', {LimitID, Version, clock(), Context}, Client).

-spec hold(limit_change(), limit_context(), client()) -> woody:result() | no_return().
hold(LimitChange, Context, Client) ->
    call('Hold', {LimitChange, clock(), Context}, Client).

-spec commit(limit_change(), limit_context(), client()) -> woody:result() | no_return().
commit(LimitChange, Context, Client) ->
    call('Commit', {LimitChange, clock(), Context}, Client).

-spec rollback(limit_change(), limit_context(), client()) -> woody:result() | no_return().
rollback(LimitChange, Context, Client) ->
    call('Rollback', {LimitChange, clock(), Context}, Client).

-spec get_values(limit_request(), limit_context(), client()) -> woody:result() | no_return().
get_values(LimitRequest, Context, Client) ->
    call('GetValues', {LimitRequest, Context}, Client).

-spec get_batch(limit_request(), limit_context(), client()) -> woody:result() | no_return().
get_batch(LimitRequest, Context, Client) ->
    call('GetBatch', {LimitRequest, Context}, Client).

-spec hold_batch(limit_request(), limit_context(), client()) -> woody:result() | no_return().
hold_batch(LimitRequest, Context, Client) ->
    call('HoldBatch', {LimitRequest, Context}, Client).

-spec commit_batch(limit_request(), limit_context(), client()) -> woody:result() | no_return().
commit_batch(LimitRequest, Context, Client) ->
    call('CommitBatch', {LimitRequest, Context}, Client).

-spec rollback_batch(limit_request(), limit_context(), client()) -> woody:result() | no_return().
rollback_batch(LimitRequest, Context, Client) ->
    call('RollbackBatch', {LimitRequest, Context}, Client).

%%

-spec legacy_create_config(legacy_create_params(), client()) -> woody:result() | no_return().
legacy_create_config(LimitCreateParams, Client) ->
    call_configurator('CreateLegacy', {LimitCreateParams}, Client).

-spec create_config(limit_config_params(), client()) -> woody:result() | no_return().
create_config(LimitCreateParams, Client) ->
    call_configurator('Create', {LimitCreateParams}, Client).

-spec get_config(limit_id(), client()) -> woody:result() | no_return().
get_config(LimitConfigID, Client) ->
    call_configurator('Get', {LimitConfigID}, Client).

%%% Internal functions

-spec call(atom(), tuple(), client()) -> woody:result() | no_return().
call(Function, Args, Client) ->
    Call = {{limproto_limiter_thrift, 'Limiter'}, Function, Args},
    Opts = #{
        url => <<"http://limiter:8022/v1/limiter">>,
        event_handler => {scoper_woody_event_handler, #{}},
        transport_opts => #{
            max_connections => 10000
        }
    },
    woody_client:call(Call, Opts, Client).

-spec call_configurator(atom(), tuple(), client()) -> woody:result() | no_return().
call_configurator(Function, Args, Client) ->
    Call = {{limproto_configurator_thrift, 'Configurator'}, Function, Args},
    Opts = #{
        url => <<"http://limiter:8022/v1/configurator">>,
        event_handler => {scoper_woody_event_handler, #{}},
        transport_opts => #{
            max_connections => 10000
        }
    },
    woody_client:call(Call, Opts, Client).

-spec clock() -> clock().
clock() ->
    {vector, #limiter_VectorClock{state = <<>>}}.
