-module(lim_configurator_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("limiter_proto/include/limproto_configurator_thrift.hrl").
-include("lim_ct_helper.hrl").

-export([all/0]).

-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([legacy_create_config/1]).
-export([create_config/1]).
-export([create_config_single_scope/1]).
-export([get_config/1]).
-export([get_inexistent_config/1]).

-type group_name() :: atom().
-type test_case_name() :: atom().

%% tests descriptions

-spec all() -> [{group, group_name()}].
all() ->
    [
        {group, default}
    ].

-spec groups() -> [{atom(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [], [
            legacy_create_config,
            create_config,
            create_config_single_scope,
            get_config,
            get_inexistent_config
        ]}
    ].

-type config() :: [{atom(), any()}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    % dbg:tracer(), dbg:p(all, c),
    % dbg:tpl({machinery, '_', '_'}, x),
    Apps =
        genlib_app:start_application_with(limiter, [
            {service_clients, #{
                accounter => #{
                    url => <<"http://shumway:8022/accounter">>
                },
                automaton => #{
                    url => <<"http://machinegun:8022/v1/automaton">>
                }
            }}
        ]),
    [{apps, Apps}] ++ Config.

-spec end_per_suite(config()) -> _.
end_per_suite(Config) ->
    _ = [application:stop(App) || App <- proplists:get_value(apps, Config)].

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    [{limit_id, mk_limit_id(Name)} | C].

-spec end_per_testcase(test_case_name(), config()) -> ok.
end_per_testcase(_Name, _C) ->
    ok.

mk_limit_id(Name) ->
    genlib:format("~p/~B", [Name, erlang:system_time(millisecond)]).

%%

-spec legacy_create_config(config()) -> _.
legacy_create_config(C) ->
    Client = lim_client:new(),
    ID = ?config(limit_id, C),
    Description = genlib:unique(),
    Params = #configurator_LimitCreateParams{
        id = ID,
        name = <<"GlobalMonthTurnover">>,
        description = Description,
        started_at = <<"2000-01-01T00:00:00Z">>
    },
    ?assertMatch(
        {ok, #config_LimitConfig{
            id = ID,
            description = Description
        }},
        lim_client:legacy_create_config(Params, Client)
    ).

-spec create_config(config()) -> _.
create_config(C) ->
    Client = lim_client:new(),
    ID = ?config(limit_id, C),
    Description = genlib:unique(),
    Params = #config_LimitConfigParams{
        id = ?config(limit_id, C),
        description = Description,
        started_at = <<"2000-01-01T00:00:00Z">>,
        shard_size = 4,
        time_range_type = ?time_range_week(),
        type = ?lim_type_turnover(?turnover_metric_amount()),
        scope = ?scope([
            ?scope_shop(),
            ?scope_party()
        ]),
        op_behaviour = ?op_behaviour(),
        context_type = ?ctx_type_payproc()
    },
    ?assertMatch(
        {ok, #config_LimitConfig{
            id = ID,
            description = Description
        }},
        lim_client:create_config(Params, Client)
    ).

-spec create_config_single_scope(config()) -> _.
create_config_single_scope(C) ->
    Client = lim_client:new(),
    Params = #config_LimitConfigParams{
        id = ?config(limit_id, C),
        started_at = <<"2000-01-01T00:00:00Z">>,
        time_range_type = ?time_range_week(),
        shard_size = 1,
        type = ?lim_type_turnover(),
        scope = {single, ?scope_party()},
        context_type = ?ctx_type_payproc(),
        op_behaviour = ?op_behaviour()
    },
    {ok, #config_LimitConfig{
        scope = Scope
    }} = lim_client:create_config(Params, Client),
    ?assertEqual(?scope([?scope_party()]), Scope).

-spec get_config(config()) -> _.
get_config(C) ->
    ID = ?config(limit_id, C),
    #{client := Client} = prepare_environment(ID, <<"GlobalMonthTurnover">>, C),
    {ok, #config_LimitConfig{id = ID}} = lim_client:get_config(ID, Client).

-spec get_inexistent_config(config()) -> _.
get_inexistent_config(_C) ->
    ?assertEqual(
        {exception, #configurator_LimitConfigNotFound{}},
        lim_client:get_config(<<"NOSUCHCONFIG">>, lim_client:new())
    ).

%%

prepare_environment(ID, LimitName, _C) ->
    Client = lim_client:new(),
    Params = #configurator_LimitCreateParams{
        id = ID,
        name = LimitName,
        description = <<"description">>,
        started_at = <<"2000-01-01T00:00:00Z">>
    },
    {ok, LimitConfig} = lim_client:legacy_create_config(Params, Client),
    #{config => LimitConfig, client => Client}.
