-module(lim_configurator_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("limiter_proto/include/lim_configurator_thrift.hrl").
-include("lim_ct_helper.hrl").

-export([all/0]).

-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([legacy_create_config/1]).
-export([create_config/1]).
-export([get_config/1]).

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
            get_config
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
    Params = #limiter_cfg_LimitCreateParams{
        id = ID,
        name = <<"GlobalMonthTurnover">>,
        description = Description,
        started_at = <<"2000-01-01T00:00:00Z">>,
        body_type = ?body_type_cash()
    },
    ?assertMatch(
        {ok, #limiter_config_LimitConfig{
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
    Params = #limiter_config_LimitConfigParams{
        id = ?config(limit_id, C),
        description = Description,
        started_at = <<"2000-01-01T00:00:00Z">>,
        body_type = ?body_type_cash(<<"RUB">>),
        shard_size = 4,
        time_range_type = ?time_range_week(),
        type = ?lim_type_turnover(),
        scope = ?scope([
            {shop, #limiter_config_LimitScopeEmptyDetails{}},
            {party, #limiter_config_LimitScopeEmptyDetails{}}
        ]),
        op_behaviour = ?op_behaviour(),
        context_type = ?ctx_type_payproc()
    },
    ?assertMatch(
        {ok, #limiter_config_LimitConfig{
            id = ID,
            description = Description
        }},
        lim_client:create_config(Params, Client)
    ).

    },
    {ok, #limiter_config_LimitConfig{}} = lim_client:create_config(Params, Client).

-spec get_config(config()) -> _.
get_config(C) ->
    ID = ?config(limit_id, C),
    #{client := Client} = prepare_environment(ID, <<"GlobalMonthTurnover">>, C),
    {ok, #limiter_config_LimitConfig{id = ID}} = lim_client:get_config(ID, Client).

%%

prepare_environment(ID, LimitName, _C) ->
    Client = lim_client:new(),
    Params = #limiter_cfg_LimitCreateParams{
        id = ID,
        name = LimitName,
        description = <<"description">>,
        started_at = <<"2000-01-01T00:00:00Z">>,
        body_type = ?body_type_cash()
    },
    {ok, LimitConfig} = lim_client:legacy_create_config(Params, Client),
    #{config => LimitConfig, client => Client}.
