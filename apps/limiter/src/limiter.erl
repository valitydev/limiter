-module(limiter).

%% Application callbacks

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% Supervisor callbacks

-behaviour(supervisor).

-export([init/1]).

%%

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    case ensure_otel_log_handler() of
        ok ->
            ok = setup_metrics(),
            supervisor:start_link({local, ?MODULE}, ?MODULE, []);
        {error, Reason} ->
            logger:error("Failed to add otel_logs handler: ~p", [Reason]),
            {error, Reason}
    end.

-spec stop(any()) -> ok.
stop(_State) ->
    ok = flush_otel_logs(),
    ok.

%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ServiceOpts = genlib_app:env(?MODULE, services, #{}),
    Healthcheck = enable_health_logging(genlib_app:env(?MODULE, health_check, #{})),
    EventHandlers = genlib_app:env(?MODULE, woody_event_handlers, [{woody_event_handler_default, #{}}]),

    ChildSpec = woody_server:child_spec(
        ?MODULE,
        #{
            ip => get_ip_address(),
            port => get_port(),
            protocol_opts => get_protocol_opts(),
            transport_opts => get_transport_opts(),
            shutdown_timeout => get_shutdown_timeout(),
            event_handler => EventHandlers,
            handlers => get_handler_specs(ServiceOpts),
            additional_routes => [erl_health_handle:get_route(Healthcheck)] ++ get_prometheus_route()
        }
    ),
    {ok,
        {
            #{strategy => one_for_all, intensity => 6, period => 30},
            [ChildSpec]
        }}.

-spec get_ip_address() -> inet:ip_address().
get_ip_address() ->
    {ok, Address} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    Address.

-spec get_port() -> inet:port_number().
get_port() ->
    genlib_app:env(?MODULE, port, 8022).

-spec get_protocol_opts() -> woody_server_thrift_http_handler:protocol_opts().
get_protocol_opts() ->
    genlib_app:env(?MODULE, protocol_opts, #{}).

-spec get_transport_opts() -> woody_server_thrift_http_handler:transport_opts().
get_transport_opts() ->
    genlib_app:env(?MODULE, transport_opts, #{}).

-spec get_shutdown_timeout() -> timeout().
get_shutdown_timeout() ->
    genlib_app:env(?MODULE, shutdown_timeout, 0).

-spec get_handler_specs(map()) -> [woody:http_handler(woody:th_handler())].
get_handler_specs(ServiceOpts) ->
    LimiterService = maps:get(limiter, ServiceOpts, #{}),
    [
        {
            maps:get(path, LimiterService, <<"/v1/limiter">>),
            {{limproto_limiter_thrift, 'Limiter'}, lim_handler}
        }
    ].

%%

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(
        fun(_, Runner) -> #{runner => Runner, event_handler => EvHandler} end,
        Check
    ).

-spec get_prometheus_route() -> [{iodata(), module(), _Opts :: any()}].
get_prometheus_route() ->
    [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}].

setup_metrics() ->
    ok = woody_ranch_prometheus_collector:setup(),
    ok = woody_hackney_prometheus_collector:setup().

ensure_otel_log_handler() ->
    case logger:get_handler_config(otel_logs) of
        {ok, _} ->
            ok;
        _ ->
            MaxQueue = application:get_env(limiter, otel_log_max_queue_size, 2048),
            DelayMs = application:get_env(limiter, otel_log_scheduled_delay_ms, 1000),
            TimeoutMs = application:get_env(limiter, otel_log_exporting_timeout_ms, 300000),
            LogLevel = application:get_env(limiter, otel_log_level, info),
            HandlerConfig = #{
                report_cb => fun lim_otel_log_filter:format_otp_report_utf8/1,
                exporter =>
                    {otel_exporter_logs_otlp, #{
                        protocol => http_protobuf,
                        ssl_options => []
                    }},
                max_queue_size => MaxQueue,
                scheduled_delay_ms => DelayMs,
                exporting_timeout_ms => TimeoutMs
            },
            LoggerHandlerConfig = #{
                level => LogLevel,
                filter_default => log,
                filters => [{lim_otel_trace_id_bytes, {fun lim_otel_log_filter:filter/2, undefined}}],
                config => HandlerConfig
            },
            case logger:add_handler(otel_logs, lim_otel_log_handler, LoggerHandlerConfig) of
                ok ->
                    ok;
                {error, {already_exist, _}} ->
                    ok;
                {error, Reason} ->
                    {error, {otel_log_handler_failed, Reason}}
            end
    end.

%% @doc Ждём отправки буферизованных логов перед остановкой.
%% otel_log_handler батчит логи и отправляет по таймеру (scheduled_delay_ms).
%% Явного API для flush у otel_log_handler нет, поэтому ждём один полный цикл
%% батчинга + запас на сетевую отправку (export overhead).
-define(FLUSH_EXPORT_OVERHEAD_MS, 700).
-define(FLUSH_MAX_WAIT_MS, 5000).

flush_otel_logs() ->
    case logger:get_handler_config(otel_logs) of
        {ok, HandlerCfg} ->
            Config = maps:get(config, HandlerCfg, #{}),
            DelayMs = maps:get(
                scheduled_delay_ms,
                Config,
                maps:get(scheduled_delay_ms, HandlerCfg, 1000)
            ),
            _ = logger:info("otel_log_handler_flush"),
            timer:sleep(erlang:min(?FLUSH_MAX_WAIT_MS, DelayMs + ?FLUSH_EXPORT_OVERHEAD_MS)),
            ok;
        _ ->
            ok
    end.
