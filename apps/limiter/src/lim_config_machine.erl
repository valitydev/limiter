-module(lim_config_machine).

-include_lib("limiter_proto/include/limproto_limiter_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").

%% Accessors

-export([created_at/1]).
-export([id/1]).
-export([description/1]).
-export([started_at/1]).
-export([shard_size/1]).
-export([time_range_type/1]).
-export([processor_type/1]).
-export([type/1]).
-export([scope/1]).
-export([context_type/1]).
-export([op_behaviour/1]).

%% API

-export([start/3]).
-export([get/2]).

-export([get_limit/3]).
-export([hold/2]).
-export([commit/2]).
-export([rollback/2]).

-export([calculate_shard_id/2]).
-export([calculate_time_range/2]).
-export([mk_scope_prefix/2]).

-type woody_context() :: woody_context:ctx().
-type lim_context() :: lim_context:t().
-type processor_type() :: lim_router:processor_type().
-type processor() :: lim_router:processor().
-type description() :: binary().

-type limit_type() :: {turnover, lim_turnover_metric:t()}.
-type limit_scope() :: ordsets:ordset(limit_scope_type()).
-type limit_scope_type() :: party | shop | wallet | identity | payment_tool | provider | terminal | payer_contact_email.
-type shard_size() :: pos_integer().
-type shard_id() :: binary().
-type prefix() :: binary().
-type time_range_type() :: {calendar, year | month | week | day} | {interval, pos_integer()}.
-type time_range() :: #{
    upper := timestamp(),
    lower := timestamp()
}.

-type context_type() :: lim_context:context_type().

-type config() :: #{
    id := lim_id(),
    processor_type := processor_type(),
    created_at := lim_time:timestamp_ms(),
    started_at := timestamp(),
    shard_size := shard_size(),
    time_range_type := time_range_type(),
    context_type := context_type(),
    type := limit_type(),
    scope => limit_scope(),
    description => description(),
    op_behaviour => op_behaviour()
}.

-type create_params() :: #{
    processor_type := processor_type(),
    started_at := timestamp(),
    shard_size := shard_size(),
    time_range_type := time_range_type(),
    context_type := context_type(),
    type => limit_type(),
    scope => limit_scope(),
    description => description(),
    op_behaviour => op_behaviour()
}.

-type op_behaviour() :: #{operation_type() := addition | subtraction}.
-type operation_type() :: invoice_payment_refund.

-type lim_id() :: limproto_limiter_thrift:'LimitID'().
-type lim_version() :: dmsl_domain_thrift:'DataRevision'() | undefined.
-type lim_change() :: limproto_limiter_thrift:'LimitChange'().
-type limit() :: limproto_limiter_thrift:'Limit'().
-type timestamp() :: dmsl_base_thrift:'Timestamp'().

-export_type([config/0]).
-export_type([limit_type/0]).
-export_type([limit_scope/0]).
-export_type([time_range_type/0]).
-export_type([time_range/0]).
-export_type([create_params/0]).
-export_type([lim_id/0]).
-export_type([lim_change/0]).
-export_type([limit/0]).
-export_type([timestamp/0]).

%% Machinery callbacks

-behaviour(machinery).

-export([init/4]).
-export([process_call/4]).
-export([process_timeout/3]).
-export([process_repair/4]).

-type timestamped_event(T) ::
    {ev, machinery:timestamp(), T}.

-type event() ::
    {created, config()}.

-type args(T) :: machinery:args(T).
-type machine() :: machinery:machine(event(), _).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).
-type result() :: machinery:result(timestamped_event(event()), none()).

-export_type([timestamped_event/1]).
-export_type([event/0]).

-define(NS, 'lim/config_v1').

%% Handler behaviour

-callback get_limit(
    ID :: lim_id(),
    Config :: config(),
    LimitContext :: lim_context()
) -> {ok, limit()} | {error, get_limit_error()}.

-callback hold(
    LimitChange :: lim_change(),
    Config :: config(),
    LimitContext :: lim_context()
) -> ok | {error, hold_error()}.

-callback commit(
    LimitChange :: lim_change(),
    Config :: config(),
    LimitContext :: lim_context()
) -> ok | {error, commit_error()}.

-callback rollback(
    LimitChange :: lim_change(),
    Config :: config(),
    LimitContext :: lim_context()
) -> ok | {error, rollback_error()}.

-type get_limit_error() :: lim_turnover_processor:get_limit_error().
-type hold_error() :: lim_turnover_processor:hold_error().
-type commit_error() :: lim_turnover_processor:commit_error().
-type rollback_error() :: lim_turnover_processor:rollback_error().

-type config_error() :: {config, notfound}.

-import(lim_pipeline, [do/1, unwrap/1, unwrap/2]).

%% Accessors

-spec created_at(config()) -> timestamp().
created_at(#{created_at := CreatedAt}) ->
    lim_time:to_rfc3339(CreatedAt).

-spec id(config()) -> lim_id().
id(#{id := ID}) ->
    ID.

-spec description(config()) -> lim_maybe:maybe(description()).
description(#{description := ID}) ->
    ID;
description(_) ->
    undefined.

-spec started_at(config()) -> timestamp().
started_at(#{started_at := Value}) ->
    Value.

-spec shard_size(config()) -> shard_size().
shard_size(#{shard_size := Value}) ->
    Value.

-spec time_range_type(config()) -> time_range_type().
time_range_type(#{time_range_type := Value}) ->
    Value.

-spec processor_type(config()) -> processor_type().
processor_type(#{processor_type := Value}) ->
    Value.

-spec type(config()) -> limit_type().
type(#{type := Value}) ->
    Value;
type(_) ->
    {turnover, number}.

-spec scope(config()) -> limit_scope().
scope(#{scope := Value}) ->
    Value;
scope(_) ->
    ordsets:new().

-spec context_type(config()) -> context_type().
context_type(#{context_type := Value}) ->
    Value.

-spec op_behaviour(config()) -> lim_maybe:maybe(op_behaviour()).
op_behaviour(#{op_behaviour := Value}) ->
    Value;
op_behaviour(_) ->
    undefined.

%%

-spec start(lim_id(), create_params(), lim_context()) -> {ok, config()}.
start(ID, Params, LimitContext) ->
    WoodyCtx = lim_context:woody_context(LimitContext),
    Config = genlib_map:compact(Params#{id => ID, created_at => lim_time:now()}),
    case machinery:start(?NS, ID, [{created, Config}], get_backend(WoodyCtx)) of
        ok ->
            {ok, Config};
        {error, exists} ->
            {ok, Machine} = machinery:get(?NS, ID, get_backend(WoodyCtx)),
            {ok, collapse(Machine)}
    end.

-spec get(lim_id(), lim_context()) -> {ok, config()} | {error, notfound}.
get(ID, LimitContext) ->
    do(fun() ->
        WoodyCtx = lim_context:woody_context(LimitContext),
        Machine = unwrap(machinery:get(?NS, ID, get_backend(WoodyCtx))),
        collapse(Machine)
    end).

-spec get_limit(lim_id(), lim_version(), lim_context()) ->
    {ok, limit()} | {error, config_error() | {processor(), get_limit_error()}}.
get_limit(ID, Version, LimitContext) ->
    do(fun() ->
        {Handler, Config} = unwrap(get_handler(ID, Version, LimitContext)),
        unwrap(Handler, Handler:get_limit(ID, Config, LimitContext))
    end).

-spec hold(lim_change(), lim_context()) -> ok | {error, config_error() | {processor(), hold_error()}}.
hold(LimitChange = #limiter_LimitChange{id = ID, version = Version}, LimitContext) ->
    do(fun() ->
        {Handler, Config} = unwrap(get_handler(ID, Version, LimitContext)),
        unwrap(Handler, Handler:hold(LimitChange, Config, LimitContext))
    end).

-spec commit(lim_change(), lim_context()) -> ok | {error, config_error() | {processor(), commit_error()}}.
commit(LimitChange = #limiter_LimitChange{id = ID, version = Version}, LimitContext) ->
    do(fun() ->
        {Handler, Config} = unwrap(get_handler(ID, Version, LimitContext)),
        unwrap(Handler, Handler:commit(LimitChange, Config, LimitContext))
    end).

-spec rollback(lim_change(), lim_context()) -> ok | {error, config_error() | {processor(), rollback_error()}}.
rollback(LimitChange = #limiter_LimitChange{id = ID, version = Version}, LimitContext) ->
    do(fun() ->
        {Handler, Config} = unwrap(get_handler(ID, Version, LimitContext)),
        unwrap(Handler, Handler:rollback(LimitChange, Config, LimitContext))
    end).

get_handler(ID, Version, LimitContext) ->
    do(fun() ->
        Config = #{processor_type := ProcessorType} = unwrap(config, get_config(ID, Version, LimitContext)),
        {ok, Handler} = lim_router:get_handler(ProcessorType),
        {Handler, Config}
    end).

-spec get_config(lim_id(), lim_version(), lim_context()) -> {ok, config()} | {error, notfound}.
get_config(ID, undefined, LimitContext) ->
    get(ID, LimitContext);
get_config(ID, Version, #{woody_context := WoodyContext}) ->
    LimitConfigRef = {limit_config, #domain_LimitConfigRef{id = ID}},
    try
        Object = dmt_client:checkout_versioned_object(Version, LimitConfigRef, #{woody_context => WoodyContext}),
        #domain_conf_VersionedObject{object = {limit_config, ConfigObject}} = Object,
        {ok, lim_config_codec:unmarshal('LimitConfigObject', ConfigObject)}
    catch
        throw:#domain_conf_ObjectNotFound{} ->
            {error, notfound}
    end.

-spec calculate_time_range(timestamp(), config()) -> time_range().
calculate_time_range(Timestamp, Config) ->
    StartedAt = started_at(Config),
    case time_range_type(Config) of
        {calendar, Range} ->
            calculate_calendar_time_range(Range, Timestamp, StartedAt);
        {interval, _Interval} ->
            erlang:error({interval_time_range_not_implemented, Config})
    end.

calculate_calendar_time_range(Range, Timestamp, StartedAt) ->
    {StartDatetime, _USec0} = lim_range_codec:parse_timestamp(StartedAt),
    {CurrentDatetime, _USec1} = lim_range_codec:parse_timestamp(Timestamp),
    CurrentSec = calendar:datetime_to_gregorian_seconds(CurrentDatetime),
    calculate_calendar_time_range(Range, CurrentSec, CurrentDatetime, StartDatetime).

calculate_calendar_time_range(year, CurrentSec, {CurrentDate, _CurrentTime}, {StartDate, StartTime}) ->
    {_StartYear, StartMonth, StartDay} = StartDate,
    {CurrentYear, _CurrentMonth, _} = CurrentDate,
    ClampedStartDay = clamp_month_start_day(CurrentYear, StartMonth, StartDay),
    LowerSec = calendar:datetime_to_gregorian_seconds(
        {{CurrentYear, StartMonth, ClampedStartDay}, StartTime}
    ),
    NextYearDay = clamp_month_start_day(CurrentYear + 1, StartMonth, StartDay),
    UpperSec = calendar:datetime_to_gregorian_seconds(
        {{CurrentYear + 1, StartMonth, NextYearDay}, StartTime}
    ),
    calculate_year_time_range(CurrentSec, LowerSec, UpperSec);
calculate_calendar_time_range(month, CurrentSec, {CurrentDate, _CurrentTime}, {StartDate, StartTime}) ->
    {_StartYear, _StartMonth, StartDay} = StartDate,
    {CurrentYear, CurrentMonth, _} = CurrentDate,
    ClampedStartDay = clamp_month_start_day(CurrentYear, CurrentMonth, StartDay),
    LowerSec = calendar:datetime_to_gregorian_seconds(
        {{CurrentYear, CurrentMonth, ClampedStartDay}, StartTime}
    ),
    UpperSec =
        case CurrentMonth < 12 of
            true ->
                NextMonthDay = clamp_month_start_day(CurrentYear, CurrentMonth + 1, StartDay),
                calendar:datetime_to_gregorian_seconds(
                    {{CurrentYear, CurrentMonth + 1, NextMonthDay}, StartTime}
                );
            false ->
                NextYearDay = clamp_month_start_day(CurrentYear + 1, CurrentMonth, StartDay),
                calendar:datetime_to_gregorian_seconds(
                    {{CurrentYear + 1, 1, NextYearDay}, StartTime}
                )
        end,
    calculate_month_time_range(CurrentSec, LowerSec, UpperSec);
calculate_calendar_time_range(week, CurrentSec, {CurrentDate, _CurrentTime}, {StartDate, StartTime}) ->
    StartWeekRem = calendar:date_to_gregorian_days(StartDate) rem 7,
    LowerWeek = (calendar:date_to_gregorian_days(CurrentDate) div 7) * 7 + StartWeekRem,
    UpperWeek = LowerWeek + 7,
    LowerSec = calendar:datetime_to_gregorian_seconds(
        {calendar:gregorian_days_to_date(LowerWeek), StartTime}
    ),
    UpperSec = calendar:datetime_to_gregorian_seconds(
        {calendar:gregorian_days_to_date(UpperWeek), StartTime}
    ),
    calculate_week_time_range(CurrentSec, LowerSec, UpperSec);
calculate_calendar_time_range(day, CurrentSec, {CurrentDate, _CurrentTime}, {_StartDate, StartTime}) ->
    Lower = calendar:date_to_gregorian_days(CurrentDate),
    UpperDate = calendar:gregorian_days_to_date(Lower + 1),
    LowerSec = calendar:datetime_to_gregorian_seconds({CurrentDate, StartTime}),
    UpperSec = calendar:datetime_to_gregorian_seconds({UpperDate, StartTime}),
    calculate_day_time_range(CurrentSec, LowerSec, UpperSec).

clamp_month_start_day(Year, Month, StartDay) ->
    Last = calendar:last_day_of_the_month(Year, Month),
    case StartDay > Last of
        true ->
            Last;
        false ->
            StartDay
    end.

calculate_year_time_range(CurrentSec, LowerSec, UpperSec) when
    CurrentSec >= LowerSec andalso
        CurrentSec < UpperSec
->
    mk_time_range(LowerSec, UpperSec);
calculate_year_time_range(CurrentSec, LowerSec, _UpperSec) when CurrentSec < LowerSec ->
    {{Year, Month, Day}, Time} = calendar:gregorian_seconds_to_datetime(LowerSec),
    PrevYearDay = clamp_month_start_day(Year - 1, Month, Day),
    LowerDate = {Year - 1, Month, PrevYearDay},
    #{
        lower => marshal_timestamp({LowerDate, Time}),
        upper => marshal_timestamp(calendar:gregorian_seconds_to_datetime(LowerSec))
    }.

calculate_month_time_range(CurrentSec, LowerSec, UpperSec) when
    CurrentSec >= LowerSec andalso
        CurrentSec < UpperSec
->
    mk_time_range(LowerSec, UpperSec);
calculate_month_time_range(CurrentSec, LowerSec, _UpperSec) when CurrentSec < LowerSec ->
    {{Year, Month, Day}, Time} = calendar:gregorian_seconds_to_datetime(LowerSec),
    LowerDate =
        case Month =:= 1 of
            true ->
                PrevYearDay = clamp_month_start_day(Year - 1, 12, Day),
                {Year - 1, 12, PrevYearDay};
            false ->
                PrevMonthDay = clamp_month_start_day(Year, Month - 1, Day),
                {Year, Month - 1, PrevMonthDay}
        end,
    #{
        lower => marshal_timestamp({LowerDate, Time}),
        upper => marshal_timestamp(calendar:gregorian_seconds_to_datetime(LowerSec))
    }.

calculate_week_time_range(CurrentSec, LowerSec, UpperSec) when
    CurrentSec >= LowerSec andalso
        CurrentSec < UpperSec
->
    mk_time_range(LowerSec, UpperSec);
calculate_week_time_range(CurrentSec, LowerSec, _UpperSec) when CurrentSec < LowerSec ->
    {Date, Time} = calendar:gregorian_seconds_to_datetime(LowerSec),
    Days = calendar:date_to_gregorian_days(Date),
    LowerDate = calendar:gregorian_days_to_date(Days - 7),
    #{
        lower => marshal_timestamp({LowerDate, Time}),
        upper => marshal_timestamp(calendar:gregorian_seconds_to_datetime(LowerSec))
    }.

calculate_day_time_range(CurrentSec, LowerSec, UpperSec) when
    CurrentSec >= LowerSec andalso
        CurrentSec < UpperSec
->
    mk_time_range(LowerSec, UpperSec);
calculate_day_time_range(CurrentSec, LowerSec, _UpperSec) when CurrentSec < LowerSec ->
    {Date, Time} = calendar:gregorian_seconds_to_datetime(LowerSec),
    Days = calendar:date_to_gregorian_days(Date),
    LowerDate = calendar:gregorian_days_to_date(Days - 1),
    #{
        lower => marshal_timestamp({LowerDate, Time}),
        upper => marshal_timestamp(calendar:gregorian_seconds_to_datetime(LowerSec))
    }.

mk_time_range(LowerSec, UpperSec) ->
    #{
        lower => marshal_timestamp(calendar:gregorian_seconds_to_datetime(LowerSec)),
        upper => marshal_timestamp(calendar:gregorian_seconds_to_datetime(UpperSec))
    }.

marshal_timestamp(DateTime) ->
    lim_range_codec:marshal(timestamp, {DateTime, 0}).

-spec calculate_shard_id(timestamp(), config()) -> shard_id().
calculate_shard_id(Timestamp, Config) ->
    StartedAt = started_at(Config),
    ShardSize = shard_size(Config),
    case time_range_type(Config) of
        {calendar, Range} ->
            calculate_calendar_shard_id(Range, Timestamp, StartedAt, ShardSize);
        {interval, _Interval} ->
            erlang:error({interval_time_range_not_implemented, Config})
    end.

calculate_calendar_shard_id(Range, Timestamp, StartedAt, ShardSize) ->
    {StartDatetime, _USec0} = lim_range_codec:parse_timestamp(StartedAt),
    {CurrentDatetime, _USec1} = lim_range_codec:parse_timestamp(Timestamp),
    Units = calculate_time_units(Range, CurrentDatetime, StartDatetime),
    SignPrefix = mk_sign_prefix(Units),
    RangePrefix = mk_unit_prefix(Range),
    mk_shard_id(<<SignPrefix/binary, "/", RangePrefix/binary>>, Units, ShardSize).

calculate_time_units(year, CurrentDatetime, StartDatetime) ->
    StartSecBase = calculate_start_of_year_seconds(StartDatetime),
    StartSec = calendar:datetime_to_gregorian_seconds(StartDatetime),
    CurrentSecBase = calculate_start_of_year_seconds(CurrentDatetime),
    CurrentSec = calendar:datetime_to_gregorian_seconds(CurrentDatetime),

    StartDelta = StartSec - StartSecBase,
    CurrentDelta = CurrentSec - (CurrentSecBase + StartDelta),
    maybe_previous_unit(CurrentDelta, year(CurrentDatetime) - year(StartDatetime));
calculate_time_units(month, CurrentDatetime, StartDatetime) ->
    StartSecBase = calculate_start_of_month_seconds(StartDatetime),
    StartSec = calendar:datetime_to_gregorian_seconds(StartDatetime),
    CurrentSecBase = calculate_start_of_month_seconds(CurrentDatetime),
    CurrentSec = calendar:datetime_to_gregorian_seconds(CurrentDatetime),

    StartDelta = StartSec - StartSecBase,
    CurrentDelta = CurrentSec - (CurrentSecBase + StartDelta),

    YearDiff = year(CurrentDatetime) - year(StartDatetime),
    MonthDiff = month(CurrentDatetime) - month(StartDatetime),

    maybe_previous_unit(CurrentDelta, YearDiff * 12 + MonthDiff);
calculate_time_units(week, {CurrentDate, CurrentTime}, {StartDate, StartTime}) ->
    StartWeekRem = calendar:date_to_gregorian_days(StartDate) rem 7,
    StartWeekBase = (calendar:date_to_gregorian_days(StartDate) div 7) * 7,
    CurrentWeekBase = (calendar:date_to_gregorian_days(CurrentDate) div 7) * 7,

    StartSecBase = calendar:datetime_to_gregorian_seconds(
        {calendar:gregorian_days_to_date(StartWeekBase), {0, 0, 0}}
    ),
    StartSec = calendar:datetime_to_gregorian_seconds(
        {calendar:gregorian_days_to_date(StartWeekBase + StartWeekRem), StartTime}
    ),
    CurrentSecBase = calendar:datetime_to_gregorian_seconds(
        {calendar:gregorian_days_to_date(CurrentWeekBase), {0, 0, 0}}
    ),
    CurrentSec = calendar:datetime_to_gregorian_seconds(
        {calendar:gregorian_days_to_date(CurrentWeekBase + StartWeekRem), CurrentTime}
    ),

    StartDelta = StartSec - StartSecBase,
    CurrentDelta = CurrentSec - (CurrentSecBase + StartDelta),

    StartWeeks = calendar:date_to_gregorian_days(StartDate) div 7,
    CurrentWeeks = calendar:date_to_gregorian_days(CurrentDate) div 7,
    maybe_previous_unit(CurrentDelta, CurrentWeeks - StartWeeks);
calculate_time_units(day, {CurrentDate, CurrentTime}, {StartDate, StartTime}) ->
    StartSecBase = calendar:datetime_to_gregorian_seconds({StartDate, {0, 0, 0}}),
    StartSec = calendar:datetime_to_gregorian_seconds({StartDate, StartTime}),
    CurrentSecBase = calendar:datetime_to_gregorian_seconds({CurrentDate, {0, 0, 0}}),
    CurrentSec = calendar:datetime_to_gregorian_seconds({CurrentDate, CurrentTime}),
    StartDelta = StartSec - StartSecBase,
    CurrentDelta = CurrentSec - (CurrentSecBase + StartDelta),
    StartDays = calendar:date_to_gregorian_days(StartDate),
    CurrentDays = calendar:date_to_gregorian_days(CurrentDate),
    maybe_previous_unit(CurrentDelta, CurrentDays - StartDays).

maybe_previous_unit(Delta, Unit) when Delta < 0 ->
    Unit - 1;
maybe_previous_unit(_Delta, Unit) ->
    Unit.

calculate_start_of_year_seconds({{Year, _, _}, _Time}) ->
    calendar:datetime_to_gregorian_seconds({{Year, 1, 1}, {0, 0, 0}}).

calculate_start_of_month_seconds({{Year, Month, _}, _Time}) ->
    calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, {0, 0, 0}}).

year({{Year, _, _}, _Time}) ->
    Year.

month({{_Year, Month, _}, _Time}) ->
    Month.

mk_unit_prefix(day) -> <<"day">>;
mk_unit_prefix(week) -> <<"week">>;
mk_unit_prefix(month) -> <<"month">>;
mk_unit_prefix(year) -> <<"year">>.

mk_sign_prefix(Units) when Units >= 0 -> <<"future">>;
mk_sign_prefix(_) -> <<"past">>.

mk_shard_id(Prefix, Units, ShardSize) ->
    ID = integer_to_binary(abs(Units) div ShardSize),
    <<Prefix/binary, "/", ID/binary>>.

-spec mk_scope_prefix(config(), lim_context()) -> {ok, prefix()} | {error, lim_context:context_error()}.
mk_scope_prefix(Config, LimitContext) ->
    mk_scope_prefix_impl(scope(Config), context_type(Config), LimitContext).

-spec mk_scope_prefix_impl(limit_scope(), context_type(), lim_context()) ->
    {ok, prefix()} | {error, lim_context:context_error()}.
mk_scope_prefix_impl(Scope, ContextType, LimitContext) ->
    do(fun() ->
        Bits = enumerate_context_bits(Scope),
        lists:foldl(
            fun(Bit, Acc) ->
                Value = unwrap(extract_context_bit(Bit, ContextType, LimitContext)),
                append_prefix(Value, Acc)
            end,
            <<>>,
            Bits
        )
    end).

-spec append_prefix(binary(), prefix()) -> prefix().
append_prefix(Fragment, Acc) ->
    <<Acc/binary, "/", Fragment/binary>>.

-type context_bit() ::
    {from, _ValueName :: atom()}
    | {prefix, prefix()}.

-spec enumerate_context_bits(limit_scope()) -> [context_bit()].
enumerate_context_bits(Types) ->
    TypesOrder = [party, shop, identity, wallet, payment_tool, provider, terminal, payer_contact_email],
    SortedTypes = lists:filter(fun(T) -> ordsets:is_element(T, Types) end, TypesOrder),
    SquashedTypes = squash_scope_types(SortedTypes),
    lists:flatmap(fun get_context_bits/1, SquashedTypes).

squash_scope_types([party, shop | Rest]) ->
    % NOTE
    % Shop scope implies party scope.
    [shop | squash_scope_types(Rest)];
squash_scope_types([identity, wallet | Rest]) ->
    % NOTE
    % Wallet scope implies identity scope.
    [wallet | squash_scope_types(Rest)];
squash_scope_types([provider, terminal | Rest]) ->
    % NOTE
    % Provider scope implies identity scope.
    [terminal | squash_scope_types(Rest)];
squash_scope_types([Type | Rest]) ->
    [Type | squash_scope_types(Rest)];
squash_scope_types([]) ->
    [].

-spec get_context_bits(limit_scope_type()) -> [context_bit()].
get_context_bits(party) ->
    [{from, owner_id}];
get_context_bits(shop) ->
    % NOTE
    % We need to preserve order between party / shop to ensure backwards compatibility.
    [{from, owner_id}, {from, shop_id}];
get_context_bits(payment_tool) ->
    [{from, payment_tool}];
get_context_bits(identity) ->
    [{prefix, <<"identity">>}, {from, identity_id}];
get_context_bits(wallet) ->
    [{prefix, <<"wallet">>}, {from, identity_id}, {from, wallet_id}];
get_context_bits(provider) ->
    [{prefix, <<"provider">>}, {from, provider_id}];
get_context_bits(terminal) ->
    [{prefix, <<"terminal">>}, {from, provider_id}, {from, terminal_id}];
get_context_bits(payer_contact_email) ->
    [{prefix, <<"payer_contact_email">>}, {from, payer_contact_email}].

-spec extract_context_bit(context_bit(), context_type(), lim_context()) ->
    {ok, binary()} | {error, lim_context:context_error()}.
extract_context_bit({prefix, Prefix}, _ContextType, _LimitContext) ->
    {ok, Prefix};
extract_context_bit({from, payment_tool}, ContextType, LimitContext) ->
    case lim_context:get_value(ContextType, payment_tool, LimitContext) of
        {ok, {bank_card, #{token := Token, exp_date := {Month, Year}}}} ->
            {ok, mk_scope_component([Token, Month, Year])};
        {ok, {bank_card, #{token := Token, exp_date := undefined}}} ->
            {ok, mk_scope_component([Token, <<"undefined">>])};
        {ok, {digital_wallet, #{id := ID, service := Service}}} ->
            {ok, mk_scope_component([<<"DW">>, Service, ID])};
        {error, _} = Error ->
            Error
    end;
extract_context_bit({from, ValueName}, ContextType, LimitContext) ->
    lim_context:get_value(ContextType, ValueName, LimitContext).

mk_scope_component(Fragments) ->
    lim_string:join($/, Fragments).

%%% Machinery callbacks

-spec init(args([event()]), machine(), handler_args(), handler_opts()) -> result().
init(Events, _Machine, _HandlerArgs, _HandlerOpts) ->
    #{
        events => emit_events(Events)
    }.

-spec process_call(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_call(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(call).

-spec process_timeout(machine(), handler_args(), handler_opts()) -> no_return().
process_timeout(_Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(timeout).

-spec process_repair(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_repair(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(repair).

%%% Internal functions

emit_events(Events) ->
    emit_timestamped_events(Events, lim_time:machinery_now()).

emit_timestamped_events(Events, Ts) ->
    [{ev, Ts, Body} || Body <- Events].

collapse(#{history := History}) ->
    lists:foldl(fun(Ev, St) -> apply_event(Ev, St) end, undefined, History).

-spec get_backend(woody_context()) -> machinery_mg_backend:backend().
get_backend(WoodyCtx) ->
    lim_utils:get_backend(?NS, WoodyCtx).

-spec not_implemented(any()) -> no_return().
not_implemented(What) ->
    erlang:error({not_implemented, What}).

%%

%%

-spec apply_event(machinery:event(timestamped_event(event())), lim_maybe:maybe(config())) -> config().
apply_event({_ID, _Ts, {ev, _EvTs, Event}}, Config) ->
    apply_event_(Event, Config).

-spec apply_event_(event(), lim_maybe:maybe(config())) -> config().
apply_event_({created, Config}, undefined) ->
    Config.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include_lib("limiter_proto/include/limproto_context_payproc_thrift.hrl").
-include_lib("limiter_proto/include/limproto_context_withdrawal_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_wthd_domain_thrift.hrl").
-include_lib("limiter_proto/include/limproto_base_thrift.hrl").

-spec test() -> _.

-spec check_sign_prefix_test() -> _.

check_sign_prefix_test() ->
    ?assertEqual(<<"past">>, mk_sign_prefix(-10)),
    ?assertEqual(<<"future">>, mk_sign_prefix(0)),
    ?assertEqual(<<"future">>, mk_sign_prefix(10)).

-spec check_calculate_day_time_range_test() -> _.
check_calculate_day_time_range_test() ->
    StartedAt1 = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(
        #{lower => <<"2000-01-01T00:00:00Z">>, upper => <<"2000-01-02T00:00:00Z">>},
        calculate_calendar_time_range(day, <<"2000-01-01T02:00:00Z">>, StartedAt1)
    ),
    ?assertEqual(
        #{lower => <<"1999-12-31T00:00:00Z">>, upper => <<"2000-01-01T00:00:00Z">>},
        calculate_calendar_time_range(day, <<"1999-12-31T02:00:00Z">>, StartedAt1)
    ),
    ?assertEqual(
        #{lower => <<"2000-01-10T00:00:00Z">>, upper => <<"2000-01-11T00:00:00Z">>},
        calculate_calendar_time_range(day, <<"2000-01-10T02:00:00Z">>, StartedAt1)
    ),
    ?assertEqual(
        #{lower => <<"1999-12-31T03:00:00Z">>, upper => <<"2000-01-01T03:00:00Z">>},
        calculate_calendar_time_range(day, <<"2000-01-01T02:00:00Z">>, <<"2000-01-01T03:00:00Z">>)
    ).

-spec check_calculate_week_time_range_test() -> _.
check_calculate_week_time_range_test() ->
    StartedAt = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(
        #{lower => <<"2000-01-01T00:00:00Z">>, upper => <<"2000-01-08T00:00:00Z">>},
        calculate_calendar_time_range(week, <<"2000-01-01T02:00:00Z">>, StartedAt)
    ),
    ?assertEqual(
        #{lower => <<"1999-12-25T00:00:00Z">>, upper => <<"2000-01-01T00:00:00Z">>},
        calculate_calendar_time_range(week, <<"1999-12-31T02:00:00Z">>, StartedAt)
    ),
    ?assertEqual(
        #{lower => <<"2000-09-30T00:00:00Z">>, upper => <<"2000-10-07T00:00:00Z">>},
        calculate_calendar_time_range(week, <<"2000-10-03T02:00:00Z">>, StartedAt)
    ),
    ?assertEqual(
        #{lower => <<"1999-12-25T03:00:00Z">>, upper => <<"2000-01-01T03:00:00Z">>},
        calculate_calendar_time_range(week, <<"2000-01-01T02:00:00Z">>, <<"2000-01-01T03:00:00Z">>)
    ).

-spec check_calculate_month_time_range_test() -> _.
check_calculate_month_time_range_test() ->
    StartedAt = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(
        #{lower => <<"2000-01-01T00:00:00Z">>, upper => <<"2000-02-01T00:00:00Z">>},
        calculate_calendar_time_range(month, <<"2000-01-01T02:00:00Z">>, StartedAt)
    ),
    ?assertEqual(
        #{lower => <<"1999-12-01T00:00:00Z">>, upper => <<"2000-01-01T00:00:00Z">>},
        calculate_calendar_time_range(month, <<"1999-12-31T02:00:00Z">>, StartedAt)
    ),
    ?assertEqual(
        #{lower => <<"2000-10-01T00:00:00Z">>, upper => <<"2000-11-01T00:00:00Z">>},
        calculate_calendar_time_range(month, <<"2000-10-03T02:00:00Z">>, StartedAt)
    ),
    ?assertEqual(
        #{lower => <<"1999-12-01T03:00:00Z">>, upper => <<"2000-01-01T03:00:00Z">>},
        calculate_calendar_time_range(month, <<"2000-01-01T02:00:00Z">>, <<"2000-01-01T03:00:00Z">>)
    ).

-spec check_calculate_year_time_range_test() -> _.
check_calculate_year_time_range_test() ->
    StartedAt = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(
        #{lower => <<"2000-01-01T00:00:00Z">>, upper => <<"2001-01-01T00:00:00Z">>},
        calculate_calendar_time_range(year, <<"2000-01-01T02:00:00Z">>, StartedAt)
    ),
    ?assertEqual(
        #{lower => <<"1999-01-01T00:00:00Z">>, upper => <<"2000-01-01T00:00:00Z">>},
        calculate_calendar_time_range(year, <<"1999-12-31T02:00:00Z">>, StartedAt)
    ),
    ?assertEqual(
        #{lower => <<"2010-01-01T00:00:00Z">>, upper => <<"2011-01-01T00:00:00Z">>},
        calculate_calendar_time_range(year, <<"2010-10-03T02:00:00Z">>, StartedAt)
    ),
    ?assertEqual(
        #{lower => <<"1999-01-01T03:00:00Z">>, upper => <<"2000-01-01T03:00:00Z">>},
        calculate_calendar_time_range(year, <<"2000-01-01T02:00:00Z">>, <<"2000-01-01T03:00:00Z">>)
    ).

-spec check_calculate_day_shard_id_test() -> _.
check_calculate_day_shard_id_test() ->
    StartedAt1 = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(<<"future/day/0">>, calculate_calendar_shard_id(day, <<"2000-01-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/day/2">>, calculate_calendar_shard_id(day, <<"2000-01-03T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"past/day/1">>, calculate_calendar_shard_id(day, <<"1999-12-31T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/day/1">>, calculate_calendar_shard_id(day, <<"2000-01-02T23:59:59Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/day/1">>, calculate_calendar_shard_id(day, <<"2000-01-04T00:00:00Z">>, StartedAt1, 2)),
    ?assertEqual(<<"future/day/366">>, calculate_calendar_shard_id(day, <<"2001-01-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/day/12">>, calculate_calendar_shard_id(day, <<"2001-01-01T00:00:00Z">>, StartedAt1, 30)),
    StartedAt2 = <<"2000-01-01T03:00:00Z">>,
    ?assertEqual(<<"past/day/1">>, calculate_calendar_shard_id(day, <<"2000-01-01T00:00:00Z">>, StartedAt2, 1)),
    ?assertEqual(<<"future/day/1">>, calculate_calendar_shard_id(day, <<"2000-01-03T00:00:00Z">>, StartedAt2, 1)).

-spec check_calculate_week_shard_id_test() -> _.
check_calculate_week_shard_id_test() ->
    StartedAt1 = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(<<"future/week/0">>, calculate_calendar_shard_id(week, <<"2000-01-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"past/week/1">>, calculate_calendar_shard_id(week, <<"1999-12-31T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/week/1">>, calculate_calendar_shard_id(week, <<"2000-01-08T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/week/1">>, calculate_calendar_shard_id(week, <<"2000-01-15T00:00:00Z">>, StartedAt1, 2)),
    ?assertEqual(<<"future/week/52">>, calculate_calendar_shard_id(week, <<"2001-01-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/week/13">>, calculate_calendar_shard_id(week, <<"2001-01-01T00:00:00Z">>, StartedAt1, 4)),
    StartedAt2 = <<"2000-01-02T03:00:00Z">>,
    ?assertEqual(<<"past/week/1">>, calculate_calendar_shard_id(week, <<"2000-01-02T00:00:00Z">>, StartedAt2, 1)),
    ?assertEqual(<<"future/week/0">>, calculate_calendar_shard_id(week, <<"2000-01-09T00:00:00Z">>, StartedAt2, 1)).

-spec check_calculate_month_shard_id_test() -> _.
check_calculate_month_shard_id_test() ->
    StartedAt1 = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(<<"future/month/0">>, calculate_calendar_shard_id(month, <<"2000-01-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"past/month/1">>, calculate_calendar_shard_id(month, <<"1999-12-31T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/month/1">>, calculate_calendar_shard_id(month, <<"2000-02-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/month/1">>, calculate_calendar_shard_id(month, <<"2000-03-01T00:00:00Z">>, StartedAt1, 2)),
    ?assertEqual(<<"future/month/12">>, calculate_calendar_shard_id(month, <<"2001-01-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/month/1">>, calculate_calendar_shard_id(month, <<"2001-01-01T00:00:00Z">>, StartedAt1, 12)),
    StartedAt2 = <<"2000-01-02T03:00:00Z">>,
    ?assertEqual(<<"past/month/1">>, calculate_calendar_shard_id(month, <<"2000-01-02T00:00:00Z">>, StartedAt2, 1)),
    ?assertEqual(<<"future/month/0">>, calculate_calendar_shard_id(month, <<"2000-02-02T00:00:00Z">>, StartedAt2, 1)).

-spec check_calculate_year_shard_id_test() -> _.
check_calculate_year_shard_id_test() ->
    StartedAt1 = <<"2000-01-01T00:00:00Z">>,
    ?assertEqual(<<"future/year/0">>, calculate_calendar_shard_id(year, <<"2000-01-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"past/year/1">>, calculate_calendar_shard_id(year, <<"1999-12-31T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/year/1">>, calculate_calendar_shard_id(year, <<"2001-01-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/year/1">>, calculate_calendar_shard_id(year, <<"2003-01-01T00:00:00Z">>, StartedAt1, 2)),
    ?assertEqual(<<"future/year/10">>, calculate_calendar_shard_id(year, <<"2010-01-01T00:00:00Z">>, StartedAt1, 1)),
    ?assertEqual(<<"future/year/2">>, calculate_calendar_shard_id(year, <<"2020-01-01T00:00:00Z">>, StartedAt1, 10)),
    StartedAt2 = <<"2000-01-02T03:00:00Z">>,
    ?assertEqual(<<"past/year/1">>, calculate_calendar_shard_id(year, <<"2000-01-01T00:00:00Z">>, StartedAt2, 1)),
    ?assertEqual(<<"future/year/0">>, calculate_calendar_shard_id(year, <<"2001-01-01T00:00:00Z">>, StartedAt2, 1)).

-define(PAYPROC_CTX_INVOICE(Invoice, Payment, Route), #limiter_LimitContext{
    payment_processing = #context_payproc_Context{
        op = {invoice_payment, #context_payproc_OperationInvoicePayment{}},
        invoice = #context_payproc_Invoice{
            invoice = Invoice,
            payment = #context_payproc_InvoicePayment{
                payment = Payment,
                route = Route
            }
        }
    }
}).

-define(PAYPROC_CTX_INVOICE(Invoice),
    ?PAYPROC_CTX_INVOICE(
        Invoice,
        ?PAYMENT(?PAYER(?PAYMENT_TOOL)),
        ?ROUTE(22, 2)
    )
).

-define(INVOICE(OwnerID, ShopID), #domain_Invoice{
    id = <<"ID">>,
    owner_id = OwnerID,
    shop_id = ShopID,
    created_at = <<"2000-02-02T12:12:12Z">>,
    status = {unpaid, #domain_InvoiceUnpaid{}},
    details = #domain_InvoiceDetails{product = <<>>},
    due = <<"2222-02-02T12:12:12Z">>,
    cost = #domain_Cash{amount = 42, currency = #domain_CurrencyRef{symbolic_code = <<"CNY">>}}
}).

-define(PAYMENT(Payer), #domain_InvoicePayment{
    id = <<"ID">>,
    created_at = <<"2000-02-02T12:12:12Z">>,
    status = {pending, #domain_InvoicePaymentPending{}},
    cost = #domain_Cash{amount = 42, currency = #domain_CurrencyRef{symbolic_code = <<"CNY">>}},
    domain_revision = 1,
    flow = {instant, #domain_InvoicePaymentFlowInstant{}},
    payer = Payer
}).

-define(PAYER(PaymentTool),
    {payment_resource, #domain_PaymentResourcePayer{
        resource = #domain_DisposablePaymentResource{payment_tool = PaymentTool},
        contact_info = #domain_ContactInfo{email = <<"email">>}
    }}
).

-define(PAYMENT_TOOL,
    {bank_card, #domain_BankCard{
        token = <<"token">>,
        bin = <<"****">>,
        last_digits = <<"last_digits">>,
        exp_date = #domain_BankCardExpDate{month = 2, year = 2022}
    }}
).

-define(WITHDRAWAL_CTX(Withdrawal, WalletID, Route), #limiter_LimitContext{
    withdrawal_processing = #context_withdrawal_Context{
        op = {withdrawal, #context_withdrawal_OperationWithdrawal{}},
        withdrawal = #context_withdrawal_Withdrawal{
            withdrawal = Withdrawal,
            wallet_id = WalletID,
            route = Route
        }
    }
}).

-define(WITHDRAWAL(OwnerID, IdentityID, PaymentTool), #wthd_domain_Withdrawal{
    destination = PaymentTool,
    sender = #wthd_domain_Identity{id = IdentityID, owner_id = OwnerID},
    created_at = <<"2000-02-02T12:12:12Z">>,
    body = #domain_Cash{amount = 42, currency = #domain_CurrencyRef{symbolic_code = <<"CNY">>}}
}).

-define(ROUTE(ProviderID, TerminalID), #base_Route{
    provider = #domain_ProviderRef{id = ProviderID},
    terminal = #domain_TerminalRef{id = TerminalID}
}).

-spec global_scope_empty_prefix_test() -> _.
global_scope_empty_prefix_test() ->
    Context = #{context => ?PAYPROC_CTX_INVOICE(?INVOICE(<<"OWNER">>, <<"SHOP">>))},
    ?assertEqual({ok, <<>>}, mk_scope_prefix_impl(ordsets:new(), payment_processing, Context)).

-spec preserve_scope_prefix_order_test_() -> [_TestGen].
preserve_scope_prefix_order_test_() ->
    Context = #{context => ?PAYPROC_CTX_INVOICE(?INVOICE(<<"OWNER">>, <<"SHOP">>))},
    [
        ?_assertEqual(
            {ok, <<"/OWNER/SHOP">>},
            mk_scope_prefix_impl(ordsets:from_list([shop, party]), payment_processing, Context)
        ),
        ?_assertEqual(
            {ok, <<"/OWNER/SHOP">>},
            mk_scope_prefix_impl(ordsets:from_list([party, shop]), payment_processing, Context)
        ),
        ?_assertEqual(
            {ok, <<"/OWNER/SHOP">>},
            mk_scope_prefix_impl(ordsets:from_list([shop]), payment_processing, Context)
        )
    ].

-spec prefix_content_test_() -> [_TestGen].
prefix_content_test_() ->
    Context = #{
        context => ?PAYPROC_CTX_INVOICE(
            ?INVOICE(<<"OWNER">>, <<"SHOP">>),
            ?PAYMENT(?PAYER(?PAYMENT_TOOL)),
            ?ROUTE(22, 2)
        )
    },
    WithdrawalContext = #{
        context => ?WITHDRAWAL_CTX(
            ?WITHDRAWAL(<<"OWNER">>, <<"IDENTITY">>, ?PAYMENT_TOOL),
            <<"WALLET">>,
            ?ROUTE(22, 2)
        )
    },
    [
        ?_assertEqual(
            {ok, <<"/terminal/22/2">>},
            mk_scope_prefix_impl(ordsets:from_list([terminal, provider]), payment_processing, Context)
        ),
        ?_assertEqual(
            {ok, <<"/terminal/22/2">>},
            mk_scope_prefix_impl(ordsets:from_list([provider, terminal]), payment_processing, Context)
        ),
        ?_assertEqual(
            {ok, <<"/OWNER/wallet/IDENTITY/WALLET">>},
            mk_scope_prefix_impl(ordsets:from_list([wallet, identity, party]), withdrawal_processing, WithdrawalContext)
        ),
        ?_assertEqual(
            {ok, <<"/token/2/2022/payer_contact_email/email">>},
            mk_scope_prefix_impl(ordsets:from_list([payer_contact_email, payment_tool]), payment_processing, Context)
        )
    ].

-endif.
