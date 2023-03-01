-module(lim_config_codec).

-include_lib("limiter_proto/include/limproto_config_thrift.hrl").
-include_lib("limiter_proto/include/limproto_timerange_thrift.hrl").

-export([marshal/2]).
-export([unmarshal/2]).
-export([marshal_config/1]).
-export([unmarshal_op_behaviour/1]).
-export([unmarshal_params/1]).
-export([maybe_apply/2]).

%% Types

-type type_name() :: atom() | {list, atom()} | {set, atom()}.

-type encoded_value() :: encoded_value(any()).
-type encoded_value(T) :: T.

-type decoded_value() :: decoded_value(any()).
-type decoded_value(T) :: T.

-spec maybe_apply(T, fun((T) -> U)) -> U | undefined.
maybe_apply(undefined, _) ->
    undefined;
maybe_apply(Value, Fun) ->
    Fun(Value).

-spec maybe_apply(T, fun((T) -> U), Default) -> U | Default.
maybe_apply(undefined, _, Default) ->
    Default;
maybe_apply(Value, Fun, _Default) ->
    Fun(Value).

%% API

-spec marshal(type_name(), decoded_value()) -> encoded_value().
marshal(timestamped_change, {ev, Timestamp, Change}) ->
    #config_TimestampedChange{
        change = marshal_change(Change),
        occured_at = marshal_timestamp(Timestamp)
    }.

marshal_timestamp({DateTime, USec}) ->
    DateTimeinSeconds = genlib_time:daytime_to_unixtime(DateTime),
    {TimeinUnit, Unit} =
        case USec of
            0 ->
                {DateTimeinSeconds, second};
            USec ->
                MicroSec = erlang:convert_time_unit(DateTimeinSeconds, second, microsecond),
                {MicroSec + USec, microsecond}
        end,
    genlib_rfc3339:format_relaxed(TimeinUnit, Unit).

marshal_change({created, Config}) ->
    {created, #config_CreatedChange{limit_config = marshal_config(Config)}}.

-spec marshal_config(decoded_value()) -> encoded_value().
marshal_config(Config) ->
    #config_LimitConfig{
        id = lim_config_machine:id(Config),
        processor_type = lim_config_machine:processor_type(Config),
        description = lim_config_machine:description(Config),
        created_at = lim_config_machine:created_at(Config),
        started_at = lim_config_machine:started_at(Config),
        shard_size = lim_config_machine:shard_size(Config),
        time_range_type = marshal_time_range_type(lim_config_machine:time_range_type(Config)),
        context_type = marshal_context_type(lim_config_machine:context_type(Config)),
        type = maybe_apply(lim_config_machine:type(Config), fun marshal_type/1),
        scope = maybe_apply(lim_config_machine:scope(Config), fun marshal_scope/1),
        op_behaviour = maybe_apply(lim_config_machine:op_behaviour(Config), fun marshal_op_behaviour/1)
    }.

marshal_op_behaviour(OpBehaviour) ->
    PaymentRefund = maps:get(invoice_payment_refund, OpBehaviour, undefined),
    #config_OperationLimitBehaviour{
        invoice_payment_refund = maybe_apply(PaymentRefund, fun marshal_behaviour/1)
    }.

marshal_behaviour(subtraction) ->
    {subtraction, #config_Subtraction{}};
marshal_behaviour(addition) ->
    {addition, #config_Addition{}}.

marshal_time_range_type({calendar, CalendarType}) ->
    {calendar, marshal_calendar_time_range_type(CalendarType)};
marshal_time_range_type({interval, Amount}) ->
    {interval, #timerange_TimeRangeTypeInterval{amount = Amount}}.

marshal_calendar_time_range_type(day) ->
    {day, #timerange_TimeRangeTypeCalendarDay{}};
marshal_calendar_time_range_type(week) ->
    {week, #timerange_TimeRangeTypeCalendarWeek{}};
marshal_calendar_time_range_type(month) ->
    {month, #timerange_TimeRangeTypeCalendarMonth{}};
marshal_calendar_time_range_type(year) ->
    {year, #timerange_TimeRangeTypeCalendarYear{}}.

marshal_context_type(payment_processing) ->
    {payment_processing, #config_LimitContextTypePaymentProcessing{}};
marshal_context_type(withdrawal_processing) ->
    {withdrawal_processing, #config_LimitContextTypeWithdrawalProcessing{}}.

marshal_type({turnover, Metric}) ->
    {turnover, #config_LimitTypeTurnover{
        metric = marshal_turnover_metric(Metric)
    }}.

marshal_turnover_metric(number) ->
    {number, #config_LimitTurnoverNumber{}};
marshal_turnover_metric({amount, Currency}) ->
    {amount, #config_LimitTurnoverAmount{currency = Currency}}.

marshal_scope(Types) ->
    {multi, ordsets:from_list(lists:map(fun marshal_scope_type/1, ordsets:to_list(Types)))}.

marshal_scope_type(party) ->
    {party, #config_LimitScopeEmptyDetails{}};
marshal_scope_type(shop) ->
    {shop, #config_LimitScopeEmptyDetails{}};
marshal_scope_type(wallet) ->
    {wallet, #config_LimitScopeEmptyDetails{}};
marshal_scope_type(identity) ->
    {identity, #config_LimitScopeEmptyDetails{}};
marshal_scope_type(payment_tool) ->
    {payment_tool, #config_LimitScopeEmptyDetails{}};
marshal_scope_type(provider) ->
    {provider, #config_LimitScopeEmptyDetails{}};
marshal_scope_type(terminal) ->
    {terminal, #config_LimitScopeEmptyDetails{}};
marshal_scope_type(payer_contact_email) ->
    {payer_contact_email, #config_LimitScopeEmptyDetails{}}.

%%

-spec unmarshal(type_name(), encoded_value()) -> decoded_value().
unmarshal(timestamped_change, TimestampedChange) ->
    Timestamp = unmarshal_timestamp(TimestampedChange#config_TimestampedChange.occured_at),
    Change = unmarshal_change(TimestampedChange#config_TimestampedChange.change),
    {ev, Timestamp, Change}.

unmarshal_timestamp(Timestamp) when is_binary(Timestamp) ->
    try
        MicroSeconds = genlib_rfc3339:parse(Timestamp, microsecond),
        case genlib_rfc3339:is_utc(Timestamp) of
            false ->
                erlang:error({bad_timestamp, not_utc}, [Timestamp]);
            true ->
                USec = MicroSeconds rem 1000000,
                DateTime = calendar:system_time_to_universal_time(MicroSeconds, microsecond),
                {DateTime, USec}
        end
    catch
        error:Error:St ->
            erlang:raise(error, {bad_timestamp, Timestamp, Error}, St)
    end.

-spec unmarshal_params(encoded_value()) -> decoded_value().
unmarshal_params(#config_LimitConfigParams{
    id = ID,
    description = Description,
    started_at = StartedAt,
    shard_size = ShardSize,
    time_range_type = TimeRangeType,
    context_type = ContextType,
    type = Type,
    scope = Scope,
    op_behaviour = OpBehaviour
}) ->
    genlib_map:compact(#{
        id => ID,
        started_at => StartedAt,
        shard_size => ShardSize,
        time_range_type => unmarshal_time_range_type(TimeRangeType),
        context_type => unmarshal_context_type(ContextType),
        type => maybe_apply(Type, fun unmarshal_type/1),
        scope => maybe_apply(Scope, fun unmarshal_scope/1),
        description => Description,
        op_behaviour => maybe_apply(OpBehaviour, fun unmarshal_op_behaviour/1)
    }).

unmarshal_change({created, #config_CreatedChange{limit_config = Config}}) ->
    {created, unmarshal_config(Config)}.

-spec unmarshal_config(limproto_config_thrift:'LimitConfig'()) -> map().
unmarshal_config(#config_LimitConfig{
    id = ID,
    processor_type = ProcessorType,
    description = Description,
    created_at = CreatedAt,
    started_at = StartedAt,
    shard_size = ShardSize,
    time_range_type = TimeRangeType,
    context_type = ContextType,
    type = TypeIn,
    scope = Scope,
    op_behaviour = OpBehaviour,
    body_type_deprecated = BodyTypeIn
}) ->
    Type = maybe_apply(TypeIn, fun unmarshal_type/1),
    BodyType = maybe_apply(BodyTypeIn, fun unmarshal_body_type_deprecated/1),
    genlib_map:compact(#{
        id => ID,
        processor_type => ProcessorType,
        created_at => lim_time:from_rfc3339(CreatedAt),
        started_at => StartedAt,
        shard_size => ShardSize,
        time_range_type => unmarshal_time_range_type(TimeRangeType),
        context_type => unmarshal_context_type(ContextType),
        type => derive_type(Type, BodyType),
        scope => maybe_apply(Scope, fun unmarshal_scope/1),
        description => Description,
        op_behaviour => maybe_apply(OpBehaviour, fun unmarshal_op_behaviour/1)
    }).

derive_type(Type, undefined) ->
    % NOTE
    % Current protocol disallows configuring (deprecated) body type, thus we trust limit type.
    Type;
derive_type({turnover, _}, {cash, Currency}) ->
    % NOTE
    % Treating limits with configured (deprecated) body type as turnover limits with amount metric.
    {turnover, {amount, Currency}};
derive_type(undefined, {cash, Currency}) ->
    {turnover, {amount, Currency}}.

-spec unmarshal_op_behaviour(encoded_value()) -> decoded_value().
unmarshal_op_behaviour(OpBehaviour) ->
    #config_OperationLimitBehaviour{
        invoice_payment_refund = Refund
    } = OpBehaviour,
    genlib_map:compact(#{
        invoice_payment_refund => maybe_apply(Refund, fun unmarshal_behaviour/1)
    }).

unmarshal_behaviour({subtraction, #config_Subtraction{}}) ->
    subtraction;
unmarshal_behaviour({addition, #config_Addition{}}) ->
    addition.

-spec unmarshal_body_type_deprecated(encoded_value()) -> decoded_value().
unmarshal_body_type_deprecated({cash, #config_LimitBodyTypeCash{currency = Currency}}) ->
    {cash, Currency}.

unmarshal_time_range_type({calendar, CalendarType}) ->
    {calendar, unmarshal_calendar_time_range_type(CalendarType)};
unmarshal_time_range_type({interval, #timerange_TimeRangeTypeInterval{amount = Amount}}) ->
    {interval, Amount}.

unmarshal_calendar_time_range_type({day, _}) ->
    day;
unmarshal_calendar_time_range_type({week, _}) ->
    week;
unmarshal_calendar_time_range_type({month, _}) ->
    month;
unmarshal_calendar_time_range_type({year, _}) ->
    year.

unmarshal_context_type({payment_processing, #config_LimitContextTypePaymentProcessing{}}) ->
    payment_processing;
unmarshal_context_type({withdrawal_processing, #config_LimitContextTypeWithdrawalProcessing{}}) ->
    withdrawal_processing.

unmarshal_type({turnover, #config_LimitTypeTurnover{metric = Metric}}) ->
    {turnover, maybe_apply(Metric, fun unmarshal_turnover_metric/1, number)}.

unmarshal_turnover_metric({number, _}) ->
    number;
unmarshal_turnover_metric({amount, #config_LimitTurnoverAmount{currency = Currency}}) ->
    {amount, Currency}.

unmarshal_scope({single, Type}) ->
    ordsets:from_list([unmarshal_scope_type(Type)]);
unmarshal_scope({multi, Types}) ->
    ordsets:from_list(lists:map(fun unmarshal_scope_type/1, ordsets:to_list(Types))).

unmarshal_scope_type({party, _}) ->
    party;
unmarshal_scope_type({shop, _}) ->
    shop;
unmarshal_scope_type({wallet, _}) ->
    wallet;
unmarshal_scope_type({identity, _}) ->
    identity;
unmarshal_scope_type({payment_tool, _}) ->
    payment_tool;
unmarshal_scope_type({provider, _}) ->
    provider;
unmarshal_scope_type({terminal, _}) ->
    terminal;
unmarshal_scope_type({payer_contact_email, _}) ->
    payer_contact_email.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec marshal_unmarshal_created_test() -> _.
marshal_unmarshal_created_test() ->
    Created =
        {created, #{
            id => <<"id">>,
            processor_type => <<"type">>,
            created_at => lim_time:now(),
            started_at => <<"2000-01-01T00:00:00Z">>,
            shard_size => 7,
            time_range_type => {calendar, day},
            context_type => payment_processing,
            type => {turnover, number},
            scope => ordsets:from_list([party, shop]),
            description => <<"description">>
        }},
    Event = {ev, lim_time:machinery_now(), Created},
    ?assertEqual(Event, unmarshal(timestamped_change, marshal(timestamped_change, Event))).

-spec unmarshal_created_w_deprecated_body_type_test_() -> [_TestGen].
unmarshal_created_w_deprecated_body_type_test_() ->
    Now = lim_time:now(),
    Config = #config_LimitConfig{
        id = <<"ID">>,
        processor_type = <<"TurnoverProcessor">>,
        created_at = lim_time:to_rfc3339(Now),
        started_at = <<"2000-01-01T00:00:00Z">>,
        shard_size = 42,
        time_range_type = {calendar, {day, #timerange_TimeRangeTypeCalendarDay{}}},
        context_type = {payment_processing, #config_LimitContextTypePaymentProcessing{}},
        body_type_deprecated = {cash, #config_LimitBodyTypeCash{currency = <<"☭☭☭"/utf8>>}}
    },
    [
        ?_assertMatch(
            {created, #{
                id := <<"ID">>,
                created_at := Now,
                type := {turnover, {amount, <<"☭☭☭"/utf8>>}}
            }},
            unmarshal_change(
                {created, #config_CreatedChange{
                    limit_config = Config#config_LimitConfig{
                        type = undefined
                    }
                }}
            )
        ),
        ?_assertMatch(
            {created, #{
                id := <<"ID">>,
                created_at := Now,
                type := {turnover, {amount, <<"☭☭☭"/utf8>>}}
            }},
            unmarshal_change(
                {created, #config_CreatedChange{
                    limit_config = Config#config_LimitConfig{
                        type = {turnover, #config_LimitTypeTurnover{}}
                    }
                }}
            )
        )
    ].

-endif.
