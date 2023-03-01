-module(lim_config_dmt_codec).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_limiter_config_thrift.hrl").

-export([unmarshal_config_object/1]).

-spec unmarshal_config_object(dmsl_domain_thrift:'LimitConfigObject'()) -> map().
unmarshal_config_object(#domain_LimitConfigObject{
    ref = #domain_LimitConfigRef{id = ID},
    data = #limiter_config_LimitConfig{
        processor_type = ProcessorType,
        created_at = CreatedAt,
        started_at = StartedAt,
        shard_size = ShardSize,
        time_range_type = TimeRangeType,
        context_type = ContextType,
        type = Type,
        scopes = Scopes,
        description = Description,
        op_behaviour = OpBehaviour
    }
}) ->
    genlib_map:compact(#{
        id => ID,
        processor_type => ProcessorType,
        created_at => lim_time:from_rfc3339(CreatedAt),
        started_at => StartedAt,
        shard_size => ShardSize,
        time_range_type => unmarshal_time_range_type(TimeRangeType),
        context_type => unmarshal_context_type(ContextType),
        type => maybe_apply(Type, fun unmarshal_type/1),
        scope => maybe_apply(Scopes, fun unmarshal_scope/1),
        description => Description,
        op_behaviour => maybe_apply(OpBehaviour, fun unmarshal_op_behaviour/1)
    }).

unmarshal_op_behaviour(OpBehaviour) ->
    #limiter_config_OperationLimitBehaviour{invoice_payment_refund = Refund} = OpBehaviour,
    genlib_map:compact(#{invoice_payment_refund => maybe_apply(Refund, fun unmarshal_behaviour/1)}).

unmarshal_behaviour({subtraction, #limiter_config_Subtraction{}}) ->
    subtraction;
unmarshal_behaviour({addition, #limiter_config_Addition{}}) ->
    addition.

unmarshal_time_range_type({calendar, CalendarType}) ->
    {calendar, unmarshal_calendar_time_range_type(CalendarType)};
unmarshal_time_range_type({interval, #limiter_config_TimeRangeTypeInterval{amount = Amount}}) ->
    {interval, Amount}.

unmarshal_calendar_time_range_type({day, _}) ->
    day;
unmarshal_calendar_time_range_type({week, _}) ->
    week;
unmarshal_calendar_time_range_type({month, _}) ->
    month;
unmarshal_calendar_time_range_type({year, _}) ->
    year.

unmarshal_context_type({payment_processing, #limiter_config_LimitContextTypePaymentProcessing{}}) ->
    payment_processing;
unmarshal_context_type({withdrawal_processing, #limiter_config_LimitContextTypeWithdrawalProcessing{}}) ->
    withdrawal_processing.

unmarshal_type({turnover, #limiter_config_LimitTypeTurnover{metric = Metric}}) ->
    {turnover, maybe_apply(Metric, fun unmarshal_turnover_metric/1, number)}.

unmarshal_turnover_metric({number, _}) ->
    number;
unmarshal_turnover_metric({amount, #limiter_config_LimitTurnoverAmount{currency = Currency}}) ->
    {amount, Currency}.

unmarshal_scope(Types) ->
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

maybe_apply(undefined, _) ->
    undefined;
maybe_apply(Value, Fun) ->
    Fun(Value).

maybe_apply(undefined, _, Default) ->
    Default;
maybe_apply(Value, Fun, _Default) ->
    Fun(Value).

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec marshal_unmarshal_config_object_test() -> _.
marshal_unmarshal_config_object_test() ->
    CreatedAt = lim_time:now(),
    Config = #{
        id => <<"id">>,
        processor_type => <<"type">>,
        created_at => CreatedAt,
        started_at => <<"2000-01-01T00:00:00Z">>,
        shard_size => 7,
        time_range_type => {calendar, day},
        context_type => payment_processing,
        type => {turnover, number},
        scope => ordsets:from_list([party, shop]),
        description => <<"description">>
    },
    Object = #domain_LimitConfigObject{
        ref = #domain_LimitConfigRef{id = <<"id">>},
        data = #limiter_config_LimitConfig{
            processor_type = <<"type">>,
            created_at = lim_time:to_rfc3339(CreatedAt),
            started_at = <<"2000-01-01T00:00:00Z">>,
            shard_size = 7,
            time_range_type = {calendar, {day, #limiter_config_TimeRangeTypeCalendarDay{}}},
            context_type = {payment_processing, #limiter_config_LimitContextTypePaymentProcessing{}},
            type =
                {turnover, #limiter_config_LimitTypeTurnover{metric = {number, #limiter_config_LimitTurnoverNumber{}}}},
            scopes = ordsets:from_list([
                {'party', #limiter_config_LimitScopeEmptyDetails{}}, {'shop', #limiter_config_LimitScopeEmptyDetails{}}
            ]),
            description = <<"description">>
        }
    },
    ?assertEqual(Config, unmarshal_config_object(Object)).

-endif.
