-module(lim_config_codec).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_limiter_config_thrift.hrl").

-export([unmarshal/2]).

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

-spec unmarshal(type_name(), encoded_value()) -> decoded_value().
unmarshal('LimitConfigObject', #domain_LimitConfigObject{
    ref = #domain_LimitConfigRef{id = ID},
    data = #limiter_config_LimitConfig{
        processor_type = ProcessorType,
        shard_size = ShardSize,
        time_range_type = TimeRangeType,
        context_type = ContextType,
        type = Type,
        scopes = Scopes,
        description = Description,
        op_behaviour = OpBehaviour,
        currency_conversion = CurrencyConversion
    }
}) ->
    genlib_map:compact(#{
        id => ID,
        processor_type => ProcessorType,
        shard_size => ShardSize,
        time_range_type => unmarshal_time_range_type(TimeRangeType),
        context_type => unmarshal_context_type(ContextType),
        type => maybe_apply(Type, fun unmarshal_type/1),
        scope => maybe_apply(Scopes, fun unmarshal_scope/1),
        description => Description,
        op_behaviour => maybe_apply(OpBehaviour, fun unmarshal_op_behaviour/1),
        currency_conversion => CurrencyConversion =/= undefined
    }).

-spec unmarshal_op_behaviour(encoded_value()) -> decoded_value().
unmarshal_op_behaviour(#limiter_config_OperationLimitBehaviour{invoice_payment_refund = Refund}) ->
    do_unmarshal_op_behaviour_refund(Refund).

do_unmarshal_op_behaviour_refund(Refund) ->
    genlib_map:compact(#{
        invoice_payment_refund => maybe_apply(Refund, fun unmarshal_behaviour/1)
    }).

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

unmarshal_scope({single, Type}) ->
    ordsets:from_list([unmarshal_scope_type(Type)]);
unmarshal_scope({multi, Types}) ->
    ordsets:from_list(lists:map(fun unmarshal_scope_type/1, ordsets:to_list(Types)));
unmarshal_scope(Types) when is_list(Types) ->
    ordsets:from_list(lists:map(fun unmarshal_scope_type/1, ordsets:to_list(Types))).

unmarshal_scope_type({party, _}) ->
    party;
unmarshal_scope_type({shop, _}) ->
    shop;
unmarshal_scope_type({wallet, _}) ->
    wallet;
unmarshal_scope_type({payment_tool, _}) ->
    payment_tool;
unmarshal_scope_type({provider, _}) ->
    provider;
unmarshal_scope_type({terminal, _}) ->
    terminal;
unmarshal_scope_type({payer_contact_email, _}) ->
    payer_contact_email;
unmarshal_scope_type({destination_field, #limiter_config_LimitScopeDestinationFieldDetails{field_path = FieldPath}}) ->
    %% Domain config variant clause
    {destination_field, FieldPath};
unmarshal_scope_type({sender, _}) ->
    sender;
unmarshal_scope_type({receiver, _}) ->
    receiver.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec unmarshal_config_object_test() -> _.
unmarshal_config_object_test() ->
    Config = #{
        id => <<"id">>,
        processor_type => <<"type">>,
        shard_size => 7,
        time_range_type => {calendar, day},
        context_type => payment_processing,
        type => {turnover, number},
        scope => ordsets:from_list([party, shop, {destination_field, [<<"path">>, <<"to">>, <<"field">>]}]),
        description => <<"description">>,
        currency_conversion => true
    },
    Object = #domain_LimitConfigObject{
        ref = #domain_LimitConfigRef{id = <<"id">>},
        data = #limiter_config_LimitConfig{
            processor_type = <<"type">>,
            shard_size = 7,
            time_range_type = {calendar, {day, #limiter_config_TimeRangeTypeCalendarDay{}}},
            context_type = {payment_processing, #limiter_config_LimitContextTypePaymentProcessing{}},
            type =
                {turnover, #limiter_config_LimitTypeTurnover{metric = {number, #limiter_config_LimitTurnoverNumber{}}}},
            scopes = ordsets:from_list([
                {'party', #limiter_config_LimitScopeEmptyDetails{}},
                {'shop', #limiter_config_LimitScopeEmptyDetails{}},
                {'destination_field', #limiter_config_LimitScopeDestinationFieldDetails{
                    field_path = [<<"path">>, <<"to">>, <<"field">>]
                }}
            ]),
            description = <<"description">>,
            currency_conversion = #limiter_config_CurrencyConversion{}
        }
    },
    ?assertEqual(Config, unmarshal('LimitConfigObject', Object)).

-endif.
