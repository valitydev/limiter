-module(lim_config_dmt_codec).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_limiter_config_thrift.hrl").

-import(lim_config_codec, [maybe_apply/2]).

-export([marshal_config_object/1]).

-spec marshal_config_object(map()) -> dmsl_domain_thrift:'LimitConfigObject'().
marshal_config_object(Config) ->
    #domain_LimitConfigObject{
        ref = #domain_LimitConfigRef{id = lim_config_machine:id(Config)},
        data = #limiter_config_LimitConfig{
            processor_type = lim_config_machine:processor_type(Config),
            created_at = lim_config_machine:created_at(Config),
            started_at = lim_config_machine:started_at(Config),
            shard_size = lim_config_machine:shard_size(Config),
            time_range_type = marshal_time_range_type(lim_config_machine:time_range_type(Config)),
            context_type = marshal_context_type(lim_config_machine:context_type(Config)),
            type = maybe_apply(lim_config_machine:type(Config), fun marshal_type/1),
            scopes = maybe_apply(lim_config_machine:scope(Config), fun marshal_scope/1),
            description = lim_config_machine:description(Config),
            op_behaviour = maybe_apply(lim_config_machine:op_behaviour(Config), fun marshal_op_behaviour/1)
        }
    }.

%%

marshal_op_behaviour(OpBehaviour) ->
    PaymentRefund = maps:get(invoice_payment_refund, OpBehaviour, undefined),
    #limiter_config_OperationLimitBehaviour{
        invoice_payment_refund = maybe_apply(PaymentRefund, fun marshal_behaviour/1)
    }.

marshal_behaviour(subtraction) ->
    {subtraction, #limiter_config_Subtraction{}};
marshal_behaviour(addition) ->
    {addition, #limiter_config_Addition{}}.

marshal_time_range_type({calendar, CalendarType}) ->
    {calendar, marshal_calendar_time_range_type(CalendarType)};
marshal_time_range_type({interval, Amount}) ->
    {interval, #limiter_config_TimeRangeTypeInterval{amount = Amount}}.

marshal_calendar_time_range_type(day) ->
    {day, #limiter_config_TimeRangeTypeCalendarDay{}};
marshal_calendar_time_range_type(week) ->
    {week, #limiter_config_TimeRangeTypeCalendarWeek{}};
marshal_calendar_time_range_type(month) ->
    {month, #limiter_config_TimeRangeTypeCalendarMonth{}};
marshal_calendar_time_range_type(year) ->
    {year, #limiter_config_TimeRangeTypeCalendarYear{}}.

marshal_context_type(payment_processing) ->
    {payment_processing, #limiter_config_LimitContextTypePaymentProcessing{}};
marshal_context_type(withdrawal_processing) ->
    {withdrawal_processing, #limiter_config_LimitContextTypeWithdrawalProcessing{}}.

marshal_type({turnover, Metric}) ->
    {turnover, #limiter_config_LimitTypeTurnover{
        metric = marshal_turnover_metric(Metric)
    }}.

marshal_turnover_metric(number) ->
    {number, #limiter_config_LimitTurnoverNumber{}};
marshal_turnover_metric({amount, Currency}) ->
    {amount, #limiter_config_LimitTurnoverAmount{currency = Currency}}.

marshal_scope(Types) ->
    {multi, ordsets:from_list(lists:map(fun marshal_scope_type/1, ordsets:to_list(Types)))}.

marshal_scope_type(party) ->
    {party, #limiter_config_LimitScopeEmptyDetails{}};
marshal_scope_type(shop) ->
    {shop, #limiter_config_LimitScopeEmptyDetails{}};
marshal_scope_type(wallet) ->
    {wallet, #limiter_config_LimitScopeEmptyDetails{}};
marshal_scope_type(identity) ->
    {identity, #limiter_config_LimitScopeEmptyDetails{}};
marshal_scope_type(payment_tool) ->
    {payment_tool, #limiter_config_LimitScopeEmptyDetails{}};
marshal_scope_type(provider) ->
    {provider, #limiter_config_LimitScopeEmptyDetails{}};
marshal_scope_type(terminal) ->
    {terminal, #limiter_config_LimitScopeEmptyDetails{}};
marshal_scope_type(payer_contact_email) ->
    {payer_contact_email, #limiter_config_LimitScopeEmptyDetails{}}.
