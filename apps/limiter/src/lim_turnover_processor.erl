-module(lim_turnover_processor).

-include_lib("limiter_proto/include/limproto_limiter_thrift.hrl").

-behaviour(lim_config_machine).

-export([get_change/4]).

-export([get_limit/3]).
-export([hold/3]).
-export([commit/3]).
-export([rollback/3]).

-type lim_context() :: lim_context:t().
-type lim_id() :: lim_config_machine:lim_id().
-type lim_change() :: lim_config_machine:lim_change().
-type limit() :: lim_config_machine:limit().
-type config() :: lim_config_machine:config().
-type amount() :: integer().

-type forbidden_operation_amount_error() :: #{
    type := positive | negative,
    partial := amount(),
    full := amount()
}.

-type get_limit_error() :: {range, notfound}.
-type get_change_error() ::
    lim_rates:conversion_error()
    | lim_context:operation_context_not_supported_error()
    | lim_turnover_metric:invalid_operation_currency_error().

-type hold_error() ::
    lim_rates:conversion_error()
    | lim_accounting:invalid_request_error()
    | lim_turnover_metric:invalid_operation_currency_error()
    | lim_context:operation_context_not_supported_error()
    | lim_context:unsupported_error({payment_tool, atom()}).

-type commit_error() ::
    {forbidden_operation_amount, forbidden_operation_amount_error()}
    | lim_rates:conversion_error()
    | lim_accounting:invalid_request_error().

-type rollback_error() ::
    lim_rates:conversion_error()
    | lim_accounting:invalid_request_error().

-export_type([get_change_error/0]).
-export_type([get_limit_error/0]).
-export_type([hold_error/0]).
-export_type([commit_error/0]).
-export_type([rollback_error/0]).

-import(lim_pipeline, [do/1, unwrap/1]).

-spec get_change(lim_turnover_metric:stage(), lim_change(), config(), lim_context()) ->
    {ok, lim_liminator:limit_change()} | {error, get_change_error()}.
get_change(Stage, #limiter_LimitChange{id = LimitID, version = Version}, Config, LimitContext) ->
    do(fun() ->
        LimitRangeID = unwrap(compute_limit_range_id(LimitID, Version, Config, LimitContext)),
        Metric = unwrap(compute_metric(Stage, Config, LimitContext)),
        lim_liminator:construct_change(LimitID, LimitRangeID, Metric)
    end).

-spec get_limit(lim_id(), config(), lim_context()) -> {ok, limit()} | {error, get_limit_error()}.
get_limit(LimitID, Config, LimitContext) ->
    do(fun() ->
        {LimitRangeID, TimeRange} = unwrap(compute_limit_time_range_location(LimitID, Config, LimitContext)),
        Amount = find_range_balance_amount(LimitRangeID, TimeRange, LimitContext),
        #limiter_Limit{
            id = LimitRangeID,
            amount = Amount,
            creation_time = lim_config_machine:created_at(Config),
            description = lim_config_machine:description(Config)
        }
    end).

find_range_balance_amount(LimitRangeID, TimeRange, LimitContext) ->
    case lim_range_machine:get_range_balance(LimitRangeID, TimeRange, LimitContext) of
        {ok, #{max_available_amount := Amount}} ->
            Amount;
        {error, notfound} ->
            0
    end.

-spec hold(lim_change(), config(), lim_context()) -> ok | {error, hold_error()}.
hold(LimitChange = #limiter_LimitChange{id = LimitID}, Config, LimitContext) ->
    do(fun() ->
        TimeRangeAccount = unwrap(ensure_limit_time_range(LimitID, Config, LimitContext)),
        Metric = unwrap(compute_metric(hold, Config, LimitContext)),
        Posting = lim_posting:new(TimeRangeAccount, Metric, currency(Config)),
        unwrap(lim_accounting:hold(construct_plan_id(LimitChange), {1, [Posting]}, LimitContext))
    end).

-spec commit(lim_change(), config(), lim_context()) -> ok | {error, commit_error()}.
commit(LimitChange = #limiter_LimitChange{id = LimitID}, Config, LimitContext) ->
    do(fun() ->
        TimeRangeAccount = unwrap(ensure_limit_time_range(LimitID, Config, LimitContext)),
        PlanID = construct_plan_id(LimitChange),
        Operations = construct_commit_plan(TimeRangeAccount, Config, LimitContext),
        ok = lists:foreach(
            fun
                ({hold, Batch}) ->
                    % NOTE
                    % This operation **can** fail with `InvalidRequest` when the plan is already
                    % committed, yet we knowingly ignore any them. Instead we rely on the fact that
                    % accounter guarantees us that it commits **only** when submitted plan consists
                    % of exactly the same set of batches which were held before.
                    lim_accounting:hold(PlanID, Batch, LimitContext);
                ({commit, Batches}) ->
                    unwrap(lim_accounting:commit(PlanID, Batches, LimitContext));
                ({rollback, Batches}) ->
                    unwrap(lim_accounting:rollback(PlanID, Batches, LimitContext))
            end,
            Operations
        )
    end).

-spec rollback(lim_change(), config(), lim_context()) -> ok | {error, rollback_error()}.
rollback(LimitChange = #limiter_LimitChange{id = LimitID}, Config, LimitContext) ->
    do(fun() ->
        TimeRangeAccount = unwrap(ensure_limit_time_range(LimitID, Config, LimitContext)),
        Metric = unwrap(compute_metric(hold, Config, LimitContext)),
        Posting = lim_posting:new(TimeRangeAccount, Metric, currency(Config)),
        unwrap(lim_accounting:rollback(construct_plan_id(LimitChange), [{1, [Posting]}], LimitContext))
    end).

compute_limit_range_id(LimitID, Version, Config, LimitContext) ->
    do(fun() ->
        Timestamp = unwrap(get_timestamp(Config, LimitContext)),
        unwrap(construct_range_id(Timestamp, LimitID, Version, Config, LimitContext))
    end).

compute_limit_time_range_location(LimitID, Config, LimitContext) ->
    do(fun() ->
        Timestamp = unwrap(get_timestamp(Config, LimitContext)),
        %% TODO: pass version from limit change here
        LimitRangeID = unwrap(compute_limit_range_id(LimitID, 0, Config, LimitContext)),
        TimeRange = lim_config_machine:calculate_time_range(Timestamp, Config),
        {LimitRangeID, TimeRange}
    end).

ensure_limit_time_range(LimitID, Config, LimitContext) ->
    do(fun() ->
        Timestamp = unwrap(get_timestamp(Config, LimitContext)),
        {LimitRangeID, TimeRange} = unwrap(compute_limit_time_range_location(LimitID, Config, LimitContext)),
        CreateParams = genlib_map:compact(#{
            id => LimitRangeID,
            type => lim_config_machine:time_range_type(Config),
            created_at => Timestamp,
            currency => currency(Config)
        }),
        unwrap(lim_range_machine:ensure_exists(CreateParams, TimeRange, LimitContext))
    end).

get_timestamp(Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_value(ContextType, created_at, LimitContext).

construct_plan_id(#limiter_LimitChange{change_id = ChangeID}) ->
    % DISCUSS
    ChangeID.

construct_range_id(Timestamp, LimitID, Version, Config, LimitContext) ->
    BinaryVersion = genlib:to_binary(Version),
    case lim_config_machine:mk_scope_prefix(Config, LimitContext) of
        {ok, Prefix} ->
            ShardID = lim_config_machine:calculate_shard_id(Timestamp, Config),
            {ok, <<LimitID/binary, "/", BinaryVersion/binary, Prefix/binary, "/", ShardID/binary>>};
        {error, _} = Error ->
            Error
    end.

construct_commit_plan(TimeRangeAccount, Config, LimitContext) ->
    MetricHold = unwrap(compute_metric(hold, Config, LimitContext)),
    MetricCommit = unwrap(compute_metric(commit, Config, LimitContext)),
    construct_commit_postings(TimeRangeAccount, MetricHold, MetricCommit, Config).

construct_commit_postings(TimeRangeAccount, MetricHold, MetricCommit, Config) ->
    OriginalHoldPosting = lim_posting:new(TimeRangeAccount, MetricHold, currency(Config)),
    case MetricCommit of
        MetricHold ->
            % Commit-time metric is equal to hold-time metric
            [{commit, [{1, [OriginalHoldPosting]}]}];
        0 ->
            % Commit-time metric is 0, this is rollback
            [{rollback, [{1, [OriginalHoldPosting]}]}];
        _MetricPartial ->
            % Partial body is less than full body
            ok = unwrap(validate_metric(MetricCommit, MetricHold)),
            ReverseHoldPosting = lim_posting:reverse(OriginalHoldPosting),
            PartialHoldPosting = lim_posting:new(TimeRangeAccount, MetricCommit, currency(Config)),
            PartialBatch = [ReverseHoldPosting, PartialHoldPosting],
            [
                {hold, {2, PartialBatch}},
                {commit, [
                    {1, [OriginalHoldPosting]},
                    {2, PartialBatch}
                ]}
            ]
    end.

compute_metric(Stage, Config, LimitContext) ->
    {turnover, Metric} = lim_config_machine:type(Config),
    lim_turnover_metric:compute(Metric, Stage, Config, LimitContext).

currency(#{type := {turnover, {amount, Currency}}}) ->
    Currency;
currency(#{type := {turnover, number}}) ->
    lim_accounting:noncurrency().

validate_metric(MetricCommit, MetricHold) when MetricHold > 0 ->
    case MetricCommit < MetricHold of
        true ->
            ok;
        false ->
            {error,
                {forbidden_operation_amount, #{
                    type => positive,
                    partial => MetricCommit,
                    full => MetricHold
                }}}
    end;
validate_metric(MetricCommit, MetricHold) when MetricHold < 0 ->
    case MetricCommit > MetricHold of
        true ->
            ok;
        false ->
            {error,
                {forbidden_operation_amount, #{
                    type => negative,
                    partial => MetricCommit,
                    full => MetricHold
                }}}
    end.
