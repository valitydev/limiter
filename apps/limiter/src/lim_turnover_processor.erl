-module(lim_turnover_processor).

-include_lib("limiter_proto/include/lim_limiter_thrift.hrl").

-behaviour(lim_config_machine).

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

-type hold_error() ::
    lim_body:get_body_error()
    | lim_accounting:invalid_request_error().

-type commit_error() ::
    {forbidden_operation_amount, forbidden_operation_amount_error()}
    | lim_body:get_body_error()
    | lim_accounting:invalid_request_error().

-type rollback_error() ::
    lim_body:get_body_error()
    | lim_accounting:invalid_request_error().

-export_type([get_limit_error/0]).
-export_type([hold_error/0]).
-export_type([commit_error/0]).
-export_type([rollback_error/0]).

-import(lim_pipeline, [do/1, unwrap/1, unwrap/2]).

-spec get_limit(lim_id(), config(), lim_context()) -> {ok, limit()} | {error, get_limit_error()}.
get_limit(LimitID, Config, LimitContext) ->
    do(fun() ->
        {LimitRangeID, TimeRange} = compute_limit_time_range_location(LimitID, Config, LimitContext),
        #{max_available_amount := Amount} =
            unwrap(range, lim_range_machine:get_range_balance(LimitRangeID, TimeRange, LimitContext)),
        #limiter_Limit{
            id = LimitRangeID,
            amount = Amount,
            creation_time = lim_config_machine:created_at(Config),
            description = lim_config_machine:description(Config)
        }
    end).

-spec hold(lim_change(), config(), lim_context()) -> ok | {error, hold_error()}.
hold(LimitChange = #limiter_LimitChange{id = LimitID}, Config, LimitContext) ->
    do(fun() ->
        TimeRangeAccount = ensure_limit_time_range(LimitID, Config, LimitContext),
        Body = unwrap(lim_body:get_body(full, Config, LimitContext)),
        Posting = lim_posting:new(TimeRangeAccount, Body),
        unwrap(lim_accounting:hold(construct_plan_id(LimitChange), {1, [Posting]}, LimitContext))
    end).

-spec commit(lim_change(), config(), lim_context()) -> ok | {error, commit_error()}.
commit(LimitChange = #limiter_LimitChange{id = LimitID}, Config, LimitContext) ->
    do(fun() ->
        TimeRangeAccount = ensure_limit_time_range(LimitID, Config, LimitContext),
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
        TimeRangeAccount = ensure_limit_time_range(LimitID, Config, LimitContext),
        Body = unwrap(lim_body:get_body(full, Config, LimitContext)),
        Posting = lim_posting:new(TimeRangeAccount, Body),
        unwrap(lim_accounting:rollback(construct_plan_id(LimitChange), [{1, [Posting]}], LimitContext))
    end).

compute_limit_time_range_location(LimitID, Config, LimitContext) ->
    {ok, Timestamp} = lim_context:get_from_context(payment_processing, created_at, LimitContext),
    LimitRangeID = construct_range_id(LimitID, Timestamp, Config, LimitContext),
    TimeRange = lim_config_machine:calculate_time_range(Timestamp, Config),
    {LimitRangeID, TimeRange}.

ensure_limit_time_range(LimitID, Config, LimitContext) ->
    {ok, Timestamp} = lim_context:get_from_context(payment_processing, created_at, LimitContext),
    {LimitRangeID, TimeRange} = compute_limit_time_range_location(LimitID, Config, LimitContext),
    CreateParams = genlib_map:compact(#{
        id => LimitRangeID,
        type => lim_config_machine:time_range_type(Config),
        created_at => Timestamp,
        currency => lim_config_machine:currency(Config)
    }),
    unwrap(lim_range_machine:ensure_exists(CreateParams, TimeRange, LimitContext)).

construct_plan_id(#limiter_LimitChange{change_id = ChangeID}) ->
    % DISCUSS
    ChangeID.

construct_range_id(LimitID, Timestamp, Config, LimitContext) ->
    Prefix = lim_config_machine:mk_scope_prefix(Config, LimitContext),
    ShardID = lim_config_machine:calculate_shard_id(Timestamp, Config),
    <<LimitID/binary, Prefix/binary, "/", ShardID/binary>>.

construct_commit_plan(TimeRangeAccount, Config, LimitContext) ->
    Body = unwrap(lim_body:get_body(full, Config, LimitContext)),
    MaybePartialBody = lim_body:get_body(partial, Config, LimitContext),
    construct_commit_postings(TimeRangeAccount, Body, MaybePartialBody).

construct_commit_postings(TimeRangeAccount, Full, MaybePartialBody) ->
    OriginalHoldPosting = lim_posting:new(TimeRangeAccount, Full),
    case determine_commit_intent(MaybePartialBody, Full) of
        commit ->
            [{commit, [{1, [OriginalHoldPosting]}]}];
        rollback ->
            [{rollback, [{1, [OriginalHoldPosting]}]}];
        {commit, Partial} ->
            % Partial body is less than full body
            ok = unwrap(assert_partial_body(Partial, Full)),
            ReverseHoldPosting = lim_posting:reverse(OriginalHoldPosting),
            PartialHoldPosting = lim_posting:new(TimeRangeAccount, Partial),
            PartialBatch = [ReverseHoldPosting, PartialHoldPosting],
            [
                {hold, {2, PartialBatch}},
                {commit, [
                    {1, [OriginalHoldPosting]},
                    {2, PartialBatch}
                ]}
            ]
    end.

determine_commit_intent({error, notfound}, _FullBody) ->
    % No partial body specified
    commit;
determine_commit_intent({ok, FullBody}, FullBody) ->
    % Partial body is equal to full body
    commit;
determine_commit_intent({ok, {amount, 0}}, _FullBody) ->
    % Partial body is 0, this is rollback
    rollback;
determine_commit_intent({ok, {cash, #{amount := 0}}}, _FullBody) ->
    % Partial body is 0, this is rollback
    rollback;
determine_commit_intent({ok, Partial}, _FullBody) ->
    {commit, Partial}.

assert_partial_body(
    {cash, #{amount := Partial, currency := Currency}},
    {cash, #{amount := Full, currency := Currency}}
) ->
    compare_amount(Partial, Full, Currency);
assert_partial_body(
    {cash, #{amount := Partial, currency := PartialCurrency}},
    {cash, #{amount := Full, currency := FullCurrency}}
) ->
    erlang:error({invalid_partial_cash, {Partial, PartialCurrency}, {Full, FullCurrency}}).

compare_amount(Partial, Full, Currency) when Full > 0 ->
    case Partial < Full of
        true ->
            ok;
        false ->
            {error,
                {forbidden_operation_amount,
                    genlib_map:compact(#{
                        type => positive,
                        partial => Partial,
                        full => Full,
                        currency => Currency
                    })}}
    end;
compare_amount(Partial, Full, Currency) when Full < 0 ->
    case Partial > Full of
        true ->
            ok;
        false ->
            {error,
                {forbidden_operation_amount,
                    genlib_map:compact(#{
                        type => negative,
                        partial => Partial,
                        full => Full,
                        currency => Currency
                    })}}
    end.
