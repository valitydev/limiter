-module(lim_turnover_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("lim_ct_helper.hrl").

-include_lib("limiter_proto/include/lim_configurator_thrift.hrl").
-include_lib("xrates_proto/include/xrates_rate_thrift.hrl").

-export([all/0]).

-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([commit_with_default_exchange/1]).
-export([partial_commit_with_exchange/1]).
-export([commit_with_exchange/1]).
-export([get_rate/1]).
-export([get_limit_ok/1]).
-export([get_limit_notfound/1]).
-export([hold_ok/1]).
-export([commit_ok/1]).
-export([rollback_ok/1]).
-export([refund_ok/1]).
-export([get_config_ok/1]).
-export([commit_inexistent_hold_fails/1]).
-export([partial_commit_inexistent_hold_fails/1]).
-export([commit_multirange_limit_ok/1]).

-export([commit_processes_idempotently/1]).
-export([full_commit_processes_idempotently/1]).
-export([partial_commit_processes_idempotently/1]).
-export([rollback_processes_idempotently/1]).

-export([commit_amount_ok/1]).
-export([rollback_amount_ok/1]).
-export([partial_commit_amount_counts_as_single_op/1]).

-type group_name() :: atom().
-type test_case_name() :: atom().

-define(RATE_SOURCE_ID, <<"dummy_source_id">>).

%% tests descriptions

-spec all() -> [{group, group_name()}].
all() ->
    [
        {group, default},
        {group, cashless},
        {group, idempotency}
    ].

-spec groups() -> [{atom(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [], [
            commit_with_default_exchange,
            partial_commit_with_exchange,
            commit_with_exchange,
            get_rate,
            get_limit_ok,
            get_limit_notfound,
            hold_ok,
            commit_ok,
            rollback_ok,
            get_config_ok,
            refund_ok,
            commit_inexistent_hold_fails,
            partial_commit_inexistent_hold_fails,
            commit_multirange_limit_ok
        ]},
        {cashless, [parallel], [
            commit_amount_ok,
            rollback_amount_ok,
            partial_commit_amount_counts_as_single_op
        ]},
        {idempotency, [parallel], [
            commit_processes_idempotently,
            full_commit_processes_idempotently,
            partial_commit_processes_idempotently,
            rollback_processes_idempotently
        ]}
    ].

-type config() :: [{atom(), any()}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    % dbg:tracer(), dbg:p(all, c),
    % dbg:tpl({lim_handler, '_', '_'}, x),
    Apps =
        genlib_app:start_application_with(limiter, [
            {service_clients, #{
                accounter => #{
                    url => <<"http://shumway:8022/accounter">>
                },
                automaton => #{
                    url => <<"http://machinegun:8022/v1/automaton">>
                },
                xrates => #{
                    url => <<"http://xrates:8022/xrates">>
                }
            }},
            {exchange_factors, #{
                <<"DEFAULT">> => {1, 1},
                <<"USD">> => {105, 100},
                <<"EUR">> => {12, 10}
            }}
        ]),
    [{apps, Apps}] ++ Config.

-spec end_per_suite(config()) -> _.
end_per_suite(Config) ->
    genlib_app:test_application_stop(?config(apps, Config)).

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    [
        {id, gen_unique_id(Name)},
        {client, lim_client:new()},
        {test_sup, lim_mock:start_mocked_service_sup()}
        | C
    ].

-spec end_per_testcase(test_case_name(), config()) -> ok.
end_per_testcase(_Name, C) ->
    _ = lim_mock:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%

-define(CHANGE_ID, 42).
-define(LIMIT_CHANGE(ID), ?LIMIT_CHANGE(ID, ?CHANGE_ID)).
-define(LIMIT_CHANGE(ID, ChangeID), #limiter_LimitChange{id = ID, change_id = gen_change_id(ID, ChangeID)}).

-spec commit_with_default_exchange(config()) -> _.
commit_with_default_exchange(C) ->
    Rational = #base_Rational{p = 1000000, q = 100},
    _ = mock_exchange(Rational, C),
    ID = configure_limit(?time_range_month(), ?global(), C),
    Context = #limiter_context_LimitContext{
        payment_processing = #limiter_context_ContextPaymentProcessing{
            op = {invoice, #limiter_context_PaymentProcessingOperationInvoice{}},
            invoice = #limiter_context_Invoice{
                created_at = <<"2000-01-01T00:00:00Z">>,
                cost = #limiter_base_Cash{
                    amount = 10000,
                    currency = #limiter_base_CurrencyRef{symbolic_code = <<"SOME_CURRENCY">>}
                }
            }
        }
    },
    {ok, {vector, _}} = hold_and_commit(?LIMIT_CHANGE(ID), Context, ?config(client, C)),
    {ok, #limiter_Limit{amount = 10000}} = lim_client:get(ID, Context, ?config(client, C)).

-spec partial_commit_with_exchange(config()) -> _.
partial_commit_with_exchange(C) ->
    Rational = #base_Rational{p = 800000, q = 100},
    _ = mock_exchange(Rational, C),
    ID = configure_limit(?time_range_month(), ?global(), C),
    Context = #limiter_context_LimitContext{
        payment_processing = #limiter_context_ContextPaymentProcessing{
            op = {invoice_payment, #limiter_context_PaymentProcessingOperationInvoicePayment{}},
            invoice = #limiter_context_Invoice{
                effective_payment = #limiter_context_InvoicePayment{
                    created_at = <<"2000-01-01T00:00:00Z">>,
                    cost = #limiter_base_Cash{
                        amount = 10000,
                        currency = #limiter_base_CurrencyRef{symbolic_code = <<"USD">>}
                    },
                    capture_cost = #limiter_base_Cash{
                        amount = 8000,
                        currency = #limiter_base_CurrencyRef{symbolic_code = <<"USD">>}
                    }
                }
            }
        }
    },
    {ok, {vector, _}} = hold_and_commit(?LIMIT_CHANGE(ID), Context, ?config(client, C)),
    {ok, #limiter_Limit{amount = 8400}} = lim_client:get(ID, Context, ?config(client, C)).

-spec commit_with_exchange(config()) -> _.
commit_with_exchange(C) ->
    Rational = #base_Rational{p = 1000000, q = 100},
    _ = mock_exchange(Rational, C),
    ID = configure_limit(?time_range_month(), ?global(), C),
    Context = #limiter_context_LimitContext{
        payment_processing = #limiter_context_ContextPaymentProcessing{
            op = {invoice, #limiter_context_PaymentProcessingOperationInvoice{}},
            invoice = #limiter_context_Invoice{
                created_at = <<"2000-01-01T00:00:00Z">>,
                cost = #limiter_base_Cash{
                    amount = 10000,
                    currency = #limiter_base_CurrencyRef{symbolic_code = <<"USD">>}
                }
            }
        }
    },
    {ok, {vector, _}} = hold_and_commit(?LIMIT_CHANGE(ID), Context, ?config(client, C)),
    {ok, #limiter_Limit{amount = 10500}} = lim_client:get(ID, Context, ?config(client, C)).

-spec get_rate(config()) -> _.
get_rate(C) ->
    Rational = #base_Rational{p = 10, q = 10},
    _ = mock_exchange(Rational, C),
    Request = #rate_ConversionRequest{
        source = <<"RUB">>,
        destination = <<"USD">>,
        amount = 100,
        datetime = <<"Timestamp">>
    },
    WoodyContext = woody_context:new(),
    {ok, Rational} = lim_client_woody:call(
        xrates,
        'GetConvertedAmount',
        {?RATE_SOURCE_ID, Request},
        WoodyContext
    ).

-spec get_limit_ok(config()) -> _.
get_limit_ok(C) ->
    ID = configure_limit(?time_range_month(), ?global(), C),
    Context = ?ctx_invoice(_Cost = undefined),
    ?assertMatch(
        {ok, #limiter_Limit{amount = 0}},
        lim_client:get(ID, Context, ?config(client, C))
    ).

-spec get_limit_notfound(config()) -> _.
get_limit_notfound(C) ->
    Context = ?ctx_invoice(_Cost = undefined),
    ?assertEqual(
        {exception, #limiter_LimitNotFound{}},
        lim_client:get(<<"NOSUCHLIMITID">>, Context, ?config(client, C))
    ).

-spec hold_ok(config()) -> _.
hold_ok(C) ->
    ID = configure_limit(?time_range_month(), ?global(), C),
    Context = ?ctx_invoice(?cash(10)),
    {ok, {vector, #limiter_VectorClock{}}} = lim_client:hold(?LIMIT_CHANGE(ID), Context, ?config(client, C)),
    {ok, #limiter_Limit{}} = lim_client:get(ID, Context, ?config(client, C)).

-spec commit_ok(config()) -> _.
commit_ok(C) ->
    ID = configure_limit(?time_range_month(), ?global(), C),
    Context = #limiter_context_LimitContext{
        payment_processing = #limiter_context_ContextPaymentProcessing{
            op = {invoice, #limiter_context_PaymentProcessingOperationInvoice{}},
            invoice = #limiter_context_Invoice{
                created_at = <<"2000-01-01T00:00:00Z">>,
                cost = #limiter_base_Cash{
                    amount = 10,
                    currency = #limiter_base_CurrencyRef{symbolic_code = <<"RUB">>}
                }
            }
        }
    },
    {ok, {vector, _}} = hold_and_commit(?LIMIT_CHANGE(ID), Context, ?config(client, C)),
    {ok, #limiter_Limit{}} = lim_client:get(ID, Context, ?config(client, C)).

-spec rollback_ok(config()) -> _.
rollback_ok(C) ->
    ID = configure_limit(?time_range_week(), ?global(), C),
    Context0 = ?ctx_invoice_payment(?cash(10), ?cash(10)),
    Context1 = ?ctx_invoice_payment(?cash(10), ?cash(0)),
    Change = ?LIMIT_CHANGE(ID),
    {ok, {vector, _}} = lim_client:hold(Change, Context0, ?config(client, C)),
    {ok, {vector, _}} = lim_client:commit(Change, Context1, ?config(client, C)).

-spec refund_ok(config()) -> _.
refund_ok(C) ->
    Client = ?config(client, C),
    OwnerID = <<"WWWcool Ltd">>,
    ShopID = <<"shop">>,
    ID = configure_limit(?time_range_day(), ?scope([?scope_party(), ?scope_shop()]), C),
    Context0 = ?ctx_invoice_payment(OwnerID, ShopID, ?cash(15), ?cash(15)),
    RefundContext1 = ?ctx_invoice_payment_refund(OwnerID, ShopID, ?cash(10), ?cash(10), ?cash(10)),
    {ok, {vector, _}} = hold_and_commit(?LIMIT_CHANGE(ID, <<"Payment">>), Context0, Client),
    {ok, {vector, _}} = hold_and_commit(?LIMIT_CHANGE(ID, <<"Refund">>), RefundContext1, Client),
    {ok, #limiter_Limit{} = Limit2} = lim_client:get(ID, RefundContext1, Client),
    ?assertEqual(Limit2#limiter_Limit.amount, 5).

-spec get_config_ok(config()) -> _.
get_config_ok(C) ->
    ID = configure_limit(?time_range_week(), ?global(), C),
    {ok, #limiter_config_LimitConfig{}} = lim_client:get_config(ID, ?config(client, C)).

-spec commit_inexistent_hold_fails(config()) -> _.
commit_inexistent_hold_fails(C) ->
    ID = configure_limit(?time_range_week(), ?global(), C),
    Context = ?ctx_invoice_payment(?cash(42), undefined),
    % NOTE
    % We do not expect `LimitChangeNotFound` here because we no longer reconcile with accounter
    % before requesting him to hold / commit.
    {exception, #limiter_base_InvalidRequest{}} =
        lim_client:commit(?LIMIT_CHANGE(ID), Context, ?config(client, C)).

-spec partial_commit_inexistent_hold_fails(config()) -> _.
partial_commit_inexistent_hold_fails(C) ->
    ID = configure_limit(?time_range_week(), ?global(), C),
    Context = ?ctx_invoice_payment(?cash(42), ?cash(21)),
    % NOTE
    % We do not expect `LimitChangeNotFound` here because we no longer reconcile with accounter
    % before requesting him to hold / commit.
    {exception, #limiter_base_InvalidRequest{}} =
        lim_client:commit(?LIMIT_CHANGE(ID), Context, ?config(client, C)).

-spec commit_multirange_limit_ok(config()) -> _.
commit_multirange_limit_ok(C) ->
    ID = ?config(id, C),
    Client = ?config(client, C),
    Params = #limiter_config_LimitConfigParams{
        id = ID,
        body_type = {cash, #limiter_config_LimitBodyTypeCash{currency = <<"RUB">>}},
        started_at = <<"2000-01-01T00:00:00Z">>,
        shard_size = 12,
        time_range_type = {calendar, {month, #time_range_TimeRangeTypeCalendarMonth{}}},
        context_type = {payment_processing, #limiter_config_LimitContextTypePaymentProcessing{}},
        type = {turnover, #limiter_config_LimitTypeTurnover{}},
        scope = ?scope([]),
        op_behaviour = #limiter_config_OperationLimitBehaviour{}
    },
    {ok, _LimitConfig} = lim_client:create_config(Params, Client),
    % NOTE
    % Expecting those 3 changes will be accounted in the same limit range machine.
    % We have no way to verify it here though.
    PaymentJan = #limiter_context_InvoicePayment{
        created_at = <<"2020-01-01T00:00:00Z">>,
        cost = ?cash(42)
    },
    {ok, _} = hold_and_commit(?LIMIT_CHANGE(ID, 1), ?ctx_invoice_payment(PaymentJan), Client),
    PaymentFeb = #limiter_context_InvoicePayment{
        created_at = <<"2020-02-01T00:00:00Z">>,
        cost = ?cash(43)
    },
    {ok, _} = hold_and_commit(?LIMIT_CHANGE(ID, 2), ?ctx_invoice_payment(PaymentFeb), Client),
    PaymentApr = #limiter_context_InvoicePayment{
        created_at = <<"2020-04-01T00:00:00Z">>,
        cost = ?cash(44)
    },
    {ok, _} = hold_and_commit(?LIMIT_CHANGE(ID, 3), ?ctx_invoice_payment(PaymentApr), Client),
    {ok, #limiter_Limit{amount = 42}} = lim_client:get(ID, ?ctx_invoice_payment(PaymentJan), Client),
    {ok, #limiter_Limit{amount = 43}} = lim_client:get(ID, ?ctx_invoice_payment(PaymentFeb), Client),
    {ok, #limiter_Limit{amount = 44}} = lim_client:get(ID, ?ctx_invoice_payment(PaymentApr), Client).

%%

-spec commit_processes_idempotently(config()) -> _.
commit_processes_idempotently(C) ->
    Client = ?config(client, C),
    ID = configure_limit(?time_range_week(), ?global(), C),
    Context = ?ctx_invoice_payment(?cash(42), undefined),
    Change = ?LIMIT_CHANGE(ID),
    {ok, _} = lim_client:hold(Change, Context, Client),
    {ok, _} = lim_client:hold(Change, Context, Client),
    {ok, _} = lim_client:commit(Change, Context, Client),
    {ok, Limit = #limiter_Limit{amount = 42}} = lim_client:get(ID, Context, Client),
    {ok, _} = lim_client:commit(Change, Context, Client),
    {ok, Limit} = lim_client:get(ID, Context, Client).

-spec full_commit_processes_idempotently(config()) -> _.
full_commit_processes_idempotently(C) ->
    Client = ?config(client, C),
    ID = configure_limit(?time_range_week(), ?global(), C),
    Cost = ?cash(42),
    Context = ?ctx_invoice_payment(Cost, Cost),
    Change = ?LIMIT_CHANGE(ID),
    {ok, _} = lim_client:hold(Change, Context, Client),
    {ok, _} = lim_client:hold(Change, Context, Client),
    {ok, _} = lim_client:commit(Change, Context, Client),
    {ok, Limit = #limiter_Limit{amount = 42}} = lim_client:get(ID, Context, Client),
    {ok, _} = lim_client:commit(Change, Context, Client),
    {ok, Limit} = lim_client:get(ID, Context, Client).

-spec partial_commit_processes_idempotently(config()) -> _.
partial_commit_processes_idempotently(C) ->
    Client = ?config(client, C),
    ID = configure_limit(?time_range_week(), ?global(), C),
    Context = ?ctx_invoice_payment(?cash(42), ?cash(40)),
    Change = ?LIMIT_CHANGE(ID),
    {ok, _} = lim_client:hold(Change, Context, Client),
    {ok, _} = lim_client:hold(Change, Context, Client),
    {ok, _} = lim_client:commit(Change, Context, Client),
    {ok, Limit = #limiter_Limit{amount = 40}} = lim_client:get(ID, Context, Client),
    {ok, _} = lim_client:commit(Change, Context, Client),
    {ok, Limit = #limiter_Limit{amount = 40}} = lim_client:get(ID, Context, Client).

-spec rollback_processes_idempotently(config()) -> _.
rollback_processes_idempotently(C) ->
    Client = ?config(client, C),
    ID = configure_limit(?time_range_week(), ?global(), C),
    Context = ?ctx_invoice_payment(?cash(42), ?cash(0)),
    Change = ?LIMIT_CHANGE(ID),
    {ok, _} = lim_client:hold(Change, Context, Client),
    {ok, _} = lim_client:hold(Change, Context, Client),
    {ok, _} = lim_client:commit(Change, Context, Client),
    {ok, Limit = #limiter_Limit{amount = 0}} = lim_client:get(ID, Context, Client),
    {ok, _} = lim_client:commit(Change, Context, Client),
    {ok, Limit = #limiter_Limit{amount = 0}} = lim_client:get(ID, Context, Client).

%%

-spec commit_amount_ok(config()) -> _.
commit_amount_ok(C) ->
    Client = ?config(client, C),
    ID = configure_limit(?time_range_week(), ?global(), ?body_type_amount(), C),
    Context = ?ctx_invoice_payment(?cash(10), ?cash(10)),
    {ok, LimitState0} = lim_client:get(ID, Context, Client),
    _ = hold_and_commit(?LIMIT_CHANGE(ID), Context, Client),
    {ok, LimitState1} = lim_client:get(ID, Context, Client),
    ?assertEqual(LimitState1#limiter_Limit.amount, LimitState0#limiter_Limit.amount + 1).

-spec rollback_amount_ok(config()) -> _.
rollback_amount_ok(C) ->
    Client = ?config(client, C),
    ID = configure_limit(?time_range_week(), ?global(), ?body_type_amount(), C),
    Context = ?ctx_invoice_payment(?cash(10), ?cash(10)),
    ContextRollback = ?ctx_invoice_payment(?cash(10), ?cash(0)),
    {ok, LimitState0} = lim_client:get(ID, Context, Client),
    _ = hold_and_commit(?LIMIT_CHANGE(ID), Context, ContextRollback, Client),
    {ok, LimitState1} = lim_client:get(ID, Context, Client),
    ?assertEqual(LimitState1#limiter_Limit.amount, LimitState0#limiter_Limit.amount).

-spec partial_commit_amount_counts_as_single_op(config()) -> _.
partial_commit_amount_counts_as_single_op(C) ->
    Client = ?config(client, C),
    ID = configure_limit(?time_range_week(), ?global(), ?body_type_amount(), C),
    Context = ?ctx_invoice_payment(?cash(10), ?cash(10)),
    ContextPartial = ?ctx_invoice_payment(?cash(10), ?cash(5)),
    {ok, LimitState0} = lim_client:get(ID, Context, Client),
    _ = hold_and_commit(?LIMIT_CHANGE(ID), Context, ContextPartial, Client),
    {ok, LimitState1} = lim_client:get(ID, Context, Client),
    ?assertEqual(LimitState1#limiter_Limit.amount, LimitState0#limiter_Limit.amount + 1).

%%

gen_change_id(LimitID, ChangeID) ->
    genlib:format("~s/~p", [LimitID, ChangeID]).

hold_and_commit(Change, Context, Client) ->
    hold_and_commit(Change, Context, Context, Client).

hold_and_commit(Change, Context, ContextCommit, Client) ->
    {ok, {vector, _}} = lim_client:hold(Change, Context, Client),
    {ok, {vector, _}} = lim_client:commit(Change, ContextCommit, Client).

mock_exchange(Rational, C) ->
    lim_mock:mock_services([{xrates, fun('GetConvertedAmount', _) -> {ok, Rational} end}], C).

configure_limit(TimeRange, Scope, C) ->
    configure_limit(TimeRange, Scope, ?body_type_cash(<<"RUB">>), C).

configure_limit(TimeRange, Scope, BodyType, C) ->
    ID = ?config(id, C),
    Params = #limiter_config_LimitConfigParams{
        id = ID,
        started_at = <<"2000-01-01T00:00:00Z">>,
        body_type = BodyType,
        time_range_type = TimeRange,
        shard_size = 1,
        type = ?lim_type_turnover(),
        scope = Scope,
        context_type = ?ctx_type_payproc(),
        op_behaviour = ?op_behaviour(?op_subtraction())
    },
    {ok, _LimitConfig} = lim_client:create_config(Params, ?config(client, C)),
    ID.

gen_unique_id(Prefix) ->
    genlib:format("~s/~B", [Prefix, lim_time:now()]).
