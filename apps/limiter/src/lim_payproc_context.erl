-module(lim_payproc_context).

-include_lib("limiter_proto/include/limproto_context_payproc_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-behaviour(lim_context).
-export([get_operation/1]).
-export([get_value/2]).

-type context() :: limproto_context_payproc_thrift:'Context'().

-type operation() ::
    invoice
    | invoice_adjustment
    | invoice_payment
    | invoice_payment_adjustment
    | invoice_payment_refund
    | invoice_payment_chargeback.

-export_type([operation/0]).
-export_type([context/0]).

%%

-spec get_operation(context()) -> {ok, operation()} | {error, notfound}.
get_operation(#context_payproc_Context{op = {Operation, _}}) ->
    {ok, Operation};
get_operation(#context_payproc_Context{op = undefined}) ->
    {error, notfound}.

-spec get_value(atom(), context()) ->
    {ok, term()}
    | {error, notfound}
    | {error, {unsupported, _}}.
get_value(ValueName, Context) ->
    case get_operation(Context) of
        {ok, Operation} ->
            get_value(ValueName, Operation, Context);
        {error, _} = Error ->
            Error
    end.

get_value(owner_id, _Operation, CtxInvoice) ->
    get_owner_id(CtxInvoice);
get_value(shop_id, _Operation, CtxInvoice) ->
    get_shop_id(CtxInvoice);
get_value(created_at, Operation, CtxInvoice) ->
    get_created_at(Operation, CtxInvoice);
get_value(cost, Operation, CtxInvoice) ->
    get_cost(Operation, CtxInvoice);
get_value(capture_cost, Operation, CtxInvoice) ->
    get_capture_cost(Operation, CtxInvoice);
get_value(payment_tool, Operation, CtxInvoice) ->
    get_payment_tool(Operation, CtxInvoice).

%%

-define(INVOICE(V), #context_payproc_Context{
    invoice = #context_payproc_Invoice{
        invoice = V = #domain_Invoice{}
    }
}).

-define(INVOICE_ADJUSTMENT(V), #context_payproc_Context{
    invoice = #context_payproc_Invoice{
        adjustment = V = #domain_InvoiceAdjustment{}
    }
}).

-define(INVOICE_PAYMENT(V), #context_payproc_Context{
    invoice = #context_payproc_Invoice{
        payment = #context_payproc_InvoicePayment{payment = V = #domain_InvoicePayment{}}
    }
}).

-define(INVOICE_PAYMENT_ADJUSTMENT(V), #context_payproc_Context{
    invoice = #context_payproc_Invoice{
        payment = #context_payproc_InvoicePayment{adjustment = V = #domain_InvoicePaymentAdjustment{}}
    }
}).

-define(INVOICE_PAYMENT_REFUND(V), #context_payproc_Context{
    invoice = #context_payproc_Invoice{
        payment = #context_payproc_InvoicePayment{refund = V = #domain_InvoicePaymentRefund{}}
    }
}).

-define(INVOICE_PAYMENT_CHARGEBACK(V), #context_payproc_Context{
    invoice = #context_payproc_Invoice{
        payment = #context_payproc_InvoicePayment{chargeback = V = #domain_InvoicePaymentChargeback{}}
    }
}).

get_owner_id(?INVOICE(Invoice)) ->
    {ok, Invoice#domain_Invoice.owner_id};
get_owner_id(_) ->
    {error, notfound}.

get_shop_id(?INVOICE(Invoice)) ->
    {ok, Invoice#domain_Invoice.shop_id};
get_shop_id(_) ->
    {error, notfound}.

get_created_at(invoice, ?INVOICE(Invoice)) ->
    {ok, Invoice#domain_Invoice.created_at};
get_created_at(invoice_adjustment, ?INVOICE_ADJUSTMENT(Adjustment)) ->
    {ok, Adjustment#domain_InvoiceAdjustment.created_at};
get_created_at(invoice_payment, ?INVOICE_PAYMENT(Payment)) ->
    {ok, Payment#domain_InvoicePayment.created_at};
get_created_at(invoice_payment_adjustment, ?INVOICE_PAYMENT_ADJUSTMENT(Adjustment)) ->
    {ok, Adjustment#domain_InvoicePaymentAdjustment.created_at};
get_created_at(invoice_payment_refund, ?INVOICE_PAYMENT_REFUND(Refund)) ->
    {ok, Refund#domain_InvoicePaymentRefund.created_at};
get_created_at(invoice_payment_chargeback, ?INVOICE_PAYMENT_CHARGEBACK(Chargeback)) ->
    {ok, Chargeback#domain_InvoicePaymentChargeback.created_at};
get_created_at(_, _CtxInvoice) ->
    {error, notfound}.

get_cost(invoice, ?INVOICE(Invoice)) ->
    lim_payproc_utils:cash(Invoice#domain_Invoice.cost);
get_cost(invoice_payment, ?INVOICE_PAYMENT(Payment)) ->
    lim_payproc_utils:cash(Payment#domain_InvoicePayment.cost);
get_cost(invoice_payment_refund, ?INVOICE_PAYMENT_REFUND(Refund)) ->
    lim_payproc_utils:cash(Refund#domain_InvoicePaymentRefund.cash);
get_cost(invoice_payment_chargeback, ?INVOICE_PAYMENT_CHARGEBACK(Chargeback)) ->
    lim_payproc_utils:cash(Chargeback#domain_InvoicePaymentChargeback.body);
get_cost(_, _CtxInvoice) ->
    {error, notfound}.

get_capture_cost(invoice_payment, ?INVOICE_PAYMENT(Payment)) ->
    get_capture_cost(Payment#domain_InvoicePayment.status);
get_capture_cost(_, _CtxInvoice) ->
    {error, notfound}.

get_capture_cost({captured, #domain_InvoicePaymentCaptured{cost = Cost}}) when Cost /= undefined ->
    lim_payproc_utils:cash(Cost);
get_capture_cost({_Status, _}) ->
    {error, notfound}.

get_payment_tool(Operation, ?INVOICE_PAYMENT(Payment)) when
    Operation == invoice_payment;
    Operation == invoice_payment_adjustment;
    Operation == invoice_payment_refund;
    Operation == invoice_payment_chargeback
->
    {_Type, Payer} = Payment#domain_InvoicePayment.payer,
    get_payer_payment_tool(Payer);
get_payment_tool(_, _CtxInvoice) ->
    {error, notfound}.

get_payer_payment_tool(#domain_PaymentResourcePayer{resource = #domain_DisposablePaymentResource{payment_tool = PT}}) ->
    lim_payproc_utils:payment_tool(PT);
get_payer_payment_tool(#domain_CustomerPayer{payment_tool = PT}) ->
    lim_payproc_utils:payment_tool(PT);
get_payer_payment_tool(#domain_RecurrentPayer{payment_tool = PT}) ->
    lim_payproc_utils:payment_tool(PT).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-define(PAYMENT_W_PAYER(Payer), #domain_InvoicePayment{
    id = <<"ID">>,
    created_at = <<"2000-02-02T12:12:12Z">>,
    status = {pending, #domain_InvoicePaymentPending{}},
    cost = #domain_Cash{
        amount = 42,
        currency = #domain_CurrencyRef{symbolic_code = <<"CNY">>}
    },
    domain_revision = 42,
    flow = {instant, #domain_InvoicePaymentFlowInstant{}},
    payer = Payer
}).

-define(CONTEXT_PAYMENT(Payment), #context_payproc_Context{
    op = {invoice_payment, #context_payproc_OperationInvoicePayment{}},
    invoice = #context_payproc_Invoice{
        payment = #context_payproc_InvoicePayment{
            payment = Payment
        }
    }
}).

-spec get_payment_tool_test_() -> [_TestGen].
get_payment_tool_test_() ->
    PaymentTool =
        {bank_card, #domain_BankCard{
            token = <<"Token">>,
            bin = <<"654321">>,
            exp_date = #domain_BankCardExpDate{month = 2, year = 2022},
            last_digits = <<"1234">>
        }},
    PaymentResourcePayer =
        {payment_resource, #domain_PaymentResourcePayer{
            resource = #domain_DisposablePaymentResource{payment_tool = PaymentTool},
            contact_info = #domain_ContactInfo{}
        }},
    CustomerPayer =
        {customer, #domain_CustomerPayer{
            customer_id = <<"customer_id">>,
            customer_binding_id = <<"customer_binding_id">>,
            rec_payment_tool_id = <<"rec_payment_tool_id">>,
            payment_tool = PaymentTool,
            contact_info = #domain_ContactInfo{}
        }},
    RecurrentPayer =
        {recurrent, #domain_RecurrentPayer{
            payment_tool = PaymentTool,
            recurrent_parent = #domain_RecurrentParentPayment{
                invoice_id = <<"invoice_id">>,
                payment_id = <<"payment_id">>
            },
            contact_info = #domain_ContactInfo{}
        }},
    ExpectedValue = {bank_card, #{token => <<"Token">>, exp_date => {2, 2022}}},
    [
        ?_assertEqual(
            {ok, ExpectedValue},
            get_value(payment_tool, ?CONTEXT_PAYMENT(?PAYMENT_W_PAYER(PaymentResourcePayer)))
        ),
        ?_assertEqual(
            {ok, ExpectedValue},
            get_value(payment_tool, ?CONTEXT_PAYMENT(?PAYMENT_W_PAYER(CustomerPayer)))
        ),
        ?_assertEqual(
            {ok, ExpectedValue},
            get_value(payment_tool, ?CONTEXT_PAYMENT(?PAYMENT_W_PAYER(RecurrentPayer)))
        )
    ].

-spec get_payment_tool_unsupported_test_() -> _TestGen.
get_payment_tool_unsupported_test_() ->
    Payer =
        {recurrent, #domain_RecurrentPayer{
            payment_tool = {payment_terminal, #domain_PaymentTerminal{}},
            recurrent_parent = #domain_RecurrentParentPayment{
                invoice_id = <<"invoice_id">>,
                payment_id = <<"payment_id">>
            },
            contact_info = #domain_ContactInfo{}
        }},
    ?_assertEqual(
        {error, {unsupported, {payment_tool, payment_terminal}}},
        get_value(payment_tool, ?CONTEXT_PAYMENT(?PAYMENT_W_PAYER(Payer)))
    ).

-endif.
