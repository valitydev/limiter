-module(lim_payproc_context).

-include_lib("limiter_proto/include/lim_limiter_payproc_context_thrift.hrl").

-export([unmarshal/1]).

-type thrift_context() :: lim_limiter_payproc_context_thrift:'Context'().

%%

-spec unmarshal(thrift_context()) -> lim_context:context().
unmarshal(#limiter_context_payproc_Context{
    op = {Operation, _},
    invoice = Invoice
}) ->
    genlib_map:compact(#{
        op => Operation,
        invoice => maybe_unmarshal(Invoice, fun unmarshal_payment_processing_invoice/1)
    }).

unmarshal_payment_processing_invoice(#limiter_context_payproc_Invoice{
    invoice = #domain_Invoice{
        id = ID,
        owner_id = OwnerID,
        shop_id = ShopID,
        cost = Cost,
        created_at = CreatedAt
    },
    payment = Payment,
    adjustment = Adjustment
}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1),
        owner_id => maybe_unmarshal(OwnerID, fun unmarshal_string/1),
        shop_id => maybe_unmarshal(ShopID, fun unmarshal_string/1),
        cost => maybe_unmarshal(Cost, fun unmarshal_cash/1),
        created_at => maybe_unmarshal(CreatedAt, fun unmarshal_string/1),
        adjustment => maybe_unmarshal(
            Adjustment,
            fun unmarshal_payment_processing_invoice_adjustment/1
        ),
        payment => maybe_unmarshal(Payment, fun unmarshal_payment_processing_invoice_payment/1)
    }).

unmarshal_payment_processing_invoice_adjustment(#domain_InvoiceAdjustment{id = ID}) ->
    #{id => unmarshal_string(ID)}.

unmarshal_payment_processing_invoice_payment(#limiter_context_payproc_InvoicePayment{
    payment = #domain_InvoicePayment{
        id = ID,
        owner_id = OwnerID,
        shop_id = ShopID,
        cost = Cost,
        created_at = CreatedAt,
        flow = Flow,
        status = Status,
        payer = Payer
    },
    adjustment = Adjustment,
    refund = EffectiveRefund,
    chargeback = EffectiveChargeback
}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1),
        owner_id => maybe_unmarshal(OwnerID, fun unmarshal_string/1),
        shop_id => maybe_unmarshal(ShopID, fun unmarshal_string/1),
        cost => maybe_unmarshal(Cost, fun unmarshal_cash/1),
        capture_cost => get_capture_cost_from_status(Status),
        created_at => maybe_unmarshal(CreatedAt, fun unmarshal_string/1),
        flow => maybe_unmarshal(Flow, fun unmarshal_payment_processing_invoice_payment_flow/1),
        payer => maybe_unmarshal(Payer, fun unmarshal_payment_processing_invoice_payment_payer/1),
        adjustment => maybe_unmarshal(
            Adjustment,
            fun unmarshal_payment_processing_invoice_payment_adjustment/1
        ),
        refund => maybe_unmarshal(EffectiveRefund, fun unmarshal_payment_processing_invoice_payment_refund/1),
        chargeback => maybe_unmarshal(
            EffectiveChargeback,
            fun unmarshal_payment_processing_invoice_payment_chargeback/1
        )
    }).

get_capture_cost_from_status({captured, #domain_InvoicePaymentCaptured{cost = Cost}}) ->
    maybe_unmarshal(Cost, fun unmarshal_cash/1);
get_capture_cost_from_status(_) ->
    undefined.

unmarshal_payment_processing_invoice_payment_flow({Flow, _}) ->
    Flow.

unmarshal_payment_processing_invoice_payment_payer({Payer, Data}) ->
    {Payer, unmarshal_payment_processing_invoice_payment_payer_data(Data)}.

unmarshal_payment_processing_invoice_payment_payer_data(#domain_PaymentResourcePayer{
    resource = #domain_DisposablePaymentResource{payment_tool = PaymentTool}
}) ->
    genlib_map:compact(#{
        payment_tool => maybe_unmarshal(PaymentTool, fun unmarshal_payment_processing_payment_tool/1)
    });
unmarshal_payment_processing_invoice_payment_payer_data(#domain_CustomerPayer{payment_tool = PaymentTool}) ->
    genlib_map:compact(#{
        payment_tool => maybe_unmarshal(PaymentTool, fun unmarshal_payment_processing_payment_tool/1)
    });
unmarshal_payment_processing_invoice_payment_payer_data(#domain_RecurrentPayer{payment_tool = PaymentTool}) ->
    genlib_map:compact(#{
        payment_tool => maybe_unmarshal(PaymentTool, fun unmarshal_payment_processing_payment_tool/1)
    }).

unmarshal_payment_processing_payment_tool({bank_card, #domain_BankCard{token = Token, exp_date = ExpDate}}) ->
    {bank_card, #{
        token => Token,
        exp_date => maybe_unmarshal(ExpDate, fun unmarshal_payment_processing_bank_card_exp_data/1)
    }};
unmarshal_payment_processing_payment_tool(_) ->
    undefined.

unmarshal_payment_processing_bank_card_exp_data(#domain_BankCardExpDate{month = Month, year = Year}) ->
    BinaryMonth = integer_to_binary(Month),
    BinaryYear = integer_to_binary(Year),
    <<BinaryMonth/binary, "/", BinaryYear/binary>>.

unmarshal_payment_processing_invoice_payment_adjustment(#domain_InvoicePaymentAdjustment{
    id = ID,
    created_at = CreatedAt
}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1),
        created_at => maybe_unmarshal(CreatedAt, fun unmarshal_string/1)
    }).

unmarshal_payment_processing_invoice_payment_refund(#domain_InvoicePaymentRefund{
    id = ID,
    cash = Cost,
    created_at = CreatedAt
}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1),
        cost => maybe_unmarshal(Cost, fun unmarshal_cash/1),
        created_at => maybe_unmarshal(CreatedAt, fun unmarshal_string/1)
    }).

unmarshal_payment_processing_invoice_payment_chargeback(#domain_InvoicePaymentChargeback{
    id = ID,
    levy = Levy,
    body = Body,
    created_at = CreatedAt
}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1),
        levy => maybe_unmarshal(Levy, fun unmarshal_cash/1),
        body => maybe_unmarshal(Body, fun unmarshal_cash/1),
        created_at => maybe_unmarshal(CreatedAt, fun unmarshal_string/1)
    }).

unmarshal_cash(#domain_Cash{amount = Amount, currency = #domain_CurrencyRef{symbolic_code = Currency}}) ->
    #{amount => Amount, currency => Currency}.

unmarshal_string(Value) ->
    Value.

maybe_unmarshal(undefined, _) ->
    undefined;
maybe_unmarshal(Value, UnmarshalFun) ->
    UnmarshalFun(Value).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec marshal_unmarshal_created_test() -> _.
marshal_unmarshal_created_test() ->
    ExpDate = #domain_BankCardExpDate{month = 2, year = 2022},
    PaymentTool = {bank_card, #domain_BankCard{token = <<"Token">>, exp_date = ExpDate}},
    Data0 = #domain_PaymentResourcePayer{
        resource = #domain_DisposablePaymentResource{payment_tool = PaymentTool},
        contact_info = #domain_ContactInfo{}
    },
    #{payment_tool := _} = unmarshal_payment_processing_invoice_payment_payer_data(Data0),
    Data1 = #domain_CustomerPayer{payment_tool = PaymentTool},
    #{payment_tool := _} = unmarshal_payment_processing_invoice_payment_payer_data(Data1),
    Data2 = #domain_RecurrentPayer{payment_tool = PaymentTool},
    #{payment_tool := _} = unmarshal_payment_processing_invoice_payment_payer_data(Data2),
    Data3 = #domain_RecurrentPayer{payment_tool = {payment_terminal, #domain_PaymentTerminal{}}},
    #{} = unmarshal_payment_processing_invoice_payment_payer_data(Data3).

-endif.
