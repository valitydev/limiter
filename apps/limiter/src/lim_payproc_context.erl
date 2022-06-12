-module(lim_payproc_context).

-include_lib("limiter_proto/include/lim_limiter_payproc_context_thrift.hrl").

-export([unmarshal/1]).

-type thrift_context() :: lim_limiter_payproc_context_thrift:'ContextPaymentProcessing'().

%%

-spec unmarshal(thrift_context()) -> lim_context:context().
unmarshal(#limiter_context_payproc_ContextPaymentProcessing{
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
    effective_payment = EffectivePayment,
    effective_adjustment = EffectiveAdjustment
}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1),
        owner_id => maybe_unmarshal(OwnerID, fun unmarshal_string/1),
        shop_id => maybe_unmarshal(ShopID, fun unmarshal_string/1),
        cost => maybe_unmarshal(Cost, fun unmarshal_cash/1),
        created_at => maybe_unmarshal(CreatedAt, fun unmarshal_string/1),
        effective_adjustment => maybe_unmarshal(
            EffectiveAdjustment,
            fun unmarshal_payment_processing_invoice_adjustment/1
        ),
        effective_payment => maybe_unmarshal(EffectivePayment, fun unmarshal_payment_processing_invoice_payment/1)
    }).

unmarshal_payment_processing_invoice_adjustment(#domain_InvoiceAdjustment{id = ID}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1)
    }).

unmarshal_payment_processing_invoice_payment(#limiter_context_payproc_InvoicePayment{
    payment = #domain_InvoicePayment{
        id = ID,
        owner_id = OwnerID,
        shop_id = ShopID,
        cost = Cost,
        created_at = CreatedAt,
        flow = Flow,
        payer = Payer
    },
    capture_cost = CaptureCost,
    effective_adjustment = EffectiveAdjustment,
    effective_refund = EffectiveRefund,
    effective_chargeback = EffectiveChargeback
}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1),
        owner_id => maybe_unmarshal(OwnerID, fun unmarshal_string/1),
        shop_id => maybe_unmarshal(ShopID, fun unmarshal_string/1),
        cost => maybe_unmarshal(Cost, fun unmarshal_cash/1),
        capture_cost => maybe_unmarshal(CaptureCost, fun unmarshal_cash/1),
        created_at => maybe_unmarshal(CreatedAt, fun unmarshal_string/1),
        flow => maybe_unmarshal(Flow, fun unmarshal_payment_processing_invoice_payment_flow/1),
        payer => maybe_unmarshal(Payer, fun unmarshal_payment_processing_invoice_payment_payer/1),
        effective_adjustment => maybe_unmarshal(
            EffectiveAdjustment,
            fun unmarshal_payment_processing_invoice_payment_adjustment/1
        ),
        effective_refund => maybe_unmarshal(EffectiveRefund, fun unmarshal_payment_processing_invoice_payment_refund/1),
        effective_chargeback => maybe_unmarshal(
            EffectiveChargeback,
            fun unmarshal_payment_processing_invoice_payment_chargeback/1
        )
    }).

unmarshal_payment_processing_invoice_payment_flow({Flow, _}) ->
    Flow.

unmarshal_payment_processing_invoice_payment_payer({Payer, Data}) ->
    {Payer, unmarshal_payment_processing_invoice_payment_payer_data(Data)}.

unmarshal_payment_processing_invoice_payment_payer_data(#domain_PaymentResourcePayer{
    resource = #domain_DisposablePaymentResource{payment_tool = PaymentTool}
}) ->
    genlib_map:compact(#{payment_tool => maybe_unmarshal(PaymentTool, fun unmarshal_payment_processing_payment_tool/1)});
unmarshal_payment_processing_invoice_payment_payer_data(#domain_CustomerPayer{payment_tool = PaymentTool}) ->
    genlib_map:compact(#{payment_tool => maybe_unmarshal(PaymentTool, fun unmarshal_payment_processing_payment_tool/1)});
unmarshal_payment_processing_invoice_payment_payer_data(#domain_RecurrentPayer{payment_tool = PaymentTool}) ->
    genlib_map:compact(#{payment_tool => maybe_unmarshal(PaymentTool, fun unmarshal_payment_processing_payment_tool/1)}).

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
