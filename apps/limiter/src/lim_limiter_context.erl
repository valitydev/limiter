-module(lim_limiter_context).

-include_lib("limiter_proto/include/lim_limiter_context_thrift.hrl").

-export([unmarshal/1]).

-type thrift_context() :: lim_limiter_context_thrift:'ContextPaymentProcessing'().

%%

-spec unmarshal(thrift_context()) -> lim_context:context().
unmarshal(#limiter_context_ContextPaymentProcessing{
    op = {Operation, _},
    invoice = Invoice
}) ->
    genlib_map:compact(#{
        op => Operation,
        invoice => maybe_unmarshal(Invoice, fun unmarshal_payment_processing_invoice/1)
    }).

unmarshal_payment_processing_invoice(#limiter_context_Invoice{
    id = ID,
    owner_id = OwnerID,
    shop_id = ShopID,
    cost = Cost,
    created_at = CreatedAt,
    effective_payment = Payment,
    effective_adjustment = Adjustment
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

unmarshal_payment_processing_invoice_adjustment(#limiter_context_InvoiceAdjustment{id = ID}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1)
    }).

unmarshal_payment_processing_invoice_payment(#limiter_context_InvoicePayment{
    id = ID,
    owner_id = OwnerID,
    shop_id = ShopID,
    cost = Cost,
    capture_cost = CaptureCost,
    created_at = CreatedAt,
    flow = Flow,
    payer = Payer,
    effective_adjustment = Adjustment,
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

unmarshal_payment_processing_invoice_payment_flow({Flow, _}) ->
    Flow.

unmarshal_payment_processing_invoice_payment_payer({Payer, _}) ->
    {Payer, #{}}.

unmarshal_payment_processing_invoice_payment_adjustment(#limiter_context_InvoicePaymentAdjustment{
    id = ID,
    created_at = CreatedAt
}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1),
        created_at => maybe_unmarshal(CreatedAt, fun unmarshal_string/1)
    }).

unmarshal_payment_processing_invoice_payment_refund(#limiter_context_InvoicePaymentRefund{
    id = ID,
    cost = Cost,
    created_at = CreatedAt
}) ->
    genlib_map:compact(#{
        id => maybe_unmarshal(ID, fun unmarshal_string/1),
        cost => maybe_unmarshal(Cost, fun unmarshal_cash/1),
        created_at => maybe_unmarshal(CreatedAt, fun unmarshal_string/1)
    }).

unmarshal_payment_processing_invoice_payment_chargeback(#limiter_context_InvoicePaymentChargeback{
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
