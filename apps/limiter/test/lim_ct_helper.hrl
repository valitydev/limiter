-ifndef(__limiter_ct_helper__).
-define(__limiter_ct_helper__, 42).

-include_lib("limiter_proto/include/lim_configurator_thrift.hrl").

-define(cash(Amount), #limiter_base_Cash{
    amount = Amount,
    currency = #limiter_base_CurrencyRef{symbolic_code = <<"RUB">>}
}).

-define(op_invoice_payment(), {invoice_payment, #limiter_context_PaymentProcessingOperationInvoicePayment{}}).

-define(ctx_invoice_payment(Cost, CaptureCost), ?ctx_invoice_payment(undefined, undefined, Cost, CaptureCost)).

-define(ctx_invoice_payment(OwnerID, ShopID, Cost, CaptureCost), #limiter_context_LimitContext{
    payment_processing = #limiter_context_ContextPaymentProcessing{
        op = ?op_invoice_payment(),
        invoice = #limiter_context_Invoice{
            owner_id = OwnerID,
            shop_id = ShopID,
            effective_payment = #limiter_context_InvoicePayment{
                created_at = <<"2000-01-01T00:00:00Z">>,
                cost = Cost,
                capture_cost = CaptureCost
            }
        }
    }
}).

-define(ctx_invoice_payment(Payment), #limiter_context_LimitContext{
    payment_processing = #limiter_context_ContextPaymentProcessing{
        op = ?op_invoice_payment(),
        invoice = #limiter_context_Invoice{
            effective_payment = Payment
        }
    }
}).

-define(ctx_invoice_payment_refund(OwnerID, ShopID, Cost, CaptureCost, RefundCost), #limiter_context_LimitContext{
    payment_processing = #limiter_context_ContextPaymentProcessing{
        op = {invoice_payment_refund, #limiter_context_PaymentProcessingOperationInvoicePaymentRefund{}},
        invoice = #limiter_context_Invoice{
            owner_id = OwnerID,
            shop_id = ShopID,
            effective_payment = #limiter_context_InvoicePayment{
                created_at = <<"2000-01-01T00:00:00Z">>,
                cost = Cost,
                capture_cost = CaptureCost,
                effective_refund = #limiter_context_InvoicePaymentRefund{
                    cost = RefundCost,
                    created_at = <<"2000-01-01T00:00:00Z">>
                }
            }
        }
    }
}).

-endif.
