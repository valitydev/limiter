-ifndef(__limiter_ct_helper__).
-define(__limiter_ct_helper__, 42).

-include_lib("limiter_proto/include/lim_configurator_thrift.hrl").

-define(currency, <<"RUB">>).

-define(cash(Amount), ?cash(Amount, ?currency)).
-define(cash(Amount, Currency), #limiter_base_Cash{
    amount = Amount,
    currency = #limiter_base_CurrencyRef{symbolic_code = Currency}
}).

-define(scope(Types), {multi, ordsets:from_list(Types)}).
-define(global(), ?scope([])).

-define(scope_party(), {party, #limiter_config_LimitScopeEmptyDetails{}}).
-define(scope_shop(), {shop, #limiter_config_LimitScopeEmptyDetails{}}).

-define(body_type_cash(), ?body_type_cash(?currency)).
-define(body_type_cash(Currency),
    {cash, #limiter_config_LimitBodyTypeCash{currency = Currency}}
).
-define(body_type_amount(),
    {amount, #limiter_config_LimitBodyTypeAmount{}}
).

-define(lim_type_turnover(),
    {turnover, #limiter_config_LimitTypeTurnover{}}
).

-define(time_range_day(),
    {calendar, {day, #time_range_TimeRangeTypeCalendarDay{}}}
).
-define(time_range_week(),
    {calendar, {week, #time_range_TimeRangeTypeCalendarWeek{}}}
).
-define(time_range_month(),
    {calendar, {month, #time_range_TimeRangeTypeCalendarMonth{}}}
).

-define(op_behaviour(), ?op_behaviour(?op_addition())).
-define(op_behaviour(Refund), #limiter_config_OperationLimitBehaviour{
    invoice_payment_refund = Refund
}).

-define(op_addition(), {addition, #limiter_config_Addition{}}).
-define(op_subtraction(), {subtraction, #limiter_config_Subtraction{}}).

-define(ctx_type_payproc(),
    {payment_processing, #limiter_config_LimitContextTypePaymentProcessing{}}
).

-define(op_invoice(), {invoice, #limiter_context_PaymentProcessingOperationInvoice{}}).
-define(op_invoice_payment(), {invoice_payment, #limiter_context_PaymentProcessingOperationInvoicePayment{}}).

-define(ctx_invoice(Cost), #limiter_context_LimitContext{
    payment_processing = #limiter_context_ContextPaymentProcessing{
        op = ?op_invoice(),
        invoice = #limiter_context_Invoice{
            created_at = <<"2000-01-01T00:00:00Z">>,
            cost = Cost
        }
    }
}).

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
