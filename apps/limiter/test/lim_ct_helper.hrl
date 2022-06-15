-ifndef(__limiter_ct_helper__).
-define(__limiter_ct_helper__, 42).

-include_lib("limiter_proto/include/lim_configurator_thrift.hrl").

-define(currency, <<"RUB">>).
-define(string, <<"STRING">>).
-define(timestamp, <<"2000-01-01T00:00:00Z">>).

-define(cash(Amount), ?cash(Amount, ?currency)).
-define(cash(Amount, Currency), #domain_Cash{
    amount = Amount,
    currency = #domain_CurrencyRef{symbolic_code = Currency}
}).

-define(scope(Types), {multi, ordsets:from_list(Types)}).
-define(global(), ?scope([])).

-define(scope_party(), {party, #limiter_config_LimitScopeEmptyDetails{}}).
-define(scope_shop(), {shop, #limiter_config_LimitScopeEmptyDetails{}}).

-define(lim_type_turnover(), ?lim_type_turnover(?turnover_metric_number())).
-define(lim_type_turnover(Metric),
    {turnover, #limiter_config_LimitTypeTurnover{metric = Metric}}
).

-define(turnover_metric_number(), {number, #limiter_config_LimitTurnoverNumber{}}).
-define(turnover_metric_amount(), ?turnover_metric_amount(?currency)).
-define(turnover_metric_amount(Currency),
    {amount, #limiter_config_LimitTurnoverAmount{currency = Currency}}
).

-define(time_range_day(),
    {calendar, {day, #limiter_time_range_TimeRangeTypeCalendarDay{}}}
).
-define(time_range_week(),
    {calendar, {week, #limiter_time_range_TimeRangeTypeCalendarWeek{}}}
).
-define(time_range_month(),
    {calendar, {month, #limiter_time_range_TimeRangeTypeCalendarMonth{}}}
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
    limiter_payment_processing = #limiter_context_ContextPaymentProcessing{
        op = ?op_invoice(),
        invoice = #limiter_context_Invoice{
            created_at = <<"2000-01-01T00:00:00Z">>,
            cost = Cost
        }
    }
}).

-define(ctx_invoice_payment(Cost, CaptureCost), ?ctx_invoice_payment(undefined, undefined, Cost, CaptureCost)).

-define(ctx_invoice_payment(OwnerID, ShopID, Cost, CaptureCost), #limiter_context_LimitContext{
    limiter_payment_processing = #limiter_context_ContextPaymentProcessing{
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
    limiter_payment_processing = #limiter_context_ContextPaymentProcessing{
        op = ?op_invoice_payment(),
        invoice = #limiter_context_Invoice{
            effective_payment = Payment
        }
    }
}).

-define(ctx_invoice_payment_refund(OwnerID, ShopID, Cost, CaptureCost, RefundCost), #limiter_context_LimitContext{
    limiter_payment_processing = #limiter_context_ContextPaymentProcessing{
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

%% Payproc

-define(payproc_op_invoice, {invoice, #limiter_context_payproc_OperationInvoice{}}).
-define(payproc_op_invoice_payment, {invoice_payment, #limiter_context_payproc_OperationInvoicePayment{}}).

-define(payproc_bank_card, #domain_BankCard{
    token = ?string,
    bin = ?string,
    last_digits = ?string
}).

-define(payproc_invoice(OwnerID, ShopID, Cost), #domain_Invoice{
    id = ?string,
    owner_id = OwnerID,
    shop_id = ShopID,
    created_at = ?timestamp,
    status = {unpaid, #domain_InvoiceUnpaid{}},
    details = #domain_InvoiceDetails{product = ?string},
    due = ?timestamp,
    cost = Cost
}).

-define(payproc_invoice_payment(Cost, CaptureCost), #domain_InvoicePayment{
    id = ?string,
    created_at = ?timestamp,
    status = {captured, #domain_InvoicePaymentCaptured{cost = CaptureCost}},
    cost = Cost,
    domain_revision = 1,
    flow = {instant, #domain_InvoicePaymentFlowInstant{}},
    payer =
        {payment_resource, #domain_PaymentResourcePayer{
            resource = #domain_DisposablePaymentResource{
                payment_tool = {bank_card, ?payproc_bank_card}
            },
            contact_info = #domain_ContactInfo{}
        }}
}).

-define(payproc_ctx_invoice(Cost), #limiter_context_LimitContext{
    payment_processing = #limiter_context_payproc_Context{
        op = ?payproc_op_invoice,
        invoice = #limiter_context_payproc_Invoice{
            invoice = ?payproc_invoice(?string, ?string, Cost)
        }
    }
}).

-define(payproc_ctx_invoice_payment(Cost, CaptureCost),
    ?payproc_ctx_invoice_payment(?string, ?string, Cost, CaptureCost)
).

-define(payproc_ctx_invoice_payment(OwnerID, ShopID, Cost, CaptureCost), #limiter_context_LimitContext{
    payment_processing = #limiter_context_payproc_Context{
        op = ?payproc_op_invoice_payment,
        invoice = #limiter_context_payproc_Invoice{
            invoice = ?payproc_invoice(OwnerID, ShopID, Cost),
            payment = #limiter_context_payproc_InvoicePayment{
                payment = ?payproc_invoice_payment(Cost, CaptureCost)
            }
        }
    }
}).

-define(payproc_ctx_invoice_payment(Payment), #limiter_context_LimitContext{
    payment_processing = #limiter_context_payproc_Context{
        op = ?payproc_op_invoice_payment,
        invoice = #limiter_context_payproc_Invoice{
            payment = Payment
        }
    }
}).

-define(payproc_ctx_invoice_payment_refund(OwnerID, ShopID, Cost, CaptureCost, RefundCost),
    #limiter_context_LimitContext{
        payment_processing = #limiter_context_payproc_Context{
            op = {invoice_payment_refund, #limiter_context_payproc_OperationInvoicePaymentRefund{}},
            invoice = #limiter_context_payproc_Invoice{
                invoice = ?payproc_invoice(OwnerID, ShopID, Cost),
                payment = #limiter_context_payproc_InvoicePayment{
                    payment = ?payproc_invoice_payment(Cost, CaptureCost),
                    refund = #domain_InvoicePaymentRefund{
                        id = ?string,
                        status = {succeeded, #domain_InvoicePaymentRefundSucceeded{}},
                        created_at = ?timestamp,
                        domain_revision = 1,
                        cash = RefundCost
                    }
                }
            }
        }
    }
).

-endif.
