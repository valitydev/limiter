-ifndef(__limiter_ct_helper__).
-define(__limiter_ct_helper__, 42).

-include_lib("limiter_proto/include/limproto_base_thrift.hrl").
-include_lib("limiter_proto/include/limproto_config_thrift.hrl").
-include_lib("limiter_proto/include/limproto_timerange_thrift.hrl").
-include_lib("limiter_proto/include/limproto_limiter_thrift.hrl").
-include_lib("limiter_proto/include/limproto_context_payproc_thrift.hrl").
-include_lib("limiter_proto/include/limproto_context_withdrawal_thrift.hrl").
-include_lib("damsel/include/dmsl_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_wthd_domain_thrift.hrl").

-define(currency, <<"RUB">>).
-define(string, <<"STRING">>).
-define(token, <<"TOKEN">>).
-define(integer, 999).
-define(timestamp, <<"2000-01-01T00:00:00Z">>).

-define(cash(Amount), ?cash(Amount, ?currency)).
-define(cash(Amount, Currency), #domain_Cash{
    amount = Amount,
    currency = #domain_CurrencyRef{symbolic_code = Currency}
}).

-define(route(), ?route(?integer, ?integer)).
-define(route(ProviderID, TerminalID), #base_Route{
    provider = #domain_ProviderRef{id = ProviderID},
    terminal = #domain_TerminalRef{id = TerminalID}
}).

-define(scope(Types), {multi, ordsets:from_list(Types)}).
-define(global(), ?scope([])).

-define(scope_party(), {party, #config_LimitScopeEmptyDetails{}}).
-define(scope_shop(), {shop, #config_LimitScopeEmptyDetails{}}).
-define(scope_payment_tool(), {payment_tool, #config_LimitScopeEmptyDetails{}}).
-define(scope_provider(), {provider, #config_LimitScopeEmptyDetails{}}).
-define(scope_terminal(), {terminal, #config_LimitScopeEmptyDetails{}}).
-define(scope_payer_contact_email(), {payer_contact_email, #config_LimitScopeEmptyDetails{}}).
-define(scope_wallet(), {wallet, #config_LimitScopeEmptyDetails{}}).
-define(scope_sender(), {sender, #config_LimitScopeEmptyDetails{}}).
-define(scope_receiver(), {receiver, #config_LimitScopeEmptyDetails{}}).
-define(scope_destination_field(FieldPath),
    {destination_field, #config_LimitScopeDestinationFieldDetails{field_path = FieldPath}}
).

-define(lim_type_turnover(), ?lim_type_turnover(?turnover_metric_number())).
-define(lim_type_turnover(Metric),
    {turnover, #config_LimitTypeTurnover{metric = Metric}}
).

-define(turnover_metric_number(), {number, #config_LimitTurnoverNumber{}}).
-define(turnover_metric_amount(), ?turnover_metric_amount(?currency)).
-define(turnover_metric_amount(Currency),
    {amount, #config_LimitTurnoverAmount{currency = Currency}}
).

-define(time_range_day(),
    {calendar, {day, #timerange_TimeRangeTypeCalendarDay{}}}
).
-define(time_range_week(),
    {calendar, {week, #timerange_TimeRangeTypeCalendarWeek{}}}
).
-define(time_range_month(),
    {calendar, {month, #timerange_TimeRangeTypeCalendarMonth{}}}
).
-define(time_range_year(),
    {calendar, {year, #timerange_TimeRangeTypeCalendarYear{}}}
).

-define(op_behaviour(), ?op_behaviour(?op_addition())).
-define(op_behaviour(Refund), #config_OperationLimitBehaviour{
    invoice_payment_refund = Refund
}).

-define(currency_conversion(), #config_CurrencyConversion{}).

-define(op_addition(), {addition, #config_Addition{}}).
-define(op_subtraction(), {subtraction, #config_Subtraction{}}).

-define(ctx_type_payproc(),
    {payment_processing, #config_LimitContextTypePaymentProcessing{}}
).

-define(ctx_type_wthdproc(),
    {withdrawal_processing, #config_LimitContextTypeWithdrawalProcessing{}}
).

%% Payproc

-define(op_invoice, {invoice, #context_payproc_OperationInvoice{}}).
-define(op_payment, {invoice_payment, #context_payproc_OperationInvoicePayment{}}).
-define(op_refund, {invoice_payment_refund, #context_payproc_OperationInvoicePaymentRefund{}}).

-define(bank_card(), ?bank_card(?string, 2, 2022)).

-define(bank_card(Token),
    {bank_card, #domain_BankCard{
        token = Token,
        bin = ?string,
        last_digits = ?string
    }}
).

-define(bank_card(Token, Month, Year),
    {bank_card, #domain_BankCard{
        token = Token,
        bin = ?string,
        last_digits = ?string,
        exp_date = #domain_BankCardExpDate{month = Month, year = Year}
    }}
).

-define(digital_wallet(ID, Service),
    {digital_wallet, #domain_DigitalWallet{
        id = ID,
        payment_service = #domain_PaymentServiceRef{id = Service}
    }}
).

-define(generic_pt(),
    {generic, #domain_GenericPaymentTool{
        payment_service = #domain_PaymentServiceRef{id = <<"ID42">>},
        data = #base_Content{
            type = <<"application/json">>, data = <<"{\"opaque\":{\"payload\":{\"data\":\"value\"}}}">>
        }
    }}
).

-define(invoice(OwnerID, ShopID, Cost), #domain_Invoice{
    id = ?string,
    owner_id = OwnerID,
    shop_id = ShopID,
    created_at = ?timestamp,
    status = {unpaid, #domain_InvoiceUnpaid{}},
    details = #domain_InvoiceDetails{product = ?string},
    due = ?timestamp,
    cost = Cost
}).

-define(invoice_payment(Cost, CaptureCost),
    ?invoice_payment(Cost, CaptureCost, ?bank_card())
).

-define(invoice_payment(Cost, CaptureCost, PaymentTool),
    ?invoice_payment(Cost, CaptureCost, PaymentTool, ?timestamp)
).

-define(invoice_payment(Cost, CaptureCost, PaymentTool, CreatedAt), #domain_InvoicePayment{
    id = ?string,
    created_at = CreatedAt,
    status = {captured, #domain_InvoicePaymentCaptured{cost = CaptureCost}},
    cost = Cost,
    domain_revision = 1,
    flow = {instant, #domain_InvoicePaymentFlowInstant{}},
    payer =
        {payment_resource, #domain_PaymentResourcePayer{
            resource = #domain_DisposablePaymentResource{
                payment_tool = PaymentTool
            },
            contact_info = #domain_ContactInfo{
                email = ?string
            }
        }}
}).

-define(payproc_ctx(Op, Invoice, InvoicePayment), #limiter_LimitContext{
    payment_processing = #context_payproc_Context{
        op = Op,
        invoice = #context_payproc_Invoice{
            invoice = Invoice,
            payment = InvoicePayment
        }
    }
}).

-define(payproc_ctx(Invoice, InvoicePayment), ?payproc_ctx(?op_invoice, Invoice, InvoicePayment)).

-define(payproc_ctx_invoice(Cost), ?payproc_ctx(?invoice(?string, ?string, Cost), undefined)).

-define(payproc_ctx_payment(Cost, CaptureCost),
    ?payproc_ctx_payment(?string, ?string, Cost, CaptureCost)
).

-define(payproc_ctx_payment(OwnerID, ShopID, Cost, CaptureCost), #limiter_LimitContext{
    payment_processing = #context_payproc_Context{
        op = ?op_payment,
        invoice = #context_payproc_Invoice{
            invoice = ?invoice(OwnerID, ShopID, Cost),
            payment = #context_payproc_InvoicePayment{
                payment = ?invoice_payment(Cost, CaptureCost),
                route = ?route()
            }
        }
    }
}).

-define(payproc_ctx_payment(Payment), #limiter_LimitContext{
    payment_processing = #context_payproc_Context{
        op = ?op_payment,
        invoice = #context_payproc_Invoice{
            invoice = ?invoice(?string, ?string, ?cash(10)),
            payment = #context_payproc_InvoicePayment{
                payment = Payment,
                route = ?route()
            }
        }
    }
}).

-define(payproc_ctx_refund(OwnerID, ShopID, Cost, CaptureCost, RefundCost), #limiter_LimitContext{
    payment_processing = #context_payproc_Context{
        op = ?op_refund,
        invoice = #context_payproc_Invoice{
            invoice = ?invoice(OwnerID, ShopID, Cost),
            payment = #context_payproc_InvoicePayment{
                payment = ?invoice_payment(Cost, CaptureCost),
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
}).

%% Wthdproc

-define(auth_data(Sender, Receiver),
    {sender_receiver, #wthd_domain_SenderReceiverAuthData{sender = Sender, receiver = Receiver}}
).

-define(withdrawal(Body), ?withdrawal(Body, ?bank_card(), ?string)).

-define(withdrawal(Body, Destination, OwnerID), ?withdrawal(Body, Destination, OwnerID, undefined)).

-define(withdrawal(Body, Destination, OwnerID, AuthData), #wthd_domain_Withdrawal{
    body = Body,
    created_at = ?timestamp,
    destination = Destination,
    sender = OwnerID,
    auth_data = AuthData
}).

-define(op_withdrawal, {withdrawal, #context_withdrawal_OperationWithdrawal{}}).

-define(wthdproc_ctx(Withdrawal), #limiter_LimitContext{
    withdrawal_processing = #context_withdrawal_Context{
        op = ?op_withdrawal,
        withdrawal = #context_withdrawal_Withdrawal{
            withdrawal = Withdrawal,
            route = ?route(),
            wallet_id = ?string
        }
    }
}).

-define(wthdproc_ctx_withdrawal(Cost), ?wthdproc_ctx(?withdrawal(Cost))).

-define(wthdproc_ctx_withdrawal_w_auth_data(Cost, Sender, Receiver),
    ?wthdproc_ctx(?withdrawal(Cost, ?bank_card(), ?string, ?auth_data(Sender, Receiver)))
).

-endif.
