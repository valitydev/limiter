-module(lim_body).

-export([get/3]).

-type amount() :: integer().
-type cash() :: #{
    amount := amount(),
    currency := currency()
}.

-type currency() :: lim_domain_thrift:'CurrencySymbolicCode'().
-type config() :: lim_config_machine:config().
-type body_type() :: full | partial.

-export_type([amount/0]).
-export_type([currency/0]).
-export_type([cash/0]).

-import(lim_pipeline, [do/1, unwrap/1]).

-spec get(body_type(), config(), lim_context:t()) ->
    {ok, cash()} | {error, notfound}.
get(BodyType, Config, LimitContext) ->
    do(fun() ->
        ContextType = lim_config_machine:context_type(Config),
        {ok, Operation} = lim_context:get_operation(ContextType, LimitContext),
        Body = unwrap(get_body_for_operation(BodyType, Operation, Config, LimitContext)),
        apply_op_behaviour(Body, Config, LimitContext)
    end).

-spec get_body_for_operation(body_type(), lim_context:context_operation(), config(), lim_context:t()) ->
    {ok, cash()} | {error, notfound}.
get_body_for_operation(full, invoice = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, Operation, LimitContext);
get_body_for_operation(full, invoice_adjustment, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, invoice, LimitContext);
get_body_for_operation(full, invoice_payment = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, Operation, LimitContext);
get_body_for_operation(full, invoice_payment_adjustment, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, invoice_payment, LimitContext);
get_body_for_operation(full, invoice_payment_refund, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, invoice_payment, LimitContext);
get_body_for_operation(full, invoice_payment_chargeback = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, body, Operation, LimitContext);
get_body_for_operation(partial, invoice, _Config, _LimitContext) ->
    {error, notfound};
get_body_for_operation(partial, invoice_adjustment, _Config, _LimitContext) ->
    {error, notfound};
get_body_for_operation(partial, invoice_payment = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, capture_cost, Operation, LimitContext);
get_body_for_operation(partial, invoice_payment_adjustment, _Config, _LimitContext) ->
    {error, notfound};
get_body_for_operation(partial, invoice_payment_refund = Operation, Config, LimitContext) ->
    ContextType = lim_config_machine:context_type(Config),
    lim_context:get_from_context(ContextType, cost, Operation, LimitContext);
get_body_for_operation(partial, invoice_payment_chargeback, _Config, _LimitContext) ->
    {error, notfound}.

apply_op_behaviour(Body, #{op_behaviour := ComputationConfig}, LimitContext) ->
    {ok, Operation} = lim_context:get_operation(payment_processing, LimitContext),
    case maps:get(Operation, ComputationConfig, undefined) of
        addition ->
            Body;
        subtraction ->
            invert_body(Body);
        undefined ->
            Body
    end;
apply_op_behaviour(Body, _Config, _LimitContext) ->
    Body.

invert_body(Cash = #{amount := Amount}) ->
    Cash#{amount := -Amount}.
