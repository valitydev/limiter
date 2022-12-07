-module(lim_turnover_metric).

-export([compute/4]).

-type amount() :: lim_body:amount().
-type currency() :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type stage() :: hold | commit.
-type t() :: number | {amount, currency()}.

-type invalid_request_error() :: {invalid_request, list(binary())}.

-export_type([t/0]).

%%

-spec compute(t(), stage(), lim_config_machine:config(), lim_context:t()) ->
    {ok, amount()} | {error, invalid_request_error()}.
compute(number, hold, Config, LimitContext) ->
    #{amount := Amount} = get_body(Config, LimitContext),
    {ok, sign(Amount)};
compute(number, commit, Config, LimitContext) ->
    case get_commit_body(Config, LimitContext) of
        #{amount := Amount} when Amount /= 0 ->
            {ok, sign(Amount)};
        #{amount := 0} ->
            % Zero amount operation currently means "rollback" in the protocol.
            {ok, 0}
    end;
compute({amount, Currency}, hold, Config, LimitContext) ->
    Body = get_body(Config, LimitContext),
    denominate(Body, Currency, Config, LimitContext);
compute({amount, Currency}, commit, Config, LimitContext) ->
    Body = get_commit_body(Config, LimitContext),
    denominate(Body, Currency, Config, LimitContext).

get_body(Config, LimitContext) ->
    {ok, Body} = lim_body:get(full, Config, LimitContext),
    Body.

get_commit_body(Config, LimitContext) ->
    case lim_body:get(partial, Config, LimitContext) of
        {ok, Body} ->
            Body;
        {error, _} ->
            get_body(Config, LimitContext)
    end.

%%

denominate(#{amount := Amount, currency := Currency}, Currency, _Config, _LimitContext) ->
    {ok, Amount};
denominate(#{currency := Currency}, DestinationCurrency, _Config, _LimitContext) ->
    {error,
        {invalid_request, [
            genlib:format(
                "Invalid operation currency: ~p, expected limit currency: ~p",
                [Currency, DestinationCurrency]
            )
        ]}}.
%% NOTE conversion disabled temporarily
%%denominate(Body = #{}, DestinationCurrency, Config, LimitContext) ->
%%    case lim_rates:convert(Body, DestinationCurrency, Config, LimitContext) of
%%        {ok, #{amount := AmountConverted}} ->
%%            {ok, AmountConverted};
%%        {error, _} = Error ->
%%            Error
%%    end.

sign(Amount) when Amount > 0 ->
    +1;
sign(Amount) when Amount < 0 ->
    -1.
