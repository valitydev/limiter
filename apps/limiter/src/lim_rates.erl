-module(lim_rates).

-include_lib("xrates_proto/include/xrates_rate_thrift.hrl").

-export([convert/4]).

-type context() :: lim_context:t().
-type timestamp() :: binary().

-type conversion_error() :: quote_not_found | currency_not_found.

-export_type([conversion_error/0]).

-define(APP, limiter).
-define(DEFAULT_FACTOR, 1.1).
-define(DEFAULT_FACTOR_NAME, <<"DEFAULT">>).

-spec convert(lim_body:cash(), lim_body:currency(), timestamp(), context()) ->
    {ok, lim_body:cash()}
    | {error, conversion_error()}.
convert(Cash = #{currency := Currency}, DestinationCurrency, Timestamp, LimitContext) ->
    Factor = get_exchange_factor(Currency),
    Request = construct_conversion_request(Cash, DestinationCurrency, Timestamp),
    case call_rates('GetConvertedAmount', {<<"CBR">>, Request}, LimitContext) of
        {ok, #base_Rational{p = P, q = Q}} ->
            Rational = genlib_rational:new(P, Q),
            Amount = genlib_rational:round(genlib_rational:mul(Rational, Factor)),
            {ok, #{amount => Amount, currency => DestinationCurrency}};
        {exception, #rate_QuoteNotFound{}} ->
            {error, quote_not_found};
        {exception, #rate_CurrencyNotFound{}} ->
            {error, currency_not_found}
    end.

construct_conversion_request(#{amount := Amount, currency := Currency}, DestinationCurrency, Timestamp) ->
    #rate_ConversionRequest{
        source = Currency,
        destination = DestinationCurrency,
        amount = Amount,
        datetime = Timestamp
    }.

get_exchange_factor(Currency) ->
    Factors = genlib_app:env(?APP, exchange_factors, #{}),
    case maps:get(Currency, Factors, undefined) of
        undefined ->
            case maps:get(?DEFAULT_FACTOR_NAME, Factors, undefined) of
                undefined ->
                    ?DEFAULT_FACTOR;
                DefaultFactor ->
                    DefaultFactor
            end;
        Factor ->
            Factor
    end.

%%

call_rates(Function, Args, LimitContext) ->
    lim_client_woody:call(xrates, Function, Args, lim_context:woody_context(LimitContext)).
