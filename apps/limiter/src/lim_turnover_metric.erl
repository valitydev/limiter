-module(lim_turnover_metric).

-export([compute/4]).

-type amount() :: lim_body:amount().
-type currency() :: dmsl_domain_thrift:'CurrencySymbolicCode'().
-type stage() :: hold | commit.
-type t() :: number | {amount, currency()}.

-type invalid_operation_currency_error() :: {invalid_operation_currency, {currency(), currency()}}.

-export_type([t/0]).
-export_type([stage/0]).
-export_type([invalid_operation_currency_error/0]).

%%

%% Если не менять логику обращений к лимитеру в ХГ то остаётся проблема с различением роллбеков при фейле платежа, отсеве в маршрутизации или других специальных случаев при финализации общения с адаптером.
%% (Метрики, атрибуты для различения счётчиков и временных отрезков в таблице опущены.)
%%
%% |  Лимит   | ID |      Тип | Изменение | Итого | Комментарий                                                |
%% |:--------:|:--:|---------:|----------:|------:|:-----------------------------------------------------------|
%% | Обычный  | A1 |     HOLD |        +1 |     1 | Участие в выборе маршрута                                  |
%% | Обычный  | A1 |   COMMIT |        +1 |     1 | Успешный кепчур в платёжной сесси по данному маршруту      |
%% | Обычный  | B1 |     HOLD |        +1 |     2 | Участие в выборе маршрута                                  |
%% | Обычный  | B1 | ROLLBACK |         0 |     1 | Отсев этого маршрута по сторонним критериям                |
%% | Обычный  | C1 |     HOLD |        +1 |     1 | Участие в выборе маршрута                                  |
%% | Обычный  | C1 | ROLLBACK |         0 |     1 | Провал платёжной сессии после использования этого маршрута |
%% | Обратный | A2 |     HOLD |        +1 |     1 | Участие в выборе маршрута                                  |
%% | Обратный | A2 |   COMMIT |         0 |     0 | Успешный кепчур в платёжной сесси по данному маршруту      |
%% | Обратный | B2 |     HOLD |        +1 |     1 | Участие в выборе маршрута                                  |
%% | Обратный | B2 | ROLLBACK |         0 |     0 | Отсев этого маршрута по сторонним критериям                |
%% | Обратный | C2 |     HOLD |        +1 |     1 | Участие в выборе маршрута                                  |
%% | Обратный | C2 | ROLLBACK |        +1 |     1 | Провал платёжной сессии после использования этого маршрута |

-spec compute(t(), stage(), lim_config_machine:config(), lim_context:t()) ->
    {ok, amount()} | {error, lim_rates:conversion_error()} | {error, invalid_operation_currency_error()}.
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
denominate(#{currency := Currency} = Body, DestinationCurrency, Config, LimitContext) ->
    case lim_config_machine:currency_conversion(Config) of
        false -> currencies_mismatch_error(Currency, DestinationCurrency);
        true -> convert_currency(Body, DestinationCurrency, Config, LimitContext)
    end.

currencies_mismatch_error(Currency, ExpectedCurrency) ->
    {error, {invalid_operation_currency, {Currency, ExpectedCurrency}}}.

convert_currency(Body, DestinationCurrency, Config, LimitContext) ->
    case lim_rates:convert(Body, DestinationCurrency, Config, LimitContext) of
        {ok, #{amount := AmountConverted}} ->
            {ok, AmountConverted};
        {error, _} = Error ->
            Error
    end.

sign(Amount) when Amount > 0 ->
    +1;
sign(Amount) when Amount < 0 ->
    -1.
