-module(lim_p_transfer).

-include_lib("damsel/include/dmsl_accounter_thrift.hrl").

-export([construct_posting/2]).
-export([reverse_posting/1]).

-type posting() :: lim_accounting:posting().
-type body() :: lim_body:t().

-spec construct_posting(lim_range_machine:time_range_ext(), body()) -> posting().
construct_posting(#{account_id_from := From, account_id_to := To}, {cash, #{amount := Amount, currency := Currency}}) ->
    #accounter_Posting{
        from_id = From,
        to_id = To,
        amount = Amount,
        currency_sym_code = Currency,
        description = <<>>
    };
construct_posting(#{account_id_from := From, account_id_to := To}, {amount, Amount}) ->
    #accounter_Posting{
        from_id = From,
        to_id = To,
        amount = Amount,
        currency_sym_code = lim_accounting:noncurrency(),
        description = <<>>
    }.

-spec reverse_posting(posting()) -> posting().
reverse_posting(Posting = #accounter_Posting{from_id = AccountFrom, to_id = AccountTo}) ->
    Posting#accounter_Posting{
        from_id = AccountTo,
        to_id = AccountFrom
    }.
