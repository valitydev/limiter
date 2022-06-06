-module(lim_posting).

-include_lib("damsel/include/dmsl_accounter_thrift.hrl").

-export([new/2]).
-export([reverse/1]).

-type posting() :: lim_accounting:posting().
-type body() :: lim_body:t().

-spec new(lim_range_machine:time_range_ext(), body()) -> posting().
new(#{account_id_from := From, account_id_to := To}, {cash, #{amount := Amount, currency := Currency}}) ->
    reverse_negative_posting(#accounter_Posting{
        from_id = From,
        to_id = To,
        amount = Amount,
        currency_sym_code = Currency,
        description = <<>>
    });
new(#{account_id_from := From, account_id_to := To}, {amount, Amount}) ->
    reverse_negative_posting(#accounter_Posting{
        from_id = From,
        to_id = To,
        amount = Amount,
        currency_sym_code = lim_accounting:noncurrency(),
        description = <<>>
    }).

reverse_negative_posting(Posting = #accounter_Posting{amount = Amount}) when Amount < 0 ->
    PostingReversed = reverse(Posting),
    PostingReversed#accounter_Posting{amount = -Amount};
reverse_negative_posting(Posting) ->
    Posting.

-spec reverse(posting()) -> posting().
reverse(Posting = #accounter_Posting{from_id = AccountFrom, to_id = AccountTo}) ->
    Posting#accounter_Posting{
        from_id = AccountTo,
        to_id = AccountFrom
    }.
