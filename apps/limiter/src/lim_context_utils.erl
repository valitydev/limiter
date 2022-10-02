-module(lim_context_utils).

-include_lib("limiter_proto/include/limproto_base_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([route_provider_id/1]).
-export([route_terminal_id/1]).

-type provider_id() :: binary().
-type terminal_id() :: binary().

%%

-spec route_provider_id(limproto_base_thrift:'Route'()) ->
    {ok, provider_id()}.
route_provider_id(#base_Route{provider = #domain_ProviderRef{id = ID}}) ->
    {ok, genlib:to_binary(ID)}.

-spec route_terminal_id(limproto_base_thrift:'Route'()) ->
    {ok, terminal_id()}.
route_terminal_id(#base_Route{terminal = #domain_TerminalRef{id = ID}}) ->
    {ok, genlib:to_binary(ID)}.
