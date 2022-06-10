-module(lim_config_machinery_schema).

%% Storage schema behaviour
-behaviour(machinery_mg_schema).

-export([get_version/1]).
-export([marshal/3]).
-export([unmarshal/3]).

%% Constants

-define(CURRENT_EVENT_FORMAT_VERSION, 1).

%% Internal types

-type type() :: machinery_mg_schema:t().
-type value(T) :: machinery_mg_schema:v(T).
-type value_type() :: machinery_mg_schema:vt().
-type context() :: machinery_mg_schema:context().

-type event() :: lim_config_machine:timestamped_event(lim_config_machine:event()).
-type aux_state() :: term().
-type call_args() :: term().
-type call_response() :: term().

-type data() ::
    aux_state()
    | event()
    | call_args()
    | call_response().

%% machinery_mg_schema callbacks

-spec get_version(value_type()) -> machinery_mg_schema:version().
get_version(event) ->
    ?CURRENT_EVENT_FORMAT_VERSION;
get_version(aux_state) ->
    undefined.

-spec marshal(type(), value(data()), context()) -> {machinery_msgpack:t(), context()}.
marshal({event, FormatVersion}, TimestampedChange, Context) ->
    marshal_event(FormatVersion, TimestampedChange, Context);
marshal(T, V, C) when
    T =:= {args, init} orelse
        T =:= {args, call} orelse
        T =:= {args, repair} orelse
        T =:= {aux_state, undefined} orelse
        T =:= {response, call} orelse
        T =:= {response, {repair, success}} orelse
        T =:= {response, {repair, failure}}
->
    machinery_mg_schema_generic:marshal(T, V, C).

-spec unmarshal(type(), machinery_msgpack:t(), context()) -> {data(), context()}.
unmarshal({event, FormatVersion}, EncodedChange, Context) ->
    unmarshal_event(FormatVersion, EncodedChange, Context);
unmarshal(T, V, C) when
    T =:= {args, init} orelse
        T =:= {args, call} orelse
        T =:= {args, repair} orelse
        T =:= {aux_state, undefined} orelse
        T =:= {response, call} orelse
        T =:= {response, {repair, success}} orelse
        T =:= {response, {repair, failure}}
->
    machinery_mg_schema_generic:unmarshal(T, V, C).

%% Internals

-spec marshal_event(machinery_mg_schema:version(), event(), context()) -> {machinery_msgpack:t(), context()}.
marshal_event(1, TimestampedChange, Context) ->
    ThriftChange = lim_config_codec:marshal(timestamped_change, TimestampedChange),
    Type = {struct, struct, {lim_limiter_config_thrift, 'TimestampedChange'}},
    {{bin, lim_proto_utils:serialize(Type, ThriftChange)}, Context}.

-spec unmarshal_event(machinery_mg_schema:version(), machinery_msgpack:t(), context()) -> {event(), context()}.
unmarshal_event(1, EncodedChange, Context) ->
    {bin, EncodedThriftChange} = EncodedChange,
    Type = {struct, struct, {lim_limiter_config_thrift, 'TimestampedChange'}},
    ThriftChange = lim_proto_utils:deserialize(Type, EncodedThriftChange),
    {lim_config_codec:unmarshal(timestamped_change, ThriftChange), Context}.

%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.

-spec marshal_unmarshal_created_test() -> _.

marshal_unmarshal_created_test() ->
    ID = <<"id">>,
    Created =
        {created, #{
            id => ID,
            processor_type => <<"type">>,
            created_at => lim_time:now(),
            started_at => <<"2000-01-01T00:00:00Z">>,
            shard_size => 7,
            time_range_type => {calendar, day},
            context_type => payment_processing,
            type => {turnover, {amount, <<"RUB">>}},
            scope => ordsets:from_list([party]),
            description => <<"description">>
        }},
    Context = #{machine_ref => ID, machine_ns => config},
    Event = {ev, lim_time:machinery_now(), Created},
    {Marshaled, _} = marshal_event(1, Event, Context),
    {Unmarshaled, _} = unmarshal_event(1, Marshaled, Context),
    ?assertEqual(Event, Unmarshaled).

-endif.
