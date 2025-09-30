-module(lim_range_codec).

-export([parse_timestamp/1]).

%% API

-spec parse_timestamp(binary()) -> calendar:datetime() | no_return().
parse_timestamp(Bin) ->
    try
        MicroSeconds = genlib_rfc3339:parse(Bin, microsecond),
        case genlib_rfc3339:is_utc(Bin) of
            false ->
                erlang:error({bad_timestamp, not_utc}, [Bin]);
            true ->
                calendar:system_time_to_universal_time(MicroSeconds, microsecond)
        end
    catch
        error:Error:St ->
            erlang:raise(error, {bad_timestamp, Bin, Error}, St)
    end.
