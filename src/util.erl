-module(util).
-include("common.hrl").

-export([now/0, local_time/0, datetime_to_timestamp/1, timestamp_to_datetime/1, format_utc_timestamp/0
, term_to_bitstring/1, bitstring_to_term/1, today/0, fbin/2, fbin/1,
date_format/2]).

get_month(MonthStr) ->
    MonthL = [{"Jan", 1}, {"Feb", 2}, {"Mar", 3}, {"Apr", 4}, {"May", 5}, {"Jun", 6}, {"Jul", 7}, {"Aug", 8}, {"Sep", 9}, {"Oct", 10}, {"Nov", 11}, {"Dec", 12}],
    case lists:keyfind(MonthStr, 1, MonthL) of
        {_, Month} -> Month;
        _ -> 1
    end.

get_hour_minu_sec(HMSStr) ->
    [HourStr, MinuStr, SecStr] =  re:split(HMSStr, ":", [{return, list}, {parts, 6}]),
    [?l2i(HourStr), ?l2i(MinuStr), ?l2i(SecStr)].

time_zone(Zone) ->
    case Zone of
        "GMT" ->
            8 * 3600;
        _ -> 0
    end.

date_format(Type, Date) ->
    case Type of
        1 -> Date;
        2 -> other_date_format(Date);
        3 -> atom_date_format(Date);
        4 -> rss_date_format(Date);
        _ -> util:now()
    end.

other_date_format(ODate) ->
    StrTime = ?IF(is_binary(ODate), ?b2l(ODate), ODate),
    [Date, HTime] = string:tokens(StrTime, " "),
    [Year, Month, Day] = string:tokens(Date, "-"),
    [Hour, Minu, Sec] = string:tokens(HTime, ":"),
    NewTime = util:datetime_to_timestamp({{?l2i(Year), ?l2i(Month),?l2i(Day)},{?l2i(Hour), ?l2i(Minu),?l2i(Sec)}}) + 8*3600,
    NewTime.

atom_date_format(AtomDate) ->
    StrTime = ?IF(is_binary(AtomDate), ?b2l(AtomDate), AtomDate),
    [Date, Time] = string:tokens(StrTime, "T"),
    [Year, Month, Day] = string:tokens(Date, "-"),
    [HTime, _] = string:tokens(Time, "."),
    [Hour, Minu, SecD] = string:tokens(HTime, ":"),
    [Sec|_] = string:tokens(SecD, "Z"),
    NewTime = util:datetime_to_timestamp({{?l2i(Year), ?l2i(Month),?l2i(Day)},{?l2i(Hour), ?l2i(Minu),?l2i(Sec)}}) + 8*3600,
    NewTime.

rss_date_format(RssDate) ->
    [_, RssDate1] =  re:split(RssDate, ",", [{return, list}, {parts, 6}]),
    [_, DayStr, MonthStr, Year, HMS, Zone] =  re:split(RssDate1, " ", [{return, list}, {parts, 6}]),
    Month = get_month(MonthStr),
    [Hour, Minu, Sec] = get_hour_minu_sec(HMS),
    Time = datetime_to_timestamp({{?l2i(Year), Month, ?l2i(DayStr)}, {Hour, Minu, Sec}}),
    Time + time_zone(Zone).

now() ->
	{M, S, _} = os:timestamp(),
    M * 1000000 + S.

today() ->
    {{Year, Month, Day}, _} = local_time(),
    datetime_to_timestamp({{Year, Month, Day}, {0, 0, 0}}).

%% {{Year, Month, Day}, {Hour, Minite, Second}}
local_time() ->
    calendar:local_time().

% 时间转时间戳，格式：{{2013,11,13}, {18,0,0}}
-define(SECONDS_FROM_0_TO_1970, 62167219200).
datetime_to_timestamp({{_Y, _M, _D}, {_H, _Mi, _S}} = Date) ->
        Now = os:timestamp(),
        LocalTime = calendar:now_to_local_time(Now),
        UTCTime = calendar:now_to_universal_time(Now),
        
        calendar:datetime_to_gregorian_seconds(Date) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}) -
        %% 时区修正
        calendar:datetime_to_gregorian_seconds(LocalTime) + 
        calendar:datetime_to_gregorian_seconds(UTCTime).
 
% 时间戳转时间
timestamp_to_datetime(Seconds) ->
    Now = os:timestamp(),
    LocalTime = calendar:now_to_local_time(Now),
    UTCTime = calendar:now_to_universal_time(Now),

    Second1 = Seconds + ?SECONDS_FROM_0_TO_1970 + 
    %% 时区修正
    calendar:datetime_to_gregorian_seconds(LocalTime) - calendar:datetime_to_gregorian_seconds(UTCTime),
    calendar:gregorian_seconds_to_datetime(Second1).

%% 时间格式化
format_utc_timestamp() ->
    TS = {_,_,Micro} = os:timestamp(),
    {{Year,Month,Day},{Hour,Minute,Second}} = 
	calendar:now_to_universal_time(TS),
    Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul",
			  "Aug","Sep","Oct","Nov","Dec"}),
    io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w",
		  [Day,Mstr,Year,Hour,Minute,Second,Micro]).

term_to_bitstring(T) ->
    NT = io_lib:format("~w",[T]),
    list_to_bitstring(NT).

bitstring_to_term(BT) ->
    {ok,Tokens,_} = erl_scan:string(bitstring_to_list(BT)++"."),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

fbin(Bin, Args) ->
    list_to_binary(io_lib:format(Bin, Args)).

fbin(Bin) ->
    fbin(Bin, []).