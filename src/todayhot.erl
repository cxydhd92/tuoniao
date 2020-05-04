-module(todayhot).
-include("common.hrl").
-export([start/0, stop/0]).
 
start() ->
    ?INFO("start run todayhot",[]),
    lager:start(),
    inets:start(),
    ibrowse:start(),
    ok = application:start(mysql),
    ok = application:start(poolboy),
    ok = application:start(mysql_poolboy),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    % ok = application:start(iconv),
    ok = application:start(todayhot),
    ?INFO("start run todayhot finish",[]),
    ok.

stop() ->
    application:stop(todayhot),
    application:stop(mysql_poolboy),
    application:stop(poolboy),
    application:stop(mysql),
    timer:apply_after(500, erlang, halt, [0]),
    ok.