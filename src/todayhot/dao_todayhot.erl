-module(dao_todayhot).

-include("common.hrl").
-include("todayhot.hrl").

-export([load/0, load_sys_id/0, up_news_db/2, up_db_nodes/1]).

load() ->
    Sql = <<"select node_id, count, add_time, users from todayhot_node ">>,
    {ok, _, Data}  = mysql_poolboy:query(?POOL, Sql),
    Now = util:now(),
    LimitTime = Now - 90*86400,
    Sql1 = <<"select id, node_id, class, abstract, title, url, source, count, sub_news, same_id, img, news_time, time from todayhot_news where time>=?">>,
    {ok, _, Data1}  = mysql_poolboy:query(?POOL, Sql1, [LimitTime]),
    Today = util:today(),
    Fun = fun([NodeId, Count, Time, Users], Acc) ->
        NUsers = util:bitstring_to_term(Users),
        [#todayhot_nodes{node_id = NodeId, count = Count,  users=NUsers, add_time = Time}|Acc]
    end,
    Nodes = lists:foldl(Fun, [], Data),
    Fun1 = fun([Id, NodeId, Class, Abstract, Title, Url, Source, Count, SubNews, SameId, Img, NewsTime, CTime], {Acc1, AccToday}) ->
        NSubNews = util:bitstring_to_term(SubNews),
        case CTime >= Today of
            true ->
                AccToday1 = add_news(AccToday, Today, [Id, NodeId, Class, Abstract, Title, Url, Source, Count, NSubNews, SameId, NewsTime, CTime, Img]),
                {Acc1, AccToday1};
            _ ->
                CToday = util:today(CTime),
                NAcc1 = add_news(Acc1, CToday, [Id, NodeId, Class, Abstract, Title, Url, Source, Count, NSubNews, SameId, NewsTime, CTime, Img]),
                {NAcc1, AccToday}
        end
    end,
    {OL, TL} = lists:foldl(Fun1, {[], []}, Data1),
    {Nodes, OL, TL}.

add_news(Acc, Today, [Id, NodeId, Class, Abstract, Title, Url, Source, Count, SubNews, SameId, NewsTime, CTime, Img]) ->
    case lists:keytake({NodeId, Today}, #todayhot_node_news.node, Acc) of
            {value, TN=#todayhot_node_news{news = NewsL}, RtAcc1} ->
                News = #todayhot_news{id = Id, node_id = NodeId, class = Class, same_id=SameId, abstract = Abstract, 
                title = Title, url=Url, count=Count, source=Source, news_time=NewsTime, sub_news=SubNews, time=CTime, img=Img},
                [TN#todayhot_node_news{news = [News|NewsL]}|RtAcc1];
            _ ->
                News = #todayhot_news{id = Id, node_id = NodeId, class = Class, same_id=SameId, abstract = Abstract, 
                title = Title, url=Url, count=Count, source=Source, news_time=NewsTime, sub_news=SubNews, time=CTime, img=Img},
                [#todayhot_node_news{node={NodeId, Today}, news = [News]}|Acc]
    end.

load_sys_id() ->
    Sql = <<"select id from sys_id ">>,
    {ok, _, Data}  = mysql_poolboy:query(?POOL, Sql),
    case Data of
        [[SysId]] -> SysId;
        _ ->
            % ?ERR("load sys id Data ~w",[Data]), 
            1            
    end.

up_sys_id(AId) ->
    SqlSub = "UPDATE sys_id set id = ",
    Sql = list_to_binary(lists:concat([SqlSub, "?"])),
    ok = mysql_poolboy:query(?POOL, Sql, [AId]),
    ok.
    
up_news_db(Changes, AId) ->
    up_sys_id(AId),
    up_news_db(Changes).

up_news_db([]) ->
    ok;
up_news_db(Changes) ->
    Len = length(Changes),
    case Len > 7000 of
        true ->
            SubChanges = lists:sublist(Changes, 7000),
            up_news_db_f1(SubChanges),
            up_news_db(lists:nthtail(7000, Changes));
        _ ->
            up_news_db_f1(Changes)
    end.

up_news_db_f1(Changes) ->
    Fun = fun(#todayhot_news{id=Id, class=Class, node_id = NodeId, abstract=Abstract, title = Title, url=Url, count=Count, source=Source, news_time=NewsTime, sub_news=SubNews, same_id=SameId, img=Img, time=Time}, NewsAcc) ->
        [Id, NodeId, Class, Abstract, Title, Url, Source, Count, NewsTime, util:term_to_bitstring(SubNews), SameId, Img, Time | NewsAcc]
    end,
    NNewsL = lists:foldl(Fun, [], Changes),
    do_up_db_news(NNewsL),
    ok.

up_db_nodes([]) -> ok;
up_db_nodes(NNodesL) ->
    Fun = fun(#todayhot_nodes{node_id=NodeId, count = Count, add_time=AddTime, users = Users}, NewsAcc) ->
        [NodeId, Count, AddTime, util:term_to_bitstring(Users) | NewsAcc]
    end,
    NewNodeL = lists:foldl(Fun, [], NNodesL),
    Len = length(NNodesL),
    SqlSub = "REPLACE INTO todayhot_node(node_id, count, add_time, users) VALUES ",
    Sql = list_to_binary(lists:concat([SqlSub, lists:duplicate(Len-1, "(?,?,?,?),"), "(?,?,?,?)"])),
    ok = mysql_poolboy:query(?POOL, Sql, NewNodeL),
    ok.

do_up_db_news(NNewsL) ->
    Len = length(NNewsL) div 13,
    SqlSub = "REPLACE INTO todayhot_news(id, node_id, class, abstract, title, url, source, count, news_time, sub_news, same_id, img, time) VALUES ",
    Sql = list_to_binary(lists:concat([SqlSub, lists:duplicate(Len-1, "(?,?,?,?,?,?,?,?,?,?,?,?,?),"), "(?,?,?,?,?,?,?,?,?,?,?,?,?)"])),
    ok = mysql_poolboy:query(?POOL, Sql, NNewsL),
    ok.
