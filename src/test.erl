%% ------------------------------------------------------------------
%% 测试模块
%% ------------------------------------------------------------------
-module(test).
-include("common.hrl").
-include("todayhot.hrl").
-include("cfg_news_source.hrl").
-include_lib("xmerl/include/xmerl.hrl").  
-compile(export_all).

test_html(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url}, TodayData, Now) ->
	case ibrowse:send_req(?b2l(Url), [{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			#cfg_news_source{data=Data, container=ContainerF, title=TitleF, link_a=LinkA,
			desc=DescF, author=AuthorF, img=ImgF, count=CountF, time=TimeF} = Cfg,
			?INFO("xxxxxxBody~w",[length(Body)]),
			{_, Container} = ?IF(Data=:=<<"">>, {ok, Body}, re:run(Body, Data, [{capture, first, binary}, global])),
			?INFO("xxxxxx~ts",[Container]),

			{_, Item} = ?IF(ContainerF=:=<<"">>, {ok, Container}, re:run(Container, ContainerF, [{capture, first, binary}, global])),
			{_, ItemTitleL} = re:run(Item, TitleF, [{capture, all_but_first, binary}, global]),
			{_, ItemLinkAL} = re:run(Item, LinkA, [{capture, all_but_first, binary}, global]),
			?INFO("ItemLinkAL ~w",[length(ItemLinkAL)]),
			Fun = fun([Link]) ->
				?INFO("ItemLinkAL ~ts",[Link])
			end,
			lists:foreach(Fun, ItemLinkAL),
			Fun1 = fun([Title]) ->
				?INFO("ItemLinkAL ~ts",[Title])
			end,
			lists:foreach(Fun1, ItemTitleL);
		_Err ->
			?ERR("Url ~ts fail ~w", [Url, _Err]),
			ok
    end.

test() ->
	case ibrowse:send_req("https://s.weibo.com", [], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			?INFO("xBoday~w",[Body]),
			{_, Container} = re:run(Body, <<"<tbody>([\\s\\S]+?)</tbody>"/utf8>>, [{capture, all_but_first, binary}, global]),
			{_, Item} = re:run(Container, <<"<li[^>]+?>\\s+?<span[^>]+?>([\\s\\S]+?)</li>">>, [{capture, all_but_first, binary}, global]),
			{_, [ItemTitle|_]} = re:run(Item, <<"<a[^>]+?>([\\s\\S]+?)</a>">>, [{capture, all_but_first, binary}, global]),
			{_, [[ItemUrl]|_]} = re:run(Item, <<"href=\"([^\"]+?)\"">>, [{capture, all_but_first, binary}, global]),
			
			% ?INFO("Container~ts",[?l2b(Container)]),
			?INFO("ItemTitle~ts",[ItemTitle]),
			?INFO("ItemTitle~ts",[fbin(<<"~s~s"/utf8>>, [<<"https://s.weibo.com">>, ItemUrl])]),
			ok;
		_Err ->
			?ERR("fail ~w", [_Err])
    end,
	ok.

test_json(Cfg=#cfg_news_source{class=Class, source_id=SourceId, url = Url,head=Head, data=Data}, TodayData, Now) ->
    case ibrowse:send_req(?b2l(Url), Head++[{"user-agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.3987.149 Safari/537.36"}], get) of
		{ok, "200", _ResponseHeaders, ResponseBody} ->
			BodyJsonBin = list_to_binary(ResponseBody),
			BodyTerm = jsx:decode(BodyJsonBin),
			DataL = get_data(Data, BodyTerm, []),
			?INFO("DataL ~ts", [DataL]);
		_Err ->
			?ERR("Url ~ts fail ~w", [Url, _Err]),
			TodayData
	end.

get_data(<<"">>, _Body, Default) -> Default;
get_data(Data, Body, Default) ->
	DataStr = ?b2l(Data),
	DataL = string:tokens(DataStr, "|"),
	get_data_f1(DataL, Body, Default).

get_data_f1([], Datas, _) -> Datas;
get_data_f1([Data|L], Body, Default) ->
	case proplists:get_value(?l2b(Data), Body) of
		DataS when DataS =/= undefined ->
			NDataS = case DataS of
				[DataL] when is_list(DataL) -> DataL;
				[_|_] -> DataS;
				_ ->
					case catch jsx:decode(DataS) of
						JDataS when is_list(JDataS) ->
							JDataS;
						_ ->
							DataS
					end
			end,
			get_data_f1(L, NDataS, Default);
		_ ->
			Default
	end.

test_xml() ->
	case ibrowse:send_req("https://rss. huxiu.com/", [], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			{XmlDoc, _B} = xmerl_scan:string(Body),
			 Items = xmerl_xpath:string("/rss/channel/item",XmlDoc),  
			lists:foreach(fun(Item)->  
                                     [#xmlText{value=IdValue}] = xmerl_xpath:string("/item/id/text()",Item),  
                                     [#xmlText{value=TItleValue}] = xmerl_xpath:string("/item/title/text()",Item),  
                                    	
                                     ?INFO("IdValue~ts ~ts",[IdValue, TItleValue])
                             end,  
                             Items),  
			?INFO("Items~w",[Items]),
			% ?INFO("ItemTitle~w",[Doc]),
			ok;
		_Err ->
			?ERR("fail ~w", [_Err])
    end,
	ok.

xml_namespace() ->
    "<?xml version=\"1.0\"?>"
	"<!-- initially, the default namespace is \"books\" -->"
	"<book xmlns='urn:loc.gov:books' xmlns:isbn='urn:ISBN:0-395-36341-6'>"
	"<title>Cheaper by the Dozen</title>"
	"<isbn:number>1568491379</isbn:number>"
	"<notes>"
	"<!-- make HTML the default namespace for some comments -->"
	"<p xmlns='urn:w3-org-ns:HTML'>"
	"This is a <i>funny</i> book!"
	"</p>"
	"</notes>"
	"</book>".

fbin(Bin, Args) ->
    list_to_binary(io_lib:format(Bin, Args)).

fbin(Bin) ->
    fbin(Bin, []).