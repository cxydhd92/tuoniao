%% ------------------------------------------------------------------
%% 测试模块
%% ------------------------------------------------------------------
-module(test).
-include("common.hrl").
-include("todayhot.hrl").
-include_lib("xmerl/include/xmerl.hrl").  
-compile(export_all).

test() ->
	case ibrowse:send_req("https://s.weibo.com/top/summary?cate=realtimehot", [], get) of
		{ok, "200", _ResponseHeaders, Body} ->
			{_, Container} = re:run(Body, <<"<tbody>([\\s\\S]+?)</tbody>">>, [{capture, all_but_first, binary}, global]),
			{_, Item} = re:run(Container, <<"<tr.+?class=\"\">\\s+?<td.+?class=\"td-\\d+.+?ranktop\">([\\s\\S]+?)</tr>">>, [{capture, all_but_first, binary}, global]),
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

test_xml() ->
	case ibrowse:send_req("https://rss.huxiu.com/", [], get) of
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