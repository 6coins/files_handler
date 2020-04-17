%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright
%%% @doc
%%%     脚本模块
%%% @end
-module(sdxj_script).

-export([is_start_fun/1, 
         is_end_fun/1, 
         content_fun/1]).
-define(IDS4, % 4.0机器
        ["sjyhwebjr10", "sjyhwebjr11", "sjyhwebjr12",
         "sjyhwebjr1", "sjyhwebjr2", "sjyhwebjr3", "sjyhwebjr4", 
         "sjyhwebjr5", "sjyhwebjr6", "sjyhwebjr7", "sjyhwebjr8", 
         "sjyhwebjr9", 

         "sjyhapp10", "sjyhapp11", "sjyhapp12", "sjyhapp13", "sjyhapp14",
         "sjyhapp1", "sjyhapp2", "sjyhapp3", "sjyhapp4", 
         "sjyhapp5", "sjyhapp6", "sjyhapp7", "sjyhapp8", 
         "sjyhapp9", 

         "sjyhmem1", "sjyhmem2", "sjyhmem3", "sjyhmem4", "sjyhpic1", "sjyhpic2", "sjyhcj1", "sjyhcj2", "sjyhmg1", "sjyhmg2",

         "sjyhtsapp1", "sjyhtsapp2", "sjyhtsmng1", "sjyhtsmng2", "sjyhxxts1", "sjyhxxts2"]).

-define(IDS3, %3.0机器
        ["sjdb1", "sjdb2", "sjmg1", "sjmg2", "sjlog", "sjjrjr5", "sjjrjr1", "sjjrap5", "sjjrap1"]). 


-define(DIRS, ["/logs", "/ewp", "/gtp", "/redis", "/oracle", "/baklogs"]).


%% 判断当前行是否是章开头的函数
is_start_fun(Line) ->
    files_handler_util:is_cantain(Line, "hxb@").                   % 以"####"区分开头


%% 判断当前行是否是章结尾的函数
is_end_fun(Line) ->
    files_handler_util:is_cantain(Line, "hxb@").                   % 以"####"区分结尾


%% 处理章每行内容时,非标题行才处理累加
content_fun(Line) ->
    case files_handler_util:is_cantain(Line, "hxb@") of             % 是否是开头或结尾
        true ->
            RE = "(" ++ string:join(?IDS4 ++ ?IDS3, "|") ++ ")",
           [_,ID|_] = re:split(Line, RE, [{return,list}]),
            {[{id, ID}], []};
        false ->
            RE2 = "(" ++ string:join(?DIRS, "|") ++ ")",
            case re:split(Line, RE2, [{return,list}]) of
                [XX, Mounted_on, _] ->                              % 匹配到目录
                    case string:tokens(XX, " ") of
                        [Size, _Used, _Avail, Use] ->
                            {[{msu, [{mounted_on, Mounted_on},
                                     {size, Size},
                                     {use, Use}]
                              }],
                             []};
                        [_Filesystem, Blocks, _Free, Used, _Iused, _Iused1] ->
                            {[{msu, [{mounted_on, Mounted_on},
                                     {size, Blocks ++ "G"},
                                     {use, Used}]
                              }],
                             []}
                    end;

                _Other ->
                    {[], []}
            end
    end.


