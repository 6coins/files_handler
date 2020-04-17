%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     程序负责
%%%         csv工具模块
%%% @end
%%%-------------------------------------------------------------------
-module(csv_util).

-export([format/1]).

-type values_list():: [values()].           % 矩阵对应的列表
-type values() :: [value()].                % 矩阵一行值对应的列表
-type value() :: string()|number()|atom().  % 值
-type csv_data() :: string().               % csv文件的内容


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 生成写入csv文件的内容(即加逗号以分隔格子内容,并再每行加/n)
%% @end
-spec format(values_list()) -> csv_data().
%%--------------------------------------------------------------------
format(V_lists) ->
    V_lists1 = [add_csv_format(List) || List <- V_lists],  % 每行加逗号和/n
    _Data = lists:concat(V_lists1).                   % 把所有行加在一起


%% 每行加双引号和逗号和/n
add_csv_format(List) ->
    %List1 = lists:foldl(fun(E, A_in) ->  A_in ++ [E, ","] end, [], List), % 加逗号
    List1 = lists:foldl(fun(E, A_in) ->  A_in ++ ["\"", E, "\"" ","] end, [], List), % 加双引号和逗号
    List2 = List1 ++ ["\n"],                                              % 加\n
    _List3 = lists:concat(List2).                                         % 合成字符串


