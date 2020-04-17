%% coding: latin-1 %% Erlang/OTP 17.0之后erlang程序文件的代码中有中文必须加这个
%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 2014 duanzhichao
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(files_handler_util).

-export([is_cantain/2, 
         replace/3,
         split/2,
         fold_file/3, 
         proplists_update/2, 
         string_to_date/2,
         date_to_string/2,
         day_of_the_week/1,
         get_before_day/1]).


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Str1是否包含Str2(注意如果是中文只能是utf-8编码)
%% @end
-spec is_cantain(Str1 :: string(), Str2 :: string()) -> boolean().
%%--------------------------------------------------------------------
is_cantain(Str1, Str2) ->
    UStr1 = unicode:characters_to_list(list_to_binary(Str1), unicode),     % 转码(Erlang/OTP 17.0之前)
    UStr2 = unicode:characters_to_list(list_to_binary(Str2), unicode),     % 转码(Erlang/OTP 17.0之前)
    case re:run(UStr1, UStr2, [unicode]) of                                 % 如果Str1是否包含Sr2 
        {match, _} -> true;                                                 % 返回true
        _ -> false                                                          % 返回false
    end.


%%--------------------------------------------------------------------
%% @doc
%% 替换字符串内容(注意如果是中文只能是utf-8编码)
%% @end
-spec replace(Subject :: string(), RE :: re:mp(), Replacement :: string()) -> Result :: string().
%%--------------------------------------------------------------------
replace(Subject, RE, Replacement) ->
    re:replace(Subject, RE, Replacement, [unicode, global, {return, list}]).


%%--------------------------------------------------------------------
%% @doc
%% 分隔字符串(注意如果是中文只能是utf-8编码)
%% @end
-spec split(Subject :: string(), [RE :: re:mp()]) -> Result :: [string()].
%%--------------------------------------------------------------------
split(Subject, RE_list) ->
    Fun = 
        fun(RE, {Rest1, Result1}) -> 
            [Res | _] = re:split(Rest1, RE, [unicode, {return,list}]),      % 如["Qq:123" | _] = re:split("Qq:123 Rr1:456 Rr2:789 Aa:000", "Rr", [unicode, {return,list}]),
            Rest2 = Rest1 -- Res,                                           % 如"Rr1:456 Rr2:789 Aa:000" = "Qq:123 Rr1:456 Rr2:789 Aa:000" -- "Qq:123",
            Result2 = Result1 ++ [Res],                                     % 如["Qq:123"] = [] ++ ["Qq:123"],
            {Rest2, Result2}
        end,
    {Rest_out, Result_out} = lists:foldl(Fun, {_Rest = Subject, _Result = []}, RE_list),    % 如{"Aa:000", ["Qq:123", "Rr1:456 Rr2:789]} = lists:foldl(Fun, {"Qq:123 Rr1:456 Rr2:789 Aa:000", []}, ["Rr", "Aa"]),
    Result_out ++ [Rest_out].


%%--------------------------------------------------------------------
%% @doc
%% 依次读取文件的每一行并处理结果,最后返回所有处理结果(类似lists:foldl/3)
%% @end
-spec fold_file(Fun, Acc0, Filename) -> Acc1 when
    Fun :: fun((Data, AccIn) -> AccOut),    % 处理一行数据的函数
        Data :: string(),                                        % 一行数据
        AccIn :: Acc,                                                % 输入的累加器的值 
        AccOut :: Acc,                                               % 输出的累加器的值 
    Acc0 :: Acc,                            % 初始的累加器的值
    Filename :: file:name(),                % 文件路径
    Acc1 :: Acc,                            % 最终的累加器的值
    Acc :: {error, Reason} | term(),        % 累加器(如果累加过程中发生错误返回错误)
    Reason :: term().                       % 错误原因
%%--------------------------------------------------------------------
fold_file(Fun, Acc0, Filename) -> 
    {ok,IO}=file:open(Filename, read),          % 打开文件
    Acc1 = fold_line(IO, Fun, Acc0, Filename),  % 依次读取文件的每一行并处理结果,最后返回所有处理结果
    file:close(IO),                             % 关闭文件
    Acc1.


%%--------------------------------------------------------------------
%% @doc
%% 根据key,把Proplist1中的内容更新到Proplist2(如果key存在就更新,不存在就插入),得到Proplist3
%% @end
-spec proplists_update(Proplist1 :: [proplists:property()], Proplist2 :: [proplists:property()]) -> Proplist3 :: [proplists:property()].
%%--------------------------------------------------------------------
proplists_update(Proplist1, Proplist2) ->
    Keys1 = proplists:get_keys(Proplist1),                                          % 得到Proplist1的key列表
    {_Keys1_proplist2, Nokeys1_proplist2} = proplists:split(Proplist2, Keys1),      % 把Proplist2中不包含Proplist1的key的部分分离出来
    Proplist3 = Proplist1 ++ Nokeys1_proplist2,                                     % 用Proplist1和Proplist2中不包含Proplist1的key的部分合起来
    Proplist3.


%%--------------------------------------------------------------------
%% @doc
%% 把形如"2018.01.02"转成(如果分隔符号用"."){2018, 1, 2}
%% @end
-spec string_to_date(Str_date :: string(), SP :: string()) -> Date :: calendar:date().
%%--------------------------------------------------------------------
string_to_date(Str_date, SP) ->
    case SP of 
        "" ->                                                                       % 如果分隔符是"",则不去掉分隔符
            Str_date1 = Str_date;
        _ ->
            Str_date1 = re:replace(Str_date, "\\" ++ SP, "", 
                                   [global, {return, list}])                        % 否则去掉分隔符
    end,
    Y = list_to_integer(string:substr(Str_date1, 1, 4)),                            % 前4位是年
    M = list_to_integer(string:substr(Str_date1, 5, 2)),                            % 接下来2位是月
    D = list_to_integer(string:substr(Str_date1, 7, 2)),                            % 接下来2位是日
    {Y, M, D}.


%%--------------------------------------------------------------------
%% @doc
%% 把形如{2018, 1, 2}转成(如果分隔符号用".")"2018.01.02"
%% @end
-spec date_to_string(Date :: calendar:date(), SP :: string()) -> Str_date :: string().
%%--------------------------------------------------------------------
date_to_string({Y, M, D}, SP) ->
    string:join([integer_to_list(Y),
                 string:right(integer_to_list(M), 2, $0),
                 string:right(integer_to_list(D), 2, $0)], SP).


%%--------------------------------------------------------------------
%% @doc
%% 得到周几(如{2018,02,01},返回"周四")
%% @end
-spec day_of_the_week(Date :: calendar:date()) -> Day :: string().
%%--------------------------------------------------------------------
day_of_the_week(Date) ->
    D_list = ["周一", "周二", "周三", "周四", "周五", "周六", "周日"],  % 周顺序列表
    Day_num = calendar:day_of_the_week(Date),                           % 得到周序号
    lists:nth(Day_num, D_list).                                         % 得到周的中文字符串 


%%--------------------------------------------------------------------
%% @doc
%% 得到前一天
%% @end
-spec get_before_day(Str_date1 :: string()) -> Str_date2 :: string().
%%--------------------------------------------------------------------
get_before_day(Str_date1) -> 
    Date1 = string_to_date(Str_date1, ""),                              % 把字符串转成日期(以便计算)
    Days = calendar:date_to_gregorian_days(Date1),                      % 得到总天数
    Before_days = Days - 1,                                             % 得到前一天的总天数
    Date2 = calendar:gregorian_days_to_date(Before_days),               % 得到前一天的date()
    _Str_date2 = date_to_string(Date2, ".").                            % 把日期转成字符串


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 依次读取文件的每一行并处理结果,最后返回所有处理结果
%% @end
%%--------------------------------------------------------------------
fold_line(IO, Fun, Acc0, Filename) ->
    case io:get_line(IO,'') of                                                      % 读一行数据
        eof ->                                                                      % 如果是文件末尾
            Acc0;                                                                       % 返回累加器
        {error, Reason} ->                                                          % 如果发生报错
            {error, Reason};                                                            % 返回错误
        Data ->                                                                     % 如果正常读取一行数据     
            AccOut = Fun(Data, Acc0),                                                   % 处理结果
            fold_line(IO, Fun, AccOut, Filename)                                        % 读取下一行处理
    end.



