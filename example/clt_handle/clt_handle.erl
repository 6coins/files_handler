-module(clt_handle).


%% Application callbacks
-export([run/1
         ]).

run(Dir) ->
    %Fun = fun(Filename, AccIn) -> [files_handler_util:fold_file(fun clt_handle:line_Fun/2, [], Filename)|AccIn] end,    % 每个文件的处理函数
    Fun = fun(Filename, Acc) -> 
            [{Filename, files_handler_util:fold_file(fun line_Fun/2, [], Filename)} | Acc] end,                         % 每个文件的处理函数
    Filename_matrix_list = filelib:fold_files(Dir, ".*log$", false, Fun, []),                                        % 处理目录下所有文件
    FunFM = fun(F,M) ->
        Rows = simple_matrix:get_rows(M),
        From_clumns = ["send_smsic2_new", "DS0402"],
        To_clumn = "result",
        Clumns_fun = 
            fun(V1, V2) when (V1=:=undefined) or (V2=:=undefined) -> 
                    undefined;
               (V1, V2) -> 
                    simple_matrix:to_number(V2) - simple_matrix:to_number(V1) 
            end,              % 相减
        MatrixOut2 = simple_matrix:clumns_fun(From_clumns, To_clumn, Rows, Clumns_fun, M),              % 得到差时间
        io:format("write file--------~p~n",[F ++ ".csv"]),
        ok = simple_matrix:generate_csv(F ++ ".csv", ["mobileno"] ++ From_clumns ++ [To_clumn], Rows, MatrixOut2)
    end,
    [FunFM(F, M) || {F, M} <-Filename_matrix_list].


%% 行处理函数
line_Fun(Line, MatrixIn) -> 
    CFun = fun(Line1, MatrixIn1, Flag) ->
            %io:format("0--------~p~n",[Line1]),
            %Tokens1 = string:tokens(Line1, " "),    % 先用" "切分
            %io:format("1--------~p~n",[Tokens1]),
            %Tokens2 = lists:append([string:tokens(T, "\t") || T <- Tokens1]),                % 再用"\t"切分
            Tokens2 = string:tokens(Line1, "\t"),                % 再用"\t"切分
            %io:format("2--------~p~n",[Tokens2]),
            [Datetime,_,Request,_,Session_id,Dev,_,_] = Tokens2,
            Seconds = get_seconds(Datetime),
            [Mac, _, _] = string:tokens(Dev, "/"),
            MatrixOut1 = simple_matrix:put_elem({Flag, Mac ++ "__" ++ Session_id, Seconds}, MatrixIn1),
            Plist = request_to_proplist(Request),
            case proplists:get_value("mobileno", Plist, undefined) of
                undefined ->MatrixOut1; 
                Mobileno -> simple_matrix:put_elem({"mobileno", Mac ++ "__" ++ Session_id, Mobileno}, MatrixOut1)
            end
    end,    % 得到把设备id+sessiond_id,标识,日期时间对应的秒数作为元素放入矩阵
    case {files_handler_util:is_cantain(Line, "send_smsic2_new") , files_handler_util:is_cantain(Line, "DS0402")} of % 判断是否包含特殊字符
        {true, false} ->        % 如包含send_smsic2_new
            CFun(Line, MatrixIn, "send_smsic2_new");
        {false, true} ->        % 如包含DS0402
            CFun(Line, MatrixIn, "DS0402");
        _ ->                    % 否则
            MatrixIn                % 不处理
    end.

%% 得到日期时间字符串对应的秒数
get_seconds(Datetime_str) ->
    YYYY = string:substr(Datetime_str, 2, 4),       % 年
    MM = string:substr(Datetime_str, 7, 2),         % 月
    DD = string:substr(Datetime_str, 10, 2),        % 日
    HH = string:substr(Datetime_str, 13, 2),         % 时
    MI = string:substr(Datetime_str, 16, 2),         % 分
    SS = string:substr(Datetime_str, 19, 2),         % 秒
    Datetime = {{list_to_integer(YYYY), list_to_integer(MM), list_to_integer(DD)}, 
                {list_to_integer(HH), list_to_integer(MI), list_to_integer(SS)}},
    calendar:datetime_to_gregorian_seconds(Datetime).


%% 把request解析成proplist
request_to_proplist(Request)->
    Tokens1 = string:tokens(Request, "&"),
    Fun = fun(T) -> 
            %io:format("2--------~p~n",[T]),
            case string:tokens(T, "=") of
                [K, V] -> {K, V};
                Other -> Other
            end
          end,
    [Fun(T) || T <- Tokens1]. 


%% 得到日期时间字符串对应的秒数
%get_seconds2(Date, Time) ->
%    YYYY = string:substr(Date, 2, 4),       % 年
%    MM = string:substr(Date, 7, 2),         % 月
%    DD = string:substr(Date, 10, 2),        % 日
%    HH = string:substr(Time, 1, 2),         % 时
%    MI = string:substr(Time, 4, 2),         % 分
%    SS = string:substr(Time, 7, 2),         % 秒
%    DateTime = {{list_to_integer(YYYY), list_to_integer(MM), list_to_integer(DD)}, 
%                {list_to_integer(HH), list_to_integer(MI), list_to_integer(SS)}},
%    calendar:datetime_to_gregorian_seconds(DateTime).
%
