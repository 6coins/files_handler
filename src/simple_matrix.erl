%%%-------------------------------------------------------------------
%%% @author duanzhichao
%%% @copyright 
%%% @doc
%%%     程序负责
%%%         矩阵的简单操作
%%% @end
%%%-------------------------------------------------------------------
-module(simple_matrix).

-export([new/0,
         get_sub/3,
         put_elem/2,
         get_elem/3,
         get_clumns/1,
         get_rows/1,
         get_elem_value/3,
         put_elem_by_list/2,
         to_number/1,
         get_max/1, 
         get_max/4, 
         get_number_max/3, 
         values_list_to_matrix/3,
         matrix_to_values_list/3,
         generate_csv/4,
         clumns_fun/5,
         clumns_fun_v_list/4,
         rows_fun/5,
         rows_fun_v_list/4,
         put_values_list/4,
         values_list_add_clums_rows/3,
         values_list_add_clums_rows/4
                ]).

%% 调试用
%-compile(export_all).


%% 公共类型说明
-type matrix() :: [] | [elem()].            % 简易矩阵
-type elem() :: {clumn(), row(),value()}.  % 矩阵中的元素
-type rows() :: [row()].                    % 行编号的列表(用于指定行顺序)
-type clumns() :: [clumn()].                % 列编号的列表(用于指定列顺序)
-type row() :: string()|number()|atom().    % 行
-type clumn() :: string()|number()|atom().  % 列
-type values_list():: [values()].           % 矩阵对应的列表
-type values() :: [value()].                % 矩阵一行值对应的列表
-type value() :: string()|number()|atom().  % 值


%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 创建一个矩阵
%% @end
-spec new() -> matrix().
%%--------------------------------------------------------------------
new() -> [].


%%--------------------------------------------------------------------
%% @doc
%% 得到子矩阵
%% @end
-spec get_sub(clumns(), rows(), Matrix :: matrix()) -> Sub_matrix :: matrix().
%%--------------------------------------------------------------------
get_sub(Clumns, Rows, Matrix) -> 
    Pred = fun({Clumn1, Row1, _}) -> lists:member(Clumn1, Clumns) and lists:member(Row1, Rows) end,
    lists:filter(Pred, Matrix).


%%--------------------------------------------------------------------
%% @doc
%% 放入矩阵(如果行列存在就更新)
%% @end
-spec put_elem(Elem :: elem(), Matrix1 :: matrix()) -> Matrix2 :: matrix().
%%--------------------------------------------------------------------
put_elem({Clumn, Row, _} = Elem, Matrix1) -> 
    Pred = fun({Clumn1, Row1, _}) -> (Clumn1 =:= Clumn) and (Row1 =:= Row) end,     %
    {_Satisfying, NotSatisfying} = lists:partition(Pred, Matrix1),                  % 把不同行列的元素分离出来
    _Matrix2 = [Elem | NotSatisfying].                                              % 用其和新元素组成新矩阵


%%--------------------------------------------------------------------
%% @doc
%% 得到元素
%% @end
-spec get_elem(clumn(), row(), matrix()) -> elem() | undefined.
%%--------------------------------------------------------------------
get_elem(Clumn, Row, Matrix) -> 
    case get_sub([Clumn], [Row], Matrix) of                                          % 根据行列,过滤出元素
        [] -> 
            undefined;
        [Elem] -> 
            Elem
    end.


%%--------------------------------------------------------------------
%% @doc
%% 得到列编号(排序的)
%% @end
-spec get_clumns(matrix()) -> clumns().
%%--------------------------------------------------------------------
get_clumns(Matrix) -> 
    Set1 = sets:new(),
    Set2 = lists:foldl(fun({C, _, _}, Set_in) -> sets:add_element(C, Set_in) end, Set1, Matrix),
    lists:sort(sets:to_list(Set2)).


%%--------------------------------------------------------------------
%% @doc
%% 得到行编号(排序的)
%% @end
-spec get_rows(matrix()) -> rows().
%%--------------------------------------------------------------------
get_rows(Matrix) -> 
    Set1 = sets:new(),
    Set2 = lists:foldl(fun({_, R, _}, Set_in) -> sets:add_element(R, Set_in) end, Set1, Matrix),
    lists:sort(sets:to_list(Set2)).


%%--------------------------------------------------------------------
%% @doc
%% 得到元素的值
%% @end
-spec get_elem_value(clumn(), row(), matrix()) -> value() | undefined.
%%--------------------------------------------------------------------
get_elem_value(Clumn, Row, Matrix) ->
    case get_elem(Clumn, Row, Matrix) of
        {_, _, V} -> V;
        undefined -> undefined
    end.


%%--------------------------------------------------------------------
%% @doc
%% 用元素列表放入矩阵
%% @end
-spec put_elem_by_list(Elem_list :: [elem()], Matrix1 :: matrix()) -> Matrix2 :: matrix().
%%--------------------------------------------------------------------
put_elem_by_list(Elem_list, Matrix1) -> 
    Fun = fun(Elem, Matrix_in) -> put_elem(Elem, Matrix_in) end,%
    _Matrix2 = lists:foldl(Fun, Matrix1, Elem_list).             % 把列表中元素依次放入到矩阵中


%%--------------------------------------------------------------------
%% @doc
%% 转成数字
%% @end
-spec to_number(Value1) -> Value2 | {error, {Value1, no_number}} when
    Value1 :: term(),
    Value2 :: number().
%%--------------------------------------------------------------------
to_number(Value1) when is_number(Value1)->                           % 如果是数字
    Value1;                                                         % 直接返回 
to_number(Value1) when is_list(Value1)->                             % 如果是字符串
    case {string:to_float(Value1), string:to_integer(Value1)} of    % 尝试把字符串分别转成浮点数和整数
        {{Float,[]}, _} -> Float;                                   % 如能转成浮点数,返回
        {_, {Int,[]}} -> Int;                                       % 如能转成整数,返回
        _ -> {error, {Value1, no_number}}                           % 如果都不能转,返回错误
    end;
to_number(Value1) ->                                            % 如果是其他
    {error, {Value1, no_number}}.                                   % 返回错误


%%--------------------------------------------------------------------
%% @doc
%% 得到矩阵中最大的元素列表(如果元素值是字符串先转成浮点再比较大小)
%% @end
-spec get_max(Matrix :: matrix()) -> Elem_list :: [elem()].
%%--------------------------------------------------------------------
get_max(Matrix) -> 
    Clumns = get_clumns(Matrix),                            % 得到列编号
    Rows = get_rows(Matrix),                                % 得到行编号
    Fun = fun(V1, V2) -> to_number(V1) >= to_number(V2) end,% 先转成数据再较大小时
    get_max(Clumns, Rows, Fun, Matrix).                     % 得到最大值列表


%%--------------------------------------------------------------------
%% @doc
%% 得到矩阵中最大的元素列表
%% @end
-spec get_max(Clumns, Rows, Fun, Matrix) -> Elem_list when
    Clumns :: clumns(),                 % 指定列范围
    Rows :: rows(),                     % 指定行范围
    Fun :: fun((V1 :: value(), V2 :: value()) -> boolean()),  % 判断两个元素大小的函数
    Matrix :: matrix(),                 % 矩阵
    Elem_list :: [elem()].              % 最大的元素列表
%%--------------------------------------------------------------------
get_max(Clumns, Rows, Fun, Matrix) -> 
    Sub_matrix = get_sub(Clumns, Rows, Matrix),         % 得到子矩阵
    Sortfun = fun({_, _, V1}, {_, _, V2}) -> Fun(V1, V2) end,
    SortList = lists:sort(Sortfun, Sub_matrix),         % 排序
    {_, _, Max_value} = hd(SortList),                   % 取第一元素(即最大的),得到最大的值
    Pred = fun({_, _, V}) -> V =:= Max_value end,       % 
    {Satisfying, _} = lists:partition(Pred, SortList),  % 把同样有最大值的元素分离出来
    _Elem_list = Satisfying.


%%--------------------------------------------------------------------
%% @doc
%% 得到矩阵中把值转成数字后最大的元素列表
%% @end
-spec get_number_max(Clumns, Rows, Matrix) -> Elem_list when
    Clumns :: clumns(),                 % 指定列范围
    Rows :: rows(),                     % 指定行范围
    Matrix :: matrix(),                 % 矩阵
    Elem_list :: [elem()].              % 最大的元素列表
%%--------------------------------------------------------------------
get_number_max(Clumns, Rows, Matrix) -> 
    Fun = fun(V1, V2) -> to_number(V1) >= to_number(V2) end,% 先转成数据再较大小时
    get_max(Clumns, Rows, Fun, Matrix).                     % 得到最大值列表



%%--------------------------------------------------------------------
%% @doc
%% 把列表转化成矩阵
%% @end
-spec values_list_to_matrix(clumns(), rows(), values_list()) -> matrix().
%%--------------------------------------------------------------------
values_list_to_matrix(Clumns, Rows, V_list) -> 
    Fun = fun(Elem, A_in) -> _M_out = [Elem | A_in] end,    % 
    _Acc2 = fold(Clumns, Rows, V_list, Fun, _Acc1 = []).   % 依次把V_list中的值组成矩阵的元素累加起来


%%--------------------------------------------------------------------
%% @doc
%% 把矩转化成阵列表(或取指定范围的值列表)
%% @end
-spec matrix_to_values_list(clumns(), rows(), matrix()) -> values_list().
%%--------------------------------------------------------------------
matrix_to_values_list(Clumns, Rows, Matrix) -> 
    Fun = fun(Row) -> [get_elem_value(Clumn, Row, Matrix)|| Clumn <- Clumns] end,
    [Fun(Row) || Row <- Rows].


%%--------------------------------------------------------------------
%% @doc
%% 根据指定的行和列顺序,把矩阵的内容写入csv文件(没有值的格子用undefined填充)
%% @end
-spec generate_csv(file:name(), clumns(), rows(), matrix()) -> ok | term().
%%--------------------------------------------------------------------
generate_csv(Filename, Clumns, Rows, Matrix) -> 
    V_list1 = matrix_to_values_list(Clumns, Rows, Matrix),                      % 把矩阵转成值列表
    V_list2 = values_list_add_clums_rows(Clumns, Rows, V_list1),                % 添加行列标题(相当与添加表格中的行列表头)
    Csv_data = csv_util:format(V_list2),                                        % 生成写入csv文件的内容(即加逗号以分隔格子内容,并再每行加/n)
    file:write_file(Filename, Csv_data).                                        % 写文件


%%--------------------------------------------------------------------
%% @doc
%% 指定矩阵的列范围来计算,把计算结果更新到新的列,返回新矩阵
%% @end
-spec clumns_fun(From_clumns, To_clumn, Rows, Fun, Matrix1) -> Matrix2 when
    From_clumns :: clumns(),    % 按顺序指定要计算的列的编号
    To_clumn :: clumn(),        % 计算后的结果会放入的列的编号
    Rows :: rows(),             % 按顺序指定行的编号
    Fun :: fun((V1 :: value(), V2 :: value()) -> V3 :: value()),  % 两个行间元素的计算规则函数
    Matrix1 :: matrix(),        % 初始矩阵
    Matrix2 :: matrix().        % 最终矩阵
%%--------------------------------------------------------------------
clumns_fun(From_clumns, To_clumn, Rows, Fun, Matrix1) ->
    Res_v_list = clumns_fun_v_list(From_clumns, Rows, Fun, Matrix1),    % 计算生成值列表
    _Matrix2 = put_values_list([To_clumn], Rows, Res_v_list, Matrix1).  % 把结果放入矩阵


%%--------------------------------------------------------------------
%% @doc
%% 指定矩阵的列范围来计算,返回计算结果的值列表
%% @end
-spec clumns_fun_v_list(From_clumns, Rows, Fun, Matrix1) -> Res_v_list when
    From_clumns :: clumns(),    % 按顺序指定要计算的列的编号
    Rows :: rows(),             % 按顺序指定行的编号
    Fun :: fun((V1 :: value(), V2 :: value()) -> V3 :: value()),  % 两个行间元素的计算规则函数
    Matrix1 :: matrix(),        % 初始矩阵
    Res_v_list :: values_list().% 最后结果的值列表
%%--------------------------------------------------------------------
clumns_fun_v_list(From_clumns, Rows, Fun, Matrix1) -> 
    V_list = matrix_to_values_list(From_clumns, Rows, Matrix1),         % 得到指定范围内的值列表
    _Res_v_list = [[foldl_list(Fun, Values)] || Values <- V_list].      % 计算生成值列表


%%--------------------------------------------------------------------
%% @doc
%% 指定矩阵的行范围来计算,把计算结果更新到新的行,返回新矩阵
%% @end
-spec rows_fun(From_rows, To_row, Clumns, Fun, Matrix1) -> Matrix2 when
    From_rows:: rows(),    % 按顺序指定要计算的行的编号
    To_row:: row(),        % 计算后的结果会放入的行的编号
    Clumns :: clumns(),    % 按顺序指定列的编号
    Fun :: fun((V1 :: value(), V2 :: value()) -> V3 :: value()),  % 两个列间元素的计算规则函数
    Matrix1 :: matrix(),        % 初始矩阵
    Matrix2 :: matrix().        % 最终矩阵
%%--------------------------------------------------------------------
rows_fun(From_rows, To_row, Clumns, Fun, Matrix1) -> 
    Res_v_list = rows_fun_v_list(From_rows, Clumns, Fun, Matrix1),      % 得到指定范围内的值列表
    _Matrix2 = put_values_list(Clumns, [To_row], Res_v_list, Matrix1).  % 把结果放入矩阵


%%--------------------------------------------------------------------
%% @doc
%% 指定矩阵的行范围来计算,返回计算结果的值列表
%% @end
-spec rows_fun_v_list(From_rows, Clumns, Fun, Matrix1) -> Res_v_list when
    From_rows:: rows(),         % 按顺序指定要计算的行的编号
    Clumns :: clumns(),         % 按顺序指定列的编号
    Fun :: fun((V1 :: value(), V2 :: value()) -> V3 :: value()),  % 两个列间元素的计算规则函数
    Matrix1 :: matrix(),        % 初始矩阵
    Res_v_list :: values_list().% 最后结果的值列表
%%--------------------------------------------------------------------
rows_fun_v_list(From_rows, Clumns, Fun, Matrix1) -> 
    V_list = matrix_to_values_list(Clumns, From_rows, Matrix1),         % 得到指定范围内的值列表
    Foldl_list_fun = 
        fun(Vs1, Vs2) ->
            [Fun(V1, V2)|| {V1, V2} <- lists:zip(Vs1, Vs2)]             % (计算相邻两行的对应元素,结果新元素)
        end,
    Res_values = foldl_list(Foldl_list_fun, V_list),                    % 依次计算相邻两行,得到最送的行
    _Res_v_list = [Res_values].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 依次把相邻的两个元素做计算,生成的结果和下一个元素做同样计算,并返回最后结果
%% @end
-spec foldl_list(Fun, Elems) -> Res_elem when
    Fun :: fun((E1 :: value(), E2 :: value()) -> E3 :: value()),    % 两个元素间的计算规则函数
    Elems :: [term()],                                              % 元素列表
    Res_elem :: term().                                             % 最终结果
%%--------------------------------------------------------------------
foldl_list(Fun, Elems) ->
    F = fun(E1, {_E0, true}) -> {E1, false};                % 如果是第一个值,只记录,并标记
           (E1,{E0, Is1}) -> {Fun(E0, E1), Is1}             % 不是是第一个值,用之前的值和当前值做计算,结果作为之前值,用于下一次计算
    end,
    {E_out, _} = lists:foldl(F, {undefined, _Is1 = true}, Elems),
    E_out.


%%--------------------------------------------------------------------
%% @doc
%% 更具值列表向矩阵中放入元素
%% @end
-spec put_values_list(clumns(), rows(), values_list(), Matrix1 :: matrix()) -> Matrix2 :: matrix().
%%--------------------------------------------------------------------
put_values_list(Clumns, Rows, V_list, Matrix1) -> 
    Fun = fun(Elem, M_in) -> _M_out = put_elem(Elem, M_in) end,                 % 
    _Matrix2 = fold(Clumns, Rows, V_list, Fun, Matrix1).                        % 依次把V_list中的值放入矩阵
   



%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% 把矩阵对应的值列表加上行列(以方便转成csv)
%% @end
-spec values_list_add_clums_rows(clumns(), rows(), V_list1 :: values_list()) -> V_list2 :: values_list().
-spec values_list_add_clums_rows(Head :: value(), clumns(), rows(), V_list1 :: values_list()) -> V_list2 :: values_list().
%%--------------------------------------------------------------------
values_list_add_clums_rows(Clumns, Rows, V_list1) ->
    values_list_add_clums_rows("", Clumns, Rows, V_list1).


values_list_add_clums_rows(Head, Clumns, Rows, V_list1) ->
    First_values = [Head | Clumns],                         % 第一行格子中的内容(开头要添加一个Head,表示表格左上角格子填充内容)
    R_V_List = lists:zip(Rows, V_list1),                    % 把其压成[{Row, Values}]
    Fun = fun({Row, List}) -> [Row | List] end,             %
    Other_values_list = [Fun(R_V)|| R_V <- R_V_List],       % 把[[1,2],[3,4]]转成[[r1,1,2],[r2,3,4]]
    _V_list2 = [First_values | Other_values_list].          % 添加第一行



%%--------------------------------------------------------------------
%% @private
%% @doc
%% 根据给的列和行编号,取处理值列表中的每个值,并累加处理结果(其必须符合列和行编号的个数)
%% @end
-spec fold(clumns(), rows(), values_list(), Fun, Acc1) -> Acc2 when
    Fun :: fun(({clumn(), row(), value()}, Acc_in) -> Acc_out),     % 处理每个值的函数
    Acc_in :: term(),                                               % 初始化的累加器
    Acc_out :: term(),                                              % 累加结果
    Acc1:: term(),                                                  % 初始化的累加器
    Acc2:: term().                                                  % 累加结果
%%--------------------------------------------------------------------
fold(Clumns, Rows, V_list, Fun, Acc1) -> 
    ok = check_clumns_rows_values_list(Clumns, Rows, V_list),   % 检测行列长度
    Fun1 = 
        fun({Row, Values}, Acc_in1) ->
            Fun2 = fun({Clumn, Value}, Acc_in2) -> _Acc_out2 = Fun({Clumn, Row, Value}, Acc_in2) end,
            C_V_List = lists:zip(Clumns, Values),                 % 把其压成[{Clumn, Value}]
            _Acc_out1 = lists:foldl(Fun2, Acc_in1, C_V_List)   % 累加处理C_V_List中的的每{Clumn, Value}
        end,
    R_V_List = lists:zip(Rows, V_list),                % 把其压成[{Row, Values}]
    _Acc2 = lists:foldl(Fun1, Acc1, R_V_List).          % 累加处理R_V_List中的的每{Row, Values}



%% 检测V_list的结构是否符合指定的Clumns, Rows(不符合会抛错)
check_clumns_rows_values_list(Clumns, Rows, V_list) ->
    Res1 = check_rows_values_list(Rows, V_list),    % 检测列
    Res2 = check_clumns_values_list(Clumns, V_list),        % 检测行
    case {Res1, Res2} of                                % 返回检测报错的错误信息
        {Res1, _} when Res1 =/= ok -> Res1;
        {_, Res2} when Res2 =/= ok -> Res2;
        _ -> ok
    end.


%% 检测V_list的结构是否符合指定的Rows(不符合会抛错)
check_rows_values_list(Rows, V_list) ->
    Rows_len = length(Rows),                                % 行长度
    V_list_r_len = length(V_list),                          % V_list的行长度
    case V_list_r_len of                                    % 判断V_list的行长度和指定的长度是否相同
        V_list_r_len when V_list_r_len =:= Rows_len ->      % 如果是,则返回ok
            ok;
        V_list_r_len ->                                     % 如果否,则返回错误信息
            {error,
             lists:concat(["Rows len is ", Rows_len, ", but V_list row len is ", V_list_r_len, ". V_list is:"]), 
             V_list}
    end.


%% 检测V_list的结构是否符合指定的Clumns(不符合会抛错)
check_clumns_values_list(Clumns, V_list)->
    Clumns_len = length(Clumns),                            % 列长度
    Pred = fun(Values) -> length(Values) =/= Clumns_len end,
    case lists:filter(Pred, V_list) of                      % 筛选出长度错误的值列表
        [] ->                                               % 如果没错,返回ok
            ok;
        Error_values -> 
            {error,
             lists:concat(["Clumns len is ", Clumns_len, ", but V_list clumn len is not. error values len and value is:"]), 
             [{length(Vs), Vs} || Vs <- Error_values]}
    end.


