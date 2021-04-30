-module(imaps).

-export([new/0,
         get/2,
         get/3,
         find/2,
         get_by_index/3,
         find_by_index/3,
         has_indices/2,
         add_index/4,
         remove_index/3,
         take_index/3,
         take_indices/2,
         put/3,
         put/4,
         fold/3,
         remove/2,
         take/2]).

-record(indexed_map, {indices, data}).
-record(data_element, {value, key_indices = #{}}).

new() ->
    #indexed_map{indices = #{}, data = #{}}.

put(Key, Value, IndexedMap) ->
    put(Key, Value, [], IndexedMap).

put(Key, Value, KeyIndices, IndexedMap = #indexed_map{data = Data}) ->
    DataElement = maps:get(Key, Data, #data_element{key_indices = #{}}),
    DataElement1 = DataElement#data_element{value = Value},
    Data1 = maps:put(Key, DataElement1, Data),
    IndexedMap1 = IndexedMap#indexed_map{data = Data1},
    Fun = fun ({IndexName, IndexValue}, Acc) ->
                  add_index(IndexName, IndexValue, Key, Acc)
          end,
    lists:foldl(Fun, IndexedMap1, KeyIndices).

get(Key, #indexed_map{data = Data}) ->
    DataElement = maps:get(Key, Data),
    #data_element{value = Value, key_indices = KeyIndices} = DataElement,
    {Value, KeyIndices}.

get(Key, #indexed_map{data = Data}, Default) ->
    DataElement = maps:get(Key, Data, #data_element{value = Default}),
    #data_element{value = Value, key_indices = KeyIndices} = DataElement,
    {Value, KeyIndices}.

find(Key, #indexed_map{data = Data}) ->
    case maps:find(Key, Data) of
      {ok, DataElement} ->
          #data_element{value = Value} = DataElement,
          {ok, Value};
      error ->
          error
    end.

get_by_index(Index, IndexValue, IndexedMap = #indexed_map{indices = Indices}) ->
    case maps:find(Index, Indices) of
      error ->
          error({bad_index, Index});
      {ok, IndexValues} ->
          case maps:find(IndexValue, IndexValues) of
            error ->
                error({bad_key, IndexValue});
            {ok, Key} ->
                get(Key, IndexedMap)
          end
    end.

find_by_index(Index, IndexValue, IndexedMap = #indexed_map{indices = Indices}) ->
    case maps:find(Index, Indices) of
      error ->
          error;
      {ok, IndexValues} ->
          case maps:find(IndexValue, IndexValues) of
            error ->
                error;
            {ok, Key} ->
                find(Key, IndexedMap)
          end
    end.

has_indices(Key, #indexed_map{data = Data}) ->
    case maps:find(Key, Data) of
      {ok, #data_element{key_indices = KeyIndices}} when map_size(KeyIndices) > 0 ->
          true;
      _ ->
          false
    end.

add_index(IndexName, IndexValue, Key, IndexedMap = #indexed_map{}) ->
    IndexedMap1 = remove_index(IndexName, IndexValue, IndexedMap),
    #indexed_map{indices = Indices, data = Data} = IndexedMap1,
    IndexValues = maps:get(IndexName, Indices, #{}),
    IndexValues1 = maps:put(IndexValue, Key, IndexValues),
    Indices1 = maps:put(IndexName, IndexValues1, Indices),
    KeyIndexUpdateFun = fun (Value) ->
                                sets:add_element(IndexValue, Value)
                        end,
    KeyIndexDefault = sets:from_list([IndexValue]),
    DataUpdateFun = fun (Value) ->
                            #data_element{key_indices = KeyIndices} = Value,
                            KeyIndices1 = maps:update_with(IndexName,
                                                           KeyIndexUpdateFun,
                                                           KeyIndexDefault,
                                                           KeyIndices),
                            Value#data_element{key_indices = KeyIndices1}
                    end,
    Data1 = maps:update_with(Key, DataUpdateFun, Data),
    IndexedMap1#indexed_map{indices = Indices1, data = Data1}.

remove_index_inner(Key, IndexName, IndexValue, Data) ->
    {DataElement, Data1} = maps:take(Key, Data),
    #data_element{key_indices = KeyIndices} = DataElement,
    {KeyIndicesInner, KeyIndices1} = maps:take(IndexName, KeyIndices),
    KeyIndicesInner1 = sets:del_element(IndexValue, KeyIndicesInner),
    KeyIndices2 = case sets:size(KeyIndicesInner1) of
                    0 ->
                        KeyIndices1;
                    _ ->
                        maps:put(IndexName, KeyIndicesInner1, KeyIndices1)
                  end,
    DataElement1 = DataElement#data_element{key_indices = KeyIndices2},
    maps:put(Key, DataElement1, Data1).

take_index(IndexName,
           IndexValue,
           IndexedMap = #indexed_map{indices = Indices, data = Data}) ->
    case maps:take(IndexName, Indices) of
      {IndexValues, Indices1} ->
          case maps:take(IndexValue, IndexValues) of
            {Key, IndexValues1} ->
                Indices2 = case maps:size(IndexValues1) of
                             0 ->
                                 Indices1;
                             _ ->
                                 maps:put(IndexName, IndexValues1, Indices1)
                           end,
                Data2 = remove_index_inner(Key, IndexName, IndexValue, Data),
                {Key, IndexedMap#indexed_map{indices = Indices2, data = Data2}};
            error ->
                error
          end;
      error ->
          error
    end.

take_indices(IndexName, IndexedMap = #indexed_map{indices = Indices, data = Data}) ->
    case maps:take(IndexName, Indices) of
      {IndexValues, Indices1} ->
          Fun = fun (IndexValue, Key, {KAcc, DAcc}) ->
                        DAcc1 = remove_index_inner(Key, IndexName, IndexValue, DAcc),
                        KAcc1 = maps:put(IndexValue, Key, KAcc),
                        {KAcc1, DAcc1}
                end,
          {Keys, Data1} = maps:fold(Fun, {#{}, Data}, IndexValues),
          {Keys, IndexedMap#indexed_map{indices = Indices1, data = Data1}};
      error ->
          error
    end.

remove_index(IndexName, IndexValue, IndexedMap) ->
    case take_index(IndexName, IndexValue, IndexedMap) of
      {_, IndexedMap1} ->
          IndexedMap1;
      error ->
          IndexedMap
    end.

remove(Key, IndexedMap) ->
    case take(Key, IndexedMap) of
      {_, _, IndexedMap1} ->
          IndexedMap1;
      error ->
          IndexedMap
    end.

take(Key, IndexedMap = #indexed_map{data = Data}) ->
    case maps:take(Key, Data) of
      {DataElement, Data1} ->
          #data_element{key_indices = KeyIndices, value = Value} = DataElement,
          Fun = fun (IndexName, KeyIndicesInner, Acc) ->
                        F = fun (IndexValue, IAcc) ->
                                    remove_index(IndexName, IndexValue, IAcc)
                            end,
                        sets:fold(F, Acc, KeyIndicesInner)
                end,
          IndexedMap1 = maps:fold(Fun, IndexedMap, KeyIndices),
          {Value, KeyIndices, IndexedMap1#indexed_map{data = Data1}};
      error ->
          error
    end.

fold(Fun, Init, #indexed_map{data = Data}) ->
    maps:fold(Fun, Init, Data).
