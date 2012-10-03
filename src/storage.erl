%% Author: atamborrino
%% Created: 1 Oct 2012
%% Description: TODO: Add description to storage
-module(storage).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([add/3,create/0,lookup/2,split/3,merge/2]).

%% ! Really inefficient storage mechanism, just for test :) !

create() ->
    [].

add(Key,Value,Storage) ->
    [{Key,Value}|Storage].

lookup(Key,Storage) ->
    lists:keyfind(Key, 1, Storage).

split(LocalKey, Pkey, Storage) ->
    {Keep, Give} = lists:foldl(fun({Key,Value},{AccSplit1,AccSplit2}) ->
                                           case key:between(Key, Pkey, LocalKey) of
                                               true ->
                                                   %% keep for local node
                                                   {[{Key,Value}|AccSplit1],AccSplit2};
                                               false ->
                                                   {AccSplit1,[{Key,Value}|AccSplit2]}
                                           end
                                           end, {[],[]}, Storage).

merge(Storage1,Storage2) ->
    Storage1 ++ Storage2.




