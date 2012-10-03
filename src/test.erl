%% Author: atamborrino
%% Created: 2 Oct 2012
%% Description: TODO: Add description to test
-module(test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%


test() ->
    %% adding nodes
    %Nodes = [node2:start(key:generate()) || _ <- lists:seq(1, 4)],
    Node = node2:start(key:generate()),
    %Node2 = node2:start(key:generate(),Node),
    %Node3 = node2:start(key:generate(),Node),
    %Node4 = node2:start(key:generate(),Node),
    
    timer:sleep(1000),
    
    Test_machines = lists:seq(1, 4),
    %lists:foreach(fun(_) ->
    %                      spawn(fun() -> add_then_lookup(Node,1000) end)
    %                      end, Test_machines).
    spawn(fun() -> add_then_lookup(Node,1000) end),
    spawn(fun() -> add_then_lookup(Node,1000) end),
    spawn(fun() -> add_then_lookup(Node,1000) end),
    spawn(fun() -> add_then_lookup(Node,1000) end).



%%
%% Local Functions
%%

add_then_lookup(Node,Nb) ->
    
    
    Tuples = [{key:generate(),N} || N <- lists:seq(1, Nb)],
    lists:foreach(fun({Key,Value}) ->
                          Node ! {add, Key, Value, Key, self()}
                          end,Tuples),
    
    T1 = erlang:now(),
    
    %% looking up 
    lists:foreach(fun({Key,Value}) ->
                          Node ! {lookup, Key, Key, self()}
                          end, Tuples),
    
     T2 = erlang:now(),
     Duration = timer:now_diff(T2, T1)/1000000,
     io:format("Duration : ~p~n",[Duration]).
    

