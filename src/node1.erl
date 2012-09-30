%% Author: atamborrino
%% Created: 30 Sep 2012
%% Description: TODO: Add description to node1
-module(node1).

%%
%% Include files
%%

-define(Stabilize,100).
-define(Timeout,1000).

%%
%% Exported Functions
%%
-export([start/1,start/2]).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor).

connect(Id, nil) ->
    {ok, {Id,self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
               {ok,{Skey,Peer}}
    after ?Timeout ->
            io:format("Time out: no response~n")
    end.

node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);
        StrangeMessage ->
            io:format("strang message : ~s~n",[StrangeMessage])
end.

create_probe(Id,{Skey,Spid}) ->
    Spid ! {probe,Id,[Id],erlang:now()}.

remove_probe(T, Nodes) ->
    Duration = timer:now_diff(erlang:now(),T),
    Printer = fun(E) -> io:format("~p ",[E]) end, 
    lists:foreach(Printer,Nodes),
    io:format("~n Time = ~p",[Duration]).

forward_probe(Ref, T, Nodes, Id, {Skey,Spid}) ->
    Spid ! {probe,Ref,Nodes ++ [Id],T}.

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} -> 
            Successor;
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true -> 
                    %% Current node is not between
                    Xpid ! {request, self()},
                    Pred;
                false ->
                    Spid ! {notify,{Id,self()}},
                    Successor
            end
     end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            {Nkey, Npid};
        {Pkey,  _} ->
            case key:between(Nkey, Pkey, Id) of
                true -> 
                    {Nkey, Npid};
                false -> 
                    Predecessor
            end
    end.





