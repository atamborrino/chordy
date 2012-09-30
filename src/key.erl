%% Author: atamborrino
%% Created: 30 Sep 2012
%% Description: TODO: Add description to key
-module(key).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([generate/0,between/3]).

%%
%% API Functions
%%

generate() ->
    random:uniform(1000000000).

between(Key,From,To) ->
    if
        From<To ->
            (Key>From) and (Key=<To);
        From>To ->
            (Key>From) or (Key=<To);
        true ->
            true
    end.
        
                    
                   
        


%%
%% Local Functions
%%

