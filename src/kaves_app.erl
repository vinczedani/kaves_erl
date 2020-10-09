%%%-------------------------------------------------------------------
%% @doc kaves public API
%% @end
%%%-------------------------------------------------------------------

-module(kaves_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:info("Starting app..."),
    {ok, SupRef} = kaves_sup:start_link(),
    lager:info("Supervisor started"),
    loadConfig(),
    {ok, SupRef}.

stop(_State) ->
    ok.

%% internal functions
loadConfig() ->
    {ok, Tables} = application:get_env(kaves, tables),
    lager:info("Creating ~p databases...", [length(Tables)]),
    lists:foreach(fun ({ TableName, TableDef }) ->
        lager:info("Creating ~p...", [TableName]),
        kaves_server:start(TableName, TableDef)
    end, Tables),
    ok.
