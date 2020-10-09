-module(kaves_server).

-behaviour(gen_server).

-include("kaves.hrl").

-export([start/2, start_link/2, set/3, get/2, del/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

% Client API
set(Server, Key, Value) ->
    gen_server:call(Server, {set, Key, Value}).

get(Server, Key) ->
    gen_server:call(Server, {get, Key}).

del(Server, Key) ->
    gen_server:call(Server, {delete, Key}).


child_spec(TableName, TableDef) ->
    #{id => TableName, start => {?MODULE, start_link, [TableName, TableDef]}}.

start(TableName, TableDef) ->
    ChildSpec = child_spec(TableName, TableDef),
    case supervisor:start_child(?SUPERVISOR, ChildSpec) of
        {error, _} = Error -> lager:emergency([Error], "Kaves service failed to start");
        _ -> lager:info("Kaves server process started ~p", [TableName])
    end.

start_link(TableName, TableDef) ->
    gen_server:start_link({local, TableName}, ?MODULE, {TableName, TableDef}, []).

% Gen Server part
init({TableName, TableDef}) ->
    lager:md([{table_name, TableName}]),
    lager:info("Kaves service started"),
    case proplists:get_value(directory, TableDef) of
        undefined -> {error, missing_file};
        FileName ->
            {ok, DetsRef} = dets:open_file(TableName, [
                {file, FileName}
            ]),
            lager:info("File opened ~p", [DetsRef]),
            {ok, {TableName, TableDef}}
    end.

terminate(_Reason, {TableName, _}) ->
    dets:close(TableName).

handle_call({set, Key, Value}, _, State) ->
    lager:info("Kaves service setting ~p ~p", [Key, Value]),
    {TableName, _} = State,
    Result = dets:insert(TableName, {Key, Value}),
    {reply, Result, State};
handle_call({get, Key}, _, State) ->
    lager:info("Kaves service geting ~p", [Key]),
    {TableName, _} = State,
    case dets:lookup(TableName, Key) of
        [H|_] -> {reply, H, State};
        _Else -> {reply, null, State}
    end;
handle_call({delete, Key}, _, State) ->
    lager:info("Kaves service deleting ~p", [Key]),
    {TableName, _} = State,
    Result = dets:delete(TableName, Key),
    {reply, Result, State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

handle_continue(_, State) ->
    {noreply, State}.
