-module(test_server).

-behaviour(gen_server).

-export([start_link/0, fun0/0, fun1/1, fun1_spawn/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(fun0, _From, State) ->
    {reply, fun0(), State};

handle_call(fun1_spawn, From, State) ->
    spawn_link(test_server, fun1_spawn, [From]),
    {noreply, State};

handle_call({fun1, Arg}, _From, State) ->
    {reply, fun1(Arg), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

fun0() ->
    true.

fun1(Arg) ->
    Arg.

fun1_spawn(From) ->
    gen_server:reply(From, ok).
