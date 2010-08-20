-module(test).
-export([start/0, test/0, get_remote_server/0]).

timeit(Msg, Fun, Ntimes) ->
    L = lists:seq(1, Ntimes),
    statistics(wall_clock),
    lists:foreach(fun(_) -> Fun() end, L),
    {_, Msec} = statistics(wall_clock),
    io:format("~.40s : ~p msec~n", [Msg, Msec]).

get_remote_server() ->
    {ok, Pid} = gen_server:start(test_server, [], []),
    Pid.

start() ->
    receive
        forever ->
            start()
    end.

test() ->
    Ntimes = 100000,
    {ok, Pid} = gen_server:start(test_server, [], []),
    RemotePid = rpc:call(list_to_atom("test1@127.0.0.1"), test, get_remote_server, []),
    timeit("call fun0", fun() -> test_server:fun0() end, Ntimes),
    timeit("call fun1", fun() -> test_server:fun1("Hello") end, Ntimes),
    timeit("gen_server:call fun0 local", fun() -> gen_server:call(Pid, fun0) end, Ntimes),
    timeit("gen_server:call fun1 local", fun() -> gen_server:call(Pid, {fun1, "Hello"}) end, Ntimes),
    timeit("gen_server:call fun1_spawn local", fun() -> gen_server:call(Pid, fun1_spawn) end, Ntimes),
    timeit("gen_server:call fun0 remote", fun() -> gen_server:call(RemotePid, fun0) end, Ntimes),
    timeit("gen_server:call fun1 remote", fun() -> gen_server:call(RemotePid, {fun1, "Hello"}) end, Ntimes),
    timeit("gen_server:call fun1_spawn remote", fun() -> gen_server:call(RemotePid, fun1_spawn) end, Ntimes),
    timeit("rpc:call fun0", fun() -> rpc:call(list_to_atom("test1@127.0.0.1"), test_server, fun0, []) end, Ntimes),
    timeit("rpc:block_call fun0", fun() -> rpc:block_call(list_to_atom("test1@127.0.0.1"), test_server, fun0, []) end, Ntimes),
    halt(0).
