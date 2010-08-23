-module(test).
-export([start/0, test/0, get_remote_server/0, get_echo_server/0]).

timeit(Msg, Fun, Ntimes) ->
    L = lists:seq(1, Ntimes),
    statistics(wall_clock),
    lists:foreach(fun(_) -> Fun() end, L),
    {_, Msec} = statistics(wall_clock),
    io:format("~.50s : ~p msec~n", [Msg, Msec]).

get_remote_server() ->
    {ok, Pid} = gen_server:start(test_server, [], []),
    Pid.

get_echo_server() ->
    Pid = spawn(fun echo/0),
    Pid.

echo() ->
    receive
        {From, X} ->
            From ! X,
            echo()
    end.

start() ->
    receive
        forever ->
            start()
    end.

do_workers(N, Fun) ->
    do_workers(N, Fun, []).
do_workers(N, Fun, Args) ->
    do_workers(N, N, Fun, Args).
do_workers(Max, 0, _Fun, _Args) ->
    wait_workers(Max, done),
    ok;
do_workers(Max, N, Fun, Args) ->
    Self = self(),
    spawn_link(fun() ->
                  ok = apply(Fun, [Max - N | Args]),
                  Self ! done
          end),
    do_workers(Max, N - 1, Fun, Args).


wait_workers(0, _Msg) ->
    ok;
wait_workers(Concurrency, Msg) ->
    receive
        Msg -> []
    after 1000 * 60 * 5->
          io:format("timeout~n")
    end,
    wait_workers(Concurrency - 1, Msg).

do_times(N, Fun) ->
    do_times(N, Fun, []).
do_times(0, _Fun, _Args) ->
    ok;
do_times(N, Fun, Args) ->
    ok = apply(Fun, Args),
    do_times(N - 1, Fun, Args).

do_parallel(Nproc, NtimesPerProc, Fun) ->
    do_workers(Nproc, fun(_X) -> do_times(NtimesPerProc, fun() -> Fun(), ok end) end).

test() ->
    Ntimes = 100000,
    {ok, Pid} = gen_server:start(test_server, [], []),
    RemotePid = rpc:call(list_to_atom("test1@127.0.0.1"), test, get_remote_server, []),
    LocalEcho = get_echo_server(),
    RemoteEcho = rpc:call(list_to_atom("test1@127.0.0.1"), test, get_echo_server, []),
    timeit("call fun0", fun() -> test_server:fun0() end, Ntimes),
    timeit("call fun1", fun() -> test_server:fun1("Hello") end, Ntimes),
    timeit("send/receive local", fun () -> LocalEcho ! {self(), x}, receive x -> ok end end, Ntimes),
    timeit("gen_server:call fun0 local", fun() -> gen_server:call(Pid, fun0) end, Ntimes),
    timeit("gen_server:call fun1 local", fun() -> gen_server:call(Pid, {fun1, "Hello"}) end, Ntimes),
    timeit("gen_server:call fun0 local 10 proc", fun() ->
                                                         do_parallel(10, Ntimes div 10, fun () -> gen_server:call(Pid, fun0) end)
                                                 end, 1),
    timeit("gen_server:call fun0 local 100 proc", fun() ->
                                                          do_parallel(100, Ntimes div 100, fun () -> gen_server:call(Pid, fun0) end)
                                                 end, 1),
    timeit("gen_server:call fun1_spawn local", fun() -> gen_server:call(Pid, fun1_spawn) end, Ntimes),
    timeit("gen_server:call fun1_spawn local 10 proc", fun() ->
                                                               do_parallel(10, Ntimes div 10, fun () -> gen_server:call(Pid, fun1_spawn) end)
                                                       end, 1),
    timeit("gen_server:call fun1_spawn local 100 proc", fun() ->
                                                               do_parallel(100, Ntimes div 100, fun () -> gen_server:call(Pid, fun1_spawn) end)
                                                       end, 1),
    timeit("send/receive remote", fun () -> RemoteEcho ! {self(), x}, receive x -> ok end end, Ntimes),
    timeit("gen_server:call fun0 remote", fun() -> gen_server:call(RemotePid, fun0) end, Ntimes),
    timeit("gen_server:call fun0 remote 10 proc", fun() ->
                                                          do_parallel(10, Ntimes div 10, fun () -> gen_server:call(RemotePid, fun0) end)
                                                  end, 1),
    timeit("gen_server:call fun0 remote 100 proc", fun() ->
                                                          do_parallel(100, Ntimes div 100, fun () -> gen_server:call(RemotePid, fun0) end)
                                                   end, 1),
    timeit("gen_server:call fun0 remote 200 proc", fun() ->
                                                          do_parallel(200, Ntimes div 200, fun () -> gen_server:call(RemotePid, fun0) end)
                                                   end, 1),
    timeit("gen_server:call fun1 remote", fun() -> gen_server:call(RemotePid, {fun1, "Hello"}) end, Ntimes),
    timeit("gen_server:call fun1_spawn remote", fun() -> gen_server:call(RemotePid, fun1_spawn) end, Ntimes),
    timeit("gen_server:call fun1_spawn remote 10 proc", fun() ->
                                                                do_parallel(10, Ntimes div 10, fun () -> gen_server:call(RemotePid, fun1_spawn) end)
                                                   end, 1),
    timeit("rpc:call fun0", fun() -> rpc:call(list_to_atom("test1@127.0.0.1"), test_server, fun0, []) end, Ntimes),
    timeit("rpc:call fun0 10 proc", fun() ->
                                            do_parallel(10, Ntimes div 10, fun () -> rpc:call(list_to_atom("test1@127.0.0.1"), test_server, fun0, []) end)
                                    end, 1),
    timeit("rpc:call fun0 100 proc", fun() ->
                                             do_parallel(100, Ntimes div 100, fun () -> rpc:call(list_to_atom("test1@127.0.0.1"), test_server, fun0, []) end)
                                     end, 1),
    timeit("rpc:block_call fun0", fun() -> rpc:block_call(list_to_atom("test1@127.0.0.1"), test_server, fun0, []) end, Ntimes),

    halt(0).
