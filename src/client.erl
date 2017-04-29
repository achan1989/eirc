-module(client).

-export([test/0,test/1,test/2]).

-define(DEFAULT_PORT, 6667).


test() ->
    test("irc.snoonet.org", 6667).
test(Server) ->
    test(Server, ?DEFAULT_PORT).
test(Server, Port) ->
    log("Start of test function, we are ~p.~n", [self()]),
    {ok, Connection} = connection:new(),
    ok = connection:subscribe(Connection),

    ok = connection:open(Connection, Server, Port),
    log("Connected.~n", []),
    timer:sleep(1000),
    flush_msgs(),

    ok = connection:register(
        Connection, "test_eirc_client",
        {"test_eirc_client", "test_pass_dbo7c3754t76"}),
    log("Registered.~n", []),
    timer:sleep(1000),
    flush_msgs(),

    log("Listening for 30.~n", []),
    IsConnected = case receive_for(30, Connection) of
        ok ->
            ok;
        connection_closed ->
            log("Aborted due to connection closed.~n", [])
    end,

    case IsConnected of
        ok ->
            log("Closing...~n", []),
            case connection:close(Connection) of
                closing ->
                    {found, _CloseMsg} = receive_for(
                        10, Connection, {connection_closed, Connection});
                closed ->
                    ok
            end,
            log("Closed.~n", []);
        connection_closed ->
            log("Skipping close, already dead.~n", [])
    end,
    ok.

flush_msgs() ->
    receive X ->
         log("Got ~p~n", [X]),
         flush_msgs()
    after 0 ->
        ok
    end.

% Returns:
% ok at the end of the time period.
% connection_closed if the connection closed.
receive_for(Seconds, Connection) ->
    erlang:send_after(Seconds*1000, self(), stop_receive),
    receive_loop(Connection).

% Returns:
% {found, Msg} if the expected message was received before the timeout.
% timeout if the expected message was not received.
% connection_closed if the connection closed.
receive_for(Seconds, Connection, StopOnMsg) ->
    StopRef = erlang:send_after(Seconds*1000, self(), stop_receive),
    Result = receive_loop(Connection, StopOnMsg),
    erlang:cancel_timer(StopRef),
    Result.

% Returns ok
receive_loop(Connection) ->
    receive
        stop_receive ->
            ok;
        {connection_closed, Connection} ->
            connection_closed;
        X ->
            log("Received: ~p~n", [X]),
            receive_loop(Connection)
    end.

% Returns:
% {ok, Msg} if the expected message was received before the timeout.
% timeout if the expected message was not received.
receive_loop(Connection, StopOnMsg) ->
    receive
        StopOnMsg = Msg ->
            {found, Msg};
        stop_receive ->
            timeout;
        {connection_closed, Connection} ->
            connection_closed;
        X ->
            log("Received: ~p~n", [X]),
            receive_loop(Connection, StopOnMsg)
    end.

log(Format, Data) ->
    io:format("[CLIENT] ", []),
    io:format(Format, Data).
