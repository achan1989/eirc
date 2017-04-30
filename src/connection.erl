-module(connection).
-behaviour(gen_server).

% public API.
-export([new/0]).
-export([subscribe/1,unsubscribe/1]).
-export([open/2,open/3,close/1,terminate/1]).
-export([register/2,register/3]).

% gen_server stuff.
-export([init/1,terminate/2,handle_call/3,handle_cast/2]).
-export([handle_info/2,code_change/3]).

% Spec says max 512 characters per command, but let's try to grab more than
% that just in case.
-define(MAX_RX_PACKET_BYTES, (3 * 512)).
-define(DEFAULT_PORT, 6667).
-define(REAL_NAME_ANON, "anon").


%% Public API.

% Returns {ok, Connection}
new() ->
    gen_server:start(?MODULE, [], []).

% Returns ok
subscribe(Connection) ->
    gen_server:call(Connection, subscribe).

% Returns ok
unsubscribe(Connection) ->
    gen_server:call(Connection, unsubscribe).

% Returns ok
open(Connection, Server) ->
    open(Connection, Server, ?DEFAULT_PORT).
open(Connection, Server, Port) ->
    gen_server:call(Connection, {open, Server, Port}).

% Close the connection nicely.
% Returns:
% closing if the connection is open, sends {connection_closed, Connection} to
%         subscribers once close is complete.
% closed if the connection is already closed.
close(Connection) ->
    try gen_server:call(Connection, close)
    catch
        exit:{noproc,{gen_server,call,[Connection,close]}} ->
            closed
    end.

% Kill the connection immediately.
% TODO
terminate(_Connection) ->
    error(not_implemented).

% Returns ok
register(Connection, Nick) ->
    gen_server:call(Connection, {register, Nick}).
register(Connection, Nick, {Username, Password}) ->
    gen_server:call(Connection, {register, Nick, {Username, Password}}).


%% gen_server callbacks.

init([]) ->
    State = #{
        state => closed,
        socket => none,
        subscribers => []},
    {ok, State}.

handle_call(subscribe, {Subscriber, _Tag}, State) ->
    Subscribers = maps:get(subscribers, State),
    NewSubscribers = case lists:member(Subscriber, Subscribers) of
        true ->
            Subscribers;
        false ->
            [Subscriber | Subscribers]
    end,
    {reply, ok, State#{subscribers := NewSubscribers}};

handle_call(unsubscribe, {Subscriber, _Tag}, State) ->
    Subscribers = maps:get(subscribers, State),
    NewSubscribers = lists:delete(Subscriber, Subscribers),
    {reply, ok, State#{subscribers := NewSubscribers}};

handle_call({open, Server, Port}, _Subscriber, State = #{state := closed}) ->
    Options = [
        inet,
        binary,
        {active, true},
        % {exit_on_close, false},
        {packet, line},
        {packet_size, ?MAX_RX_PACKET_BYTES}
    ],
    {ok, Socket} = gen_tcp:connect(Server, Port, Options),

    {reply, ok, State#{state := open, socket := Socket}};

handle_call(close, _Subscriber, State = #{state := open}) ->
    Socket = get_socket(State),
    ok = send_message(Socket, messages:quit()),
    ok = gen_tcp:shutdown(Socket, write),
    {reply, closing, State#{state := closing}};
% handle_call(close, _Subscriber, State = #{state := closed}) ->

handle_call({register, Nick}, _Subscriber, State = #{state := open}) ->
    Socket = get_socket(State),
    ok = send_message(Socket, messages:nickname(Nick)),
    {reply, ok, State};
handle_call({register, Nick, {Username, Password}}, _Subscriber, State = #{state := open}) ->
    Socket = get_socket(State),
    ok = send_message(Socket, messages:password(Password)),
    ok = send_message(Socket, messages:nickname(Nick)),
    ok = send_message(Socket, messages:user(Username, ?REAL_NAME_ANON)),
    {reply, ok, State}.

handle_info({tcp_closed, Socket}, State = #{state := open, socket := Socket}) ->
    notify_closed(State),
    {stop, normal, State#{state := closed}};
handle_info({tcp_closed, Socket}, State = #{state := closing, socket := Socket}) ->
    notify_closed(State),
    {stop, normal, State#{state := closed}};

handle_info({tcp, Socket, Rx}, State = #{socket := Socket}) ->
    notify_rx(Rx, State),
    {noreply, State};

handle_info(Info, State) ->
    log("Unhandled message in handle_info():~n    got ~p~n    in state ~p.~n", [Info, State]),
    {noreply, State}.

% TODO: handle_cast()
handle_cast(Request, State) ->
    log("Unhandled message in handle_cast():~n    got ~p~n    in state ~p.~n", [Request, State]),
    {stop, unexpected_cast, State}.


% gen_server default implementations (boring).

terminate(_Reason, _State) ->
    % TODO: graceful/forceful close of open socket?
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal functions.

get_socket(State = #{state := open}) ->
    maps:get(socket, State);
get_socket(State = #{state := closing}) ->
    maps:get(socket, State).

send_message(Socket, Message) ->
    log("Sending to ~p: ~p.~n", [Socket, Message]),
    gen_tcp:send(Socket, Message).

notify_closed(State) ->
    Msg = {connection_closed, self()},
    notify_subscribers(Msg, State).

notify_rx(Rx, State) ->
    Msg = {rx, self(), Rx},
    notify_subscribers(Msg, State).

notify_subscribers(Message, State) ->
    Subscribers = maps:get(subscribers, State),
    notify_subscribers_loop(Message, Subscribers).

notify_subscribers_loop(_Message, _Subscribers = []) -> ok;
notify_subscribers_loop(Message, [Sub | Subs]) ->
    Sub ! Message,
    notify_subscribers_loop(Message, Subs).

log(Format, Data) ->
    io:format("[CONNECTION] ", []),
    io:format(Format, Data).
