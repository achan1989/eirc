-module(messages).

-export([password/1,nickname/1,user/2,user/3,quit/0,quit/1]).


password(Password) ->
    message:new(<<"PASS">>, [Password]).

nickname(Nickname) ->
    message:new(<<"NICK">>, [Nickname]).

user(Username, Realname) ->
    user(Username, <<"0">>, Realname).
user(Username, Mode, Realname) ->
    message:new(<<"USER">>, [Username, Mode, <<"*">>, Realname]).

quit() ->
    quit(none).
quit(none) ->
    message:new(<<"QUIT">>, []);
quit(Reason) ->
    message:new(<<"QUIT">>, [Reason]).
