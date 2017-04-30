-module(message).

-export([new/1,new/2,new/3,new/4]).

-define(EMPTY, <<"">>).
-define(SPACE, <<" ">>).
-define(COLON, <<":">>).
-define(SPACE_COLON, <<" :">>).
-define(CRLF, <<"\r\n">>).

% NUL, LF, CR, space, colon
-define(NOSPCRLFCL, [0, 10, 13, ($ ), $:]).

-define(MAX_MESSAGE_LENGTH, 512).
-define(MAX_NUM_PARAMS, 15).


%% See https://tools.ietf.org/html/rfc2812#section-2.3


% Create a new message from prefix (optional), command, [mid parameters] (optional), trail parameter (optional).
% All inputs must be non-binary strings, or lists thereof.
% Returns a binary.
new(Command) ->
    new("", Command, [], "").
new(Command, MidParams) ->
    new("", Command, MidParams, "").
new(Command, MidParams, TrailParam) ->
    new("", Command, MidParams, TrailParam).
new(Prefix, Command, MidParams, TrailParam) when is_list(Prefix), is_list(Command), is_list(MidParams), is_list(TrailParam) ->
    PrefixB = transform_prefix(Prefix),
    CommandB = transform_command(Command),
    ParamsB = transform_params(MidParams, TrailParam),
    Packet = <<PrefixB/bytes, CommandB/bytes, ParamsB/bytes, ?CRLF/bytes>>,
    assert_length(Packet),
    Packet.

transform_prefix("") ->
    ?EMPTY;
transform_prefix(P) when is_list(P) ->
    assert_valid_prefix(P),
    B = list_to_binary(P),
    <<?COLON/bytes, B/bytes, ?SPACE/bytes>>.

transform_command([_|_] = C) ->
    assert_valid_command(C),
    list_to_binary(C).

% transform_params([MidParams], TrailParam)
% Transform non-binary string parameters into a single binary string in
% IRC format.
%
% If provided, TrailParam will always be formatted with a preceding colon.
% None of the [MidParams] may start with a colon or contain a space.
%
% Returns:
% <<"">> (?EMPTY) or
% Binary (with leading space)
% Errors:
% {invalid_message, Reason}
transform_params([], []) ->
    ?EMPTY;

% When only a trailing param is given.
transform_params([], Trail) when is_list(Trail) ->
    transform_trail(Trail);

% When only middle parameters are given.
transform_params(Mid, []) when is_list(Mid), length(Mid) =< ?MAX_NUM_PARAMS ->
    transform_mid(Mid);

% When both middle and trailing params are given.
transform_params(Mid, Trail) when is_list(Mid), is_list(Trail), length(Mid) =< ?MAX_NUM_PARAMS - 1 ->
    MidB = transform_mid(Mid),
    TrailB = transform_trail(Trail),
    <<MidB/bytes, TrailB/bytes>>.

% Transform the mid parameters.
% Returns:
% Binary (always with leading space)
% Errors:
% {invalid_message, Reason}
transform_mid([_|_] = Mid) ->
    transform_mid(Mid, ?EMPTY).

% Last mid param.
transform_mid([P|[]], BS) when is_list(P) ->
    assert_valid_mid(P),
    B = list_to_binary(P),
    <<BS/bytes, ?SPACE/bytes, B/bytes>>;

% More than one mid param.
transform_mid([P|Params], BS) when is_list(P) ->
    assert_valid_mid(P),
    B = list_to_binary(P),
    transform_mid(Params, <<BS/bytes, ?SPACE/bytes, B/bytes>>).

% Transform the trailing parameter.
% Returns:
% Binary (always with leading space)
% Errors:
% {invalid_message, Reason}
transform_trail(Trail) when is_list(Trail) ->
    assert_valid_trail(Trail),
    B = list_to_binary(Trail),
    << ?SPACE_COLON/bytes, B/bytes>>.

assert_length(Packet) ->
    Size = size(Packet),
    case (Size =< ?MAX_MESSAGE_LENGTH) of
        true ->
            ok;
        false ->
            error({invalid_message, {"Message too long", Size}})
    end.

assert_valid_prefix(P) ->
    case lists:any(fun is_nospcrlfcl_char/1, P) of
        true ->
            err;
        false ->
            ok
    end.

assert_valid_command(C) ->
    case lists:all(fun is_letter_or_digit/1, C) of
        true ->
            ok;
        false ->
            error({invalid_message, {"Invalid command", C}})
    end.

assert_valid_mid(M) ->
    case (hd(M) /= $:) andalso lists:all(fun is_param_mid_char/1, M) of
        true ->
            ok;
        false ->
            error({invalid_message, {"Invalid mid parameter", M}})
    end.

assert_valid_trail(T) ->
    case lists:all(fun is_param_trail_char/1, T) of
        true ->
            ok;
        false ->
            error({invalid_message, {"Invalid trailing parameter", T}})
    end.

is_param_mid_char(C) ->
    (C == $:) orelse not is_nospcrlfcl_char(C).

is_param_trail_char(C) ->
    (C == ($ )) orelse is_param_mid_char(C).

is_nospcrlfcl_char(C) ->
    lists:member(C, ?NOSPCRLFCL).

is_letter_or_digit(C) when C >= $a, C =< $z ->
    true;
is_letter_or_digit(C) when C >= $A, C =< $Z ->
    true;
is_letter_or_digit(C) when C >= $0, C =< $9 ->
    true;
is_letter_or_digit(_C) ->
    false.
