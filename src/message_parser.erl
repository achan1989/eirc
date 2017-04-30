-module(message_parser).

% Public API.
-export([parse/1,from_binary/1]).

% To fix weirdness.
-export([is_number/1]).

% NUL, LF, CR, space, colon
-define(NOSPCRLFCL, [0, 10, 13, ($ ), $:]).

-define(MAX_MESSAGE_LENGTH, 512).
-define(MAX_NUM_PARAMS, 15).


% Parse a binary into its constituent parts.
% Returns:
% {ok, {Prefix, Command, [Parameters]}} or
% {ok, {none, Command, [Parameters]}} or
% empty
% {not_recognised, Reason}
from_binary(Packet) when is_binary(Packet) ->
    message_parser:parse(binary:bin_to_list(Packet)).


parse(S) when length(S) > ?MAX_MESSAGE_LENGTH ->
    {not_recognised, "Too long"};
parse(S) ->
    try
        parse(start, S)
    catch throw:{not_recognised, _Reason} = NR ->
        NR
    end.

parse(start, S) ->
    case S of
        "\r\n" ->
            empty;
        [$:|Rest] ->
            parse(prefix, Rest);
        _ ->
            {Command, MidParams, TrailParam} = parse(command, S),
            {ok, {none, Command, MidParams, TrailParam}}
    end;
parse(prefix, S) ->
    {Prefix, Rest} = lists:splitwith(fun is_prefix_char/1, S),
    case Prefix of
        [] ->
            throw({not_recognised, "Invalid start of prefix"});
        _ ->
            ok
    end,
    case hd(Rest) of
        % Prefix must end with a space.
        ($ ) ->
            % Remove the space before parsing the command.
            {Command, MidParams, TrailParam} = parse(command, tl(Rest)),
            {ok, {Prefix, Command, MidParams, TrailParam}};
        _ ->
            throw({not_recognised, "Invalid end of prefix"})
    end;

parse(command, S) ->
    {Command, Rest} = lists:splitwith(fun is_command_char/1, S),
    case Command of
        [] ->
            throw({not_recognised, "Invalid start of command"});
        _ ->
            {MidParams, TrailParam} = parse(params, Rest),
            {Command, MidParams, TrailParam}
    end;

parse(params, "\r\n") ->
    {[], ""};
parse(params, []) ->
    throw({not_recognised, "Missing CRLF"});
parse(params, S) ->
    parse(params_mid, S, [], 0).

% End of parameters.
parse(params_mid, "\r\n", MidParsed, NumParsed) when NumParsed > 0 ->
    % No trail param.
    {lists:reverse(MidParsed), ""};
% Too many parameters.
parse(params_mid, _S, _MidParsed, NumParsed) when NumParsed >= ?MAX_NUM_PARAMS ->
    throw({not_recognised, "Too many params"});
% More parameters.
parse(params_mid, S, MidParsed, NumParsed) ->
    Body = try
        ($ ) = hd(S),
        tl(S)
    catch error:{badmatch, _} ->
        throw({not_recognised, "Invalid param"})
    end,

    % io:format("split gives ~p~n", [lists:splitwith(fun is_param_mid_char/1, Body)]),
    {Param, MoreParams} = lists:splitwith(fun is_param_mid_char/1, Body),
    case Param of
        [] ->
            throw({not_recognised, "Missing or invalid param"});
        _ ->
            ok
    end,
    case hd(Param) of
        $: ->
            % If a param starts with colon, it must be the trailing param.
            % Strip off the colon and hand off to other parser.
            % We haven't parsed it yet, so NumParsed is unchanged.
            parse(params_trail, tl(Body), MidParsed, NumParsed);
        _ ->
            NewMidParsed = [Param|MidParsed],
            parse(params_mid, MoreParams, NewMidParsed, NumParsed+1)
    end;

parse(params_trail, S, MidParsed, _NumParsed) ->
    Result = lists:splitwith(fun is_param_trail_char/1, S),
    case Result of
        {Trail, _End="\r\n"} ->
            {lists:reverse(MidParsed), Trail};
        _ ->
            throw({not_recognised, "Invalid end of params"})
    end.

is_prefix_char(C) ->
    not lists:member(C, ?NOSPCRLFCL).

is_command_char(C) ->
    is_letter(C) orelse ?MODULE:is_number(C).

is_param_mid_char(C) ->
    (C == $:) orelse not lists:member(C, ?NOSPCRLFCL).

is_param_trail_char(C) ->
    (C == ($ )) orelse is_param_mid_char(C).

is_letter(C) ->
    (C >= $A andalso C =< $Z) orelse
    (C >= $a andalso C =< $z).

is_number(C) ->
    C >= $0 andalso C =< $9.
