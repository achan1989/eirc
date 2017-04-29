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
% {Prefix, Command, [Parameters]} or
% {none, Command, [Parameters]} or
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
    io:format("start, S=~p~n", [S]),
    case S of
        "\r\n" ->
            empty;
        [$:|Rest] ->
            parse(prefix, Rest);
        _ ->
            {Command, Parameters} = parse(command, S),
            {none, Command, Parameters}
    end;
parse(prefix, S) ->
    io:format("prefix, S=~p~n", [S]),
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
            {Command, Parameters} = parse(command, tl(Rest)),
            {Prefix, Command, Parameters};
        _ ->
            throw({not_recognised, "Invalid end of prefix"})
    end;

parse(command, S) ->
    io:format("command, S=~p~n", [S]),
    {Command, Rest} = lists:splitwith(fun is_command_char/1, S),
    case Command of
        [] ->
            throw({not_recognised, "Invalid start of command"});
        _ ->
            Parameters = parse(params, Rest),
            {Command, Parameters}
    end;

parse(params, "\r\n") ->
    [];
parse(params, []) ->
    throw({not_recognised, "Missing CRLF"});
parse(params, S) ->
    io:format("params, S=~p~n", [S]),
    parse(params_mid, S, [], 0).

% End of parameters.
parse(params_mid, "\r\n", Parsed, NumParsed) when NumParsed > 0 ->
    lists:reverse(Parsed);
% Too many parameters.
parse(params_mid, _S, _Parsed, NumParsed) when NumParsed >= ?MAX_NUM_PARAMS ->
    throw({not_recognised, "Too many params"});
% More parameters.
parse(params_mid, S, Parsed, NumParsed) ->
    io:format("params_mid, S=~p, Parsed=~p, NumParsed=~p~n", [S,Parsed,NumParsed]),
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
            parse(params_trail, tl(Body), Parsed, NumParsed);
        _ ->
            NewParsed = [Param|Parsed],
            parse(params_mid, MoreParams, NewParsed, NumParsed+1)
    end;

parse(params_trail, S, Parsed, NumParsed) ->
    io:format("params_trail, S=~p, Parsed=~p, NumParsed=~p~n", [S,Parsed,NumParsed]),
    Result = lists:splitwith(fun is_param_trail_char/1, S),
    case Result of
        {Param, _End="\r\n"} ->
            lists:reverse([Param|Parsed]);
        _ ->
            todo_unmatched
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
