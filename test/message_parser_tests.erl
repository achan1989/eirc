-module(message_parser_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_empty_test_() ->
    [
    ?_assertMatch(
        empty,
        message_parser:parse("\r\n"))
    ].

parse_invalid_prefix_test_() ->
    [
    ?_assertMatch(
        {not_recognised, "Invalid start of prefix"},
        message_parser:parse([$:, 0])),

    ?_assertMatch(
        {not_recognised, "Invalid start of prefix"},
        message_parser:parse(":")),

    ?_assertMatch(
        {not_recognised, "Invalid start of prefix"},
        message_parser:parse(":\r")),

    ?_assertMatch(
        {not_recognised, "Invalid start of prefix"},
        message_parser:parse(":\n")),

    ?_assertMatch(
        {not_recognised, "Invalid start of prefix"},
        message_parser:parse(":\r\n")),

    ?_assertMatch(
        {not_recognised, "Invalid start of prefix"},
        message_parser:parse(": ")),

    ?_assertMatch(
        {not_recognised, "Invalid start of prefix"},
        message_parser:parse(": \r\n")),

    ?_assertMatch(
        {not_recognised, "Invalid start of prefix"},
        message_parser:parse("::\r\n")),

    ?_assertMatch(
        {not_recognised, "Invalid start of prefix"},
        message_parser:parse(":\r\n"))
    ].

parse_invalid_command_test_() ->
    [
    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse("")),

    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse([0])),

    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse("\r")),

    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse("\n")),

    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse(" ")),

    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse(" \r")),

    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse(" \n")),

    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse(" \r\n")),

    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse(" FOO\r\n")),

    ?_assertMatch(
        {not_recognised, "Invalid start of command"},
        message_parser:parse(" FOO \r\n"))
    ].

parse_invalid_param_test_() ->
    [
    ?_assertMatch(
        {not_recognised, "Missing or invalid param"},
        message_parser:parse("FOO ")),

    ?_assertMatch(
        {not_recognised, "Missing or invalid param"},
        message_parser:parse("FOO \r\n")),

    ?_assertMatch(
        {not_recognised, "Missing or invalid param"},
        message_parser:parse("FOO \rsome-param\r\n")),

    ?_assertMatch(
        {not_recognised, "Invalid param"},
        message_parser:parse("FOO param1\rsome-param\r\n")),

    ?_assertMatch(
        {not_recognised, "Too many params"},
        message_parser:parse(":prefix FOO 1 2 3 4 5 6 7 8 9 10 11 12 13 14 "
            "15 16\r\n")),

    ?_assertMatch(
        {not_recognised, "Too many params"},
        message_parser:parse(":prefix FOO 1 2 3 4 5 6 7 8 9 10 11 12 13 14 "
            "15 :16\r\n"))
    ].

parse_invalid_crlf_test_() ->
    [
    ?_assertMatch(
        {not_recognised, "Missing CRLF"},
        message_parser:parse("FOO"))
    ].

parse_valid_message_test_() ->
    [
    ?_assertMatch(
        {none, "FOO", []},
        message_parser:parse("FOO\r\n")),

    ?_assertMatch(
        {"prefix", "FOO", []},
        message_parser:parse(":prefix FOO\r\n")),

    ?_assertMatch(
        {none, "FOO", ["one"]},
        message_parser:parse("FOO one\r\n")),

    ?_assertMatch(
        {"prefix", "FOO", ["one"]},
        message_parser:parse(":prefix FOO one\r\n")),

    ?_assertMatch(
        {none, "FOO", ["one", "two"]},
        message_parser:parse("FOO one two\r\n")),

    ?_assertMatch(
        {"prefix", "FOO", ["one", "two"]},
        message_parser:parse(":prefix FOO one :two\r\n")),

    ?_assertMatch(
        {none, "FOO", ["one", "two", "three"]},
        message_parser:parse("FOO one two :three\r\n")),

    ?_assertMatch(
        {"prefix", "FOO", ["one", "two", "three"]},
        message_parser:parse(":prefix FOO one two :three\r\n")),

    ?_assertMatch(
        {"prefix", "FOO", ["one", "two", "three: blind mice"]},
        message_parser:parse(":prefix FOO one two :three: blind mice\r\n")),

    ?_assertMatch(
        {"prefix", "FOO", ["one", "two", "three :blind mice"]},
        message_parser:parse(":prefix FOO one two :three :blind mice\r\n")),

    ?_assertMatch(
        {"prefix", "FOO", ["1","2","3","4","5","6","7","8","9","10","11",
            "12","13","14","15"]},
        message_parser:parse(":prefix FOO 1 2 3 4 5 6 7 8 9 10 11 12 13 "
            "14 15\r\n"))
    ].

parse_long_message_test() ->
    % Test that a maximum length message with a command and a param is accepted.
    % (FOO space) and the CRLF are 6 bytes, 512 byte message requires a
    % parameter of 506 bytes.
    OneParam = lists:duplicate(506, $x),
    ?assertMatch(
        {none, "FOO", [OneParam]},
        message_parser:parse("FOO " ++ OneParam ++ "\r\n")).

parse_too_long_message_test_() ->
    [
    ?_assertMatch(
        {not_recognised, "Too long"},
        % (FOO space) and the CRLF are 6 bytes, 513 byte message requires a
        % parameter of 507 bytes.
        message_parser:parse("FOO " ++ lists:duplicate(507, $x) ++ "\r\n"))
    ].

-endif.
