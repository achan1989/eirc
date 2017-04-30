-module(message_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_valid_test_() ->
    [
    ?_assertMatch(
        <<"FOO\r\n">>,
        message:new("FOO")),

    ?_assertMatch(
        <<"FOO a\r\n">>,
        message:new("FOO", ["a"])),

    ?_assertMatch(
        <<"FOO a b\r\n">>,
        message:new("FOO", ["a", "b"])),

    ?_assertMatch(
        <<"FOO a b :c\r\n">>,
        message:new("FOO", ["a", "b"], "c")),

    ?_assertMatch(
        <<"FOO a b :c1 c2\r\n">>,
        message:new("FOO", ["a", "b"], "c1 c2")),

    ?_assertMatch(
        <<"FOO a b ::c1 c2\r\n">>,
        message:new("FOO", ["a", "b"], ":c1 c2")),

    ?_assertMatch(
        <<":prefix3!@{} FOO a b ::c1 c2\r\n">>,
        message:new("prefix3!@{}", "FOO", ["a", "b"], ":c1 c2"))
    ].

-endif.
