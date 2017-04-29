-module(message).

-export([new/2,new/3,transform_params/1]).

-define(EMPTY, <<"">>).
-define(SPACE, <<" ">>).
-define(COLON, <<":">>).
-define(CRLF, <<"\r\n">>).

-define(MAX_MESSAGE_LENGTH, 512).


%% See https://tools.ietf.org/html/rfc2812#section-2.3


new(Command, Parameters) ->
    new(none, Command, Parameters).
new(none, Command, Parameters) ->
    Packet = case transform_params(Parameters) of
        ?EMPTY ->
            <<Command/bytes, ?CRLF/bytes>>;
        ParamsBin ->
            <<Command/bytes, ?SPACE/bytes, ParamsBin/bytes, ?CRLF/bytes>>
    end,
    assert_length(Packet);
new(Prefix, Command, Parameters) ->
    Packet = case transform_params(Parameters) of
        ?EMPTY ->
            <<?COLON/bytes, Prefix/bytes, ?SPACE/bytes,
              Command/bytes, ?CRLF/bytes>>;
        ParamsBin ->
            <<?COLON/bytes, Prefix/bytes, ?SPACE/bytes,
              Command/bytes, ?SPACE/bytes, ParamsBin/bytes, ?CRLF/bytes>>
    end,
    assert_length(Packet).

% Transform a list of (normal or binary) strings into a single binary string in
% IRC format.
% IRC format mean parameters are separated by a space, and the last parameter
% is preceded by a colon (not always necessary, but we'll do it every time to
% make the transformation easier).
transform_params([]) ->
    ?EMPTY;
transform_params([P|[]]) when is_binary(P) ->
    <<?COLON/bytes, P/bytes>>;
transform_params([P|[]]) when is_list(P) ->
    B = list_to_binary(P),
    <<?COLON/bytes, B/bytes>>;
transform_params([P|Params]=List) when length(List) =< 15, is_binary(P) ->
    transform_params(Params, <<P/bytes>>);
transform_params([P|Params]=List) when length(List) =< 15, is_list(P) ->
    B = list_to_binary(P),
    transform_params(Params, <<B/bytes>>).

transform_params([P|[]], BS) when is_binary(P) ->
    <<BS/bytes, <<" :">>/bytes, P/bytes>>;
transform_params([P|[]], BS) when is_list(P) ->
    B = list_to_binary(P),
    <<BS/bytes, <<" :">>/bytes, B/bytes>>;
transform_params([P|Params], BS) when is_binary(P) ->
    transform_params(Params, <<BS/bytes, ?SPACE/bytes, P/bytes>>);
transform_params([P|Params], BS) when is_list(P) ->
    B = list_to_binary(P),
    transform_params(Params, <<BS/bytes, ?SPACE/bytes, B/bytes>>).

assert_length(Packet) when size(Packet) =< ?MAX_MESSAGE_LENGTH ->
    Packet.
