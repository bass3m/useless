-module(useless_irc_parser).
-compile(export_all). % replace with export later

% return a list containing tuples with nick, user and host
% or tuple containing the server
parse_prefix(Prefix) ->
    % try to split tokens, if it's one element then it's the server
    case string:tokens(Prefix,"!|@") of
        [Nick, User, Host] -> lists:zip([nick,user,host], [Nick, User, Host]);
        [Server] -> {server, Server}
    end.

handle_prefix([Prefix | Rest]) ->
    case string:substr(Prefix,1,1) =:= ":" of
        true -> parse_prefix(string:substr(Prefix,2));
        %true -> [parse_prefix(string:substr(Prefix,2)) | Rest];
        false -> [Prefix | Rest]
    end.

parse_msg(Msg) ->
    io:format("parse_msg:raw: ~p~n",[Msg]),
    MsgStr = binary_to_list(Msg),
    StripedMsg = string:substr(MsgStr,1,string:len(MsgStr) - 2),
    Tokens = string:tokens(StripedMsg," "),
    io:format("Tokens ~p~n",[Tokens]),
    Prefix = handle_prefix(Tokens),
    parse_params(Prefix,Tokens).

parse_params({server, Server}, Tokens) ->
    io:format("parse_params server: ~p raw: ~p~n",
              [Server,Tokens]),
    ok;

parse_params([{nick, Nick},{_, _},{_,_ }], Tokens) ->
    io:format("parse_params with Nick: ~p raw: ~p~n",
              [Nick,Tokens]),
    ok;

parse_params(Command, _) ->
    parse_cmd(Command).

parse_cmd(["PING" | Server]) ->
    %% string the : in server name
    {response, "PONG " ++ string:substr(hd(Server),2)};

parse_cmd(Command) ->
    io:format("Got Command ~p~n",[Command]),
    ok.
