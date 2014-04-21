-module(useless_irc_parser).
-compile(export_all). % replace with export later

-define(BOTPREFIX,":,").
%nick_re = re.compile('.*?Nickname is already in use')
%nick_change_re = re.compile(':(?P<old_nick>.*?)!\S+\s+?NICK\s+:\s*(?P<new_nick>[-\w]+)')
%ping_re = re.compile('^PING (?P<payload>.*)')
%chanmsg_re = re.compile(':(?P<nick>.*?)!\S+\s+?PRIVMSG\s+(?P<channel>#+[-\w]+)\s+:(?P<message>[^\n\r]+)')
%privmsg_re = re.compile(':(?P<nick>.*?)!~\S+\s+?PRIVMSG\s+[^#][^:]+:(?P<message>[^\n\r]+)')
%part_re = re.compile(':(?P<nick>.*?)!\S+\s+?PART\s+(?P<channel>#+[-\w]+)')
%join_re = re.compile(':(?P<nick>.*?)!\S+\s+?JOIN\s+.*?(?P<channel>#+[-\w]+)')
%quit_re = re.compile(':(?P<nick>.*?)!\S+\s+?QUIT\s+.*')
%registered_re = re.compile(':(?P<server>.*?)\s+(?:376|422)')

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
        true -> [parse_prefix(string:substr(Prefix,2)) | Rest];
        false -> [Prefix | Rest]
    end.

parse_msg(Msg) ->
    io:format("parse_msg:raw: ~p~n",[Msg]),
    MsgStr = binary_to_list(Msg),
    StripedMsg = string:substr(MsgStr,1,string:len(MsgStr) - 2),
    Tokens = string:tokens(StripedMsg," "),
    io:format("Tokens ~p~n",[Tokens]),
    Prefix = handle_prefix(Tokens),
    parse_params(Prefix).

parse_params([{server, Server} | Tokens]) ->
    io:format("parse_params server: ~p raw: ~p~n",
              [Server,Tokens]),
    ok;

parse_params([[{nick, Nick},{_, _},{_,_ }] | ["PRIVMSG" | Rest]]) ->
    io:format("PRIVMSG for Nick: ~p raw: ~p~n",
              [Nick,Rest]),
    {msg, Rest};

parse_params([[{nick, Nick},{_, _},{_,_ }] | Tokens]) ->
    io:format("parse_params with Nick: ~p raw: ~p~n",
              [Nick,Tokens]),
    ok;

parse_params(Command) ->
    process_cmd(Command).

process_cmd(["PING" | Server]) ->
    %% string the : in server name
    {response, "PONG " ++ string:substr(hd(Server),2)};

process_cmd(Command) ->
    io:format("Got Command ~p~n",[Command]),
    ok.

process_channel_msg(Msg) ->
    case re:run(hd(Msg),?BOTPREFIX) of
        {match,_} -> process_bot_msg(Msg);
        _ -> io:format("Channel Msg ignoring ~p~n",[Msg])
    end.

process_bot_msg(Msg) ->
    io:format("Got a Bot Channel Msg ~p~n",[Msg]),
    %% remove bot prefix
    Str = string:strip(re:replace(string:join(Msg," "),
                                  ?BOTPREFIX,"",[{return, list}])),
    io:format("Cleaned Str ~p~n",[Str]),
    ok.

process_private_msg(_Msg) ->
    %(if-let [[_ message] (re-find #"\u0001(.*)\u0001" text)]
    ok.

