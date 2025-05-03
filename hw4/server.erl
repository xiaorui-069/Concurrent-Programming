-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to leave a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    {ChatroomPid, State1} = 
        case maps:find(ChatName, State#serv_st.chatrooms) of
            error ->
                ChatPid = spawn(chatroom, start_chatroom, [ChatName]),
                {ChatPid, add_chatroom(ChatName, ChatPid, State)};
            {ok, Pid} ->
                {Pid, State}
        end,
    {ok, ClientNick} = maps:find(ClientPID, State1#serv_st.nicks),
    ChatroomPid ! {self(), Ref, register, ClientPID, ClientNick},
    add_client(ChatName, ClientPID, State1).

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    {ok, ChatroomPid} = maps:find(ChatName, State#serv_st.chatrooms),
	State1 = remove_client(ChatName, ClientPID, State),
	ChatroomPid ! {self(), Ref, unregister, ClientPID},
	ClientPID ! {self(), Ref, ack_leave},
	State1.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
	case is_nick_used(State#serv_st.nicks, NewNick) of
		true ->
			ClientPID ! {self(), Ref, err_nick_used},
			State;
		_ ->
			NewState = update_nick(ClientPID, NewNick, Ref, State),
			ClientPID ! {self(), Ref, ok_nick},
			NewState
	end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    Map = State#serv_st.nicks,
	Map_ = maps:remove(ClientPID, Map),
	NewState = State#serv_st{nicks = Map_},
	Chatrooms = maps:map(fun(K, V) ->
				case lists:member(ClientPID, V) of
					true ->
						maps:get(K, NewState#serv_st.chatrooms) ! {self(), Ref, unregister, ClientPID},
						lists:delete(ClientPID, V);
					_ ->
						V
				end
			end, NewState#serv_st.registrations),
	ClientPID ! {self(), Ref, ack_quit},
	NewState#serv_st{registrations = Chatrooms}.
	

add_chatroom(ChatName, ChatPid, State) ->
    Chatrooms1 = State#serv_st.chatrooms,
    Chatrooms2 = Chatrooms1#{ ChatName => ChatPid },
    State#serv_st{ chatrooms = Chatrooms2 }.

add_client(ChatName, ClientPID, State) ->
    Map = State#serv_st.registrations,
    UpdatedMap =
        case maps:get(ChatName, Map, undefined) of
            undefined ->
                maps:put(ChatName, [ClientPID], Map);
            PidList ->
                maps:put(ChatName, [ClientPID | PidList], Map)
        end,
    State#serv_st{ registrations = UpdatedMap }.

remove_client(ChatName, ClientPID, State) ->
    Map = State#serv_st.registrations,
    ClientList = maps:get(ChatName, Map, []),
    UpdatedList = lists:delete(ClientPID, ClientList),
    UpdatedMap = maps:put(ChatName, UpdatedList, Map),
    State#serv_st{ registrations = UpdatedMap }.

is_nick_used(Map, Nick) ->
    lists:any(
        fun(Value) -> 
            is_list(Value) andalso string:equal(Value, Nick)
        end,
        maps:values(Map)
    ).

update_nick(ClientPID, NewNick, Ref, State) ->
	NewNicks = maps:put(ClientPID, NewNick, State#serv_st.nicks),
	Chatrooms = [RoomName || {RoomName, PidList} <- 
					maps:to_list(State#serv_st.registrations), 
					lists:member(ClientPID, PidList)
				],
	lists:foreach(
        fun(RoomName) ->
            case maps:get(RoomName, State#serv_st.chatrooms, undefined) of
                undefined -> ok;
                RoomPid ->
                    RoomPid ! {self(), Ref, update_nick, ClientPID, NewNick}
            end
        end,
        Chatrooms
    ),
	State#serv_st{nicks = NewNicks}.

