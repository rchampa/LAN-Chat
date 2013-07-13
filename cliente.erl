-module(cliente) .
-behavior(gen_server) . 

% API functions
-export([start_link/1, login/1, logout/1, send_msg/2, recieve_msg/3, lista_usuarios/1, times/2, stop/0]).

% Callback functions
-export([init/1, handle_call/3, handle_info/2, terminate/2, handle_cast/2, code_change/3]).

-define(NAME_SERVER, ?MODULE).
-define(LOGUEADOS, logeados).

start_link(Servidor) ->
	Ok = net_kernel:connect_node(Servidor),
	if
		Ok == true -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);

		true -> io:format("Unable to connect server~n")
	end.	

stop() -> 
	gen_server:cast(?MODULE, stop).

send_msg(Nick, Message) ->
	gen_server:cast(?MODULE, {send_msg, {Nick, Message}}).

recieve_msg(Nick, Type, Message) ->
	gen_server:cast(?MODULE, {recieve_msg, {Nick, {Type, Message}}}).

login(Nick) ->
	gen_server:cast(?MODULE, {login, Nick}).

logout(Nick) ->
	gen_server:cast(?MODULE, {logout, Nick}).


lista_usuarios(Nick) ->
	gen_server:call(?MODULE, {lista_usuarios,Nick}).

times(N, M) ->
	?MODULE ! {times_request, self(), {N, M}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callback functions implementation %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	{ok, self()}.

terminate(_Reason, _State) ->
	{_, Tab} = _State,
	db_login:cerrar(Tab),
	ok.

handle_call({lista_usuarios, Nick}, _From, _State) ->
	io:format("The users' list is: "),
	Respuesta = servidor:lista_usuarios(Nick),
	{reply, Respuesta, _State}.

handle_cast({login, Nick}, _State) ->
	servidor:login(Nick, self()),
	io:format("Login complete~n"),
	{noreply, _State};

handle_cast({logout, Nick}, _State) ->
	io:format("Logout... ~n"),
	servidor:logout(Nick, self()),
	io:format("Logout complete ~n"),
	{noreply, _State};

handle_cast({send_msg, {Nick, Msg}}, _State) -> 
	io:format("trying send a message...: ~n"),
	servidor:send_msg(Nick, Msg),
	{noreply, _State};

handle_cast({recieve_msg, {_Nick, {Type, Msg}}}, _State) -> % Msg siempre ha de llegar como string
	case Type of
		info 	->		io:format("~s > ~s~n", [hora_formateada(), Msg]);
		message	->		io:format("~w says (~s)> ~s ~n", [_Nick, hora_formateada(), Msg])
	end,
	{noreply, _State};

handle_cast(stop, _State) ->
	io:format("Stopped~n", []),
	{_, Tab} = _State,
	db_login:cerrar(Tab),
	{stop, normal, _State} .

handle_info({times_request, Pid, {N, M}}, State) ->
	io:fwrite("\t~w server handling info, number\n ~w~n", [?MODULE, State]),
	Pid ! {times_response, N * M},
	{noreply, State} .

code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.

hora_formateada() -> 
	{H, M, S} = time(),
	HS = erlang:integer_to_list(H, 10),
	MS = erlang:integer_to_list(M, 10),
	SS = erlang:integer_to_list(S, 10),
	lists:concat([HS, ':', MS, ':', SS]).

%%Last version 1.4

