-module(servidor) .
-behavior(gen_server) . 
-import(db_login).

% API functions
-export([start_link/0, login/2, logout/2,send_msg/2, get_pid_of/1, lista_usuarios/1, stop/0]).

% Callback functions
-export([init/1, terminate/2, handle_call/3, handle_info/2, handle_cast/2, code_change/3]).

-define(NAME_SERVER, ?MODULE).
-define(LOGUEADOS, logeados).


start_link() ->
	gen_server:start_link({global, ?NAME_SERVER}, ?MODULE, [], []).

lista_usuarios(Nick) ->
	gen_server:call({global, ?NAME_SERVER}, {lista_usuarios, Nick}).

get_pid_of(Nick) ->
	gen_server:call({global, ?NAME_SERVER}, {get_pid_of, Nick}).


login(Nick,Pid) ->
	gen_server:cast({global, ?NAME_SERVER}, {login, {Nick, Pid}}). 

logout(Nick, Pid) ->
	gen_server:cast({global, ?NAME_SERVER}, {logout, {Nick, Pid}}).  

send_msg(Nick, Message) ->
	gen_server:cast({global, ?NAME_SERVER}, {send_msg, {Nick, Message}}).

stop() -> 
	gen_server:cast({global, ?NAME_SERVER}, stop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callback functions implementation %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	compile:file(db_login),
	{ok, db_login:crear(?LOGUEADOS)}.

terminate(Reason, _State) ->
	io:fwrite("~w server terminated with reason: ~p~n", [?MODULE, Reason]) .

handle_call({lista_usuarios, Nick}, _From, _State) ->
	io:format("Listing users..."),
	BOOL = db_login:esta(_State, Nick),
	io:format("Was the user online? = ~w ~n", [BOOL]),
	if
		BOOL == true -> Lista = db_login:lista_usuarios(_State);
		true -> Lista = []
	end,
	{reply, Lista, _State};


handle_call({get_pid_of, Nick}, _From, _State) ->
	io:format("Getting Pid...~n", []),
	Pid = db_login:getValor(_State, Nick),
	{reply, Pid, _State}.

handle_cast({send_msg, {Nick, Msg}}, _State) ->
	io:format("trying to re-send the message for all from server... y state = ~w: ~n", [_State]),
	BOOL = db_login:esta(_State, Nick),
	io:format("Was the user online? = ~w ~n", [BOOL]),
	if
		BOOL == true ->
					F = fun(User) -> 
							Pid = db_login:getValor(_State, User),
							gen_server:cast(Pid, {recieve_msg, {Nick, {message, Msg}}})
						end,
					lists:foreach(F, db_login:lista_usuarios(_State));

		true -> io:format("~w was not logged", [Nick])
	end,

	{noreply, _State};


handle_cast({login, {Nick, _Pid}}, _State) ->
	BOOL = db_login:esta(_State, Nick),
	io:format("Was the user online? = ~w ~n", [BOOL]),
	io:format("State? = ~w ~n", [_State]),
	%if
	%	BOOL == true -> io:format("~w it's already login", [Nick]);

	%	true ->	db_login:login(_State, {Nick, Pid}),
	%			io:format("Login succesful ~n")
	%end,
	%{noreply, _State};

	if
		BOOL == true -> io:format("~w it's already login", [Nick]);

		true -> 	io:format("~w state", [_State]),
				db_login:login(_State,{Nick, _Pid}),
				io:format("Login succesful ~n"),
				EM = fun(User) -> 
					Pid = db_login:getValor(_State, User),
					Nick_S = lists:flatten(io_lib:format("~p", [Nick])),%Pasar a string 
					gen_server:cast(Pid, {recieve_msg, {Nick, {info, string:concat(Nick_S," has connected...")}}})
				end,
				lists:foreach ( EM, db_login:lista_usuarios(_State))
	end,
	{noreply, _State};



handle_cast({logout, {Nick, _Pid}}, _State) ->
	io:format("Logout from server...~n"),
	BOOL = db_login:esta(_State, Nick),
	io:format("Was the user online? = ~w ~n", [BOOL]),
	if
		BOOL == true -> db_login:logout(_State,Nick),
				io:format("Logout succesful ~n"),
				EM = fun(User) -> 
					Pid = db_login:getValor(_State, User),
					Nick_S = lists:flatten(io_lib:format("~p", [Nick])),%Pasar a string 
					gen_server:cast(Pid, {recieve_msg, {Nick, {info, string:concat(Nick_S," has disconnected...")}}})
				end,
				lists:foreach ( EM, db_login:lista_usuarios(_State));

		true -> io:format("~w was not logged", [Nick])
	end,
	{noreply, _State};


handle_cast(stop, _State) ->
	db_login:cerrar(_State),
	{stop, normal, _State} .


	
handle_info({'EXIT', Pid, Reason}, _State) ->
	io:fwrite("~w server: process ~w sent me an exit signal with reason ~w, server still up~n", [?MODULE, Pid, Reason]),
	{noreply, _State} .

code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.




%%Pruebas
%Se inicia un nodo server(desde un terminal con erl -sname server) y todos los clientes que se quieran de manera análoga
%Se inicia el modulo servidor desde el nodo servidor: servidor:start_link()
%Se inicia el modulo cliente desde el nodo cliente: cliente:start_link(nodo_servidor)
%se verifica con nodes() que estan en un mismo host

%Finalmente se envian peticiones desde cualquier cliente de la forma:
%(clienteN@dominio)cliente:login(pepito).
%(clienteN@dominio)cliente:lista_usuarios(juanito).
%(clienteN@dominio)cliente:send_msg(pepito,mensaje). 
%(clienteN@dominio)cliente:logout(pepito).



