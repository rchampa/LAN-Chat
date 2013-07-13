-module(db_login).
-export([crear/1,  login/2, logout/2, cerrar/1, getValor/2, esta/2, lista_usuarios/1, getTablas/0, getInfo/1]).

% Creacion de la ETS.
crear(Tab) ->
	ets:new(Tab, [set]). %% No permite duplicidades de claves

% Insertar nuevos usuarios, si la clave ya pertenece a la BD, devuelve false;
%	agrega al usuario en caso contrario y devuelve true.
login(Tab, Objects) ->
	ets:insert_new(Tab, Objects).

% Cerrar la ETS.
cerrar(Tab) ->
	ets:delete(Tab).

% Hacer el logout, sacarlo de la tabla de login.
logout(Tab, Key) ->
	ets:delete(Tab, Key).

% Devolver los valores asociados a la clave correspondiente.
getValor(Tab, Key) ->
	X = ets:lookup(Tab, Key),
	if
		X == [] -> false;
		true ->	[{_, V} | _] = X,
				V
	end.

% Busca la clave en la tabla y devuelve true si la encuentra y false en otro caso.
esta(Tab, Key) ->
	ets:member(Tab, Key).

% Listar usuarios
lista_usuarios(Tabla) -> 
	Lista = lista_usuarios(Tabla, ets:first(Tabla), []),
	ets:safe_fixtable(Tabla, true),
	Lista.

lista_usuarios(_, '$end_of_table', Acc) ->
	Acc;

lista_usuarios(Tabla, Clave, Acc) ->
	{Nick, _} = cabezaLista(ets:lookup(Tabla, Clave)),
    	lista_usuarios(Tabla, ets:next(Tabla, Clave), [Nick | Acc]).

% Devolver la informacion de la tabla.
getInfo(Tab) ->
	ets:info(Tab).

% Retorna una lista con todas las tablas del nodo.
getTablas() ->
	ets:all().

%%%%%%%%%%%%%%% OPERACIONES PRIVADAS %%%%%%%%%%%%%%%%%%

% Funcion que solo se puede usar con listas de al menos un elemento
cabezaLista([X|_])	-> X.


% Retorna la clave del primer elemento de la tabla. Si la entidad está vacia retorna $end_of_table.
%primera(Tab) ->
%	ets:first(Tab).


% Retorna la clave del ultimo elemento de la tabla. Si la entidad está vacia retorna $end_of_table.
%ultima(Tab) ->
%	ets:last(Tab).

% Retorna la clave del elemento siguiente a una clave dada.
%siguiente(Tab, Key) ->
%	ets:next(Tab, Key).

% Retorna la clave del elemento anterior a una clave dada.
%previo(Tab, Key) ->
%	ets:prev(Tab, Key).

