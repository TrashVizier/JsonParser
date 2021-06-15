%%%% Funziona tutto

%%%% -*- Mode: Prolog -*-
%%%%  json-parsing.pl

%%% json_parse/2
% Il predicato è vero quando JSONString è un atomo che può essere parsato in un json Object     %% da riscrivere meglio come commento 

%% Caso oggetto
json_parse(JSONString, Object) :-
    string_chars(JSONString, JSONChars),
    pulisci_stringa(JSONChars, JSONChars_pulita),
    json_object(JSONChars_pulita, Resto, [], Object),
    pulisci_stringa(Resto, Resto_pulito),
    is_empty(Resto_pulito),
    !.
%% Caso array
json_parse(JSONString, Object) :-
    string_codes(JSONString, JSONChars),
    pulisci_stringa(JSONChars, JSONChars_pulita),
    json_array(JSONChars_pulita, Resto, [], Object),
    pulisci_stringa(Resto, Resto_pulito),
    is_empty(Resto_pulito),
    !.

%%% json_object/4
%% il predicato è di supporto a json_parse
%% il predicato parsa l'input nel caso in cui sia un oggetto

%% caso oggetto vuoto {}
json_object(Input, Restante, Precedente, json_obj(Precedente)) :-    
    first_char("{", Input, Input_senza_graffa),
    pulisci_stringa(Input_senza_graffa, Input_senza_graffa_pulito),
    first_char("}", Input_senza_graffa_pulito, Restante),
    !.

%% caso oggetto "pieno" {"a" : 1, "b" : 2}
json_object(Input, Resto, Precedente, json_obj(Successivo)) :-   
    first_char("{", Input, Input_senza_graffa),
    !,
    pulisci_stringa(Input_senza_graffa, Input_senza_graffa_pulito),
    json_members(Input_senza_graffa_pulito, Resto_members, Precedente, Successivo),
    pulisci_stringa(Resto_members, Resto_members_pulito),
    first_char("}", Resto_members_pulito, Resto).

%%%  json_array/4
%% il predicato è di supporto a json_parse
%% il predicato parsa l'input nel caso in cui sia un array

%% Caso array vuoto []
json_array(Input, Resto, Precedente, json_array(Precedente)) :-
    first_char("[", Input, Input_senza_quadra),
    pulisci_stringa(Input_senza_quadra, Input_senza_quadra_pulito),
    first_char("]", Input_senza_quadra_pulito, Resto),
    !.

%% caso array pieno [1, 2, 3]
json_array(Input, Resto, Precedente, json_array(Successivo)) :-
    first_char("[", Input, Input_senza_quadra),
    !,
    pulisci_stringa(Input_senza_quadra, Input_senza_quadra_pulito),
    json_elements(Input_senza_quadra_pulito, Resto_elements, Precedente, Successivo),
    pulisci_stringa(Resto_elements, Resto_elements_pulito),
    first_char("]", Resto_elements_pulito, Resto).

%%%  json_member/4
%% il predicato è di supporto a json_object
%% il predicato parsa l'input nel caso in cui sia una lista di pairs

%% caso generale 
json_members(Input, Resto, Precedente, Successivo) :-
    json_pair(Input, Resto_pair, Precedente, Pair_successivo),
    pulisci_stringa(Resto_pair, Resto_pair_pulito),
    first_char(",", Resto_pair_pulito, Resto_senza_virgola),
    pulisci_stringa(Resto_senza_virgola, Resto_senza_virgola_pulito),
    json_members(Resto_senza_virgola_pulito, Resto, Pair_successivo, Successivo),
    !.

%% caso ultima pair
json_members(Input, Resto, Precedente, Successivo) :-
    json_pair(Input, Resto, Precedente, Successivo),
    !.

%%% json_elements/4
%% il predicato è di supporto a json_array
%% il predicato parsa l'input nel caso in cui sia una lista di value all'interno di un array

%% caso generale
json_elements(Input, Resto, Precedente, Successivo) :-
    json_value(Input, Resto_value, Value),
    pulisci_stringa(Resto_value, Resto_value_pulito),
    first_char(",", Resto_value_pulito, Resto_con_virgola),
    pulisci_stringa(Resto_con_virgola, Resto_con_virgola_pulito),
    !,
    append(Precedente, [Value], Successivo_value),
    json_elements(Resto_con_virgola_pulito, Resto, Successivo_value, Successivo).

%% caso ultima value
json_elements(Input, Resto, Precedente, Successivo) :-
    json_value(Input, Resto, Value),
    append(Precedente, [Value], Successivo),
    !.

%%% json_pair/4
%% il predicato è di supporto a json_members
%% il predicato parsa l'input nel caso in cui sia una pair

json_pair(Input, Resto_value, Precedente, Successivo) :-
    json_string(Input, Resto_stringa, String),
    pulisci_stringa(Resto_stringa, Resto_stringa_pulito),
    first_char(":", Resto_stringa_pulito, Resto_senza_dp),
    pulisci_stringa(Resto_senza_dp, Resto_senza_dp_pulito),
    json_value(Resto_senza_dp_pulito, Resto_value, Value),
    append(Precedente, [(String, Value)], Successivo).

%%% json_string/3
%% il predicato è di supporto
% il predicato è vero quando dalla lista di char Input può essere estratta la stringa String e con rimanente la lista di char Resto 

%% caso stringa con apici 'stringa'
json_string(Input, Resto, String) :-
    first_char("\'", Input, Input_senza_apice),      %'%COMMENTO DA TOGLIERE
    !,
    spezza_stringa_apice(Input_senza_apice, Value_char_dp, String_char),
    first_char("\'", Value_char_dp, Resto),     %'%COMMENTO DA TOGLIERE
    string_chars(String, String_char).

%% caso stringa con virgolette "stringa"
json_string(Input, Resto, Key) :-
    first_char("\"", Input, Input_senza_apice),
    !,
    spezza_stringa_virgolette(Input_senza_apice, Value_char_dp, Key_char),
    first_char("\"", Value_char_dp, Resto),
    string_chars(Key, Key_char).

%%% json_value/3
%% il predicato è di supporto
%% il predicato determina la tipologia della value Input e la parsas ne modo corretto

%% caso string
json_value(Input, Resto, Output) :-
    json_string(Input, Resto, Output),
    !.

%% caso number
json_value(Input, Resto, Output) :-
    json_number(Input, Resto, Output),
    !. 

%% caso oggetto annidato
json_value(Input, Resto, Output) :-
    json_object(Input, Resto, [], Output),
    !.

%% caso array annidato
json_value(Input, Resto, Output) :-
    json_array(Input, Resto, [], Output),
    !.


%%% json_number/3
%% il predicato è di supporto
%% il predicato parsa l'input nel caso in cui sia un numero (integer o float)

%% caso numero float (numero.numero)
json_number(Input, Resto, Num) :-
    number_creation(Input, Resto, Int),
    is_not_empty(Int),
    first_char(".", Resto, Resto_senza_punto),
    !,
    append(Int, ['.'], Int_con_punto),
    number_creation(Resto_senza_punto, Resto, Decimale),
    is_not_empty(Decimale),
    append(Int_con_punto, Decimale, Float),
    number_chars(Num, Float).

%% caso numero int 
json_number(Input, Resto, Num) :-
    number_creation(Input, Resto, Int),
    is_not_empty(Int),
    !,
    number_chars(Num, Int).


%%% is_not_empty/1
%% predicato di controllo
is_not_empty(Lista) :- Lista \= [], !.


%%% first_char/3
%% rimuove il primo carattere dalla lista solo se CharToMatch è il primo della lista

first_char(String_chars, [Char | Tail], Tail):-
    atom_string(Char, String_chars).

%%% number_creation/3
number_creation([H | Tail], [H | Tail], []) :-
    not(cifra(H)),
    !.
number_creation([H | Tail1], X, [H | Tail2]) :-
    number_creation(Tail1, X, Tail2).

cifra('0').
cifra('1').
cifra('2').
cifra('3').
cifra('4').
cifra('5').
cifra('6').
cifra('7').
cifra('8').
cifra('9').

%%% spezza_stringa_apice/3 
%% il predicato cerca carattere per carattere l'apice e poi quando lo trova restituisce le 2 liste a sx e a dx di esso
%% "asd' : 123"=> "' : 123" e "asd"

%% caso in cui trovo delle virgolette => fallisco
spezza_stringa_apice(['\"' | _], _, _) :-   
    !,
    fail.
    
%% caso in cui trovo l'apice => restituisco la parte restante e riempio la lista a dx con il backtracking       
spezza_stringa_apice(['\'' | Tail], ['\'' | Tail], []).

%% caso generale
spezza_stringa_apice([H | Tail1], X, [H | Tail2]) :-
    spezza_stringa_apice(Tail1, X, Tail2).



%%% spezza_stringa_virgolette/3
%% il predicato è analogo a spezza_stringa_apice, ma con le virgolette al posto dell'apice

%% caso in cui trovo un apice => fallisco
spezza_stringa_virgolette(['\'' | _], _, _) :-
    !,
    fail.

%% caso in cui trovo le virgolette => restituisco la parte restante e riempio la lista a dx con il backtracking
spezza_stringa_virgolette(['\"' | Tail], ['\"' | Tail], []).

%% caso generale
spezza_stringa_virgolette([H | Tail1], X, [H | Tail2]) :-
    spezza_stringa_virgolette(Tail1, X, Tail2).


%%% pulisci_stringa/2
%% Il predicato è vero quando la lista a dx è uguale alla lista a sx, ma senza i caratteri spazio iniziali  
pulisci_stringa([],[]) :- !.

pulisci_stringa([Char | Tail], Lista_pulita) :-
    is_spazio(Char),
    !,
    pulisci_stringa(Tail, Lista_pulita).

pulisci_stringa([H | Tail], [H | Tail]) :- !.

%%% is_spazio/1
%% Il predicato è di controllo
is_spazio(' ').
is_spazio('\n').
is_spazio('\t').

%%% json_access/3
%% Il predicato è  vero quando il terzo argomento è recuperabile 
%% seguendo la catena di campi presenti nel secondo argomento
%% (una lista) a partire dal primo argomento (un json-parsato).
%% Un campo rappresentato da N (con N un numero maggiore o
%% uguale a 0) corrisponde a un indice di un array JSON.

%% Caso campo lista vuota => fallisco
json_access(_, [], _) :- !, fail.


%% Casi di controllo banalità
json_access(json_obj(), _, _) :- !, fail.
json_access(json_array(), _, _) :- !, fail.

%% Caso campo mono-argomento 
json_access(JSON_obj, X, Finale) :-
    json_access_supp(JSON_obj, X, Finale),
    !.

%% Caso campo lista lunghezza 1
json_access(JSON_obj, [X], Finale) :-
    json_access_supp(JSON_obj, X, Finale),
    !.

%% Caso campo lista lunghezza > 1
json_access(JSON_obj, [X|Xs], Finale) :-
    json_access_supp(JSON_obj, X, Annidato),
    !,
    json_access(Annidato, Xs, Finale).


%%% json_access_supp/3
%% Il predicato è di supporto a json_access
%% Il predicato controlla gli elementi di un array

% Caso oggetto
json_access_supp(json_obj([Lista_membri]), Chiave, Finale) :-
    access_member([Lista_membri], Chiave, Finale).

%% Caso array
json_access_supp(json_array([Lista_elementi]), Posizione , Result) :-
    access-posizione-array([Lista_elementi], Posizione, Result).

%%% access_member/3
%% Cerca la value data la Stringa in un oggetto
access_member([], _, _) :- fail.

access_member([(S, V) | _], S, V) :-
    string(S), !.

access_member([_ | Tail], S, V) :-
    string(S),
    access_member(Tail, S, V).

%%% access-posizione-array/3
%% Cerca l'elemento data la posizione in un array

access-posizione-array([],[_], _) :- fail.

access-posizione-array([H | _], 0, H).

access-posizione-array([_ | Tail], Contatore, Risultato) :-
    number(Contatore),
    Count is Contatore - 1,
    access-posizione-array(Tail, Count, Risultato).


%%% json_read/2
%% Il predicato è vero quando Filename è il nome di un file .json e Parsed è
%% il contenuto del file parsato

json_read(Filename, Parsed) :-
    open(Filename, read, In),
    read_stream_to_codes(In, Ascii),
    close(In),
    atom_codes(JSONString, Ascii),
    json_parse(JSONString, Parsed).

%%% json_dump/2
%% Il predicato è vero quando il json parsato JSON viene scritto sul file 
%% Filename

json_dump(JSON, Filename) :-
    open(Filename, write, Out),
    json_print(JSON, JSONString),
    write(Out, JSONString),
    close(Out).

%%% json_print/2
%% Il predicato è di supporto a json_dump
%% Il predicato "de-parsa" il json parsato e lo restituisce come stringa

%% Caso oggetto vuoto
json_print(json_obj([]), "{}").

%% Caso generale oggetto
json_print(json_obj(Lista_membri), JSONString) :-
    !,
    json_print_object(Lista_membri, "", Membri),
    concat("{", Membri, Membri_con_graffa),
    concat(Membri_con_graffa, "}", JSONString).

%% Caso array vuoto
json_print(json_array([]), "[]").

%% Caso generale array
json_print(json_array(Lista_elementi), JSONString) :-
    !,
    json_print_array(Lista_elementi, "", Elementi),
    concat("[", Elementi, Elementi_con_graffa),
    concat(Elementi_con_graffa, "]", JSONString).


%%% json_print_object/3
%% Il predicato è di supporto a json_print

json_print_object([], Precedente, Finale) :-
    !,
    string_concat(Finale, ", ", Precedente).    % tolgo la virgola extra finale

json_print_object([(K,V)| Tail], Precedente, Finale) :-
    json_print_stringa(K, Key),
    string_concat(Precedente, Key, Precedente_Key),
    string_concat(Precedente_Key, " : ", Precedente_Key_dp),
    json_print_value(V, Value),
    string_concat(Precedente_Key_dp, Value, Precedente_KV),
    string_concat(Precedente_KV, ", ", Precedente_KV_v),
    json_print_object(Tail, Precedente_KV_v, Finale).

%%% json_print_array/3
%% Il predicato è di supporto a json_print

json_print_array([], Precedente, Finale) :-
    !,
    string_concat(Finale, ", ", Precedente).    % tolgo la virgola extra finale

json_print_array([E| Tail], Precedente, Finale) :-
    json_print_value(E, Elemento),
    string_concat(Precedente, Elemento, Precedente_el),
    string_concat(Precedente_el, ", ", Precedente_el_v),
    json_print_array(Tail, Precedente_el_v, Finale).

%%% json_print_value/2
%% Il predicato è di supporto

%% Caso elemento = numero
json_print_value(E, E) :-
    number(E), !.

%% Caso elemento = stringa
json_print_value(E, R) :-
    json_print_stringa(E, R), !.

%% caso elemento = sottooggetto o sottoarray
json_print_value(E, R) :-
    json_print(E, R),
    !.

%%% json_print_stringa/2
%% Il predicato è di supporto a json_print_object

json_print_stringa(S, R) :-
    string(S),
    !,
    string_concat("\"", S, S_virogoletta),
    string_concat(S_virogoletta, "\"", R).

%%%%  end of file -- json-parsing.pl







