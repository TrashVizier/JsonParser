%%%% Funziona tutto

%%%% -*- Mode: Prolog -*-
%%%%  json-parsing.pl

%%% json_parse/2
% Il predicato è vero quando JSONString è un atomo che può essere parsato in un json Object     %% da riscrivere meglio come commento 

%% Caso oggetto
json_parse(JSONString, Object) :-
    string_chars(JSONString, JSONChars),
    pulisci_stringa(JSONChars, JSONChars_pulita),
    json_object(JSONChars_pulita, JSONChars2, [], Object),
    pulisci_stringa(JSONChars2, JSONChars3),
    is_empty(JSONChars3),
    !.
%% Caso array
json_parse(JSONString, Object) :-
    string_codes(JSONString, JSONChars),
    pulisci_stringa(JSONChars, JSONChars_pulita),
    json_array(JSONChars_pulita, JSONChars2, [], Object),
    pulisci_stringa(JSONChars2, JSONChars3),
    %% Checks if empty to make sure there's nothing nasty at the end
    is_empty(JSONChars3),
    !.

%%% json_object/4

%% caso oggetto vuoto {}
json_object(Input, Restante, Precedente, json_obj(Precedente)) :-    
    first_char("{", Input, Input_senza_graffa),
    pulisci_stringa(Input_senza_graffa, Input_senza_graffa_pulito),
    first_char("}", Input_senza_graffa_pulito, Restante),
    !.
 %% caso di un oggetto "pieno" { bla bla bla}
json_object(Input, Resto, Precedente, json_obj(Successivo)) :-   
    first_char("{", Input, Input_senza_graffa),
    !,
    pulisci_stringa(Input_senza_graffa, Input_senza_graffa_pulito),
    json_members(Input_senza_graffa_pulito, Resto_members, Precedente, Successivo),
    pulisci_stringa(Resto_members, Resto_members_pulito),
    first_char("}", Resto_members_pulito, Resto).

%%%  json_array/4

%% Caso array vuoto []
json_array(Input, Resto, Precedente, json_array(Precedente)) :-
    first_char("[", Input, Input_senza_quadra),
    pulisci_stringa(Input_senza_quadra, Input_senza_quadra_pulito),
    first_char("]", Input_senza_quadra_pulito, Resto),
    !.
json_array(Input, Resto, Precedente, json_array(Successivo)) :-
    first_char("[", Input, Input_senza_quadra),
    !,
    pulisci_stringa(Input_senza_quadra, Input_senza_quadra_pulito),
    json_elements(Input_senza_quadra_pulito, Resto_elements, Precedente, Successivo),
    pulisci_stringa(Resto_elements, Resto_elements_pulito),
    first_char("]", Resto_elements_pulito, Resto).

%%%  json_member/4

%% It is essential to write both json_members and json_elements in this
%% order, since using the opposite one would result in an error (they
%% would both unify, which is wrong!). The same idea is used in the rest
%% of this program with many predicates.
json_members(Input, Resto, Precedente, Successivo) :-
    json_pair(Input, Resto_pair, Precedente, Pair_successivo),
    pulisci_stringa(Resto_pair, Resto_pair_pulito),
    first_char(",", Resto_pair_pulito, Resto_senza_virgola),
    pulisci_stringa(Resto_senza_virgola, Resto_senza_virgola_pulito),
    json_members(Resto_senza_virgola_pulito, Resto, Pair_successivo, Successivo),
    !.

json_members(Input, Resto, Precedente, Successivo) :-
    json_pair(Input, Resto, Precedente, Successivo),
    !.

%%% json_elements/4

json_elements(Input, Resto, Precedente, Successivo) :-
    json_value(Input, Resto_value, Value),
    pulisci_stringa(Resto_value, Resto_value_pulito),
    first_char(",", Resto_value_pulito, Resto_con_virgola),
    pulisci_stringa(Resto_con_virgola, Resto_con_virgola_pulito),
    !,
    append(Precedente, [Value], Successivo_value),
    json_elements(Resto_con_virgola_pulito, Resto, Successivo_value, Successivo).

json_elements(Input, Resto, Precedente, Successivo) :-
    json_value(Input, Resto, Value),
    append(Precedente, [Value], Successivo),
    !.

%%% json_pair/4
json_pair(Input, Resto_value, Precedente, Successivo) :-
    json_string(Input, Resto_stringa, Key),
    pulisci_stringa(Resto_stringa, Resto_stringa_pulito),
    first_char(":", Resto_stringa_pulito, Resto_senza_dp),
    pulisci_stringa(Resto_senza_dp, Resto_senza_dp_pulito),
    json_value(Resto_senza_dp_pulito, Resto_value, Value),
    append(Precedente, [(Key,Value)], Successivo).

%%% json_string/3
% il predicato è vero quando dalla lista di char Input può essere estratta la stringa Key e con rimanente la lista di char Resto 
json_string(Input, Resto, Key) :-
    first_char("\'", Input, Input_senza_apice),      %'
    !,
    spezza_stringa_apice(Input_senza_apice, Value_char_dp, Key_char),
    first_char("\'", Value_char_dp, Resto),     %'
    string_chars(Key, Key_char).

json_string(Input, Resto, Key) :-
    first_char("\"", Input, Input_senza_apice),
    !,
    spezza_stringa_virgolette(Input_senza_apice, Value_char_dp, Key_char),
    first_char("\"", Value_char_dp, Resto),
    string_chars(Key, Key_char).

%%% json_value/3
json_value(Input, Resto, Output) :-
    json_string(Input, Resto, Output), % is a string
    !.
json_value(Input, Resto, Output) :-
    json_annidato(Input, Resto, Output), % is a JSON
    !.
json_value(Input, Resto, Output) :-
    json_number(Input, Resto, Output), % is a number
    !.

%%% json_number/3
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
is_not_empty(Lista) :- Lista \= [], !.

%%% is_empty/1
is_empty([]) :- !.

%%% json_annidato/3
json_annidato(Input, Resto, Output) :-
    json_object(Input, Resto, [], Output),
    !.
json_annidato(Input, Resto, Output) :-
    json_array(Input, Resto, [], Output),
    !.

%%% first_char/3
%% rimuove il primo carattere dalla lista solo se CharToMatch è il primo della lista
%% Please note: the following predicates also REMOVES the object
%% from the actual char list!
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

%%% spezza_stringa_apice/3      %% "asd' : 123"=> "' : 123" e "asd"
% cerco carattere per carattere il \' , poi quando lo trovo restituisco le 2 parti
spezza_stringa_apice(['\"' | _], _, _) :-   %se trovo \" ==> fallisco
    !,
    fail.

spezza_stringa_apice(['\'' | Tail], ['\'' | Tail], []) :- !.    %Se trovo l'apice, restituisco la parte restante e inizio a riempire la lista a dx con il backtracking       

spezza_stringa_apice([H | Tail1], X, [H | Tail2]) :-
    spezza_stringa_apice(Tail1, X, Tail2).

%%% spezza_stringa_virgolette/3
spezza_stringa_virgolette(['\'' | _], _, _) :-
    !,
    fail.
spezza_stringa_virgolette(['\"' | Tail], ['\"' | Tail], []) :- !.
spezza_stringa_virgolette([H | Tail1], X, [H | Tail2]) :-
    spezza_stringa_virgolette(Tail1, X, Tail2).


%%% pulisci_stringa/2
pulisci_stringa([],[]) :- !.

pulisci_stringa([Char | Tail], Lista_pulita) :-
    is_spazio(Char),
    !,
    pulisci_stringa(Tail, Lista_pulita).

pulisci_stringa([H | Tail], [H | Tail]) :- !.

%%% is_spazio/1
is_spazio(' ').
is_spazio('\n').
is_spazio('\t').

%%% json_access/3
%% -Follows a chain of keys (iff JSON_obj at current level is an object)
%%  or indexes (iff JSON_obj at current level is an array) in order to
%%  retrieve a certain value.
%% -The main idea is to retrieve the list inside the JSON_obj
%%  and recursively work on that list.

%% Caso campo lista vuota
json_access(_, [], _) :- !, fail.


%% Casi di controllo
json_access(json_obj(), _, _) :- !, fail.
json_access(json_array(), _, _) :- !, fail.


json_access(JSON_obj, [X], Finale) :-
    access_element(JSON_obj, X, Finale),
    !.

json_access(JSON_obj, [X|Xs], Finale) :-
    access_element(JSON_obj, X, Annidato),
    !,
    json_access(Annidato, Xs, Finale).

json_access(JSON_obj, X, Finale) :-
    access_element(JSON_obj, X, Finale),
    !.

%%% access_element(JSON_obj, Fields, Result)
% Caso oggetto
access_element(json_obj([Lista_membri]), Chiave, Finale) :-
    access_member([Lista_membri], Chiave, Finale).

%% Caso array
access_element(json_array([Lista_elementi]), Posizione , Result) :-
    access_element_position([Lista_elementi], Posizione, Result).

%%% access_member/3
%% Cerca la value data la Stringa in un oggetto
access_member([], _, _) :- fail.

access_member([(S,V) | _], S, V) :-
    string(S), !.

access_member([_ | Tail], S, V) :-
    string(S),
    access_member(Tail, S, V).

%%% access_element_position/3
%% Cerca l'elemento data la posizione in un array
access_element_position([],[_], _) :- fail.

access_element_position([H | _], 0, H).

access_element_position([_ | Tail], Contatore, Risultato) :-
    number(Contatore),
    Count is Contatore-1,
    access_element_position(Tail, Count, Risultato).


%%% json_read/2
json_read(Filename, Parsed) :-
    open(Filename, read, In),
    read_stream_to_codes(In, Ascii),
    close(In),
    atom_codes(JSONString, Ascii),
    json_parse(JSONString, Parsed).

%% Arrivato Qua a commentare

%%% json_dump/2
json_dump(JSON, Filename) :-
    open(Filename, write, Out),
    json_print(JSON, JSONString),
    write(Out, JSONString),
    close(Out).

%%% json_print/2

%% Caso oggetto vuoto
json_print(json_obj([]), "{}") :- !.

json_print(json_obj(Lista_membri), JSONString) :-
    !,
    json_print_object(Lista_membri, "", Membri),
    concat("{", Membri, Membri_con_graffa),
    concat(Membri_con_graffa, "}", JSONString).

%% Caso array vuoto
json_print(json_array([]), "[]") :- !.

json_print(json_array(Lista_elementi), JSONString) :-
    !,
    json_print_array(Lista_elementi, "", Elementi),
    concat("[", Elementi, Elementi_con_graffa),
    concat(Elementi_con_graffa, "]", JSONString).

%%% json_print_object/3

json_print_object([], Precedente, Finale) :-
    !,
    string_concat(Finale, ", ", Precedente).    %% tolgo la virgola extra finale

json_print_object([(K,V)| Tail], Precedente, Finale) :-
    json_print_stringa(K, Key),
    string_concat(Precedente, Key, Precedente_Key),
    string_concat(Precedente_Key, " : ", Precedente_Key_dp),
    json_print_element(V, Value),
    string_concat(Precedente_Key_dp, Value, Precedente_KV),
    string_concat(Precedente_KV, ", ", Precedente_KV_v),
    json_print_object(Tail, Precedente_KV_v, Finale).

%%% json_print_array/3
json_print_array([], JSONString, Finale) :-
    !,
    string_concat(Finale, ",", JSONString).

json_print_array([E| Tail], Precedente, Finale) :-
    json_print_element(E, Elemento),
    string_concat(Precedente, Elemento, Precedente_el),
    string_concat(Precedente_el, ",", Precedente_el_v),
    json_print_array(Tail, Precedente_el_v, Finale).

%%% json_print_element/2
%% Caso elemento = numero
json_print_element(E, E) :-
    number(E), !.

%% Caso elemento = stringa
json_print_element(E, R) :-
    json_print_stringa(E, R), !.

%% caso elemento = sottooggetto o sottoarray
json_print_element(E, R) :-
    json_print(E, R),
    !.

%%% json_print_stringa/2
json_print_stringa(S, R) :-
    string(S),
    !,
    string_concat("\"", S, S_virogoletta),
    string_concat(S_virogoletta, "\"", R).

%%%%  end of file -- json-parsing.pl







