%%%% Funziona tutto

%%%% -*- Mode: Prolog -*-
%%%%  json-parsing.pl

%%% json_parse/2
% Il predicato è vero quando JSONString è un atomo che può essere parsato in un json Object     %% da riscrivere meglio come commento 

%% If JSONString is an object...
json_parse(JSONString, Object) :-
    string_chars(JSONString, JSONChars),
    pulisci_stringa(JSONChars, JSONChars_pulita),
    json_object(JSONChars_pulita, JSONChars2, [], Object),
    pulisci_stringa(JSONChars2, JSONChars3),
    is_empty(JSONChars3),
    !.
%% If JSONString is an array...
json_parse(JSONString, Object) :-
    string_codes(JSONString, JSONChars),
    pulisci_stringa(JSONChars, JSONChars_pulita),
    json_array(JSONChars_pulita, JSONChars2, [], Object),
    pulisci_stringa(JSONChars2, JSONChars3),
    %% Checks if empty to make sure there's nothing nasty at the end
    is_empty(JSONChars3),
    !.

%%% json_object(JSONSChars, JSONChars2, ObjectIn,json_obj(OutObject))

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

%%%  json_array(JSONSChars, JSONChars2, ObjectIn,json_array(ObjectOut))
%% If there's an empty array []...
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

%%%  json_member(JSONSChars, JSONCharsOut, ObjectIn, ObjectOut)
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

%%% json_elements(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut)
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

%%% json_pair(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut)
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

%%% json_value(JSONCharsIn, JSONCharsOut, Object)
json_value(Input, Resto, Output) :-
    json_string(Input, Resto, Output), % is a string
    !.
json_value(Input, Resto, Output) :-
    json_nested(Input, Resto, Output), % is a JSON
    !.
json_value(Input, Resto, Output) :-
    json_number(Input, Resto, Output), % is a number
    !.

%%% json_number(JSONCharsIn, JSONCharsOut, Object)
%% If number is float...
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

%% if number is int   
json_number(Input, Resto, Num) :-
    number_creation(Input, Resto, Int),
    is_not_empty(Int),
    !,
    number_chars(Num, Int).


%%% is_not_empty(List)
is_not_empty(Lista) :- Lista \= [], !.

%%% is_empty(List)
is_empty([]) :- !.

%%% json_nested(JSONCharsIn, JSONCharsOut, Object)
json_nested(Input, Resto, Output) :-
    json_object(Input, Resto, [], Output),
    !.
json_nested(Input, Resto, Output) :-
    json_array(Input, Resto, [], Output),
    !.

%%% first_char(CharToMatch, JSONCharsIn, JSONCharsOut)                      %%% rimuove il primo carattere dalla lista solo se CharToMatch è il primo della lista
%% Please note: the following predicates also REMOVES the object
%% from the actual char list!
first_char(String_chars, [Char | Tail], Tail):-
    atom_string(Char, String_chars).

%%% number_creation(JSONCharsIn, JSONCharsOut, JSONCharsNumber)
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

%%% spezza_stringa_apice(JSONCharsIn, JSONCharsOut, JSONCharsString)      %% "asd' : 123"=> "' : 123" e "asd"
% cerco carattere per carattere il \' , poi quando lo trovo restituisco le 2 parti
spezza_stringa_apice(['\"' | _], _, _) :-   %se trovo \" ==> fallisco
    !,
    fail.

spezza_stringa_apice(['\'' | Tail], ['\'' | Tail], []) :- !.    %Se trovo l'apice, restituisco la parte restante e inizio a riempire la lista a dx con il backtracking       

spezza_stringa_apice([H | Tail1], X, [H | Tail2]) :-
    spezza_stringa_apice(Tail1, X, Tail2).

%%% spezza_stringa_virgolette(JSONCharsIn, JSONCharsOut, JSONCharsString)
spezza_stringa_virgolette(['\'' | _], _, _) :-
    !,
    fail.
spezza_stringa_virgolette(['\"' | Tail], ['\"' | Tail], []) :- !.
spezza_stringa_virgolette([H | Tail1], X, [H | Tail2]) :-
    spezza_stringa_virgolette(Tail1, X, Tail2).


%%% pulisci_stringa(JSONCharsIn, JSONCharsOut)
pulisci_stringa([],[]) :- !.

pulisci_stringa([Char | Tail], Lista_pulita) :-
    is_spazio(Char),
    !,
    pulisci_stringa(Tail, Lista_pulita).

pulisci_stringa([H | Tail], [H | Tail]) :- !.

%%% is_spazi(JSONChars)
is_spazio(' ') :- !.
is_spazio('\n') :- !.
is_spazio('\t') :- !.

%%% json_access(JSON_obj, Fields, Result)
%% -Follows a chain of keys (iff JSON_obj at current level is an object)
%%  or indexes (iff JSON_obj at current level is an array) in order to
%%  retrieve a certain value.
%% -The main idea is to retrieve the list inside the JSON_obj
%%  and recursively work on that list.

%% Returns an identity if fields is empty
json_access(X, void, X) :- !.

%% Fails if Elements is empty
json_access(_, [], _) :- !, fail.


%% Cannot find anything in an empty object-array!
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
% If Object is an object...
access_element(json_obj([Lista_membri]), Chiave, Finale) :-
    access_member([Lista_membri], Chiave, Finale).

%% If Object is an array...
access_element(json_array([Lista_elementi]), Posizione , Result) :-
    access_element_position([Lista_elementi], Posizione, Result).

%%% access_member(JSON_list, Key, Result)
%% Searches an element given a Key (Object only!)
access_member([], _, _) :- fail.

access_member([(K,V) | _], K, V) :-
    string(K), !.

access_member([_ | Tail], K, V) :-
    string(K),
    access_member(Tail, K, V).

%%% access_element_position(JSON_list, Position, Result)
% Searches an element given an index (Array only!)
access_element_position([],[_], _) :- fail.

access_element_position([H | _], 0, H).

access_element_position([_ | Tail], Contatore, Risultato) :-
    number(Contatore),
    Count is Contatore-1,
    access_element_position(Tail, Count, Risultato).


%%% json_read(FileName, JSON).
%% -Loads a json file and returns its equivalent JSON_Object form
%% -Quite self explanatory...
json_read(Filename, Parsed) :-
    open(Filename, read, In),
    read_stream_to_codes(In, Ascii),
    close(In),
    atom_codes(JSONString, Ascii),
    json_parse(JSONString, Parsed).

%% Arrivato Qua a commentare

%%% json_dump(JSON, Filename).
%% -Writes a JSON_Object into a .json file (in JSON-compatible syntax!)
%% -The main idea is to retrieve the list inside the JSON_obj and
%%  recursively work on that list.
%%  (Same as json_access, the only difference being that it returns a string
%%  representing the list instead of a single element)
json_dump(JSON, Filename) :-
    open(Filename, write, Out),
    json_print(JSON, JSONString),
    write(Out, JSONString),
    close(Out).

%%% json_print(JSON, JSONString)
json_print(json_obj([]), "{}") :- !.

json_print(json_obj([Y | Ys]), JSONString) :-
    !,
    json_print_object([Y | Ys], "", Membri),
    concat("{", Membri, Membri_con_graffa),
    concat(Membri_con_graffa, "}", JSONString).

json_print(json_array([]), "[]") :- !.

json_print(json_array([Y | Ys]), JSONString) :-
    !,
    json_print_array([Y | Ys], "", Elementi),
    concat("[", Elementi, Elementi_con_graffa),
    concat(Elementi_con_graffa, "]", JSONString).

%%% json_print_object(JSONList, JSONString, Result)
json_print_object([], JSONString, Result) :-                %%%  Non credo serva perchè entra sempre una lista
    !,
    string_concat(Result, ", ", JSONString).

json_print_object([(X,Y)| Tail], JSONString, Result) :-
    json_print_stringa(X, Key),
    string_concat(JSONString, Key, Precedente_Key),
    string_concat(Precedente_Key, " : ", Precedente_Key_dp),
    json_print_element(Y, Value),
    string_concat(Precedente_Key_dp, Value, Precedente_KV),
    string_concat(Precedente_KV, ", ", Precedente_KV_v),
    json_print_object(Tail, Precedente_KV_v, Result).

%%% json_print_array(JSONList, JSONString, Result)
json_print_array([], JSONString, Result) :-
    !,
    string_concat(Temp, ",", JSONString),
    Result = Temp.
json_print_array([X| Xs], JSONString, Result) :-
    json_print_element(X, JSONString1),
    string_concat(JSONString, JSONString1, JSONString2),
    string_concat(JSONString2, ",", JSONString3),
    json_print_array(Xs, JSONString3, Result).

%%% json_print_element(Element, Result)
json_print_element(X, X) :-
    number(X), !.
json_print_element(X, Result) :-
    json_print(X, Result),
    !.

json_print_element(X, Result) :-
    json_print_stringa(X, Result), !.

json_print_stringa(X, Result) :-
    string(X),
    !,
    string_concat("\"", X, X_virogoletta),
    string_concat(X_virogoletta, "\"", Result).

%%%%  end of file -- json-parsing.pl







