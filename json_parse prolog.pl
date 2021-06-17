%%%% -*- Mode: Prolog -*-
%%%%  json_parsing.pl

%%% json_parse/2
%% Il predicato è vero quando JSONString è un atomo che può essere parsato in 
%% un json Object

%% Caso oggetto
json_parse(JSONString, Object) :-
    atom_chars(JSONString, JSONChars),
    pulisci_stringa(JSONChars, JSONChars_pulita),
    parse_object(JSONChars_pulita, Resto, [], Object),
    pulisci_stringa(Resto, Resto_pulito),
    is_empty(Resto_pulito),
    !.
%% Caso array
json_parse(JSONString, Object) :-
    string_codes(JSONString, JSONChars),
    pulisci_stringa(JSONChars, JSONChars_pulita),
    parse_array(JSONChars_pulita, Resto, [], Object),
    pulisci_stringa(Resto, Resto_pulito),
    is_empty(Resto_pulito),
    !.

%%% parse_object/4
%% il predicato è di supporto a json_parse
%% il predicato parsa l'input nel caso in cui sia un oggetto

%% caso oggetto vuoto {}
parse_object(Input, Restante, Prec, json_obj(Prec)) :-    
    tronca_char("{", Input, Input_senza_graffa),
    pulisci_stringa(Input_senza_graffa, Input_senza_graffa_pulito),
    tronca_char("}", Input_senza_graffa_pulito, Restante),
    !.

%% caso oggetto "pieno" {"a" : 1, "b" : 2}
parse_object(Input, Resto, Prec, json_obj(Succ)) :-   
    tronca_char("{", Input, Input_senza_graffa),
    !,
    pulisci_stringa(Input_senza_graffa, Input_senza_graffa_pulito),
    parse_members(Input_senza_graffa_pulito, Resto_members, Prec, Succ),
    pulisci_stringa(Resto_members, Resto_members_pulito),
    tronca_char("}", Resto_members_pulito, Resto).

%%%  parse_array/4
%% il predicato è di supporto a json_parse
%% il predicato parsa l'input nel caso in cui sia un array

%% Caso array vuoto []
parse_array(Input, Resto, Prec, json_array(Prec)) :-
    tronca_char("[", Input, Input_senza_quadra),
    pulisci_stringa(Input_senza_quadra, Input_senza_quadra_pulito),
    tronca_char("]", Input_senza_quadra_pulito, Resto),
    !.

%% caso array pieno [1, 2, 3]
parse_array(Input, Resto, Prec, json_array(Succ)) :-
    tronca_char("[", Input, Input_senza_quadra),
    !,
    pulisci_stringa(Input_senza_quadra, Input_senza_quadra_pulito),
    parse_elements(Input_senza_quadra_pulito, Resto_elements, Prec, Succ),
    pulisci_stringa(Resto_elements, Resto_elements_pulito),
    tronca_char("]", Resto_elements_pulito, Resto).

%%%  parse_members/4
%% il predicato è di supporto a parse_object
%% il predicato parsa l'input nel caso in cui sia una lista di pairs

%% caso generale 
parse_members(Input, Resto, Prec, Succ) :-
    parse_pair(Input, Resto_pair, Prec, Pair_succ),
    pulisci_stringa(Resto_pair, Resto_pair_pulito),
    tronca_char(",", Resto_pair_pulito, Resto_senza_virgola),
    pulisci_stringa(Resto_senza_virgola, Resto_senza_virgola_pulito),
    parse_members(Resto_senza_virgola_pulito, Resto, Pair_succ, Succ),
    !.

%% caso ultima pair
parse_members(Input, Resto, Prec, Succ) :-
    parse_pair(Input, Resto, Prec, Succ),
    !.

%%% parse_elements/4
%% il predicato è di supporto a parse_array
%% il predicato parsa l'input nel caso in cui sia una lista di value 
%% all'interno di un array

%% caso generale
parse_elements(Input, Resto, Prec, Succ) :-
    parse_value(Input, Resto_value, Value),
    pulisci_stringa(Resto_value, Resto_value_pulito),
    tronca_char(",", Resto_value_pulito, Resto_con_virgola),
    pulisci_stringa(Resto_con_virgola, Resto_con_virgola_pulito),
    !,
    append(Prec, [Value], Succ_value),
    parse_elements(Resto_con_virgola_pulito, Resto, Succ_value, Succ).

%% caso ultima value
parse_elements(Input, Resto, Prec, Succ) :-
    parse_value(Input, Resto, Value),
    append(Prec, [Value], Succ),
    !.

%%% parse_pair/4
%% il predicato è di supporto a parse_members
%% il predicato parsa l'input nel caso in cui sia una pair

parse_pair(Input, Resto_value, Prec, Succ) :-
    parse_string(Input, Resto_stringa, String),
    pulisci_stringa(Resto_stringa, Resto_stringa_pulito),
    tronca_char(":", Resto_stringa_pulito, Resto_senza_dp),
    pulisci_stringa(Resto_senza_dp, Resto_senza_dp_pulito),
    parse_value(Resto_senza_dp_pulito, Resto_value, Value),
    append(Prec, [(String, Value)], Succ).

%%% parse_string/3
%% il predicato è di supporto
%% il predicato è vero quando dalla lista di char Input può essere estratta la
%% stringa String e con rimanente la lista di char Resto 

%% caso stringa con apici 'stringa'
parse_string(Input, Resto, String) :-
    tronca_char("\'", Input, Input_senza_apice),      %'%COMMENTO DA TOGLIERE
    !,
    spezza_string_apice(Input_senza_apice, Value_char_dp, String_char),
    tronca_char("\'", Value_char_dp, Resto),     %'%COMMENTO DA TOGLIERE
    string_chars(String, String_char).

%% caso stringa con virgolette "stringa"
parse_string(Input, Resto, Key) :-
    tronca_char("\"", Input, Input_senza_apice),
    !,
    spezza_string_virgolette(Input_senza_apice, Value_char_dp, Key_char),
    tronca_char("\"", Value_char_dp, Resto),
    string_chars(Key, Key_char).

%%% parse_value/3
%% il predicato è di supporto
%% il predicato determina il tipo di value Input e la parsa nel modo corretto

%% caso string
parse_value(Input, Resto, Output) :-
    parse_string(Input, Resto, Output),
    !.

%% caso number
parse_value(Input, Resto, Output) :-
    parse_number(Input, Resto, Output),
    !. 

%% caso oggetto annidato
parse_value(Input, Resto, Output) :-
    parse_object(Input, Resto, [], Output),
    !.

%% caso array annidato
parse_value(Input, Resto, Output) :-
    parse_array(Input, Resto, [], Output),
    !.


%%% parse_number/3
%% il predicato è di supporto
%% il predicato parsa l'input nel caso in cui sia un numero (integer o float)

%% caso numero float (numero.numero)
parse_number(Input, Resto, Num) :-
    spezza_number(Input, Resto, Int),
    is_not_empty(Int),
    tronca_char(".", Resto, Resto_senza_punto),
    !,
    append(Int, ['.'], Int_con_punto),
    spezza_number(Resto_senza_punto, Resto, Decimale),
    is_not_empty(Decimale),
    append(Int_con_punto, Decimale, Float),
    number_chars(Num, Float).

%% caso numero int 
parse_number(Input, Resto, Num) :-
    spezza_number(Input, Resto, Int),
    is_not_empty(Int),
    !,
    number_chars(Num, Int).


%%% is_not_empty/1
%% predicato di controllo
is_not_empty(Lista) :- Lista \= [].

%%% is_empty/1
%% predicato di controllo
is_empty([]).

%%% tronca_char/3
%% Il predicato rimuove il primo carattere dalla lista solo se CharToMatch 
%% e' il primo char della lista

tronca_char(String_chars, [Char | Tail], Tail):-
    atom_string(Char, String_chars).

%%% spezza_number/3
%% Il predicato ha una funziona analoga a spezza_string_apice e
%% spezza_string_virgolette, ma con i numeri

spezza_number([H | Tail], [H | Tail], []) :-
    not(digit(H)),
    !.
spezza_number([H | Tail1], X, [H | Tail2]) :-
    spezza_number(Tail1, X, Tail2).

digit('0').
digit('1').
digit('2').
digit('3').
digit('4').
digit('5').
digit('6').
digit('7').
digit('8').
digit('9').

%%% spezza_string_apice/3 
%% il predicato cerca carattere per carattere l'apice e poi quando lo
%% trova restituisce le 2 liste a dx e a sx di esso
%% N.B. Le 2 liste sono invertite
%% "asd' : 123"=> "' : 123" e "asd"

%% caso in cui trovo delle virgolette
spezza_string_apice(['\"' | _], _, _) :-   
    !,
    fail.
    
%% caso in cui trovo l'apice    
spezza_string_apice(['\'' | Tail], ['\'' | Tail], []) :- !.

%% caso generale
spezza_string_apice([H | Tail1], X, [H | Tail2]) :-
    spezza_string_apice(Tail1, X, Tail2).



%%% spezza_string_virgolette/3
%% il predicato è analogo a spezza_string_apice,
%% ma con le virgolette al posto dell'apice

%% caso in cui trovo un apice 
spezza_string_virgolette(['\'' | _], _, _) :-
    !,
    fail.

%% caso in cui trovo le virgolette
spezza_string_virgolette(['\"' | Tail], ['\"' | Tail], []) :- !.

%% caso generale
spezza_string_virgolette([H | Tail1], X, [H | Tail2]) :-
    spezza_string_virgolette(Tail1, X, Tail2).


%%% pulisci_stringa/2
%% Il predicato è vero quando la lista a dx è uguale alla lista a sx,
%% ma senza i caratteri spazio iniziali  

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
    cerca_valore([Lista_membri], Chiave, Finale).

%% Caso array
json_access_supp(json_array([Lista_elementi]), Posizione , Result) :-
    cerca_posizione([Lista_elementi], Posizione, Result).

%%% cerca_valore/3
%% Cerca la value data la Stringa in un oggetto
cerca_valore([], _, _) :- fail.

cerca_valore([(S, V) | _], S, V) :-
    string(S), !.

cerca_valore([_ | Tail], S, V) :-
    string(S),
    cerca_valore(Tail, S, V).

%%% cerca_posizione/3
%% Cerca l'elemento data la posizione in un array

cerca_posizione([],[_], _) :- fail.

cerca_posizione([H | _], 0, H).

cerca_posizione([_ | Tail], Contatore, Risultato) :-
    number(Contatore),
    Count is Contatore - 1,
    cerca_posizione(Tail, Count, Risultato).


%%% json_read/2
%% Il predicato è vero quando Filename è il nome di un file .json e Parsed è
%% il contenuto del file parsato

json_read(Filename, Parsed) :-
    open(Filename, read, In),
    read_stream_to_codes(In, Ascii),
    close(In),
    pulisci_ascii(Ascii, Ascii_pulito),
    atom_codes(JSONString, Ascii_pulito),
    json_parse(JSONString, Parsed).

%%% pulisci_ascii/2
%% Il predicato è di supporto
%% Il predicato è vero quando il 2° argomento è una lista uguale al 1° 
%% argomento, ma con tutti i /t, /n sostituiti da uno spazio 

pulisci_ascii([], []).
pulisci_ascii([10 | T1], [32 | T2]) :- %caso \t
    pulisci_ascii(T1, T2), !. 
pulisci_ascii([9 | T1], [32 | T2]) :- %caso \n
    pulisci_ascii(T1, T2), !.
pulisci_ascii([H | T1], [H | T2]) :- %caso generale
    pulisci_ascii(T1, T2), !.

%%% json_dump/2
%% Il predicato è vero quando il json parsato JSON viene scritto sul file 
%% Filename

json_dump(JSON, Filename) :-
    open(Filename, write, Out),
    scrivi_json(JSON, JSONString),
    write(Out, JSONString),
    close(Out).

%%% scrivi_json/2
%% Il predicato è di supporto a json_dump
%% Il predicato "de-parsa" il json parsato e lo restituisce come stringa

%% Caso oggetto vuoto
scrivi_json(json_obj([]), "{}").

%% Caso generale oggetto
scrivi_json(json_obj(Lista_membri), JSONString) :-
    !,
    scrivi_object(Lista_membri, "", Membri),
    concat("{", Membri, Membri_con_graffa),
    concat(Membri_con_graffa, "}", JSONString).

%% Caso array vuoto
scrivi_json(json_array([]), "[]").

%% Caso generale array
scrivi_json(json_array(Lista_elementi), JSONString) :-
    !,
    scrivi_array(Lista_elementi, "", Elementi),
    concat("[", Elementi, Elementi_con_graffa),
    concat(Elementi_con_graffa, "]", JSONString).


%%% scrivi_object/3
%% Il predicato è di supporto a scrivi_json

scrivi_object([], Prec, Finale) :-
    !,
    string_concat(Finale, ", ", Prec). % tolgo la virgola extra finale

scrivi_object([(K,V)| Tail], Prec, Finale) :-
    scrivi_string(K, Key),
    string_concat(Prec, Key, Prec_Key),
    string_concat(Prec_Key, " : ", Prec_Key_dp),
    scrivi_value(V, Value),
    string_concat(Prec_Key_dp, Value, Prec_KV),
    string_concat(Prec_KV, ", ", Prec_KV_v),
    scrivi_object(Tail, Prec_KV_v, Finale).

%%% scrivi_array/3
%% Il predicato è di supporto a scrivi_json

scrivi_array([], Prec, Finale) :-
    !,
    string_concat(Finale, ", ", Prec). % tolgo la virgola extra finale

scrivi_array([E| Tail], Prec, Finale) :-
    scrivi_value(E, Elemento),
    string_concat(Prec, Elemento, Prec_el),
    string_concat(Prec_el, ", ", Prec_el_v),
    scrivi_array(Tail, Prec_el_v, Finale).

%%% scrivi_value/2
%% Il predicato è di supporto

%% Caso elemento = numero
scrivi_value(E, E) :-
    number(E), !.

%% Caso elemento = stringa
scrivi_value(E, R) :-
    scrivi_string(E, R), !.

%% caso elemento = sottooggetto o sottoarray
scrivi_value(E, R) :-
    scrivi_json(E, R),
    !.

%%% scrivi_string/2
%% Il predicato è di supporto a scrivi_object

scrivi_string(S, R) :-
    string(S),
    !,
    string_concat("\"", S, S_virogoletta),
    string_concat(S_virogoletta, "\"", R).

%%%%  end of file -- json-parsing.pl







