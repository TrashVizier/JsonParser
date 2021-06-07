% -*- Mode: Prolog -*-
% JSON ::= Object | Array
% Object ::= '{}' | '{' Members '}'
% Members ::= Pair | Pair ',' Members
% Pair ::= String ':' Value
% Array ::= '[]' | '[' Elements ']'
% Elements ::= Value | Value ',' Elements
% Value ::= JSON | Number | String
% Number ::= Digit+ | Digit+ '.' Digit+
% Digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
% String ::= '"' AnyCharSansDQ* '"' | '’' AnyCharSansSQ* '’'
% AnyCharSansDQ ::= <qualunque carattere (ASCII) diverso da '"'>
% AnyCharSansSQ ::= <qualunque carattere (ASCII) diverso da '’'>


%% 'asd' -> atomo
%% "asd" -> String

%%%% Sistemare i vari cut

%%% json/1
% Il predicato è vero quando J è un Json

json(J) :-
    atom_string(J, J1),             %%sperimentale
    rimuovi_newline(J1, String),
    json_sup(String).

json_sup(O) :- object(O), !.

json_sup(A) :- array(A), !.


%%% object/1
% Il predicato è vero quando Input è un oggetto

object(Input) :- 
    togligraffe(Input, I_senza_graffe),
    member_breaker(I_senza_graffe, Lista),
    members(Lista).



%%% members/1
% il predicato è vero quando Input è un membro
members([]).
members([H | Tail]) :- 
    pair(H),
    members(Tail).

%%% pair/1
% Il predicato è vero quando Input è una coppia del tipo 'String : Valore' 

pair(Input) :- 
    spezza_pair(Input, S, V),
    trim(S, St),
    trim(V, Vt),
    stringa(St),
    value(Vt).

%%% array/1
array('').
array(Input) :- 
    togliquadre(Input, E),
    member_breaker(E, Lista),
    elements(Lista).

%%% elements/1
elements([]).
elements([H | Tail]) :-
    trim(H, Ht),
    value(Ht),
    elements(Tail).


%%% value/1 
value(I) :- num(I), !.
value(I) :- stringa(I), !.
value(I) :- object(I), !.
value(I) :- array(I), !.


%%% stringa/1
% Il predicato è vero se S è una stringa 
                                                            % N.B. '"313"' è una stringa, "313" no
% stringa(S) :-
%     toglivirgolette(S, X),
%     atom_string(X, Y),
%     string(Y).

stringa(S) :-
    string(S), !.

stringa(S) :-
    atom(S), !.

%%% num/1
% Il predicato è vero se l'argomento è un numero

num(An) :-
    atom_number(An, N), 
    number(N), !.               

num(Input) :-
    toglivirgolette(Input, X),
    num(X), !.

%%% togligraffe/2
togligraffe(I, I_senza_graffe) :- 
    string_concat('{', I_sx, I),
    string_concat(I_senza_graffe, '}', I_sx).





%%% togliquadre/2
togliquadre(I, I_senza_quadre) :- 
    string_concat('[', I_sx, I),
    string_concat(I_senza_quadre, ']', I_sx).

%%% toglivirgolette/2
toglivirgolette(I, I_pulito) :- 
    string_concat('"', I_sx, I),
    string_concat(I_pulito, '"', I_sx).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

json_parse(JSONString, Object) :-
    rimuovi_newline(JSONString, String),
    % atom_string(String, String1),   %%aggiunta sperimentale
    parse_supp(String, Object).

parse_supp(H, json_obj(Obj)) :- 
    object(H), !,
    togligraffe(H, H_senza_graffe),
    member_breaker(H_senza_graffe, Lista_membri),       %%Lista_membri è una lista di atomi
    maplist(spezza_pair, Lista_membri, Lista_s, Lista_v),
    maplist(check_value, Lista_v, Lista_v_aggiornata),
    maplist(incapsula_tonde, Lista_s, Lista_v_aggiornata, Obj).

parse_supp(H, json_array(Obj)) :-
    array(H), !,
    togliquadre(H, H_senza_graffe),
    member_breaker(H_senza_graffe, Lista_elementi),
    maplist(check_value, Lista_elementi, Obj).                   

%% roba inutile

attacca_obj(Lista, json_obj(Lista)).

attacca_array(Lista, json_array(Lista)).

%%%check_value/2
% Il predicato è vero quadno Il secondo argomento è il risultato del parsing del primo argomento

check_value(N, N1) :- 
    num(N),
    !, 
    atom_string(N, N1).

check_value(S, S) :-
    stringa(S),
    !.
    % atom_string(S, S1).

check_value(V, X) :- parse_supp(V, X).


%%% trim/2
% Il predicato è vero quando il secondo argomento è il primo argomento senza gli spazi più esterni 
trim(String, Trim) :-
    atom_chars(String, Chars),
    trim_sx(Chars, Tsx),
    reverse(Tsx, Tsxm),
    trim_sx(Tsxm, Tdxm),
    reverse(Tdxm, Tdx),
    string_chars(Trim, Tdx).

trim_sx([' ' | Tail], T) :-
    trim_sx(Tail, T).

trim_sx([H | Tail], T) :-
    not(H = ' '),
    append([H], Tail, T).


%%% spezza_pair/3
% Il predicato è vero quando il primo argomento è la coppia formata dal secondo e dal terzo argomento con un ':' nel mezzo (es. "a" : 3)

spezza_pair(Coppia, Ss, Vt) :- 
    atom_chars(Coppia, Chars),
    spezza_pair_sup(Chars, V_chars),
    append(S_chars_dp, V_chars, Chars),
    atom_chars(V, V_chars),
    atom_chars(S_dp, S_chars_dp),
    atom_concat(S, ':', S_dp),
    trim(S, St),
    trim(V, Vt),
    toglivirgolette(St, Ss).    %%sperimentale  %% funge per ora
    % toglivirgolette(Vt, Vs).    %%sperimentale

spezza_pair_sup([':' | Tail], Tail) :-!.

spezza_pair_sup([H | Tail], Restante):-
    not(H = ':'),
    spezza_pair_sup(Tail, Restante).

%%% rimuovi_newline/2
% Il predicato è vero quando il secondo argomento è il primo argomento privato di tutti gli \n
rimuovi_newline(String, Riga) :-
    atomic_list_concat(Lista, '\n', String),
    concatena_lista(Lista, "", Riga).

%%% concatena_lista/3
concatena_lista([], X, X).

concatena_lista([H | Tail], Precedente, String) :-
    string_concat(Precedente, H, Successiva),  %%modificato atom_concat in string_concat
    concatena_lista(Tail, Successiva, String).

%%% incapsula_tonde/3
% Il predicato è vero quando il terzo argomento è una parentesi contentente i primi 2 argomenti separati da una virgola
incapsula_tonde(S, V, (S, V)).

% incapsula_tonde(S, V, Capsula) :-
%     atom_concat('(', S, C1),
%     atom_concat(C1, ', ', C2),
%     atom_concat(C2, V, C3),
%     atom_concat(C3, ')', Capsula).


%%supporto a caso WIP

supporto_value(V, X) :-
    atom_string(V1, V),
    stringa(V1), !, 
    toglivirgolette(Vt, Vs).


%%% Funziona, ma mi da troppe opzioni e va ottimizzato
%%% member_breaker/2

member_breaker(String, Lista) :-
    atom_chars(String, Chars),
    spezza_members(Chars, "", [], Lista).

%%% spezza_members/4
spezza_members([], Buffer, Precedente, Finale) :-
    trim(Buffer, B),
    append(Precedente, [B], Finale), !.

%% caso della virgola
spezza_members([',' | Tail], Buffer, Precedente, Finale) :-
    trim(Buffer, B),
    append(Precedente, [B], Successiva),
    spezza_members(Tail, "", Successiva, Finale).

%% caso della graffa aperta
spezza_members(['{' | Tail], Buffer, Precedente, Finale) :-
    atom_concat(Buffer, '{', Buffer_con_graffa),
    spezza_alla_graffa(Tail, Sottoggetto, Coda),
    atom_concat(Buffer_con_graffa, Sottoggetto, Buffer_con_so),
    spezza_members(Coda, Buffer_con_so, Precedente, Finale).

%% caso della quadra aperta
spezza_members(['[' | Tail], Buffer, Precedente, Finale) :-
    atom_concat(Buffer, '[', Buffer_con_quadra),
    spezza_alla_quadra(Tail, Sottoggetto, Coda),
    atom_concat(Buffer_con_quadra, Sottoggetto, Buffer_con_array),
    spezza_members(Coda, Buffer_con_array, Precedente, Finale).

%% caso generale
spezza_members([H | Tail], Buffer, Precedente, Finale) :-
    atom_concat(Buffer, H, Buffer_aggiornato), 
    spezza_members(Tail, Buffer_aggiornato, Precedente, Finale). 


%%% spezza_alla_graffa/3
% il predicato è vero quando Sottoggetto è il sottooggetto estratto dalla lista di chars Chars e Chars_Coda sono i chars riamnenti
%% N.B. Chars ha la prima graffa rimossa
spezza_alla_graffa(Chars, Sottoggetto, Chars_coda) :-
    spezza_sottoggetto(Chars, 1, Chars_coda),
    append(Chars_SO, Chars_coda, Chars),
    atom_chars(Sottoggetto, Chars_SO).


%% spezza_sottoggetto/3
spezza_sottoggetto(Tail, 0, Tail):- !.

spezza_sottoggetto(['{' | Tail], Contatore, Lista_Tail) :-
    Count is Contatore + 1,
    spezza_sottoggetto(Tail, Count, Lista_Tail).
    
spezza_sottoggetto(['}' | Tail], Contatore, Lista_Tail) :-
    Count is Contatore - 1,
    spezza_sottoggetto(Tail, Count, Lista_Tail).

spezza_sottoggetto([H | Tail], Contatore,  Lista_Tail) :-
    not(H = '{'),
    not(H = '}'),
    spezza_sottoggetto(Tail, Contatore, Lista_Tail).

%%% spezza_alla_quadra/3
% il predicato è vero quando Sottoggetto è il sottoarray estratto dalla lista di chars Chars e Chars_Coda sono i chars riamnenti
%% N.B. Chars ha la prima quadra rimossa
spezza_alla_quadra(Chars, Sottoggetto, Chars_coda) :-
    spezza_sottoarray(Chars, 1, Chars_coda),
    append(Chars_SO, Chars_coda, Chars),
    atom_chars(Sottoggetto, Chars_SO).

%%%spezza_sottoarray/3

spezza_sottoarray(Tail, 0, Tail):- !.

spezza_sottoarray(['[' | Tail], Contatore, Lista_Tail) :-
    Count is Contatore + 1,
    spezza_sottoarray(Tail, Count, Lista_Tail).
    
spezza_sottoarray([']' | Tail], Contatore, Lista_Tail) :-
    Count is Contatore - 1,
    spezza_sottoarray(Tail, Count, Lista_Tail).
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> parent of 03a1712 (Update pastrugno.pl)
=======
>>>>>>> parent of 03a1712 (Update pastrugno.pl)

spezza_sottoarray([H | Tail], Contatore,  Lista_Tail) :-
    not(H = '['),
    not(H = ']'),
    spezza_sottoarray(Tail, Contatore, Lista_Tail).


<<<<<<< HEAD
<<<<<<< HEAD
%%%%%   Funge
scrivi_su_file(Filename, Atom) :-
    open(Filename, write, In),
    write(In, Atom),
    close(In).

%%%% Funge
leggi_da_file(Filename, Riga):-
    open(Filename, read, Str),
    read_file(Str, Lines),
    concatena_lista(Lines, "", Riga),
    close(Str).
=======

spezza_sottoarray([H | Tail], Contatore,  Lista_Tail) :-
    not(H = '['),
    not(H = ']'),
    spezza_sottoarray(Tail, Contatore, Lista_Tail).
>>>>>>> parent of 03a1712 (Update pastrugno.pl)

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    read_line_to_string(Stream,X),
    read_file(Stream,L).
=======
>>>>>>> parent of 03a1712 (Update pastrugno.pl)
=======
>>>>>>> parent of 03a1712 (Update pastrugno.pl)

%%%%%COSE UTILI
% {"type": "menu", "value": "File", "items": [{"value": "New", "action": "CreateNewDoc"}, {"value": "Open", "action": "OpenDoc"}, {"value": "Close", "action": "CloseDoc"}]}

% atomic_list_concat([gnu, gnat], ', ', A).     =====> A = 'gnu, gnat'
% atom_concat(1, 2, X)                          =====> X = '12'
% atom_codes(pippo, X)                          =====> X = [112, 105, 112, 112, 111]
% split_string("a.b.c.d", ".", "", L)           =====> L = ["a", "b", "c", "d"].

%% '{"a" : 1, "x" : {"b" : "e"}}'

%%['"', a, '"', ':', 5, ',', '"', p, '"', ':', '{', '"', q, '"', ':', '"', z, '"', ',', '"', e, '"', ':', 1, '}']
%% '"a" : 5, "p" : { "q" : "z", "e" : 1 }'
