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

%%%% SISTEMARE I CONCAT


json(O) :- object(O).

json(A) :- array(A).


%%% object/1
% Il predicato è vero quando Input è un oggetto

object(Input) :- 
    togligraffe(Input, I_senza_graffe),
    atomic_list_concat(Lista, ', ', I_senza_graffe),
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
    stringa(S),                                                              %% ERRORE
    value(V).

%%% array/1
array('').
array(Input) :- 
    togliquadre(Input, E),
    atomic_list_concat(Lista, ', ', E),
    elements(Lista).

%%% elements/1
elements([]).
elements([H | Tail]) :- 
    value(H),
    elements(Tail).


%%% value/1 
value(I) :- json(I).
value(I) :- num(I).
value(I) :- stringa(I).


stringa(S) :-
    toglivirgolette(S, X),
    atom_string(X, Y),
    string(Y).

%%% num/1
% Il predicato è vero se Input è un numero
% num(Input) :- 
%     atomic_list_concat([Int_char, Float_char], '.', Input),
%     atom_number(Int_char, Int),
%     atom_number(Float_char, Float),
%     integer(Int),
%     integer(Float).

% num(Input) :- integer(Input).


num(An) :-                           %% se mi arriva un numero atomico (es. '42')
    atom_number(An, N), 
    number(N).               

num(Input) :-                       %% se mi arriva una stringa con un numero (es "42")
    toglivirgolette(Input, X),
    num(X).

% %%% string/1
% stringa(Input) :-
%     toglivirgolette(Input, C),
%     atom_codes(C, Lista_ascii),
%     carattere(Lista_ascii).

% %%% carattere/1
% carattere([]).
% carattere([H | Tail]) :-
%     not(H is 34),
%     carattere(Tail).

%%% cifra/1
cifra(0).
cifra(1).
cifra(2).
cifra(3).
cifra(4).
cifra(5).
cifra(6).
cifra(7).
cifra(8).
cifra(9).

%%% togligraffe/2
togligraffe(I, I_senza_graffe) :- 
    atom_concat('{', I_sx, I),
    atom_concat(I_senza_graffe, '}', I_sx).
    
%%% togliquadre/2
togliquadre(I, I_senza_quadre) :- 
    atom_concat('[', I_sx, I),
    atom_concat(I_senza_quadre, ']', I_sx).

%%% toglivirgolette/2
toglivirgolette(I, I_pulito) :- 
    atom_concat('"', I_sx, I),
    atom_concat(I_pulito, '"', I_sx).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

json_parse(JSONString, Object) :-
    rimuovi_newline(JSONString, String),
    parse_supp([String], [], Object).

parse_supp([], _, _) :- !.

parse_supp([H | Tail], Precedente, Obj) :- 
  %%  atom_string(Head, H),                                   %% lo converto a stringa
    object(H), !,                                             %% SE è UN OGGETTO
    togligraffe(H, H_senza_graffe),                         %% tolgo le graffe
    atomic_list_concat(Lista_membri, ', ', H_senza_graffe), %% lo spezzo nella lista [P, P, P]
    parse_supp_pair(Lista_membri, Precedente, Obj),              %% chiamo sul primo membro
    parse_supp(Tail, Precedente, Obj).                      %% chiamo sulla coda

parse_supp([H | Tail], Precedente, Obj) :-  
   %% atom_string(Head, H),                                       %% lo converto a stringa
    array(H), !,                                                 %% SE è UN ARRAY
    togliquadre(H, H_senza_graffe),                             %% tolgo le quadre
    atomic_list_concat(Lista_elementi, ', ', H_senza_graffe),   %% spezzo nella lista [E, E, E]
    parse_supp(Lista_elementi, Precedente, Obj),                %% chiamo sulla testa
    parse_supp(Tail, Precedente, Obj).                          %% chiamo sulla coda

%%%%%%%%%%%%%%%%%% WIP

parse_supp_pair([H | Tail], Precedente, Obj) :- 
%%  atom_string(Head, H),                           %% lo converto a stringa
    pair(H), !,                                       %% SE è un PAIR       %% PROBABILMENTE INUTILE
    spezza_pair(H, S, V),                           %% lo spezzo nella lista [S, V]  
    stringa(S),                                      %% se S è una stringa  %% PROBABILMENTE è INUTILE
    check_value(V, V_trattata),                      %% chiamo su V
    incapsula_tonde(S, V_trattata, Coppia),
    append(Precedente, Coppia, Successiva),
    parse_supp_pair(Tail, Successiva, Obj).              %% chiamo sulla coda

parse_supp_pair([], X, X).

% parse_supp([H | Tail], Precedente, Obj) :-
%     num(H),     %%%WIP

% parse_supp([Head | Tail], Precedente, Obj) :-
%     atom_string(Head, H),                           %% converto a stringa               %%% DUBBIO
%     string(H),                                      %% se è un STRING
%     append(Precedente, H, Obj).



%%% Supporto a caso

check_value(S, S) :- stringa(S).

check_value(N, N) :- num(N).

check_value(V, X) :- 
    json(V),
    parse_supp([V], [], X).





%%FUNGE

spezza_pair(Coppia, S, V) :- atomic_list_concat([S, V], ' : ', Coppia), !.
spezza_pair(Coppia, S, V) :- atomic_list_concat([S, V], ': ', Coppia), !.

%%FUNGE

rimuovi_newline(String, Riga) :-
    atomic_list_concat(Lista, '\n', String),
    concatena_lista(Lista, "", Riga).

%%FUNGE

concatena_lista([], X, X).

concatena_lista([H | Tail], Precedente, String) :-
    atom_concat(Precedente, H, Successiva),
    concatena_lista(Tail, Successiva, String).

%%FUNGE
incapsula_tonde(S, V, Capsula) :-
    atom_concat('(', S, C1),
    atom_concat(C1, ', ', C2),
    atom_concat(C2, V, C3),
    atom_concat(C3, ')', Capsula).

%%%%%COSE UTILI
% {"type": "menu", "value": "File", "items": [{"value": "New", "action": "CreateNewDoc"}, {"value": "Open", "action": "OpenDoc"}, {"value": "Close", "action": "CloseDoc"}]}

% atomic_list_concat([gnu, gnat], ', ', A).     =====> A = 'gnu, gnat'
% atom_concat(1, 2, X)                          =====> X = '12'
% atom_codes(pippo, X)                          =====> X = [112, 105, 112, 112, 111]
% split_string("a.b.c.d", ".", "", L)           =====> L = ["a", "b", "c", "d"].