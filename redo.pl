%%%% SONO ARRIVATO A JSON_ARRAY

%%%% -*- Mode: Prolog -*-
%%%%  json-parsing.pl

%%% json_parse(JSONString, Object)
%% -Parses a given JSONString by following its recursive nature
%%  (e.g. split an object into members, members into pairs and so on) in
%%  order to produce a Prolog friendly list-like form (from now
%%  on 'JSON_Object').
%% -The main idea is to consume the characters one-by-one, starting from
%%  the left side until either an error is found or the string is
%%  correctly parsed. In order to do so, almost all of the predicates
%%  have this form: predicate_name(InputCharlist, OutputCharlist,
%%  InputObject, OutputObject). This is done in order to improve the
%%  re-usability of the code.

%% If JSONString is an object...
json_parse(JSONString, Object) :-
    string_chars(JSONString, JSONChars),
    remove_whitespaces_newlines_tabs(JSONChars, JSONChars1),        %% trimma a sx
    json_object(JSONChars1, JSONChars2, [], Object),
    remove_whitespaces_newlines_tabs(JSONChars2, JSONChars3),
    %% Checks if empty to make sure there's nothing nasty at the end
    is_empty(JSONChars3),
    !.
%% If JSONString is an array...
json_parse(JSONString, Object) :-
    string_codes(JSONString, JSONChars),
    remove_whitespaces_newlines_tabs(JSONChars, JSONChars1),
    json_array(JSONChars1, JSONChars2, [], Object),
    remove_whitespaces_newlines_tabs(JSONChars2, JSONChars3),
    %% Checks if empty to make sure there's nothing nasty at the end
    is_empty(JSONChars3),
    !.

%%% json_object(JSONSChars, JSONChars2, ObjectIn,json_obj(OutObject))
%% If there's an empty object {}...
json_object(JSONCharsIn, JSONCharsOut, ObjectIn, json_obj(ObjectOut)) :-    %% caso oggetto vuoto {}
    first_char("{", JSONCharsIn, JSONChars1),
    remove_whitespaces_newlines_tabs(JSONChars1, JSONChars2),
    first_char("}", JSONChars2, JSONCharsOut),
    !,
    ObjectIn = ObjectOut.

json_object(JSONCharsIn, JSONCharsOut, ObjectIn, json_obj(ObjectOut)) :-    %% caso di un oggetto "pieno"
    first_char("{", JSONCharsIn, JSONChars1),                                       %% tolgo la prima graffa
    !,
    remove_whitespaces_newlines_tabs(JSONChars1, JSONChars2),                       %% trim eventuale
    json_members(JSONChars2, JSONChars3, ObjectIn, ObjectOut),                      %% chiamo sui membri
    remove_whitespaces_newlines_tabs(JSONChars3, JSONChars4),
    first_char("}", JSONChars4, JSONCharsOut).

%%%  json_array(JSONSChars, JSONChars2, ObjectIn,json_array(ObjectOut))
%% If there's an empty array []...
json_array(JSONCharsIn, JSONCharsOut, ObjectIn, json_array(ObjectIn)) :-
    first_char("[", JSONCharsIn, JSONChars1),
    remove_whitespaces_newlines_tabs(JSONChars1, JSONChars2),
    first_char("]", JSONChars2, JSONCharsOut),
    !.
json_array(JSONCharsIn, JSONCharsOut, ObjectIn, json_array(ObjectOut)) :-
    first_char("[", JSONCharsIn, JSONChars1),
    !,
    remove_whitespaces_newlines_tabs(JSONChars1, JSONChars2),
    json_elements(JSONChars2, JSONChars3, ObjectIn, ObjectOut),
    remove_whitespaces_newlines_tabs(JSONChars3, JSONChars4),
    first_char("]", JSONChars4, JSONCharsOut).

%%%  json_member(JSONSChars, JSONCharsOut, ObjectIn, ObjectOut)
%% It is essential to write both json_members and json_elements in this
%% order, since using the opposite one would result in an error (they
%% would both unify, which is wrong!). The same idea is used in the rest
%% of this program with many predicates.
json_members(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut1) :-
    json_pair(JSONCharsIn, JSONChars2, ObjectIn, ObjectOut),
    remove_whitespaces_newlines_tabs(JSONChars2, JSONChars3),
    first_char(",", JSONChars3, JSONChars4),
    remove_whitespaces_newlines_tabs(JSONChars4, JSONChars5),
    json_members(JSONChars5, JSONCharsOut, ObjectOut, ObjectOut1),
    !.
json_members(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut) :-
    json_pair(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut),
    !.
%%% json_elements(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut)
json_elements(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut2) :-
    json_value(JSONCharsIn, JSONChars2, ObjectOut),
    remove_whitespaces_newlines_tabs(JSONChars2, JSONChars3),
    first_char(",", JSONChars3, JSONChars4),
    remove_whitespaces_newlines_tabs(JSONChars4, JSONChars5),
    !,
    append(ObjectIn, [ObjectOut], ObjectOut1),
    json_elements(JSONChars5, JSONCharsOut, ObjectOut1, ObjectOut2).

json_elements(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut1) :-
    json_value(JSONCharsIn, JSONCharsOut, ObjectOut),
    append(ObjectIn, [ObjectOut], ObjectOut1),
    !.

%%% json_pair(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut)
json_pair(JSONCharsIn, JSONCharsOut, ObjectIn, ObjectOut) :-
    json_string(JSONCharsIn, JSONChars2, Key),
    remove_whitespaces_newlines_tabs(JSONChars2, JSONChars3),
    first_char(":", JSONChars3, JSONChars4),
    remove_whitespaces_newlines_tabs(JSONChars4, JSONChars5),
    json_value(JSONChars5, JSONCharsOut, Value),
    append(ObjectIn, [(Key,Value)], ObjectOut).

%%% json_string(JSONCharsIn, JSONCharsOut, Key)
json_string(JSONCharsIn, JSONCharsOut, Key) :-
    first_char("\'", JSONCharsIn, JSONChars2),      %'
    !,
    string_creation_sq(JSONChars2, JSONChars3, Result),  %% Result is the Key of the pair without squotes, and JSONChars3 is the rest (from : on)
    first_char("\'", JSONChars3, JSONCharsOut),     %'
    string_chars(Key, Result).
json_string(JSONCharsIn, JSONCharsOut, Key) :-
    first_char("\"", JSONCharsIn, JSONChars2),
    !,
    string_creation_dq(JSONChars2, JSONChars3, Result),
    first_char("\"", JSONChars3, JSONCharsOut),
    string_chars(Key, Result).

%%% json_value(JSONCharsIn, JSONCharsOut, Object)
json_value(JSONCharsIn, JSONCharsOut, Object) :-
    json_string(JSONCharsIn, JSONCharsOut, Object), % is a string
    !.
json_value(JSONCharsIn, JSONCharsOut, Object) :-
    json_nested(JSONCharsIn, JSONCharsOut, Object), % is a JSON
    !.
json_value(JSONCharsIn, JSONCharsOut, Object) :-
    json_number(JSONCharsIn, JSONCharsOut, Object), % is a number
    !.

%%% json_number(JSONCharsIn, JSONCharsOut, Object)
%% If number is float...
json_number(JSONCharsIn, JSONCharsOut, Object) :-
    number_creation(JSONCharsIn, Coda, Int),
    is_not_empty(Int),
    first_char(".", Coda, Coda_senza_punto),
    !,
    append(Int, ['.'], Int_con_punto),
    number_creation(Coda_senza_punto, JSONCharsOut, Decimale),
    is_not_empty(Decimale),
    append(Int_con_punto, Decimale, Float),
    number_chars(Object, Float).

%% if number is int   
json_number(JSONCharsIn, JSONCharsOut, Object) :-
    number_creation(JSONCharsIn, JSONCharsOut, Num),
    is_not_empty(Num),
    !,
    number_chars(Object, Num).


%%% is_not_empty(List)
is_not_empty(List) :- List \= [], !.

%%% is_empty(List)
is_empty([]) :- !.

%%% json_nested(JSONCharsIn, JSONCharsOut, Object)
json_nested(JSONCharsIn, JSONCharsOut, Object) :-
    json_object(JSONCharsIn, JSONCharsOut, [], Object),
    !.
json_nested(JSONCharsIn, JSONCharsOut, Object) :-
    json_array(JSONCharsIn, JSONCharsOut, [], Object),
    !.

%%% first_char(CharToMatch, JSONCharsIn, JSONCharsOut)                      %%% rimuove il primo carattere dalla lista solo se CharToMatch è il primo della lista
%% Please note: the following predicates also REMOVES the object
%% from the actual char list!
first_char(String_chars, [Char | Xs], Xs):-
    atom_string(Char, String_chars).

%%% number_creation(JSONCharsIn, JSONCharsOut, JSONCharsNumber)
number_creation([X | Xs], [X | Xs], []) :-
    not(is_digit(X)),
    !.
number_creation([X | Xs], Zs, [X | Ys]) :-
    number_creation(Xs, Zs, Ys).

is_digit(0).
is_digit(1).
is_digit(2).
is_digit(3).
is_digit(4).
is_digit(5).
is_digit(6).
is_digit(7).
is_digit(8).
is_digit(9).

%%% string_creation_sq(JSONCharsIn, JSONCharsOut, JSONCharsString)      %% "asd' : 123"=> "asd" e "' : 123"
string_creation_sq(['\"' | _], _, _) :-
    !,
    fail.
string_creation_sq(['\'' | Xs], ['\'' | Xs], []) :- !.
string_creation_sq([X | Xs], Zs, [X | Ys]) :-
    string_creation_sq(Xs, Zs, Ys).

%%% string_creation_dq(JSONCharsIn, JSONCharsOut, JSONCharsString)      %% "asd\" : 123"=> "asd" e "\" : 123"
string_creation_dq(['\'' | _], _, _) :-
    !,
    fail.
string_creation_dq(['\"' | Xs], ['\"' | Xs], []) :- !.
string_creation_dq([X | Xs], Zs, [X | Ys]) :-
    string_creation_dq(Xs, Zs, Ys).


%%% remove_whitespaces_newlines_tabs(JSONCharsIn, JSONCharsOut)
remove_whitespaces_newlines_tabs([],[]) :- !.
remove_whitespaces_newlines_tabs([X | Xs], Ys) :-
    is_spazio(X),
    !,
    remove_whitespaces_newlines_tabs(Xs, Ys).
remove_whitespaces_newlines_tabs([X | Xs], [X | Xs]) :- !.

%%% is_spazi(JSONChars)
is_spazio(' ') :- !.
is_spazio('\n') :- !.
is_spazio('\t') :- !.

%%% json_get(JSON_obj, Fields, Result)
%% -Follows a chain of keys (iff JSON_obj at current level is an object)
%%  or indexes (iff JSON_obj at current level is an array) in order to
%%  retrieve a certain value.
%% -The main idea is to retrieve the list inside the JSON_obj
%%  and recursively work on that list.

%% Returns an identity if fields is empty
json_get(X, void, X) :- !.

%% Fails if Elements is empty
json_get(_, [], _) :- !, fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%ARRIVATO QUA


%% Cannot find anything in an empty object-array!
json_get(json_obj(), _, _) :- !, fail.
json_get(json_array(), _, _) :- !, fail.

json_get(JSON_obj, [X], Result) :-
    json_get_elements(JSON_obj, X, Result),
    !.
json_get(JSON_obj, [X|Xs], Result) :-
    json_get_elements(JSON_obj, X, Temp),
    !,
    json_get(Temp, Xs, Result).
json_get(JSON_obj, X, Result) :-
    json_get_elements(JSON_obj, X, Result),
    !.

%%% json_get_elements(JSON_obj, Fields, Result)
% If Object is an object...
json_get_elements(JSON_obj, Fields, Result) :-
    json_obj([Y|Ys]) = JSON_obj,
    !,
    json_get_member([Y|Ys], Fields, Result).

%% If Object is an array...
json_get_elements(JSON_obj, Index , Result) :-
    json_array([X|Xs]) = JSON_obj,
    !,
    json_get_member_position([X | Xs], Index, Result).

%%% json_get_member(JSON_list, Key, Result)
%% Searches an element given a Key (Object only!)
json_get_member([], _, _) :- fail.
json_get_member([(X,Y)| _], Z, Result) :-
    string(Z),
    X = Z,
    !,
    Result = Y.
json_get_member([_| Xs], Z, Result) :-
    string(Z),
    json_get_member(Xs, Z, Result).

%%% json_get_member_position(JSON_list, Position, Result)
% Searches an element given an index (Array only!)
json_get_member_position([],[_], _) :- fail.
json_get_member_position([X | _], Y, Result) :-
    number(Y),
    Y = 0,
    !,
    Result = X.
json_get_member_position([_ | Xs], Y, Result) :-
    number(Y),
    Z is Y-1,
    json_get_member_position(Xs, Z, Result).


%%% json_read(FileName, JSON).
%% -Loads a json file and returns its equivalent JSON_Object form
%% -Quite self explanatory...
json_read(Filename, JSON) :-
    open(Filename, read, In),
    read_stream_to_codes(In, X),
    close(In),
    atom_codes(JSONString, X),
    json_parse(JSONString, JSON).


%%% json_write(JSON, Filename).
%% -Writes a JSON_Object into a .json file (in JSON-compatible syntax!)
%% -The main idea is to retrieve the list inside the JSON_obj and
%%  recursively work on that list.
%%  (Same as json_get, the only difference being that it returns a string
%%  representing the list instead of a single element)
json_write(JSON, Filename) :-
    open(Filename, write, Out),
    json_print(JSON, JSONString),
    write(Out, JSONString),
    close(Out).

%%% json_print(JSON, JSONString)
json_print(JSON, JSONString) :-
    JSON = json_obj([]),
    !,
    JSONString = "{}".
json_print(JSON, JSONString) :-
    json_obj([Y | Ys]) = JSON,
    !,
    concat("", "{", JSONString1),
    json_print_object([Y | Ys], "", JSONString2),
    concat(JSONString1, JSONString2, JSONString3),
    concat(JSONString3, "}", JSONString).

json_print(JSON, JSONString) :-
    JSON = json_array([]),
    !,
    JSONString = "[]".
json_print(JSON, JSONString) :-
    json_array([Y | Ys]) = JSON,
    !,
    concat("", "[", JSONString1),
    json_print_array([Y | Ys], "", JSONString2),
    concat(JSONString1, JSONString2, JSONString3),
    concat(JSONString3, "]", JSONString).

%%% json_print_object(JSONList, JSONString, Result)
json_print_object([], JSONString, Result) :-
    !,
    string_concat(Temp, ",", JSONString),
    Result = Temp.
json_print_object([(X,Y)| Xs], JSONString, Result) :-
    json_print_element(X, JSONString1),
    string_concat(JSONString, JSONString1, JSONString2),
    string_concat(JSONString2, ":", JSONString3),
    json_print_element(Y, JSONString4),
    string_concat(JSONString3, JSONString4, JSONString5),
    string_concat(JSONString5, ",", JSONString6),
    json_print_object(Xs, JSONString6, Result).

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
json_print_element(X, Result) :-
    number(X),
    !,
    Result = X.
json_print_element(X, Result) :-
    json_print(X, Result),
    !.
json_print_element(X, Result) :-
    string(X),
    !,
    string_concat("", "\"", JSONString1),
    string_concat(JSONString1, X, JSONString2),
    string_concat(JSONString2, "\"", JSONString3),
    Result = JSONString3.

%%%%  end of file -- json-parsing.pl







