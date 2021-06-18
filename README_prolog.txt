Membri del gruppo

    822433 Giorgio Gragnano
    829602 Andrea Lamparella
    856115 Pietro Venturini

Descrizione

    Costruzione di strutture dati per la rappresentazione, interrogazione e lettura/scrittura 
    da file di oggetti in formato JSON.

Linguaggio utilizzato

    Abbiamo utilizzato per questa parte di progetto il linguaggio Prolog.

Grammatica utilizzata

    Abbiamo considerato una versione semplificata della sintassi delle stringhe JSON, 
    descritta da questa grammatica:

        JSON ::= Object | Array
        Object ::= '{}' | '{' Members '}'
        Members ::= Pair | Pair ',' Members
        Pair ::= String ':' Value
        Array ::= '[]' | '[' Elements ']'
        Elements ::= Value | Value ',' Elements
        Value ::= JSON | Number | String
        Number ::= Digit+ | Digit+ '.' Digit+
        Digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
        String ::= '"' AnyCharSansDQ* '"' | '’' AnyCharSansSQ* '’'
        AnyCharSansDQ ::= <qualunque carattere (ASCII) diverso da '"'>
        AnyCharSansSQ ::= <qualunque carattere (ASCII) diverso da '’'>

N.B. Con questa grammatica, fra le altre varie limitazioni, non sono contemplati i valori
 booleani e i numeri negativi.

Predicati principali

    I principali predicati con cui è possibile interagire in questo programma sono le seguenti.

        json_parse(JSONString, Object)

            Effettua il parsing di JSONString, inserito come atomo Prolog, restituendo un 
            output in forma di json_obj([ ... ]) se JSONString è un oggetto o in forma 
            json_array([ ... ]) se JSONString è un array.

        json_access(JSON, Fields, Result)

            Restituisce la value Result recuperandola dal JSON, seguendo ricorsivamente
            la lista di campi Fields, composta da stringhe e/o numeri.

        json_read(Filename, JSON)

            Chiama json_parse/2 con il contenuto del file Filename restituendo in output JSON .

        json_dump(JSON, Filename)

            Scrive nel file Filename, sottoforma di stringa, l'atomo prolog JSON.