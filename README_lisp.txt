Membri del gruppo

    822433 Giorgio Gragnano
    829602 Andrea Lamparella
    856115 Pietro Venturini

Descrizione

    Costruzione di strutture dati per la rappresentazione, interrogazione e lettura/scrittura 
    da file di oggetti in formato JSON.

Linguaggio utilizzato

    Abbiamo utilizzato per questa parte di progetto il linguaggio Lisp.

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

Funzioni principali

    Le principali funzioni con cui è possibile interagire in questo programma sono le seguenti.

        (json-parse (json))

            Effettua il parsing di json restituendo una lista contentente la stringa
            json scomposta.

        (json-access (json &rest fields)) 

            Restituisce la value recuperandola dal json, seguendo ricorsivamente
            i termini di fields, cioè stringhe o numeri.

        (json-read (filename))

            Chiama la funzione json-parse/1 con il contenuto di filename.

        (json-dump (json filename))

            Scrive json nel file filename.