;;;; -*- Mode: Lisp -*-
;;;; json_parse.lisp

;;; json-parse (json)
;; La funzione prende in input una stringa e restituisce una lista dei singoli
;; componenti della stringa in input

; input  ==> "{\"nome\" : \"Arthur\", \"cognome\" : \"Dent\"}"
; output ==> (JSON-OBJ ("nome" "Arthur") ("cognome" "Dent"))
(defun json-parse (json)
   (let ((lista-chars 
          (pulisci-lista (coerce json 'list))))
    (cond
     ((equal (first lista-chars) '#\{) 
      (parse-object (rest lista-chars)))
     ((equal (first lista-chars) '#\[) 
      (parse-array (rest lista-chars)))
     (T (error "errore json-parse"))
    )))

;;; parse-array (json)
;; La funzinone è di supporto
;; La funzione prende in input una lista di char rappresentate un array lo
;; restituisce in forma parsata

; input  ==> (coerce "1, 2, 3]" 'list)
; output ==> (JSON-ARRAY 1 2 3) 
(defun parse-array (json)
(let ((elements (pulisci-lista json)))
  (cond
   ((and 
     (equal (first elements) '#\])
     (null (pulisci-lista (rest elements)))) 
    '(json-array))
   (T (let ((result (parse-elements elements NIL)))
        (if (null (pulisci-lista (first (rest result))))
            (append '(json-array) (first result))
          (error "errore parse-array")))))))
       

;;; parse-object (json)
;; La funzinone è di supporto
;; La funzione prende in input una lista di char rappresentate un object e lo
;; restituisce in forma parsata

; input  ==> (coerce "\"nome\" : \"Arthur\", \"cognome\" : \"Dent\"}" 'list)
; output ==> (JSON-OBJ ("nome" "Arthur") ("cognome" "Dent"))
(defun parse-object (json)
(let ((members (pulisci-lista json)))
  (cond
   ((and 
     (equal (first members) '#\})
     (null (pulisci-lista (rest members)))) 
    '(json-obj))
   (T (let ((m (parse-members members NIL)))
        (if (null (pulisci-lista (first (rest m))))
            (append '(json-obj) (first m))
          (error "errore parse-object")))))))


;;; parse-elements (json obj)
;; La funzione è di supporto
;; La funzione prende in input una lista di caratteri e una lista di supporto
;; e ritorna una lista con dentro 2 liste, la 1° ha dentro tutti gli 
;; elementi dell'array, mentre la 2° è la lista di char restanti dell'input  


; input  ==> (coerce "1, 2, 3], {'a' : 1}" 'list) NIL
; output ==> ((1 2 3) (#\, #\Space #\{ #\' #\a #\' #\Space #\: #\Space #\1 #\}))

(defun parse-elements (input precedente)
  (incolla-array (parse-value input) precedente))

;;; parse-members (json obj)
;; La funzione è di supporto
;; La funzione è analoga a parse-elements

(defun parse-members (input precedente)
  (incolla-object (parse-pair input)  precedente))


;;; parse-pair (json obj)
;; La funzione è di supporto
;; La funzione prende in input una lista di caratteri e restituisce un output
;; analogo a parse-elements, ma con la lista della prima pair come primo 
;; elemento della lista dell'output

; input  ==> (coerce "\"asd\" : 5, \"pippo\" : \"franco\"}" 'list)
; output ==> (("asd" 5) (#\, #\Space #\" #\p #\i #\p #\p #\o #\" #\Space #\: #\Space #\" #\f #\r #\a #\n #\c #\o #\" #\}))

(defun parse-pair (lista)
  (let((lista-pulita (pulisci-lista lista)))
    (if (or (equal (first lista-pulita) '#\") 
            (equal (first lista-pulita) '#\'))
        (let ((result (parse-string lista-pulita)))
          (incolla-pair result))
      (error "errore parse-pair"))))

;;; incolla-pair (json)
;; La funzione è di supporto
;; La funzione "attacca" la value alla string della pair, 
;; come mostrato nell'esempio qua sotto

; input  ==> ("stringa" (#\: #\5 #\, #\Space ... char-vari-restanti))
; output ==> ((stringa 5) (#\, #\Space ... char-vari-restanti))

(defun incolla-pair (input)
    (let (
        (lista-restante (pulisci-lista (first (rest input))))
        (lista-pair (list (first input))))
    (if (equal (first lista-restante) '#\:)
        (let ((result 
               (parse-value 
                (pulisci-lista (rest lista-restante)))))
           (append
            (list (append 
                   lista-pair
                   (list (first result))))
            (list (first (rest result)))))
      (error "errore incolla-pair"))))

;;; parse-value (json)
;; La funzione è di supporto
;; La funzione prende in input una lista di char e restituisce una lista con 
;; al 1° membro la prima value trovata
;; e al 2° membro la lista di char rimanente

; input  ==> (coerce " 543, \"x\" : [1, 2, 3]}" 'list)
; output ==> (543 (#\, #\Space #\" #\x #\" #\Space #\: #\Space #\[ #\1 #\, #\Space #\2 #\, #\Space #\3 #\] #\}))

(defun parse-value (input)
  (let ((input-pulito (pulisci-lista input)))
    (cond
     ((or (equal (first input-pulito) '#\") 
          (equal (first input-pulito) '#\')) 
      (parse-string input-pulito)) ;caso di value = stringa
     ((and (char<= '#\0 (first input-pulito)) 
           (char>= '#\9 (first input-pulito))) 
      (parse-number input-pulito NIL)) ;caso di value = number
     ((or 
       (equal (first input-pulito) '#\{) 
       (equal (first input-pulito) '#\[)) 
      (parse-annidato input-pulito)) ; caso di value = oggetto/array annidato
     (T (error "errore parse-value")))))

;;; parse-number (json buffer)
;; La funzione è di supporto
;; La funzione prende in input una lista di char e una lista buffer di supporto
;; e restituisce in output una lista con al 1° membro il numero e al 2° membro
;; la lista dei char restanti

; input  ==> (coerce "5, \"x\" : [1, 2, 3]}" 'list) NIL
; output ==> (5 (#\, #\Space #\" #\x #\" #\Space #\: #\Space #\[ #\1 #\, #\Space #\2 #\, #\Space #\3 #\] #\}))
(defun parse-number (input buffer)
  (cond
   ((null input) (error "errore parse-number"))
   ((is-digit (first input))  
    (parse-number (rest input) (append buffer (list (first input)))))
   ((char= '#\. (first input)) ; caso numero float
    (parse-number-decimale (rest input) (append buffer (list (first input)))))
   (T (append 
       (list (parse-integer (coerce buffer 'string)))
       (pulisci-lista (list input))))
))

;;; parse-number-decimale (json buffer)
;; La funzione è di supporto a parse-number
;; La funzione serve per i numeri float

; input  ==> (coerce "4, \"x\" : [1, 2, 3]}" 'list) '(#\5 #\6 #\.)
; output ==> (56.4 (#\, #\Space #\" #\x #\" #\Space #\: #\Space #\[ #\1 #\, #\Space #\2 #\, #\Space #\3 #\] #\}))
(defun parse-number-decimale (input buffer)
  (cond
   ((or (null input) (char= '#\. (first input))) 
    (error "errore parse-number-decimale"))
   ((is-digit (first input)) 
    (parse-number-decimale (rest input) (append buffer (list (first input)))))
   (T (append 
       (list (parse-float (coerce buffer 'string))) ; converte il buffer da string a float
       (pulisci-lista (list input))))))

;;; is-digit (char)
;; La funzione è di supporto
;; La funzione ritorna T se l'input è un char di una cifra,
;; altrimenti ritorna NIL 
(defun is-digit (char) 
  (and 
    (char<= '#\0 char)
    (char>= '#\9 char)))

;;; parse-string (input)
;; La funzione è di supporto
;; La funzione prende in input una lista di char e restituisce una lista con
;; al 1° membro la prima stringa e al secondo membro la lista dei char restanti

; input  ==> (coerce "\"asd\" : 5, \"pippo\" : \"franco\"}" 'list)
; output ==> ("asd" (#\Space #\: #\Space #\5 #\, #\Space #\" #\p #\i #\p #\p #\o #\" #\Space #\: #\Space #\" #\f #\r #\a #\n #\c #\o #\" #\}))
(defun parse-string (input)
  (cond 
   ((char= '#\' (first input)) ; caso di "asd"
    (parse-string-apici (rest input) NIL))
   ((char= '#\" (first input)) ; caso di 'asd'
    (parse-string-virgolette (rest input) NIL))))

;;; parse-string-apici (input buffer)
;; La funzione è di supporto
;; La funzione è analoga a parse-string, ma funziona solo sulle stringhe scritte tra apici

; input  ==> (coerce "\"asd\" : 5, \"pippo\" : \"franco\"}" 'list) Nil
; output ==> ("asd" (#\Space #\: #\Space #\5 #\, #\Space #\" #\p #\i #\p #\p #\o #\" #\Space #\: #\Space #\" #\f #\r #\a #\n #\c #\o #\" #\}))
(defun parse-string-apici (input buffer)
  (cond
   ((null input) (error "apici non chiusi"))
   ((equal (first input) '#\") (error "virgolette dentro gli apici"))
   ((not (equal (first input) '#\'))
    (parse-string-apici 
     (rest input) 
     (append buffer (list (first input)))))
   (T (append 
        (list (coerce buffer 'string))
        (pulisci-lista (list (rest input)))))
))

;;; parse-string-virgolette (input buffer)
;; La funzione è di supporto
;; La funzione è analoga a parse-string, ma funziona solo sulle stringhe scritte tra virgolette

; input e output analoghi a parse-string-apici, ma con gli apici al posto delle virgolette

(defun parse-string-virgolette (input buffer)
  (cond
   ((null input) (error "virgolette non chiuse"))
   ((equal (first input) '#\') (error "apici dentro le virgolette"))
   ((not (equal (first input) '#\"))
    (parse-string-virgolette 
     (rest input) 
     (append buffer (list (first input)))))
   (T (append 
        (list (coerce buffer 'string))
        (pulisci-lista (list (rest input)))))
))
   
;;; parse-annidato (input)
;; La funzione è di supporto
;; La funzione prende in input una lista di char e restituisce come output una
;; lista con al 1° membro l'array/object parsato e al 2° membro la lista dei
;; char restanti

; input  ==> (coerce "[1, 2, 3], 7, 5] " 'list)
; output ==> ((JSON-ARRAY 1 2 3) (#\, #\Space #\7 #\, #\Space #\5 #\] #\Space))
(defun parse-annidato (input)
  (cond
   ((equal (first input) '#\{) 
    (let ((result (parse-object-annidato (rest input))))
      result))
   ((equal (first input) '#\[) 
    (let ((result (parse-array-annidato (rest input))))
      result))
    ))

;;; parse-array-annidato (input)
;; La funzione è di supporto
;; La funzione è analoga a parse-array, ma la lista dei char rimanenti non 
;; deve essere per forza vuota

; input  ==> (coerce "[1, 2, 3], 7, 5] " 'list)
; output ==> ((JSON-ARRAY 1 2 3) (#\, #\Space #\7 #\, #\Space #\5 #\] #\Space))

(defun parse-array-annidato (input)
  (let ((input-pulito (pulisci-lista  input)))
   (cond
   ((equal (first input-pulito) '#\]) 
    (append 
     (list '(json-array)) 
     (list (rest input-pulito))))
   (T (let ((result (parse-elements input-pulito NIL)))
        (append 
         (list (append '(json-array) (first result))) 
         (list (first (rest result)))))))))

;;; parse-object-annidato (input)
;; La funzione è di supporto
;; La funzione è analoga a parse-object, ma la lista dei char rimanenti non 
;; deve essere per forza vuota

; input e output analoghi a parse-array-annidato
(defun parse-object-annidato (input)
  (let ((input-pulito (pulisci-lista input)))
   (cond
   ((equal (first input-pulito) '#\}) 
    (append 
     (list '(json-obj)) 
     (list (rest input-pulito))))
   (T (let ((result (parse-members input-pulito NIL))) 
        (append 
         (list (append '(json-obj) (first result))) 
         (list (first (rest result)))))))))

;;; incolla-array (lista parse-precedente)
;; La funzione è di supporto
;; La funzione procede al parsing di una lista di elementi di un array

(defun incolla-array (lista parse-precedente)
  (let ((parse-successivo (append parse-precedente (list (first lista))))
        (lista-pulita (pulisci-lista (first (rest lista)))))
    (cond
     ((char= (first lista-pulita) '#\]) 
      (append (list parse-successivo) (list (rest lista-pulita))))
     ((char= (first lista-pulita) '#\,) 
      (parse-elements (rest lista-pulita) parse-successivo))  
     (T (error "errore incolla-array")))))

;;; incolla-object (lista parse-precedente)
;; La funzione è di supporto
;; La funzione procede al parsing di una lista di elementi di un object

; input  ==> (("asd" 5) (#\, #\Space #\" #\p #\i #\p #\p #\o #\" #\Space #\: #\Space #\" #\f #\r #\a #\n #\c #\o #\" #\})) NIL
; output ==> ((("asd" 5) ("pippo" "franco")) NIL)

(defun incolla-object (lista parse-precedente)
  (let ((parse-successivo (append parse-precedente (list (first lista))))
        (lista-pulita (pulisci-lista (first (rest lista)))))
    (cond
     ((char= (first lista-pulita) '#\}) 
      (append (list parse-successivo) (list (rest lista-pulita))))
     ((char= (first lista-pulita) '#\,) 
      (parse-members (rest lista-pulita) parse-successivo))  
     (T (error "errore incolla-object")))))

;;; pulisci-lista (lista)
;; La funzione è di supporto
;; La funzione prende in input una lista di caratteri e ritorna come output
;; La stessa lista con tutti i caratteri di spaziatura iniziali rimossi

(defun pulisci-lista (lista)
  (if (is-spazio (first lista))
      (pulisci-lista (rest lista))
    lista))

;;; is-spazio (char)
;; La funzione è di supporto
;; La funzione ritorna T se char è uno spazio/Tab/Newline, 
;; altrimenti ritorna NIL

(defun is-spazio (char) 
  (or 
    (equal char '#\Space)
    (equal char '#\Newline)
    (equal char '#\Tab)))

;;; json-access(json, &rest fields)
;; -Follows a chain of keys (iff JSON_obj at current level 
;;  is an object) or indexes (iff JSON_obj at current level 
;;  is an array) in order to retrieve a certain value.
;; -The main idea is to simply go through the list as needed.
;; -Two different predicates are used since the keyword 
;;  &rest had a few issues with recursive calls.


(defun json-access (json fields)
  (cond
    ((null fields) 
      (error "errore json-access (campo vuoto)"))
   ((caso-object-mono-fields json fields)
    (cerca-valore (rest json) (first fields)))
   ((caso-array-mono-fields json fields) 
    (cerca-posizione (rest json) (first fields)))
   ((caso-object-multi-fields json fields)
    (json-access
     (cerca-valore (rest json) (first fields))
     (rest fields)))
   ((caso-array-multi-fields json fields)
    (json-access
     (cerca-posizione (rest json) (first fields))
     (rest fields)))
   (T (error "errore json-access"))))

;; Funzioni per aumentare la comprensibilità del cond di json-access
(defun caso-object-mono-fields (json fields) 
  (and 
    (eq (list-length fields) 1)
    (listp json)
    (stringp (first fields))
    (eq (first JSON) 'json-obj)))

(defun caso-array-mono-fields (json fields) 
 (and 
  (eq (list-length fields) 1) 
  (listp json)
  (numberp (first fields))
  (>= (first fields) 0)
  (eq (first JSON) 'json-array)))

(defun caso-object-multi-fields (json fields)
  (and 
    (> (list-length fields) 1) 
    (listp json)
    (stringp (first fields))
    (eq (first JSON) 'json-obj)))

(defun caso-array-multi-fields (json fields)
  (and 
    (> (list-length fields) 1)
    (listp json)
    (numberp (first fields))
    (>= (first fields) 0)
    (eq (first JSON) 'json-array)))

;;; cerca-valore (input string)
;; La funzione è di supporto
;; La funzione restituisce la value corrispondente a string nella lista input

; input  ==> (("nome" "Arthur") ("cognome" "Dent")) "nome"
; output ==> "Arthur"

(defun cerca-valore (input string)
  (cond
   ((NULL input) (error "errore cerca-valore"))
   ((equal (first (first input)) string) (first (rest (first input))))
   (T (cerca-valore (rest input) string))
   ))

;;; cerca-posizione (input pos)
;; La funzione è di supporto
;; La funzione restituisce il valore presente in input alla posizione pos
; input  ==> '(1 2 3) 2
; output ==> 3
(defun cerca-posizione (input pos)
  (cond
   ((NULL input) (error "errore cerca-posizione"))
   ((eq pos 0) (first input))
   (T (cerca-posizione (rest input) (- pos 1)))
   ))

;;; json-read(filename)
;; La funzione restituisce in forma parsata il json presente in filename 
(defun json-read (filename)
  (with-open-file (stream filename 
                          :direction :input 
                          :if-does-not-exist :error)
    (let ((stringa-allocata (make-string (file-length stream))))
       (let ((contenuto-file (read-sequence stringa-allocata stream)))
         (json-parse (subseq stringa-allocata 0 contenuto-file))))))

;;; json-dump(json filename).
;; La funzione scrive in forma lista sul file Filename il json in input in forma parsata 
(defun json-dump (JSON filename)
  (with-open-file (stream filename 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
  (format stream (scrivi-json JSON))
  filename))

;;; scrivi-json (json)
;; La funzione è di supporto
;; La funzione scrive le parentesi graffe/quadre nel caso che json sia un object/array

(defun scrivi-json (JSON)
  (cond
   ((eq (first JSON) 'json-obj) ;caso oggetto
    (concatenate 'string 
                 "{" 
                 (cancella-virgola-finale
                  (scrivi-object (rest JSON))) 
                 "}"
                 ))
   ((eq (first JSON) 'json-array) ;caso array
    (concatenate 'string 
                 "[" 
                 (cancella-virgola-finale
                  (scrivi-array (rest JSON)))
                 "]"
                 ))
   (T (error "errore scrivi-json"))))

;;; scrivi-object (members)
;; La funzione è di supporto
;; La funzione scrive i membri (lista di pair) di un object

; input  ==> '(("nome" "Arthur") ("cognome" "Dent"))
; output ==> "\"nome\":\"Arthur\",\"cognome\":\"Dent\","

(defun scrivi-object (members)
  (cond
   ((NULL members) "")
   ((listp (first members)) 
    (concatenate 'string 
                 (scrivi-pair (first members)) 
                 (scrivi-object (rest members))))))

;;; scrivi-pair (pair)
;; La funzione è di supporto
;; La funzione scrive la singola pair 

; input  ==> '("nome" "Arthur")
; output ==> "\"nome\" : \"Arthur\", "

(defun scrivi-pair (pair)
  (concatenate 'string "\""
               (first pair)
               "\"" " : " 
               (scrivi-value (first (rest pair)))
               ", "
               ))

;;; scrivi-value (value)
;; La funzione è di supporto
;; La funzione scrive la value nel caso sia un numero, una stringa o un json
;; annidato

(defun scrivi-value (value)
  (cond
   ((numberp value) ; caso numero
    (write-to-string value))
   ((stringp value) ; caso stringa
    (concatenate 'string "\"" value "\""))
   (T (scrivi-json value)))) ; caso annidato

;;; scrivi-array (elements)
;; La funzione è di supporto
;; La funzione scrive gli elementi (lista di value) di un array

; input  ==> '(1 2 3)
; output ==> "1, 2, 3, "

(defun scrivi-array (elements)
  (cond
   ((NULL elements) "")
   (T (concatenate 'string 
      (scrivi-value (first elements))
       ", "
      (scrivi-array (rest elements))
    ))))

;;; cancella-virgola-finale (stringa)
;; La funzione è di supporto
;; La funzione elimina la virgola finale extra (", "), causata dal modo in cui
;; abbiamo gestito le liste da scrivere in modo iterativo  

(defun cancella-virgola-finale (stringa)
  (cond
    ((zerop (length stringa)) stringa)
    (T (subseq stringa 0 (- (length stringa) 2)))))

;;;; end of file -- json-parsing.lisp
