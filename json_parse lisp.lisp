;;;; -*- Mode: Lisp -*-
;;;; json-parsing.lisp

;;; json-parse (json)
;; -Parses a given json by following its recursive nature
;;  (e.g. split an object into members, members into pairs
;;  and so on) in order to produce a Lisp friendly list-form.
;; -The main idea is to consume the characters one-by-one,
;;  starting from the left side until either an error is found 
;;  or the string is correctly parsed. 
;;  In order to do so, data is passed through all the functions 
;;  in the following form: (parse-result (remaining-chars)).

;input  ==> "{\"nome\" : \"Arthur\", \"cognome\" : \"Dent\"}"
;output ==> (JSON-OBJ ("nome" "Arthur") ("cognome" "Dent"))
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
; input  ==> (coerce "\"nome\" : \"Arthur\", \"cognome\" : \"Dent\"}" 'list)
; output ==> (JSON-OBJ ("nome" "Arthur") ("cognome" "Dent"))
(defun parse-object (json)
(let ((members (pulisci-lista json)))
  (cond
   ((and 
     (equal (first members) '#\})
     (null (pulisci-lista (rest members)))) 
    '(json-obj))
   (T (let ((result (parse-members members NIL)))
        (if (null (pulisci-lista (first (rest result))))
            (append '(json-obj) (first result))
          (error "errore parse-object")))))))


;;; parse-elements (json obj)
;; The idea here is to parse the first element/member, and then
;; look for a delimiter (either a comma or a closed parenthesis)

; input  ==> (coerce "1, 2, 3]" 'list) NIL
; output ==> ((1 2 3) NIL)

(defun parse-elements (input precedente)
  (fine-array (parse-value input) precedente))

;;; parse-members (json obj)
; input  ==> (coerce "\"asd\" : 5, \"pippo\" : \"franco\"}" 'list) NIL
; output ==> ((("asd" 5) ("pippo" "franco")) NIL)
(defun parse-members (input precedente)
  (fine-object (parse-pair input)  precedente))


;;; parse-pair (json obj)
; input  ==> (coerce "\"asd\" : 5, \"pippo\" : \"franco\"}" 'list)
; output ==> (("asd" 5) (#\, #\Space #\" #\p #\i #\p #\p #\o #\" #\Space #\: #\Space #\" #\f #\r #\a #\n #\c #\o #\" #\}))
(defun parse-pair (lista)
  (let((lista-pulita (pulisci-lista lista)))
    (if (or (equal (first lista-pulita) '#\") 
            (equal (first lista-pulita) '#\'))
        (let ((result (parse-string lista-pulita)))
          (fine-pair result))
      (error "errore parse-pair"))))

;;; fine-pair (json)
; input  ==> ("stringa" (#\: #\5 #\, #\Space ... char vari restanti))
; output ==> ((stringa 5) (#\, #\Space ... char vari restanti))
(defun fine-pair (input)
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
      (error "errore fine-pair"))))

;;; parse-value (json)
; input  ==> (coerce " 5, \"x\" : [1, 2, 3]}" 'list)
; output ==> (5 (#\, #\Space #\" #\x #\" #\Space #\: #\Space #\[ #\1 #\, #\Space #\2 #\, #\Space #\3 #\] #\}))

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
; input  ==> (coerce "5, \"x\" : [1, 2, 3]}" 'list) NIL
; output ==> (5 (#\, #\Space #\" #\x #\" #\Space #\: #\Space #\[ #\1 #\, #\Space #\2 #\, #\Space #\3 #\] #\}))
(defun parse-number (input buffer)
  (cond
   ((null input) (error "errore parse-number"))
   ((and (char<= '#\0 (first input)) (char>= '#\9 (first input))) 
    (parse-number (rest input) (append buffer (list (first input)))))
   ((char= '#\. (first input)) ; caso numero float
    (parse-number-float (rest input) (append buffer (list (first input)))))
   (T (append 
       (list (parse-integer (coerce buffer 'string)))
       (pulisci-lista (list input))))
))

;;; parse-number-float (json buffer)
; input  ==> (coerce "4, \"x\" : [1, 2, 3]}" 'list) '(#\5 #\6 #\.)
; output ==> (56.4 (#\, #\Space #\" #\x #\" #\Space #\: #\Space #\[ #\1 #\, #\Space #\2 #\, #\Space #\3 #\] #\}))
(defun parse-number-float (input buffer)
  (cond
   ((or (null input) (char= '#\. (first input))) 
    (error "errore parse-number-float"))
   ((and (char<= '#\0 (first input)) (char>= '#\9 (first input))) 
    (parse-number-float (rest input) (append buffer (list (first input)))))
   (T (append 
       (list (parse-float (coerce buffer 'string))) ; converte il buffer da string a float
       (pulisci-lista (list input))))
))

;;; parse-string (input)
; input  ==> (coerce "\"asd\" : 5, \"pippo\" : \"franco\"}" 'list)
; output ==> ("asd" (#\Space #\: #\Space #\5 #\, #\Space #\" #\p #\i #\p #\p #\o #\" #\Space #\: #\Space #\" #\f #\r #\a #\n #\c #\o #\" #\}))
(defun parse-string (input)
  (cond 
   ((char= '#\' (first input)) ; caso di "asd"
    (parse-string-sq (rest input) NIL))
   ((char= '#\" (first input)) ; caso di 'asd'
    (parse-string-dq (rest input) NIL))))

;;; parse-string-sq (input buffer)
; input  ==> (coerce "\"asd\" : 5, \"pippo\" : \"franco\"}" 'list)
; output ==> ("asd" (#\Space #\: #\Space #\5 #\, #\Space #\" #\p #\i #\p #\p #\o #\" #\Space #\: #\Space #\" #\f #\r #\a #\n #\c #\o #\" #\}))
(defun parse-string-sq (input buffer)
  (cond
   ((null input) (error "apici non chiusi"))
   ((equal (first input) '#\") (error "virgolette dentro gli apici"))
   ((not (equal (first input) '#\'))
    (parse-string-sq 
     (rest input) 
     (append buffer (list (first input)))))
   (T (append 
        (list (coerce buffer 'string))
        (pulisci-lista (list (rest input)))))
))

;;; parse-string-dq (input buffer)
; input e output analoghi a parse-string-sq, ma con gli apici al posto delle virgolette
(defun parse-string-dq (input buffer)
  (cond
   ((null input) (error "virgolette non chiuse"))
   ((equal (first input) '#\') (error "apici dentro le virgolette"))
   ((not (equal (first input) '#\"))
    (parse-string-dq 
     (rest input) 
     (append buffer (list (first input)))))
   (T (append 
        (list (coerce buffer 'string))
        (pulisci-lista (list (rest input)))))
))
   
;;; parse-annidato (input)
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

;;; fine-array (json obj)
;; Determines whether an array has ended or a new
;; element has to be parsed
(defun fine-array (lista-precedente parse-precedente)
  (let ((parse-successivo (append parse-precedente (list (first lista-precedente))))
        (lista-succesiva (pulisci-lista (first (rest lista-precedente)))))
    (cond
     ((char= (first lista-succesiva) '#\]) 
      (append (list parse-successivo) (list (tronca-primo lista-succesiva))))
     ((char= (first lista-succesiva) '#\,) 
      (parse-elements (tronca-primo lista-succesiva) parse-successivo))  
     (T (error "errore fine-array")))))

;;; fine-object (json obj)
;; Determines whether an object has ended or a new
;; pair has to be parsed
; input  ==> (("asd" 5) (#\, #\Space #\" #\p #\i #\p #\p #\o #\" #\Space #\: #\Space #\" #\f #\r #\a #\n #\c #\o #\" #\})) NIL
; output ==> ((("asd" 5) ("pippo" "franco")) NIL)
(defun fine-object (lista-precedente parse-precedente)
  (let ((parse-successivo (append parse-precedente (list (first lista-precedente))))
        (lista-succesiva (pulisci-lista (first (rest lista-precedente)))))
    (cond
     ((char= (first lista-succesiva) '#\}) 
      (append (list parse-successivo) (list (rest lista-succesiva))))
     ((char= (first lista-succesiva) '#\,) 
      (parse-members (rest lista-succesiva) parse-successivo))  
     (T (error "errore fine-object")))))

;;; pulisci-lista (list)
(defun pulisci-lista (list)
  (if (or (equal (first list) '#\Space)
          (equal (first list) '#\Newline)
          (equal (first list) '#\Tab))
      (pulisci-lista (rest list))
    list))

;;; json-access(json, &rest fields)
;; -Follows a chain of keys (iff JSON_obj at current level 
;;  is an object) or indexes (iff JSON_obj at current level 
;;  is an array) in order to retrieve a certain value.
;; -The main idea is to simply go through the list as needed.
;; -Two different predicates are used since the keyword 
;;  &rest had a few issues with recursive calls.

(defun json-access (json &rest fields)
  (if (null fields)        ; returns an identity if fields is empty
      (error "errore json-access (campo vuoto)")
  (access-supp json fields)))


; input e output analoghi a json-access
(defun access-supp (json fields)
  (cond
   ;; base case: only 1 field
   ((and (eq (list-length fields) 1)
         (listp json)
         (stringp (first fields))
         (eq (first JSON) 'json-obj)) ; caso oggetto
    (json-search-by-key (rest json) (first fields)))
   ((and (eq (list-length fields) 1) 
         (listp json)
         (numberp (first fields))
         (>= (first fields) 0)
         (eq (first JSON) 'json-array)) ;caso array
    (json-search-by-index (rest json) (first fields)))
   ;; inductive case: more than 1 field.
   ;; retrieve the value which has key/index equal
   ;; to first field, then call access-supp again
   ((and (> (list-length fields) 1) 
         (listp json)
         (stringp (first fields))
         (eq (first JSON) 'json-obj)) ; caso oggetto
    (access-supp
     (json-search-by-key (rest json) (first fields))
     (rest fields)
     ))
   ((and (> (list-length fields) 1)
         (listp json)
         (numberp (first fields))
         (>= (first fields) 0)
         (eq (first JSON) 'json-array)) ; caso array
    (access-supp
     (json-search-by-index (rest json) (first fields))
     (rest fields)
     ))
   (T (error "errore access-supp"))))

;;; json-search-by-key (json key)
; input  ==> (("nome" "Arthur") ("cognome" "Dent")) "nome"
; output ==> "Arthur"
(defun json-search-by-key (input string)
  (cond
   ((NULL input) (error "errore json-search-by-key"))
   ((equal (first (first input)) string) (first (rest (first input))))
   (T (json-search-by-key (rest input) string))
   ))

;;; json-search-by-index (json index)
; input  ==> '(1 2 3) 2
; output ==> 3
(defun json-search-by-index (input posizione)
  (cond
   ((NULL input) (error "errore json-search-by-index"))
   ((eq posizione 0) (first input))
   (T (json-search-by-index (rest input) (- posizione 1)))
   ))

;;; json-read(filename)
;; -Loads a json file and returns its equivalent list-form
;; -Quite self explanatory...
(defun json-read (filename)
  (with-open-file (stream filename 
                          :direction :input 
                          :if-does-not-exist :error)
    (let ((stringa-allocata (make-string (file-length stream))))        ; allocco una stringa lunga quanto il file
       (let ((contenuto-file (read-sequence stringa-allocata stream)))  ; rimpiazzo i caratteri nella stringa con quelli dello stream
         (json-parse (subseq stringa-allocata 0 contenuto-file))))))

;;; json-dump(json filename).
;; -Loads a json file and returns its equivalent list-form
;; -Quite self explanatory...
(defun json-dump (JSON filename)
  (with-open-file (stream filename 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
  (format stream (json-to-string JSON))
  filename))

;;; json-to-string (json)
;; Checks if json is an object, the final string
;; will be { something }; if json is an array
;; it will be [ something ].
(defun json-to-string (JSON)
  (cond
   ((eq (first JSON) 'json-obj) ;caso oggetto
    (concatenate 'string 
                 "{" 
                 (cancella-virgola-finale
                  (json-print-obj (rest JSON))) 
                 "}"
                 ))
   ((eq (first JSON) 'json-array) ;caso array
    (concatenate 'string 
                 "[" 
                 (cancella-virgola-finale
                  (json-print-array (rest JSON)))
                 "]"
                 ))
   (T (error "errore json-to-string"))))

;;; json-print-obj (json)
;; Prints the first pair and then the others.
; input  ==> '(("nome" "Arthur") ("cognome" "Dent"))
; output ==> "\"nome\":\"Arthur\",\"cognome\":\"Dent\","
(defun json-print-obj (JSON)
  (cond
   ((NULL JSON) "")
   ((listp (first JSON)) 
    (concatenate 'string 
                 (json-print-pair (first JSON)) 
                 (json-print-obj (rest JSON))
                 ))))

;;; json-print-pair (json)
; input  ==> '("nome" "Arthur")
; output ==> "\"nome\" : \"Arthur\", "
(defun json-print-pair (JSON)
  (concatenate 'string "\""
               (first JSON)
               "\"" " : " 
               (json-print-value (first (rest JSON)))
               ", "
               ))

;;; json-print-value (value)
;; Note: only the value to print is passed
(defun json-print-value (value)
  (cond
   ((numberp value) ; caso numero
    (write-to-string value))
   ((stringp value) ; caso stringa
    (concatenate 'string "\"" value "\""))
   (T (json-to-string value)))) ; caso annidato

;;; json-print-array (json)
;; Prints the first element and then the others.
; input  ==> '(1 2 3)
; output ==> "1, 2, 3, "
(defun json-print-array (JSON)
  (cond
   ((NULL JSON) "")
   (T (concatenate 'string 
      (json-print-value (first JSON))
       ", "
      (json-print-array (rest JSON))
    ))))

;;; cancella-virgola-finale (json)
(defun cancella-virgola-finale (JSON)
  (cond
    ((string= "" JSON) JSON)
    (T (subseq JSON 0 (- (length JSON) 2)))))

;;;; end of file -- json-parsing.lisp
