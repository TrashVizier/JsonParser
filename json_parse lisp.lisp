;;;; -*- Mode: Lisp -*-
;;;; json_parse.lisp

;; Giorgio Gragnano   822433
;; Andrea Lamparella  829602
;; Pietro Venturini   856115

;;; json-parse (json)
;; La funzione prende in input una stringa e ne restituisce una lista dei 
;; singoli componenti

(defun json-parse (json)
  (let ((lista-chars 
         (pulizia-iniziale (coerce json 'list))))
   (cond 
     ((and 
      (equal (first lista-chars) '#\{)
      (equal (first (last lista-chars)) '#\})) 
     (parse-object (rest lista-chars)))
     ((and (equal (first lista-chars) '#\[)
     (equal (first (last lista-chars)) '#\]))
     (parse-array (rest lista-chars)))
     (T (error "errore json-parse")))))

;;; parse-array (json)
;; La funzione prende in input una lista di char rappresentante un array e lo
;; restituisce in forma parsata

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
;; La funzione prende in input una lista di char rappresentante un object e lo
;; restituisce in forma parsata

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
;; La funzione prende in input una lista di caratteri e una lista di supporto 
;; Ritorna una lista con dentro 2 liste, la 1^ ha dentro tutti gli 
;; elementi dell'array, la 2^ e' la lista di char restanti dell'input  

(defun parse-elements (input precedente)
  (incolla-array (parse-value input) precedente))

;;; parse-members (json obj)
;; La funzione e' analoga a parse-elements

(defun parse-members (input precedente)
  (incolla-object (parse-pair input)  precedente))

;;; parse-pair (json obj)
;; La funzione prende in input una lista di caratteri e restituisce un output
;; analogo a parse-elements ma con la lista della prima pair come primo 
;; elemento della lista dell'output

(defun parse-pair (lista)
  (let((lista-pulita (pulisci-lista lista)))
    (if (or (equal (first lista-pulita) '#\") 
            (equal (first lista-pulita) '#\'))
        (let ((result (parse-string lista-pulita)))
          (incolla-pair result))
      (error "errore parse-pair"))))

;;; incolla-pair (json)
;; La funzione "attacca" la value alla string della pair, 
;; come mostrato nell'esempio sottostante

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
;; La funzione prende in input una lista di char e restituisce una lista con 
;; la prima value trovata come 1^ membro e la lista di char rimanente come 2^ 
;; membro

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
;; La funzione prende in input una lista di char e una lista buffer di supporto
;; e restituisce in output una lista con al 1° membro il numero e al 2° membro
;; la lista dei char restanti

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
;; La funzione serve per i numeri float

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
;; La funzione ritorna T se l'input e' un char di una cifra
;; Altrimenti ritorna NIL

(defun is-digit (char) 
  (and 
    (char<= '#\0 char)
    (char>= '#\9 char)))

;;; parse-string (input)
;; La funzione prende in input una lista di char e restituisce una lista con
;; al 1^ membro la prima stringa e al 2^ membro la lista dei char restanti

(defun parse-string (input)
  (cond 
   ((char= '#\' (first input)) ; caso di "asd"
    (parse-string-apici (rest input) NIL))
   ((char= '#\" (first input)) ; caso di 'asd'
    (parse-string-virgolette (rest input) NIL))))

;;; parse-string-apici (input buffer)
;; La funzione e' analoga a parse-string, ma funziona solo sulle stringhe scritte tra apici

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
;; La funzione e' analoga a parse-string, ma funziona solo sulle stringhe scritte tra virgolette

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
;; La funzione prende in input una lista di char e restituisce come output una
;; lista con al 1° membro l'array/object parsato e al 2° membro la lista dei
;; char restanti

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
;; La funzione e' analoga a parse-array, ma la lista dei char rimanenti non 
;; deve essere per forza vuota

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
;; La funzione e' analoga a parse-object, ma la lista dei char rimanenti non 
;; deve essere per forza vuota

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
;; La funzione procede al parsing di una lista di elementi di un object

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
;; La funzione prende in input una lista di caratteri e ritorna come output
;; La stessa lista con tutti i caratteri di spaziatura iniziali rimossi

(defun pulisci-lista (lista)
  (if (is-spaziovuoto (first lista))
      (pulisci-lista (rest lista))
    lista))

;;; is-spaziovuoto (char)
;; La funzione ritorna T se char e' uno spazio/Tab/Newline, 
;; altrimenti ritorna NIL

(defun is-spaziovuoto (char)
  (if (equal NIL  (member char '(#\Space #\Newline #\Tab)))
    NIL
    T))

;;; pulizia-iniziale (lista)
;; La funzione applica pulisci-lista all'inizio e alla fine della lista

(defun pulizia-iniziale (lista)
 (reverse (pulisci-lista (reverse (pulisci-lista lista)))))

;;; json-access-supp(json, &rest fields)
;; -Follows a chain of keys (iff JSON_obj at current level 
;;  is an object) or indexes (iff JSON_obj at current level 
;;  is an array) in order to retrieve a certain value.
;; -The main idea is to simply go through the list as needed.
;; -Two different predicates are used since the keyword 
;;  &rest had a few issues with recursive calls.

(defun json-access (json &rest fields) 
  (json-access-supp json fields))

(defun json-access-supp (json fields)
  (cond
    ((null fields) 
      (error "errore json-access-supp (campo vuoto)"))
   ((caso-object-mono-fields json fields)
    (cerca-valore (rest json) (first fields)))
   ((caso-array-mono-fields json fields) 
    (cerca-posizione (rest json) (first fields)))
   ((caso-object-multi-fields json fields)
    (json-access-supp
     (cerca-valore (rest json) (first fields))
     (rest fields)))
   ((caso-array-multi-fields json fields)
    (json-access-supp
     (cerca-posizione (rest json) (first fields))
     (rest fields)))
   (T (error "errore json-access-supp"))))

;; Funzioni per aumentare la comprensibilità del cond di json-access-supp

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
;; La funzione restituisce la value corrispondente a string nella lista input

(defun cerca-valore (input string)
  (cond
   ((NULL input) (error "errore cerca-valore"))
   ((equal (first (first input)) string) (first (rest (first input))))
   (T (cerca-valore (rest input) string))
   ))

;;; cerca-posizione (input pos)
;; La funzione restituisce il valore presente in input alla posizione pos

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

;;; json-dump(json filename) 
;; La funzione scrive in forma lista sul file filename il json in input in forma parsata 

(defun json-dump (JSON filename)
  (with-open-file (stream filename 
                          :direction :output 
                          :if-exists :supersede
                          :if-does-not-exist :create)
  (format stream (scrivi-json JSON))
  filename))

;;; scrivi-json (json)
;; La funzione scrive le parentesi graffe/quadre nel caso in cui json sia un 
;; object/array

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
;; La funzione scrive i membri (lista di pair) di un object

(defun scrivi-object (members)
  (cond
   ((NULL members) "")
   ((listp (first members)) 
    (concatenate 'string 
                 (scrivi-pair (first members)) 
                 (scrivi-object (rest members))))))

;;; scrivi-pair (pair)
;; La funzione scrive la singola pair

(defun scrivi-pair (pair)
  (concatenate 'string "\""
               (first pair)
               "\"" " : " 
               (scrivi-value (first (rest pair)))
               ", "
               ))

;;; scrivi-value (value)
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
;; La funzione scrive gli elementi (lista di value) di un array

(defun scrivi-array (elements)
  (cond
   ((NULL elements) "")
   (T (concatenate 'string 
      (scrivi-value (first elements))
       ", "
      (scrivi-array (rest elements))
    ))))

;;; cancella-virgola-finale (stringa)
;; La funzione elimina la virgola finale extra (", "), dovuto al modo iterativo
;; con cui sono gestite le liste da scrivere

(defun cancella-virgola-finale (stringa)
  (cond
    ((zerop (length stringa)) stringa)
    (T (subseq stringa 0 (- (length stringa) 2)))))

;;;; end of file -- json-parsing.lisp
