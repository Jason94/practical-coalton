(in-package :cl-user)
(defpackage :practical-coalton.simple-database
  (:use
    #:coalton
    #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:str #:coalton-library/string)))
(in-package :practical-coalton.simple-database)

; (named-readtables:in-readtable coalton:coalton)

(cl:defun convert-simple-vector (simple-vector)
  (cl:make-array (cl:length simple-vector)
            :element-type (cl:array-element-type simple-vector)
            :initial-contents simple-vector
            :adjustable cl:t
            :fill-pointer cl:t))

(coalton-toplevel
  (declare prompt-read (String -> String))
  (define (prompt-read prompt)
    (lisp String (prompt)
      (cl:format cl:*query-io* "~a: " prompt)
      (cl:force-output cl:*query-io*)
      (cl:read-line cl:*query-io*)))
  
  (declare prompt-y-n (String -> Boolean))
  (define (prompt-y-n prompt)
    (lisp Boolean (prompt)
      (cl:y-or-n-p (cl:format cl:nil "~a [y/n]: " prompt)))))

(coalton-toplevel
  (define-struct CD
    (title String)
    (artist String)
    (rating Integer)
    (ripped Boolean))
  
  (repr :transparent)
  (define-type Database 
    (Database (Vector CD)))
  
  (declare new-database (Unit -> Database))
  (define (new-database)
    (Database (vec:new)))
  
  (declare add-record (Database -> CD -> Database))
  (define (add-record db cd)
    (match db
      ((Database cds)
        (vec:push! cd cds)
        db)))
  
  (declare dump-cd (CD -> Unit))
  (define (dump-cd cd)
    (match cd
      ((CD title artist rating ripped)
       (traceobject "title" title)
       (traceobject "artist" artist)
       (traceobject "rating" rating)
       (traceobject "ripped" ripped))))

  (declare dump-db (Database -> Unit))ti
  (define (dump-db db)
    (match db
      ((Database cds)
       (for cd in cds
            (dump-cd cd)
            (trace "--"))))
    Unit)
  
  (declare prompt-for-cd (Unit -> CD))
  (define (prompt-for-cd)
    (CD
      (prompt-read "Title")
      (prompt-read "Artist")
      (with-default 0 (str:parse-int (prompt-read "Rating")))
      (prompt-y-n "Ripped")))
  
  (declare add-cds (Database -> Database))
  (define (add-cds db)
    (add-record db (prompt-for-cd))
    (if (prompt-y-n "Another?")
          (add-cds db)
        db))
  
  (declare save-db (Database -> String -> Database))
  (define (save-db db filename)
    (lisp Database (db filename)
      (cl:with-open-file (out filename
                              :direction :output
                              :if-exists :supersede)
        (cl:with-standard-io-syntax
          (cl:print db out)))))
  
  (declare load-db (String -> Database))
  (define (load-db filename)
    (lisp Database (filename)
      (convert-simple-vector
        (cl:with-open-file (in filename)
          (cl:with-standard-io-syntax
            (cl:read in))))))
  
  (declare select ((CD -> Boolean) -> Database -> (Vector CD)))
  (define (select selector-fn db)
    (match db
      ((Database cds) cds)))
  )

(coalton
  (save-db (add-cds (new-database)) "cds.db"))

(coalton
  (load-db "cds.db"))

(cl:with-open-file (out "vec.txt" :direction :output :if-exists :supersede)
  (cl:with-standard-io-syntax (cl:print #(1 2 3 4) out)))

(cl:type-of (cl:with-open-file (in "vec.txt")
  (cl:with-standard-io-syntax (cl:read in))))