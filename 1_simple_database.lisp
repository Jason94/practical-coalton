(in-package :cl-user)
(defpackage :practical-coalton.simple-database
  (:use
    #:coalton
    #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:lst #:coalton-library/list)
   (#:cel #:coalton-library/cell)
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
    (Database (Cell (List CD))))
  
  (declare from-cds ((List CD) -> Database))
  (define (from-cds cd-list)
    (Database (cel:new cd-list)))
  
  (declare cds (Database -> (List CD)))
  (define (cds db)
    (match db
      ((Database cds-cell) (cel:read cds-cell))))
  
  (declare new-database (Unit -> Database))
  (define (new-database)
    (Database (cel:new Nil)))
  
  (declare add-record (Database -> CD -> Database))
  (define (add-record db cd)
    (match db
      ((Database cds)
        (cel:push! cds cd)
        db)))
  
  (declare dump-cd (CD -> Unit))
  (define (dump-cd cd)
    (match cd
      ((CD title artist rating ripped)
       (traceobject "title" title)
       (traceobject "artist" artist)
       (traceobject "rating" rating)
       (traceobject "ripped" ripped))))

  (declare dump-db (Database -> Unit))
  (define (dump-db db)
    (match db
      ((Database cds)
       (for cd in (cel:read cds)
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
    (let ((cd-list (cds db)))
      (lisp :a (cd-list filename)
        (cl:with-open-file (out filename
                                :direction :output
                                :if-exists :supersede)
          (cl:with-standard-io-syntax
            (cl:print cd-list out)))))
    db)
  
  (declare load-db (String -> Database))
  (define (load-db filename)
    (from-cds
      (lisp (List CD) (filename)
        (cl:with-open-file (in filename)
          (cl:with-standard-io-syntax
            (cl:read in))))))
  
  ; (declare select ((CD -> Boolean) -> Database -> (Vector CD)))
  ; (define (select selector-fn db)
  ;   (match db
  ;     ((Database cds) cds)))
  )

(coalton
  (save-db (add-cds (new-database)) "cds.db"))

(coalton
  (load-db "cds.db"))
