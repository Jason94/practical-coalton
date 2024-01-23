(in-package :cl-user)
(ql:quickload "coalton")
(ql:quickload "alexandria")
(defpackage :practical-coalton.simple-database
  (:use
    #:coalton
    #:coalton-prelude)
  (:import-from
   #:alexandria
   :with-gensyms)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:lst #:coalton-library/list)
   (#:cel #:coalton-library/cell)
   (#:str #:coalton-library/string)))
(in-package :practical-coalton.simple-database)

(cl:defun keyword-to-struct-accessor (keyword)
  (cl:intern (cl:concatenate 'cl:string (cl:string '#:.) (cl:string keyword))))

(cl:defun comparison-clause (attr-keyword value var-sym)
    `(== ,value (,(keyword-to-struct-accessor attr-keyword) ,var-sym)))

(cl:defun comparison-clauses (var-sym clauses)
  (cl:loop while clauses
    collecting (comparison-clause (cl:pop clauses)
                                  (cl:pop clauses)
                                  var-sym)))

(cl:defmacro where (cl:&rest clauses)
  (with-gensyms (cd-sym)
    `(fn (,cd-sym)
       (and ,@(comparison-clauses cd-sym clauses)))))

(cl:defmacro to (cl:&key title artist rating (ripped nil ripped-p))
  (with-gensyms (cd-sym)
    `(fn (,cd-sym)
      (CD
        ,(cl:if title title `(.title ,cd-sym))
        ,(cl:if artist artist `(.artist ,cd-sym))
        ,(cl:if rating rating `(.rating ,cd-sym))
        ,(cl:if ripped-p ripped `(.ripped ,cd-sym))))))

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
  
  (declare cd-data (Database -> (Cell (List CD))))
  (define (cd-data (Database cd-cell))
    cd-cell)

  (declare from-cds ((List CD) -> Database))
  (define (from-cds cd-list)
    (Database (cel:new cd-list)))
  
  (declare cds (Database -> (List CD)))
  (define (cds (Database cds-data))
    (cel:read cds-data))
  
  (declare new-database (Unit -> Database))
  (define (new-database)
    (Database (cel:new Nil)))
  
  (declare add-record (Database -> CD -> Database))
  (define (add-record db cd)
    (cel:push! (cd-data db) cd)
    db)
  
  (declare dump-cd (CD -> Unit))
  (define (dump-cd cd)
    (traceobject "title" (.title cd))
    (traceobject "artist" (.artist cd))
    (traceobject "rating" (.rating cd))
    (traceobject "ripped" (.ripped cd)))

  (declare dump-db (Database -> Unit))
  (define (dump-db (Database cd-data))
    (for cd in (cel:read cd-data)
        (dump-cd cd)
        (trace "--"))
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
      
  (declare select (Database -> (CD -> Boolean) -> (List CD)))
  (define (select db selector-fn)
    (lst:filter selector-fn (cds db)))
 
  (declare delete! (Database -> (CD -> Boolean) -> Database))
  (define (delete! db selector-fn)
    (cel:write! (cd-data db) (lst:remove-if selector-fn (cds db)))
    db)
  
  (declare update! (Database -> (CD -> Boolean) -> (CD -> CD) -> Database))
  (define (update! db selector-fn update-fn)
    (cel:write! (cd-data db)
      (map (fn (cd) (if (selector-fn cd)
                      (update-fn cd)
                      cd))
           (cds db)))
    db)

  )

; (coalton
;   (load-db "cds.db"))

; (coalton
;   (update!
;     (load-db "cds.db")
;     (where :artist "Dua Lipa")
;     (to :ripped True :rating 1)))

; (coalton-toplevel
;   (declare set-ripped (Boolean -> CD -> CD))
;   (define (set-ripped! ripped cd)
;     (CD ()))

; (coalton
;   (save-db (add-cds (new-database)) "cds.db"))

; (coalton
;   (select (load-db "cds.db")
;           (where :ripped True)))

; (coalton
;   (delete! (load-db "cds.db")
;            (where :artist "Dua Lipa" :ripped False)))