; (named-readtables:in-readtable coalton:coalton)

; (cl:defmacro define-with-attribute (struct-type attr attr-type)
;   (cl:let ((accessor-keyword (cl:intern (cl:concatenate 'cl:string
;                                                         (cl:string '#:.)
;                                                         (cl:string attr))))
;            (function-keyword (cl:intern (cl:concatenate 'cl:string
;                                                         (cl:string '#:with-)
;                                                         (cl:string attr)))))
;     (with-gensyms (attr-variable cd-variable)
;       `(progn
;         (declare ,function-keyword (,attr-type -> (,struct-type -> Boolean)))
;         (define (,function-keyword ,attr-variable)
;           (fn (,cd-variable) (== ,attr-variable (,accessor-keyword ,cd-variable))))))))

;   (declare where ((List (CD -> Boolean)) -> CD -> Boolean))
;   (define (where selectors cd)
;     (lst:all (fn (f) (f cd)) selectors))

  ; (declare with-title (String -> (CD -> Boolean)))
  ; (define (with-title title)
  ;   (fn (cd) (== title (.title cd))))
  
  ; (declare with-artist (String -> (CD -> Boolean)))
  ; (define (with-artist artist)
  ;   (fn (cd) (== artist (.artist cd))))
  
  ; (declare with-rating (Integer -> (CD -> Boolean)))
  ; (define (with-rating rating)
  ;   (fn (cd) (== rating (.rating cd))))
  
  ; (declare with-ripped (Boolean -> (CD -> Boolean)))
  ; (define (with-ripped ripped)
  ;   (fn (cd) (== ripped (.ripped cd))))