(in-package :cl-user)
(ql:quickload "coalton")
(defpackage :macro-test
  (:use
    #:coalton
    #:coalton-prelude))
(in-package :macro-test)

(cl:defmacro make-one-fn (fn-keyword)
  `(progn
    (define (,fn-keyword)
      1)))

(cl:defmacro make-foo-fn (fn-keyword)
  (cl:let ((full-keyword (cl:intern (cl:concatenate 'cl:string 
                                                    "foo-"
                                                    (cl:string fn-keyword)))))
    `(progn
      (define (,full-keyword)
        1))))

(coalton-toplevel
  (make-one-fn bar))

(coalton
  (bar))

(coalton-toplevel (make-foo-fn :bar))

(coalton (foo-bar))