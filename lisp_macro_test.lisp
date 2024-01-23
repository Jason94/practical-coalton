(in-package :cl-user)
(defpackage :macro-test-lisp
  (:use
    #:common-lisp))
(in-package :macro-test-lisp)

(defmacro make-one-fn (fn-keyword)
  `(defun ,fn-keyword ()
      1))

(make-one-fn bar)

(bar)

(defmacro make-foo-fn (fn-sym)
  (let ((full-sym (intern (concatenate  'string (string '#:foo-) (symbol-name fn-sym)))))
    `(defun ,full-sym ()
        1)))

(make-foo-fn :bar)

(foo-bar)