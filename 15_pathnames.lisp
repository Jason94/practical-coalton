(in-package :cl-user)
(ql:quickload "coalton")
(ql:quickload "Chapter-15")
(defpackage :practical-coalton.pathnames
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
    (#:pth #:com.gigamonkeys.pathnames))
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))
(in-package :practical-coalton.pathnames)

(coalton-toplevel

  (define-type (Either :a :b)
    (Left :a)
    (Right :b))

  (define-instance (Into :a (Either :a :b))
    (define into Left))

  (define-instance (Into :b (Either :a :b))
    (define into Right))

  )

(coalton-toplevel

  (define-class (Path :a))

  (define-instance (Path String))

  (repr :native cl:pathname)
  (define-type Pathname)

  (define-instance (Path Pathname))

  (repr :native cl:pathname)
  (define-type WildPathname)

  (define-instance (Path WildPathname))

  (declare list-directory ((Either String Pathname) -> (List Pathname)))
  (define (list-directory dirname)
    (match dirname
      ((Left str)
       (lisp (List Pathname) (str)
         (pth:list-directory str)))
      ((Right pth)
       (lisp (List Pathname) (pth)
         (pth:list-directory pth)))))

  (declare file-exists-p ((Path :a) => :a -> (Optional Pathname)))
  (define (file-exists-p pth)
    (lisp (Optional Pathname) (pth)
       (cl:let ((result (pth:file-exists-p pth)))
         (cl:if result
           (coalton (Some (lisp :a () result)))
           (coalton None)))))

  )

(coalton
 (do
  (pth <- (file-exists-p "."))
  (let files = (list-directory (into pth)))
  (head files)))
  (file-exists-p first-file)))
