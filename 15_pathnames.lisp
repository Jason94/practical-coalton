;; To load this file, you need to load Chapter 15 from the Practical
;; Common Lisp repository. The source code can be found at the link
;; below. Then you can copy the Chapter 15 directory into
;; ~/quicklisp/local-projects and run (quickload "Chapter-15").
;; https://gigamonkeys.com/book/

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

(named-readtables:in-readtable coalton:coalton)

;; Port of Chapter 15 from Practical Common Lisp, by Peter Seibel,
;; to Coalton.
;;
;; In the previous chapter, we took the approach of rewriting the
;; Common Lisp functions in Coalton. In this chapter, we instead
;; write a thin wrapper around the Common Lisp functions.
;;
;; First, we start by defining a few Coalton types that provide
;; a type-safe interface to the Common Lisp pathname functions.
;; For example, in several of the Common Lisp functions a wildcard
;; pathname is an invalid argument. Instead of checking for wildcard
;; paths and erroring if found, we can use the type system to ensure
;; that we never pass a wildcard path to a function that doesn't
;; accept it.
;;
;; Second, we define a corresponding Coalton function for each
;; function exported by the Common Lisp pathname package.

(cl:defmacro wrap-lisp-call (type function value)
  "Wrap a call to a Lisp function that returns a value of type TYPE
in a Coalton function that returns a value of type (Optional TYPE).
A nil return value is converted to None."
  `(lisp (Optional ,type) (,value)
     (cl:let ((result (,function ,value)))
       (cl:if result
         (coalton (Some (lisp ,type () result)))
         (coalton None)))))

(coalton-toplevel

  (define-class (Path :a))

  (define-instance (Path String))

  (repr :native cl:pathname)
  (define-type Pathname)

  (declare pathname->string (Pathname -> String))
  (define (pathname->string pathname)
    (lisp String (pathname)
      (cl:write-to-string pathname)))

  (define-instance (Path Pathname))

  (repr :native cl:pathname)
  (define-type WildPathname)

  (define-instance (Path WildPathname))

  )

(coalton-toplevel

  (declare list-directory ((Result String Pathname) -> (List Pathname)))
  (define (list-directory dirname)
    "Return a list of pathnames for the files in the directory named by DIRNAME."
    (match dirname
      ((Err str)
       (lisp (List Pathname) (str)
         (pth:list-directory str)))
      ((Ok pth)
       (lisp (List Pathname) (pth)
         (pth:list-directory pth)))))

  (declare file-exists-p ((Path :a) => :a -> (Optional Pathname)))
  (define (file-exists-p pth)
    "Return a pathname for the file named by PTH if it exists, otherwise return None."
    (wrap-lisp-call :a pth:file-exists-p pth))

  (declare directory-pathname-p ((Path :a) => :a -> (Optional :a)))
  (define (directory-pathname-p pth)
    (wrap-lisp-call :a pth:directory-pathname-p pth))

  (declare file-pathname-p ((Path :a) => :a -> (Optional :a)))
  (define (file-pathname-p pth)
    (wrap-lisp-call :a pth:file-pathname-p pth))

  (declare pathname-as-directory ((Result String Pathname) -> Pathname))
  (define (pathname-as-directory pth)
    (match pth
      ((Err str)
       (lisp :a (str)
         (pth:pathname-as-directory str)))
      ((Ok pth)
       (lisp :a (pth)
         (pth:pathname-as-directory pth)))))

  (declare pathname-as-file ((Result String Pathname) -> Pathname))
  (define (pathname-as-file pth)
    (match pth
      ((Err str)
       (lisp :a (str)
         (pth:pathname-as-file str)))
      ((Ok pth)
       (lisp :a (pth)
         (pth:pathname-as-file pth)))))

  )

(coalton-toplevel

  ;; TODO: Are coalton functions always guaranteed to be callable by APPLY
  ;; in Common Lisp?
  (declare walk-directory-if (String -> (Pathname -> :b) -> Boolean -> (Pathname -> Boolean) -> Unit))
  (define (walk-directory-if dirname fn include-dirs test-fn)
    (lisp :t (dirname fn include-dirs test-fn)
      (cl:let ((lisp-fn (cl:lambda (fn pthnm)
                          (coalton (fn (lisp :a (pthnm)
                                           pthnm))))))
        (pth:walk-directory dirname (cl:lambda (pthnm)
                                      (cl:print pthnm)))))
    Unit)

;                          :directories include-dirs :test test-fn))
;    Unit)
      ; (cl:let ((lisp-fn (cl:lambda (pthnm)
      ;                     (coalton (fn (lisp Pathname () pthnm)))))
      ;          (lisp-test (cl:lambda (pthnm)
      ;                       (coalton (test-fn (lisp Pathname () pthnm))))))
      ;   (pth:walk-directory dirname lisp-fn :directories include-dirs :test lisp-test)))
  )

(coalton-toplevel
  (declare foo-pth (Pathname -> Unit))
  (define (foo-pth pth)
    (lisp :a (pth)
      (cl:format cl:t "~a" pth))
    Unit)

  )

(coalton
 (walk-directory-if "." foo-pth False (fn (y) False)))

; (coalton
;  (do
;   (pth <- (file-exists-p "."))
;   (let files = (list-directory (into pth)))
;   (first-file <- (head files))
;   (file-exists-p first-file)
;   (pure first-file)))
