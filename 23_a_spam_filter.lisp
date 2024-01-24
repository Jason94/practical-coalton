(in-package :cl-user)
(ql:quickload "coalton")
(ql:quickload "cl-ppcre")
(defpackage :practical-coalton.spam-filter
  (:use
    #:coalton
    #:coalton-prelude)
  (:local-nicknames
    (#:cel #:coalton-library/cell)
    (#:lst #:coalton-library/list)
    (#:htbl #:coalton-library/hashtable)
    (#:rl #:coalton-library/math/real)
    (#:ath #:coalton-library/math/arith)))
(in-package :practical-coalton.spam-filter)

; (named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  
  ;;
  ;; Coalton wrappers around Lisp functions
  ;;

  (declare all-matches (String -> String -> (List String)))
  (define (all-matches regexp str)
    (lisp (List String) (regexp str)
      (cl-ppcre:all-matches-as-strings regexp str)))

  ;;
  ;; Set up the parameters, types, and basic type utilities
  ;;

  (define MAX-HAM-SCORE 0.4d0)
  (define MIN-SPAM-SCORE 0.6d0)

  (define TOTAL-HAMS (cel:new 0))
  (define TOTAL-SPAMS (cel:new 0))

  (define-type Classification
    Ham
    Spam
    Unsure)

  (declare classification (Double-Float -> Classification))
  (define (classification score)
    (cond
     ((<= score MAX-HAM-SCORE) Ham)
     ((>= score MIN-SPAM-SCORE) Spam)
     (True Unsure)))
  
  (define-struct Word-Feature
    (word String)
    (spam-count (Cell Integer))
    (ham-count (Cell Integer)))
  
  (declare new-feature (String -> Word-Feature))
  (define (new-feature word)
    (Word-Feature word 0 0))
  
)

(coalton-toplevel
  ;;
  ;; Set up the feature database and functions to read/write to it
  ;;

  (declare FEATURE-DATABASE (Cell (Hashtable String Word-Feature)))
  (define FEATURE-DATABASE (cel:new (htbl:new)))

  (declare db-table (Unit -> (Hashtable String Word-Feature)))
  (define (db-table)
    (cel:read FEATURE-DATABASE))
  
  (define (clear-database!)
    (cel:write! FEATURE-DATABASE (htbl:new))
    (cel:write! TOTAL-HAMS 0)
    (cel:write! TOTAL-SPAMS 0))
  
  (declare intern-feature! (String -> Word-Feature))
  (define (intern-feature! word)
    (match (htbl:get (db-table) word)
      ((Some feature) feature)
      ((None)
       (let ((feature (new-feature word)))
         (htbl:set! (db-table) word feature)
         feature))))
  
  (declare untrained-p (Word-Feature -> Boolean))
  (define (untrained-p feature)
    (and (ath:zero? (.ham-count feature))
         (ath:zero? (.spam-count feature))))
  
  )

    
(coalton-toplevel
  ;;
  ;; Spam Training
  ;;

  (declare extract-words (String -> (List String)))
  (define (extract-words text)
    (lst:remove-duplicates (all-matches "[a-zA-Z]{3,}" text)))
  
  (declare extract-features! (String -> (List Word-Feature)))
  (define (extract-features! text)
    (map intern-feature! (extract-words text)))
  
  (declare increment-count! (Word-Feature -> Classification -> Word-Feature))
  (define (increment-count! feature type)
    (match type
      ((Ham) (cel:increment! (.ham-count feature)))
      ((Spam) (cel:increment! (.spam-count feature)))
      ((Unsure) 0))
    feature)
  
  (declare increment-total-count! (Classification -> Integer))
  (define (increment-total-count! type)
    (match type
      ((Ham) (cel:increment! TOTAL-HAMS))
      ((Spam) (cel:increment! TOTAL-SPAMS))
      ((Unsure) 0)))

  (declare train! (String -> Classification -> Unit))
  (define (train! text type)
    (for feature in (extract-features! text)
      (increment-count! feature type))
    (increment-total-count! type)
    Unit)
  
  )

(coalton-toplevel
  ;;
  ;; Spam Analysis
  ;;

  (declare spam-probability (Word-Feature -> Double-Float))
  (define (spam-probability feature)  
    (let ((spam-frequency (rl:inexact/ (cel:read (.spam-count feature))
                                       (max 1 (cel:read TOTAL-SPAMS))))
          (ham-frequency (rl:inexact/ (cel:read (.ham-count feature))
                                      (max 1 (cel:read TOTAL-HAMS)))))
      (/ spam-frequency (+ spam-frequency ham-frequency))))

  (declare bayesian-spam-probability (Word-Feature -> Double-Float -> Double-Float -> Double-Float))
  (define (bayesian-spam-probability feature assumed-probability weight)
    (let ((basic-probability (spam-probability feature))
          (data-points (fromInt (+ (cel:read (.spam-count feature))
                                   (cel:read (.ham-count feature))))))
      (/ (+ (* weight assumed-probability)
            (* data-points basic-probability))
         (+ weight data-points))))

  (declare fisher (Integer -> Integer -> Double-Float))
  (define (fisher probs number-of-probs)
    (inverse-chi-square
     (* -2 (log)))

  ; (declare classify (String -> Double-Float))
  ; (define (classify text)
  ;   (classification (score (extract-features text))))

  )

(coalton-toplevel
  
  (declare foobar (Double-Float -> Integer -> Double-Float))
  (define (foobar d n)
    (* d (fromInt n)))

  )