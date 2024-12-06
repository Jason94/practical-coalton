(in-package :cl-user)
(ql:quickload "cl-ppcre")
(ql:quickload "coalton")
(defpackage :practical-coalton.spam-filter
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/state)
  (:import-from
   #:coalton-library/math/real
   :inexact/)
  (:local-nicknames
   (#:m #:coalton-library/ord-map)
   (#:o #:coalton-library/optional)))
(in-package :practical-coalton.spam-filter)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (define (trace-tuple tup)
    (match tup
      ((Tuple a b)
       (progn
        (traceobject "a" a)
        (traceobject "b" b))))))

(coalton-toplevel

  (define MAX-HAM-SCORE 0.4)
  (define MIN-SPAM-SCORE 0.6)

  (define-type Classification
    Ham
    Spam
    Unsure)

  (define-struct WordFeature
    (word "The word this feature represents." String)
    (spam-count "Number of spams we have seen this feature in." Integer)
    (ham-count "Number of hams we have seen this feature in." Integer))

  (declare new-word-feature (String -> WordFeature))
  (define (new-word-feature word)
    (WordFeature word 0 0))

  (declare increment-feature (Classification -> WordFeature -> WordFeature))
  (define (increment-feature type feature)
    (traceobject "type" type)
    (match feature
      ((WordFeature word spam-count ham-count)
       (match type
         ((Unsure) feature)
         ((Ham) (WordFeature word spam-count (1+ ham-count)))
         ((Spam) (WordFeature word (1+ spam-count) ham-count))))))

  (define-struct FeatureDatabase
    (db (m:Map String WordFeature))
    (total-hams Integer)
    (total-spams Integer))

  (declare increment-total (Classification -> FeatureDatabase -> FeatureDatabase))
  (define (increment-total type feature-database)
    (match feature-database
      ((FeatureDatabase db total-hams total-spams)
       (match type
         ((Unsure) feature-database)
         ((Ham) (FeatureDatabase db (1+ total-hams) total-spams))
         ((Spam) (FeatureDatabase db total-hams (1+ total-spams)))))))

  (declare empty-database FeatureDatabase)
  (define empty-database (FeatureDatabase m:empty 0 0))

  (declare classification (Single-Float -> Classification))
  (define (classification score)
    (cond
     ((<= score MAX-HAM-SCORE) Ham)
     ((>= score MIN-SPAM-SCORE) Spam)
     (True Unsure)))

  (declare classify (String -> Classification))
  (define (classify _)
    Ham)

  (declare extract-words (String -> (List String)))
  (define (extract-words text)
    (lisp (List String) (text)
      (cl:delete-duplicates
        (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
        :test #'cl:string=)))

  (declare spam-probability (FeatureDatabase -> WordFeature -> Double-Float))
  (define (spam-probability
            (FeatureDatabase _ total-hams total-spams)
            (WordFeature _ spam-count ham-count))
    (let ((spam-frequency (inexact/ spam-count (max 1 total-spams)))
          (ham-frequency (inexact/ ham-count (max 1 total-hams))))
      (/ spam-frequency (+ spam-frequency ham-frequency))))

  (declare %bayesian-spam-probability (Double-Float -> Double-Float -> FeatureDatabase -> WordFeature -> Double-Float))
  (define (%bayesian-spam-probability assumed-probability weight db feature)
    (let ((basic-probability (spam-probability db feature))
          (data-points (+ (.spam-count feature) (.ham-count feature))))
      ; (inexact/ (+ (* weight assumed-probability)
      ;              (* data-points basic-probability))
                (+ 0.2d0 basic-probability)))

  (declare bayesian-spam-probability (FeatureDatabase -> WordFeature -> Double-Float))
  (define bayesian-spam-probability (%bayesian-spam-probability 0.5d0 1))))

(coalton-toplevel

  (declare intern-feature (String -> (ST FeatureDatabase WordFeature)))
  (define (intern-feature word)
    "Add a WordFeature for WORD to the database if it doesn't have one already."
    (do
      ((FeatureDatabase db total-hams total-spams) <- get)
      (let updated-db = (with-default db (m:insert db word (new-word-feature word))))
      (let feature = (from-some "Error inserting feature" (m:lookup updated-db word)))
      (put (FeatureDatabase updated-db total-hams total-spams))
      (pure feature)))

  (declare extract-features (String -> (ST FeatureDatabase (List WordFeature))))
  (define (extract-features text)
    "Add a WordFeature for each word in TEXT to the database if it doesn't have them already."
    (sequence (map intern-feature (extract-words text))))

  (declare increment-count (Classification -> String -> (ST FeatureDatabase (Optional WordFeature))))
  (define (increment-count type word)
    (do
      ((FeatureDatabase db total-hams total-spams) <- get)
      (let updated-map = (with-default db (m:update (increment-feature type)
                                                    db
                                                    word)))
      (put (FeatureDatabase updated-map total-hams total-spams))
      (pure (m:lookup updated-map word))))

  (declare train (String -> Classification -> (ST FeatureDatabase Unit)))
  (define (train text type)
    "Train the filter to associate each word in TEXT with the classification TYPE."
    (do
      (word-features <- (extract-features text))
      (let words = (map .word word-features))
      (sequence (map (increment-count type) words))
      (db <- get)
      (put (increment-total type db)))))

(coalton
  (trace-tuple
    (run
      (do
        (train "hello how are you are you good?" Spam))
      empty-database)))