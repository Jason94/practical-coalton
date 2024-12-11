(in-package :cl-user)
(ql:quickload "cl-ppcre")
(ql:quickload "coalton")
(defpackage :practical-coalton.spam-filter
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/list
   #:coalton-library/monad/state)
  (:import-from
   #:coalton-library/math/real
   :inexact/
   :floor/)
  (:import-from
   #:coalton-library/math/arith
   :negate)
  (:local-nicknames
   (#:m #:coalton-library/ord-map)
   (#:o #:coalton-library/optional)
   (#:c #:coalton-library/cell)))
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

  (define MAX-HAM-SCORE 0.4d0)
  (define MIN-SPAM-SCORE 0.6d0)

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
  
  (declare untrained? (WordFeature -> Boolean))
  (define (untrained? feature)
    (and (zero? (.spam-count feature)) (zero? (.ham-count feature))))

  (declare increment-feature (Classification -> WordFeature -> WordFeature))
  (define (increment-feature type feature)
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

  (declare classification (Double-Float -> (Tuple Classification Double-Float)))
  (define (classification score)
    (Tuple
     (cond
       ((<= score MAX-HAM-SCORE) Ham)
       ((>= score MIN-SPAM-SCORE) Spam)
       (True Unsure))
     score))

  (declare extract-words (String -> (List String)))
  (define (extract-words text)
    (lisp (List String) (text)
      (cl:delete-duplicates
        (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
        :test #'cl:string=))))

(coalton-toplevel

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
          (data-points (fromint (+ (.spam-count feature) (.ham-count feature)))))
      (/ (+ (* weight assumed-probability)
            (* data-points basic-probability))
         (+ weight data-points))))

  (declare bayesian-spam-probability (FeatureDatabase -> WordFeature -> Double-Float))
  (define bayesian-spam-probability (%bayesian-spam-probability 0.5d0 1))

  (declare inverse-chi-square (Double-Float -> Integer -> Double-Float))
  (define (inverse-chi-square value degrees-of-freedom)
    (lisp :a (degrees-of-freedom)
      (cl:assert (cl:evenp degrees-of-freedom)))
    (let m = (/ value 2))
    (min
     (let ((sum (c:new (exp (negate m))))
           (prob (c:new (exp (negate m)))))
       (when (> (inexact/ degrees-of-freedom 2) 1)
         (for i in (range 1 (1- (/ (fromint degrees-of-freedom) 2.0d0)))
           (c:update! (* (/ m i)) prob)
           (c:update! (+ (c:read prob)) sum)
           Unit))
       (c:read sum))
      1.0d0)))

(coalton-toplevel
  (declare fisher ((List Double-Float) -> Integer -> Double-Float))
  (define (fisher probs number-of-probs)
    (inverse-chi-square
     (* -2 (fold (fn (sum prob)
                   (+ sum (ln prob)))
                 0d0
                 probs))
     (* 2 number-of-probs)))

  (declare score (FeatureDatabase -> (List WordFeature) -> Double-Float))
  (define (score feature-db features)
    (let ((sum-probs
           (fn (remaining spam-probs ham-probs number-of-probs)
             (match remaining
               ((Nil)
                (let ((h (- 1 (fisher spam-probs number-of-probs)))
                      (s (- 1 (fisher ham-probs number-of-probs))))
                  (/ (+ (- 1 h) s) 2.0d0)))
               ((Cons feature new-remaining)
                (if (untrained? feature)
                    (sum-probs new-remaining spam-probs ham-probs number-of-probs)
                    (let ((spam-prob (bayesian-spam-probability feature-db feature)))
                      (sum-probs new-remaining
                                 (Cons spam-prob spam-probs)
                                 (Cons (- 1.0d0 spam-prob) ham-probs)
                                 (1+ number-of-probs)))))))))
      (sum-probs features Nil Nil 0))))

(coalton-toplevel

  (define-type-alias (SpamState :a) (ST FeatureDatabase :a))

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

  (declare classify (String -> (SpamState (Tuple Classification Double-Float))))
  (define (classify text)
    (do
     (word-features <- (extract-features text))
     (features-db <- get)
     (pure (classification (score features-db word-features)))))

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
   (run
     (do
       (train "Make money fast" Spam)
       (c1 <- (classify "Make money fast"))
       (let _ = (trace-tuple c1))
       (pure Unit))
     empty-database))
