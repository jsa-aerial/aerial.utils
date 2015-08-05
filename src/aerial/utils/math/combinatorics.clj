;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;          U T I L S . M A T H . C O M B I N A T O R I C S                 ;;
;;                                                                          ;;
;; Permission is hereby granted, free of charge, to any person obtaining    ;;
;; a copy of this software and associated documentation files (the          ;;
;; "Software"), to deal in the Software without restriction, including      ;;
;; without limitation the rights to use, copy, modify, merge, publish,      ;;
;; distribute, sublicense, and/or sell copies of the Software, and to       ;;
;; permit persons to whom the Software is furnished to do so, subject to    ;;
;; the following conditions:                                                ;;
;;                                                                          ;;
;; The above copyright notice and this permission notice shall be           ;;
;; included in all copies or substantial portions of the Software.          ;;
;;                                                                          ;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          ;;
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       ;;
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                    ;;
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE   ;;
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION   ;;
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION    ;;
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.          ;;
;;                                                                          ;;
;; Author: Jon Anthony & Shermin Pei                                        ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
;;

(ns aerial.utils.math.combinatorics

  "Various supplementary math functions centering on combinatorics
   that are not supplied by clojure.math.combinatorics. Effectively an
   extension of that library."

  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.math.combinatorics :as comb]))


;;; "Had to" bring this in from COMB name space as it was private, but
;;; needed to apply it here for index return version of combins
(defn index-combinations
  "Generates all n-tuples, as indices, over set cardinality cnt"
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
         iter-comb
         (fn iter-comb [c j]
           (if (> j n) nil
               (let [c (assoc c j (dec (c j)))]
                 (if (< (c j) j) [c (inc j)]
                     (loop [c c, j j]
                       (if (= j 1) [c j]
                           (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
         step
         (fn step [c j]
           (cons (rseq (subvec c 1 (inc n)))
                 (lazy-seq (let [next-step (iter-comb c j)]
                             (when next-step
                               (step (next-step 0) (next-step 1)))))))]
     (step c 1))))

;;; "Had to" change combins to directly use index-combinations for two
;;; return version case.
;;;
;;; Originally just (lazy-seq (map vec (comb/combinations coll k)))
(defn combins
  "Return the set of all K element _combinations_ (not permutations)
   formed from coll.  Coll does not need to be a set.  In particular,
   repeated elements are legal.

   Examples:
   (combins 2 \"abcdef\")
   => ([\\a \\b] [\\a \\c] [\\a \\d] [\\a \\e] [\\a \\f] [\\b \\c]
       [\\b \\d] [\\b \\e] [\\b \\f] [\\c \\d] [\\c \\e] [\\c \\f]
       [\\d \\e] [\\d \\f] [\\e \\f])

   (map #(apply str %) (combins 2 \"AAGGGCGUGG\"))
   => (\"AA\" \"AG\" \"AG\" \"AC\" \"AG\" \"AU\" \"AG\" \"AG\" \"AC\"
       \"AG\" \"AU\" \"GG\" \"GC\" \"GG\" \"GU\" \"GC\" \"GG\" \"GU\"
       \"CG\" \"CU\" \"GU\")
  "
  [k coll & [indices]]
  (let [v (vec (reverse coll))]
    (if (zero? k) (list [])
        (let [cnt (count coll)]
          (cond (> k cnt) nil
                (= k cnt) (list (vec coll))
                ;; Just the sets
                (not indices)
                (map #(vec (map v %)) (index-combinations k cnt))

                :else ; Sets and their indices
                [(map #(vec (map v %)) (index-combinations k cnt))
                 (combins k (range cnt))]
                )))))

(defn choose-k
  "Synonym for combins"
  [k coll]
  (combins k coll))


(defn n!
  "For positive integer N, compute N factorial."
  [n]
  {:pre [(integer? n) (> n -1)]}
  (reduce *' (range n 1 -1)))

(defn n-k!
  "For positive integers N and K, compute (n-k)!"
  [n k]
  {:pre [(integer? n) (integer? k) (> n -1) (<= 0 k n)]}
  (if (= n k)
    1
    (n! (-' n k))))

(defn nCk
  "For positive integers N and K, compute N choose K (binomial
   coefficient): n!/((n-k)!k!)"
  [n k]
  {:pre [(integer? n) (integer? k) (> n -1) (<= 0 k)]}
  (if (< n k)
    0
    (/ (reduce *' (range n (-' n k) -1))
       (n! k))))

#_
(defn nchoosek
  "Shermin idea for neater implementation, but at present about 1/2 as
   fast...
   "
  [n k]
  (let [f (fn[n k]
            (reduce *' (take k (range n 1 -1))))]
    (/ (f n k) (f k k))))


