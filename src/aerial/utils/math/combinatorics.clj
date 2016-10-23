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
;; Author: Jon Anthony                                                      ;;
;;                                                                          ;;
;;--------------------------------------------------------------------------;;
;;

(ns aerial.utils.math.combinatorics

  "Various supplementary math functions centering on combinatorics
   that are not supplied by clojure.math.combinatorics. Effectively an
   extension of that library."

  (:require
   [clojure.math.numeric-tower :as math]))


(set! *unchecked-math* :warn-on-boxed)
(defn next-index-combo
  "Generate, the next index combination of N things taken K at a time,
  from index combination COMBO. Uses persistent data
  structures (vectors). Works by spinning the larger indices faster."
  [combo, ^long n, ^long k]
  (let [n-k (long (- n k))
        k-1 (long (dec k))
        i (long (loop [i (long k-1)]
                  (let [Ci (long (combo i))]
                    (if (or (= i 0) (< Ci (+ n-k i))) i (recur (dec i))))))
        Ci (long (combo i))]
    (cond
      (and (= i 0) (>= Ci n-k)) nil ; done
      (or (= i k-1)                 ; No downstream elements
          (= (inc Ci) (+ n-k i)))   ; All elements from i to k-1 maxed
      (assoc combo i (inc Ci))
      :else
      (first (reduce (fn [[C, ^long i, ^long m] _]
                       [(assoc C i m) (inc i) (inc m)])
                     [combo i (inc Ci)] (range i k))))))
(set! *unchecked-math* false)

(defn index-combos
  "Generate all index combinations - in lexical graphic order and 0
  based - of N items taken K at a time. Uses persistent data
  structures (vectors), and returns a lazy-seq of vectors."
  [n k]
  (let [next (fn next [combo]
               (when (seq combo)
                 (cons combo
                       (lazy-seq (next (next-index-combo combo n k))))))]
    (next (into [] (range k)))))

(defn index-combosv
  "Eagerly generate all index combinations in same fashsion as
  index-combos (see for details).

  WARNING: for large examples where you do not use them all (for
  example, taking some subset) this could crash your program. See
  index-combos for a LAZY version!"
  [n k]
  (let [combo (into [] (range k))]
    (loop [combo combo
           combos [combo]]
      (let [combo (next-index-combo combo n k)]
        (if combo
          (recur combo (conj combos combo))
          combos)))))


(defn combins
  "Return the set of all K element _combinations_ (not permutations)
  formed from coll.  Coll does not need to be a set.  In particular,
  repeated elements are legal. Combinations are returned as a lazy seq
  of vectors, each vector a combination.

  In the three argument case, xfn is a transform function applied to
  each combination which are passed as vectors - see examples for a
  use case. The default is simply identity (used for the 2 argument
  signature).

   Examples:
   (combins 2 \"abcdef\")
   => ([\\a \\b] [\\a \\c] [\\a \\d] [\\a \\e] [\\a \\f] [\\b \\c]
       [\\b \\d] [\\b \\e] [\\b \\f] [\\c \\d] [\\c \\e] [\\c \\f]
       [\\d \\e] [\\d \\f] [\\e \\f])

   (combins #(apply str %) 2 \"AAGGGCGUGG\")
   => (\"AA\" \"AG\" \"AG\" \"AC\" \"AG\" \"AU\" \"AG\" \"AG\" \"AC\"
       \"AG\" \"AU\" \"GG\" \"GC\" \"GG\" \"GU\" \"GC\" \"GG\" \"GU\"
       \"CG\" \"CU\" \"GU\")"
  ([k coll]
   (combins identity k coll))
  ([xfn k coll]
   (let [v (vec coll)]
     (map #(xfn (mapv v %)) (index-combos (count coll) k)))))

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


(defn- perm-swap [perm i j]
  (assoc perm i (perm j) j (perm i)))

(set! *unchecked-math* :warn-on-boxed)
(defn next-index-perm
  "Generate next permutation from permutation PERM of size N. Uses
  Heap's algorithm."
  [^long n, ^long i, idx perm]
  (loop [i i
         idx idx]
    (if (= i n)
      nil
      (let [IDXi (long (idx i))]
        (if (< IDXi i)
          (let [k (if (even? i) 0 IDXi) ; (* (mod i 2) IDXi)
                perm (assoc perm k (perm i) i (perm k))] ; Swap
            [1, (assoc idx i (inc IDXi)), perm])
          (recur (inc i) (assoc idx i 0)))))))
(set! *unchecked-math* false)

(defn index-perms
  "Lazily generate all permutation indices, without repitition, of
  size N. Uses Heap's algorithm."
  [n]
  (let [next (fn next [[i idx perm]]
               (when perm
                     (cons perm
                           (lazy-seq (next (next-index-perm n i idx perm))))))]
    (next [1 (vec (repeat n 0)) (into [] (range n))])))


(defn perms
  "Permutation item generation without repetition over COLL. xfn is a
  transform function applied to each item set generated, and defaults
  to simply aggregating them in a vector.

  Examples:
  (perms \"ADN\")
  => [[\\A \\A] [\\A \\D] [\\A \\N] [\\D \\A] [\\D \\D]
      [\\D \\N] [\\N \\A] [\\N \\D] [\\N \\N]]
  (perms str \"ADN\")
  => [\"AA\" \"AD\" \"AN\" \"DA\" \"DD\" \"DN\" \"NA\" \"ND\" \"NN\"]"
  ([coll]
   (perms vec coll))
  ([xfn coll]
   (let [v (vec coll)]
     (map #(xfn (mapv v %)) (index-perms (count coll))))))
