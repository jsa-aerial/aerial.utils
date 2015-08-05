;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                         U T I L S . M A T H                              ;;
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

(ns aerial.utils.math

  "Various math functions that don't quite have enough totality to
   have their own home namespace just yet."

  (:require [clojure.math.numeric-tower :as math]
            [aerial.utils.coll :refer [transpose reducem take-until]]))


;;; -----------------------------------------------------------------
;;; Various ancillary math/arithmetic stuff.

(defn div
  "Integer division.  Return [q r], such that floor(n / d) * q + r = q"
  [n d]
  (let [q (math/floor (/ n d))]
    [q (rem n d)]))


(defn sum
  "Return the sum of the numbers in COLL.  If COLL is a map, return
   the sum of the (presumed all numbers) in (vals coll).  For function
   F versions, return the sum x in COLL (f x) or sum x in COLL1, y in
   COLL2 (f x y) or sum x1 in C1, x2 in C2, ... xn in Cn (f x1 ... xn).

   By default, summation proceeds on the results of F applied over the
   _cross product_ of the collections.  If summation should proceed
   over the collections in parallel, the first \"coll\" given should
   be the special keyword :||.  If given this causes F to be applied
   to the elements of colls as stepped in parallel: f(coll1[i]
   coll2[i] .. colln[i]), i in [0 .. (count smallest-given-coll)].

   Examples:

   (sum + [1 2 3] [1 2 3])
   => 36 ; sum of all [1 2 3] X [1 2 3] pairwise sums
   (sum + :|| [1 2 3] [1 2 3])
   => 12 ; sum of [(+ 1 1) (+ 2 2) (+ 3 3)]

   (sum (fn[x y] (* x (log2 y))) :|| [1 2 3] [1 2 3])
   => 6.754887502163469
  "
  ([coll]
     (let [vs (if (map? coll) (vals coll) coll)]
       (apply +' vs)))
  ([f coll]
     (reducem f +' coll))
  ([f coll1 coll2 & colls]
     (let [colls (cons coll2 colls)]
       (apply reducem f +' coll1 colls))))


(defn cumsum
  "Cumulative sum of values in COLL.  If COLL is a map, return the
   cumulative sum of the (presumed all numbers) in (vals coll).  For
   function F versions, return the cumulative sum of x in COLL (f x)
   or the cumulative sum of x1 in COLL1, x2 in COLL2, ... xn in
   COLLn (f x1 ... xn).

   The cumulative sum is the seq of partial sums across COLL treated
   as a vector for each (i (range (count COLL))), effectively:

   [(coll 0) (sum (subvec coll 0 2)) .. (sum (subvec coll 0 (count coll)))]

   Except actual computational time is linear.
  "
  ([coll]
     (let [cv (vec (if (map? coll) (vals coll) coll))
           c0 (cv 0)]
       (first
        (reduce (fn[[v s] x]
                  (let [s (+ s x)]
                    [(conj v s) s]))
                [[c0] c0] (rest cv)))))
  ([f coll]
     (cumsum (map f coll)))
  ([f coll1 coll2 & colls]
     (cumsum (apply map f coll1 coll2 colls))))


(defn prod
  "Return the product of the numbers in COLL.  If COLL is a map,
   return the product of the (presumed all numbers) in (vals coll).
   For function F versions, return the product of x in COLL (f x) or
   prod x in COLL1, y in COLL2 (f x y) or prod x1 in C1, x2 in C2,
   ... xn in Cn (f x1 ... xn).

   By default, multiplication proceeds on the results of F applied
   over the _cross product_ of the collections.  If multiplication
   should proceed over the collections in parallel, the first \"coll\"
   given should be the special keyword :||.  If given, this causes F
   to be applied to the elements of colls as stepped in parallel:
   f(coll1[i] coll2[i] .. colln[i]), i in [0 .. (count smallest-coll)].

   Examples:

   (prod + [1 2 3] [1 2 3])
   => 172800 ; product of all [1 2 3] X [1 2 3] pairwise sums
   (prod + :|| [1 2 3] [1 2 3])
   => 48 ; product of [(+ 1 1) (+ 2 2) (+ 3 3)]
  "
  ([coll]
     (let [vs (if (map? coll) (vals coll) coll)]
       (apply *' vs)))
  ([f coll]
     (reducem f *' coll))
  ([f coll1 coll2 & colls]
     (let [colls (cons coll2 colls)]
       (apply reducem f *' coll1 colls))))


(defn sqr
  "Square x, i.e., returns (* x x)"
  [x]
  (*' x x))


(defn logb
  "Return the log to the base b _function_ of x"
  [b]
  (let [lnb (Math/log b)]
    (fn[x] (/ (Math/log x) lnb))))

(defn log
  "Return the natural log of x"
  [x]
  (Math/log x))

(defn ln
  "Return the natural log of x"
  [x]
  (Math/log x))

(def
 ^{:doc
   "Named version of (logb 2).  Important enough to have a named top
    level function"
   :arglists '([x])}
 log2 (logb 2))

(def
 ^{:doc
   "Named version of (logb 10).  Important enough to have a named top
    level function"
   :arglists '([x])}
 log10 (logb 10))


(defn logistic
  "Compute logistic of X (a number which will be cast as a double.
   Returns 1.0 / (1.0 + e^-x) = e^x / (e^x + 1.0)
  "
  [x]
  (let [ex (math/expt Math/E (double x))]
    (/ ex (+ ex 1.0))))

(defn logit
  "Compute the log odds of P an element of interval [0..1].  LOGFN is
   the logarithmic function to use and defaults to log2, log base 2.
   Other directly available log functions are ln (or simply log) and
   log10; for any other base see logb which can generate a log
   function of base b. Returns logfn (p / (1 - p)), the inverse of
   logistic.
  "
  [p & {:keys [logfn] :or {logfn log2}}]
  (logfn (/ p (- 1 p))))


(defn primes
  "RHickey paste.lisp.org with some fixes by jsa. Returns a list of
   all primes from 2 to n.  Wicked fast!"
  [n]
  (if (< n 2)
    ()
    (let [n (long n)]
      (let [root (long (Math/round (Math/floor (Math/sqrt n))))]
        (loop [i (long 3)
               a (int-array (inc n))
               result (list 2)]
          (if (> i n)
            (reverse result)
            (recur (+ i (long 2))
                   (if (<= i root)
                     (loop [arr a
                            inc (+ i i)
                            j (* i i)]
                       (if (> j n)
                         arr
                         (recur (do (aset arr j (long 1)) arr)
                                inc
                                (+ j inc))))
                     a)
                   (if (zero? (aget a i))
                     (conj result i)
                     result))))))))


(def +prime-set+ (atom (primes 1000)))

(defn prime-factors
  "Return the prime factorization of num as a seq of pairs [p n],
   where p is a prime and n is the number of times it is a factor.
   Ex: (prime-factors 510) => [[2 1] [3 1] [5 1] [17 1]]
  "
  [num]
  (if (< num 2)
    num
    (do
      (when (> num (last @+prime-set+))
        (swap! +prime-set+ (fn[_](primes (+ num (long (/ num 2)))))))
      (loop [ps (take-until #(> % num) @+prime-set+)
             factors []]
        (if (empty? ps)
          factors
          (let [pf (loop [n num
                          cnt 0]
                     (let [f (first ps)
                           [q r] (div n f)]
                       (if (not= 0 r)
                         (when (> cnt 0) [f cnt])
                     (recur q (inc cnt)))))]
            (recur (rest ps)
                   (if pf (conj factors pf) factors))))))))




;;; -----------------------------------------------------------------
;;; Simple vector stuff.  dot product, norm, distances, and such.  Is
;;; all this stuff in Incanter??


(defn dot [v1 v2]
  (double (reduce + 0.0 (map * v1 v2))))

(defn norm [v]
  (math/sqrt (dot v v)))

(defn vhat [v]
  (let [n (norm v)] (vec (map #(/ % n) v))))

(defn cos-vangle [v1 v2]
  (dot (vhat v1) (vhat v2)))

(defn vangle-dist [v1 v2]
  (math/abs (dec (cos-vangle v1 v2))))

(defn vecdist [v1 v2]
  (let [v (map - v1 v2)] (math/sqrt (dot v v))))

(defn vecmean
  ([v1 v2]
     (map #(/ (+ %1 %2) 2) v1 v2))
  ([vs]
     (let [d (count vs)]
       (map (fn[xs] (/ (apply + xs) d))
            (transpose vs)))))

