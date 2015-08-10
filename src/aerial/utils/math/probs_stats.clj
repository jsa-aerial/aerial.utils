;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                 U T I L S . P R O B S - S T A T S                        ;;
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

(ns aerial.utils.math.probs-stats

  "Various frequency, combinatorial, probability, statistical,
   measure, and metrics for a variety of sequence and string data."

  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [clojure.set :as set]

            [aerial.utils.misc
             :refer [raise]]
            [aerial.utils.coll
             :refer [vfold coalesce-xy-yx reducem]]
            [aerial.utils.string
             :refer [string-less?]]
            [aerial.utils.math
             :refer [sum sqr]]
            [aerial.utils.math.combinatorics
             :refer [combins nCk n!]]
            ))


;;; -----------------------------------------------------------------
;;; Various frequency counts, combinations and combinators,
;;; probabilities, similarity coefficients, corresponding difference
;;; fns, ngram operations, edit distances, etc.

(defn letter-pairs [n s]
  (set (map #(apply str %) (partition n 1 s))))

(defn word-letter-pairs [n s]
  (reduce (fn[m s] (set/union m (letter-pairs n s))) #{} (str/split s #" ")))

(defn keysort [map]
  (sort #(string-less? (key %1) (key %2)) map))


(defn- freqm-probm
  "Helper for some functions that can't make direct use of other
   higher level capabilities.  Mostly (only?) needed when there needs
   to be a choice between symmetric and non symmetric frequency
   distributions.
  "
  [freq-map]
  (let [fs freq-map
        sz (double (sum fs))]
    (reduce (fn[m [k v]] (assoc m k (double (/ v sz))))
            {} fs)))


(defn freqn
  "Frequencies of n-grams (aka l-mers \"features\", et. al.) in
   collection COLL treated as a sequence.  n is the \"window\" or
   resolution width.  Slide is always fixed at 1 (one) position.

   Ex: (freqn 2 \"acagtcaacctggagcctggt\")
   =>
   {\"aa\" 1, \"cc\" 2, \"gg\" 2, \"ac\" 2, \"ag\" 2, \"gt\" 2,
   \"tc\" 1, \"ct\" 2, \"tg\" 2, \"ga\" 1, \"gc\" 1, \"ca\" 2}
  "
  [n coll]
  (if (= 1 n)
    (frequencies (seq coll))
    (loop [s (seq coll)
           res (transient {})]
      (let [k (apply str (take n s))]
        (if (>= (count s) n)
          (recur (rest s)
                 (assoc! res k (inc (get res k 0))))
          (persistent! res))))))

(defn freqs&probs
  "Generate frequencies of items in coll by means (freq-fn n coll),
   take those and compute the items' corresponding probabilities.
   Return triple: [freqs probs cnt], where freqs and probs are maps
   keyed by item and cnt is the total count of all items.  Lower level
   functional base for more public functions freqs-probs, et.al.
  "
  [n coll freq-fn]
  (let [freqs (freq-fn n coll)
        sz (double (sum (vals freqs)))
        probs (reduce (fn[m [k v]]
                        (assoc m k (double (/ v sz))))
                      {} freqs)]
    [freqs probs sz]))

(defn freqs-probs
  "freqs&probs for n and coll with freq-fn equal freqn"
  [n coll]
  (freqs&probs n coll freqn))

(defn probs
  "Probabilities for items from coll taken n at a time (see freqn) or
   as determined by the frequency dist map freq-dist-map."
  ([n coll]
     (second (freqs-probs n coll)))
  ([freq-dist-map]
     (second (freqs&probs 0 freq-dist-map (fn[n coll] coll)))))


(defn alphabet2
  "Return the alphabet for coll.  If coll is a map, return it's keys.
   If a set, return coll, otherwise return alphabet generated over
   coll by freqn with size n."
  [coll & {n :n :or {n 1}}]
  (cond
   (map? coll) (keys coll)
   (set? coll) coll
   :else
   (keys (freqn n coll))))


(defn cc-freqs
  "Frequencies of items in each collection in the collection of
   collections colls.  Uses freq-fn to generate and count items.  Set
   PAR to parallelize when (count colls) is large and each (freq-fn
   c), c in colls, is expensive.  Returns a seq of frequency maps, one
   for each collection in colls.
  "
  [n colls freq-fn & {:keys [par] :or {par false}}]
  (let [mfn (if par vfold map)]
    (mfn #(freq-fn n %) colls)))

(defn cc-tfreqs
  "Same as cc-freqs, but also compute total item frequencies reduced
   over all collections in colls.  Returns pair [ccfreqs cctfreqs],
   where ccfreqs is the seq of frequency maps per cc-freqs and
   cctfreqs is the single map for all items.
  "
  [n colls freq-fn & {par :par :or {par 1}}]
  (let [ccfreqs (cc-freqs n colls freq-fn :par par)
        cctfreqs (reduce
                  (fn[m cfreq]
                    (merge-with #(+ %1 %2) m cfreq))
                  {} ccfreqs)]
    [ccfreqs cctfreqs]))

(defn cc-freqs&probs
  "For each C in colls (a collection of collecions), compute the
   frequency and probability map for C and its total item count as
   given by freqs&probs-fn (for example, combins-freqs-probs).  Using
   these maps, compute the total item frequency and probablity maps
   and total count over all collections in colls.  Low level
   functional base for public functions such as cc-freqs-probs,
   et. al.

   For large (count colls) with expensive freqs&probs-fn, set par to
   parallelize computation via vfold (and fork/join) using auto
   partitioning

   Return [ccfs&ps allfs allps tcount], where

   ccfs&ps is a seq of triples [fs ps cnt], for each C in colls,
   allfs is the map of freqs over all Cs,
   allps is the map of probs over all Cs and
   tcount is the total items over coll.
  "
  [n colls freqs&probs-fn & {:keys [par] :or {par false}}]
  (let [mfn (if par vfold map)
        ccfs&ps (mfn #(freqs&probs-fn n %) colls)
        allfs (reduce (fn[m xyz] (merge-with + m (first xyz))) {} ccfs&ps)
        sz (sum allfs)
        allps (reduce (fn[m [k v]] (assoc m k (double (/ v sz)))) {} allfs)]
    [ccfs&ps allfs allps sz]))



(defn cc-freqn
  "cc-freqs for freqn - use freqn over colls"
  [n colls & {par :par :or {par 1}}]
  (cc-freqs n colls freqn :par par))

(defn cc-tfreqn
  "cc-tfreqs for freqn, cc-freqs with freqn plus overall totals."
  [n colls & {par :par :or {par 1}}]
  (cc-tfreqs n colls freqn :par par))

(defn cc-freqs-probs
  "cc-freqs&probs using freqn as base frequency function"
  [n colls & {par :par :or {par 1}}]
  (cc-freqs&probs n colls #(freqs&probs %1 %2 freqn) :par par))


(defn combin-count-reduction
  "Performs (freqn 1 item-coll), giving result M.  If sym is true,
   coalesces items in M whose keys are reversible by retaining one key
   with the sum of the values of previouis two.  See coalesce-xy-yx
   for complete description.  This was originally a one off
   specifically for coalescing combination sets (see combins), hence
   the (now bogus) name.
  "
  [item-coll sym]
  (let [fm (freqn 1 item-coll)]
    (if sym
      (coalesce-xy-yx fm (fn[x v] (if (not v) 0 (+ (val x) v))))
      fm)))


(defn combins-freqn
  "Compute frequency map for coll where items are formed by all
   choices of coll things taken n at a time: (combins n coll).  If SYM
   is true (the default), treat vec/seq keys as symmetric.  Examples:

   (combins-freqn
     2 \"UUAAAAAACAAAAAAAAAAAACAAAACACAAAAACAAAUUCUACAAAAAAUAAAAACA\")
   {[\\C \\C] 28, [\\A \\C] 352, [\\A \\A] 946, [\\U \\C] 48,
    [\\U \\A] 264, [\\U \\U] 15}

   (combins-freqn
     2 \"UUAAAAAACAAAAAAAAAAAACAAAACACAAAAACAAAUUCUACAAAAAAUAAAAACA\" :sym false
   {[\\C \\U] 23, [\\C \\C] 28, [\\C \\A] 149, [\\A \\U] 131,
    [\\A \\C] 203, [\\A \\A] 946, [\\U \\C] 25, [\\U \\A] 133, [\\U \\U] 15}
  "
  [n coll &
   {sym :sym :or {sym true}}]
  (combin-count-reduction (combins n coll) sym))

(defn choose-k-freqn
  "Synonym for combins-freqn"
  [n coll]
  (combins-freqn n coll))

(defn cc-combins-freqn
  "combins-freqn over all collections C in colls.  Basically cc-freqs
   with freq-fn equal combins-freqn"
  [n colls & {par :par :or {par 1}}]
  (cc-freqs n colls combins-freqn :par par))

(defn combins-freqs-probs
  "freqs&probs with freq-fn equal to combins-freqn.  So, triple with
  fs and ps maps based on combinations of coll items taken n at time,
  i.e., keys are such items, with cnt element (nCk (count coll) n)."
  [n coll]
  (freqs&probs n coll combins-freqn))

(defn cc-combins-freqs-probs
  "cc-freqs&probs with freqs&probs-fn equal to combins-freqs-probs.
   So, computes freqs and probs for each C in colls based on
   combinations of coll items taken n at a time (the keys for the
   maps).  Returns [ccfsps allfs allps cnt], where ccfsps is the seq
   of triples for each C in colls, allfs is the map for all freqs,
   allps is corresponding map for all probs and cnt is total count of
   all elements.
  "
  [n colls &
   {par :par :or {par 1}}]
  (cc-freqs&probs n colls combins-freqs-probs :par par))


(defn num-key-freq-map?
  "Returns true if X is a number keyed frequency map"
  [x]
  (and (map? x) (number? (ffirst x))))

(defn pair-coll?
  "Returns true if X is a collection of number pairs."
  [x]
  (or (num-key-freq-map? x)
      (and (sequential? (first x))
           (= (count (first x)) 2)
           (number? (ffirst x)))))

(defn flatten-pair-coll
  "COLL is a collection of pairs [v w] where v is a value and w its
   weight. For maps, keys are vs and values are ws.  Returns a lazy
   seq of of the expansions as a single collection, with w repetitions
   of v for each such [v w] pair.
  "
  [coll] ;m {4 1 2 1 3 1 1 1}
  (mapcat (fn[[v w]] (repeat w v)) coll))


(defn- mean-freq-map
  "Helper for mean when it is passed a frequency map with keys numbers"
  [m]
  (let [[n d] (reduce (fn [v [val n]]
                        [(+ (first v) (* val n))
                         (+ (second v) n)])
                      [0 0] m)]
    (/ n d)))

(defn mean
  "Compute the expectaion of the collection COLL.  If coll is a number
   keyed frequency map, computes as if seq of expansion of keys
   weighted by frequency.  If coll is a simple map use (vals coll).
   Also coll can be a collection of pairs [n w], where w is the weight
   to assign to n.  Lastly, if single numbers are given, their weight
   is taken as simply 1.  Effectively returns:

     let c* (flatten-pair-coll coll)
         cnt (count c*)
       (/ (sum c*) cnt)

   But without computing actual weighted sequence c*.
  "
  ([coll]
     (if (empty? coll)
       (raise :type :invalid-operation
              :msg "Mean of empty set not defined"
              :coll coll)
       (double
        (if (pair-coll? coll)
          (mean-freq-map coll)
          (let [coll (if (map? coll) (vals coll) coll)]
            (/ (sum coll)
               (count coll)))))))
  ([x & xs]
     (mean (cons x xs))))

(defn median
  "Compute the median of the given collection COLL.  If coll is a
   collection of number pairs [v w], where w is the weight for v,
   effectively computes its median based on expansion of keys weighted
   by frequency.  This includes frequency maps where keys are numbers
   and values are their weights.  If coll is a simple map computes the
   median of (vals coll).
  "
  ([coll]
     (if (empty? coll)
       (raise :type :invalid-operation
              :msg "Meadian of empty set not defined"
              :coll coll)
       (if (pair-coll? coll)
         (median (flatten-pair-coll coll))
         (let [coll (if (map? coll) (vals coll) coll)
               cnt (count coll)
               cnt2 (/ cnt 2)
               v (vec (sort coll))]
           (if (even? cnt)
             (/ (+ (v (dec cnt2)) (v cnt2)) 2.0)
             (v (math/floor cnt2)))))))
  ([x & xs]
     (median (cons x xs))))


(defn variance
  "Compute the variance of the collection COLL.  If coll is a map
   use (vals coll).  Returns average of squared differences of xs in
   coll with 'mean' of coll.  The functions distfn and avgfn are used
   for distances of points in coll and the averaging function.  Or,
   the 'mean' can be given explicitly as M.

   The rfn function is the 'reducer' function to use for calculating
   the set of squared differences.  It defaults to 'map', but the user
   can change this to some variant of fold, most typically vfold, to
   parallelize the computation for large collections and/or expensive
   distfns (such as various RE functions).

   The single parameter case uses '-' as distfn and 'mean' as avgfn
   and 'map' as the reducer.
  "
  ([coll]
     ;; Uses Var(X) = E(sqr X) - (sqr E(X))
     (- (if (pair-coll? coll)
          (mean (reduce (fn [m [v w]] (assoc m (sqr v) w)) {} coll))
          (mean (map sqr coll)))
        (sqr (mean coll))))
  ([coll & {:keys [distfn avgfn m rfn] :or {distfn - avgfn mean rfn map}}]
     (let [coll (if (map? coll) (vals coll) coll)
           m (if m m (avgfn coll))]
       (mean (rfn #(sqr (distfn % m)) coll)))))

(defn avg-variance
  "Compute the average of the n variances for the n samples in
   SAMPLE-SET.  This is not as obvious as it may at first seem.  It is
   _not_ the mean of the variances of each sample.  The correct
   averaging is the average of the weighted variances of each sample,
   where the weighting is the sample size:

   (/ (sum (fn[Si] (dec (count Si)) (variance Si)) sample-set)
      (- (sum count sample-set) (count sample-set)))

   This does reduce to the mean of the variances if the samples are
   all of the same size, but this is often (typically?) not the case.

   DISTFN and AVGFN are as for the function variance (for which, see)
  "
  ([sample-set]
     (/ (sum (fn[Si]
               (* (dec (count Si))
                  (variance Si)))
             sample-set)
        (- (sum count sample-set)
           (count sample-set))))
  ([sample-set & {:keys [distfn avgfn m] :or {distfn - avgfn mean}}]
     (/ (sum (fn[Si]
               (* (dec (count Si))
                  (variance Si :distfn distfn :avgfn avgfn)))
             sample-set)
        (- (sum count sample-set)
           (count sample-set)))))

(defn covariance
  "For populations/sample sets X and Y, compute the covariance of X and Y,
   cov(X,Y). The size of X and Y must be the same. Provides a
   'measure' of how X and Y 'follow' one another or change together.
   Do they both go up and down together, vice versa, or something in
   between.

     let N (count X)
         mux (mean X)
         muy (mean Y)
      (sum (fn[xi yi] (/ (* (- xi mux) (- yi muy)) (dec N))) X Y)

      (the (dec N) would be just N, if using true populations)

      which, by some algebra, is E(XY) - E(X)E(Y).

   Note: If X and Y are independent, E(XY) = E(X)E(Y) and so
   covariance must be 0.  If X=Y, cov(X,X) = E(X^2)-E(X)^2 = var(X).

   Note: a particular useful application is for the variance of a
   linear combination of X & Y:

    var(aX + bY) = a^2*E(X^2)+2ab*E(XY)+b^2*E(Y^2) -
                   a^2*E(X)^2+2ab*E(X)E(Y)+b^2*E(Y)^2

                 = a^2*(E(X^2)-E(X)^2) +
                   b^2*(E(Y^2)-E(Y)^2) +
                   2ab*(E(XY)-E(X)E(Y))

                 = a^2*var(X)+b^2*var(Y)+2ab*cov(X,Y)

   Note: cov is very dependent on the sample matching of X and Y, so
   be sure X and Y are in the correct order on call:

    (covariance [1 2 3 4 5] [1 2 3 4 5])
    => 2.0
    (covariance [1 2 3 4 5] [2 1 4 3 5])
    => 1.6
    (covariance [1 2 3 4 5] [5 4 3 2 1])
    => -2.0

   Bugs: not an unbiased estimator of population variance.
  "
  [X Y]
  (let [getvs (fn[X] (cond (pair-coll? X) (flatten-pair-coll X)
                           (map? X) (vals X)
                           :else X))
        xs (getvs X)
        ys (getvs Y)]
    (if (not= (count xs) (count ys))
      (raise :type :invalid-operation
             :msg "Covariance of X & Y requires = size sample sets"
             :X X :Y Y)
      (- (mean (map * xs ys))
         (* (mean xs) (mean ys))))))


(declare std-deviation)

(defn correlation
  "For population/sample sets X and Y, compute the correlation between
   X and Y, cor(X,Y).  The size of X and Y must be the same.  Provides
   a 'measure' of the strength and connection of a linear relationship
   beween X and Y.  Bounded by -1 (anti correlation) and 1 (perfect
   correlation), i.e., cor(X,Y) in [-1, 1].  A value of zero indicates
   no linear correlation, but does not mean independence, however
   independence will produce a zero value.

     cor(X,Y) = (/ cov(X,Y) (* (std-deviation X) (std-deviation Y)))

   Defined only if both standard deviations are nonzero.

   Also known as Pearson correlation coefficient, Pearson
   product-moment correlation, or simply Pearson correlation.

   Correlation is a normalized covariance.
  "
  [X Y]
  (let [prod-stdevs (* (std-deviation X) (std-deviation Y))]
    (if (zero? prod-stdevs)
      (raise :type :invalid-operation
             :msg "Correlation only defined for finite nonzero stdevs"
             :X X :Y Y
             :stdevX (std-deviation X)
             :stdevY (std-deviation Y))
      (/ (covariance X Y)
         prod-stdevs))))

(defn pearson-correlation
  "Often used synonym of correlation.  See correlation for details."
  [X Y]
  (correlation X Y))


(defn std-deviation
  "Compute the standard deviation of collection COLL.  If coll is a
   map uses (vals coll).  Returns the sqrt of the variance of
   coll. The functions distfn and avgfn are used for distances of
   points in coll and the averaging function for the mean of
   coll. These are used to compute the variance, or if V is given it
   is taken as the variance and variance computation is skipped.  For
   the single parameter case, distfn is '-' and avgfn is 'mean'.
  "
  ([coll]
     (math/sqrt (variance coll)))
  ([coll & {:keys [distfn avgfn v] :or {distfn - avgfn mean}}]
     (math/sqrt (if v v (variance coll :distfn distfn :avgfn avgfn)))))

(defn avg-std-deviation
  "Compute the average of the n standard deviations for the n samples
   in SAMPLE-SET.  This is not as obvious as it may at first seem.  It
   is _not_ the mean of the stdevs of each sample.  The correct
   averaging is the sqrt of the average of the weighted variances of
   each sample, where the weighting is the sample size.  This latter
   is given by avg-variance (see its documentation for details).  So,
   we return:

   (sqrt (avg-variance sample-set))
   "
  ([sample-set]
     (math/sqrt (avg-variance sample-set)))
  ([sample-set {:keys [distfn avgfn v] :or {distfn - avgfn mean}}]
     (math/sqrt (avg-variance sample-set :distfn distfn :avgfn avgfn))))


(defn joint-prob-x
  "Xperimental.  Not sure how to make this work in general while still
   being useful.  Intent would be to supply sample space, random
   variables X & Y (as functions - including maps & vectors), that
   obey and/or define a constraint CONSTR, which may involve further
   random variables.  Generate all joint occurances and from there the
   joint probabilities.
  "
  [Omega X Y & {constr :constr :or {constr true}}]
  (let [d (nCk 8 4)]
    (for [x (range 0 4) y (range 0 3)
          :let [z (- 4 x y)]
          :when (and (<= 0 z 3)
                     (= (+ x y z) 4))]
      [x y z (double (/ (* (nCk 3 x) (nCk 2 y) (nCk 3 z)) d))])))

(defn joint-probability
  "Given a set of collections c1, c2, c3, .. cn, and combinator, a
   function of n variables which generates joint occurances from {ci},
   returns the joint probability distribution.  If sym? is true
   coalesce reversable element occurances.

   Ex: (apply joint-probability
              transpose true (take 2 (seq/rotations \"GGCGGAAGACCGCCUCGA\")))
   => {[\\U \\C] 0.11111111, [\\A \\G] 0.2777778, [\\C \\G] 0.2777778,
       [\\A \\C] 0.055555556, [\\A \\A] 0.055555556, [\\G \\G] 0.11111111,
       [\\C \\C] 0.11111111}

   Ex: (joint-probability #(combins 2 %) true \"GGCGGAAGACCGCCUCGA\")
   => {[\\C \\C] 0.09803921568627451, [\\G \\G] 0.13725490196078433,
       [\\A \\A] 0.0392156862745098, [\\A \\C] 0.1568627450980392,
       [\\C \\G] 0.27450980392156865, [\\A \\G] 0.1830065359477124,
       [\\U \\A] 0.026143790849673203, [\\G \\U] 0.0457516339869281,
       [\\U \\C] 0.0392156862745098}

   Ex: (joint-probability
         (fn[X Y]
           (for [x X]
             (if (even? x)
                 [:e (.charAt Y 0)]
                 [(second (div x 3)) (rand-nth (rest Y))])))
         false
         (take 100 (iterate inc 0)) \"abcde\")
   => {[2 \\b] 0.04, [1 \\b] 0.05, [2 \\c] 0.04, [0 \\b] 0.07, [1 \\c] 0.05,
       [2 \\d] 0.04, [0 \\c] 0.05, [1 \\d] 0.05, [2 \\e] 0.04, [0 \\d] 0.04,
       [1 \\e] 0.02, [0 \\e] 0.01, [:e \\a] 0.5}
   "
  ([combinator sym? coll]
     (freqm-probm (combin-count-reduction (combinator coll) sym?)))
  ([combinator sym? coll1 coll2]
     (freqm-probm (combin-count-reduction (combinator coll1 coll2) sym?)))
  ([combinator sym? coll1 coll2 & colls]
     (freqm-probm (combin-count-reduction
                   (apply combinator (conj colls coll1 coll2)) sym?))))

(defn JPSxy
  "Joint Probability Symmetric: joint-probability with sym? true"
  ([combinator coll]
     (joint-probability combinator true coll))
  ([combinator coll & colls]
     (apply joint-probability combinator true (cons coll colls))))

(defn JPxy
  "Joint Probability non symmetric: joint-probability with sym? false"
  ([combinator coll]
     (joint-probability combinator false coll))
  ([combinator coll & colls]
     (apply joint-probability combinator false (cons coll colls))))


(defn cond-probability
  "Given collections coll1 and coll2, and combinator, a function of 2
   variables which generates joint occurances from coll1 & coll2,
   returns the conditional probability distributions for coll1
   conditioned on elements of coll2.  If sym? is true coalesce
   reversable element occurances during joint occurance
   computation.

   Alternatively, in the distribution version, PXY is a joint
   distribution in map form, and PY is a distribution in map form.
   That is, in this variant, the probability distributions are
   explicitly provided.

   OCCURS? is a function of two variables, pkx and pkxy, where pkx is
   a key derived from coll2/PY and pkxy is a key derived from the
   combinator map of coll1 with coll2 or a key in PXY.  It returns
   whether pkx occurs in pkxy.

   cond-probability returns a map of maps, where each inner map is a
   distribution of coll1 given ei in coll2:

   {{ei {P(COLL1)|ei}} ei in COLL2.

   Use pXY|y to obtain any given distribution, where y = some ei.

   Examples:

   (cond-probability
     transpose false #(= %1 (.charAt %2 1)) \"defdeef\" \"abcaabb\")
   => {\\a {\"ea\" 0.3333333333333333, \"da\" 0.6666666666666666},
       \\b {\"fb\" 0.3333333333333333, \"eb\" 0.6666666666666666},
       \\c {\"fc\" 0.9999999999999997}}

   (cond-probability
     transpose false #(= %1 (.charAt %2 1)) \"abcaabb\" \"defdeef\")
   => {\\d {\"ad\" 1.0},
       \\e {\"ae\" 0.3333333333333333, \"be\" 0.6666666666666666},
       \\f {\"bf\" 0.5, \"cf\" 0.5}}
  "
  ([PXY PY occurs?]
     (reducem
      (fn[[pkx pvx] [pkxy pvxy]]
        #_(prn :f pkx pvx pkxy pvxy)
        (if (occurs? pkx pkxy)
          {pkx {pkxy  (/ pvxy pvx)}}
          {}))
      (fn
        ([] {})
        ([m subm]
           #_(prn :FR m subm)
           (merge-with into m subm)))
      PY PXY))

  ([combinator sym? occurs? coll1 coll2]
     (let [PXY (joint-probability combinator sym? coll1 coll2)
           PY  (probs 1 coll2)]
       (cond-probability PXY PY occurs?))))


(defn pXY|y
  "Given a total conditional distribution XY (see cond-probability),
   return the distribution of XY with respect to y.  XY should be a
   map of maps of the form returned from cond-probability.  Typically,
   this would be used in conjunction with cond-probability, but need
   not be as long as the form of the map of maps is the same, where
   each inner map is the joint distribution conditioned on an element.

   Ex:

   (pXY|y (cond-probability transpose false #(= %1 (.charAt %2 1))
                            \"defdeef\" \"abcaabb\")
          \\a)
  => {\"ea\" 0.3333333333333333, \"da\" 0.6666666666666666}
  "
  [XY y] (XY y))


(defn binomial-pdf
  "Return the binomial probability mass/distribution _function_
   corresponding to P, the probability of a hit in some Bernoulli
   trial.  The resulting function returned, call it bnpdf, is a
   function of two parameters, the number of hits k (successes), and
   the number of trials N.  So, if the result of (binomial-pdf 1/4) is
   bound to bnpdf, then (bnpdf 3 10) would be the probability of
   getting 3 hits in 10 trials for sample probability 1/4.  A
   distribution of bnpdf can then be generated via some technique
   like (map #(bnpdf % 1000) (range 1 (inc 1000))).
  "
  [p]
  (let [q (- 1 (double p))]
    (fn[k N] (* (nCk N k) (math/expt p k) (math/expt q (- N k))))))

(defn binomial-dist
  "Binomial probability distribution. P is the probability of a hit
   and N is the number of trials.  Returns a seq of pairs [k pr],
   where k in (range 1 (inc N)) and pr is probability of getting k
   successes in N trials.
  "
  [N p]
  (if (= N 0)
    0.0
    (let [q (- 1 (double p))]
      (for [k (range 1 (inc N))]
        [k (* (nCk N k) (math/expt p k) (math/expt q (- N k)))]))))


(defn poisson-pdf
  "Return the Poisson probability mass/distribution _function_
   corresponding to mu, the mean or expected value of a random
   variable X over some time/space interval. In the Poisson case the
   mean is also the variance of X.

   The resulting function returned, call it pspdf, is a function of
   one parameter, the number of occurances K.  NOTE: pspdf is only
   defined for integer values.

   So, if the result of (poisson-pdf 1.0) is bound to pspdf,
   then (pspdf 3) would be the probability of getting 3 hits.  A
   distribution of pspdf can then be generated via some technique
   like: (map pspdf (range 1 (inc 100))).
  "
  [mu]
  (fn[k] {:pre [(integer? k)]}
    (/ (* (math/expt mu k) (math/expt Math/E (- mu)))
       (n! k))))

(defn poisson-dist
  "Poisson probability distribution. "
  [N mu]
  (for [k (range N)] ; effectively k-1
    [k (/ (* (math/expt mu k) (math/expt Math/E (- mu)))
          (n! k))]))

(defn poisson-cdf
  "Return the Poisson cumulative probability _function_ corresponding
   to mu, the mean or expected value of a random variable X over some
   time/space interval.  In the Poisson case, the mean is also the
   variance of X.

   The resulting function returned, call it pscdf, is a function of
   two parameters: lower limit l, and upper limit u, both >= 0.  l and
   u-1 are the minimum and maximum number of occurances, i.e., the
   interval is the half open [0, u) interval.

   Ex: If 2 bugs/week are introduced on average in statware, what is
   the probability there will be less than 5 bugs introduced next
   month.  So, mu = 2/week * 4 week = 8, l = 0 and u = 5:

   ((poisson-cdf 8) 0 5) => 0.0996 or about 10% chance.  Of course,
   those dudes aren't using Clojure! :)
  "
  [mu]
  (fn[l u] {:pre [(integer? l) (integer? u) (>= u l 0)]}
    (sum (map (poisson-pdf mu) (range l u)))))

(defn poisson-sample
  "Generate a \"random\" sample from a Poisson distribution
   characterized by mean/variance mu.  In the two parameter case,
   generate N such samples.
  "
  ([mu]
     (let [L (math/expt Math/E (- mu))]
       (loop [k 1 p 1.0]
         (if (<= p L)
           (- k 1)
           (recur (inc k) (* p (rand)))))))
  ([N mu]
     (for [k (range (inc N))]
       (poisson-sample mu))))


(defn geometric-pdf
  "Return the geometric probability mass/distribution _function_
   corresponding to p, the probability of a hit in some Bernoulli
   trial.

   The resulting function returned, call it gmpdf, is a function of
   one parameter, the number of occurances K.  NOTE: gmpdf is only
   defined for integer values.

   So, if the result of (geometric-pdf 1/4) is bound to gmpdf,
   then (gmpdf 3) would be the probability of getting a hit after 3
   tries.  A distribution of gmpdf can then be generated via some
   technique like:

   (map pspdf (range 10))
  "
  [p]
  (let [p (double p)
        q (- 1.0 p)]
    (fn[k] (* (math/expt q k) p))))

(defn geometric-dist
  "Geometric probability distribution.  P is the probability of an
   event.  Return seq of pairs [k pr], where k in (range N) and pr is
   probability of success after k failures.
  "
  [N p]
  (let [p (double p)
        q (- 1.0  p)]
    (for [k (range N)]
      [k (* (math/expt q k) p)])))


(defn sampling
  "First small step toward 'generic' sampler.  CNT is the size of the
   sample to create.  DIST is some distribution over some event
   space (aka omega).
  "
  [cnt dist]
  (loop [sample []
         i cnt]
    (if (= i 0)
      sample
      (recur (conj sample (rand-nth dist))
             (dec i)))))


(defn p-value
  "Compute p-value from distribution D and sample statistics x y
  "
  [d hits trials]
  (raise :type :NYI :msg "p-value Not Yet Implemented"))




(defn pdsum
  "NOT CURRENTLY USED.  REVISIT AND POSSIBLY ELIMINATE"
  [f pdf1 pdf2 & pdfs]
  (let [parallel (= pdf1 :||)
        pdfs (cons pdf2 pdfs)
        pdfs (if parallel pdfs (cons pdf1 pdfs))
        Omega (apply set/union (map #(set (keys %)) pdfs))]
    (apply sum (fn[k]
                  (let [pdf-vals (map #(get % k (double 0.0)) pdfs)]
                    (apply f pdf-vals)))
           pdfs)))
