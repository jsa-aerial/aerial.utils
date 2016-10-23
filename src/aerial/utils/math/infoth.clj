;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                      U T I L S . I N F O T H                             ;;
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

(ns aerial.utils.math.infoth

  "Various Information Theory functions and measures rooted in Shannon
   Entropy measure and targetting a variety of sequence and string
   data."

  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [clojure.set :as set]

            [aerial.utils.misc
             :refer [raise catch-all]]
            [aerial.utils.coll
             :refer [reducem subsets vfold]]
            [aerial.utils.string :as austr
             :refer [codepoints]]
            [aerial.utils.math
             :refer [sum log2]]
            [aerial.utils.math.combinatorics
             :refer [combins]]
            [aerial.utils.math.probs-stats
             :refer [joint-probability freqn probs word-letter-pairs]]
            ))



(defn shannon-entropy
  "Returns the Shannon entropy of a sequence: -sum(* pi (log pi)),
   where i ranges over the unique elements of S and pi is the
   probability of i in S: (freq i s)/(count s)"
  [s & {logfn :logfn :or {logfn log2}}]
  (let [fs (frequencies s)
        cnt (double (sum fs))
        H (sum (fn[[_ v]]
                 (let [p (double (/ v cnt))]
                   (double (* p (double (logfn p))))))
               fs)]
    (double (- H))))

(defn entropy
  "Entropy calculation for the probability distribution dist.
   Typically dist is a map giving the PMF of some sample space.  If it
   is a string or vector, this calls shannon-entropy on dist.
  "
  [dist & {logfn :logfn :or {logfn log2}}]
  (if (or (string? dist) (vector? dist))
    (shannon-entropy dist :logfn logfn)
    (let [dist (if (map? dist) (vals dist) dist)]
      (- (sum (fn[v]
                (let [v (double v)]
                  (if (= 0.0 v)
                    0.0
                    (double (* v (logfn v))))))
              dist)))))


(defn joint-entropy
  "Given a set of collections c1, c2, c3, .. cn, and combinator, a
   function of n variables which generates joint occurances from {ci},
   returns the joint entropy over all the set:

   -sum(* px1..xn (log2 px1..xn))

   OPTS is a map of options, currently sym? and logfn.  The defaults
   are false and log2.  If sym? is true, treat [x y] and [y x] as
   equal.  logfn can be used to provide a log of a different base.
   log2, the default, reports in bits.  If no options are required,
   the empty map must be passed: (joint-entropy transpose {} my-coll)
  "
  ([combinator opts coll]
     (let [{:keys [sym? logfn] :or {sym? false logfn log2}} opts]
       (entropy (joint-probability combinator sym? coll)
                :logfn logfn)))
  ([combinator opts coll & colls]
     (let [{:keys [sym? logfn] :or {sym? false logfn log2}} opts]
       (entropy (apply joint-probability combinator sym? coll colls)
                :logfn logfn))))

(defn HXY
  "Synonym for joint-entropy"
  [& args]
  (apply joint-entropy args))


(defn cond-entropy
  "Given the joint probability distribution PXY and the distribution
   PY, return the conditional entropy of X given Y = H(X,Y) - H(Y).

   Alternatively, given a set of collections c1, c2, c3, .. cn, and
   combinator, a function of n variables which generates joint
   occurances from {ci}, returns the multivariate conditional entropy
   induced from the joint probability distributions.

   OPTS is a map of options, currently sym? and logfn.  The defaults
   are false and log2.  If sym? is true, treat [x y] and [y x] as
   equal.  logfn can be used to provide a log of a different base.
   log2, the default, reports in bits.  If no options are required,
   the empty map must be passed: (cond-entropy transpose {} my-coll)

   For the case of i > 2, uses the recursive chain rule for
   conditional entropy (bottoming out in the two collection case):

   H(X1,..Xn-1|Xn) = (sum H(Xi|Xn,X1..Xi-1) (range 1 (inc n)))

  "
  ([PXY PY]
     (- (entropy PXY) (entropy PY)))
  ([combinator opts coll1 coll2]
     (let [{:keys [sym? logfn] :or {sym? false logfn log2}} opts]
       (- (entropy (joint-probability combinator sym? coll1 coll2) :logfn logfn)
          (entropy (probs 1 coll2) :logfn logfn))))
  ([combinator opts coll1 coll2 & colls]
     (let [{:keys [sym? logfn] :or {sym? false logfn log2}} opts]
       ;; Apply chain rule...
       (- (entropy (apply joint-probability combinator sym? coll1 coll2 colls)
                   :logfn logfn)
          (apply cond-entropy combinator opts coll2 colls)))))

(defn HX|Y
  "Synonym for cond-entropy"
  [& args]
  (apply cond-entropy args))


(defn combin-joint-entropy
  "Given a set of collections c1, c2, c3, .. cn, return the joint
   entropy: - (sum (* px1..xn (log2 px1..xn)) all-pairs-over {ci}).
   Where all-pairs-over is an exhaustive combination of elements of
   {ci} taken n at a time, where each n-tuple has exactly one element
   from each ci (i.e., the cross product of {ci}).

   Reports in bits (logfn = log2) and treats [x y] [y x] elements as
   the same.
  "
  ([coll1 coll2]
     (joint-entropy
      (fn[s1 s2] (reducem (fn[x y] [[x y]]) concat s1 s2))
      {:sym? true}
      coll1 coll2))
  ([coll1 coll2 & colls]
     (apply joint-entropy
            (fn[& cs]
              (apply reducem (fn[& args] [args]) concat cs))
            {:sym? true} coll1 coll2 colls)))


(defn seq-joint-entropy
  "Returns the joint entropy of a sequence with itself: -sum(* pi (log
   pi)), where probabilities pi are of combinations of elements of S
   taken 2 at a time.  If sym?, treat [x y] and [y x] as equal.
  "
  [s & {:keys [sym? logfn] :or {sym? false logfn log2}}]
  (joint-entropy #(combins 2 %) sym? s))


(defn relative-entropy
  "Take two distributions (that must be over the same space) and
   compute the expectation of their log ratio: Let px be the PMF of
   pdist1 and py be the PMF pdist2, return

   (sum (fn[px py] (* px (log2 (/ px py)))) xs ys)

   Here, pdist(1|2) are maps giving the probability distributions (and
   implicitly the pmfs), as provided by freqs-probs, probs,
   cc-freqs-probs, combins-freqs-probs, cc-combins-freqs-probs,
   et. al.  Or any map where the values are the probabilities of the
   occurance of the keys over some sample space.  Any summation term
   where (or (= px 0) (= py 0)), is taken as 0.0.

   NOTE: maps should have same keys! If this is violated it is likely
   you will get a :negRE exception or worse, bogus results.  However,
   as long as the maps reflect distributions _over the same sample
   space_, they do not need to be a complete sampling (a key/value for
   all sample space items) - missing keys will be included as 0.0
   values.

   Also known as Kullback-Leibler Divergence (KLD)

   KLD >= 0.0 in all cases.
  "
  [pdist1 pdist2 & {:keys [logfn] :or {logfn log2}}]
  ;; Actually, we try to 'normalize' maps to same sample space.
  (let [Omega (set/union (set (keys pdist1)) (set (keys pdist2)))
        KLD (sum (fn[k]
                   (let [pi (get pdist1 k 0.0)
                         qi (get pdist2 k 0.0)]
                     (if (or (= pi 0.0) (= qi 0.0))
                       (double 0.0)
                       (* pi (logfn (/ pi qi))))))
                 Omega)]
    (cond
     (>= KLD 0.0) KLD
     (< (math/abs KLD) 1.0E-10) 0.0
     :else
     (raise
      :type :negRE :KLD KLD :Pdist pdist1 :Qdist pdist2))))


(defn KLD
  "Synonym for relative-entropy.
   args is [pd1 pd2 & {:keys [logfn] :or {logfn log2}}"
  [& args] (apply relative-entropy args))
(defn DX||Y
  "Synonym for relative-entropy.
   args is [pd1 pd2 & {:keys [logfn] :or {logfn log2}}"
  [& args] (apply relative-entropy args))


(defn mutual-information
  "Mutual information between the content of coll1 and coll2 as
   combined by combinator, a function of two variables, presumed to be
   over coll1 and coll2 returning a collection of combined elements.

   OPTS is a map of options, currently sym? and logfn.  The defaults
   are false and log2.  If sym? is true, treat [x y] and [y x] as
   equal.  logfn can be used to provide a log of a different base.
   log2, the default, reports in bits.  If no options are required,
   the empty map must be passed: (mutual-information transpose {} my-coll)

   Mutual information is the relative entropy (KLD) of the joint
   probability distribution to the product distribution.  Here the
   distributions arise out of the frequencies over coll1 and coll2
   individually and jointly over the result of combinator on coll1 and
   coll2.

   Let C be (combinator coll1 coll2).  Computes

   (sum (* pxy (log2 (/ pxy (* px py)))) xs ys) =

   (+ (shannon-entropy coll1) (shannon-entropy coll2) (- (joint-entropy C))) =

   Hx + Hy - Hxy = I(X,Y)

   (<= 0.0 I(X,Y) (min [Hx Hy]))
  "
  [combinator opts coll1 coll2]
  (let [{:keys [sym? logfn] :or {sym? false logfn log2}} opts
        HX  (shannon-entropy coll1 :logfn logfn)
        HY  (shannon-entropy coll2 :logfn logfn)
        HXY (joint-entropy combinator opts coll1 coll2)
        IXY (+ HX HY (- HXY))]
    (cond
     (>= IXY 0.0) IXY
     (< (math/abs IXY) 1.0E-10) 0.0
     :else
     (raise
      :type :negIxy :Ixy IXY :Hxy HXY :Hx HX :Hy HY))))

(defn IXY "Synonym for mutual information"
  [combinator opts coll1 coll2]
  (mutual-information combinator opts coll1 coll2))


(defn conditional-mutual-information
  "Conditional mutual information I(X;Y|Z).  Conditional mutual
   information is the relative entropy (KLD) of the conditional
   distribution (of two random variables on a third) with the product
   distribution of the distributed conditionals.

   Here these arise out of frequencies for the information in collz
   individually, collx & collz and colly & collz jointly as the result
   of combinator, and collx & colly & collz jointly as the result of
   combinator as well.  Hence, combinator needs to be able to handle
   the cases of 2 and 3 collections passed to it (collectively or as
   variadic).

   OPTS is a map of options, currently sym? and logfn.  The defaults
   are false and log2.  If sym? is true, treat [x y] and [y x] as
   equal.  logfn can be used to provide a log of a different base.
   log2, the default, reports in bits.  If no options are required,
   the empty map must be passed: (conditional-mutual-information
   transpose {} my-coll)

   Other interpretations/meanings: Conditional mutual information is
   the mutual information of two random variables conditioned on a
   third.  Or, put another way, it is the expected value of the mutual
   information of two RV over the values of a third.  It measures the
   amount of information shared by X & Y beyond that provided by a
   common \"mediator\".

   Let XYZ be (combinator collx colly collz)
       XZ  be (combinator collx collz)
       YZ  be (combinator colly collz)

   Computes

   sum (* pxy|z (log2 (/ pxy|z (* px|z py|z)))) xs ys zs =

    ... (some algebra and applications of Bayes) ...

   sum (* pxyz (log2 (/ (* pz pxyz) (* pxz pyz)))) xyzs xzs yzs

    ... (some more algebra and entropy identitities) ...

   (+ H(X,Z) H(Y,Z) -H(X,Y,Z) -H(Z))

   which is easier to work with...
  "
  [combinator opts collx colly collz]
  (let [{:keys [sym? logfn] :or {sym? false logfn log2}} opts
        PXYZ  (vals (joint-probability combinator sym? collx colly collz))
        HXYZ  (entropy PXYZ :logfn logfn)

        PXZ   (vals (joint-probability combinator sym? collx collz))
        HXZ   (entropy PXZ :logfn logfn)

        PYZ   (vals (joint-probability combinator sym? colly collz))
        HYZ   (entropy PYZ :logfn logfn)

        PZ    (probs 1 collz)
        HZ    (entropy PZ :logfn logfn)

        IXY|Z (+ HXZ HYZ (- HXYZ) (- HZ))]
    (cond
     (>= IXY|Z 0.0) IXY|Z
     (< (math/abs IXY|Z) 1.0E-10) 0.0
     :else
     (raise
      :type :negIxy|z
      :Ixy|z IXY|Z :Hxz HXZ :Hyz HYZ :Hxyz HXYZ :Hz HZ))))

(defn IXY|Z "Synonym for conditional mutual information"
  [combinator opts collx colly collz]
  (conditional-mutual-information combinator opts collx colly collz))


(defn variation-information
  ""
  [combinator opts coll1 coll2]
  (let [Hxy (joint-entropy combinator opts coll1 coll2)
        Ixy (mutual-information combinator opts coll1 coll2)]
    (- 1.0 (double (/ Ixy Hxy)))))


(defn total-correlation
  "One of two forms of multivariate mutual information provided here.
   The other is \"interaction information\".  Total correlation
   computes what is effectively the _total redundancy_ of the
   information in the provided content - here the information in coll1
   .. colln.  As such it can give somewhat unexpected answers in
   certain situations.

   Information content \"measure\" is based on the distributions
   arising out of the frequencies over coll1 .. colln _individually_,
   and jointly over the result of combinator applied to coll1 .. colln
   collectively.

   OPTS is a map of options, currently sym? and logfn.  The defaults
   are false and log2.  If sym? is true, treat [x y] and [y x] as
   equal.  logfn can be used to provide a log of a different base.
   log2, the default, reports in bits.  If no options are required,
   the empty map must be passed: (total-correlation transpose {}
   my-coll)

   NOTE: the \"degenerate\" case of only two colls, is simply mutual
   information.

   Let C be (combinator coll1 coll2 .. colln), so xi1..xin in C is an
   element in the joint sample space, and xi in colli is an element in
   a \"marginal\" space . Computes

   sum (* px1..xn (log2 (/ px1..xn (* px1 px2 .. pxn)))) x1s x2s .. xns =

      Hx1 + Hx2 + .. + Hxn - Hx1x2..xn

   (<= 0.0
       TC(X1,..,Xn)
       (min|i (sum Hx1 .. Hxi Hxi+2 .. Hxn, i = 0..n-1, Hx0=Hxn+1=0)))

   Ex:

   (shannon-entropy \"AAAUUUGGGGCCCUUUAAA\")
   => 1.9440097497163569

   (total-correlation transpose {}
     \"AAAUUUGGGGCCCUUUAAA\" \"AAAUUUGGGGCCCUUUAAA\")
   => 1.9440097497163569 ; not surprising

   (total-correlation transpose {}
     \"AAAUUUGGGGCCCUUUAAA\" \"AAAUUUGGGGCCCUUUAAA\"
     \"AAAUUUGGGGCCCUUUAAA\" \"AAAUUUGGGGCCCUUUAAA\")
   => 5.832029249149071 ; possibly surprising if not noting tripled redundancy
  "
  ([combinator opts coll1 coll2]
     (mutual-information combinator opts coll1 coll2))
  ([combinator opts coll1 coll2 & colls]
     (let [{:keys [sym? logfn] :or {sym? false logfn log2}} opts
           colls (cons coll1 (cons coll2 colls))
           Hxs (map #(shannon-entropy % :logfn logfn) colls)
           HX1-Xn (apply joint-entropy combinator opts coll1 coll2 colls)
           TC (- (sum Hxs) HX1-Xn)]
       (cond
        (>= TC 0.0) TC
        (< (math/abs TC) 1.0E-10) 0.0
        :else
        (raise
         :type :negTC
         :TC TC :Hxs Hxs :HX1-XN HX1-Xn)))))

(defn TCI
  "Synonym for total-correlation information"
  [combinator opts coll1 coll2 & colls]
  (apply total-correlation combinator opts coll1 coll2 colls))


(defn interaction-information
  "One of two forms of multivariate mutual information provided here.
   The other is \"total correlation\".  Interaction information
   computes the amount of information bound up in a set of variables,
   beyond that present in _any subset_ of those variables. Well, that
   seems to be the most widely held view, but there are disputes. This
   can either indicate inhibition/redundancy or facilitation/synergy
   between the variables.  It helps to look at the 3 variable case, as
   it at least lends itself to 'information' (or info venn) diagrams.

   II(X,Y,Z) = I(X,Y|Z) - I(X,Y)

   II measures the influence of Z on the information connection
   between X and Y.  Since I(X,Y|Z) < I(X,Y) is possible, II can be
   negative, as well as 0 and positive.  For example if Z completely
   determines the information content between X & Y, then I(X,Y|Z) -
   I(X,Y) would be 0 (as overlap contributed by X & Y alone is totally
   redundant), while for X & Y independent of Z, I(X,Y) could still be
   > 0.  A _negative_ interaction indicates Z inhibits (accounts for,
   causes to be irrelevant/redundant) the connection between X & Y.  A
   _positive_ interaction indicates Z promotes, is synergistic with,
   or enhances the connection.  The case of 0 is obvious...

   If we use the typical info theory venn diagrams, then II is
   represented as the area in all the circles. It is possible (and
   typically so done) to draw the Hxi (xi in {X,Y,Z}) circles so that
   all 3 have an overlap.  In this case the information between any
   two (the mutual information...) is partially accounted for by the
   third.  The extra overlap accounts for the negative value
   \"correction\" of II in this case.

   However, it is also possible to draw the circles so that any two
   overlap but all three have null intersect.  In this case, the third
   variable provides an extra connection between the other two (beyond
   their MI), something like a \"mediator\".  This extra connection
   accounts for the positive value \"facillitation\" of II in this
   case.

   It should be reasonably clear at this point that negative II is the
   much more intuitive/typical case, and that positive cases are
   surprising and typically more interesting and informative (so to
   speak...).  A positive example, would be the case of two mutually
   exclusive causes for a common effect (third variable).  In such a
   case, knowing any two completely determines the other.

   All of this can be bumped up to N variables via typical chain rule
   recurrance.

   In this implementation, the information content \"measure\" is
   based on the distributions arising out of the frequencies over
   coll1 .. colln _individually_, and jointly over the result of
   combinator applied collectively to all subsets of coll1 .. colln.

   OPTS is a map of options, currently sym? and logfn.  The defaults
   are false and log2.  If sym? is true, treat [x y] and [y x] as
   equal.  logfn can be used to provide a log of a different base.
   log2, the default, reports in bits.  If no options are required,
   the empty map must be passed: (interaction-information transpose {}
   my-coll)

   For collections coll1..colln and variadic combinator over any
   subset of collections, Computes:

   (sum (fn[ss] (* (expt -1 (- n |ss|))
                   (HXY combinator sym? ss)))
        (subsets all-colls))

   |ss| = cardinality/count of subset ss.  ss is a subset of
   coll1..colln.  If sym? is true, treat reversable combined items as
   equal (see HYX/joint-entropy).

   For n=3, this reduces to (- (IXY|Z combinator sym? collx colly collz)
                               (IXY combinator sym? collx colly))

   Ex:

   (interaction-information transpose {}
     \"AAAGGGUUUUAAUAUUAAAAUUU\"
     \"AAAGGGUGGGAAUAUUCCCAUUU\"
     \"AAAGGGUGGGAAUAUUCCCAUUU\")
   => -1.1673250256261127

   Ex:

   (interaction-information transpose {}
     \"AAAGGGUUUUAAUAUUAAAAUUU\"
     \"AAAGGGUGGGAAUAUUCCCAUUU\"
     (reverse-compliment \"AAAGGGUGGGAAUAUUCCCAUUU\"))
   => -0.282614135781623

   Ex: Shows synergy (positive value)

   (let [X1 [0 1] ; 7 independent binary vars
         X2 [0 1]
         X3 [0 1]
         X4 [0 1]
         X5 [0 1]
         X6 [0 1]
         X7 [0 1]
         X8 [0 1]
         Y1 [X1 X2 X3 X4 X5 X6 X7]
         Y2          [X4 X5 X6 X7]
         Y3             [X5 X6 X7]
         Y4                   [X7]
         bitval #(loop [bits (reverse %) n 0 v 0]
                   (if (empty? bits)
                     v
                     (recur (rest bits)
                            (inc n)
                            (+ v (* (first bits) (math/expt 2 n))))))
         rfn #(map bitval (apply reducem (fn[& args] [args]) concat %))
         ;; Turn to (partially redundant) value sets - each Yi is the set
         ;; of all numbers for all (count Yi) bit patterns.  So, Y1, Y2,
         ;; and Y3 share all values of 3 bits, while the group with Y4
         ;; added shares just all 1 bit patterns
         Y1 (rfn Y1)
         Y2 (rfn Y2)
         Y3 (rfn Y3)
         Y4 (rfn Y4)]
     [(interaction-information concat {} Y1 Y2 Y3)
      (interaction-information concat {} Y1 Y2 Y3 Y4)])
  => [-3.056592722891465   ; Intuitive, since 3 way sharing
       1.0803297840536592] ; Unintuitive, since 1 way sharing induces synergy!?
  "
  #_([combinator opts collx colly collz]
       (let [Ixy|z (IXY|Z combinator opts collx colly collz)
             Ixy   (IXY combinator opts collx colly)]
         (- Ixy|z Ixy)))
  ([combinator opts collx colly collz & colls]
     (let [colls (->> colls (cons collz) (cons colly) (cons collx))
           ssets (remove empty? (subsets colls))
           tcnt (count colls)]
       (- (sum (fn[ss]
                 (let [sscnt (count ss)]
                   (* (math/expt -1.0 (- tcnt sscnt))
                      (apply joint-entropy combinator opts ss))))
               ssets)))))

(defn II
  "Synonym for interaction-information"
  [combinator opts collx colly collz & colls]
  (apply interaction-information combinator opts collx colly collz colls))


(defn lambda-divergence
  "Computes a symmetrized KLD variant based on a probability
   parameter, typically notated lambda, in [0..1] for each
   distribution:

   (+ (* lambda (DX||Y Pdist M)) (* (- 1 lambda) (DX||Y Qdist M)))

   Where M = (+ (* lambda Pdist) (* (- 1 lambda) Qdist))
           = (merge-with (fn[pi qi] (+ (* lambda pi) (* (- 1 lambda) qi)))
                         Pdist Qdist)

   For lambda = 1/2, this reduces to

   M = 1/2 (merge-with (fn[pi qi] (+ pi qi)) P Q)

   and (/ (+ (DX||Y Pdist M) (DX||Y Qdist M)) 2) = jensen shannon
  "
  [lambda Pdist Qdist]
  (let [Omega (set/union (set (keys Pdist)) (set (keys Qdist)))
        M (reduce (fn[M k]
                    (assoc M k (+ (* lambda (get Pdist k 0.0))
                                  (* (- 1 lambda) (get Qdist k 0.0)))))
                  {}
                  Omega)]
    (+ (* lambda (DX||Y Pdist M)) (* (- 1 lambda) (DX||Y Qdist M)))))

(defn DLX||Y
  "Synonym for lambda divergence"
  [l Pdist Qdist]
  (lambda-divergence l Pdist Qdist))


(defn jensen-shannon
  "Computes Jensen-Shannon Divergence of the two distributions Pdist
  and Qdist.  Pdist and Qdist _must_ be over the same sample space!"
  [Pdist Qdist]
  #_(let [Omega (set/union (set (keys Pdist)) (set (keys Qdist)))
          M (reduce (fn[M k]
                      (assoc M k (/ (+ (get Pdist k 0.0)
                                       (get Qdist k 0.0))
                                    2.0)))
                    {}
                    Omega)]
      (/ (+ (DX||Y Pdist M) (DX||Y Qdist M)) 2.0))
  (lambda-divergence 0.5 Pdist Qdist))




(defn log-odds [frq1 frq2]
  (log2 (/ frq1 frq2)))

(defn lod-score [qij pi pj]
  (log2 (/ qij (* pi pj))))

(defn raw-lod-score [qij pi pj & {scaling :scaling :or {scaling 1.0}}]
  (if (= scaling 1.0)
    (lod-score qij pi pj)
    (int (/ (lod-score qij pi pj) scaling))))




;;; ----------------------------------------------------------------------
;;;


;;; Fixed cost Edit distance.
;;;
;;; (levenshtein "this" "")
;;; (assert (= 0 (levenshtein "" "")))
;;; (assert (= 3 (levenshtein "foo" "foobar")))
;;; (assert (= 3 (levenshtein "kitten" "sitting")))
;;; (assert (= 3 (levenshtein "Saturday" "Sunday")))
;;; (assert (= 22 (levenshtein
;;;   "TATATTTGGAGTTATACTATGTCTCTAAGCACTGAAGCAAA"
;;;   "TATATATTTTGGAGATGCACAT"))
;;;
(defn- new-row [prev-row row-elem t]
  (reduce
   (fn [row [d-1 d e]]
     (conj row (if (= row-elem e) d-1 (inc (min (peek row) d d-1)))))
    [(inc (first prev-row))]
    (map vector prev-row (next prev-row) t)))

(defn levenshtein
  "Compute the Levenshtein (edit) distance between S and T, where S
   and T are either sequences or strings.

   Examples:  (levenshtein [1 2 3 4] [1 1 3]) ==> 2
              (levenshtein \"abcde\" \"bcdea\")   ==> 2
  "
  [s t]
  (cond
   (or (= s t "") (and (empty? s) (empty? t))) 0
   (= 0 (count s)) (count t)
   (= 0 (count t)) (count s)
   :else
   (peek (reduce
          (fn [prev-row s-elem] (new-row prev-row s-elem t))
          (range (inc (count t)))
          s))))


(defn- hamming-stgs [s1 s2]
  (let [sz1 (long (count s1))
        len (long (min sz1 (long (count s2))))]
    (loop [i (long 0)
           cnt (long (Math/abs (- len sz1)))]
      (if (= i sz1)
        cnt
        (if (= (austr/get s1 i) (austr/get s2 i))
          (recur (inc i) cnt)
          (recur (inc i) (inc cnt)))))))

(defn hamming
  "Compute hamming distance between sequences S1 and S2. If both s1
  and s2 are strings, performs an optimized version"
  [s1 s2]
  (if (and (string? s1) (string? s2))
    (hamming-stgs s1 s2)
    (reduce + (map (fn [b1 b2] (if (= b1 b2) 0 1)) s1  s2))))


(defn diff-fn
  "Return the function that is 1-F applied to its args: (1-(apply f
   args)).  Intended for normalized distance metrics.

   Ex: (let [dice-diff (diff-fn dice-coeff) ...]
         (dice-diff some-set1 some-set2))
  "
  [f]
  (fn [& args] (- 1 (apply f args))))


(defn dice-coeff [s1 s2]
  (/ (* 2 (count (set/intersection s1 s2)))
     (+ (count s1) (count s2))))

(defn jaccard-index [s1 s2]
  (/ (count (set/intersection s1 s2))
     (count (set/union s1 s2))))

(defn tversky-index
  "Tversky index of two sets S1 and S2.  A generalized NON metric
   similarity 'measure'.  Generalization is through the ALPHA and BETA
   coefficients:

   TI(S1,S2) = (/ |S1^S2| (+ |S1^S2| (* ALPHA |S1-S2|) (* BETA |S2-S1|)))

   For example, with alpha=beta=1,  TI is jaccard-index
                with alpha=beta=1/2 TI is dice-coeff
   "
  [s1 s2 alpha beta]
  (let [s1&s2 (count (set/intersection s1 s2))
        s1-s2 (count (set/difference s1 s2))
        s2-s1 (count (set/difference s2 s1))]
    (/ s1&s2
       (+ s1&s2 (* alpha s1-s2) (* beta s2-s1)))))


(def
 ^{:doc
   "Named version of (diff-fn jaccard-index s1 s2).  This difference
    function is a similarity that is a proper _distance_ metric (hence
    usable in metric trees like bk-trees)."
   :arglists '([s1 s2])}
 jaccard-dist
 (diff-fn jaccard-index))


(defn freq-jaccard-index
  ""
  [s1 s2]
  (let [freq-s1 (set s1)
        freq-s2 (set s2)
        c1 (sum (set/intersection freq-s1 freq-s2))
        c2 (sum (set/union freq-s1 freq-s2))]
    (/ c1 c2)))


(defn bi-tri-grams [s]
  (let [bi-grams (set (keys (freqn 2 s)))
        tri-grams (set (keys (freqn 3 s)))]
    [(set/union bi-grams tri-grams)
     [bi-grams tri-grams]]))

(defn all-grams [s]
  (let [all-gram-sets
        (for [n (range 1 (count s))]
          (-> (freqn n s) keys set))]
    [(apply set/union all-gram-sets) all-gram-sets]))

(defn ngram-compare
  ""
  [s1 s2 & {uc? :uc? n :n scfn :scfn ngfn :ngfn
            :or {n 2 uc? false scfn dice-coeff ngfn word-letter-pairs}}]
  (let [s1 (ngfn n (if uc? (str/upper-case s1) s1))
        s2 (ngfn n (if uc? (str/upper-case s2) s2))]
    (scfn s1 s2)))

;;;(float (ngram-compare "FRANCE" "french"))
;;;(float (ngram-compare "FRANCE" "REPUBLIC OF FRANCE"))
;;;(float (ngram-compare "FRENCH REPUBLIC" "republic of france"))
;;;(float (ngram-compare
;;;        "TATATTTGGAGTTATACTATGTCTCTAAGCACTGAAGCAAA"
;;;        "TATATATTTTGGAGATGCACAT"))


(defn normed-codepoints [s]
  (vec (map #(let [nc (- % 97)]
               (cond
                (>= nc 0) nc
                (= % 32) 27
                :else 28))
            (codepoints s))))

(defn ngram-vec [s & {n :n :or {n 2}}]
  (let [ngrams (word-letter-pairs s n)
        ngram-points (map (fn [[x y]]
                            (int (+ (* x 27) y)))
                          (map normed-codepoints ngrams))
        v (int-array 784 0)]
    (doseq [i ngram-points]
      (aset v i 1))
    v))



;;;-------------------------------------------------------------------------;;;
;;;                                                                         ;;;
;;;      Minimization and Maximization of Entropy Principles                ;;;
;;;                                                                         ;;;
;;; Cumulative Relative Entropy, Centroid Frequency and Probability         ;;;
;;; dictionaries, information capacity, et.al.                              ;;;
;;;                                                                         ;;;
;;;-------------------------------------------------------------------------;;;


(defn expected-qdict [q-1 q-2 & {:keys [alpha] :or {alpha ["A" "U" "G" "C"]}}]
  (reduce (fn[m lmer]
            (let [l (dec (count lmer))
                  x (subs lmer 1)
                  y (subs lmer 0 l)
                  z (subs lmer 1 l)]
              (if (and (q-1 x) (q-1 y) (q-2 z))
                (assoc m lmer (/ (* (q-1 x) (q-1 y)) (q-2 z)))
                m)))
          {} (for [x (keys q-1) a alpha] (str x a))))


(defn freq-xdict-dict
  [q sq]
  (let [ext-sq (str sq "X")
        residue-key (subs ext-sq (- (count sq) (- q 1)))
        q-xfreq-dict (freqn q ext-sq)
        q-freq-dict (dissoc q-xfreq-dict residue-key)]
    [q-xfreq-dict q-freq-dict]))

(defn q-1-dict
  ([q-xdict]
     (reduce (fn[m [k v]]
               (let [l (count k)
                     pre (subs k 0 (dec l))]
                 (assoc m pre (+ (get m pre 0) v))))
             {} q-xdict))
  ([q sq]
     (probs (dec q) sq)))

(defn q1-xdict-dict
  [q sq & {:keys [ffn] :or {ffn probs}}]
  (let [[q-xfreq-dict q-freq-dict] (freq-xdict-dict q sq)
        q-xpdf-dict (probs q-xfreq-dict)
        q-pdf-dict (probs q-freq-dict)]
    {:xfreq q-xfreq-dict :xpdf q-xpdf-dict
     :freq q-freq-dict :pdf q-pdf-dict}))



(defn reconstruct-dict
  [l sq & {:keys [alpha] :or {alpha ["A" "U" "G" "C"]}}]
  {:pre [(> l 2)]}
  (let [q (dec l)
        qmaps (q1-xdict-dict q sq)
        [q-xdict q-dict] (map qmaps [:xpdf :pdf])
        q-1dict (q-1-dict q-xdict)]
    (expected-qdict q-dict q-1dict :alpha alpha)))


(defn max-qdict-entropy
  [q & {:keys [alpha] :or {alpha ["A" "U" "G" "C"]}}]
  (let [base (count alpha)]
    (* q (log2 base))))

(defn informativity
  ([q sq]
     (- (max-qdict-entropy q) (entropy (probs q sq))))
  ([q-dict]
     (let [q (count (first (keys q-dict)))]
       (- (max-qdict-entropy q) (entropy q-dict)))))


(defn limit-entropy
  [q|q-dict sq|q-1dict &
   {:keys [alpha NA] :or {alpha ["A" "U" "G" "C"] NA -1.0}}]
  {:pre [(or (and (integer? q|q-dict)
                  (or (string? sq|q-1dict) (coll? sq|q-1dict)))
             (and (map? q|q-dict) (map? sq|q-1dict)))]}

  (if (map? q|q-dict)
    (let [q-dict q|q-dict
          q-1dict sq|q-1dict]
      (/ (- (entropy q-dict) (entropy q-1dict))
         (log2 (count alpha))))

    (let [q q|q-dict
          sq sq|q-1dict
          lgcnt (log2 (count alpha))]
      (if (= q 1)
        (/ (entropy (probs 1 sq)) lgcnt)

        (let [qmaps (q1-xdict-dict q sq)
              [q-xdict q-dict] (map qmaps [:xpdf :pdf])
              q-1dict (q-1-dict q-xdict)]
          (if (< (count q-dict) (count q-1dict))
            (if (fn? NA) (NA q-dict q-1dict) NA)
            (/ (- (entropy q-dict) (entropy q-1dict)) lgcnt)))))))


(defn limit-informativity
  ([q sq]
     )
  ([q-dict]
     ))


(defn CREl
  [l sq & {:keys [limit alpha]
           :or {limit 15}}]
  {:pre [(> l 2) alpha]}
  (sum (fn[k]
         (catch-all (DX||Y
                     (probs k sq)
                     (reconstruct-dict k sq :alpha alpha))))
       (range l (inc limit))))


(defn information-capacity
  [q sq & {:keys [cmpfn] :or {cmpfn jensen-shannon}}]
  (catch-all (cmpfn (probs q sq)
                    (reconstruct-dict q sq))))


(defn hybrid-dictionary
  "Compute the 'hybrid', aka centroid, dictionary or Feature Frequency
   Profile (FFP) for sqs.  SQS is either a collection of already
   computed FFPs (probability maps) of sequences, or a collection of
   sequences, or a string denoting a sequence file (sto, fasta, aln,
   ...) giving a collection of sequences.  In the latter cases, the
   sequences will have their FFPs computed based on word/feature
   length L (resolution size).  In all cases the FFPs are combined,
   using the minimum entropy principle, into a joint ('hybrid' /
   centroid) FFP.
  "
  [l sqs]
  {:pre [(or (string? sqs) (coll? sqs))]}

  (let [sqs (if (-> sqs first map?)
              sqs
              (if (coll? sqs) sqs (raise :type :old-read-seqs :sqs sqs)))
        cnt (count sqs)
        par (max (math/floor (/ cnt 10)) 2)
        dicts (if (-> sqs first map?) sqs (vfold #(probs l %) sqs))
        hybrid (apply merge-with +
                      (vfold (fn[subset] (apply merge-with + subset))
                             (partition-all (/ (count dicts) par) dicts)))]
    (reduce (fn[m [k v]] (assoc m k (double (/ v cnt))))
            {} hybrid)))


