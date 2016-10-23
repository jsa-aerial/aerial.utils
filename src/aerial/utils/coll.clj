;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                         U T I L S . C O L L                              ;;
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

(ns aerial.utils.coll

  "Various supplementary collection functions not included in the
   dstandard Clojure ecosystem.  Mostly for seqs, but also for
   vectors, maps and sets."

  (:refer-clojure :exclude [map-entry?])

  (:require
   [clojure.core.reducers :as r]
   [clojure.string :as str]
   [aerial.utils.math.combinatorics :as combin]
   ))


(defn third [coll]
  (->> coll (drop 2) first))

(defn in
  "Return whether e is an element of coll."
  [e coll]
  (if (map? coll)
    (find coll e)
    (some #(= % e) coll)))

(defn positions [pred coll]
  (keep-indexed (fn[i x] (when (pred x) i)) coll))

(defn pos
  "Returns a lazy seq of positions of X within COLL taken as a sequence"
  [x coll]
  (keep-indexed #(when (= x %2) %1) coll))

(defn pos-any
  "Returns a lazy seq of positions of any element of TEST-COLL within
   COLL taken as a sequence"
  [test-coll coll]
  (keep-indexed #(when (in %2 test-coll) %1) coll))


(defn drop-until
  "Complement of drop-while"
  [pred coll] (drop-while (complement pred) coll))

(defn take-until
  "Complement of take-while"
  [pred coll] (take-while (complement pred) coll))

(defn take-until-nochange
  "Eagerly takes from SQ until consecutive elements are the same.  So,
   take until and up to element Ei, where Ei=Ei+1.  Equality of
   elements is determined by elt=, which defaults to =.
  "
  [sq & {:keys [elt=] :or {elt= =}}]
  (loop [rsq (rest sq)
         prev (first sq)
         res [prev]]
    (if (elt= prev (first rsq))
      res
      (recur (rest rsq)
             (first rsq)
             (conj res (first rsq))))))

(defn sliding-take
  "Sliding window take. N is the \"window\" size to slide across
  collection COLL treated as a sequence. D is the slide displacement
  and defaults to 1."
  ([n coll]
   (sliding-take 1 n coll))
  ([d n coll]
   (let [next (fn next [s]
                (when (seq s)
                  (cons (take n s)
                        (lazy-seq (next (drop d s))))))]
     (next coll))))

(defn separate
  "Returns a vector:
   [(filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

(defn rotate
  "Rotate (seq coll) by n positions. In single arg case, n=1."
  ([coll]
   (rotate 1 coll))
  ([n coll]
   (concat (drop n coll) (take n coll))))

(defn rotations
  "Returns a lazy seq of all rotations of a seq"
  [x]
  (if (seq x)
    (map
     (fn [n _]
       (lazy-cat (drop n x) (take n x)))
     (iterate inc 0) x)
    (list nil)))


(defn concatv
  "Eager concat. WARNING: Use with caution on large colls. Will
  infinite loop on infinite colls!"
  ([] [])
  ([coll & colls]
   (reduce (fn[C c] (reduce conj C c)) [] (cons coll colls))))

(defn takev
  "Eager take. Uses transducers to eagerly take from a coll"
  [n coll]
  (reduce ((take n) conj) [] coll))

(defn dropv
  "Eager drop. Uses transducers to eagerly drop from a coll

   WARNING: Use with caution on large colls. Will infinite loop on
   infinite colls!
  "
  [n coll]
  (reduce ((drop n) conj) [] coll))

(defn takev-while
  "Eager take-while. Uses transducers to eagerly take from a coll

   WARNING: Use with caution on large colls. May infinite loop on
   infinite colls!
  "
  [pred coll]
  (reduce ((take-while pred) conj) [] coll))

(defn dropv-while
  "Eager drop-while. Uses transducers to eagerly drop from a coll

   WARNING: Use with caution on large colls. May infinite loop on
   infinite colls!
  "
  [pred coll]
  (reduce ((drop-while pred) conj) [] coll))

(defn takev-until
  "Eager take-until. Uses transducers to eagerly take from a coll

   WARNING: Use with caution on large colls. May infinite loop on
   infinite colls!
  "
  [pred coll]
  (takev-while (complement pred) coll))

(defn dropv-until
  "Eager drop-until. Uses transducers to eagerly drop from a coll

   WARNING: Use with caution on large colls. May infinite loop on
   infinite colls!
  "
  [pred coll]
  (dropv-while (complement pred) coll))

(defn splitv-at
  "Eager split. Uses transducers to eagerly split a coll a pos n.

   WARNING: Uses eager drop - use with caution on large colls. Will
   infinite loop on infinite colls!
  "
  [n coll]
  [(transduce (take n) conj [] coll)
   (transduce (drop n) conj [] coll)])

(defn separatev
  "Eager separate, returns [(filterv f s) (filterv (complement f) s)]

   WARNING: Uses eager drop - use with caution on large colls. Will
   infinite loop on infinite colls!
  "
  [f s]
  [(filterv f s) (filterv (complement f) s)])

(defn partitionv-all
  "Eager partition-all. Uses transducers to eagerly partition coll
   into partitions of size n (with possibly fewer than n items at the
   end).

   WARNING: use with caution on large colls. Will infinite loop on
   infinite colls!
  "
  [n coll]
  (transduce (partition-all n) conj [] coll))


(defn mkseq [x]
  (if (or (seq? x) (nil? x))
    x
    (list x)))

(defn ensure-vec
  "Return a vector representation for x.  If x is a vector just return
   it, if it is a seqable return (vec x), if it is an \"atom\" return
   [x]."
  [x]
  (cond
   (vector? x) x
   (coll? x) (vec x)
   :else [x]))


(defn subsets
  "All the subsets of elements of coll"
  [coll]
  (mapcat #(combin/combins % coll) (range (inc (count coll)))))

(defn random-subset
  "Create a \"random\" N element subset of the collection s treated as a set,
   i.e., s with no duplicate elements.  If n <= 0, return #{}, the
   empty set.  If (count (set s)) <= n, return (set s).  Otherwise,
   pick N random elements from (set s) to form subset.
  "
  [s n]
  (let [s (vec (set s))]
    (cond
     (<= n 0) #{}
     (<= (count s) n) (set s)
     :else
     (loop [ss #{(rand-nth s)}]
       (if (= (count ss) n)
         ss
         (recur (conj ss (rand-nth s))))))))


(defn map-entry?
  "Return whether x is a map entry"
  [x]
  (if (resolve (symbol "clojure.core/map-entry?"))
    (clojure.core/map-entry? x)
    (instance? clojure.lang.MapEntry x)))

(defn merge-with*
  "Merge-with needs to call user supplied F with the KEY as well!!!
  Returns a map that consists of the rest of the maps conj-ed onto the
  first.  If a key occurs in more than one map, the mapping(s) from
  the latter (left-to-right) will be combined with the mapping in the
  result by calling (f key val-in-result val-in-latter)."
  {:added "1.jsa"} [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f k (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn map->csv-map
  "Transforms a nested map into a \"flattened\" map where keys are
  column names formed by concatenating the path of keys to each
  element. If prefix is given it is catenated to the front of each
  column name.

  Ex:

  (map->csv-map {:one {:a 1 :b 2}, :two {\"hi\" 1 \"there\" 7}})
  => {\"one_a\" [1], \"one_b\" [2], \"two_hi\" [1], \"two_there\" [7]}

  (map->csv-map \"P\" {:one {:a 1 :b 2}, :two {\"hi\" 1 \"there\" 7}})
  => {\"P_one_a\" [1], \"P_one_b\" [2], \"P_two_hi\" [1], \"P_two_there\" [7]}
  "
  ([map] (map->csv-map "" map))
  ([prefix map]
   (reduce-kv (fn [m k v]
                (let [prefixed (if (empty? prefix)
                                 (name k)
                                 (str prefix "_" (name k)))]
                  (merge m (if (map? v)
                             (map->csv-map prefixed v)
                             {prefixed [v]}))))
              {}
              map)))

(defn coalesce-xy-yx
  "Coaleseces elements of item-coll, which are or have common \"keys\",
   according to the function f.  Two keys k1 and k2 are considered
   common if (or (= k1 k2) (= (reverse k1) k2)) for reversible keys or
   simply (= k1 k2) for non reversible keys.  Reversible keys are
   vectors, seqs, or string types.

   F is a function of two parameters [x v], where x is an element of
   item-coll, and v is the current value associated with the key of x
   or nil if no association yet exists.  F is expected to return the
   current association for key of x based on x and v.  If x is a
   mapentry, (key x) is used to determine the association.  If x is a
   list or vector (first x) is used to determine the association.

   Ex:
   (freqn 1 (map #(apply str %) (combins 2 \"auauuagcgccg\")))
   => {\"aa\" 3, \"cc\" 3, \"gg\" 3, \"uu\" 3, \"ac\" 9, \"cg\" 4,
       \"ag\" 9, \"ua\" 4, \"uc\" 9, \"ug\" 9, \"au\" 5, \"gc\" 5}

   (coalesce-xy-yx *1 (fn[x v] (if (not v) 0 (+ (val x) v))))
   => {\"aa\" 3, \"cc\" 3, \"gg\" 3, \"uu\" 3, \"ac\" 9, \"cg\" 9,
       \"ag\" 9, \"ua\" 9, \"uc\" 9, \"ug\" 9}
  "
  [item-coll f]
  (let [rev (fn[x] (if (string? x) (str/reverse x) (reverse x)))
        res (reduce (fn[m x]
                      (let [k (cond
                               (map-entry? x) (key x)
                               (coll? x) (first x)
                               :else x)
                            keycoll? (or (vector? x) (string? x) (seq? x))
                            [k v] (if (not keycoll?)
                                    [k (get m k (f x nil))]
                                    (if-let [v (get m k)]
                                      [k v]
                                      (let [rk (rev k)]
                                        (if-let [v (get m rk)]
                                          [rk v]
                                          [k (f x nil)]))))]
                        (assoc m k (f x v))))
                    {} item-coll)]
    (cond
     (map? item-coll) res
     (vector? item-coll) (vec res)
     :else (seq res))))


(defn transpose
  "Matrix transposition.  Well, effectively.  Can be used in some
   other contexts, but does the same computation.  Takes colls a
   collection of colletions, treats this as a matrix of (count colls)
   rows, each row being a string or seqable data structure: M[rij],
   where rij is the jth element of the ith row.  Returns M' = M[cji],
   where cji is the ith element of the jth column of M.

   For the cases where colls is a string or a collection of strings,
   returns M with rows as strings (effectively M[(apply str cji)]).
  "
  ([colls]
     {:pre [(or (string? colls)
                (and (coll? colls)
                     (every? #(or (coll? %) (string? %)) colls)))]}
     (let [colls (if (string? colls) [colls] colls)]
       (if (empty? colls)
         colls
         (let [tm (apply map vector colls)]
           (if (every? string? colls)
             (map #(apply str %) tm)
             tm)))))

  ([coll1 coll2 & colls]
     (transpose (cons coll1 (cons coll2 colls)))))


(defn- reduce-in-parallel
  "Helper function for reducem.  When reducem determines that the
   function application should proceed in parallel over the sequences,
   it defers to this function to compute the resuls"
  [f fr coll1 & colls]
  (reduce
   (fn[x y]
       (fr x y))
   (fr) (apply map f (cons coll1 colls))))

(defn reducem
  "Multiple collection reduction. FR is a reducing function which must
   return an identity value when called with no arguments. F is a
   transform function that is applied to the arguments from the
   supplied collections (treated as seqs).  Note, for the first
   application, the result is (fr (fr) (f ...)).

   By default, reduction proceeds on the results of F applied over the
   _cross product_ of the collections.  If reduction should proceed
   over collections in parallel, the first \"coll\" given should be
   the special keyword :||.  If given, this causes F to be applied to
   the elements of colls as stepped in parallel: f(coll1[i] coll2[i]
   .. colln[i]), i in [0 .. (count smallest-given-coll)].
  "
  ([f fr coll]
     (reduce
      (fn[x y] (fr x (f y)))
      (fr) coll))

  ([f fr coll1 & colls]
     (let [colls (cons coll1 colls)]
       (if (= coll1 :||)
         (apply reduce-in-parallel f fr (rest colls))
         ;;Else: We reduce X-product reductions by currying outer args
         ;;into f.
         (reduce
          (fn[r xr]
            (apply reducem
                   (fn[& args] (apply f xr args))
                   (fn([] r) ([x y] (fr x y)))
                   (rest colls)))
          (fr) (first colls))))))



(defn xprod
  "Cross product item generation of size K over COLL. xfn is a
  transform function applied to each item set generated, and defaults
  to simply aggregating them in a vector.

  Examples:
  (xprod 2 \"ADN\")
  => [[\\A \\A] [\\A \\D] [\\A \\N] [\\D \\A] [\\D \\D]
      [\\D \\N] [\\N \\A] [\\N \\D] [\\N \\N]]
  (xprod str 2  \"ADN\")
  => [\"AA\" \"AD\" \"AN\" \"DA\" \"DD\" \"DN\" \"NA\" \"ND\" \"NN\"]"
  ([k coll]
   (xprod vector k coll))
  ([xfn k coll]
   (let [v (vec coll)]
     (apply reducem xfn (fn ([] []) ([R x] (conj R x))) (repeat k coll)))))

(defn xprod-rng1k
  "Cross product item generation ranging from size 1 to K over
  COLL. The cross products for each size are concatenated in order.
  xfn is a transform function applied to each item set generated, and
  defaults to simply aggregating them in a vector.

  Examples:
  (xprod-rng1k 1 \"ADN\")
  => ([\\A] [\\D] [\\N])
  (xprod-rng1k 2 \"ADN\")
  => ([\\A] [\\D] [\\N] [\\A \\A] [\\A \\D] [\\A \\N] [\\D \\A] [\\D \\D]
      [\\D \\N] [\\N \\A] [\\N \\D] [\\N \\N])
  (xprod-rng1k str 3 \"ADN\")
  => (\"A\" \"D\" \"N\" \"AA\" \"AD\" \"AN\" \"DA\" \"DD\"
      \"DN\" \"NA\" \"ND\" \"NN\" \"AAA\" \"AAD\" \"AAN\"
      \"ADA\" \"ADD\" \"ADN\" \"ANA\" \"AND\" \"ANN\" \"DAA\"
      \"DAD\" \"DAN\" \"DDA\" \"DDD\" \"DDN\" \"DNA\" \"DND\"
      \"DNN\" \"NAA\" \"NAD\" \"NAN\" \"NDA\" \"NDD\" \"NDN\"
      \"NNA\" \"NND\" \"NNN\")"
  ([k coll]
   (xprod-rng1k vector k coll))
  ([xfn k coll]
   (mapcat #(xprod xfn % coll) (range 1 (inc k)))))



(defn vfold
  "Fold function f over a collection or set of collections (c1, ...,
   cn) producing a collection (concrete type of vector).  Arity of f
   must be equal to the number of collections being folded with
   parameters matching the order of the given collections.  Folding
   here uses the reducer lib fold at base, and thus uses work stealing
   deque f/j to mostly relieve the partition problem.  In signatures
   with N given, N provides the packet granularity, or if (< N 1),
   granularity is determined automatically (see below) as in the
   base (non N case) signature.

   While vfold is based on r/fold, it abstracts over the combiner,
   reducer, work packet granularity, and transforming multiple
   collections for processing by f by chunking the _transpose_ of the
   collection of collections.

   As indicated above, vfold's fold combiner is monoidal on vectors:
   it constructs a new vector from an l and r vector, and has identity
   [] (empty vector).  In line with this, vfold's reducer builds up
   new vectors from elements by conjing (f a1, ... an) onto a prior
   result or the combiner identity, [], as initial result.

   Packet granularity is determined automatically (base case or N=0)
   or supplied with N > 1 in signatures with N.  Automatic
   determination tries to balance significant work chunks (keep thread
   overhead low), with chunks that are small enough to have multiple
   instances per worker queue (supporting stealing by those workers
   that finish ahead of others).
  "
  ([f coll]
     (let [cores (.. Runtime getRuntime availableProcessors)
           workers (int (Math/floor (* 3/4 cores)))
           base (* 8 cores)
           n (max 2 (int (Math/floor (/ (count coll) (* 2 workers)))))
           n (min base n)]
       #_(println :>>N n)
       (vfold f n coll)))
  ([f n coll]
     (assert (integer? n)
             (str "VFOLD: Folding granularity N " n " must be an integer."))
     (if (< n 1)
       (vfold f coll)
       (r/fold n
               (fn
                 ([] [])
                 ([l r] (apply conj l r)))
               (fn[v x] (conj v (f x)))
               (vec coll))))
  ([f n coll & colls]
     (vfold (fn[v] (apply f v)) n (apply transpose coll colls))))

(defn xfold
  "Deprecated! Use vfold!!!"
  [& args] (apply vfold args))


(defn pxmap
  "Constrained pmap.  Constrain pmap to at most par threads.
   Generally, to ensure non degrading behavior, par should be
   <= (.. Runtime getRuntime availableProcessors).  It can be more,
   but if par >> availableProcessors, thrashing (excessive context
   switching) can become an issue.  Nevertheless, there are cases
   where having par be larger can reduce the ill effects of the
   partition problem.  NOTE: no effort is made to provide the true (or
   even a \"good\") solution to the partitioning of f over coll(s).

   Effectively, (pmap f (partition-all (/ (count coll) par) coll).
   Implicit doall on results to force execution.  For multiple
   collection variants, chunks the _transpose_ of the collection of
   collections.
  "
  ([f par coll]
     (if (= par 1)
       (map f coll)
       (apply concat
              (doall (pmap (fn[subset] (doall (map f subset)))
                           (partition-all (/ (count coll) par) coll))))))
  ([f par coll1 coll2]
     (if (= par 1)
       (map f coll1 coll2)
       (pxmap (fn[[x y]] (f x y)) par (transpose coll1 coll2))))
  ([f par coll1 coll2 & colls]
     (if (= par 1)
       (apply map f coll1 coll2 colls)
       (pxmap (fn[v] (apply f v)) par (apply transpose coll1 coll2 colls)))))

