;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                       U T I L S . S T R I N G                            ;;
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

(ns aerial.utils.string

  "Various supplementary string functions not included in the standard
   Clojure ecosystem or some that are but which now have broken
   argument order in terms of threading. Most of this stuff really
   should be in clojure.string, but for unknown reasons isn't."

  (:refer-clojure
   :exclude [take replace drop butlast partition map
             contains? get repeat reverse partial])
  (:require
   [clojure.string :as str])

  (:import (java.util.regex Pattern)))


(defn string-less?
  "Case insensitve string comparison.  Usable as a sort comparator"
  [l r]
  (neg? (.compareToIgnoreCase l r)))

(defn string-greater?
  "Case insensitve string comparison.  Usable as a sort comparator"
  [l r]
  (pos? (.compareToIgnoreCase l r)))

(defn string-equal?
  "Case insensitve string comparison.  Usable as a sort comparator"
  [l r]
  (zero? (.compareToIgnoreCase l r)))


(defn intstg?
  "Test and return whether S is a string of only digits 0-9.  If so,
  return generalized boolean else return nil/false"
  [s]
  (let [hit (re-find #"[0-9]+" s)]
    (and hit (= hit s))))


(defn ^String take
  "Take first n characters from s, up to the length of s."
  [^long n ^String s]
  (if (< (count s) n)
    s
    (.substring s 0 n)))

(defn ^String drop
  "Drops first n characters from s.  Returns an empty string if n is
  greater than the length of s."
  [^long n ^String s]
  (if (< (count s) n)
    ""
    (.substring s n)))

(defn ^String butlast
  "Returns s without the last n characters.  Returns an empty string
  if n is greater than the length of s."
  [^long n ^String s]
  (if (< (count s) n)
    ""
    (.substring s 0 (- (count s) n))))

(defn ^String tail
  "Returns the last n characters of s."
  [^long n ^String s]
  (if (< (count s) n)
    s
    (.substring s (- (count s) n))))

(defn ^String repeat
  "Returns a new String containing s repeated n times."
  [^long n ^String s]
  (apply str (clojure.core/repeat n s)))


(defn ^String replace
  "Replaces all instance of match with replacement in s.

   match/replacement can be:

   string / string
   char / char
   pattern / (string or function of match).

   See also replace-first.

   The replacement is literal (i.e. none of its characters are treated
   specially) for all cases above except pattern / string.

   For pattern / string, $1, $2, etc. in the replacement string are
   substituted with the string that matched the corresponding
   parenthesized group in the pattern.  If you wish your replacement
   string r to be used literally, use (re-quote-replacement r) as the
   replacement argument.  See also documentation for
   java.util.regex.Matcher's appendReplacement method.

   Example:
   (clojure.string/replace \"Almost Pig Latin\" #\"\\b(\\w)(\\w+)\\b\" \"$2$1ay\")
   -> \"lmostAay igPay atinLay\""
  [match replacement ^CharSequence s]
  (str/replace s match replacement))

(defn replace-re
  "Replaces all matches of re with replacement in s."
  [^Pattern re ^String replacement ^String s]
  (.replaceAll (re-matcher re s) replacement))

(defn ^String replace-first
  "Replaces the first instance of match with replacement in s.

   match/replacement can be:

   char / char
   string / string
   pattern / (string or function of match).

   See also replace.

   The replacement is literal (i.e. none of its characters are treated
   specially) for all cases above except pattern / string.

   For pattern / string, $1, $2, etc. in the replacement string are
   substituted with the string that matched the corresponding
   parenthesized group in the pattern.  If you wish your replacement
   string r to be used literally, use (re-quote-replacement r) as the
   replacement argument.  See also documentation for
   java.util.regex.Matcher's appendReplacement method.

   Example:
   (clojure.string/replace-first \"swap first two words\"
                                 #\"(\\w+)(\\s+)(\\w+)\" \"$3$2$1\")
   -> \"first swap two words\""
  [match replacement ^CharSequence s]
  (str/replace-first s match replacement))


(defn split
  "Splits string on a regular expression.  Optional argument limit is
  the maximum number of splits. Not lazy. Returns vector of the splits."
  ([^Pattern re ^CharSequence s]
     (str/split s re))
  ([^Pattern re limit ^CharSequence s]
     (str/split s re limit)))


(defn ^String map
  "Apply f to each element of coll, concatenate all results into a
  String."
  [f coll]
  (apply str (map f coll)))

(defn partition-stg
  "Returns a sequence of strings of n chars each, at offsets step
   apart. If step is not supplied, defaults to n, i.e. the partitions
   do not overlap. If a pad collection (a string or other collection
   of chars) is supplied, use its characters as necessary to complete
   last partition upto n characters. In case there are not enough
   padding chars, return a partition with less than n characters."
  ([n stg]
     (partition-stg n n stg))
  ([n step stg]
     (partition-stg n step "" stg))
  ([n step pad stg]
     (let [pad (if (string? pad) pad (str/join "" pad))
           stg (str stg pad)]
       (loop [s stg
              sv []]
         (if (= s "")
           sv
           (recur (drop step s)
                  (conj sv (take n s))))))))


(defn codepoints
  "Returns a sequence of integer Unicode code points in s.  Handles
  Unicode supplementary characters (above U+FFFF) correctly."
  [^String s]
  (let [len (.length s)
        f (fn thisfn [^String s i]
            (when (< i len)
              (let [c (.charAt s i)]
                (if (Character/isHighSurrogate c)
                  (cons (.codePointAt s i) (thisfn s (+ 2 i)))
                  (cons (int c) (thisfn s (inc i)))))))]
    (lazy-seq (f s 0))))
