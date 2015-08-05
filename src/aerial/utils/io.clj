;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                            U T I L S . I O                               ;;
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

(ns aerial.utils.io

  "Various supplementary I/O functions and macros. Some new, some
   recapitulations of previous extremely useful functionality that is
   no longer included in the dstandard Clojure ecosystem."

  (:require
   [clojure.java.io :as cjio :refer [reader writer]]
   [clojure.string :as str :refer [split]]
   [aerial.fs :as fs]
   [aerial.utils.misc :refer [self-process-id runx]]
   ))


(defn get-pairs-with-fds [pairs]
  (let [rdrwtrs #{"read-lines"
                  "reader"
                  "writer"}
        nss #{(find-ns 'aerial.utils.io)
              #_(find-ns 'clojure.java.io)}]
    (reduce (fn[[P fds] [v x]]
              (if (and (list? x)
                       (symbol? (first x))
                       (rdrwtrs (name (first x)))
                       (nss (->> (resolve (first x)) meta :ns)))
                (let [r (second x)
                      prefix (if (symbol? r)
                               (name r)
                               (-> r (str/split #"(/|\\)")
                                   last (str/replace #"\." "")))
                      rs (gensym prefix)]
                  [(conj P [rs `(if (instance? java.io.Reader ~r)
                                  ~r
                                  (reader ~r))]
                         [v  `(~(first x) ~rs)])
                   (conj fds rs)])
                [(conj P [v x]) fds]))
            [[] []] pairs)))

(defmacro letio
  "Like let, but will close all file descriptors opened in the
   bindings and will check for certain lazy readers and will reach
   into the call and pull out the designated file, create a binding of
   a reader for it to a generated sym, and replace the file in the
   call with the sym.
  "
  [bindings & body]
  (let [pairs (->> bindings
                   (partition-all 2))
        [npairs fds] (get-pairs-with-fds pairs)
        syms (vec (map first npairs))
        bindings (vec (apply concat npairs))]
    `(let ~bindings
       (try
         ~@body
         (finally
          (doseq [fd# (reverse ~syms)]
            (when (instance? java.io.Closeable fd#)
              (. fd# close))))))))


(defn write-lines
  "Writes lines (a collection of strings) to f, separated by newlines.
   f is opened with writer, and automatically closed at the end of the
   sequence."
  [f lines]
  (with-open [^java.io.BufferedWriter writer (writer f)]
    (loop [lines lines]
      (when-let [line (first lines)]
        (.write writer (str line))
        (.newLine writer)
        (recur (rest lines))))))

(defn read-lines
  "Lazily read lines from text file f by creating a lazying seq over a
   buffered reader.  Will automatically close the reader after all
   lines are read, but will leak the associated file descriptor if the
   returned lazy seq is dropped before file is fully consumed.  Hence,
   this should always be used inside a letio block"
  [f]
  (letfn [(readone [^java.io.BufferedReader rdr]
                   (lazy-seq
                    (if-let [line (.readLine rdr)]
                      (cons line (readone rdr))
                      (.close rdr))))]
    (if (instance? java.io.Reader f)
      (readone f)
      (readone (reader f)))))


(defmacro with-out-writer
  "Opens a writer on f, binds it to *out*, and evalutes body.
   Anything printed within body will be written to f."
  [f & body]
  `(with-open [stream# (writer ~f)]
     (binding [*out* stream#]
       ~@body)))

(defmacro with-out-append-writer
  "Like with-out-writer but appends to file."
  [f & body]
  (with-open [stream# (writer "/tmp/test.txt" :append true)]
    (binding [*out* stream#]
      ~@body)))

(defmacro with-in-reader
  "Opens a PushbackReader on f, binds it to *in*, and evaluates body."
  [f & body]
  `(with-open [stream# (PushbackReader. (reader ~f))]
     (binding [*in* stream#]
       ~@body)))


(defn fd-use
  "Returns count of current file descriptors held.  Often (typically)
   this can be larger (indeed, _much_ larger) than the count actually
   being used.  This is due to the fact that things like io/read-lines
   or equivalent will not close the file unless it is read in
   entirety.  There are many cases where reading only the first few
   lines is what is needed.  Further, reading them all just to get a
   few is completely loses all advantage of lazy evaluation (not to
   mention possible memory blow up)

   *** NOTE: (bug!) UNIX only.  Uses /proc/<pid>/fd to determine all
       descriptors currently held by the process (the JVM here).
  " []
  (let [pid (self-process-id)]
    (->> pid (#(fs/join "/proc" % "fd"))
         (runx "ls") (str/split #"\n") count)))

(defn force-gc-finalize
  "Tries to force a GC in order to finalize no longer used OS
   resources - in particular file descriptors (see fd-use).

   *** NOTE: as described by System.gc(), which we use here, this may
       or may not actually accomplish the task at hand as it is only a
       'suggestion' to the collector to run.  If it does run, all no
       longer used resoureces should be finalized, i.e., closed and
       returned.
  "
  []
  (System/gc))



;;; ----------------------------------------------------------------
;;; Some simple text file processing utils.


(defn reduce-file
  "Reduce text file denoted by FNAME (filespec/File obj) using
   function FUNC per line and reduction seed accumulator ACC.  FUNC is
   a function of two arguments: first is the current line from file
   and second is the current value of ACC."
  [fname func acc]
  (reduce func acc (read-lines fname)))


(defn process-line [acc line]
  (reduce #(assoc %1 %2 (inc (get %1 %2 0))) acc (str/split #"," line)))


(defmacro do-text-file [[in & options] & body]
  `(doseq [~'$line (read-lines ~in)]
     (do ~@body)))


(defmacro do-text-to-text [[in out] & body]
  `(with-out-writer ~out
     (doseq [~'$line (read-lines ~in)]
       (let [result# (do ~@body)]
         (when result#
           (println result#))))))

