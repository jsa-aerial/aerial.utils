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
   no longer included in the standard Clojure ecosystem."

  (:require
   [clojure.java.io :as cjio :refer [reader writer]]
   [clojure.walk :as walk]
   [aerial.fs :as fs]
   [aerial.utils.coll :refer [concatv take-until drop-until]]
   [aerial.utils.string :as str]
   [aerial.utils.misc :refer [self-process-id runx]])

  (:import
   [java.io InputStream OutputStream]
   [java.nio ByteBuffer]
   [java.util Arrays]))


;;; ----------------------------------------------------------------
;;; Low level streaming helpers

(defn open-streaming-gzip
  "Open a gziped file stream on file given by file specification
  fspec (a string). The input/output mode is given by io: :in for
  input (reading), :out for output (writing)"
  [fspec io]
  (if (= io :in)
    (->> fspec clojure.java.io/input-stream
         java.util.zip.GZIPInputStream.)
    (->> fspec clojure.java.io/output-stream
         java.util.zip.GZIPOutputStream.
         clojure.java.io/writer)))

(defn read-stream
  "Read a the next (binary) chunk from input stream instrm into byte
  buffer buf. NOTE: this _modifies_ buf!"
  [^InputStream instrm, #^bytes buf]
  (let [n (.read instrm buf)]
    (if (neg? n)
      nil
      (Arrays/copyOfRange buf 0 n))))

(defn write-stream
  "Write a chunk of output data to output stream otstrm. data is
  typically a byte buffer representing whatever data is being written."
  [^OutputStream otstrm, data]
  (.write otstrm data))

(defn close-stream
  "Close the i/o stream strm."
  [strm]
  (.close strm))




;;; ----------------------------------------------------------------
;;; letio form support

(declare read-lines)

(def rwfl-syms
  (atom
   [#'clojure.java.io/reader
    #'clojure.java.io/writer
    #'aerial.utils.io/read-lines
    ;; NOT really sure what to do with open-streaming-gzip - if anything!
    #_#'aerial.utils.io/open-streaming-gzip]))

(def rdrwtrs-set
  (atom #{"read-lines" #_"open-streaming-gzip" "reader" "writer"}))

(def nss-set
  (atom #{(find-ns 'aerial.utils.io) (find-ns 'clojure.java.io)}))

(defrecord auioReader [fspec rdr])
(def auio-rdrs (atom {}))

(defn add-rdrwtr
  "Add the symbol RW to the set of reader and writer operations letio
  will look for and handle."
  ([rw]
   (swap! rdrwtrs-set (fn[cur x] (conj cur x)) (name rw))
   (swap! nss-set (fn[cur x] (conj cur x)) (find-ns (symbol (namespace rw))))
   (swap! rwfl-syms (fn[cur x] (conj cur x)) (find-var rw)))
  ([rw use-auioRdr]
   (add-rdrwtr rw)
   (swap! auio-rdrs assoc (find-var rw) true)))

(defn- rdrwtrs? [x]
  (@rdrwtrs-set x))

(defn- nss? [sym]
  (let [nss @nss-set]
    (nss (->> (resolve sym) meta :ns))))


(defn- split-bindings
  "Split bindings into two vectors [bvec rwbinds], where bvec contains
  the binding pairs that are not top level reader or writer bindings,
  and rwbinds are all the reader and writer bindings. Either bvec or
  rwbinds may be empty."
  [bindings]
  (let [toplevel #{#_"open-streaming-gzip" "writer", "reader"}
        {:keys [bvec rwbinds]}
        (->> bindings
             (partition-all 2)
             (group-by
              (fn[[b v]]
                (if (and (list? v) (toplevel (name (first v))))
                  :rwbinds
                  :bvec))))]
    [(apply concat bvec) (apply concat rwbinds)]))

(defn- xform-sexp
  "Walk the expressions in sexp (body, binding vector, ...) and for
  each expression whose first place symbol satisfies rdrwtrs? and
  nss?, lift the corresponding reader / writer to a binding pair and
  replace the expression with the binding symbol (gensym'd). Updates
  fds (and atom vector) with the binding pairs so constructed"
  [sexp fds deps]
  (let [[rdr wtr & rwseq-forms] @rwfl-syms
        rwseq-forms (set rwseq-forms)]
    (walk/postwalk
     (fn[f]
       (let [op (if (list? f) (first f) f)]
         (if (and (list? f)
                  (symbol? op)
                  (rdrwtrs? (name op))
                  (nss? op))
           (let [rop (resolve op)
                 gfd (gensym "fd-")
                 rw (if (= rop wtr)
                      'clojure.java.io/writer
                      'clojure.java.io/reader)]
             (swap! fds conj gfd `(~rw ~(second f)))
             (swap! deps assoc gfd (second f))
             (if (rwseq-forms rop)
               (if (@auio-rdrs rop)
                 `(~(first f) (auioReader. ~(second f) ~gfd) ~@(drop 2 f))
                 `(~(first f) ~gfd ~@(drop 2 f)))
               gfd))
           f)))
     sexp)))

(defn get-xform-rdrwrtr-bindings
  "Transform the binding vector bvec by reaching into any value
  form (right hand side of pair) which has a reader or writer form (as
  from clojure.javas.io) and lifting that form out, creating a binding
  for it (with a gensym for the fd bind symbol - left hand side of
  binding). Top level reader or writer bindings are left as is. Any
  read-lines binding or use in a subform, has its argument lifted into
  an explicit reader binding. Returns the xformed binding vector."
  [bvec]
  (let [fds (atom [])
        deps (atom {}) ; Not actually used at present
        ;; Number the original bvec ordering
        ordering (map #(vector %1 %2) (partition-all 2 bvec) (iterate inc 1))
        [bvec rwbinds] (split-bindings bvec)
        rwbinds (partition-all 2 rwbinds)
        lclrw-syms (map first rwbinds) ; get local rw syms
        b1 (xform-sexp bvec fds deps)  ; get xformed bvec with new fd-s
        new-pairs (concatv rwbinds (partition-all 2 b1))
        ;; Number each new pair with original placement number
        numbered-pairs (reduce (fn[R np]
                                 (let [n (some #(when (= (first np) (ffirst %))
                                                  (second %))
                                               ordering)]
                                   (conj R (vector n np))))
                               [] new-pairs)
        ;; Sort the new pairs by placement
        newbvec (mapcat second (sort-by first numbered-pairs))]
    [@fds newbvec lclrw-syms deps]))

(defn get-xform-body-bindings
  "Walk sexps in body and for each that satisfiles rdrwtrs? and nss?,
  lift the corresponding reader / writer to a binding pair (gensym'd
  symbol with r/w value) and replace the r/w subexpr with the binding
  var. Returns a pair [fds xbody], where fds is a binding vector of
  the new bindings (could be empty) and the xform'd body."
  [body deps]
  (let [fds (atom [])
        [rdr wtr rdlines] @rwfl-syms
        body (xform-sexp body fds deps)]
    [@fds body deps]))

(defn assemble-bindings
  "Assemble the fd-bindings created from the original binding vector,
  the original binding vector bindings, and the fd-bindings created
  from the letio body. Basically need to ensure an ordering such that
  any values required in the fds-* bindings are placed above those
  bindings and above their using forms of fds-* bindings are placed
  below those bindings."
  [bv-fds bvec body-fds deps]
  (let [lvs? (into #{} (mapv first (partition-all 2 bvec)))]
    (loop [fds bv-fds
           bvec bvec
           done {}
           bv []]
      (let [[gs [rw arg] :as x] (take 2 fds)
            [s v :as y] (take 2 bvec)]
        (cond
          (= gs nil)
          (concatv (apply concat bv) bvec body-fds)
          (= s nil)
          (concatv (apply concat bv) fds body-fds)

          (or (string? arg)     ; rdr/wtr on string arg
              (not (lvs? arg))  ; on symbol NOT in this let!
              (done arg))       ; Dependency already in bindings
          (recur (drop 2 fds)
                 bvec
                 done
                 (conj bv x))
          (= arg s)             ; New dependency
          (recur (drop 2 fds)
                 (drop 2 bvec)
                 (assoc done s arg)
                 (conj bv y x))
          :else
          (recur fds
                 (drop 2 bvec)
                 done
                 (conj bv y)))))))


(defmacro letio
  "Like let, but will close all file descriptors opened in the
  bindings (and implicitly in body) and will check for certain lazy
  readers and will reach into the call and pull out the designated
  file, create a binding of a reader for it to a generated sym, and
  replace the file in the call with the sym."
  [bindings & body]
  (let [[bv-fds bvec lclrws deps] (get-xform-rdrwrtr-bindings bindings)
        [body-fds body deps] (get-xform-body-bindings body deps)
        fdsyms (concatv lclrws
                        (map first (partition-all 2 (concat bv-fds body-fds))))
        bindings (assemble-bindings bv-fds bvec body-fds deps)]
    `(let ~bindings
       (try
         ~@body
         (finally
           (doseq [fd# (reverse ~fdsyms)]
             (when (instance? java.io.Closeable fd#)
               (. fd# close))))))))


;;; ----------------------------------------------------------------
;;; Abstracted readers and writers


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




;;; ----------------------------------------------------------------
;;; Some GC level file descriptor functions


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
