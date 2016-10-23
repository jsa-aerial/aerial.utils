;;--------------------------------------------------------------------------;;
;;                                                                          ;;
;;                               M I S C                                    ;;
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

(ns aerial.utils.misc

  "General utility functions and macros.  Basically these resources
   are fairly general and intended to be usable on most any part of
   most any project"

  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [cl-format]]

            [slingshot.slingshot :refer [throw+ try+ get-throw-context]]
            [clj-stacktrace.repl :as cljst]

            [me.raynes.conch :as sh]
            [me.raynes.conch.low-level :as shl]
            [clj-dns.core :as rdns] ; reverse dns

            [aerial.fs :as fs]
            [aerial.utils.string :as str]
            [aerial.utils.coll :refer [positions]])

  (:import [java.util Date Calendar Locale]
           [java.util Arrays]
           [java.net NetworkInterface Inet4Address Inet6Address]
           java.lang.Thread
           [java.text SimpleDateFormat]
           java.lang.management.ManagementFactory))



;;; -----------------------------------------------------------------
;;; Miscellaneous changes/additions/"fixes" to various operations that
;;; are either already in Clojure or in bits of contribs or probably
;;; _will_ be in future (at which point these can be retired...)


(defmacro defparameter
  "def with auto declaration of symbol SYM to be dynamic"
  ([sym val]
     `(def ~(with-meta sym (assoc (or (meta sym) {}) :dynamic true)) ~val))
  ([mmap sym val]
     (let [sym-meta (or (meta sym) {})
           mmap (if (string? mmap)
                  (assoc sym-meta :doc mmap)
                  (into sym-meta mmap))]
       `(def ~(with-meta sym (assoc mmap :dynamic true)) ~val))))

(defn add-meta
  [x m]
  (with-meta x (merge (or (meta x) {}) m)))




(defn timefn
  "Time the application of F (a function) to ARGS (any set of args as
   expected by f.  Returns a two element vector [ret time] where,

   ret is the return value of f

   time is the time f took to compute ret in milliseconds
  "
  [f & args]
  (let [start-time (. java.lang.System (nanoTime))
        ret (apply f args)]
    [ret (/ (double (- (. java.lang.System (nanoTime)) start-time))
            1000000.0)]))

(defparameter {:private true} *uid* (atom (.getTime (Date.))))


(defn gen-uid
  "Generates a unique integer ID based on universal time."
  []
  (swap! *uid* inc))

(defn gen-kwuid
  "Generates a unique keyword id whose name is the str of gen-uid"
  []
  (keyword (str (gen-uid))))


(defn sleep [msecs]
  (Thread/sleep msecs))

(defn str-date
  ([] (str-date (Date.) "yyyy-MM-dd HH:mm:ss"))
  ([fm] (str-date (Date.) fm))
  ([d fm] (.format (SimpleDateFormat. fm) d)))


(declare runx)

;;; Move to use
;;; (. (ManagementFactory/getOperatingSystemMXBean) getSystemLoadAverage)
;;; Instead or in addition????
(defn cpu-use
  "Obtain and return cpu utilization.  Requires the 'top' command is
   available, and takes the second of two samplings (first samples
   from top are typically inaccurate).  INFO indicates the type of
   information returned:

   :idle, just the avg % idle, this is the default
   :used, just the avg % used
   :both, both idle and used

   In  all cases  uses  aggregate cpu  utilization  over SMP  systems.
   Values are returned as Doubles,  or for :both, a two element vector
   of Doubles [idle used].
  "
  [& {:keys [info] :or {info :idle}}]
  (let [idle (->> (runx "top" "-n" "2" "-b" "-d" "0.01" "-p" "1")
                  (str/split #"\n") (filter #(re-find #"^Cpu" %))
                  last (str/split #"\s*,\s*") (filter #(re-find #"%id" %))
                  first (str/split #"\s*%\s*") first Double.)
        use (- 100.0 idle)]
    (case info
          :idle idle
          :used use
          :both [idle use]
          idle)))

(defn self-process-id
  "Return the process id of the running JVM.  Uses ManagementFactory
   getRuntimeMXBean, which has a getName method.  The getName method
   will _likely_ return name as: <pid>@<host-name>, as a string.  This
   method then pulls off the pid from this and returns it as a string.
  "
  []
  (->> (. (ManagementFactory/getRuntimeMXBean) getName)
       (str/split #"@") first))


(defn host-ipaddress
  "Obtain the host IP addresses (:inet4 and :inet6) for the machine on
   which we are running. Returns a map of interfaces filtered for
   being up, non virtual, and not the loopback, and their associated
   vector of inet6 and inet4 addresses. For example:

   {:eth0 {:inet6 [\"fe80:0:0:0:225:90ff:fe21:66d2%2\"],
           :inet4 [\"136.167.54.82\"]}}
  "
  []
  (->> (NetworkInterface/getNetworkInterfaces)
       enumeration-seq
       (filter #(and (.isUp %) (not (.isVirtual %)) (not (.isLoopback %))))
       (map #(vector (.getName %) %))
       (mapv (fn[[n intf]]
               [(keyword n)
                (->> intf .getInetAddresses enumeration-seq
                     (group-by #(cond (instance? Inet4Address %) :inet4
                                      (instance? Inet6Address %) :inet6
                                      :else :unknown))
                     (reduce (fn[M [k ifv]]
                               (assoc M k (mapv #(.getHostAddress %) ifv)))
                             {})
                     )]))
       (into {})))

(defn host-dns-name []
  (-> (host-ipaddress) :eth0 :inet4 first rdns/hostname))


;;; ----------------------------------------------------------------
;;; Slightly abstracted things from Java that are often used from/in
;;; many contexts...

(defn sys-property [prop-name]
  "Return the System property with name PROP-NAME (a string)"
  (System/getProperty prop-name))

(defn sys-properties []
  "Return the set of System properties as a map"
  (System/getProperties))

(defn classpath []
  (str/split #":" (sys-property "java.class.path")))

(defn getenv
  "Return the value of the environment variable EV (a string).
   Optionally, (no argument variant) return a map of all the
   environment variables.
  "
  ([ev] (System/getenv ev))
  ([] (System/getenv)))


;;; ----------------------------------------------------------------
;;; Definition macros and helpers for providing proper keyword
;;; arguments and maintaining doc string, user meta data, and special
;;; pre and post condition meta data.  Actually, this is just curried
;;; into the pre and post processing of the body.  This should really
;;; be resubmitted to clojure.contrib.def as the defnk and helper
;;; there do not account for pre and post conditions.


(defn name-with-attrs [name macro-args]
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. Also handles pre/post conditions. If the
   first macro argument is a string, it is added as a docstring to name and
   removed from the macro argument list. If afterwards the first macro argument
   is a map, its entries are added to the name's metadata map and the map is
   removed from the macro argument list. If0 the first form past the arg list
   is a map with :pre and/or :post, the map is removed and the pre and post
   vectors of forms are separated out.  The return value is a vector containing
   the name with its extended metadata map, the args form, followed by the pre
   and post forms (if any) and lastly, the body forms."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
        [attr macro-args]      (if (map? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [{} macro-args])
        attr                   (if docstring
                                 (assoc attr :doc docstring)
                                 attr)
        attr                   (if (meta name)
                                 (conj (meta name) attr)
                                 attr)
        [pre-post args body]   (if (and (map? (second macro-args))
                                        (or ((second macro-args) :pre)
                                            ((second macro-args) :post)))
                                 [(second macro-args)
                                  (first macro-args)
                                  (drop 2 macro-args)]
                                 [nil (first macro-args) (drop 1 macro-args)])
        [pre post]             (if pre-post
                                 [(pre-post :pre) (pre-post :post)]
                                 [nil nil])]
    [(with-meta name attr) args pre post body]))


(defmacro defnk [name & args-body]
  "Same as DEFN, but supports keyword style arguments.  Adapted and modified
   from Rich Hickey's original on google groups Clojure group, to support doc
   strings, meta data, and pre/post conditions."
  (let [[sym args pre post body] (name-with-attrs name args-body)
        pos-keys (split-with (complement keyword?) args)
        ps (pos-keys 0)
        ks (apply array-map (pos-keys 1))
        gkeys (gensym "gkeys__")
        letk (fn [ke]
               (let [k (key ke)
                     ;; The next oddity is due to some weird bug in
                     ;; Clojure 1.2 - for some reason in this context
                     ;; name returns nil despite k being a keyword!??!
                     kname (symbol (if (name k) (name k) (subs (str k) 1)))
                     v (val ke)]
                 `(~kname (if (contains? ~gkeys ~k) (~gkeys ~k) ~v))))]
    `(defn ~sym [~@ps & k#]
       (let [~gkeys (apply hash-map k#)
             ~@(apply concat (map letk ks))]
         ~@(if pre
             `((assert ~(conj (seq pre) 'and)))
             ())
         (let [res# (do ~@body)]
           ~@(if post
               `((assert ~(conj (map (fn [v] (replace `{% res#} v))
                                      (seq post))
                                'and)))
               ())
           res#)))))




;;; ----------------------------------------------------------------
;;; Some helpers for enhanced / simpler exception raising and
;;; handling.


;;; All of this is based on the support in slingshot lib and
;;; clojure.stacktrace.
;;;

(defmacro raise
  "Wraps throw+ for throwing a map object.  Ensures that (count args)
   is even, then places pairs into a map which is given to throw+
  "
  [& args]
  (let [m (into {} (map vec (partition 2 args)))]
    `(throw+ ~m)))

(defmacro handler-case
  "Hide some try+ details.  Form is the expression that is exception
   protected.  Cases are handler arms of the form [c e & body].  Where
   c is the predicate for the case arm (with out preceding 'catch'), e
   is the variable (symbol) to hold the exception object and body is
   the set of forms to execute.  Within body, the captured symbol
   contextMap is holds the exception context map.
  "
  [form & cases]
  `(try+
    ~form
    ~@(map (fn [[c# e# & body#]]
             `(catch ~c# ~e#
                (let [~'contextMap ~'&throw-context]
                  ~@body#)))
           cases)))


(defmacro with-handled
  "Wraps FORM in a slingshot try+ with catch arms for each condition
   in CONDITIONS. Each handle arm catches the condition C and prints a
   stack trace for it.  Hence, while this catches the conditions, it
   stops execution.  For catch and continue see CATCH-ALL
  "
  [form & conditions]
  `(try+
    ~form
    ~@(map (fn [c]
             `(catch [:type ~c] x#
                (cljst/pst
                 (:throwable ~'&throw-context))))
           conditions)))


(defmacro with-ckd
  "FORMS wrapped by handlers and last ditch try/catch for standard exceptions.
   For conditions, the condition info meta data map is returned. For exceptions,
   the exception is converted to a string representation and that is returned."
  [& forms]
  `(try+
    (do ~@forms)
    (catch #(or (map? %) (set? %)) c#
      ~'&throw-context)
    (catch Exception e#
      (with-out-str
        (print e#)))))


(defmacro catch-all
  "FORMS wrapped by handlers and last ditch try/catch for standard exceptions.
   For conditions, the condition info meta data map is returned. For exceptions,
   the exception is converted to a string representation and that is returned."
  [& forms]
  `(with-ckd ~@forms))


;;; ----------------------------------------------------------------
;;; Some helpers for running external programs.  In particular,
;;; running them while ensuring they actually terminate normally.

(defn- sh-result [proc]
  (let [output (proc :out)
        errout (proc :err)
        outfn (proc :outfn)
        errfn (proc :errfn)
        result {:out (if outfn
                       (do (future (outfn output)) (proc :outfile))
                       (shl/stream-to-string proc :out))
                :err (if errfn
                       (do (errfn errout) (proc :outfile))
                       (shl/stream-to-string proc :err))
                :exit (shl/exit-code proc)}]
    result))

(defn runx
  "RUNs an eXternal program PROGRAM (a string naming the program executable),
   passing it the (typically, _command line_) arguments ARGS.  ARGS is either
   a single vector or list of the arguments, or a sequential list of the
   arguments given to the function as further arguments after the program."
  [program & args]
  (let [the-args (vec (if (and (= 1 (count args))
                               (sequential? (first args)))
                        (first args)
                        args))

        i (first (positions  #(= :> %1) the-args))
        [the-args stdout-file] (if (not i)
                                 [the-args nil]
                                 [(vec (concat (take i the-args)
                                               (subvec the-args (+ 2 i))))
                                  (the-args (+ 1 i))])

        i (first (positions #(= :?> %1) the-args))
        [the-args stderr-file] (if (not i)
                                 [the-args nil]
                                 [(concat (take i the-args)
                                          (subvec the-args (+ 2 i)))
                                  (the-args (+ 1 i))])

        i (first (positions #(= :binary %1) the-args))
        [the-args binary?] (if (not i)
                             [the-args nil]
                             [(concat (take i the-args)
                                      (subvec the-args (+ 1 i)))
                              true])

        write-out (fn[stdout]
                    (if binary?
                      (let [buf (byte-array (* 64 1024))
                            out (io/output-stream (fs/fullpath stdout-file))]
                        (loop [n (.read stdout buf)]
                            (if (neg? n)
                              (.close out)
                              (do (.write out (Arrays/copyOfRange buf 0 n))
                                  (recur (.read stdout buf))))))
                      (with-open [rdr (io/reader stdout)
                                  wtr (io/writer (fs/fullpath stdout-file))]
                        (doseq [l (line-seq rdr)] (.write wtr l)))))

        write-err (fn[stderr]
                    (with-open [rdr (io/reader stderr)
                                wtr (fs/fullpath stderr-file)]
                      (doseq [l (line-seq rdr)] (.write wtr l))))

        proc (apply shl/proc program the-args)
        proc (if stdout-file
               (assoc proc :outfn write-out :outfile stdout-file)
               proc)
        proc (if stderr-file
               (assoc proc :errfn write-err :errfile stderr-file)
               proc)

        result (sh-result proc)]

    (when (not= 0 (result :exit))
      (if stderr-file
        :err-in-result
        (raise :type :program-failed
               :exit (result :exit)
               :pgm program :err (result :err)
               :args the-args)))
    (if stdout-file
      result
      (result :out))))
