(defproject aerial.utils "1.2.0"
  :description "Utility 'tool belt' of functions for common tasks; trees; clustering; probability, stats, and information theory; et.al."
  :url "https://github.com/jsa-aerial/aerial.utils"
  :license {:name "The MIT License (MIT)"
            :url  "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :dependencies
  [[org.clojure/clojure "1.11.3"]
   [org.clojure/math.numeric-tower "0.1.1"]
   [org.clojure/math.combinatorics "0.3.2"]
   [aysylu/loom "0.5.4"] ; graphs and graph algos
   [clj-stacktrace "0.2.8"]
   [slingshot "0.12.2"]
   [net.apribase/clj-dns "0.1.0"] ; reverse-dns-lookup
   [me.raynes/conch "0.8.0"]
   [bigml/sampling "3.2"] ; random sampling (simple, reservoir, stream)
   [aerial.fs "1.1.5"]
   ]

  :scm {:name "git"
        :url "https://github.com/jsa-aerial/aerial.utils"}
  ;:aot :all
  )
