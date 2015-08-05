(defproject aerial.utils "1.0.0"
  :description "Utility 'tool belt' of functions for common tasks; trees; clustering; probability, stats, and information theory; et.al."
  :url "https://github.com/jsa-aerial/aerial.utils"
  :license {:name "The MIT License (MIT)"
            :url  "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :dependencies
  [[org.clojure/clojure "1.7.0"]
   [org.clojure/math.numeric-tower "0.0.4"]
   [org.clojure/math.combinatorics "0.1.1"]
   [aysylu/loom "0.5.4"] ; graphs and graph algos
   [clj-stacktrace "0.2.8"]
   [slingshot "0.12.2"]
   [me.raynes/conch "0.8.0"]
   [aerial.fs "1.1.3"]
   ]

  :repl-options
  {:init (do (require 'swank.swank) (swank.swank/start-repl 4023))}

  :scm {:name "git"
        :url "https://github.com/jsa-aerial/aerial.utils"}
  ;:aot :all
  )
