(defproject forage "0.1.0"
  :description "Random foraging models in Clojure with MASON."
  :url "https://github.com/mars0i/forage"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.11.0"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 ;[org.apache.commons/commons-math3 "3.6.1"]
                 [org.apache.commons/commons-rng-core "1.4"]
                 [org.apache.commons/commons-rng-simple "1.4"]
                 [org.apache.commons/commons-rng-client-api "1.4"]
                 [org.apache.commons/commons-rng-sampling "1.4"]
                 [aerial.hanami "0.17.0"]
                 [techascent/tech.viz "6.00-beta-16-2"]
                 [io.github.nextjournal/clerk "0.5.346"]
                 [com.taoensso/nippy "3.1.1"] ; for preventing a problem with clerk's use of nippy
		;; NOTE oz MUST BE LISTED *AFTER* clerk (if clerk is present):
                 [metasoarous/oz "2.0.0-alpha5"]
                 [mason "20"]] ; just for Continuous2D

  :source-paths ["src/clj"]

  ;; Use (into [] (.getInputArguments (java.lang.management.ManagementFactory/getRuntimeMXBean)))
  ;; to check JVM params.

  ;; To use a profile with the repl, add "+" to the profile name:
  :profiles {:fullmason {:dependencies [[org.clojure/tools.cli "1.0.206"] ; command line processing
                                        [mars0i/masonclj "0.2.0"]
                                        [org.beanshell/bsh "2.0b4"]
                                        [com.lowagie/itext "1.2.3"] ; version that comes with MASON. Not in maven.org: [com.lowagie/itext "1.2"] 
                                        [org.jfree/jcommon "1.0.21"]
                                        [org.jfree/jfreechart "1.0.17"]
                                        [javax.media/jmf "2.1.1e"]]
                         :main ^:skip-aot forage.mason.core
                         :aot [forage.mason.foodspot
                               forage.mason.Sim
                               forage.mason.GUI
                               forage.mason.core]}

;; Usage tip: lein with-profile +production
             :production {;:dependencies [[generateme/fastmath "2.1.8"]
                          :jvm-opts ["-Xms4g"  ; initial heap
                                     "-Xmx16g" ; max heap
                                     "-XX:TieredStopAtLevel=4"]} ; 3X improvement

             ;; Usage tip: lein with-profile +production,profiling
             :profiling  {:dependencies [;[generateme/fastmath "2.1.8"]
                                         [criterium "0.4.6"]
                                         ;[com.clojure-goes-fast/jvm-hiccup-meter "0.1.1"]
                                         [com.clojure-goes-fast/clj-async-profiler "0.5.1"]]
                          :jvm-opts ["-Djdk.attach.allowAttachSelf"   ; for clj-async-profile: needed for JDK9+
                                     "-XX:+UnlockDiagnosticVMOptions" ; for clj-async-profiler
                                     "-XX:+DebugNonSafepoints"]}      ; for clj-async-profiler

             :notespace {:dependencies [[scicloj/notespace "4-alpha-21"]
                                        [org.scicloj/tempfiles "1-alpha2"]
                                        [org.scicloj/kindly "1-alpha3"]]
                         :repl-options {:nrepl-middleware [scicloj.notespace.v4.nrepl/middleware]}}}

;:repl-options {:init-ns forage.core}
  ;:global-vars {*warn-on-reflection* true}
  ;:aot [forage.Sim forage.GUI]
  ; SEE pasta/project.clj for other lines here I might want to include

  ;; TO INSTALL A NON-MAVENIZED JAR LOCALLY, uncomment lein-localrepo
  ;; below, run 
  ;;    lein localrepo install <jarfilename> <libname> <version num>'
  ;:plugins [[lein-localrepo "0.5.3"]
  ;          [lein-expand-resource-paths "0.0.1"]] ; allows wildcards in resource-paths (https://github.com/dchelimsky/lein-expand-resource-paths)
  )
