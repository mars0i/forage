(defproject forage "0.1.0"
  :description "Random foraging models in Clojure with MASON."
  :url "https://github.com/mars0i/forage"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.apache.commons/commons-math3 "3.6.1"]    ; NOTE USING DIFFERENT INCOMPATIBLE
                 [org.apache.commons/commons-rng-core "1.5"]   ; VERSIONS OF Apache Commons math
                 [org.apache.commons/commons-rng-simple "1.5"] ; libs because of nonoverlapping features
                 [org.apache.commons/commons-rng-client-api "1.5"]
                 [org.apache.commons/commons-rng-sampling "1.5"]
                 ;[generateme/fastmath "2.1.8"]
                 ;[generateme/fastmath "2.1.9-SNAPSHOT"]
                 [generateme/fastmath "2.2.2-SNAPSHOT"]
                 [com.cnuernber/ham-fisted "2.010"]
                 ; [org.jcuda/jcuda-natives "11.8.0"]
                 ; [org.jcuda/jcublas-natives "11.8.0"]
                 ; [uncomplicate/neanderthal "0.46.0"]
                 [org.flatland/ordered "1.15.10"] ; for ordered-set
                 [net.mikera/core.matrix "0.63.0"]
                 [aerial.hanami "0.17.0"]
                 ; [dk.ative/docjure "1.19.0"] ; Excel file creation functions
                 [techascent/tech.ml.dataset "7.012"]
                 [techascent/tech.viz "6.00-beta-16-2"]
                 [scicloj/tablecloth "7.012"]
                 [org.scicloj/clay "2-alpha35"]
                 [io.github.nextjournal/clerk "0.5.346"]
                 ;[com.taoensso/nippy "3.1.1"] ; for preventing a problem with clerk's use of nippy
                 ;; NOTE oz MUST BE LISTED *AFTER* clerk (if clerk is present):
                 [metasoarous/oz "2.0.0-alpha5"]
                 [cljplot "0.0.2a-SNAPSHOT"]
                 ;[clojure2d "1.4.4"] ; not required for cljplot, but allows additional choices
                 ;[metasoarous/darkstar "0.1.0"] ; require as applied-science.darkstar (fork of original applied-science/darkstar, which isn't on Clojars)
                 [mason "21"]
                ]

  :source-paths ["src/clj"]
  :java-source-paths ["src/java"] ; such sources will be compiled; apparently there's no analog in deps.edn

  ; :plugins [[cider/cider-nrepl "0.24.0"]] ; FOR CONJURE

  ;; TIP:
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


             ;; -Xss specifies per-thread max stack size.  If you set it too 
             ;; small (< 144K) or too large (> 1G), java won't run.

             ;; Usage tip: lein with-profile +production
             :production {;:dependencies [[generateme/fastmath "2.1.8"]
                          :jvm-opts ["-Xms4g"  ; initial heap
                                     "-Xmx18g" ; max heap
                                     "-Xss1g"  ; max per-thread stack size (s/b smaller?)
                                     "-XX:TieredStopAtLevel=4"]} ; 3X improvement

             ;; For use with my MBA:
             ;; Usage tip: lein with-profile +smallproduction repl
             :smallproduction {;:dependencies [[generateme/fastmath "2.1.8"]
                               :jvm-opts ["-Xms4g" ; initial heap
                                          "-Xmx8g" ; max heap
                                          "-Xss1g" ; max per-thread stack size (s/b smaller?)
                                          "-XX:TieredStopAtLevel=4"]} ; 3X improvement

             ;; Usage tip: lein with-profile +[small]production,+profiling repl
             :profiling  {:dependencies [;[generateme/fastmath "2.1.8"]
                                         [criterium "0.4.6"]
                                         ;[com.clojure-goes-fast/jvm-hiccup-meter "0.1.1"]
                                         [com.clojure-goes-fast/clj-async-profiler "0.5.1"]]
                          :jvm-opts ["-Djdk.attach.allowAttachSelf"   ; for clj-async-profile: needed for JDK9+
                                     "-XX:+UnlockDiagnosticVMOptions" ; for clj-async-profiler
                                     "-XX:+DebugNonSafepoints"]}}     ; for clj-async-profiler

  ;; TO INSTALL A NON-MAVENIZED JAR LOCALLY, uncomment lein-localrepo
  ;; below, run 
  ;;    lein localrepo install <jarfilename> <libname> <version num>'
  ;:plugins [[lein-localrepo "0.5.3"]
  ;          [lein-expand-resource-paths "0.0.1"]] ; allows wildcards in resource-paths (https://github.com/dchelimsky/lein-expand-resource-paths)

)


;:repl-options {:init-ns forage.core}
  ;:global-vars {*warn-on-reflection* true}
  ;:aot [forage.Sim forage.GUI]
  ; SEE pasta/project.clj for other lines here I might want to include
