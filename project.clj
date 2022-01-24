(defproject forage "0.1.0-SNAPSHOT"
  :description "Random foraging models in Clojure with MASON."
  :url "https://github.com/mars0i/forage"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [mars0i/masonclj "0.2.0"]
                 ;[criterium "0.4.6"]
                 [mason "20"] ; (slightly hacked version) can be installed in local maven repo: uncomment lein-localrepo below, run 'lein localrepo install lib/mason.19.noAddlApps.jar mason 19'
                 ;; Libs that MASON wants and can be gotten from maven.org, so they don't need to be in my lib dir:
                 [org.beanshell/bsh "2.0b4"]
                 [com.lowagie/itext "1.2.3"] ; version that comes with MASON. Not in maven.org: [com.lowagie/itext "1.2"] 
                 [org.jfree/jcommon "1.0.21"]
                 [org.jfree/jfreechart "1.0.17"]
                 [javax.media/jmf "2.1.1e"]]

  :repl-options {:init-ns forage.core}
  
  ;:jvm-opts ["-Xms2g"]
  :source-paths ["src/clj"]

  ;:aot [forage.Sim forage.GUI]
  
  ; see pasta/project.clj for other lines here I might want to include
)
