(defproject blogrify "0.1.0-SNAPSHOT"
  :description "Move a blog from blogger to Smeagol (or, actually, any other markdown based content engine)."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[clojure.java-time "0.3.3"]
                 [com.cemerick/url "0.1.1"]
                 [com.taoensso/timbre "4.10.0"]
                 [enlive "1.1.6"]
                 [image-resizer "0.1.10"]
                 [org.clojure/clojure "1.8.0"]
                 [markdown-clj "1.10.2"]
                 [me.raynes/fs "1.4.6"]
                 [nio "1.0.3"]
                 [watney "0.1.0-SNAPSHOT"]]
  :repl-options {:init-ns blogrify.core})
