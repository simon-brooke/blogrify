(defproject blogrify "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [enlive "1.1.6"]
                 [markdown-clj "1.10.2"]
                 [me.raynes/fs "1.4.6"]
                 [watney "0.1.0-SNAPSHOT"]]
  :repl-options {:init-ns blogrify.core})
