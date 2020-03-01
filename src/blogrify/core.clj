(ns blogrify.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as cs]
            [me.raynes.fs :as fs]
            [net.cgrand.enlive-html :as html]
            [watney.core :as wc]
            ))


(defn tidy-whitespace
  [s]
  (cond
    (string? s) (cs/replace (cs/replace s #"  +" " ") "\n" "")
    (seq? s) (apply str (map tidy-whitespace s))
    :else (tidy-whitespace (str s))))


(defn enlive->md
  "This could be worked into something general purpose, but for the moment it
  is for Blogger tag soup only"
  [e]
  (cond
    (map? e) (let [c (enlive->md (:content e))]
               (case (:tag e)
                 :a (when (-> e :attrs :href)
                      (str "[" (cs/trim c) "](" (-> e :attrs :href) ")"))
                 (:b :strong) (str "**" (cs/trim c) "**")
                 :br "\n" ;; trust fucking Google to emit tag soup.
                 (:code :samp :var) (str "`" (cs/trim c) "`")
                 :div (when-not (= (-> e :attrs :class) "post-footer") (str "\n\n" c))
                 :h1 (str "\n\n# " (cs/trim c) "\n\n")
                 :h2 (str "\n\n## " (cs/trim c) "\n\n")
                 :h3 (str "\n\n# " (cs/trim c) "\n\n") ;; Trust Google to mark the title of a post <h3>!
                 :h4 (str "\n\n#### " (cs/trim c) "\n\n")
                 :h5 (str "\n\n##### " (cs/trim c) "\n\n")
                 (:i :em) (str "*" (cs/trim c) "*")
                 :img (when
                        (-> e :attrs :src)
                        (str "![" (cs/trim (or (-> e :attrs :alt)(-> e :attrs :title)(-> e :attrs :src))) "](" (-> e :attrs :src)))
                 :li (str "\n* " c)
                 (:ol :ul) (str c "\n\n") ;; <<-- TODO: need to capture the difference!
                 :p (str "\n\n" c)
                 :pre (str "```\n" c "\n```\n") ;; <<-- TODO this is not right
                 :span (case (-> e :attrs :class)
                         "post-icons" nil
                         c)
                 :table (str c "\n\n") ;; <<-- TODO: nor is this
                 c))
    (seq? e) (apply str (map enlive->md e))
    :else (tidy-whitespace (str e))))

(defn mung-post
  [file-path]
  (let [enlivened (html/html-resource (fs/file file-path))
        content (html/select enlivened  [:div.hentry])
        date-header (html/select enlivened [:h2.date-header])
        posting-date (try
                       (when date-header (str "##### " (enlive->md (:content (first date-header)))))
                       (catch Exception _))
        title (cs/trim (tidy-whitespace (:content (first (html/select content [:h3.post-title])))))
        directory-path (subs (str file-path) 0 (cs/last-index-of (str file-path) "/"))
        output-name (str (cs/replace title #"[^a-zA-Z0-9 ]" "") ".md")
        output-path (fs/file "content" output-name)]
    (spit output-path (str posting-date "\n" (enlive->md content)))
    output-name))


(defn mung-posts
  [file-path]
  (cond
    (fs/file? file-path) (mung-post file-path)
    (fs/directory? file-path) (map mung-posts (fs/list-dir file-path))
    (string? file-path) (mung-posts (fs/file file-path))))

(mung-posts "resources/blog.journeyman.cc/")

(mung-post "resources/blog.journeyman.cc/2019/12/world-enough-and-time.html")

(def enlivened (html/html-resource (fs/file "resources/blog.journeyman.cc/2014/09/is-currency-union-with-fuk-worth-126.html")))

 (html/select enlivened [:h2.date-header])
(enlive->md (:content (first (html/select enlivened [:h2.date-header]))))
