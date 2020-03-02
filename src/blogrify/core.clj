(ns blogrify.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as cs]
            [markdown.core :as md]
            [me.raynes.fs :as fs]
            [net.cgrand.enlive-html :as html]))


(defn tidy-whitespace
  [s]
  (cond
    (string? s) (cs/replace (cs/replace s #"  +" " ") "\n" " ")
    (seq? s) (cs/join " " (map tidy-whitespace s))
    :else (tidy-whitespace (str s))))


(defn debloggerise
  "Google's Blogger 'html' is the most appalling tag soup. This attempts to
  sort out some of the worst infelicities."
  [e]
  (-> e
      (html/transform [:h2.date-header] #(assoc % :tag :h4))
      (html/transform [:h3.entry-title] #(assoc % :tag :h1))
      (html/transform [:td] #(if
                          (and
                            (= (count (:content %)) 1)
                            (= (:tag (first (:content %))) :b))
                          (merge % {:tag :th :content (:content (first (:content %)))})
                          %))))


(defn enlive->md
  "This could be worked into something general purpose, but for the moment it
  is for Blogger tag soup only"
  ([e]
   (enlive->md e nil))
  ([e context]
   (cond
     (map? e) (let [;; all children, processed as markdown
                    c (enlive->md (:content e) (cons (:tag e) context))
                    ;; only children which are elements, processed as markdown
                    c' (enlive->md (filter map? (:content e)) (cons (:tag e) context))]
                (case (:tag e)
                  :a (when (-> e :attrs :href)
                       (str "[" (cs/trim c) "](" (-> e :attrs :href) ")"))
                  (:b :strong) (str "**" (cs/trim c) "**")
                  :br (when-not (= (first context) :td) "\n") ;; trust fucking Google to emit tag soup.
                  (:code :samp :var) (str "`" (cs/trim c) "`")
                  :div (when-not
                         (= (-> e :attrs :class) "post-footer")
                         (when-not (empty? (cs/trim c)) ;; empty divs should not introduce vertical space.
                           (str "\n\n" c)))
                  :h1 (str "\n# " (cs/trim c) "\n\n")
                  :h2 (str "\n## " (cs/trim c) "\n\n")
                  :h3 (str "\n### " (cs/trim c) "\n\n")
                  :h4 (str "\n#### " (cs/trim c) "\n\n")
                  :h5 (str "\n##### " (cs/trim c) "\n\n")
                  (:i :em) (str "*" (cs/trim c) "*")
                  :img (when
                         (-> e :attrs :src)
                         (str "!["
                              (cs/trim
                                (or
                                  (-> e :attrs :alt)
                                  (-> e :attrs :title)
                                  (-> e :attrs :src)))
                              "]("
                              (-> e :attrs :src)))
                  :li (str "\n" (if (= (first context) :ul) "*" "1.") " " c)
                  (:ol :ul) (str c "\n\n")
                  :p (str "\n\n" c)
                  :pre (str "```\n" c "\n```\n") ;; <<-- TODO this is not right
                  :span (case (-> e :attrs :class)
                          "post-icons" nil
                          c)
                  :table (str c' "\n\n")
                  (:tbody :thead) c'
                  :td (str (cs/trim c) " | ")
                  :th (str (cs/trim c) " | ")
                  :tr (str
                          "\n| "
                          (cs/trim c')
                          (let [child-tags (map :tag (filter map? (:content e)))]
                            (when (some #(= % :th) child-tags)
                              (apply
                                str
                                (cons
                                  "\n|"
                                  (map
                                    #(if (= % :th) " --- |" "     |")
                                    child-tags))))))
                  ;; else
                  c))
     (vector? e) (enlive->md (seq e) context)
     (seq? e) (apply str (map #(enlive->md % context) e))
     :else (tidy-whitespace (str e)))))


(defn mung-post
  [file-path]
  (let [enlivened (debloggerise (html/html-resource (fs/file file-path)))
        content (html/select enlivened  [:div.hentry])
        date-header (enlive->md (html/select enlivened [:h4.date-header]))
        title (cs/trim (tidy-whitespace (enlive->md (:content (first (html/select content [:h1.post-title]))))))
        directory-path (subs (str file-path) 0 (cs/last-index-of (str file-path) "/"))
        output-name (str (cs/replace title #"[^a-zA-Z0-9 ]" "") ".md")
        output-path (fs/file "content" output-name)]
    (spit output-path (str date-header (enlive->md content)))
    output-name))


(defn mung-posts
  [file-path]
  (cond
    (fs/file? file-path) (mung-post file-path)
    (fs/directory? file-path) (map mung-posts (fs/list-dir file-path))
    (string? file-path) (mung-posts (fs/file file-path))))

(cs/trim "\n  \n")

(mung-posts "resources/blog.journeyman.cc/")

(mung-post "resources/blog.journeyman.cc/2013/07/populating-game-world.html")

(def enlivened (debloggerise (html/html-resource (fs/file "resources/blog.journeyman.cc/2013/07/populating-game-world.html"))))

(def table (first (html/select enlivened [:table])))

(filter map? (:content table))

(def td (nth (html/select table [:td]) 0))

(enlive->md td)
(def tr (nth (html/select table [:tr]) 0))
(enlive->md tr)

(enlive->md table)

(md/md-to-html-string (enlive->md table))

(enlive->md table)
