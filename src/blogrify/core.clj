(ns blogrify.core
  (:require [blogrify.boots :refer [sanitise-image-table]]
            [blogrify.images :refer [relative-image-url-path handle-image]]
            [blogrify.time-index :refer [format-date-index]]
            [blogrify.utils :refer [enlive->plain-text tidy-whitespace]]
            ;; [clojure.pprint :refer [pprint]]
            [clojure.string :as cs]
            ;; [markdown.core :as md]
            [me.raynes.fs :as fs]
            [net.cgrand.enlive-html :as html]))

(def url-regex
  "I tried John Gruber's Liberal Pattern for Matching Web URLs
   (https://gist.github.com/gruber/8891611), but this performed poorly and
   seemed overkill. This is much simpler and will work in most cases."
  #"^https?:/{1,3}.*")

(def ^:dynamic output-dir
  "Where we will write output to."
  "content")

(defn debloggerise
  "Extract the interesting bits out of this enlive-style structure `e `
   representing Blogger tag soup. Google's Blogger 'html' is the most 
   appalling tag soup. This attempts to sort out some of the worst 
   infelicities."
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
  "Convert the enlive-style structure `e`, representing HTML markup, into a
   markdown formatted string. This could be worked into something general 
   purpose, but for the moment it is for Blogger tag soup only"
  ([e]
   (enlive->md e nil))
  ([e context]
   (cond
     (map? e) (let [;; all children, processed as markdown
                    c (enlive->md (:content e) (cons (:tag e) context))
                    ;; only children which are elements, processed as markdown
                    c' (enlive->md (filter map? (:content e)) (cons (:tag e) context))]
                (case (:tag e)
                  ;; this isn't working: images in anchors (which is all of them)
                  ;; now do not come through at all
                  :a (when (-> e :attrs :href)
                       (cond (-> e :attrs :imageanchor) (cs/trim c)
                             (= (-> e :attrs :rel) "tag") (let [c' (cs/trim c)]
                                                            (str "[" c' "](category?cat=" c' ")"))
                             :else (str "[" (cs/trim c) "](" (-> e :attrs :href) ")")))
                  (:b :strong) (str "**" (cs/trim c) "**")
                  :br (when-not (= (first context) :td) "\n") ;; trust fucking Google to emit tag soup.
                  (:code :samp :var) (str "`" (cs/trim c) "`")
                  :div (case (-> e :attrs :class)
                         "post-footer" nil
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
                         (let [alt (enlive->plain-text
                                    (or
                                     (-> e :attrs :alt)
                                     (-> e :attrs :title)
                                     (-> e :attrs :src)))
                               alt' (if (re-matches url-regex alt)
                                      "(Image)"
                                      alt)]
                           (str "![" alt' "]("
                                relative-image-url-path
                                (handle-image (-> e :attrs :src))
                                ")")))
                  :li (str "\n" (if (= (first context) :ul) "*" "1.") " " c)
                  (:ol :ul) (str c "\n\n")
                  :p (str "\n\n" c)
                  :pre (str "```\n" c "\n```\n") ;; <<-- TODO this is not right
                  :span (case (-> e :attrs :class)
                          "post-icons" nil
                          "post-labels" (str "\n\n#### " (cs/trim c) "\n\n")
                          c)
                  :table (str (sanitise-image-table e relative-image-url-path) "\n\n")
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
  "Mung an individual blog post at `file-path`."
  [file-path]
  (when-not
   (= (fs/base-name file-path) "index.html")
  ;; something in the way I'm walking the directory structure may call
  ;; this function several times on the same file. This is a hack!
    (let [enlivened (debloggerise (html/html-resource (fs/file file-path)))
          content (html/select enlivened [:div.hentry])
          date-header (enlive->md (html/select enlivened [:h4.date-header]))
          labels (enlive->md (html/select enlivened [:span.post-labels]))
          title (cs/trim (tidy-whitespace (enlive->md (:content (first (html/select content [:h1.post-title]))))))
          output-name (str (cs/replace title #"[^a-zA-Z0-9 ]" "") ".md")
          output-path (fs/file output-dir output-name)]
      (spit output-path
            (str date-header (enlive->md content) labels))
      output-name)))

(defn mung-posts
  "Process all files below this `file-path`. If `initialise?` is `true`, 
   smash and recreate the `wget-requests.sh` file before starting."
  ;; ([file-path initialise?]
  ;;  (when (true? initialise?)
  ;;    (doall
  ;;     (spit "wget-requests.sh" "")
  ;;     (mung-posts file-path))))
  ([file-path]
   (cond
     (fs/directory? file-path) (map mung-posts (fs/list-dir file-path))
     (fs/file? file-path) (mung-post file-path)
     (string? file-path) (doall (mung-posts (fs/file file-path))
                                (format-date-index output-dir)))))

;; (cs/trim "\n  \n")

;; (mung-posts "resources/blog.journeyman.cc/")

;; (mung-post "resources/blog.journeyman.cc/2013/07/populating-game-world.html")

;; (def enlivened (debloggerise (html/html-resource (fs/file "resources/blog.journeyman.cc/2013/07/populating-game-world.html"))))

;; (def table (first (html/select enlivened [:table])))

;; (filter map? (:content table))

;; (def td (nth (html/select table [:td]) 0))

;; (enlive->md td)
;; (def tr (nth (html/select table [:tr]) 0))
;; (enlive->md tr)

;; (enlive->md table)

;; (md/md-to-html-string (enlive->md table))

;; (enlive->md table)
