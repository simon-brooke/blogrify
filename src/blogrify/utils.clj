(ns blogrify.utils
  "Utility functions used in more than one other namespace."
  (require [blogrify.images :refer [handle-image]]
           [clojure.string :as cs]))

(defmacro children
  [e]
  `(filter map? (:content ~e)))

(defmacro first-child
  "Return the first child element of the enlive-style structure `e`, if `e`
   represents an XML element, else nil."
  [e]
  `(when (map? ~e)
     (first (children ~e))))

(defmacro nth-child
  "Return the `n`th child element of the enlive-style structure `e`, if `e`
   represents an XML element, else nil."
  [e n]
  `(when (map? ~e)
     (nth (children ~e) ~n)))

(defmacro second-child
  "Return the second child element of the enlive-style structure `e`, if `e`
   represents an XML element, else nil."
  [e]
  `(nth-child ~e 1))

(defn tidy-whitespace
  "Remove, from `s`, expected to be a string or something else representing
   unmarked text, anything which could be interpreted by Markdown as 
   formatting."
  [s]
  (cond
    (string? s) (cs/replace (cs/replace s #"[ \*\_\#]+" " ") "\n" " ")
    (coll? s) (cs/join " " (map tidy-whitespace s))
    :else (tidy-whitespace (str s))))

(defn enlive->plain-text
  "Convert the enlive-style structure `e `representing HTML markup, into
   a plain text string."
  ([e] (enlive->plain-text e nil))
  ([e context]
   (tidy-whitespace
    (cond (map? e) (let [c (enlive->plain-text (:content e) (cons (:tag e) context))]
                     (cs/trim c))
          (coll? e) (cs/join " " (map #(enlive->plain-text % context) e))
          :else e))))

(defn format-image
  [e alt relative-image-url-path]
  (str "![" (enlive->plain-text alt) "]("
       relative-image-url-path
       (handle-image (-> e :attrs :src))
       ")"))