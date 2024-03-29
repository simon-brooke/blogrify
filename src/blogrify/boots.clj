(ns blogrify.boots
  "Sanitise images from tables which blogger classifies as `tr-caption-container`."
  (require [blogrify.utils :refer [first-child format-image second-child tidy-whitespace]]))

(def image-table
  "This is a sample of an image-table from Blogger, to use for testing a 
   function to identify such tables."
  {:tag :table,
   :attrs
   {:cellpadding "0",
    :cellspacing "0",
    :class "tr-caption-container",
    :style "float: left; margin-right: 1em; text-align: left;"},
   :content
   [{:tag :tbody,
     :attrs nil,
     :content
     ["\n"
      {:tag :tr,
       :attrs nil,
       :content
       [{:tag :td,
         :attrs {:style "text-align: center;"},
         :content
         [{:tag :a,
           :attrs
           {:href
            "https://1.bp.blogspot.com/-3jGyg7JJtJ8/WUQPQR4JvFI/AAAAAAAAiEM/LOiAHs2QiVY1R7HgOE_jgj_h6mTDhQEYgCLcBGAs/s1600/DSC_0094.jpg",
            :imageanchor "1",
            :style
            "clear: left; margin-bottom: 1em; margin-left: auto; margin-right: auto;"},
           :content
           [{:tag :img,
             :attrs
             {:border "0",
              :data-original-height "900",
              :data-original-width "1600",
              :height "180",
              :src
              "https://1.bp.blogspot.com/-3jGyg7JJtJ8/WUQPQR4JvFI/AAAAAAAAiEM/LOiAHs2QiVY1R7HgOE_jgj_h6mTDhQEYgCLcBGAs/s320/DSC_0094.jpg",
              :width "320"},
             :content []}]}]}]}
      "\n"
      {:tag :tr,
       :attrs nil,
       :content
       [{:tag :td,
         :attrs {:class "tr-caption", :style "text-align: center;"},
         :content ["Loveson boots, bought about 1984 and still good."]}]}
      "\n"]}]})

(defn image-table?
  "Return `true` if and only if the enlive-style structure `e` appears to be
   a blogger image formatted as a table."
  [e]
  (and
   (map? e)
   (= (-> e :attrs :class) "tr-caption-container")
   (=
    :img
    (-> e
        first-child
        first-child
        first-child
        first-child
        first-child
        :tag))))

(defmacro extract-caption
  "If the enlive-style structure `e` appears to be a blogger image formatted 
   as a table, return the appropriate caption for that image."
  [e]
  `(:content (-> ~e first-child second-child first-child)))

(defn sanitise-image-table
  "If the enlive-style structure `e` appears to be a blogger image formatted 
   as a table, extract the image and caption and return it; otherwise, return
   `e`."
  [e relative-image-url-path]
  (if (image-table? e)
    (format-image (-> e
                      first-child
                      first-child
                      first-child
                      first-child
                      first-child)
                  (extract-caption e)
                  relative-image-url-path)
    e))