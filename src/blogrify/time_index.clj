(ns blogrify.time-index
  "Index blog entries temporally"
  (:require [clojure.java.io :refer [as-file reader]]
            [clojure.string :as s :refer [ends-with? replace split]]
            [java-time :refer [format local-date]]
            [me.raynes.fs :as fs])
  (:import [java.nio.file Files FileSystems LinkOption]
           [java.time ZoneId]))

;; see this gist: 

(defn creation-time
  "Adapted shamelessly from this gist:
   https://gist.github.com/saidone75/0844f40d5f2d8b129cb7302b7cf40541"
  [file]
  (.creationTime
   (java.nio.file.Files/readAttributes
    (.toPath (as-file file))
    java.nio.file.attribute.BasicFileAttributes
    (into-array java.nio.file.LinkOption []))))

(defn file-attribute
  "Return the value of the specified `attribute` of the file at `file-path`
   in the current default file system. The argument `attribute` may be passed
   as a keyword or a string, but must be an attribute name understood be 
   `java.nio.file.Files`."
  [file-path attribute]
  (Files/getAttribute
   (.getPath
    (FileSystems/getDefault)
    (str file-path)
    (into-array java.lang.String []))
   (name attribute)
   (into-array LinkOption [])))

(defn dateline
  "Attempt to extract the dateline from a blog post at this `file-path`,
   and return it as a `java.time.LocalDate` object."
  [file-path]
  (let [dl (nth (s/split (with-open [eddie (reader file-path)]
                           (first (remove empty?(line-seq eddie))))
                         #" " 2) 1)]
    (try (local-date "EEEE, dd MMMM yyyy" dl)
         (catch Exception
                any
                ;;; if we get an exception, we'll go for the file creation date,
                ;;; or, failing that, the last modified date.
           (local-date
            (.toInstant
             (first
              (remove nil?
                      (map
                       #(file-attribute file-path %)
                       ["creationTime" "lastModifiedTime"]))))
            (ZoneId/systemDefault))))))

(defn date-index
  "Return a map LocalDate->File of all the files in the directory indicated
   by `dir-path`. Note that this implementation does assume there will never
   be more than one blog post on a given day, which, given that it's my blog,
   is true."
  [dir-path]
  (let [d (as-file dir-path)]
    (when (fs/directory? d)
      (into (sorted-map)
            (remove
             nil?
             (map
              #(try (when (ends-with? (str %) ".md")(vector (dateline (str %)) %))
                    (catch Exception any
                      ;; if we can't get a valid dateline, don't index it
                      ))
              (remove
               fs/directory?
               (file-seq d))))))))

(def ^:dynamic
  date-format
  "EEEE, dd MMMM yyyy")

(defn format-date-index
  "Format, as a Markdown document, an index of the directory indicated by `dir-path`"
  [dir-path]
  (let [index (date-index dir-path)
        f (reduce
           #(str %1 "\n* " (format date-format %2) ": [[" 
                 (s/replace (.getName (index %2)) #"\.md$" "")
                 "]]")
           "# Index by date\n"
           (reverse (keys index)))]
    (spit (fs/file dir-path "Index by date.md") f)
    f))