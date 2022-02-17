(ns blogrify.images
  (:require [cemerick.url :refer [url]]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as cs]
            [image-resizer.core :refer [resize]]
            [image-resizer.scale-methods :as sm]
            [image-resizer.util :refer :all]
            [me.raynes.fs :as fs]
            [net.cgrand.enlive-html :as html]
            [taoensso.timbre :as log])
  (:import java.security.MessageDigest
           java.math.BigInteger
           [java.io File]
           [java.awt Image]
           [java.awt.image RenderedImage BufferedImageOp]
           [javax.imageio ImageIO ImageWriter ImageWriteParam IIOImage]
           [javax.imageio.stream FileImageOutputStream]))

;; Serving images from blogger's infrastructure makes no sense; ultimately
;; they will delete or at least archive them. We need to download those image
;; locally. At the same time we need to get rid of the crackers Blogger image
;; names.
;;
;; It would be nice if we could guarantee that the names generated would be
;; the same on every run; an MD5 sum of the blogger name might be good enough.

(def image-file-extns
  "Extensions of file types we will attempt to thumbnail. GIF is excluded
  because by default the javax.imageio package can read GIF, PNG, and JPEG
  images but can only write PNG and JPEG images."
  #{".jpg" ".jpeg" ".png"})

(defn read-image
  "Reads a BufferedImage from source, something that can be turned into
  a file with clojure.java.io/file, or else a URL"
  [source]
  (ImageIO/read
    (if (fs/exists? (fs/file source))
      (fs/file source)
      (url source))))

(defn write-image
  "Writes img, a RenderedImage, to dest, something that can be turned into
  a file with clojure.java.io/file.
  Takes the following keys as options:
  :format  - :gif, :jpg, :png or anything supported by ImageIO
  :quality - for JPEG images, a number between 0 and 100"
  [^RenderedImage img dest & {:keys [format quality] :or {format :jpg}}]
  (log/info "Writing as" format "to"  dest)
  (let [fmt (subs (fs/extension (cs/lower-case dest)) 1)
        iw (doto ^ImageWriter (first
                                (iterator-seq
                                  (ImageIO/getImageWritersByFormatName
                                    fmt)))
             (.setOutput (FileImageOutputStream. (io/file dest))))
        iw-param (case format
                   :jpg (doto ^ImageWriteParam (.getDefaultWriteParam iw)
                          (.setCompressionMode ImageWriteParam/MODE_EXPLICIT)
                          (.setCompressionQuality (float (/ (or quality 75) 100))))
                   (:png :gif)  nil)
        iio-img (IIOImage. img nil nil)]
    (log/info "smeagol.uploads/write-image: fmt=" fmt "format=" format)
    (if iw
      (.write iw nil iio-img iw-param)
      (log/error "smeagol.uploads/write-image: no suitable writer found"))))


(def relative-image-directory
  "Relative path from the working directory to the directory into which
  images will be saved."
  "content/img/blogrify")

(def relative-image-url-path
  "Assumes documents are stored in the content directory."
  "img/blogrify/")

(def images-map
  "Map blogger-name -> our-name for images"
  (atom {}))

(defn md5 [^String s]
  "Stolen from https://gist.github.com/jizhang/4325757"
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn our-name-for
  ;; Return our local name for the specified
  [url]
  (let [extn (subs url (cs/last-index-of url "."))]
    (str (gensym "img") extn)))

;; (our-name-for "froboz.png")

(defn ensure-image-directory
  []
  (when-not
    (fs/exists? relative-image-directory)
    (fs/mkdirs relative-image-directory)))


(defn handle-image
  "If we've already handled an image with this `blogger-name`, return the name
  we stored it under. Otherwise, retrieve it, store it under a generated name,
  and return that name."
  [blogger-url]
  (let [n (@images-map blogger-url)]
    (if
      n
      n
      (let [n' (our-name-for blogger-url)]
        (ensure-image-directory)
        (spit
          "wget-requests.sh"
          (str "wget -O "
               (fs/absolute
                 (fs/file relative-image-directory n'))
               " \""
               blogger-url "\"\n")
          :append true)
        (swap! images-map assoc blogger-url n')
        n'))))

