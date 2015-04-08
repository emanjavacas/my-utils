(ns my-utils.io
  (:require [clojure.java.io :as io]))

;;; https://groups.google.com/forum/#!topic/clojure/y1JG0HFCo9w
(defn frm-save
 "Save a clojure form to file."
  [file form]
  (with-open [w (java.io.FileWriter. (io/file file))]
    (binding [*out* w *print-dup* true] (prn form))))

(defn frm-load
  "Load a clojure form from file."
  [file]
  (with-open [r (java.io.PushbackReader.
                 (java.io.FileReader. (io/file file)))] 
     (read r)))

(defn lazy-lines [in-fn & {:keys [input] :or {input :file}}]
  (letfn [(helper [rdr]
            (lazy-seq (if-let [line (.readLine rdr)]
                        (cons line (helper rdr))
                        (do (.close rdr) nil))))]
    (case input
      :file (helper (io/reader in-fn))
      :dir (let [fs (.listFiles (io/file in-fn))]
             (flatten (map #(helper (io/reader %)) fs))))))

(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^-?\d+\.?\d*([Ee]\+\d+|[Ee]-\d+|[Ee]\d+)?$" (.trim s))
    (read-string s)))

(defn file-names 
  "return filenames from a dir, optionally filter extensions"
  ([dir] (map #(str dir (.getName %)) (file-seq (io/file dir))))
  ([dir ext] (filter #(.endsWith % ext) (file-names dir))))
