(ns my-utils.syntax)

(defn- enumerate [seq]
  (map-indexed vector seq))

(defn positions [pred coll]
  (keep-indexed
   (fn [idx x]
     (when (pred x) idx))
   coll))
