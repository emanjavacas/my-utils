(ns my-utils.syntax)

(defn- enumerate [seq]
  (map-indexed vector seq))

(defn positions [pred coll]
  (keep-indexed
   (fn [idx x]
     (when (pred x) idx))
   coll))

(defn invert-map [m]
  (zipmap (vals m) (keys m)))

(defn map-kv
  "apply function f over the vals of m.
  returns a map with the new vals"
  [m f]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(defn map-vk
  "same as map-kv but it updates keys"
  [m f]
  (reduce-kv #(assoc %1 (f %2) %3) {} m))
