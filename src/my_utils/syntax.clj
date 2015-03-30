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

(defn until
  "subseq of n first elements before
  pred returns true"
  [pred coll]
  (lazy-seq
   (when (and (seq coll) (not (pred (first coll))))
     (cons (first coll)
           (until pred (rest coll))))))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level."
  {:added "1.7"}
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn deep-merge
  "Recursively merges maps. If keys are not maps, the last value wins."
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))
