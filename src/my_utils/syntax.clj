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

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn isin? [coll n]
  (loop [acc []
         s coll]
    (let [item (first s)]
      (cond (= item n) (recur (cons item acc) (rest s))
            (empty? s) acc
            :else (recur acc (rest s))))))

(defn arange
  "returns a lazy-seq of x (= num-steps) equally distant nums"
  [start stop num-steps]
  (let [step (/ (- stop start) num-steps)]
    (range start stop step)))

(defn find-idx
  "find the indices where pred returns true"
  [pred coll]
  (map first
       (filter #(pred (second %))
               (map-indexed vector coll))))

(defn coll-depth
  "returns the depth of a nested collection"
  [coll]
  (loop [depth 0
         coll  coll]
    (if-let [newcoll (some #(when (coll? %) %) coll)]
      (recur (inc depth) newcoll)
      depth)))
