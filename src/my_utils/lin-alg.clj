(ns my-utils.lin-alg
  (:use [clojure.core.matrix])
  (:require [clojure.core.matrix.operators :as M]
            [clatrix.core :as cl]))

(defn square-mat
  "computes a square matrix of size nxn with element e"
  [n e & {:keys [implementation]
          :or   {implementation :clatrix}}]
  (let [repeater #(repeat n %)]
    (matrix implementation (-> e repeater repeater))))

(defn id-mat
  "computes a identity matrix of size nxn"
  [n & {:keys [implementation]
         :or {implementation :clatrix}}]
  (let [init (square-mat n 0 :implementation implementation)
        id-fn (fn [i j n] (if (= i j) 1 n))]
    (cl/map-indexed id-fn init)))

(def A (matrix :clatrix [[0 1 2] [2 3 4] [3 4 5]]))
(def B (id-mat 3 :implementation :vectorz))
(def C (matrix :vectorz [[0 1 2] [2 4 3] [5 3 6]]))

(M/* A B C)

;;; sparse matrix
;;; https://groups.google.com/forum/#!topic/numerical-clojure/LLpq4WHx-k8
(def A (new-sparse-array :vectorz [10 10]))
;;; mutable update
(dotimes [i 1000]
  (mset! A (rand-int 10) (rand-int 10) (rand-int 5)))
