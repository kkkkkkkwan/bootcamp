(ns aoc2018-7
  (:require [clojure.string :as str]
            [clojure.set :refer [difference union]]))

(defn parse-input [lines]
  (map (fn [line] (re-seq #"\b[A-Z]+\b" line)) lines))

(def input (-> "resources/7.txt"
               slurp
               str/split-lines
               parse-input))
(def all-nodes
  (-> input
      flatten
      set
      )
  )
(comment
  (prn all-nodes)
  )

(defn conj-in-sorted [m k v]
  (if-let [nodes (get m k)]
    (assoc m k (conj nodes v))
    (assoc m k (sorted-set v))))

(def graph (reduce
             (fn [coll pair]
               (apply conj-in-sorted coll pair))
             {}
             (map reverse input)
             )
  )

(def can-remove-nodes
  (->> input
       (map second)
       (set)
       (difference all-nodes)
       (apply sorted-set)
       )
  )

(defn remove-node-from-graph [graph remove-target-node]
  (reduce-kv (fn [m node node-needs]
               (let [visited (disj node-needs remove-target-node)]
                 (if (seq visited) (assoc m node visited) m))
               )
             {}
             graph))
(comment
  (prn graph)
  (remove-node-from-graph graph "C")
  )

(defn remove-all [graph]
  (loop [base-graph graph
         available-nodes can-remove-nodes
         result []]
    (if (seq available-nodes)
      (let [next-step (first available-nodes)
            new-graph (remove-node-from-graph base-graph next-step)
            new-available-nodes (apply sorted-set
                                 (difference (set (keys base-graph))
                                             (set (keys new-graph))))]
        (recur new-graph
               (union new-available-nodes (disj available-nodes next-step))
               (conj result next-step)))
      result)))

(defn solve1 [input]
  (->> input
       remove-all
       (apply str))
  )

(comment
  (solve1 graph)
  )
