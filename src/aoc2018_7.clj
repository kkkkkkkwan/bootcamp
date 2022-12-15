(ns aoc2018-7
  (:require [clojure.string :as str]
            [clojure.set :refer [difference union]]))

(defn parse-input [lines]
  (map (fn [line] (re-seq #"\b[A-Z]+\b" line)) lines))

(def input (-> "resources/7.txt"
               slurp
               str/split-lines
               parse-input))
(comment
  (prn input)
  )

(def all-nodes
  "존재하는 모든 node들을 나열"
  (-> input
      flatten
      set
      )
  )
(comment
  (prn all-nodes)
  )

(defn insert-relationship-to-graph [m k v]
  "그래프에 노드간의 관계를 추가."
  (assoc
    m
    k
    (conj (get
            m
            k
            (sorted-set v))
          v
          )
    )
  )

(def graph (reduce
             (fn [coll pair]
               (apply insert-relationship-to-graph coll pair))
             {}
             (map reverse input)
             )
  )
(comment
  (prn graph)
  )

(def can-remove-nodes
  (->> input
       (map second)
       (set)
       (difference all-nodes)
       (apply sorted-set)
       )
  )
(comment
  (prn can-remove-nodes)
  )

(defn remove-node-from-graph [graph remove-target-node]
  "그래프에서 특정 노드를 모두 제거한다. 제거후 특정 노드에대한 dependency가 모두 없어지면 그래프에서 해당 노드를 키로 하는 데이터가 없어진다."
  (reduce-kv (fn [m node node-needs]
               (let [visited (disj node-needs remove-target-node)]
                 (if (seq visited) (assoc m node visited) m))
               )
             {}
             graph))
(comment
  (prn graph)
  (remove-node-from-graph graph "C")
  (remove-node-from-graph graph "A")
  )

(defn remove-all-and-get-sequence [graph]
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
       remove-all-and-get-sequence
       (apply str))
  )

(comment
  (solve1 graph)
  )

(defn char-to-time [base char]
  (->> char
       (map int)
       (apply +)
       (- (int \A))
       Math/abs
       (+ 1 base)
       )
  )

(comment
  (int "A")
  (map int "A")
  (char-to-time 60 "A")
  )

(defn process-parallel [graph workers]

  )

; 참고 -> 2020 day 8
;{
; :graph {
;         "T" #{"C" "F" "H" "J" "L" "R" "W"},
;         "K" #{"I" "N" "R"},
;         "Q" #{"C" "F" "N" "V"},
;         "G" #{"E" "H" "I" "K" "P" "Q" "S" "U" "Y"},
;         "J" #{"D"},
;         "M" #{"L"},
;         "S" #{"E" "O" "U" "W" "Z"},
;         "Y" #{"E" "H" "K" "O" "Q" "S" "T" "X"},
;         "Z" #{"B" "C" "E" "H" "I" "R" "X"},
;         "H" #{"N" "V" "W"},
;         "E" #{"I" "T"},
;         "R" #{"B" "N" "W"},
;         "C" #{"D"},
;         "F" #{"L"},
;         "P" #{"B" "C" "J" "K" "M" "N" "O" "Q" "T" "W" "Y" "Z"},
;         "V" #{"D" "F"},
;         "U" #{"E" "J" "K" "N" "Q" "T" "W"},
;         "O" #{"F" "K" "R" "T" "U" "X"},
;         "X" #{"C" "H" "J" "M" "Q"},
;         "N" #{"B"},
;         "I" #{"B" "D" "F" "H" "J" "W"}
;         }
; :after-graph {}
; :pool [[] [] [] [] []]
; :pool-size 5
; :next  ("A" "B" "C")
; }
