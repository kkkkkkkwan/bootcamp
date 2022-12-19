(ns aoc2020-1
  (:require [clojure.spec.alpha :as spec])
  (:require [clojure.java.io :as io]))

(spec/def :num/is-2020? #(= 2020 %))

(spec/valid? :num/is-2020? 2020)
(spec/valid? :num/is-2020? 2021)

(def input (-> "2020_1.in"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

(prn input)

(defn solve1 [input]
  (->> (for [num1 input num2 input]
         (when (spec/valid? :num/is-2020? (+ num1 num2))
           (* num1 num2)
           )
         )
       (filter #(not (nil? %)))
       (first)
       )
  )

(defn solve2 [input]
  (->> (for [num1 input num2 input num3 input]
         (when (spec/valid? :num/is-2020? (+ num1 num2 num3))
           (* num1 num2 num3)
           )
         )
       (filter #(not (nil? %)))
       (first)
       )
  )

(comment
  (->> input
       (map #(Integer/parseInt %))
       (solve1)
       )

  (->> input
       (map #(Integer/parseInt %))
       (solve2)
       )
  )
