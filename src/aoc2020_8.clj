(ns aoc2020-8
  (:require [clojure.string :as str])
  )

(def input (-> "resources/2020_8.in"
               slurp
               str/split-lines
               )
  )

(def example-input (-> "resources/2020_8.example.in"
               slurp
               )
  )

(defn increase-current-line-in-state [state-machine number]
  (assoc state-machine :current-line (+ number (:current-line state-machine)))
  )

(defn apply-command-to-state-machine [state-machine [command number-str]]
  (let [number (Integer/parseInt number-str)]
    (cond
      (= command "jmp") (increase-current-line-in-state state-machine (- number 1))
      (= command "acc") (assoc state-machine :acc (+ number (:acc state-machine)))
      (= command "nop") state-machine
      :else (assoc state-machine :stop (:acc state-machine))
      )
    )
  )

(defn increase-and-check-visited-current-line-in-state-machine [line state-machine]
  (-> state-machine
      (update :visited conj (:current-line state-machine))
      (increase-current-line-in-state 1)
      (apply-command-to-state-machine line)
      )
  )

(defn is-visited? [state]
  (contains? (:visited state) (:current-line state))
  )

(defn solve1 [input]
  (let [state {:acc 0 :current-line 0 :visited #{}}
        lines input
        ]
    (->> state
         (iterate #(increase-and-check-visited-current-line-in-state-machine (nth lines (:current-line %)) %))
         (filter is-visited?)
         (first)
         )
    )
  )

(comment
  (->> input
       (map #(str/split % #" "))
       (vec)
       solve1
       )

  (let [state {:acc 0 :current-line 0 :visited #{}}
        line ["acc" "49"]
        ]
    (apply-to-state-machine [line] state)
    )

  (keep-indexed inc input)
  )

(defn parse-command-from-line [line]
  (let [[command acc] (str/split line #" ")]
    [(keyword command) (Integer/parseInt acc)]
    )
  )

;// part 2

(defn apply-command-to-state-machine-part2 [state-machine [command number-str]]
  (let [number (Integer/parseInt number-str)]
    (case command
      :jmp (increase-current-line-in-state state-machine (- number 1))
      :acc (assoc state-machine :acc (+ number (:acc state-machine)))
      :nop state-machine
      :end (assoc state-machine :stop (:acc state-machine))
      )
    )
  )

(defn increase-and-check-visited-current-line-in-state-machine-part2 [line state-machine]
  (-> state-machine
      (update :visited conj (:current-line state-machine))
      (increase-current-line-in-state 1)
      (apply-command-to-state-machine-part2 line)
      )
  )

(defn swap-non-acc-commands [input]
  (fn [i [op _]]
    (when (#{:nop :jmp} op)
      (update-in input [i 0] {:jmp :nop, :nop :jmp}))))

(defn solve2 [input]
  (let [state {:acc 0 :current-line 0 :visited #{}}
        lines input
        ]
    (->> state
         (iterate #(increase-and-check-visited-current-line-in-state-machine (nth lines (:current-line %)) %))
         (filter )
         (first)
         )
    )
  )

(comment
  (->> input
       (mapv parse-command-from-line))
  )
