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
(def initial-state {:acc 0 :current-line 0 :visited #{}})
(def commands (->> input
                   (mapv parse-command-from-line)))

(defn jmp-or-nop? [line]
  "해당 커맨드라인이 jmp 혹은 nop 커맨드이면 true 반환"
  (not= (first (nth commands line)) :acc)
  )

(defn is-visited? [{:keys [current-line acc visited]}]
  "visited에 current-line이 존재하면 true"
  (some #(= current-line %) visited)
  )

(defn has-end? [state]
  "상태에 :end가 존재하면 true"
  (contains? state :end)
  )

(defn process-command [{:keys [current-line acc visited]}]
  "문제 1의 상태 프로세싱 메소드."
  (let [[command value] (nth commands current-line)]
    (case command
      :acc {:current-line (inc current-line)
            :acc (+ value acc)
            :visited (conj visited current-line)}
      :jmp {:current-line (+ current-line value)
            :acc acc
            :visited (conj visited current-line)}
      :nop {:current-line (inc current-line)
            :acc acc
            :visited (conj visited current-line)}
      )
    )
  )

(def fix-candidates
  "한번이라도 실행된 적 있는 라인 중 교체 대상인 jmp와 nop커맨드가 포함된 라인들 리스트"
  (->> initial-state
       (iterate process-command)
       (filter is-visited?)
       (first)
       (:visited)
       (filter jmp-or-nop?))
  )
(comment
  (prn fix-candidates)
  )

(defn fix-command [commands]
  "커맨드의 특정 라인이 candidates에 포함되어있으면 오퍼레이터를 교체해서 리스트로 내려줌"
  (fn [i _]
    (when (some #(= i %) fix-candidates)
      (update-in commands [i 0] {:jmp :nop, :nop :jmp}))
    )
  )

(defn generate-fixed-commands [commands]
  (keep-indexed (fix-command commands) commands)
  )

(defn change-state-with-fixed-commands [fixed-commands {:keys [current-line acc visited]}]
  "수정된 command 를 가지고 실행해야하기때문에 fixed-commands를 인자로 받은 상태변환메소드"
  (let [[command value] (nth fixed-commands current-line [:end 0])]
    (case command
      :acc {:current-line (inc current-line)
            :acc (+ value acc)
            :visited (conj visited current-line)}
      :jmp {:current-line (+ current-line value)
            :acc acc
            :visited (conj visited current-line)}
      :nop {:current-line (inc current-line)
            :acc acc
            :visited (conj visited current-line)}
      :end {:end acc}
      )
    )
  )

(defn run-commands [commands]
  (->> initial-state
       (iterate #(change-state-with-fixed-commands commands %))
       (filter #(or (is-visited? %) (has-end? %)))
       (first)
       )
  )

(defn solve2 [state commands]
  (->> (generate-fixed-commands commands)
       (map run-commands)
       (filter has-end?)
       (first)
       (:end)
  ))

(comment
  (solve2 initial-state commands)
  )
