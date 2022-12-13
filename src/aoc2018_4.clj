(ns aoc2018_4
  (:require [clojure.string :as str]))

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-05 00:55] wakes up
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.√
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.
(def input (slurp "resources/4.txt"))

;; {:guard 1 {[0 20 1 1 2 12]}}

(defn line-to-number-list [line repeat-count]
  (let [matcher (re-matcher #"\d+" line)]
    (->> (repeatedly repeat-count #(re-find matcher))
         (map #(Integer/parseInt %))
         (zipmap [:year :month :day :hour :minute :guard]))))

(defn parse-input [input]
  (for [line (str/split-lines input)]
    (cond (str/includes? line "shift") (line-to-number-list line 6) 
          :else (line-to-number-list line 5))
    ))

(defn sort-schedules [schedules]
  (->> (sort-by (juxt :month :day :hour :minute) schedules)))

(defn generate-guard-id-work-minutes-map [schedule-groups]
  (reduce (fn [m group]
            (let [guard-id (get (last (first group)) :guard)
                  sleep-awake-groups (partition 2 (second group))]
              (->> (get m guard-id ())
                   (merge (for [[start end] sleep-awake-groups]
                                        (range
                                          (get start :minute)
                                          (get end :minute)
                                          )
                                        )
                                      )
                   (flatten)
                   (assoc m guard-id))
              )
            ) {} schedule-groups))

(defn get-most-sleepy-guard-id [guard-id-work-minutes-map]
  (let [guard-ids (keys guard-id-work-minutes-map)]
    (->> (for [guard-id guard-ids]
           [guard-id (count (get guard-id-work-minutes-map guard-id))])
         (sort-by second >)
         ffirst
         )
    ))

(defn get-result [guard-id-work-minutes-map]
  (let [most-sleepy-guard-id (get-most-sleepy-guard-id guard-id-work-minutes-map)
        most-sleepy-guard-sleep-freq (frequencies (get guard-id-work-minutes-map most-sleepy-guard-id))]
    (->> most-sleepy-guard-sleep-freq
         (sort-by second >)
         ffirst
         (* most-sleepy-guard-id)
         )
    )
  )

(defn make-guard-schedule-group [schedules]
  (->> schedules
       (partition-by #(contains? % :guard))
       (partition 2)
       generate-guard-id-work-minutes-map
       )
  )

(defn solve1 [input]
  (->> (parse-input input)
       (sort-schedules)
       (make-guard-schedule-group)
       get-result
       )
  )

(comment
  (solve1 input)
  )



;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
