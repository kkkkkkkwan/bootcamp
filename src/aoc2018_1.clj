(ns aoc2018-1
  (:require [clojure.java.io :as io]))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(defn sum [args] (reduce + args))

(def input (-> "aoc2018_1_1.sample.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))
(comment
  (sum input)
  (apply + [1 2 3])
  )

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(defn find-duplicate-field-in-list [list]
  (reduce (fn [set arg]
            (if (set arg)
             (reduced arg)
             (conj set arg))) #{} list))

(defn solve [args]
      (->> (map parse-long args)
          cycle
          (reductions +)
          find-duplicate-field-in-list))

(def input (-> "aoc2018_1_2.sample.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

#_(solve [3, 3, 4, -2, -4])
(solve input)
