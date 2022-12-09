(ns aoc2018_3
  (:require [clojure.string :as str]))


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)
(def input (slurp "resources/3_1.txt"))

(defn process-input-to-records
  "input 을 [{:line :x :y :width :height} ...] 형태로 변환"
  [lines]
  (for [line (str/split-lines lines)]
    (let [matcher (re-matcher #"\d+" line)]
      (->> (repeatedly 5 #(re-find matcher))
           (map #(Integer/parseInt %))
           (zipmap [:line :x :y :width :height])))))

(defn record-to-coordinate-list [{:keys [x y width height]}]
  "{:line :x :y :width :height} 를 ['x:y', 'x:y+1' ... 'x+width:y+height'] 형태로 변환 "
  (let [base-x x
        base-y y]
    (for [x (range base-x (+ base-x width))
          y (range base-y (+ base-y height))]
      (apply str [x ":" y])
      )))

(defn apply-coordinates-to-matrix [matrix coordinates]
  "행렬에서 좌표위치들의 count를 +1"
  (loop [coords coordinates
         mat matrix]
    (if (empty? coords)
      mat
      (recur (rest coords)
             (assoc mat
               (first coords)
               (inc (mat (first coords) 0))
               )
             )
      )
    )
  )

(defn count-more-than-once-checked [matrix]
  (->> (vals matrix)
       (filter #(< 1 %))
       (count)))

(defn generate-matrix [records]
  (reduce #((->> (record-to-coordinate-list %2)
                 (apply-coordinates-to-matrix %1)) {} records))
  #_(loop [recs records
         mat {}]
    (if (empty? recs)
      mat
      (recur (rest recs)
             (->> (record-to-coordinate-list (first recs))
                  (apply-coordinates-to-matrix mat)
                  )
             )
      )
    ))

(defn solve1 []
  (->> (process-input-to-records input)
       (generate-matrix)
       (count-more-than-once-checked)))

(defn solve11 []
  (->> (-> (process-input-to-records input))
       (generate-matrix)
       (count-more-than-once-checked)))

(solve1)

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
(defn unique-coords? [refined-matrix coordinates]
  (->> (clojure.set/intersection coordinates refined-matrix)
       (count)
       (= (count coordinates))
       )
  )

(defn find-line-which-records-has-unique-area-in-refined-matrix [records refined-matrix]
  "matrix는 1번만 체크된 애들만 정제하여 들고 있고, 레코드의 모든 좌표가 해당 matrix에 존재하면 유일하게 겹치지 않는 영역이다"
  #_(reduce (fn [mat coords] (let []) (if () (reduced (record :line)))))
  (loop [recs records]
    (if (empty? recs)
      nil
      (let [record (first recs)
            coords (into #{} (record-to-coordinate-list record))
            is-unique (unique-coords? refined-matrix coords)]
        (if (true? is-unique)
          (record :line)
          (recur (rest recs))
          )
        )
      )
    )
  )

(defn find-answer-line [records matrix]
  (->> (filter #(< 1 (val %)) matrix)
       (map #(first %))
       (into #{})
       (find-line-which-records-has-unique-area-in-refined-matrix records))
  )

(defn solve2 []
  (let [records (process-input-to-records input)]
    (->> (generate-matrix records)
         (find-answer-line records)
         )))

(solve2)