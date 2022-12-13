(ns aoc2018_6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(def input
  (->> (for [line (str/split-lines (slurp "resources/6.txt"))]
         (str/split line #", "))
       (map (fn [[x y]]
              (vector (Integer/parseInt x) (Integer/parseInt y))
              )
            )
       )
  )

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn get-matrix-area [coords]
  "유효한 행렬 범위를 구함. [min-x max-x min-y max-y] 예제에서는 [1 8 1 9]"
  (vector (apply min (map first coords))
          (apply max (map first coords))
          (apply min (map second coords))
          (apply max (map second coords))
          )
  )

(defn calculate-manhattan-distance-for-each-coord [coords x y]
  "주어진 좌표에 대해 기준점에 대한 맨하탄 거리를 맨하탄 거리 기준으로 정렬하여 반환"
  "([0 [1 1]] [5 [1 6]] [5 [3 4]] [8 [5 5]] [9 [8 3]] [15 [8 9]])"
  (->> (map (fn [coord] [(manhattan-distance [x y] coord) coord]) coords)
       (sort-by first <))
  )

(defn closest-coord [coords x y]
  "각 좌표에 대해 기준점에 대한 맨하탄 거리를 구하고, 거리가 같으면 nil, 같지 않으면 맨하탄거리가 제일 가까운 점을 [기준점x 기준점y] 형태로 반환"
  (let [distances (calculate-manhattan-distance-for-each-coord coords x y)]
    ; ([4 [1 1]] [4 [5 5]] [5 [8 3]] [5 [3 4]] [9 [1 6]] [11 [8 9]]) -> 4 4 로 맨하탄 거리가 같으므로 -> nil
    ; ([1 [1 1]] [4 [3 4]] [6 [1 6]] [7 [5 5]] [8 [8 3]] [14 [8 9]]) -> 1 4 로 [1 1]좌표가 제일 가까우므로 -> [1 1]
    (when-not (= (ffirst distances) (first (second distances)))
      (second (first distances)))))


(defn generate-result-matrix [coords [x1 x2 y1 y2]]
  "유효한 매트릭스 범위 내에서 가장 가까운 점을 나열"
  ; [1 1] [1 1] [1 1] [1 1] nil   [8 3] [8 3] [8 3]
  ; [1 1] [1 1] [3 4] [3 4] [5 5] [8 3] [8 3] [8 3]
  ; ... -> ([1 1] [1 1] [1 1] [1 1] nil   [8 3] [8 3] [8 3] [1 1] [1 1] [3 4] [3 4] [5 5] [8 3] [8 3] [8 3]...)
  (->> (for [y (range y1 (inc y2))
             x (range x1 (inc x2))]
    (closest-coord coords x y))))

(defn solve1 [coords]
    (->> coords
         get-matrix-area
         (generate-result-matrix coords)
         frequencies
         (sort-by second >)
         first
         second))

(comment
  (solve1 input)
  )


;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(defn get-manhattan-distance-sum [coords [x1 x2 y1 y2]]
  "각 좌표의 모든 기준점에 대한 거리 합을 구함"
  (for [y (range y1 (inc y2))
        x (range x1 (inc x2))]
    (reduce + (for [coord coords] (manhattan-distance coord [x y])))))

(defn solve2 [coords N]
  (->> (get-matrix-area coords)
       (get-manhattan-distance-sum coords)
       (filter #(< % N))
       count))

(comment
  (solve2 input)
  )
