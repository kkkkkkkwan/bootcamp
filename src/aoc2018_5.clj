(ns aoc2018_5
  (:require [clojure.string :as str]))
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.
(def input (-> "resources/5.txt"
               slurp
               str/trim-newline))

(defn react? [char1 char2]
  (if (and (some? char1) (some? char2))
    (and
      (not= char1 char2)
      (= (str/lower-case char1) (str/lower-case char2))
      )
    false
    )

  )

(defn solve1 [char-list]
  (->> (reduce (fn [result x]
                 (if (react? x (first result))
                   (rest result)
                   (cons x result)
                   )
                 ) [(first char-list)] (rest char-list))
       (count))
  )

(comment
  (solve1 input)
  )

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.
(def a-to-z "abcdefghijklmnopqrstuvwxyz")

(defn remove-each-char-from-string-to-list [string]
  (for [char a-to-z]
    (-> string
        (str/replace (re-pattern (str #"(?i)"char)) "")
        str/trim)
    )
  )

(comment
  (str/replace "asdfASDF" (re-pattern (str \a)) "")
  (str/replace "asdfASDF" (re-pattern (str #"(?i)" \a)) "")
  )

(defn solve2 [input]
  (->> input
       remove-each-char-from-string-to-list
       (map solve1)
       sort
       first
       )
  )

(comment
  (solve2 input)
  )
