(ns aoc2018-2)

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12
(def input (-> "aoc2018_2.sample.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

(defn increase-twice-or-thrice-count-if-exists [[twice, thrice] freq-set]
  (cond (and (contains? freq-set 2) (contains? freq-set 3)) [(inc twice) (inc thrice)]
        (contains? freq-set 2) [(inc twice) thrice]
        (contains? freq-set 3) [twice (inc thrice)]
        :else [twice thrice]))

(defn count-twice-thrice-from-frequency-sets [freq-sets]
  (reduce increase-twice-or-thrice-count-if-exists [0 0] freq-sets))

(defn make-frequency-set-data [input]
  #_(map #(-> % frequencies vals set) input)
  (-> (frequencies input)
      vals
      set)
  )
; thread last
(defn solve-p1 []
  (->> (map make-frequency-set-data input)
       count-twice-thrice-from-frequency-sets
       (apply *)))

(solve-p1)

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.
(def input2 (-> "aoc2018_2.sample.txt"
                (io/resource)
                (slurp)
                (clojure.string/split-lines)))

(defn just-diff-one? [[s1 s2]]
  (->> (map (fn [s1char s2char] (if (= s1char s2char) 0 1)) s1 s2)
       (apply +)
       (= 1))
  )

(defn get-if-same-character [char1 char2]
  (if (= char1 char2) char1))

(defn create-same-character-list [[string1 string2]]
  (map get-if-same-character string1 string2))

(defn create-pair-from-list-by-base-element [base string-list]
  (reduce (fn [acc, value] (conj acc (list base value))) `() string-list))

(defn generate-combinations-from-string-list-using-recur [string-list]
  (loop [result `() base-list string-list]
    (if (empty? base-list)
      result
      (recur (clojure.set/union result (create-pair-from-list-by-base-element (first base-list) (rest base-list))) (rest base-list))
      )))

(defn generate-combinations-from-string-list-using-for [string-list]
  (let [result `()]
    (for [word string-list
          :let [rest-list (remove #{word} string-list)]]
      (create-pair-from-list-by-base-element word rest-list)
      )
    )
  )

(defn solve-p2 []
  (->> (generate-combinations-from-string-list-using-recur input)
       (filter just-diff-one?)
       first
       create-same-character-list
       (apply str)))

(defn solve-p2-using-for []
  (->> (generate-combinations-from-string-list-using-for input)
       (reduce (fn [list pairs] (clojure.set/union list pairs)) `())
       (filter just-diff-one?)
       first
       create-same-character-list
       (apply str)))

(solve-p2-using-for)

(comment
  (generate-combinations-from-string-list-using-recur `("1" "2" "3" "4"))
  (->> (generate-combinations-from-string-list-using-for `("1" "2" "3" "4"))
       (reduce (fn [list pairs] (clojure.set/union list pairs)) `()))

  )

;; #################################
;; ###        Refactoring        ###
;; #################################

;; for문 써서 조합 만들어보기
;; destructuring