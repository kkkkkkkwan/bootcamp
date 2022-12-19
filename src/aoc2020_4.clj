(ns aoc2020-4
  (:require [clojure.spec.alpha :as spec])
  (:require [clojure.java.io :as io])
  (:require
    [clojure.set :as set]
    [clojure.string :as str])
  )

(def input (-> "2020_4.in"
               (io/resource)
               (slurp)
               (str/split #"\n\n")
               ))

(defn split [regex str]
  (str/split str regex)
  )

(defn validate [data]
  (set/subset?
    #{:byr :iyr :eyr :hgt :hcl :ecl :pid}
    (set (keys data))
    )
  )

(comment
  (->> input
       (map parse-passport)
       (map validate)
       (filter true?)
       (count)
       )
  )

;; part 2
(defn parse-int [int-str]
  (try
    (Integer/parseInt int-str)
    (catch NumberFormatException e nil)
    )
  )

(spec/def :aoc2020-2/valid-birth-year? #(and (not (nil? %))
                                             (and (<= 1920 %) (>= 2002 %))))
(spec/def :aoc2020-2/valid-issue-year? #(and (not (nil? %))
                                             (and (<= 2010 %) (>= 2020 %))))
(spec/def :aoc2020-2/valid-expiration-year? #(and (not (nil? %))
                                                  (and (<= 2020 %) (>= 2030 %))))

(spec/def :aoc2020-2/valid-height? #(and (not (nil? %))
                                         (let [[height unit] (drop 1 (re-matches #"(\d+)(cm|in)" %))
                                               h (parse-int height)]
                                           (case unit
                                             "cm" (and (>= h 150) (<= h 193))
                                             "in" (and (>= h 59) (<= h 76))
                                             false))))

(spec/def :aoc2020-2/valid-hair-color? #(and (not (nil? %))
                                             (not (nil? (re-matches #"#[a-f0-9]{6}" %)))))
(spec/def :aoc2020-2/valid-eye-color? #(and (not (nil? %))
                                            (not (nil? ((set '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")) %)))))
(spec/def :aoc2020-2/valid-pid? #(and (not (nil? %))
                                      (not (nil? (re-matches #"\d{9}" %)))))

(spec/def :aoc2020-2/ecl :aoc2020-2/valid-eye-color?)

(spec/def :aoc2020-2/hcl :aoc2020-2/valid-hair-color?)
(spec/def :aoc2020-2/hgt :aoc2020-2/valid-height?)
(spec/def :aoc2020-2/pid :aoc2020-2/valid-pid?)
(spec/def :aoc2020-2/eyr :aoc2020-2/valid-expiration-year?)
(spec/def :aoc2020-2/byr :aoc2020-2/valid-birth-year?)
(spec/def :aoc2020-2/iyr :aoc2020-2/valid-issue-year?)
;(spec/def :aoc2020-2/cid (or nil? int? string?))

(spec/def :aoc2020-2/passport (spec/keys :req [:aoc2020-2/ecl
                                               :aoc2020-2/byr
                                               :aoc2020-2/hcl
                                               :aoc2020-2/hgt
                                               :aoc2020-2/pid
                                               :aoc2020-2/eyr
                                               :aoc2020-2/iyr]))

(defn line->passport [{:keys [ecl byr hcl hgt pid eyr iyr cid]}]
  {:aoc2020-2/ecl ecl
   :aoc2020-2/byr (parse-int byr)
   :aoc2020-2/hcl hcl
   :aoc2020-2/hgt hgt
   :aoc2020-2/pid pid
   :aoc2020-2/eyr (parse-int eyr)
   :aoc2020-2/iyr (parse-int iyr)
   :aoc2020-2/cid (parse-int cid)}
  )

(defn parse-passport [input]
  (->> input
       (split #"\s")
       (map #(split #":" %))
       (into {})
       clojure.walk/keywordize-keys
       )
  )

(defn validate-passport [data]
  (spec/valid? :aoc2020-2/passport data)
  )

(comment
  (->> input
       (map parse-passport)
       (map line->passport)
       (map validate-passport)
       (filter true?)
       (count)
       )

  (spec/valid? :aoc2020-2/valid-eye-color? "oth")
  (spec/valid? :aoc2020-2/valid-birth-year? 1921)
  (spec/valid? :aoc2020-2/valid-hair-color? "#b6652a")
  (spec/valid? :aoc2020-2/valid-height? "170cm")
  (spec/valid? :aoc2020-2/valid-expiration-year? 2029)
  (spec/valid? :aoc2020-2/valid-pid? "223041037")
  (spec/valid? :aoc2020-2/valid-issue-year? 2012)
  )
