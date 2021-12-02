(ns clojure-util.util
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as string]))

(defn attach-namespace-to-keys
  "This function attaches namespace keywords to a map
  For example:
  (attach-namespace-to-keys
    {:first-name \"Jon\"
     :last-name \"Ramos\"
     :birthplace \"Urnieta\"
     :zipcode \"12345\"}
     :user
     :except [:zipcode])
    =>
    {:zipcode \"12345\",
     :user/first-name \"Jon\",
     :user/last-name \"Ramos\",
     :user/birthplace \"Urnieta\"}"
  [my-map namespace & {:keys [except]}]
  (let [keywords-to-rename (remove (set except) (keys my-map))
        new-keywords (mapv #(keyword (name namespace) (name %))
                           keywords-to-rename)]
    (set/rename-keys my-map (zipmap keywords-to-rename new-keywords))))

(defn valid-number-string?
  "Returns true if a string is a valid number and false if it is not a valid number.
  For example:
  (valid-number-string? \"2.1\")
  => true
  (valid-number-string? \"-2.1\")
  => true
  (valid-number-string? \"-\")
  => false
  (valid-number-string? \"-1.1.\")
  => false"
  [number-str]
  (let [ans (-> number-str
                (string/replace #"," ".")
                (string/replace #"'" ".")
                (string/replace #" " ""))]
    (and (->> ans
              (map #(or (Character/isDigit %)
                        (= "." (str %))
                        (and (= "-" (str %))
                             (string/starts-with? ans "-")
                             (> (count ans) 1))))
              (every? true?))
         (->> ans
              (map #(= "." (str %)))
              (remove false?)
              count
              (> 2))
         (->> ans
              (map #(= "-" (str %)))
              (remove false?)
              count
              (> 2)))))

(defn number-ans-val
  "Converts string to number, only in case input is a valid number.
  For example:
  (number-ans-val \"-1.1.\")
  => nil
  (number-ans-val \"-1.1\")
  => -1.1
  (number-ans-val \"10,1\")
  => 10.1"
  [number-str]
  (if (and (seq number-str) (valid-number-string? number-str))
    (let [val-str1 (some->
                     number-str
                     (string/replace #"," ".")
                     (string/replace #" " "")
                     (string/replace #"'" "."))
          val-str
          (if (empty? (first (string/split val-str1 #"\.")))
            (string/join ["0" val-str1])
            val-str1)]
      (if (not (empty? val-str))
        (cond
          (not (string? val-str)) nil
          (get (set val-str) \.) (read-string val-str)
          :else (Long/parseLong val-str 10))
        0))))

(defn round-with-n-decimals
  "Rounds a number with 'n' number of decimals.
  For example:
  (round-with-n-decimals 2 10.4312)
  => 10.43
  (round-with-n-decimals 1 10.46)
  => 10.5"
  [n value]
  (if-not (empty? (str value))
    (let [value (if (double? value)
                  value
                  (double value))
          rounded (some->> value (format (str "%." n "f")))
          val-vec (string/split rounded #"\.")]
      (if (= (second val-vec) "00")
        (-> val-vec first number-ans-val)
        (number-ans-val rounded)))))

(defn average
  "Returns the average number of numbers of a list"
  [numbers-coll]
  (let [numbers-coll (remove nil? numbers-coll)]
    (if (seq numbers-coll)
      (/ (reduce + numbers-coll )
         (count numbers-coll))
      0)))


(defn deaccent
  "Remove accent from string.
   For example: (deaccent \"Policía\")->\"Policia\""
  [str]
  (let [normalized (java.text.Normalizer/normalize str java.text.Normalizer$Form/NFD)]
    (string/replace normalized #"\p{InCombiningDiacriticalMarks}+" "")))


(defn normalize-string
  "Normalize string"
  [string]
  (-> string
      (string/lower-case)
      (string/replace #" " "")
      (java.text.Normalizer/normalize java.text.Normalizer$Form/NFD)
      (string/replace #"\p{InCombiningDiacriticalMarks}+" "")
      (string/replace #"\"" "")
      (string/replace #"'" "")
      (string/replace #"´" "")
      (string/replace #"`" "")
      (string/replace #"," "")
      (string/replace "(" "")
      (string/replace ")" "")
      (string/replace "[" "")
      (string/replace "]" "")
      (string/replace "{" "")
      (string/replace "}" "")
      (string/replace "\t" "")))


(defn vector-from-txt
  "Reads a txt file and converts each line of the txt as an element in a vector"
  [filename]
  (with-open [rdr (io/reader filename)]
    (vec (line-seq rdr))))