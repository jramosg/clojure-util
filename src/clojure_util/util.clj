(ns clojure-util.util
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as string]))

(defn attach-namespace-to-keys
  "This function attaches namespace keywords to the passing map"
  [map namespace & {:keys [except]}]
  (let [keywords-to-rename (remove (set except) (keys map))
        new-keywords (mapv #(keyword (name namespace) (name %))
                           keywords-to-rename)]
    (set/rename-keys map (zipmap keywords-to-rename new-keywords))))


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
   For example: (clojure-util.util/deaccent \"Policía\")->\"Policia\""
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