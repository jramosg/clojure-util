(ns clojure-util.util
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as string]))

(defn attach-namespace-to-keys
  "This function attaches namespace keywords to a map.
  See `clojure-util.util-test/attach-namespace-to-keys-test` for examples."
  [my-map namespace & {:keys [except]}]
  (let [keywords-to-rename (remove (set except) (keys my-map))
        new-keywords (mapv #(keyword (name namespace) (name %))
                           keywords-to-rename)]
    (set/rename-keys my-map (zipmap keywords-to-rename new-keywords))))

(defn not-more-than-one-decimal-separator?
  "Returns true if a string contains one or less decimal separators. False if not.
  See `clojure-util.util-test/not-more-than-one-decimal-separator-test` for examples."
  [s]
  (let [decimal-separator? #{\. \,}]
    (->
      (filter decimal-separator? s)
      count
      (<= 1))))

(defn one-or-more-digit?
  "Returns true if a string contains one or more digits. False if not.
  See `clojure-util.util-test/one-or-more-digit-test` for examples."
  [s]
  (boolean (some #(Character/isDigit %) s)))

(defn not-more-than-one-negative-sign?
  "Returns true if a string contains one or less negative signs. False if not.
  See `clojure-util.util-test/not-more-than-one-negative-sign-test` for examples."
  [s]
  (->
    (filter #{\-} s)
    count
    (<= 1)))

(defn valid-number-string?
  "Returns true if a string is a valid number and false if it is not a valid number.
  `\".\"` and `\",\"` are admitted as decimal separators.
  Empty strings are accepted
  See `clojure-util.util-test/valid-number-string-test` for examples.
"
  [number-str]
  (let [ans (-> number-str
                (string/replace #"," ".")
                (string/replace #"'" ".")
                (string/replace #" " "")
                string/trim)]
    (or
      (empty? ans)
      (and
        (one-or-more-digit? ans)
        (->> ans
             (map #(or (Character/isDigit %)
                       (= "." (str %))
                       (and (= "-" (str %))
                            (string/starts-with? ans "-")
                            (> (count ans) 1))))
             (every? true?))
        (not-more-than-one-decimal-separator? ans)
        (not-more-than-one-negative-sign? ans)))))

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
  (if (seq (str value))
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
   For example: (deaccent \"Polic??a\")->\"Policia\""
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
      (string/replace #"??" "")
      (string/replace #"`" "")
      (string/replace #"," "")
      (string/replace "(" "")
      (string/replace ")" "")
      (string/replace "[" "")
      (string/replace "]" "")
      (string/replace "{" "")
      (string/replace "}" "")
      (string/replace "\t" "")))

(defn read-file-and-create-if-needed [path]
  (try
    (io/reader path)
    (catch
      java.io.FileNotFoundException _
      (prn "Creating file " path)
      (spit path "")
      (prn "Created file " path)
      (io/reader path))))

(defn vector-from-txt
  "Reads a txt file and converts each line of the txt as an element in a vector"
  [filename]
  (with-open [rdr (io/reader filename)]
    (vec (line-seq rdr))))

(defn every-val-of-map-to-str
  "Converts every value of the map to a string.
  For example:
  ```
  (every-val-of-map-to-str {:unit \"??l\",
                            :max-value 1200.0,
                            :min-value {:man 1000
                                        :woman 1100}})
  => {:unit \"??l\", :max-value \"1200.0\", :min-value {:man \"1000\", :woman \"1100\"}}```"
  [m]
  (if (map? m)
    (into
      {}
      (map
        (fn [[e1 e2 :as v]]
          (if (map? (second v))
            {e1 (every-val-of-map-to-str e2)}
            (update v 1 str)))
        m))
    (prn "WARNING! Input should be a map")))

(defn index-by
  "Returns a map of the elements of `coll` keyed by the result of `f` on each
  element.  The value at each key will be a single element (in contrast to
  `clojure.core/group-by`).  Therefore `f` should generally return an unique
  key for every element - otherwise elements get discarded."
  [f coll]
  (persistent!
    (reduce
      (fn [ret x] (assoc! ret (f x) x))
      (transient {}) coll)))

(defn sort-by-reverse
  "Returns a reversed sorted sequence of the items in coll.
  Guaranteed to be stable: equal elements will not be reordered.  If coll is a
  Java array, it will be modified. To avoid this, sort a copy of the array."
  [keyfn coll]
  (sort-by keyfn #(compare %2 %1) coll))


(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defmacro if-seq-let
  "bindings => binding-form test

  If test is seq, evaluates then with binding-form bound to the value of
  test, if not, yields else"
  ([bindings then]
   `(if-let ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if (seq temp#)
          (let [~form temp#]
            ~then)
          ~else)))))

(defmacro when-seq-let
  "bindings => binding-form test

  When test is seq, evaluates body with binding-form bound to the value of test"
  [bindings & body]
  (assert-args
    (vector? bindings) "a vector for its binding"
    (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (when (seq temp#)
         (let [~form temp#]
           ~@body)))))