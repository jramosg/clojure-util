(ns clojure-util.util-test
  (:require [clojure.test :refer :all]
            [clojure-util.util :as util]))

(deftest attach-namespace-to-keys-test
  (testing "Attaching namespace to a map"
    (testing "Without exception"
      (is (= (util/attach-namespace-to-keys
               {:first-name "Jon"
                :last-name "Ramos"
                :birthplace "Urnieta"}
               :user)
             {:user/first-name "Jon",
              :user/last-name "Ramos",
              :user/birthplace "Urnieta"})))
    (testing "With exception"
      (is (= (util/attach-namespace-to-keys
               {:first-name "Jon"
                :last-name "Ramos"
                :birthplace "Urnieta"
                :zipcode "12345"}
               :user
               :except [:zipcode])
             {:zipcode "12345",
              :user/first-name "Jon",
              :user/last-name "Ramos",
              :user/birthplace "Urnieta"})))))

(deftest not-more-than-one-decimal-separator-test
  (testing "Check if a string has one or zero decimal separator"
    (testing "One or none decimal separators"
      (is (true? (util/not-more-than-one-decimal-separator? "1")))
      (is (true? (util/not-more-than-one-decimal-separator? "2,1")))
      (is (true? (util/not-more-than-one-decimal-separator? "2.1")))
      (is (true? (util/not-more-than-one-decimal-separator? "-2.1"))))
    (testing "Two or more decimal separators"
      (is (false? (util/not-more-than-one-decimal-separator? "-1.1.")))
      (is (false? (util/not-more-than-one-decimal-separator? "1,1."))))))

(deftest one-or-more-digit-test
  (testing "Check if a string has one or more digits"
    (testing "One or more digits"
      (is (true? (util/one-or-more-digit? "1")))
      (is (true? (util/one-or-more-digit? "2,1")))
      (is (true? (util/one-or-more-digit? "2.1,12")))
      (is (true? (util/one-or-more-digit? "-2.1"))))
    (testing "Zero digits"
      (is (false? (util/one-or-more-digit? ".")))
      (is (false? (util/one-or-more-digit? "")))
      (is (false? (util/one-or-more-digit? "-"))))))

(deftest not-more-than-one-negative-sign-test
  (testing "Check if a string has one or more digits"
    (testing "Zero or one negative signs"
      (is (true? (util/not-more-than-one-negative-sign? "1")))
      (is (true? (util/not-more-than-one-negative-sign? "-2,1")))
      (is (true? (util/not-more-than-one-negative-sign? "")))
      (is (true? (util/one-or-more-digit? "-2.1"))))
    (testing "More than one negative signs"
      (is (false? (util/not-more-than-one-negative-sign? "-1-2")))
      (is (false? (util/not-more-than-one-negative-sign? "111-22-1"))))))

(deftest valid-number-string-test
  (testing "Check if a string is a valid number"
    (testing "Valid numbers"
      (is (true? (util/valid-number-string? "1")))
      (is (true? (util/valid-number-string? "-1")))
      (is (true? (util/valid-number-string? "2,1")))
      (is (true? (util/valid-number-string? "2.1")))
      (is (true? (util/valid-number-string? "-2.1")))
      (is (true? (util/valid-number-string? "-2,1"))))
    (testing "Invalid numbers"
      (is (false? (util/valid-number-string? "-")))
      (is (false? (util/valid-number-string? "-1.1.")))
      (is (false? (util/valid-number-string? "1,1.")))
      (is (false? (util/valid-number-string? "."))))))

(deftest round-with-n-decimals-test
  (testing "Testing that numbers are round correctly and with specified decimals"
    (is (= 12.34 (util/round-with-n-decimals 2 12.344)))
    (is (= 12 (util/round-with-n-decimals 0 12.344)))
    (is (= 12.7 (util/round-with-n-decimals 1 12.674)))
    (is (nil? (util/round-with-n-decimals 0 nil)))))

(deftest every-val-of-map-to-str-test
  (testing "Check that every value of a map is converted to string"
    (is
      (=
        (util/every-val-of-map-to-str
          {:unit "µl"
           :max-value 1200.0
           :min-value {:man 1000 :woman 1100}})
        {:unit "µl"
         :max-value "1200.0"
         :min-value {:man "1000" :woman "1100"}}))
    (is
      (=
        (util/every-val-of-map-to-str
          {:name "Jon"
           :liken-fruits ["apple" "banana"]})
        {:name "Jon"
         :liken-fruits (str ["apple" "banana"])}))
    (is
      (=
        (util/every-val-of-map-to-str
          [{:name "Jon"
            :liked-fruits ["apple" "banana"]}])
        nil))
    (is
      (=
        (mapv
          util/every-val-of-map-to-str
          [{:name "Jon"
            :liked-fruits ["apple" "banana"]}
           {:name "Ane"
            :liked-fruits ["orange"]}])
        [{:name "Jon"
          :liked-fruits (str ["apple" "banana"])}
         {:name "Ane"
          :liked-fruits (str ["orange"])}]))))

(deftest index-by-test
  (testing "`index-by fn returns a new map with the elements of `coll` keyed by the result of `f``."
    (is (= (util/index-by :id [{:id 1 :questions [1 2 3]} {:id 2 :questions [1 2 4]}])
           {1 {:id 1, :questions [1 2 3]}, 2 {:id 2, :questions [1 2 4]}}))))

(run-tests)