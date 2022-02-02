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

(run-tests)