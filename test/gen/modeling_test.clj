(ns gen.modeling-test
  (:require [clojure.test :refer [testing deftest is]]
            [gen :as g]
            [gen.distribution.kixi :as dist]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.generative-function :as gf]))

(def simple-model
  (gen [switch]
    (let [x (gen/trace :initial-x dist/normal 0 1)]
      (if (< switch 0)
        (+ x (gen/trace :addition-to-x dist/normal 2 1))
        x))))

(comment
  ;; NOTE: the easiest way to inspect a trace is to use `(into {} <trace>)`
  (into {} (gf/simulate simple-model [])))

(deftest simple-model-tests
  (testing "if switch is negative, both keys are present"
    (is (= #{:initial-x :addition-to-x}
           (set
            (keys (gf/simulate simple-model [-1]))))))

  (testing "if switch is positive, only one key is present"
    (is (= #{:initial-x}
           (set
            (keys (gf/simulate simple-model [1])))))))

(def second-model
  (gen []
    (if (or (g/trace :first-choice dist/bernoulli)
            (g/trace :second-choice dist/bernoulli))
      "true!"
      "false!")))

(deftest second-model-tests
  (is (#{"true!" "false!"} (second-model))
      "return value is sensible")

  (let [ks (set
            (keys (gf/simulate second-model [])))]
    (is (or (= #{:first-choice} ks)
            (= #{:first-choice :second-choice} ks)))))
