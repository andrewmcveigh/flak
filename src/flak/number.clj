(ns flak.number
  (:refer-clojure :exclude [read])
  (:require
   [flak.lang :as l]
   [flak.type :as t]
   [clojure.spec :as s]))

(def exclude-java-classes
  '[Integer])

(doseq [c exclude-java-classes] (ns-unmap *ns* c))

(s/def ::integer
  (s/conformer #(if (s/valid? integer? %) (bigint %) ::s/invalid)))

(s/def ::fractional
  (s/conformer (fn [n]
                 (if (or (instance? BigDecimal n)
                         (instance? Double n)
                         (instance? Float n))
                   (bigdec n)
                   ::s/invalid))
               identity))

(t/type Integer ::integer)
(t/type Fractional ::fractional)
(t/type Rational ratio?)

(def integer-pattern
  (re-pattern (str "([-+]?)(?:(0)|"
                   "([1-9][0-9]*)|"
                   "0[xX]([0-9A-Fa-f]+)|"
                   "0([0-7]+)|"
                   "([1-9][0-9]?)[rR]([0-9A-Za-z]+)|"
                   "0[0-9]+)(N)?")))

(def ratio-pattern #"([-+]?[0-9]+)/([0-9]+)")
(def fractional-pattern #"([-+]?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?")

(defn read-integer [groups]
  (if (nth groups 2)
    (t/just (integer 0))
    (let [negate? (= "-" (nth groups 1))
          a (cond
              (nth groups 3) [(nth groups 3) 10]
              (nth groups 4) [(nth groups 4) 16]
              (nth groups 5) [(nth groups 5) 8]
              (nth groups 7) [(nth groups 7) (Integer. (nth groups 6))]
              :else          [nil nil])
          n (a 0)]
      (if n
        (t/just (integer
                 (let [bn (BigInteger. n (int (a 1)))
                       bn (if negate? (* -1 bn) bn)]
                   bn)))
        t/nothing))))

(defn read-ratio
  [groups]
  (let [numerator (nth groups 1)
        denominator (nth groups 2)
        numerator (if (= \+ (first numerator))
                    (subs numerator 1)
                    numerator)]
    (t/just
     (rational (/ (BigInteger. numerator) (BigInteger. denominator))))))

(defn read-fractional [groups]
  (t/just
   (fractional
    (if (nth groups 4)
      (BigDecimal. (second groups))
      (BigDecimal. (first groups))))))

(defn read [s]
  (l/or (some->> s (re-matches integer-pattern) (read-integer))
        (some->> s (re-matches fractional-pattern) (read-fractional))
        (some->> s (re-matches ratio-pattern) (read-ratio))))
