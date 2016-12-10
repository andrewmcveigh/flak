(ns flak.type
  (:refer-clojure :exclude [key name type])
  (:require [clojure.spec :as s]))

(alias 'c 'clojure.core)

(defn key [name]
  (keyword "flak.types" (c/name name)))

(deftype Type [tag value])

(defmethod print-method Type [t writer]
  (.write writer (format "#<Type[%s]: %s>" (.-tag t) (pr-str (.-value t)))))

(defmacro type [name spec]
  `(do
     (defn ~name [value#]
       (let [spec# ~spec]
         (assert (s/valid? spec# value#)
                 (format "Value did not match spec: %s\n%s" spec#
                         (s/explain-data spec# value#))))
       (Type. ~(key name) value#))
     (s/fdef ~name
       :args (s/cat :value ~spec)
       :ret (partial instance? Type))))

(deftype Sum [tag type value])

(defmethod print-method Sum [t writer]
  (.write writer
          (format "#<%s: %s>"
                  (c/name (.-type t))
                  (.-value t))))

(defmacro sum [name & types]
  (cons 'do (for [type types]
              `(defn ~type [value#]
                 (Sum. ~(key name) ~(key type) value#)))))

(either? (Left 10)) => true
(left? (Left 10)) => true
(pcase (Left 10) [Left x] x) => 10

(sum Either Left)

(type Character char?)
(type String    string?)
(type Integer   integer?)
(type Boolean   boolean?)
(type List      seq?)
(type Vector    vector?)
(type Map       map?)

(type Region #{10 11 12 13 14 15 16 17 18 19 20 21 22 23})

(s/def ::type-name
  (s/and symbol? #(re-matches #"^[A-Z][A-Za-z]*$" (c/name %))))

(s/def ::type-parameter
  (s/and symbol? #(re-matches #"^[a-z]$" (c/name %))))

(s/def ::type-args
  (s/+ (s/or ::type-name ::type-name ::type-parameter ::type-parameter)))

(s/def ::parameterized-constructor
  (s/and list? (s/cat ::type-name ::type-name ::type-args ::type-args)))

(s/def ::type-constructor
  (s/or ::type-name ::type-name
        ::parameterized-constructor ::parameterized-constructor))

(s/def ::sum-constructor
  (s/and list?
         (s/cat :_ #(= % 'or)
                ::type-constructor (s/+ ::type-constructor))))

(s/def ::data-constructor
  (s/or ::type-constructor ::type-constructor
        ::sum-constructor ::sum-constructor))

(s/def ::data-type
  (s/and list?
         (s/cat :type ::type-constructor
                :data ::data-constructor)))

(defmacro data [& declaration]
  `(let [{:keys [~'type ~'data]} (s/conform ::data-type '~declaration)]
     [~'type ~'data]
     ))

(data (Either a b) (or (Left a) (Right b)))
(data Dynamic )
(data Boolean (or True False))
(data (Maybe a) (or Nothing (Just a)))

(data (Point Int Int) (Point x y))

;; (pcase x ; <= x is Dynamic
;;   Integer (Right (Integer x))
;;   _       (Left "Error: Unknown type"))

;; (defn dyn-fn [x] x)

;; (defmacro ann [name & annotation])

;; (defn strict-fn [Just a] (Right a))
;; (defn strict-fn [_]      (Left (Error "Nothing")))

;;; At the point of entering the above dyn-fn, we have type information, but
;;; as soon as anything dynamic is called, we lose static type information. Is
;;; the entry into the world of dynamic typing as simple as no longer checking?
;;; If so, then can we just mark the type checker to stop checking further?
;;; We'd probably still want the same types. Left/Right/Just/Nothing are still
;;; Useful.

;;; We want a way to program completely dynamically, just like Clojure, but
;;; without the yucky java types.
;;; You wouldn't want to cast, or destructure types all the time. You'd want
;;; a more dynamic protocol/multimethod based dispatch.
;;; But you'd also want a type-fixed way to program. Which would be much like
;;; Haskell but with Lispyness, syntax, dependent (spec like) types, etc.
;;; The boundary between static and dynamic code is really important. There
;;; should be a very easy but strict way to blend the two.
;;; Something like a forced handler that casts to correct/incorrect.

;;; The collections protocols are a think of beauty, we want that

;;; But also ADTs, Functors, Monads, etc.
