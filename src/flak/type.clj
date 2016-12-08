(ns flak.type
  (:refer-clojure :exclude [key name type])
  (:require [clojure.spec :as s]))

(alias 'c 'clojure.core)

(defn key [name]
  (keyword "flak.types" (c/name name)))

(deftype Type [tag value])

(defmethod print-method Type [t writer]
  (.write writer (format "#<Type[%s]: %s>" (.-tag t) (.-value t))))

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
          (format "#<Sum[%s][%s]: %s>" (.-tag t) (.-type t) (.-value t))))

(defmacro sum [name & types]
  (cons 'do (for [type types]
              `(defn ~type [value#]
                 (Sum. ~(key name) ~(key type) value#)))))

(type Character char?)
(type String    string?)
(type Integer   integer?)
(type Boolean   boolean?)
(type List      seq?)
(type Vector    vector?)
(type Map       map?)

(s/def ::type-name
  (s/and symbol? #(re-matches #"^[A-Z][A-Za-z]*$" (c/name %))))

(s/def ::type-parameter
  (s/and symbol? #(re-matches #"^[a-z]$" (c/name %))))

(s/def ::type-args
  (s/+ (s/or ::type-name ::type-name ::type-parameter ::type-parameter)))

(s/def ::product-constructor
  (s/cat ::type-name ::type-name ::type-args ::type-args))

(s/def ::type-constructor
  (s/or ::type-name ::type-name ::type-args ::product-constructor))

(s/def ::sum-constructor
  (s/cat :or #(= % 'or) ::type-constructor (s/+ ::type-constructor)))

(s/def ::data-constructor (s/or ::type-constructor ::sum-constructor))

(s/conform ::sum-constructor '(or Nothing (Just a)))

(s/def ::data-type
  (s/cat ::type-constructor ::type-constructor
         ::data-constructor ::data-constructor))

(s/conform ::data-type '[(Maybe a) (or Nothing (Just a))])

(data (Either a b) (or (Left a) (Right b)))
(data (Point Int Int) (Point x y))

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
