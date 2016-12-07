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

(data Maybe (or Nothing (Just a)))

(data Either (or (Left a) (Right b)))
