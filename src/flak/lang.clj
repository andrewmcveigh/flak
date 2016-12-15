(ns flak.lang
  (:refer-clojure :exclude [or when])
  (:require [flak.type :as t]))

(defmacro if* [test then else]
  (list 'if (list '= 't/true* (list 't/truthy? test)) then else))

(defmacro when [test then]
  (list `if* test then test))

(defmacro or
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if* or# or# (or ~@next)))))
