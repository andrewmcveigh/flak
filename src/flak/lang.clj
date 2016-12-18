(ns flak.lang
  (:refer-clojure :exclude [defn fn let or when])
  (:require [flak.type :as t]
            [clojure.spec :as s]))

(alias 'c 'clojure.core)

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

(defmacro fn [args expr]
  `(clojure.core/fn ~args ~expr))

(defmacro let1 [bindings expr]
  `((fn [~(first bindings)]
      ~expr)
    ~(second bindings)))

(defmacro let [bindings expr]
  `(let1 [~(first bindings) ~(second bindings)]
     ~(if (second (next (next bindings)))
        `(let ~(vec (next (next bindings)))
           ~expr)
        expr)))

(defmacro defn [& args]
  `(let [defn-args# (s/conform :clojure.core.specs/defn-args '~args)
         sig# (t/signature (symbol ~(name (.getName *ns*))
                                   (name (:name defn-args#))))
         [t# bs#] (:bs defn-args#)]
     (case t#
       :arity-1
       (let [args# (-> bs# :args :args)
             expr# (-> bs# :body first)]
         [sig# args# expr#]))))
