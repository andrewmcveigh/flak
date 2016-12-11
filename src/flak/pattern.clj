(ns flak.pattern
  (:refer-clojure :exclude [case destructure let])
  (:require [clojure.spec :as s]))

(alias 'c 'clojure.core)

(defprotocol Destructurable
  (-destructure [x]))

(deftype Point [x y]
  Destructurable
  (-destructure [this] [(.-x this) (.-y this)]))

(defmethod print-method Point [m writer]
  (.write writer (format "#<Point %s, %s>" (.-x m) (.-y m))))

(deftype Nothing [])
(deftype Just [a])

(defmethod print-method Nothing [m writer]
  (.write writer "#<Nothing>"))

(defmethod print-method Just [m writer]
  (.write writer (format "#<Just %s>" (.-a m))))

(deftype Left [a])
(deftype Right [b])

(defmethod print-method Left [m writer]
  (.write writer (format "#<Left %s>" (.-a m))))

(defmethod print-method Right [m writer]
  (.write writer (format "#<Right %s>" (.-b m))))

(extend-protocol Destructurable
  Just
  (-destructure [x] [(.-a x)])
  Left
  (-destructure [x] [(.-a x)])
  Right
  (-destructure [x] [(.-b x)]))

(s/def ::destructurable-name
  (s/and symbol? #(re-matches #"^[A-Z][A-Za-z]*$" (name %))))

(s/def ::case-binding
  (s/or :type    ::destructurable-name
        :literal (s/and #(not (s/valid? ::case-destructuring %))
                        (complement #{:as})
                        (complement symbol?))
        :binding symbol?))

(s/def ::case-destructuring
  (s/and vector?
         (s/cat :type ::destructurable-name
                :args (s/+ ::case-pattern)
                :bind (s/? (s/cat :as #{:as} :binding symbol?)))))

(s/def ::case-pattern
  (s/or :binding ::case-binding
        :destructuring ::case-destructuring))

(defn arg-bindings [[t v g?]]
  (c/case t
    :binding (c/let [[btype binding] v]
               (c/case btype
                 :type '_
                 :literal '_
                 :binding binding))
    :destructuring g?))

(defn literal= [destructured args]
  (->> destructured
       (map (fn [[_ [btype a]] b] (when (= :literal btype) (= a b))) args)
       (remove nil?)
       (every? true?)))

(defn let-destructure [match mval value expr]
  (c/case match
    :destructuring
    (c/let [args (map (fn [[t :as v]]
                        (c/case t
                          :binding v
                          :destructuring (conj v (gensym))))
                      (:args mval))
            bind (:bind mval)
            dest (remove (comp #{:binding} first) args)
            dsym (gensym)]
      `(when (instance? ~(resolve (:type mval)) ~value)
         (c/let [value# ~value
                 ~dsym (-destructure value#)
                 ~(if bind (:binding bind) (gensym)) value#
                 [~@(map arg-bindings args)] ~dsym]
           (or ~(when (some (comp #{:literal} first second) args)
                  `(when (literal= ~dsym '~args)
                    ~(if (seq dest)
                       (apply let-destructure (conj (first dest) expr))
                       expr)))
               ~(when (every? (comp #{:binding} first second) args) expr)
               ~(when (not (some (comp #{:literal} first second) args))
                  (if (seq dest)
                    (apply let-destructure (conj (first dest) expr))
                    expr))))))
    :binding
    (c/let [[match' value'] mval]
      (prn match value match' value')
      (c/case match'
        :type `(when (instance? ~(resolve value') ~value) ~expr)
        :literal `(when (= ~value' ~value) ~expr)
        :binding `(c/let [~value' ~value] ~expr)))))

(defmacro let [bindings expr]
  (c/let [[bind value] (take 2 bindings)]
    (when (and bind value)
      (c/let [[match mval] (s/conform ::case-pattern bind)]
        (let-destructure match mval value expr)))))

(defmacro case [e & bindings]
  (cons 'or
        (map (fn [[pattern expr]] `(let [~pattern ~e] ~expr))
             (partition 2 bindings))))

(case (Just. (Just. (Point. 0 0)))
  "thing" 8
  [Just 1] 7
  [Point x y] [x y]
  ;; [Just a] a
  [Just [Point x y]] {:x x :y y}
  [Just [Just [Point x y]]] {:a x :b y}
  )
