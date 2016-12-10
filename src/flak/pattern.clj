(ns flak.pattern
  (:refer-clojure :exclude [case])
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

(s/conform ::case-pattern '[Just \@ a])
[:destructuring {:type Just, :args [[:binding [:literal \@]]]}]
;; [match value] (s/conform ::case-pattern '[Point 1 y])

(defmacro case [x & bindings]
  (list 'let ['x' x]
        (cons 'or
              (map (fn [[pattern expr]]
                     (let [[match value] (s/conform ::case-pattern pattern)]
                       (c/case match
                         :destructuring
                         (let [{:keys [type args bind]} value]
                           `(when (instance? ~(resolve type) ~'x')
                              (let [x# (-destructure ~'x')
                                    [~@(map (fn [[ptype [btype binding]]]
                                              (if (= :binding btype) binding '_))
                                            args)
                                     ~@(when bind [(:as bind) (:binding bind)])] x#]
                                (cond ~(some (comp #{:literal} first second) args)
                                      (when (->> x#
                                                 (map (fn [[_# [btype# a#]] b#]
                                                        (when (= :literal btype#) (= a# b#)))
                                                      ~args)
                                                 (remove nil?)
                                                 (every? true?))
                                        ~expr)
                                      ~(every? (comp #{:binding} first second) args)
                                      ~expr))))
                         :binding
                         (let [[match value] value]
                           (c/case match
                             :type `(when (instance? ~(resolve value) ~'x') ~expr)
                             :literal `(when (= ~value ~'x') ~expr)
                             :binding `(let [~value ~'x'] ~expr))))))
                   (partition 2 bindings)))))

(case 66
  x [x]
  Nothing 44
  ;; [Just _] 1
  ;; [Point 1 2] {:x 0 :y 1}
  ;; [Point 1 y] {:x 12 :y y}
;;  [Point x y] {:x x :y y}
  )

;;; what's the outcome?
;;; all bindings bound, destructuring
