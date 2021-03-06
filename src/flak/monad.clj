(ns flak.monad
  (:refer-clojure :exclude [get let])
  (:require [flak.type :as t]))

(alias 'c 'clojure.core)

(t/class Monad m
  (>>=     m a -> (a -> m b) -> m b)
  (>>      m a -> m b -> m b)
  ;; (return  a -> m a)
  (fail    String -> m a))

(clojure.spec/explain-data :flak.spec/class-decl
                           '(Monad m
                              (>>=     m a -> (a -> m b) -> m b)
                              (>>      m a -> m b -> m b)
                              (return  a -> m a)
                              (fail    String -> m a)))

(deftype State [f]
  Monad
  (m-return [_ v]
    (State. (fn [s] [v s])))
  (m-bind   [m f]
    (State. (fn [s]
              (c/let [[v s'] ((.-f m) s)
                      m2     (f v)
                      f2     (.-f m2)]
                (f2 s'))))))

(defmethod print-method State [m writer]
  (.write writer (format "#<State %s>" (.-f m))))

(defn state [x] (m-return (State. identity) x))

(defn get
  []
  (State. (fn [s] [s s])))

(defn put
  [s]
  (State. (fn [_] [nil s])))

(defn modify
  [f & args]
  (State. (fn [s]
            (c/let [s' (apply f s args)] [s' s']))))

(defn run-state [m s]
  ((.-f m) s))

(defn eval-state [m s]
  (first (run-state s)))

(defn exec-state [m s]
  (second (run-state s)))

(defn return [& args]
  (throw
   (RuntimeException.
    "return must only be called inside flak.monad/let context")))

(defmacro let [bindings expr]
  (c/let [mt     (second bindings)
          steps  (reverse (partition 2 bindings))
          result (reduce (fn [expr [sym mv]]
                           `(m-bind ~mv
                                    (fn [~sym] ~expr)))
                         expr
                         steps)]
    `(c/let [~'return (partial m-return ~mt)]
       (if (= (class ~result) (class ~mt))
         ~result
         (throw
          (ex-info "Monad Types wrong"
                   {:error `(not (= ~(class ~result) ~(class ~mt)))}))))))

(defn >>= [mv f] (m-bind mv f))

(defn <=<
  ([g f] (fn [x] (>>= (f x) g)))
  ([h g & fs] (reduce <=< (cons h (cons g fs)))))
