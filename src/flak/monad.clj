(ns flak.monad
  (:refer-clojure :exclude [get let]))

(alias 'c 'clojure.core)

(defprotocol Monad
  (m-return [_ v])
  (m-bind [mv f]))

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
  (throw (RuntimeException. "return must only be called inside mlet context")))

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
