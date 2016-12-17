(ns flak.type
  (:refer-clojure :exclude [def key keyword list map symbol type])
  (:require [clojure.spec :as s]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [flak.functor :as f]
            [flak.monad :as m]
            [flak.pattern :as p]))

(alias 'c 'clojure.core)

(defprotocol Show (show [t]))
(defprotocol Value (-value [_]))
(defprotocol Destructurable (-destructure [x]))
(defprotocol Truthy (truthy? [_]))
(defprotocol Named (name [x]))

(def ^:private -reg (atom {}))
(def ^:private -literals (atom #{}))

(deftype Literal [s])
(defmethod print-method Literal [t writer]
  (.write writer (name (.-s t))))


(def NaN (Literal. 'NaN))
(def -Infinity (Literal. '-Infinity))
(def Infinity (Literal. 'Infinity))

(def exclude-java-classes
  '[Character String Integer Boolean])

(doseq [c exclude-java-classes] (ns-unmap *ns* c))

(def word #"-|_|(?=[A-Z]+)")

(defn kebab-case [x]
  (letfn [(kebabize [s]
            (->> (string/split s word)
                 (c/map string/lower-case)
                 (string/join "-")))]
    (cond (symbol? x)  (c/symbol (kebabize (c/name x)))
          (keyword? x) (c/keyword (kebabize (c/name x)))
          (string? x)  (kebabize x)
          :else        (throw (ex-info (str "Don't know how to kebab:" x)
                                       {:type :unknown-type :x x})))))


(defmacro type [name spec]
  `(do
     (deftype ~name [~'value] Value (-value [~'_] ~'value))
     (defmethod print-method ~name [t# writer#]
       (.write writer# (format "#<%s: %s>" ~(c/name name) (pr-str (-value t#)))))
     (defn ~(kebab-case name) [value#]
       (let [spec# ~spec]
         (assert (s/valid? spec# value#)
                 (format "Value did not match spec: %s\n%s" spec#
                         (s/explain-data spec# value#))))
       (new ~name value#))
     (s/fdef ~(kebab-case name)
       :args (s/cat :value ~spec)
       :ret (partial instance? ~name))))

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

(defn type-params [type]
  (reduce (fn [init [k v]]
            (update init k (fnil conj []) v))
          {}
          (::type-args type)))

(defn type-constructor-ast [[t tc]]
  (case t
    ::type-name {:type tc}
    ::parameterized-constructor {:type (::type-name tc)
                                 :parameters (type-params tc)
                                 :args (c/mapv second (::type-args tc))}))

(defn data-constructor-ast [[d data]]
  (case d
    ::sum-constructor
    {:type 'Sum
     :variants (mapv type-constructor-ast
                     (::type-constructor data))}
    ::type-constructor
    {:type 'Data
     :constructor (type-constructor-ast data)}))

(defn type-ast [[t type] data-constructor]
  (case t
    ::parameterized-constructor
    (let [type-name (::type-name type)]
      [type-name
       (merge {:parameters (type-params type) :name type-name}
              data-constructor)])
    ::type-name
    [type (merge {:name type} data-constructor)]))

(defn is-sym? [s]
  (c/symbol (str (kebab-case (name s)) \?)))

(defn def-literal [type-name]
  `(do
     (ns-unmap ~*ns* '~type-name)
     (def ~type-name (Literal. '~type-name))
     (defn ~(is-sym? type-name) [value#]
       (= ~type-name value#))))

(defn def-parameterized-type [type-name arglist]
  `(do
     ;; ~(when (resolve type-name) `(ns-unmap ~*ns* '~type-name))
     (deftype ~type-name ~arglist
       Destructurable
       (-destructure [~'_] ~arglist))
     (defmethod print-method ~type-name [c# writer#]
       (if (satisfies? Show c#)
         (.write writer# (show c#))
         (throw
          (Exception.
           (str "Type " '~type-name " doesn't implement Show")))))
     (defn ~(kebab-case type-name) ~arglist
       (new ~type-name ~@arglist))
     (defn ~(is-sym? type-name) [value#]
       (instance? ~type-name value#))))

(defn normalize-arglist [args]
  (mapv #(if (s/valid? ::type-name %) (gensym) %) args))

(defn register-type! [type-name ast]
  (swap! -reg assoc type-name ast))

(defmacro data [& declaration]
  (let [{:keys [type data]} (s/conform ::data-type declaration)
        data-constructor    (data-constructor-ast data)
        [type-name ast]     (type-ast type data-constructor)]
    `(do
       (register-type! '~type-name '~ast)
       ~@(if (= 'Data (:type ast))
           (let [{:keys [type args]} (:constructor ast)
                 arglist (normalize-arglist args)]
             [(if (seq arglist)
                (def-parameterized-type type-name args)
                (def-literal type-name))])
           (for [variant (:variants ast)]
             (let [type-name  (:type variant)
                   parameters (:parameters variant)
                   arglist    (normalize-arglist (:args variant))]
               (if (seq arglist)
                 (def-parameterized-type type-name arglist)
                 (def-literal type-name)))))
       '~type-name)))

(s/def ::-> (partial = '->))

(s/def ::signature
  (s/and sequential?
         (s/cat :input ::type-constructor
                :_ ::->
                :return (s/or :value ::type-constructor
                              :function ::signature))))

(defmacro def [name & signature]
  (s/conform ::signature signature))


;; ;; (t/def add Int -> Int -> Int)

;; (defmacro guard [x & guards])

;; (let [x (dynamic 1 2 3)]
;;   (guard x
;;          Boolean (p/case x True 1 False 2)
;;          Keyword (name x)))

