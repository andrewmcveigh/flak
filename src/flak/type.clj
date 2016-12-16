(ns flak.type
  (:refer-clojure :exclude [key keyword list map symbol type])
  (:require [clojure.spec :as s]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [flak.functor :as f]
            [flak.monad :as m]))

(def ^:private -reg (atom {}))

(alias 'c 'clojure.core)

(def NaN (Object.))
(def -Infinity (Object.))
(def Infinity (Object.))

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

(defprotocol Value (-value [_]))

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

(defprotocol Destructurable (-destructure [x]))

(deftype Symbol [ns name])
(defn symbol
  ([name] (symbol nothing name))
  ([ns name] (Symbol. ns name)))
(defmethod print-method Symbol [m writer]
  (.write writer
          (let [ns (.-ns m)
                name (.-name m)]
            (if (nothing? ns)
              name
              (str ns \/ name)))))

(deftype Keyword [ns name])
(defn keyword
  ([name] (keyword nothing name))
  ([ns name] (Keyword. ns name)))
(defmethod print-method Keyword [m writer]
  (.write writer
          (let [ns (.-ns m)
                name (.-name m)]
            (if (nothing? ns)
              (str \: name)
              (str \: ns \/ name)))))


(defn unquote-ks [ks x]
  (cond (contains? ks x)
        x
        (symbol? x)
        (c/list 'quote x)
        (seq? x)
        (cons 'list x)
        :else x))

(defmacro either [test expr]
  (let [ks (set (keys &env))]
    `(if ~test
       ~expr
       (m/m-return (left nil)
                   (left (c/list 'clojure.core/not
                                 ~(walk/postwalk (partial unquote-ks ks) test)))))))

(defmacro error
  ([type msg]
   `(left (ex-info ~msg {:type ~type})))
  ([type msg e]
   `(left (ex-info ~msg {:type ~type :cause ~e}))))

(extend-protocol f/Functor
  Left
  (f/-fmap [Fa f] Fa)
  Right
  (f/-fmap [Fa f] (right (f (.-b Fa)))))

(extend-protocol m/Monad
  Left
  (m/m-return [_ v] (right v))
  (m/m-bind   [m f] m)
  Right
  (m/m-return [_ v] (right v))
  (m/m-bind   [m f] (f (.-b m))))

(defmethod print-method Nothing [m writer]
  (.write writer "#<Nothing>"))

(defmethod print-method Just [m writer]
  (.write writer (format "#<Just %s>" (pr-str (.-a m)))))

(defmethod print-method Left [m writer]
  (.write writer (format "#<Left %s>" (pr-str (.-a m)))))

(defmethod print-method Right [m writer]
  (.write writer (format "#<Right %s>" (pr-str (.-b m)))))

(extend-protocol Destructurable
  Just
  (-destructure [x] [(.-a x)])
  Left
  (-destructure [x] [(.-a x)])
  Right
  (-destructure [x] [(.-b x)]))

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

(defmacro data [& declaration]
  (let [{:keys [type data]} (s/conform ::data-type declaration)
        [t type] type
        [d data] data
        data-constructor (case d
                           ::sum-constructor
                           {:type 'Sum
                            :variants (mapv type-constructor-ast
                                            (::type-constructor data))}
                           ::type-constructor {:type 'Data})
        [type-name ast]
        (case t
          ::parameterized-constructor
          (let [type-name (::type-name type)]
            [type-name
             (merge {:parameters (type-params type) :name type-name}
                    data-constructor)])
          ::type-name
          [type (merge {:name type} data-constructor)])]
    `(swap! -reg assoc '~type-name '~ast)
    (cons
     'do
     (for [variant (:variants ast)]
       (let [type-name  (:type variant)
             parameters (:parameters variant)
             arglist    (mapv #(if (s/valid? ::type-name %) (gensym) %)
                              (:args variant))]
         `(do
            (deftype ~type-name ~arglist
              ~@(when (seq arglist)
                  ['Destructurable
                   (c/list '-destructure
                           (mapv (constantly '_) arglist)
                           arglist)]))
            (defmethod print-method ~type-name [~'t ~'writer]
              ~(if (seq arglist)
                 `(->> (-destructure ~'t)
                       (c/map pr-str)
                       (string/join ",")
                       (format "#<%s: %s>" ~(c/name type-name))
                       (.write ~'writer))
                 `(.write ~'writer (format "#<%s>" (c/name ~type-name)))))
            (defn ~(c/symbol (str (kebab-case (name type-name)) \?)) [~'value]
              ~(if (seq arglist)
                 (c/list 'instance? type-name 'value)
                 (c/list '= type-name 'value)))
            ~@(when (seq arglist)
                [(c/list 'defn (kebab-case type-name) ['value]
                         (c/list 'new type-name 'value))])
            ~(mapv :type (:variants ast))))))))

(data Boolean (or True False))
(data (Maybe a) (or Nothing (Just a)))
(data (Either a b) (or (Left a) (Right b)))

(defprotocol Truthy (truthy? [_]))
(extend-protocol Truthy
  nil     (truthy? [_] false*)
  Class   (truthy? [x] (cond (true? x)  True
                             (false? x) False))
  True    (truthy? [_] true*)
  False   (truthy? [_] false*)
  Just    (truthy? [_] true*)
  Nothing (truthy? [_] false*)
  Right   (truthy? [_] true*)
  Left    (truthy? [_] false*))
