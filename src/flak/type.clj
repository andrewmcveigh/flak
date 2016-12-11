(ns flak.type
  (:refer-clojure :exclude [key name type])
  (:require [clojure.spec :as s]
            [clojure.string :as string]))

(alias 'c 'clojure.core)

(def exclude-java-classes
  '[Character String Integer Boolean])

(doseq [c exclude-java-classes] (ns-unmap *ns* c))

(def word #"-|_|(?=[A-Z]+)")

(defn kebab-case [x]
  (letfn [(kebabize [s]
            (->> (string/split s word)
                 (map string/lower-case)
                 (string/join "-")))]
    (cond (symbol? x)  (symbol (kebabize (c/name x)))
          (keyword? x) (keyword (kebabize (c/name x)))
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

(defmacro data [& declaration]
  `(let [{:keys [~'type ~'data]} (s/conform ::data-type '~declaration)]
     [~'type ~'data]
     ))

;; (data (Either a b) (or (Left a) (Right b)))
;; (data Dynamic )
;; (data Boolean (or True False))
;; (data (Maybe a) (or Nothing (Just a)))
;; (data (Point Int Int) (Point x y))

(defprotocol Destructurable
  (-destructure [x]))

(deftype Nothing [])
(defn nothing [] (Nothing.))
(deftype Just [a])
(defn just [a] (Just. a))
(deftype Left [a])
(defn left [a] (Left. a))
(deftype Right [b])
(defn right [b] (Right. b))

(defmethod print-method Nothing [m writer]
  (.write writer "#<Nothing>"))

(defmethod print-method Just [m writer]
  (.write writer (format "#<Just %s>" (.-a m))))

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
