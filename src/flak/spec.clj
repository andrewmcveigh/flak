(ns flak.spec
  (:require [clojure.spec :as s]))

(s/def ::type-name
  (s/and symbol? #(re-matches #"^[A-Z][A-Za-z]*$" (name %))))

(s/def ::type-parameter
  (s/and symbol? #(re-matches #"^[a-z]$" (name %))))

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

(s/def ::binding
  (s/or :type    ::type-name
        :literal (s/and #(not (s/valid? ::destructuring %))
                        (complement #{:as})
                        (complement symbol?))
        :binding symbol?))

(s/def ::destructuring
  (s/and vector?
         (s/cat :type (s/? ::type-name)
                :args (s/+ ::pattern)
                :bind (s/? (s/cat :as #{:as} :binding symbol?)))))

(s/def ::pattern
  (s/or :binding ::binding
        :destructuring ::destructuring))

(s/def ::-> (partial = '->))

(s/def ::signature
  (s/and sequential?
         (s/cat :input ::type-constructor
                :_ ::->
                :return (s/or :value ::type-constructor
                              :function ::signature))))

(s/def ::binding-form
  (s/or :sym :clojure.core.specs/local-name
        :dst ::destructuring))

(s/def ::arg-list
  (s/coll-of :clojure.core.specs/local-name
             :kind vector?
             :min-count 1
             :max-count 1))

(s/def ::destructuring-arg-list
  (s/coll-of ::destructuring :kind vector? :min-count 1 :max-count 1))

(s/def ::generic-instance-impl
  (s/and seq? (s/cat :name symbol? :args ::arg-list :expr any?)))

(s/def ::specific-instance-impl
  (s/and seq? (s/cat :name symbol? :args ::destructuring-arg-list :expr any?)))

(s/def ::instance-impl
  (s/or :gen (s/coll-of ::generic-instance-impl :kind list? :max-count 1)
        :spc (s/coll-of ::specific-instance-impl :kind list?)))
