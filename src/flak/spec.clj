(ns flak.spec
  (:refer-clojure :exclude [not])
  (:require [clojure.spec :as s]))

(alias 'c 'clojure.core)

(defn not [spec]
  (complement (partial s/valid? spec)))

(s/def ::type-name
  (s/and simple-symbol? #(re-matches #"^[A-Z][A-Za-z]*$" (name %))))

(s/def ::type-parameter
  (s/and simple-symbol? #(re-matches #"^[a-z]$" (name %))))

(s/def ::type-args
  (s/+ (s/or ::type-name ::type-name ::type-parameter ::type-parameter)))

(s/def ::parameterized-constructor
  (s/and list? (s/cat ::type-name ::type-name ::type-args ::type-args)))

(s/def ::type-constructor
  (s/or ::type-name ::type-name
        ::type-var  ::type-parameter
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
        :literal (s/and (not ::destructuring)
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

(s/def ::typeclass-sig
  (s/cat :class ::type-name
         :tvars (s/+ ::type-parameter)
         :=>    (partial = '=>)))

(s/def ::signature
  (s/and sequential?
         (s/cat :class (s/? ::typeclass-sig)
                :input ::type-constructor
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

(s/def ::class-function-signature
  (s/cat :input ::type-parameter
         :_ ::->
         :return (s/or :value ::type-constructor
                       :function ::signature)))

(s/def ::class-decl
  (s/and seq?
         (s/cat :class ::type-name
                :tvars (s/+ ::type-parameter)
                :tsign (s/+ (s/and seq?
                                   (s/cat :name :clojure.core.specs/local-name
                                          :tsig ::class-function-signature))))))

(s/def ::type-constructor
  (s/or ::type-name ::type-name
        ::type-var  ::type-parameter
        ::parameterized-constructor ::parameterized-constructor))

(s/def ::type-variable
  (s/and simple-symbol? #(re-matches #"^[a-z]$" (name %))))

(s/def ::list-a
  (s/and seq? (s/cat :a ::type)))

(s/def ::vector-a
  (s/and vector? (s/cat :a ::type)))

(s/def ::pair-a
  (s/and vector? (s/cat :a ::type :b ::type)))

(s/def ::n-tuple-a
  (s/and vector? (s/cat :a ::type :b ::type :ns (s/+ ::type))))

(s/def ::type
  (s/+ (s/alt :type ::type-name
              :tvar ::type-variable
              :coll (s/or :list ::list-a
                          :vect ::vector-a
                          :pair ::pair-a
                          :tupl ::n-tuple-a)
              :expr (s/and seq?
                           #(> (count %) 1)
                           (s/or :type ::type :func ::function-type)))))

(s/def ::function-type
  (s/cat :input ::type
         :-> ::->
         :return (s/alt :type ::type :func ::function-type)))

(s/def ::type-signature
  (s/alt :type ::type
         :func ::function-type))

(s/def ::function-name
  (s/and symbol? #(re-matches #"^[^A-Z&%]+$" (name %))))

(s/def ::default-class-impl
  (s/alt :expr any?
         :lambda (s/cat :binding (s/coll-of ::binding-form :kind vector?)
                        :expr any?)))

(s/def ::class
  (s/and seq?
         (s/cat :class ::type-name
                :tvars (s/+ ::type-variable)
                :fsigs (s/+ (s/and seq?
                                   (s/cat :name ::function-name
                                          :body (s/alt :typesig ::function-type
                                                       :default ::default-class-impl)))))))

;; (-> (s/conform
;;      ::class
;;      '(Monad m
;;              (>>=     m a -> (a -> m b) -> m b)
;;              (>>      m a -> m b -> m b)
;;              (return  a -> m a)
;;              (return  [a] a)
;;              (fail    String -> m a)))
;;     :fsigs
;;     first
;;     )
;; load
;; '{Monad {:class Monad
;;          :tvars [m]
;;          :fsigs {>>= {:args }}
;;          }}
