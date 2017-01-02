(ns flak.lang.spec
  (:require
   [clojure.spec :as s]))

(s/def ::expr any?)

(s/def ::name
  (s/and symbol? #(re-matches #"^[^A-Z&%]+$" (name %))))

(s/def ::def
  (s/cat :def  #{'def}
         :name ::name
         :expr ::expr))

(s/def ::if
  (s/cat :test ::expr
         :then ::expr
         :else ::expr))

(s/def ::binding-form
  (s/or :name ::name ))

(s/def ::fn
  (s/cat :name (s/? ::name)
         :args (s/coll-of ::binding :kind vector?)
         :expr ::expr))
