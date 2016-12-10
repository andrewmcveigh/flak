(ns flak.core
  (:require
   [clojure.spec :as s]
   [flak.type :as T]))

(s/conform ::case-pattern '[Just \@])
[:destructuring {:type Just, :args [[:binding [:literal \@]]]}]

(let [[match value] (s/conform ::case-pattern '[Just \@])]
  (case pattern
    :destructuring
    (let [{:keys [type args bind]} value]
      (case ))
    :binding
    (let [[match value]]
      (case match
        :type
        :literal
        :binding))))

(s/conform ::case-pattern '[Just [Maybe s :as x]])
[:destructuring {:type Just, :args [[:destructuring {:type Maybe, :args [[:binding [:binding s]]], :bind {:as :as, :binding x}}]]}]

(s/conform ::case-pattern 'Nothing)
[:case-binding [:type Nothing]]

(s/conform ::case-pattern '_)
[:case-binding [:binding _]]

(s/conform ::case-pattern '[Just Nothing])
[:destructuring {:type Just, :args [[:binding [:type Nothing]]]}]


;; (s/conform ::case-args
;;            '([Just \@] (wrapping-reader 'unquote-splicing c)
;;              [Just ch] (>>= (unread-char ch)
;;                             (wrapping-reader 'unquote c))))
