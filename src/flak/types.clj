(ns flak.types
  (:refer-clojure :exclude [name])
  (:require
   [flak.functor :as f]
   [flak.monad :as m]
   [flak.pattern :as p]
   [flak.source :refer [source]]
   [flak.type :as t])
  (:import
   [flak.type Literal]
   [java.io LineNumberReader PushbackReader]))

(defprotocol Named (name [x]))
(t/def name Named -> String)

(t/class Named a
  (name a -> String))

(t/class Named a
  (name a -> String))

(clojure.spec/conform :flak.spec/signature '(Named a => a -> String))

(t/data  Boolean     (or True False))
(t/data (Maybe a)    (or Nothing (Just a)))
(t/data (Either a b) (or (Left a) (Right b)))
(t/data  Symbol      (or (SimpleSymbol String) (Symbol String String)))
(t/data  Keyword     (or (SimpleKeyword String) (Keyword String String)))
;; (t/data  List a      (or Nil (Cons a (List a))))

;; (t/def is-nothing? (Maybe a) -> Boolean)
;; (defn is-nothing? [a] (nothing? a))
;; (source is-nothing?)
;; '(defn is-nothing? [a] (nothing? a))

;; (t/def nothing? (Maybe a) -> Boolean)
(t/instance Named Symbol
  (name [[SimpleSymbol name]] name)
  (name [[Symbol _     name]] name))

(t/instance Named Keyword
  (name [[SimpleKeyword name]] name)
  (name [[Keyword _     name]] name))

(t/instance t/Show Symbol
  (show [[SimpleSymbol name]] name)
  (show [[Symbol    ns name]] (str ns \/ name)))

(t/instance t/Show Keyword
  (show [[SimpleKeyword name]] (str \: name))
  (show [[Keyword    ns name]] (str \: ns \/ name)))

(defmacro error
  ([type msg]
   `(left (ex-info ~msg {:type ~type})))
  ([type msg e]
   `(left (ex-info ~msg {:type ~type :cause ~e}))))

;; (t/def fmap (a -> b) -> ((f a) -> (f b)))
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

(extend-protocol t/Truthy
  nil     (t/truthy? [_] False)
  Literal (t/truthy? [x] (if (true? x) True False))
  Just    (t/truthy? [_] True)
  Right   (t/truthy? [_] True)
  Left    (t/truthy? [_] False))
