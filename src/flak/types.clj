(ns flak.types
  (:refer-clojure :exclude [destructure name])
  (:require
   [flak.functor :as f]
   [flak.monad :as m]
   [flak.pattern :as p]
   [flak.source :refer [source]]
   [flak.type :as t]))

(t/class Show a
  (show a -> String))

(t/class Named a
  (name a -> String))

(t/class Value a
  (value a -> Any))

(t/class Destructurable a
  (destructure a -> (List b)))

(t/class Truthy a
  (truthy? a -> Bool))

(t/type Character char?)
(t/type String    string?)
(t/type Integer   integer?)

(t/data  Boolean     (or True False))
(t/data (Maybe a)    (or Nothing (Just a)))
(t/data (Either a b) (or (Left a) (Right b)))
(t/data  Symbol      (or (SimpleSymbol String) (Symbol String String)))
(t/data  Keyword     (or (SimpleKeyword String) (Keyword String String)))
;; (t/data  List a      (or Nil (Cons a (List a))))

(t/instance Named Symbol
  (name [[SimpleSymbol name]] name)
  (name [[Symbol _     name]] name))

(t/instance Named Keyword
  (name [[SimpleKeyword name]] name)
  (name [[Keyword _     name]] name))

(t/instance Show Symbol
  (show [[SimpleSymbol name]] name)
  (show [[Symbol    ns name]] (str ns \/ name)))

(t/instance Show Keyword
  (show [[SimpleKeyword name]] (str \: name))
  (show [[Keyword    ns name]] (str \: ns \/ name)))

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

(extend-protocol Truthy
  nil     (truthy? [_] False)
  Literal (truthy? [x] (if (true? x) True False))
  Just    (truthy? [_] True)
  Right   (truthy? [_] True)
  Left    (truthy? [_] False))
