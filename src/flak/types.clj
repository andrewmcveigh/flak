(ns flak.types
  (:require [flak.type :as t]))

(t/data Boolean (or True False))
(t/data (Maybe a) (or Nothing (Just a)))
(t/data (Either a b) (or (Left a) (Right b)))
(t/data Symbol (or (SimpleSymbol String) (Symbol String String)))
(t/data Keyword (or (SimpleKeyword String) (Keyword String String)))

(extend-protocol Named
  Symbol
  (name [x]
    (p/case x
      [SimpleSymbol name] name
      [Symbol _ name]     name)))

(t/def name (Named a) -> (String -> (Named a)))

(extend-protocol Show
  Symbol
  (show [t]
    (let [[ns name] (-destructure t)]
      (if (nothing? ns)
        name
        (str ns \/ name)))))

(extend-protocol Show
  Keyword
  (show [t]
    (let [[ns name] (-destructure t)]
      (if (nothing? ns)
        (str \: name)
        (str \: ns \/ name)))))

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
