(ns flak.types
  (:refer-clojure :exclude [name])
  (:require
   [flak.pattern :as p]
   [flak.type :as t]))

(defprotocol Named (name [x]))

(t/data Boolean (or True False))
(t/data (Maybe a) (or Nothing (Just a)))
(t/data (Either a b) (or (Left a) (Right b)))
(t/data Symbol (or (SimpleSymbol String) (Symbol String String)))
(t/data Keyword (or (SimpleKeyword String) (Keyword String String)))

(t/def name Named -> String)
(t/instance Named Symbol
  (name [[SimpleSymbol name]] name)
  (name [[Symbol _     name]] name))

(t/instance Show Int
  (show [x] (str x)))

[:gen ({:name show, :args [x], :expr (str x)})]

[:spc ({:name name
        :args [{:type SimpleSymbol
                :args [[:binding [:binding name]]]}]
        :expr name}
       {:name name
        :args [{:type Symbol
                :args [[:binding [:binding _]] [:binding [:binding name]]]}]
        :expr name})]

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
