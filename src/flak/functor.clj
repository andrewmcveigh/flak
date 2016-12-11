(ns flak.functor)

(defprotocol Functor
  (-fmap [Fa f]))

(defn fmap [f fa] (-fmap fa f))
