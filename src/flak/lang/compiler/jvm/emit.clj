(ns flak.lang.compiler.jvm.emit)

(defmulti emit :op)

(defmethod emit :def [ast])
(defmethod emit :quote [ast])
(defmethod emit :if [ast])
(defmethod emit :fn [ast])

(quote x)
(atom x)
(eq x y)
(cons x y)
(cond (x y) (w z) (t q))
(car x)
(cdr x)

