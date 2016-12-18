(ns flak.type
  (:refer-clojure :exclude [class def type])
  (:require
   [clojure.spec :as spec]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [flak.functor :as f]
   [flak.monad :as m]
   [flak.spec :as s]))

(alias 'c 'clojure.core)

;; (defprotocol Show (show [t]))
;; (defprotocol Value (-value [_]))
;; (defprotocol Destructurable (-destructure [x]))
;; (defprotocol Truthy (truthy? [_]))

(def ^:private -reg (atom {}))
(def ^:private -literals (atom #{}))
(def ^:private -signatures (atom {}))

(deftype Literal [s])
(defmethod print-method Literal [t writer]
  (.write writer (name (.-s t))))
(defn literal [s] (Literal. s))

(def NaN (Literal. 'NaN))
(def -Infinity (Literal. '-Infinity))
(def Infinity (Literal. 'Infinity))

(def word #"-|_|(?=[A-Z]+)")

(defn kebab-case [x]
  (letfn [(kebabize [s]
            (->> (string/split s word)
                 (c/map string/lower-case)
                 (string/join "-")))]
    (cond (symbol? x)  (c/symbol (kebabize (c/name x)))
          (keyword? x) (c/keyword (kebabize (c/name x)))
          (string? x)  (kebabize x)
          :else        (throw (ex-info (str "Don't know how to kebab:" x)
                                       {:type :unknown-type :x x})))))

(defmacro type [name spec]
  `(do
     ~(when (resolve name)
        `(ns-unmap ~*ns* '~name))
     (deftype ~name [~'value] ~'Value (value [~'_] ~'value))
     (defmethod print-method ~name [t# writer#]
       (.write writer# (format "#<%s: %s>" ~(c/name name) (pr-str (~'value t#)))))
     (defn ~(kebab-case name) [value#]
       (let [spec# ~spec]
         (assert (spec/valid? spec# value#)
                 (format "Value did not match spec: %s\n%s" spec#
                         (spec/explain-data spec# value#))))
       (new ~name value#))
     (spec/fdef ~(kebab-case name)
       :args (spec/cat :value ~spec)
       :ret (partial instance? ~name))))

(defn type-params [type]
  (reduce (fn [init [k v]]
            (update init k (fnil conj []) v))
          {}
          (::s/type-args type)))

(defn type-constructor-ast [[t tc]]
  (case t
    ::s/type-name {:type tc}
    ::s/parameterized-constructor {:type (::s/type-name tc)
                                 :parameters (type-params tc)
                                 :args (c/mapv second (::s/type-args tc))}))

(defn data-constructor-ast [[d data]]
  (case d
    ::s/sum-constructor
    {:type 'Sum
     :variants (mapv type-constructor-ast
                     (::s/type-constructor data))}
    ::s/type-constructor
    {:type 'Data
     :constructor (type-constructor-ast data)}))

(defn type-ast [[t type] data-constructor]
  (case t
    ::s/parameterized-constructor
    (let [type-name (::s/type-name type)]
      [type-name
       (merge {:parameters (type-params type) :name type-name}
              data-constructor)])
    ::s/type-name
    [type (merge {:name type} data-constructor)]))

(defn is-sym? [s]
  (c/symbol (str (kebab-case (name s)) \?)))

(defn def-literal [type-name]
  `(do
     (ns-unmap ~*ns* '~type-name)
     (def ~type-name (literal '~type-name))
     (defn ~(is-sym? type-name) [value#]
       (= ~type-name value#))))

(defn def-parameterized-type [type-name arglist]
  `(do
     ;; ~(when (resolve type-name) `(ns-unmap ~*ns* '~type-name))
     (deftype ~type-name ~arglist
       ~'Destructurable
       (~'destructure [~'_] ~arglist))
     (defmethod print-method ~type-name [c# writer#]
       (if (satisfies? ~'Show c#)
         (.write writer# (~'show c#))
         (throw
          (Exception.
           (str "Type " '~type-name " doesn't implement Show")))))
     ~(when (resolve (kebab-case type-name))
        `(ns-unmap *ns* '~(kebab-case type-name)))
     (defn ~(kebab-case type-name) ~arglist
       (new ~type-name ~@arglist))
     ~(when (resolve (is-sym? type-name))
        `(ns-unmap *ns* '~(is-sym? type-name)))
     (defn ~(is-sym? type-name) [value#]
       (instance? ~type-name value#))))

(defn normalize-arglist [args]
  (mapv #(if (spec/valid? ::s/type-name %) (gensym) %) args))

(defn register-type! [type-name ast]
  (swap! -reg assoc type-name ast))

(defmacro data [& declaration]
  (let [{:keys [type data]} (spec/conform ::s/data-type declaration)
        data-constructor    (data-constructor-ast data)
        [type-name ast]     (type-ast type data-constructor)]
    `(do
       (register-type! '~type-name '~ast)
       ~@(if (= 'Data (:type ast))
           (let [{:keys [type args]} (:constructor ast)
                 arglist (normalize-arglist args)]
             [(if (seq arglist)
                (def-parameterized-type type-name args)
                (def-literal type-name))])
           (for [variant (:variants ast)]
             (let [type-name  (:type variant)
                   parameters (:parameters variant)
                   arglist    (normalize-arglist (:args variant))]
               (if (seq arglist)
                 (def-parameterized-type type-name arglist)
                 (def-literal type-name)))))
       '~type-name)))

(defn register-signature! [name ast]
  (swap! -signatures assoc name ast)
  name)

(defn- ->sym [v]
  (symbol (str (.name (.ns v))) (str (.sym v))))

(defn qualify [ns x]
  (or (some-> x resolve ->sym)
      (symbol (name (.name ns)) (name x))))

(defmacro def [name & signature]
  `(let [ast# (spec/conform ::s/signature '~signature)]
     (if (= ::spec/invalid ast#)
       (throw (ex-info (format "Invalid type signature %s" ~(pr-str signature))
                       {:type ::invalid-type-signature
                        :signature '~signature
                        :ast ast#
                        :how (spec/explain-data ::s/signature '~signature)}))
       (register-signature! '~(qualify *ns* name) ast#))))

(defmacro class [& decl]
  (let [class-decl (spec/conform ::s/class-decl decl)
        _ (when (= ::spec/invalid class-decl)
            (throw (ex-info (format "Invalid class signature %s"
                                    (pr-str decl))
                            {:type ::invalid-type-signature
                             :signature decl
                             :how (spec/explain-data ::s/class-decl
                                                     class-decl)})))
        {:keys [class tvars tsign]} class-decl
        tsig (spec/unform ::s/class-function-signature (:tsig tsign))]
    `(do
       (t/def ~(:name tsign) ~class ~@tvars ~'=> ~@tsig)
       (defprotocol ~class (~(:name tsign) ~tvars)))))

(defn render-bindings [bindings]
  (mapv (comp (fn [[t x]]
                (case t :binding x))
              second)
        bindings))

(defmacro instance [typeclass type & impls]
  (let [[t impls'] (spec/conform ::s/instance-impl impls)]
    `(do
       ~@(case t
           :gen (for [{:keys [name args expr]} impls']
                  `(extend-protocol ~typeclass ~type (~name ~args ~expr)))
           :spc (for [{:keys [name args expr]} impls']
                  (let [{:keys [type args]} (first args)]
                    `(extend-protocol ~typeclass
                       ~type
                       (~name [x#]
                        (let  [~(render-bindings args) (~'destructure x#)]
                          ~expr)))))))))

(defn signature [sym]
  (get @-signatures sym))

(defmacro infer [expr]
  (let [[op :as expr'] (macroexpand expr)]
    (case op
      'fn* (let [argspec (spec/coll-of :clojure.core.specs/args+body
                                       :max-count 1)
                 [sig] (spec/conform argspec (rest expr'))
                 args (-> sig :args :args)
                 expr (-> sig :body first)]
             `['~(mapv second args) '~expr])
      )))

;; (infer* 'x)

;; (defmacro guard [x & guards])

;; (let [x (dynamic 1 2 3)]
;;   (guard x
;;          Boolean (p/case x True 1 False 2)
;;          Keyword (name x)))

