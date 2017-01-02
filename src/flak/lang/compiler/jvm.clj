(ns flak.lang.compiler.jvm
  (:require
   [clojure.spec :as s])
  (:import
   [clojure.lang Namespace]))

'{:type list
  :form [{:type symbol
          :form fn}
         {:type vector
          :form [{:type vector
                  :form [{:type symbol :form a}
                         {:type symbol :form b}
                         {:type keyword :form :as}
                         {:type symbol :form x}]}]}
         {:type list
          :form [{:type symbol :form +}
                 {:type integer :form 1}
                 {:type integer :form 2}]}]}

;; expr
'{:op :invoke
  :form (+ 1 2)
  :fn-expr }
'{:type def
  :name name
  :expr {:type expr
         :call f
         :args [{:type symbol
                 :value x}]}}

(def ^:private aliases (atom {}))
(def ^:private constants (atom []))
(def ^:private constant-ids (atom {}))
(def ^:private vars (atom {}))

(defn alias [alias-sym ns-sym]
  (swap! aliases assoc-in [ns-sym alias-sym]))

(defn lookup-alias [ns-sym sym]
  (get-in @aliases [ns-sym sym]))

(def ^:dynamic *current-ns* (Namespace/findOrCreate 'flak))
(defn current-ns [] *current-ns*)

(defn namespace-for
  ([sym]
   (namespace-for (current-ns) sym))
  ([in-ns sym]
   (let [ns-sym (some-> 'sym namespace sym)]
     (or (lookup-alias in-ns ns-sym)
         (Namespace/find ns-sym)))))

(defn register-constant! [o]
  (let [v @constants
        c (count v)
        ids @constant-ids
        i (get ids o)]
    (or i
        (do
          (swap! constants conj o)
          (swap! constant-ids assoc o c)
          c))))

(register-constant! 'sym2)

(defn register-var! [v]
  (let [id (get @vars v)]
    (when-not id
      (swap! vars assoc v )
      ))
	if(!VARS.isBound())
		return;
	IPersistentMap varsMap = (IPersistentMap) VARS.deref();
	Object id = RT.get(varsMap, var);
	if(id == null)
		{
		VARS.set(RT.assoc(varsMap, var, registerConstant(var)));
		}
  )

(defn lookup-var
  ([sym intern-new? register-macro?]
  (let [ns (namespace sym)
         name (name sym)]
     ) )
  ([sym intern-new?]
   (lookup-var sym intern-new? true)
   ))
(defn parse-def [form]
  (let []))
(def specials)

static final Symbol DEF = Symbol.intern("def");
static final Symbol LOOP = Symbol.intern("loop*");
static final Symbol RECUR = Symbol.intern("recur");
static final Symbol IF = Symbol.intern("if");
static final Symbol LET = Symbol.intern("let*");
static final Symbol LETFN = Symbol.intern("letfn*");
static final Symbol DO = Symbol.intern("do");
static final Symbol FN = Symbol.intern("fn*");
static final Symbol FNONCE = (Symbol) Symbol.intern("fn*").withMeta(RT.map(Keyword.intern(null, "once"), RT.T));
static final Symbol QUOTE = Symbol.intern("quote");
static final Symbol THE_VAR = Symbol.intern("var");
static final Symbol DOT = Symbol.intern(".");
static final Symbol ASSIGN = Symbol.intern("set!");
//static final Symbol TRY_FINALLY = Symbol.intern("try-finally");
static final Symbol TRY = Symbol.intern("try");
static final Symbol CATCH = Symbol.intern("catch");
static final Symbol FINALLY = Symbol.intern("finally");
static final Symbol THROW = Symbol.intern("throw");
static final Symbol MONITOR_ENTER = Symbol.intern("monitor-enter");
static final Symbol MONITOR_EXIT = Symbol.intern("monitor-exit");
static final Symbol IMPORT = Symbol.intern("clojure.core", "import*");
//static final Symbol INSTANCE = Symbol.intern("instance?");
static final Symbol DEFTYPE = Symbol.intern("deftype*");
static final Symbol CASE = Symbol.intern("case*");

//static final Symbol THISFN = Symbol.intern("thisfn");
static final Symbol CLASS = Symbol.intern("Class");
static final Symbol NEW = Symbol.intern("new");
static final Symbol THIS = Symbol.intern("this");
static final Symbol REIFY = Symbol.intern("reify*");
//static final Symbol UNQUOTE = Symbol.intern("unquote");
//static final Symbol UNQUOTE_SPLICING = Symbol.intern("unquote-splicing");
//static final Symbol SYNTAX_QUOTE = Symbol.intern("clojure.core", "syntax-quote");
static final Symbol LIST = Symbol.intern("clojure.core", "list");
static final Symbol HASHMAP = Symbol.intern("clojure.core", "hash-map");
static final Symbol VECTOR = Symbol.intern("clojure.core", "vector");
static final Symbol IDENTITY = Symbol.intern("clojure.core", "identity");

static final Symbol _AMP_ = Symbol.intern("&");
static final Symbol ISEQ = Symbol.intern("clojure.lang.ISeq");

static final Keyword loadNs = Keyword.intern(null, "load-ns");
static final Keyword inlineKey = Keyword.intern(null, "inline");
static final Keyword inlineAritiesKey = Keyword.intern(null, "inline-arities");
static final Keyword staticKey = Keyword.intern(null, "static");
static final Keyword arglistsKey = Keyword.intern(null, "arglists");
static final Symbol INVOKE_STATIC = Symbol.intern("invokeStatic");

static final Keyword volatileKey = Keyword.intern(null, "volatile");
static final Keyword implementsKey = Keyword.intern(null, "implements");
static final String COMPILE_STUB_PREFIX = "compile__stub";

static final Keyword protocolKey = Keyword.intern(null, "protocol");
static final Keyword onKey = Keyword.intern(null, "on");
static Keyword dynamicKey = Keyword.intern("dynamic");
static final Keyword redefKey = Keyword.intern(null, "redef");

static final Symbol NS = Symbol.intern("ns");
static final Symbol IN_NS = Symbol.intern("in-ns");
