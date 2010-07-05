(ns at.ac.tuwien.complang.jacop
  (:use clojure.set
	clojure.contrib.def)
  (:require clojure.contrib.seq-utils)
  (:import [JaCoP.core Store Variable]
	   [JaCoP.constraints Alldifferent Predicate]))

(defvar- *caching* false)
(defvar- *vars* nil)
(defvar- *constraints* nil)

(defn- make-or-cache-var [name min max store]
  (let [name (or name (str (gensym "__var__")))
	maker (fn [store] (Variable. store name min max))]
    (if *caching*
      (do
	(set! *vars* (conj *vars* maker))
	#^{:type ::cached-var} {:name name :index (dec (count *vars*))})
      (maker store))))

(defn- variable? [v]
  (let [t (type v)]
    (or (= t Variable)
	(= t ::cached-var))))

(defmulti variable-name type)

(defmethod variable-name ::cached-var [v]
	   (:name v))

(defmethod variable-name Variable [v]
	   (.id v))

(defmethod variable-name clojure.lang.Symbol [v]
	   v)

(defn make-variable
  ([constant store] (make-or-cache-var nil constant constant store))
  ([name-or-min const-or-max store] (if (string? name-or-min)
				      (make-or-cache-var name-or-min const-or-max const-or-max store)
				      (make-or-cache-var nil name-or-min const-or-max store)))
  ([name min max store] (make-or-cache-var name min max store)))

(defn- resolve-var [var]
  (if *caching*
    (if (= (type var) ::cached-var)
      var
      (let [poss (clojure.contrib.seq-utils/positions #(= var %) *vars*)]
	(assert (= (count poss) 1))
	#^{:type ::cached-var} {:name (.id var) :index (first poss)}))
    var))

(defn make-constraint [maker vars]
  (if *caching*
    {:vars (doall (map resolve-var vars)) :maker maker}
    (maker vars)))

(defn- add-constraint [imposer constraint store]
  (if *caching*
    (set! *constraints* (conj *constraints* {:imposer imposer :constraint constraint}))
    (imposer constraint store)))

(defn- impose [constraint store]
  (add-constraint (fn [c s] (.impose s c)) constraint store))

(defn impose-decomposition [constraint store]
  (add-constraint (fn [c s] (.imposeDecomposition s c)) constraint store))

(defn with-caching [init-vars imposer]
  (binding [*caching* true
	    *vars* (apply vector init-vars)
	    *constraints* []]
    (imposer)
    {:num-init-vars (count init-vars)
     :new-vars (doall (drop (count init-vars) *vars*))
     :constraints *constraints*}))

(defn impose-cache [cache init-vars store]
  (let [all-vars (concat init-vars (map (fn [maker]
					  (maker store))
					(:new-vars cache)))]
    (doseq [{imposer :imposer {vars :vars maker :maker} :constraint} (:constraints cache)]
      (let [vars (map #(nth all-vars (:index %)) vars)]
	(imposer (maker vars) store)))))

(defn all-different [vars store]
  (impose (make-constraint (fn [vars] (Alldifferent. (into-array vars))) vars)
	  store))

(defn- expr-vars [expr]
  (cond (seq? expr) (apply union (map expr-vars (rest expr)))
	(symbol? expr) #{expr}
	(variable? expr) #{expr}
	:else (throw (Exception. (str "not a legal constraint expression: " expr)))))

(defn- compile-expr [var-map expr]
  (let [ops {"abs" [1 "abs"]
	     "+" [2 "add"]
	     "-" [2 "sub"]
	     "*" [2 "mul"]
	     "/" [2 "div"]
	     "mod" [2 "mod"]
	     "pow" [2 "pow"]
	     "=" [2 "eq"]
	     "<>" [2 "ne"]
	     ">=" [2 "ge"]
	     ">" [2 "gt"]
	     "<=" [2 "le"]
	     "<" [2 "lt"]
	     "not" [1 "not"]
	     "and" [2 "and"]
	     "or" [2 "or"]
	     "xor" [2 "xor"]}]
    (cond (seq? expr) (let [[arity name] (ops (name (first expr)))
			    args (rest expr)]
			(when (not (= arity (count args)))
			  (throw (Exception. (str "wrong number of arguments in " (seq expr)))))
			(str name "(" (apply str (interpose "," (map (partial compile-expr var-map) args))) ")"))
	  (symbol? expr) (var-map expr)
	  (variable? expr) (var-map expr)
	  :else (throw (Exception. (str "cannot compile " expr))))))

(defn- compile-predicate-constraint [expr]
  (let [vars (seq (expr-vars expr))
	var-map (into {} (map (fn [var i] [var (str "x" i)])
			      vars
			      (range (count vars))))
	decl (apply str (interpose " " (map #(str "int " (var-map %)) vars)))
	expr-str (compile-expr var-map expr)]
    [vars decl expr-str]))

(defn var-list-string [variable-names]
  (apply str (interpose " " variable-names)))

(defn computed-predicate [expr store]
  (let [[vars decl expr-str] (compile-predicate-constraint expr)]
    (impose-decomposition (make-constraint (fn [vars]
					     (Predicate. (var-list-string (map variable-name vars))
							 decl expr-str store))
					   vars)
			  store)))

(defmacro predicate [expr store]
  (let [[vars decl expr-str] (compile-predicate-constraint expr)]
    `(impose-decomposition (make-constraint (fn [vars#] (Predicate. (var-list-string (map variable-name vars#))
								   ~decl ~expr-str ~store))
					    [~@vars])
			   ~store)))
