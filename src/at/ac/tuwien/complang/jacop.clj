(ns at.ac.tuwien.complang.jacop
  (:use clojure.set)
  (:import [JaCoP.core Store Variable]
	   [JaCoP.constraints Predicate]))

(defn- expr-vars [expr]
  (cond (seq? expr) (apply union (map expr-vars (rest expr)))
	(symbol? expr) #{expr}
	(= (type expr) Variable) #{(.id expr)}
	:else (throw (Exception. (str "not a legal constraint expression: " expr)))))

(defn- compile-expr [var-map expr]
  (let [ops {'abs [1 "abs"]
	     '+ [2 "add"]
	     '- [2 "sub"]
	     '* [2 "mul"]
	     '/ [2 "div"]
	     'mod [2 "mod"]
	     'pow [2 "pow"]
	     '= [2 "eq"]
	     '<> [2 "ne"]
	     '>= [2 "ge"]
	     '> [2 "gt"]
	     '<= [2 "le"]
	     '< [2 "lt"]
	     'not [1 "not"]
	     'and [2 "and"]
	     'or [2 "or"]
	     'xor [2 "xor"]}]
    (cond (seq? expr) (let [[arity name] (ops (first expr))
			    args (rest expr)]
			(when (not (= arity (count args)))
			  (throw (Exception. (str "wrong number of arguments in " (seq expr)))))
			(str name "(" (apply str (interpose "," (map (partial compile-expr var-map) args))) ")"))
	  (symbol? expr) (var-map expr)
	  (= (type expr) Variable) (var-map (.id expr))
	  :else (throw (Exception. (str "cannot compile " expr))))))

(defn- compile-predicate-constraint [expr]
  (let [vars (seq (expr-vars expr))
	var-map (into {} (map (fn [var i] [var (str "x" i)])
			      vars
			      (range (count vars))))
	decl (apply str (interpose " " (map #(str "int " (var-map %)) vars)))
	expr-str (compile-expr var-map expr)]
    [vars decl expr-str]))

(defn- var-list-string [var-names]
  (apply str (interpose " " var-names)))

(defn make-predicate-constraint [expr store]
  (let [[vars decl expr-str] (compile-predicate-constraint expr)
	var-list (var-list-string vars)]
    (Predicate. var-list decl expr-str store)))

(defmacro predicate-constraint [expr store]
  (let [[vars decl expr-str] (compile-predicate-constraint expr)]
    `(.imposeDecomposition ~store (Predicate. (var-list-string (map (fn [v#] (.id v#)) (list ~@vars)))
					      ~decl
					      ~expr-str
					      ~store))))
