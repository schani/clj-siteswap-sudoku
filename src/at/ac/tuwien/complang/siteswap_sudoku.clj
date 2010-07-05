(ns at.ac.tuwien.complang.siteswap-sudoku
  (:use at.ac.tuwien.complang.jacop
	clojure.contrib.def)
  (:require clojure.contrib.seq-utils)
  (:import [JaCoP.core Store]
	   [JaCoP.search DepthFirstSearch InputOrderSelect IndomainMin IndomainRandom]))

(defn- sudoku-dimensions [matrix]
  [(count matrix)
   (count (first matrix))])

(defn- make-variables [assignments throw-min throw-max store]
  (let [[rows cols] (sudoku-dimensions assignments)]
    (map (fn [row]
	   (map (fn [col]
		  (let [name (str "t" row col)
			assignment (nth (nth assignments row) col)]
		    (if (nil? assignment)
		      (make-variable name throw-min throw-max store)
		      (make-variable name assignment store))))
		(range cols)))
	 (range rows))))

(defn- is-siteswap [ss store]
  (let [period (count ss)
	period-v (make-variable period store)
	ls (map (fn [t i]
		  (let [l (make-variable 0 (dec period) store)
			iv (make-variable i store)]
		    (predicate (= l (mod (+ t iv) period-v)) store)
		    l))
		ss
		(range period))]
    (all-different ls store)))

(defn- exprs-disjunction [exprs]
  (reduce (fn [a & b]
	    (if b
	      (list 'or a (first b))
	      a))
	  exprs))

(defn- siteswaps-different [ss1 ss2 store]
  (let [period (count ss1)]
    (assert (= period (count ss2)))
    (doseq [expr (map (fn [rot]
			(exprs-disjunction (map (fn [t1 t2]
						  (list '<> t1 t2))
						ss1 rot)))
		      (clojure.contrib.seq-utils/rotations ss2))]
      (computed-predicate expr store))))

(defn- mapseq [f s]
  (if (empty? s)
    ()
    (cons (f s) (mapseq f (rest s)))))

(defn- siteswap-nontrivial [ss store]
  (when (> (count ss) 1)
    (let [expr (exprs-disjunction (apply concat
					 (mapseq (fn [ss]
						   (let [t1 (first ss)]
						     (map (fn [t2]
							    (list '<> t1 t2))
							  (rest ss))))
						 ss)))]
      (computed-predicate expr store))))

(defn- transpose [m]
  (apply map list m))

(defn- do-pairs [f s]
  (doseq [s (mapseq identity s)]
    (let [x (first s)]
      (doseq [y (rest s)]
	(f x y)))))

(defn- solve [vars store]
  (let [label (DepthFirstSearch.)
	select (InputOrderSelect. store (into-array vars) (IndomainRandom.))]
    (.setPrintInfo label false)
    (.labeling label store select)))

(defn- num-solutions [vars store]
  (let [label (DepthFirstSearch.)
	select (InputOrderSelect. store (into-array vars) (IndomainMin.))]
    (.searchAll (.getSolutionListener label) true)
    (.setPrintInfo label false)
    (.labeling label store select)
    (.solutionsNo (.getSolutionListener label))))

(defn- sudoku-constraints [matrix store]
  (let [matrix-t (transpose matrix)]
    (doseq [ss matrix]
      (is-siteswap ss store)
      (siteswap-nontrivial ss store))
    (doseq [ss matrix-t]
      (is-siteswap ss store)
      (siteswap-nontrivial ss store))
    (do-pairs (fn [ss1 ss2]
		(siteswaps-different ss1 ss2 store))
	      matrix)
    (do-pairs (fn [ss1 ss2]
		(siteswaps-different ss1 ss2 store))
	      matrix-t)))

(defvar sudoku-constraints-cache
  (memoize (fn [rows cols throw-min throw-max]
	     (let [store (Store.)
		   matrix (make-variables (repeat rows (repeat cols nil))
					  throw-min throw-max store)]
	       (with-caching (flatten matrix)
		 (fn [] (sudoku-constraints matrix store)))))))

(defn- process-sudoku [assignments throw-min throw-max solver]
  (let [store (Store.)
	matrix (make-variables assignments throw-min throw-max store)
	constraints-cache (sudoku-constraints-cache (count matrix)
						    (count (first matrix))
						    throw-min throw-max)
	vars (flatten matrix)]
    (impose-cache constraints-cache vars store)
    (solver matrix vars store)))

(defn solve-sudoku [sudoku throw-min throw-max]
  (process-sudoku sudoku throw-min throw-max
		  (fn [matrix vars store]
		    (if (solve vars store)
		      (apply vector (map (fn [ss]
					   (apply vector (map #(.value %) ss)))
					 matrix))
		      nil))))

(defn- count-sudoku-solutions [sudoku throw-min throw-max]
  (process-sudoku sudoku throw-min throw-max
		  (fn [matrix vars store]
		    (num-solutions vars store))))

(defvar- matrix-positions
  (memoize (fn [rows cols]
	     (for [row (range rows)
		   col (range cols)]
	       [row col]))))

(defn- depopulate-sudoku [sudoku throw-min throw-max num-nils]
  (let [[rows cols] (sudoku-dimensions sudoku)
	all-positions (matrix-positions rows cols)
	nil-positions (set (take num-nils (shuffle all-positions)))
	new-sudoku (map-indexed (fn [row-index row]
				  (map-indexed (fn [col-index throw]
						 (if (contains? nil-positions [row-index col-index])
						   nil
						   throw))
					       row))
				sudoku)
	num-solutions (count-sudoku-solutions new-sudoku throw-min throw-max)]
    (assert (> num-solutions 0))
    (if (> num-solutions 1)
      nil
      new-sudoku)))

(defn sudoku-to-string [sudoku]
  (let [throw-str (fn [t]
		    (cond (nil? t) "_"
			  (< t 10) (str t)
			  :else (str (char (+ (int \a) (- t 10))))))]
    (apply str
	   (interpose "\n"
		      (map (fn [row]
			     (apply str
				    (interpose " "
					       (map throw-str
						    row))))
			 sudoku)))))

(defn make-siteswap-sudoku [rows cols throw-min throw-max num-nils num-tries]
  (let [sudoku (map (fn [_] (map (fn [_] nil) (range cols))) (range rows))
	sudoku (solve-sudoku sudoku throw-min throw-max)]
    (loop [i 0]
      (if (>= i num-tries)
	nil
	(if-let [result (depopulate-sudoku sudoku throw-min throw-max num-nils)]
	  result
	  (recur (inc i)))))))
