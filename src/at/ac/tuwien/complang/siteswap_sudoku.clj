(ns at.ac.tuwien.complang.siteswap-sudoku
  (:use at.ac.tuwien.complang.jacop)
  (:require clojure.contrib.seq-utils)
  (:import [JaCoP.core Store Variable]
	   [JaCoP.search DepthFirstSearch InputOrderSelect IndomainMin IndomainRandom]
	   [JaCoP.constraints Alldifferent]))

(defn- make-variables [assignments throw-min throw-max store]
  (let [rows (count assignments)
	cols (count (first assignments))]
    (map (fn [row]
	   (map (fn [col]
		  (let [name (str "t" row col)
			assignment (nth (nth assignments row) col)]
		    (if (nil? assignment)
		      (Variable. store name throw-min throw-max)
		      (Variable. store name assignment assignment))))
		(range cols)))
	 (range rows))))

(defn- is-siteswap [ss store]
  (let [period (count ss)
	period-v (Variable. store period period)
	ls (map (fn [t i]
		  (let [l (Variable. store 0 (dec period))
			iv (Variable. store i i)]
		    (predicate-constraint (= l (mod (+ t iv) period-v)) store)
		    l))
		ss
		(range period))]
    (.impose store (Alldifferent. (into-array ls)))))

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
      (.imposeDecomposition store (make-predicate-constraint expr store)))))

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
						 ss)))
	  pred (make-predicate-constraint expr store)]
      (.imposeDecomposition store pred))))

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
    (.labeling label store select)))

(defn- num-solutions [vars store]
  (let [label (DepthFirstSearch.)
	select (InputOrderSelect. store (into-array vars) (IndomainMin.))]
    (.searchAll (.getSolutionListener label) true)
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

(defn- process-sudoku [assignments throw-min throw-max solver]
  (let [store (Store.)
	matrix (make-variables assignments throw-min throw-max store)
	vars (flatten matrix)]
    (sudoku-constraints matrix store)
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

(defn- depopulate-sudoku [sudoku throw-min throw-max num-nils]
  (let [num-solutions (count-sudoku-solutions sudoku throw-min throw-max)]
    (assert (> num-solutions 0))
    (if (> num-solutions 1)
      nil
      (if (>= (count (filter nil? (flatten sudoku))) num-nils)
	sudoku
	(let [rows (count sudoku)
	      cols (count (first sudoku))
	      coords (for [i (range rows) j (range cols)
			   :when (not (nil? (nth (nth sudoku i) j)))]
		       [i j])]
	  (let [[row col] (rand-nth coords)
		new-sudoku (assoc sudoku row (assoc (nth sudoku row) col nil))]
	  (depopulate-sudoku new-sudoku throw-min throw-max num-nils)))))))

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

(defn make-siteswap-sudoku [rows cols throw-min throw-max num-nils]
  (let [sudoku (map (fn [_] (map (fn [_] nil) (range cols))) (range rows))
	sudoku (solve-sudoku sudoku throw-min throw-max)]
    (depopulate-sudoku sudoku throw-min throw-max num-nils)))
