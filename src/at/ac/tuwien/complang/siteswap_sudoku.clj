(ns at.ac.tuwien.complang.siteswap-sudoku
  (:use at.ac.tuwien.complang.jacop
	clojure.contrib.def
	clojure.contrib.str-utils)
  (:require clojure.contrib.seq-utils)
  (:import [JaCoP.core Store]
	   [JaCoP.search DepthFirstSearch InputOrderSelect IndomainMin IndomainRandom]))

(defn make-shape [rows cols]
  (repeat rows (repeat cols true)))

(defn- throw-for-number [t throw-min throw-max]
  (if (and (>= t throw-min)
	   (<= t throw-max))
    t
    nil))

(defn- throw-for-character [c throw-min throw-max]
  (cond (java.lang.Character/isDigit c) (throw-for-number (java.lang.Character/digit c 10) throw-min throw-max)
	(java.lang.Character/isLowerCase c) (throw-for-number (java.lang.Character/getNumericValue c) throw-min throw-max)
	:else nil))

(defn parse-shape [shape-string throw-min throw-max]
  (map (fn [row] (map (fn [c]
			(or (throw-for-character c throw-min throw-max)
			    (not (= c \space))))
		      row))
       (re-split #"\n" shape-string)))

(defn- make-variables [shape throw-min throw-max store]
  (map-indexed (fn [row-index row]
		 (map-indexed (fn [col-index t]
				(if t
				  (let [name (str "t" row-index col-index)]
				    (if (number? t)
				      (make-variable name t store)
				      (make-variable name throw-min throw-max store)))
				  nil))
			      row))
	       shape))

(defn- sudoku-dimensions [matrix]
  [(count matrix)
   (count (first matrix))])

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

(defn- pad-matrix [m]
  (let [num-cols (apply max (map count m))]
    (map (fn [row]
	   (concat row
		   (repeat (- num-cols (count row))
			   nil)))
	 m)))

(defn- transpose [m]
  (apply map list (pad-matrix m)))

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

(defn- num-solutions [vars store max]
  (let [label (DepthFirstSearch.)
	select (InputOrderSelect. store (into-array vars) (IndomainMin.))]
    (if max
      (.setSolutionLimit (.getSolutionListener label) max)
      (.searchAll (.getSolutionListener label) true))
    (.setPrintInfo label false)
    (.labeling label store select)
    (.solutionsNo (.getSolutionListener label))))

(defn- gather-row-siteswaps [matrix]
  (mapcat (fn [row]
	    (loop [row row
		   sss []]
	      (cond (empty? row) sss
		    (nil? (first row)) (recur (rest row) sss)
		    :else (recur (drop-while #(not (nil? %)) row)
				 (conj sss (take-while #(not (nil? %)) row))))))
	  matrix))

(defn- sudoku-constraints [matrix complex-rules store]
  (let [siteswaps (concat (gather-row-siteswaps matrix)
			  (gather-row-siteswaps (transpose matrix)))
	siteswaps (filter #(> (count %) 1) siteswaps)]
    (doseq [ss siteswaps]
      (is-siteswap ss store)
      (siteswap-nontrivial ss store))
    (when complex-rules
      (doseq [[_ sss] (group-by count siteswaps)]
	(do-pairs (fn [ss1 ss2]
		    (siteswaps-different ss1 ss2 store))
		  sss)))))

(defn- sudoku-variables [matrix]
  (filter #(not (nil? %)) (flatten matrix)))

(defvar sudoku-constraints-cache
  (memoize (fn [shape throw-min throw-max complex-rules]
	     (let [store (Store.)
		   matrix (make-variables shape throw-min throw-max store)]
	       (with-caching (sudoku-variables matrix)
		 (fn [] (sudoku-constraints matrix complex-rules store)))))))

(defn- process-sudoku [shape assignments throw-min throw-max complex-rules solver]
  (let [store (Store.)
	matrix (make-variables assignments throw-min throw-max store)
	constraints-cache (sudoku-constraints-cache shape throw-min throw-max complex-rules)
	vars (sudoku-variables matrix)]
    (impose-cache constraints-cache vars store)
    (solver matrix vars store)))

(defn solve-sudoku [shape sudoku throw-min throw-max complex-rules]
  (process-sudoku shape sudoku throw-min throw-max complex-rules
		  (fn [matrix vars store]
		    (if (solve vars store)
		      (apply vector (map (fn [ss]
					   (apply vector (map #(if % (.value %) nil) ss)))
					 matrix))
		      nil))))

(defn- count-sudoku-solutions [shape sudoku throw-min throw-max complex-rules max-solutions]
  (process-sudoku shape sudoku throw-min throw-max complex-rules
		  (fn [matrix vars store]
		    (num-solutions vars store max-solutions))))

(defvar- matrix-positions
  (memoize (fn [shape]
	     (apply concat
		    (map-indexed (fn [row-index row]
				   (apply concat
					  (map-indexed (fn [col-index t]
							 (if t
							   [[row-index col-index]]
							   []))
						       row)))
				 shape)))))

(defn- depopulate-sudoku [shape sudoku throw-min throw-max complex-rules num-nils]
  (let [all-positions (matrix-positions shape)
	nil-positions (set (take num-nils (shuffle all-positions)))
	new-sudoku (map-indexed (fn [row-index row]
				  (map-indexed (fn [col-index throw]
						 (if (contains? nil-positions [row-index col-index])
						   true
						   throw))
					       row))
				sudoku)
	num-solutions (count-sudoku-solutions shape new-sudoku throw-min throw-max complex-rules 2)]
    (assert (> num-solutions 0))
    (if (> num-solutions 1)
      nil
      new-sudoku)))

(defn sudoku-to-string [sudoku org-mode]
  (let [unknown (if org-mode "." "_")
	throw-str (fn [t]
		    (cond (or (false? t) (nil? t)) " "
			  (true? t) unknown
			  (< t 10) (str t)
			  :else (str (char (+ (int \a) (- t 10))))))
	line-builder (if org-mode
		       (fn [throws]
			 (str "|" (apply str (interpose "|" throws)) "|"))
		       (fn [throws]
			 (apply str (interpose " " throws))))]
    (apply str
	   (interpose "\n"
		      (map (fn [row]
			     (line-builder (map throw-str row)))
			   (pad-matrix sudoku))))))

(defn make-siteswap-sudoku [shape throw-min throw-max num-nils complex-rules num-tries verbose]
  (let [sudoku (solve-sudoku shape shape throw-min throw-max complex-rules)]
    (when verbose
      (println (sudoku-to-string sudoku false)))
    (loop [i 0]
      (when verbose
	(println "subtry" i))
      (if (>= i num-tries)
	nil
	(if-let [result (depopulate-sudoku shape sudoku throw-min throw-max complex-rules num-nils)]
	  result
	  (recur (inc i)))))))
