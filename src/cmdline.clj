(ns cmdline
  (:gen-class)
  (:use at.ac.tuwien.complang.siteswap-sudoku
	clojure.contrib.command-line))

(defn -main [& args]
  (with-command-line args
    "Siteswap Sudoku generator"
    [[rows "Number of rows" "3"]
     [cols "Number of columns" "4"]
     [min-throw "Minimum throw" "1"]
     [max-throw "Maximum throw" "9"]
     [unknowns "Number of unknowns" "5"]
     remaining]
    (loop []
      (let [min-throw (java.lang.Integer/parseInt min-throw)
	    max-throw (java.lang.Integer/parseInt max-throw)
	    sudoku (make-siteswap-sudoku (java.lang.Integer/parseInt rows)
					 (java.lang.Integer/parseInt cols)
					 min-throw
					 max-throw
					 (java.lang.Integer/parseInt unknowns))]
	(if sudoku
	  (do
	    (println (sudoku-to-string sudoku))
	    (println)
	    (println (sudoku-to-string (solve-sudoku sudoku min-throw max-throw))))
	  (do
	    (println "No luck - retrying")
	    (recur)))))))
