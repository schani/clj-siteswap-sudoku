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
      (let [sudoku (make-siteswap-sudoku (java.lang.Integer/parseInt rows)
					 (java.lang.Integer/parseInt cols)
					 (java.lang.Integer/parseInt min-throw)
					 (java.lang.Integer/parseInt max-throw)
					 (java.lang.Integer/parseInt unknowns))]
	(if sudoku
	  (println (sudoku-to-string sudoku))
	  (recur))))))
