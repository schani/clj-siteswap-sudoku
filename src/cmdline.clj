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
     [complex-rules? "Siteswaps have to be different" false]
     [max-tries "Maximum number of tries" nil]
     [sub-tries "Number of sub-tries per try" "50"]
     [verbose? "Print progress messages" false]
     [org-mode? "Output result in org-mode syntax" false]
     remaining]
    (let [rows (java.lang.Integer/parseInt rows)
	  cols (java.lang.Integer/parseInt cols)
	  min-throw (java.lang.Integer/parseInt min-throw)
	  max-throw (java.lang.Integer/parseInt max-throw)
	  unknowns (java.lang.Integer/parseInt unknowns)
	  max-tries (and max-tries (java.lang.Integer/parseInt max-tries))
	  sub-tries (java.lang.Integer/parseInt sub-tries)
	  try-nums (if max-tries (range 1 (inc max-tries)) (iterate inc 1))
	  lock-agent (agent nil)
	  tries (map (fn [try-num]
		       (when verbose?
			 (send lock-agent (fn [_]
					    (println "Trying" try-num))))
		       (make-siteswap-sudoku rows cols
					     min-throw max-throw
					     unknowns complex-rules? sub-tries verbose?))
		     try-nums)
	  sudoku (first (filter #(not (nil? %)) tries))]
      (if sudoku
	(do
	  (when org-mode?
	    (println "*** Puzzle"))
	  (println (sudoku-to-string sudoku org-mode?))
	  (if org-mode?
	    (println "*** Solution")
	    (println))
	  (println (sudoku-to-string (solve-sudoku sudoku min-throw max-throw complex-rules?) org-mode?))
	  (System/exit 0))
	(do
	  (println "No puzzles found")
	  (System/exit 1))))))
