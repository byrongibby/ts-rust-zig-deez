(ns monkey.repl
  (:require [monkey.lexer :refer [lexer next-token]]
            [monkey.token :refer [token]]))

(defn start []
  (while true
    (print ">> ")
    (flush)
    (->> (lexer (read-line))
         (iterate next-token)
         (take-while #(not= (:eof token) (get-in % [:token :type])))
         (rest)
         (map #(println (:token %)))
         doall)))
