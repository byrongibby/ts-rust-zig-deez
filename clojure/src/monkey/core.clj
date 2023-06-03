(ns monkey.core
  (:require [monkey.repl :as repl])
  (:gen-class))

(defn main [_]
  (try
    (let [user (System/getenv "USER")]
      (println (format "Hello %s! This is the Monkey programming language!\n" user))
      (println "Feel free to type in commands")
      (println)
      (repl/start))
    (catch Throwable e (throw e))))
