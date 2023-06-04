(ns monkey.parser
  (:require [monkey.ast]
            [monkey.lexer :as lex]
            [monkey.token]))

(defrecord Parser [lexer current-token peek-token])

(defn next-token [parser]
  (-> parser
      (assoc :current-token (:peek-token parser))
      (assoc :peek-token (lex/next-token (:lexer parser)))))

(defn parser [lexer]
  (-> (->Parser lexer nil nil)
      next-token
      next-token))

(defn parse-program [parser])
