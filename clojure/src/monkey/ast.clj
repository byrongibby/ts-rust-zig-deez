(ns monkey.ast
  (:require [monkey.token]))

(defprotocol PNode
  (token-literal [node]))

(defprotocol PStatement
  (statement-node [node]))

(defprotocol PExpression
  (expression-node [node]))

(defrecord Program [statements]
  PNode
  
  (token-literal [node]
    (if (> (count (:statements node)) 0)
      (token-literal (first (:statements node)))
      "")))

(defrecord LetStatement [token name value]
  PNode

  (token-literal [this]
    (get-in this [:token :literal]))

  PStatement

  (statement-node [_]))

(defrecord Identifier [token value]
  PNode

  (token-literal [node]
    (get-in node [:token :literal]))

  PExpression

  (expression-node [_]))


