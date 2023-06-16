(ns monkey.ast
  (:require [monkey.token]))

(defprotocol PNode
  (string [node])
  (token-literal [node]))

(defprotocol PStatement
  (statement-node [node]))

(defprotocol PExpression
  (expression-node [node]))

(extend-protocol PNode
  nil

  (string [_])
  
  (token-literal [_]))

(defrecord LetStatement [token name value]
  PNode

  (string [node]
    (-> (token-literal node)
        (str " ")
        (str (string (:name node)))
        (str " = ")
        (str (when (:value node) (string (:value node))))
        (str ";")))

  (token-literal [node]
    (get-in node [:token :literal]))

  PStatement

  (statement-node [_]))

(defn let-statement [token]
  (->LetStatement token nil nil))

(defrecord ReturnStatement [token value]
  PNode

  (string [node]
    (-> (token-literal node)
        (str " ")
        (str (when (:value node) (string (:value node))))
        (str ";")))

  (token-literal [node]
    (get-in node [:token :literal]))
  
  PStatement
  
  (statement-node [_]))

(defn return-statement [token]
  (->ReturnStatement token nil))

(defrecord ExpressionStatement [token expression]
  PNode

  (string [node]
    (str (when (:expression node) (string (:expression node)))))

  (token-literal [node]
    (get-in node [:token :literal]))
  
  PStatement
  
  (statement-node [_]))

(defn expression-statement [token]
  (->ExpressionStatement token nil))

(defrecord Identifier [token value]
  PNode

  (string [node]
    (str (:value node)))

  (token-literal [node]
    (get-in node [:token :literal]))

  PExpression

  (expression-node [_]))

(defn identifier [token value]
  (->Identifier token value))

(defrecord Program [statements]
  PNode

  (string [node]
    (reduce str (apply string (:statements node))))
  
  (token-literal [node]
    (if (> (count (:statements node)) 0)
      (token-literal (first (:statements node)))
      "")))

(defn program [statements-map]
  (map->Program statements-map))
