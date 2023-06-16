(ns monkey.parser
  (:require [monkey.ast :as ast]
            [monkey.lexer :as lex]
            [monkey.token :refer [token]]))

(defrecord Parser [lexer current-token peek-token errors prefix-parse-fns infix-parse-fns])

(defn next-token [parser]
  (let [lexer (lex/next-token (:lexer parser))]
    (-> parser
        (assoc :lexer lexer)
        (assoc :current-token (:peek-token parser))
        (assoc :peek-token (:token lexer)))))

(defn curr-token= [parser type]
  (= (get-in parser [:current-token :type]) type))

(defn peek-token= [parser type]
  (= (get-in parser [:peek-token :type]) type))

(defn append-error [parser expected-type]
  (let [observed-type (get-in parser [:peek-token :type])
        error (format "expected next token to be %s, got %s instead"
                      expected-type
                      observed-type)]
    (update parser :errors conj error)))

(defn parse-let-stmt [parser]
  (let [stmt (ast/let-statement (:current-token parser))]
    (if (peek-token= parser (:ident token))
      (let [parser (next-token parser)
            stmt (->> (get-in parser [:current-token :literal]) 
                      (ast/identifier (:current-token parser))
                      (assoc stmt :name))]
        (if (peek-token= parser (:assign token))
          (let [parser (next-token parser)]
            [(->> (iterate next-token parser)
                  (drop-while #(not (curr-token= % (:semicolon token))))
                  first)
             stmt])
          [(append-error parser (:assign token)) nil]))
      [(append-error parser (:ident token)) nil])))

(defn parse-return-stmt [parser]
  (let [stmt (ast/return-statement (:current-token parser))
        parser (next-token parser)]
    [(->> (iterate next-token parser)
          (drop-while #(not (curr-token= % (:semicolon token))))
          first)
     stmt]))

(def lowest
  {:equals 0
   :less-greater 1
   :sum 2
   :product 3
   :prefix 4
   :call 5})

(defn parse-expression [parser precedence]
  (let [type (get-in parser [:current-token :type])
        prefix (get-in parser [:prefix-parse-fns type])]
    (when prefix
      (prefix parser))))

(defn parse-expression-stmt [parser]
  (let [stmt (ast/expression-statement (:current-token parser))
        expression (parse-expression parser lowest)]
    [(if (peek-token= parser (:semicolon token)) (next-token parser) parser)
     (assoc stmt :expression expression)]))

(defn parse-identifier [parser]
  (let [token (:current-token parser)]
    (ast/identifier token (:literal token))))

(defn parse-stmt [parser]
  (condp = (get-in parser [:current-token :type])
    (:let token) (parse-let-stmt parser)
    (:return token) (parse-return-stmt parser)
    (parse-expression-stmt parser)))

(defn prefix-parse-fn [])

(defn infix-parse-fn [expression])

(defn register-prefix [parser type fn]
  (assoc-in parser [:prefix-parse-fns type] fn))

(defn register-infix [parser type fn]
  (assoc-in parser [:infix-parse-fns type] fn))

(defn parser [lexer]
  (-> (->Parser lexer nil nil [] {} {})
      (register-prefix (:ident token) parse-identifier)
      next-token
      next-token))

(defn parse-program [parser]
  (loop [parser parser
         program (ast/program {:statements []})]
    (if-not (curr-token= parser (:eof token)) 
      (let [[parser stmt] (parse-stmt parser)]
        (recur (next-token parser)
               (update program :statements conj stmt)))
      [parser
       (update program :statements #(remove nil? %))])))
