(ns monkey.lexer
  (:require [monkey.token :refer [token new-token]]))

(defn- letter? [ch]
  (or (Character/isLetter ch) (= ch \_)))

(defn- digit? [ch]
  (Character/isDigit ch))

(defn- whitespace? [ch]
  (Character/isWhitespace ch))

(defrecord Lexer [input position read-position ch])

(defn- peek-char [l]
  (if (>= (:read-position l) (count (:input l)))
    (char 0)
    (nth (:input l) (:read-position l))))

(defn- read-char 
  ([l]
   (-> (if (>= (:read-position l) (count (:input l)))
         (assoc l :ch (char 0))
         (assoc l :ch (nth (:input l) (:read-position l))))
       (assoc :position (:read-position l))
       (update :read-position inc)))
  ([l type]
   (-> (read-char l)
       (assoc :token (new-token type (str (:ch l))))))) 

(defn- skip-whitespace [l]
  (if (whitespace? (:ch l))
    (skip-whitespace (read-char l))
    l))

(defn- read-nchar [l n type]
  (let [l (last (take (inc n) (iterate read-char l)))
        literal (subs (:input l) (- (:position l) n) (:position l))]
    (assoc l :token (new-token type literal))))

(defn- read-identifier [l]
  (if (letter? (:ch l))
    (-> (read-char l)
        (update :start #(or % (:position l)))
        read-identifier)
    (let [literal (subs (:input l) (:start l) (:position l))
          type (or ((keyword literal) token) (:ident token))]
      (-> (dissoc l :start)
          (assoc :token (new-token type literal))))))

(defn- read-number [l]
  (if (digit? (:ch l))
    (-> (read-char l)
        (update :start #(or % (:position l)))
        read-number)
    (let [literal (subs (:input l) (:start l) (:position l))]
      (-> (dissoc l :start)
          (assoc :token (new-token (:int token) literal))))))

(defn next-token [l]
  (let [{ch :ch :as l} (skip-whitespace l)]
    (case ch
      \= (if (= (peek-char l) \=)
           (read-nchar l 2 (:eq token))
           (read-char l (:assign token)))
      \+ (read-char l (:plus token))
      \- (read-char l (:minus token))
      \! (if (= (peek-char l) \=)
           (read-nchar l 2 (:not-eq token))
           (read-char l (:bang token)))
      \/ (read-char l (:slash token))
      \* (read-char l (:asterisk token))
      \< (read-char l (:lt token))
      \> (read-char l (:gt token))
      \; (read-char l (:semicolon token))
      \, (read-char l (:comma token))
      \( (read-char l (:lparen token))
      \) (read-char l (:rparen token))
      \{ (read-char l (:lbrace token))
      \} (read-char l (:rbrace token))
      (cond
        (letter? ch) (read-identifier l)
        (digit? ch) (read-number l)
        (= ch (char 0)) (read-char l (:eof token))
        :else (read-char l (:illegal token))))))

(defn lexer [input]
  {:pre [string?]}
  (read-char (->Lexer input -1 0 nil)))
