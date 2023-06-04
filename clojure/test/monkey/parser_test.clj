(ns monkey.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [monkey.ast :refer [token-literal]]
            [monkey.lexer :refer [lexer]]
            [monkey.parser :refer [parser parse-program]])
  (:import [monkey.ast LetStatement]))


(deftest parser-let-statements-test

  (let [input "let x = 5;
              let y = 10;
              let foobar = 838383;
              "
        program (parse-program (parser (lexer input)))]
    (testing "program returns non-nil value" 
      (is program))
    (testing "program contains 3 statements" 
      (is (= 3 (:statements program))))
    (let [statements (:statements program)
          identifiers ["x" "y" "foobar"]]
      (doseq [n (range (count statements))
              :let [let-statement (nth statements n)
                    expected-identifier (nth identifiers n)]]
        (testing "let statement type"
          (is (instance? LetStatement
                         let-statement)))
        (when (instance? LetStatement let-statement)
          (testing "let statement literal"
            (is (= "let"
                   (token-literal let-statement))))
          (testing "let statement's name's value"
            (is (= expected-identifier
                   (get-in let-statement [:name :value]))))
          (testing "let statement's name's literal"
            (is (= expected-identifier
                   (token-literal (:name let-statement))))))))))
