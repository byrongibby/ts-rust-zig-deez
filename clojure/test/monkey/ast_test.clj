(ns monkey.ast-test
  (:require [clojure.test :refer [deftest testing is]]
            [monkey.ast :as ast]
            [monkey.token :refer [new-token]]))

(deftest ast-to-string-test
  (testing "program returns expected string"
    (let [program
          (ast/map->Program
            {:statements [(ast/map->LetStatement
                            {:token (new-token :let "let")
                             :name (ast/map->Identifier
                                     {:token (new-token :ident "myVar")
                                      :value "myVar"})
                             :value (ast/map->Identifier
                                      {:token (new-token :ident "anotherVar")
                                       :value "anotherVar"})})]})]
      (is (= (ast/string program) "let myVar = anotherVar;"))))) 
