(ns zanscript.parser-test
  (:require [clojure.test :refer [is deftest]]
            [zanscript.lexer :refer [lex]]
            [zanscript.parser :refer [parse-program]]))

(defn lex-then-parse [input] (->> input lex (into []) parse-program))

(defn statement-is [statement expected] (= expected (:token statement)))

(deftest let-tests
  (let [let-example "let x = 5; 
                     let y = 10; 
                     let foobar = 838383;"

        let-program (lex-then-parse let-example)]

    (is (every?
         (fn [s] (-> s (statement-is :let))) let-program)
        "Every parsed statement should be a let statement")

    (is (= ["x" "y" "foobar"]
           (->> let-program (map :name) (into [])))
        "Let identifier names should be correct")))
