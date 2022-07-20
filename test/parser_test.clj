(ns zanscript.parser-test
  (:require [clojure.test :refer [is deftest]]
            [zanscript.lexer :refer [lex]]
            [zanscript.parser :refer [parse-program]]))

(defn lex-then-parse [input] (parse-program (into [] (lex input))))

(defn statement-is [statement expected] (= expected (:token statement)))

(deftest let-tests
  (let [let-example "let x = 5; 
                     let y = 10; 
                     let foobar = 838383;"
        {:keys [program]} (lex-then-parse let-example)]

    (is (every?
         (fn [s] (-> s (statement-is :let))) program)
        "Every parsed statement should be a let statement")

    (is (= ["x" "y" "foobar"]
           (->> program (map :name) (into [])))
        "Let identifier names should be correct")))

(deftest let-error-tests
  (let [let-example-with-errors "let x; let 10;let let let;"
        {:keys [failed errors]} (lex-then-parse let-example-with-errors)]

    (is failed "Parser should have failed to parse")

    (is (=
         [{:error "Expected next token to be `:ident` but got `:int` instead.",
           :offset 3}
          {:error "Expected next token to be `:ident` but got `:let` instead.",
           :offset 6}
          {:error "Expected next token to be `:ident` but got `:let` instead.",
           :offset 7}
          {:error
           "Expected next token to be `:ident` but got `:semicolon` instead.",
           :offset 8}]
         errors))))


