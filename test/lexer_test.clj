(ns zanscript.lexer-test
  (:require [clojure.test :refer [deftest is]]
            [zanscript.lexer :refer [lex]]))

(deftest lex-tests
  (let [input "=+(){},;"
        expected '([:assign "="]
                   [:plus "+"]
                   [:l-paren "("]
                   [:r-paren ")"]
                   [:l-brace "{"]
                   [:r-brace "}"]
                   [:comma ","]
                   [:semicolon ";"])]
    (is (= expected (lex input))))

  (let [input "
let five = 5;
let ten = 10;
let add = fn(x, y) {
     let inner = x + y;
     let inner2 = y / x;
     x * x;          
};

let result = add(five, ten);
"
        expected [[:let "let"] [:ident "five"] [:assign "="] [:int "5" 5] [:semicolon ";"]
                  [:let "let"] [:ident "ten"] [:assign "="] [:int "10" 10] [:semicolon ";"]
                  [:let "let"] [:ident "add"] [:assign "="] [:function "fn"] [:l-paren "("] [:ident "x"] [:comma ","] [:ident "y"] [:r-paren ")"] [:l-brace "{"]
                  [:let "let"] [:ident "inner"] [:assign "="] [:ident "x"] [:plus "+"] [:ident "y"] [:semicolon ";"]
                  [:let "let"] [:ident "inner2"] [:assign "="] [:ident "y"] [:division "/"] [:ident "x"] [:semicolon ";"]
                  [:ident "x"] [:multiplication "*"] [:ident "x"] [:semicolon ";"]
                  [:r-brace "}"] [:semicolon ";"]
                  [:let "let"] [:ident "result"] [:assign "="] [:ident "add"] [:l-paren "("] [:ident "five"] [:comma ","] [:ident "ten"] [:r-paren ")"] [:semicolon ";"]]]
    (is (= expected (lex input))))

  (let [input "if (x < y) { x + 10; } else { x + 100; }"
        expected [[:if "if"]
                  [:l-paren "("]
                  [:ident "x"]
                  [:less-than "<"]
                  [:ident "y"]
                  [:r-paren ")"]
                  [:l-brace "{"]
                  [:ident "x"]
                  [:plus "+"]
                  [:int "10" 10]
                  [:semicolon ";"]
                  [:r-brace "}"]
                  [:else "else"]
                  [:l-brace "{"]
                  [:ident "x"]
                  [:plus "+"]
                  [:int "100" 100]
                  [:semicolon ";"]
                  [:r-brace "}"]]]
    (is (= expected (lex input))))

  (let [input "
let embiggen = fn(x, y) {
  if (x < y) {
    return !x + 10;
  } else {
    x + 100;
  }
}"
        expected [[:let "let"]
                  [:ident "embiggen"]
                  [:assign "="]
                  [:function "fn"]
                  [:l-paren "("]
                  [:ident "x"]
                  [:comma ","]
                  [:ident "y"]
                  [:r-paren ")"]
                  [:l-brace "{"]
                  [:if "if"]
                  [:l-paren "("]
                  [:ident "x"]
                  [:less-than "<"]
                  [:ident "y"]
                  [:r-paren ")"]
                  [:l-brace "{"]
                  [:return "return"]
                  [:not "!"]
                  [:ident "x"]
                  [:plus "+"]
                  [:int "10" 10]
                  [:semicolon ";"]
                  [:r-brace "}"]
                  [:else "else"]
                  [:l-brace "{"]
                  [:ident "x"]
                  [:plus "+"]
                  [:int "100" 100]
                  [:semicolon ";"]
                  [:r-brace "}"]
                  [:r-brace "}"]]]
    (is (= expected (lex input)))))
