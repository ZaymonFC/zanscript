(ns zanscript.repl
  (:require [zanscript.lexer :refer [lex]]))

(defn repl []
  (println "|> Welcome to the Zanscript REPL")
  (loop []
    (let [input (read-line)]
      (when (not= input "-quit")
        (println (lex input))
        (recur)))))
