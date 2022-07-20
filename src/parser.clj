(ns zanscript.parser
  :gen-class)

(comment
  "Parsing: The process of going from a stream of tokens into
              a structured representation of data and logic.

  - Recursive descent parser.
  - Top-down operator precedence - Known as \"Pratt Parser\"
      - Closely mirrors the conceptual representation of code
        and computation

  - For some reason I thought parsers would be ... more elegant?
")

(defprotocol Node
  (token-literal [_] "Return the token literal for a node"))

(defprotocol Statement)
(defprotocol Expression)

(defrecord LetStatement [token literal name value]
  Statement
  Node (token-literal [_] literal))

(defrecord Identifier [token value]
  Expression
  Node (token-literal [_] (token :literal)))

;; === Parser Machinery =============================================
(defprotocol IParser
  (parser-tokens [_] "Return the current and next token")
  (parser-advance [_] "Return a new parser with it's position advanced.")
  (parser-track-error [_this error] "Record an error"))

(defrecord Parser [tokens offset errors]
  IParser
  (parser-tokens [_] [(get tokens offset [:EOF])
                      (get tokens (inc offset) [:EOF])])

  (parser-advance [_]
    (Parser. tokens (inc offset) errors))

  (parser-track-error [_ error]
    (let [error {:error error :offset offset}]
      (Parser. tokens offset (conj errors error)))))

(let [parser-atom (atom nil)]
  (defn- check-initialised [action]
    (assert
     (instance? Parser @parser-atom)
     (str "Parser must be initialised before" action)))

  (defn init-parser! [tokens] (swap! parser-atom (fn [_] (Parser. tokens 0 []))))

  (defn current-tokens []
    (check-initialised :get-current-tokens)
    (parser-tokens @parser-atom))

  (defn current-token [] (first (current-tokens)))

  (defn next-token [] (second (current-tokens)))

  (defn advance-parser! []
    (check-initialised :advance)
    (swap! parser-atom (fn [parser] (parser-advance parser))))

  (defn track-error! [error]
    (check-initialised :track-error)
    (swap! parser-atom (fn [parser] (parser-track-error parser error))))

  (defn check-parser-state
    "Return the current state of the parser atom"
    [] @parser-atom)

  (defn current-token-is? [expected-token]
    (let [[[token _literal] _next] (current-tokens)]
      (= token expected-token)))

  (defn next-token-is? [expected-token]
    (let [[_current [next-token _literal]] (current-tokens)]
      (= next-token expected-token)))

  (defn expect-next!
    "Run an assertion against the next token.
    If the next token is as expected, advance the parser one step."
    [expected]
    (let [[_current [next-token]] (current-tokens)]
      (when (= next-token expected) (advance-parser!) true))))


;; === Language Constructs ===
(defn parse-let []
  (let [[let-token let-literal] (current-token)]
    (when (expect-next! :ident)
      (let [[_ident name] (current-token)]

        ;; TODO: Parse the right hand expression here instead of fast forwarding
        (while (not (or (current-token-is? :semicolon)
                        (current-token-is? :EOF)))
          (advance-parser!))

        (LetStatement. let-token let-literal name nil)))))

(defn parse-statement! []
  (let [[[current-token _literal _value]] (current-tokens)]

    (condp = current-token
      :let (parse-let)
      :UNKNOWN-TOKEN)))

(defn end-of-program? [[token _literal]] (= token :EOF))

;; === Root Parser ===
(defn parse-program
  "Parse program in a lazy sequence until we reach EOF or encounter an error"

  ;; Base
  ([tokens] (init-parser! tokens) (parse-program))

  ([]
   (lazy-seq
    (let [[token] (current-tokens)]
      (when (not (end-of-program? token))
        (let [statement (parse-statement!)]

          (advance-parser!)

          (if statement
            (cons statement (parse-program))
            (cons nil (parse-program)))))))))

(init-parser! [])
(advance-parser!)
(track-error! "HELLO")
