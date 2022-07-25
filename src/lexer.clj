(ns zanscript.lexer)

(def tokens
  {\= :assign
   \; :semicolon

   \( :l-paren
   \) :r-paren
   \, :comma

   \{ :l-brace
   \} :r-brace

   \+ :plus
   \- :negation
   \* :multiplication
   \/ :division

   \< :less-than
   \> :greater-than

   \! :not})

(def two-character-tokens
  {[\= \=] :equality
   [\! \=] :inequality})

(def keywords
  {"fn" :function
   "let" :let
   "true" :true
   "false" :false
   "if" :if
   "else" :else
   "return" :return})

(comment
  "The Lexer Consumes characters and chunks input into tokens
  - Reads each character
  - If starts with a letter
     - Consume the symbol or keyword
  - If other character, might have to look ahead for operators like
      == != etc.
  - Consumes or eats whitespace

  The token slurping is abstractly like this:
   - We reduce over the input
   - When we get to a new token, we can either determine it's token in place (operators) (requires PEEK)
     - or we need to start slurping with some sub-slurper (symbols, number literals, whitespace, etc.)")

(defn is-alphabetic? [c] (Character/isAlphabetic (int c)))
(defn is-digit? [c] (Character/isDigit (int c)))

(def single-char-token? #{\; \( \) \, \+ \{ \} \< \> \* \- \/})
(def requires-peek? #{\= \!})
(def whitespace? #{\tab \space \newline})

(defn valid-symbol-char? [c]
  (or (is-alphabetic? c)
      (is-digit? c)
      (#{\- \? \_} c)))

(defn operator-with-lookahead [op next-op]
  (if-let [token (get two-character-tokens [op next-op])]
    [token (str op next-op)]
    [(get tokens op :illegal) (str op)]))

(defn slurp-whitespace [input]
  [:whitespace (->> input (take-while whitespace?) (apply str))])

(defn slurp-symbol [input]
  (let [symbol (->> input (take-while valid-symbol-char?) (apply str))
        kw (get keywords symbol)]
    (if kw
      [kw symbol]
      [:ident symbol])))

(defn slurp-number-literal [input]
  (let [number (->> input (take-while is-digit?) (apply str))]
    [:int number (Integer/parseInt number)]))

(defn next-token [input]
  (let [nxt (first input)
        token (cond
                (single-char-token? nxt) [(tokens nxt) (str nxt)]
                (requires-peek? nxt) (operator-with-lookahead nxt (second input))
                (whitespace? nxt) (slurp-whitespace input)
                (is-alphabetic? nxt) (slurp-symbol input)
                (is-digit? nxt) (slurp-number-literal input)
                :else [:illegal (str nxt)])]
    [token (drop (count (second token)) input)]))

(defn lex-input [input]
  (loop [acc []
         rst input]
    (if (seq rst)
      (let [[token remaining] (next-token rst)]
        (recur (conj acc token) remaining))
      acc)))

(def whitespace-token? (fn [[token]] (= token :whitespace)))

(defn lex [input]
  (->> input (into []) lex-input (filter (complement whitespace-token?))))
