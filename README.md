*experimental*: this is a site project for learning purposes

# Parseval
This Parser Combinator library is a small excursion I did into parsers. I tried to build a concise and readable DSL that behaves in a functional manner.

```Scala
val plus   = lexeme(char('+'))
val minus  = lexeme(char('-'))
val number = lexeme(integer) withError "failed to read number"

lazy val expr: Parser[Int] = (
      number
    | expr.left(plus).flatMap(a => number.map(_ + a))
    | expr.left(minus).flatMap(a => number.map(_ - a))
)

expr.evalConsumeStream("1 + 2 - 3") == Success(-1)
```

You can find a [JSON](https://www.json.org/) parser [here](https://github.com/pheymann/parseval/tree/master/json/src/main/scala/parseval/json).
