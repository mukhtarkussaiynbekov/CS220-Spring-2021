KAIST CS220 - Course Project 2021 Spring
===

This repository contains a simple Lambda expression parser as well as type
definitions for Lambda expressions and De Bruijn expressions. Your goal is to
successfully implement lines annotated with `failwith "IMPLEMENT"` to pass all
the unit tests. That is, `dotnet test` should return a success for all cases.

The parser implementation is fully given, so you don't need to fix it. You can
use the parser to construct complex lambda expressions in DU (discriminated
union). Note that you don't need to fullly understand the parsing routine, as it
is not the main part of this project. You can just use it as a black box.

### Test Your Implementation

You can test your implementation by typing `dotnet test` at the root of the
project (where the Lambda.sln is located at). You will observe that all the
tests will fail at first, but as you implement your code, you should be able to
pass them all.

You can also run the basic terminal application by typing `dotnet run` at the
`Lambda` directory:
```
cd Lambda
dotnet run
```

The above command will launch a command prompt (like REPL) where you can type in
a lambda expression and the program will then output the following three lines:
(1) pretty-printed line of lambda expression (`Expr`) constructed from the given
input string; (2) De Bruijn expression
(https://en.wikipedia.org/wiki/De_Bruijn_index) obtained by converting (1); and
(3) the normal form of the given lambda expression in `DebruijnExpr`. For example,
```
> (\x. x)
(λx.x)
λ 1
λ 1
```

The parser of a lambda expression is given. It always expects to have
fully-parenthesized expressions as input. Also, we use the backslash `\` to
represent the lambda symbol, and the dot `.` to distinguish the arguments and
body of a lambda term.

Note that you don't really need to use the program to test your
implementation. You can always override the `main` function of the program to
debug your implementation.

### How to Start?

You should start by understanding De Bruijn notation first. Read the Wikipedia
link and understand the concept. We have two separate types for regular lambda
expressions (`Expr`) and De Bruijn expressions (`DeBruijnExpr`). Note that `Var`
in `Expr` corresponds to `Ref` in `DeBruijnExpr`, `Lambda` corresponds to `Abs`,
and `Apply` corresponds to `App`.

You then implement the conversion logic in `Translate.fs`. This will allow you
to translate `Expr` to `DeBruijnExpr` and vice versa.

Once you have the translation logic ready, you can start implement the
normalization routine in `Evaluate.fs`. We provide useful references as follows:
- https://en.wikipedia.org/wiki/Beta_normal_form
- Constructive Computation Theory (http://gallium.inria.fr/~huet/PUBLIC/CCT.pdf)

After that, you can define Church numerals and their arithmetics with the
lambda expressions in `Arith.fs`.

### Extra Points

The factorial function implementation at the end of `Arith.fs` may take
significant time if the input is bigger than five (or six). This is obvious
because lambda expressions include lots of nested expressions. So we will test
your code only upto `factorial(5)`. However, if you can manage to make it
efficient, and if your code runs well with `factorial(7)` or above within a few
seconds (on a single core of a normal desktop machine). Then we will give some
extra points.

### Grading

If your code can pass all the tests, you get 90%. If your code can pass extra
test cases (currently hidden from students), then you get 95%. The rest 5% is
based on the readability of your code. Particularly, we expect that your code is
properly commented.

### FAQs

##### Do I need to consider negative integers?

No. Just encode positive integers and zero using the Church encoding you learned
in the class.

##### Do I need to modify the parser?

No. The current parser is to showcase that a simple REPL-like program can be
written with monadic parser combinators. However, we don't expect you to improve
the parser. Of course, you can modify it for your own practice.

##### Should I create my own repository by myself to proceed?

No. TAs will let you know how to proceed.
