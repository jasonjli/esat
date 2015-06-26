-module(cnf).

-type literal() :: integer().
-type clause() :: [literal()].
-type cnf() :: [clause()].
