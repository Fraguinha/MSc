{
module Lexer where
}

%wrapper "basic"

$var=[_ a-z A-Z]
$num=[0-9]

rule :-
  $white+     ;
  $num+       { \s -> TINT (read s) }
  \+          { \s -> TADD }
  \-          { \s -> TSUB }
  \*          { \s -> TMUL }
  \(          { \s -> TLP }
  \)          { \s -> TRP }
  true        { \s -> TTRUE }
  false       { \s -> TFALSE }
  not         { \s -> TNOT }
  and         { \s -> TAND }
  or          { \s -> TOR }
  =           { \s -> TEQ }
  \<=         { \s -> TLE }
  skip        { \s -> TSKIP }
  :=          { \s -> TASSIGN }
  if          { \s -> TIF }
  then        { \s -> TTHEN }
  else        { \s -> TELSE }
  while       { \s -> TWHILE }
  do          { \s -> TDO }
  \{          { \s -> TLCB }
  \}          { \s -> TRCB }
  \;          { \s -> TSEMI }
  $var+       { \s -> TVAR s }

{
data Token =
  TINT Int
  | TVAR String
  | TADD
  | TSUB
  | TMUL
  | TTRUE
  | TFALSE
  | TNOT
  | TAND
  | TOR
  | TEQ
  | TLE
  | TSKIP
  | TASSIGN
  | TIF
  | TTHEN
  | TELSE
  | TWHILE
  | TDO
  | TLCB
  | TRCB
  | TSEMI
  | TLP
  | TRP
  deriving(Eq, Show)
}
