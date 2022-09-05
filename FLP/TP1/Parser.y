{
module Parser where
import Ast
import Lexer
}

%name parse
%tokentype  { Token }
%error      { parseError }

%token
  var     { TVAR $$ }
  int     { TINT $$ }
  add     { TADD }
  sub     { TSUB }
  mul     { TMUL }
  true    { TTRUE }
  false   { TFALSE }
  not     { TNOT }
  and     { TAND }
  or      { TOR }
  eq      { TEQ }
  le      { TLE }
  skip    { TSKIP }
  assign  { TASSIGN }
  if      { TIF }
  then    { TTHEN }
  else    { TELSE }
  while   { TWHILE }
  do      { TDO }
  semi    { TSEMI }
  lcb     { TLCB }
  rcb     { TRCB }
  lp      { TLP }
  rp      { TRP }

%left
  semi
  or
  and
  add
  sub
  mul

%nonassoc
  not
  eq
  lt
  lsb
  lcb
  lp
  rp
  non

%%

stmt:
  lcb stmt rcb                                  { $2 }
  | skip                                        { SSkip }
  | var assign aexp                             { SAssign ($1, $3) }
  | if bexp then stmt else stmt %prec non       { SIf ($2, $4, $6) }
  | while bexp do stmt %prec non                { SWhile ($2, $4) }
  | stmt semi stmt                              { SSeq ($1, $3) }
  | stmt or stmt                                { SNd ($1, $3) }

aexp:
  lp aexp rp      { $2 }
  | var           { AVar $1 }
  | int           { AInt $1 }
  | aexp add aexp { AAdd ($1, $3) }
  | aexp sub aexp { ASub ($1, $3) }
  | aexp mul aexp { AMul ($1, $3) }

bexp:
  lp bexp rp      { $2 }
  | true          { BTrue }
  | false         { BFalse }
  | not bexp      { BNot $2 }
  | bexp and bexp { BAnd ($1, $3) }
  | bexp or bexp  { BOr ($1, $3) }
  | aexp eq aexp  { BEq ($1, $3) }
  | aexp le aexp  { BLe ($1, $3) }

{
parseError :: [Token] -> a
parseError _ = error "Parsing Error"
}
