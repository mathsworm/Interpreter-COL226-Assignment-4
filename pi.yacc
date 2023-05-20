(* pi.yacc *)
open DataTypes
%%
%name Pi


%term T_COMMA 
        | T_SEMI 
        | T_RAT
        | T_INT
        | T_BOOL
        | T_IDENT of string
        | T_BOOLVAL of bool
        | T_AND | T_OR | T_NOT
        | T_LPAREN | T_RPAREN | T_LCURLY | T_RCURLY
        | T_RATADD | T_RATSUB | T_RATMUL | T_RATDIV
        | T_INTADD | T_INTSUB | T_INTMUL | T_INTDIV | T_INTMOD
        | T_ASSIGN | T_CALL | T_READ | T_PRINT
        | T_WHILE | T_DO | T_OD
        | T_IF | T_THEN | T_ELSE | T_FI
        | T_LE | T_GE | T_LT | T_GT | T_EQ | T_NEQ
        | T_INVERSE | T_MAKERAT | T_RATFROMINT 
        | T_RATVAL of Rational.rational
        | T_INTVAL of BigInt.bigint
        | T_ADDINV
        | T_PROCEDURE
        | EOF

%nonterm prog of PROG | block of BLOCK | decl_seq of (DEC list) | var_decl of (DEC list) | proc_decl of (DEC list) | proc_def of (DEC) | ratvar_decl of (DEC list) | boolvar_decl of (DEC list) | intvar_decl of (DEC list) | idents_rat of (DEC list) | idents_bool of (DEC list) | idents_int of (DEC list) | cmd_seq of (CMD list) | cmds of (CMD list) | cmd of CMD | assign_cmd of CMD | exp of EXP | while_cmd of CMD | print_cmd of CMD | call_cmd of CMD | read_cmd of CMD | if_cmd of CMD 


%right T_ASSIGN
%left T_OR
%left T_AND
%left T_EQ T_NEQ 
%left T_LE T_LT T_GE T_GT
%left T_INTADD T_INTSUB
%left T_INTMUL T_INTDIV T_INTMOD
%left T_RATADD T_RATSUB
%left T_RATMUL T_RATDIV 
%right T_MAKERAT T_RATFROMINT T_INVERSE T_ADDINV T_NOT
%left T_LPAREN T_RPAREN



%pos int
%eop EOF
%noshift EOF

%nonassoc

%nodefault
%verbose
%keyword T_BOOL T_INT T_RAT T_IF T_THEN T_ELSE T_FI T_WHILE T_DO T_OD T_READ T_PRINT T_CALL T_INVERSE T_MAKERAT T_RATFROMINT
%arg (fileName) : string


%%

prog : block (PROG (block)) 

block : decl_seq cmd_seq (BLOCK (decl_seq, cmd_seq))

decl_seq : var_decl proc_decl (var_decl @ proc_decl)
        | var_decl (var_decl)
        | proc_decl (proc_decl)
        | ([])

var_decl : ratvar_decl intvar_decl boolvar_decl  (ratvar_decl @ intvar_decl @ boolvar_decl)
        | ratvar_decl intvar_decl (ratvar_decl @ intvar_decl)
        | ratvar_decl boolvar_decl (ratvar_decl @ boolvar_decl)
        | intvar_decl boolvar_decl (intvar_decl @ boolvar_decl)
        | ratvar_decl (ratvar_decl)
        | intvar_decl (intvar_decl)
        | boolvar_decl (boolvar_decl)

ratvar_decl : T_RAT idents_rat T_SEMI (idents_rat)
intvar_decl : T_INT idents_int T_SEMI (idents_int)
boolvar_decl : T_BOOL idents_bool T_SEMI (idents_bool)

idents_rat : T_IDENT T_COMMA idents_rat (RATIONAL(T_IDENT) :: idents_rat) 
        | T_IDENT ([RATIONAL(T_IDENT)])

idents_int : T_IDENT T_COMMA idents_int (INTEGER(T_IDENT) :: idents_int)
        | T_IDENT ([INTEGER(T_IDENT)])

idents_bool : T_IDENT T_COMMA idents_bool (BOOLEAN(T_IDENT) :: idents_bool)
        |  T_IDENT ([BOOLEAN(T_IDENT)])



proc_decl : proc_def T_SEMI proc_decl (proc_def :: proc_decl)
        | proc_def T_SEMI ([proc_def])

proc_def : T_PROCEDURE T_IDENT block (PROCEDURE(T_IDENT,block))




assign_cmd : T_IDENT T_ASSIGN exp (ASSIGNCMD(T_IDENT, exp))
call_cmd : T_CALL T_IDENT (CALLCMD(T_IDENT))
read_cmd : T_READ T_LPAREN T_IDENT T_RPAREN (READCMD(T_IDENT))
print_cmd : T_PRINT T_LPAREN exp T_RPAREN (PRINTCMD(exp))
while_cmd : T_WHILE exp T_DO cmd_seq T_OD (WHILECMD(exp,cmd_seq))
if_cmd : T_IF exp T_THEN cmd_seq T_ELSE cmd_seq T_FI (ITE(exp,cmd_seq1,cmd_seq2))


cmd_seq : T_LCURLY cmds T_RCURLY (cmds)
cmds : cmd T_SEMI cmds (cmd :: cmds)
        | ([])

cmd : assign_cmd (assign_cmd)
        | print_cmd (print_cmd)
        | while_cmd (while_cmd)
        | call_cmd (call_cmd)
        | read_cmd (read_cmd)
        | if_cmd (if_cmd)



exp : T_IDENT (IDENT(T_IDENT)) 
        | T_BOOLVAL (BOOLVAL(T_BOOLVAL))
        | exp T_LE exp (LE(exp1,exp2))
        | exp T_GE exp (GE(exp1,exp2))
        | exp T_LT exp (LT(exp1,exp2))
        | exp T_GT exp (GT(exp1,exp2))
        | exp T_EQ exp (EQ(exp1,exp2))
        | exp T_NEQ exp (NEQ(exp1,exp2))
        | exp T_OR exp (OR(exp1,exp2))
        | exp T_AND exp (AND(exp1,exp2))
        | T_NOT exp (NOT(exp))
        | T_LPAREN exp T_RPAREN (exp) 
        | exp T_INTADD exp (INTADD(exp1,exp2))
        | exp T_INTSUB exp (INTSUB(exp1,exp2))
        | exp T_INTMUL exp (INTMUL(exp1,exp2))
        | exp T_INTDIV exp (INTDIV(exp1,exp2))
        | exp T_INTMOD exp (INTMOD(exp1,exp2))
        | T_ADDINV exp (ADDINV (exp))
        | T_INTVAL (INTVAL(T_INTVAL))
        | exp T_RATADD exp (RATADD(exp1,exp2))
        | exp T_RATSUB exp (RATSUB(exp1,exp2))
        | exp T_RATMUL exp (RATMUL(exp1,exp2))
        | exp T_RATDIV exp (RATDIV(exp1,exp2))
        | T_INVERSE exp (INVERSE(exp))
        | T_MAKERAT T_LPAREN exp T_COMMA exp T_RPAREN (MAKERAT(exp1,exp2))
        | T_RATFROMINT exp (RAT(exp))
        | T_RATVAL (RATVAL(T_RATVAL))
