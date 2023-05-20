(* pi.lex *)
structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn
    (fileName,bad,line,col) =>
    TextIO.output(TextIO.stdOut,fileName^"["
        ^Int.toString line^"."^Int.toString col
        ^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);


%%
%full
%header (functor PiLexFun(structure Tokens: Pi_TOKENS));
%arg (fileName:string);
%s PI COMMENT;
%states INITIAL COMMENT
alpha = [A-Za-z]
digit = [0-9];
ws = [\ \t];
eol = ("\013"|"\010"|"\010"|"\013");

%%

<INITIAL>{ws}* => (continue());
<INITIAL>{eol} => (lin := !lin + 1; eolpos := yypos + size yytext; continue());


<COMMENT>{eol} => (lin := !lin + 1; eolpos := yypos + size yytext; continue());
<COMMENT> . => (continue());

<INITIAL> "(*" => (YYBEGIN COMMENT; continue());

<COMMENT> "*)" => (YYBEGIN INITIAL; continue());

<INITIAL>"fromDecimal" => (continue());
<INITIAL>"rational" => (col := yypos - (!eolpos); T.T_RAT(!lin, !col));
<INITIAL>"integer" => (col := yypos - (!eolpos); T.T_INT(!lin, !col));
<INITIAL>"boolean" => (col := yypos - (!eolpos); T.T_BOOL(!lin, !col));

<INITIAL>"call" => (col := yypos - (!eolpos); T.T_CALL(!lin, !col));
<INITIAL>"read" => (col := yypos - (!eolpos); T.T_READ(!lin, !col));
<INITIAL>"print" => (col := yypos - (!eolpos); T.T_PRINT(!lin, !col));

<INITIAL>"while" => (col := yypos - (!eolpos); T.T_WHILE(!lin, !col));
<INITIAL>"do" => (col := yypos - (!eolpos); T.T_DO(!lin, !col));
<INITIAL>"od" => (col := yypos - (!eolpos); T.T_OD(!lin, !col));
<INITIAL>"if" => (col := yypos - (!eolpos); T.T_IF(!lin, !col));
<INITIAL>"then" => (col := yypos - (!eolpos); T.T_THEN(!lin, !col));
<INITIAL>"else" => (col := yypos - (!eolpos); T.T_ELSE(!lin, !col));
<INITIAL>"fi" => (col := yypos - (!eolpos); T.T_FI(!lin, !col));

<INITIAL>":=" => (col := yypos - (!eolpos); T.T_ASSIGN(!lin, !col)); 

<INITIAL>"(" => (col := yypos - (!eolpos); T.T_LPAREN(!lin, !col)); 
<INITIAL>")" => (col := yypos - (!eolpos); T.T_RPAREN(!lin, !col)); 
<INITIAL>"{" => (col := yypos - (!eolpos); T.T_LCURLY(!lin, !col)); 
<INITIAL>"}" => (col := yypos - (!eolpos); T.T_RCURLY(!lin, !col)); 

<INITIAL>"tt" | "ff" => (col := yypos - (!eolpos); T.T_BOOLVAL(if yytext = "tt" then true else false, !lin, !col)); 

<INITIAL>"!" => (col := yypos - (!eolpos); T.T_NOT(!lin, !col));
<INITIAL>"&&" => (col := yypos - (!eolpos); T.T_AND(!lin, !col));
<INITIAL>"||" => (col := yypos - (!eolpos); T.T_OR(!lin, !col));

<INITIAL>".+." => (col := yypos - (!eolpos); T.T_RATADD(!lin, !col));
<INITIAL>".-." => (col := yypos - (!eolpos); T.T_RATSUB(!lin, !col));
<INITIAL>".*." => (col := yypos - (!eolpos); T.T_RATMUL(!lin, !col));
<INITIAL>"./." => (col := yypos - (!eolpos); T.T_RATDIV(!lin, !col));


<INITIAL>[~]?[0-9]*"."[0-9]*"("[0-9]+")" => (col := yypos - (!eolpos); T.T_RATVAL ((Rational.fromDecimal yytext), !lin, !col));

<INITIAL>[~]?[0-9]+ => (col := yypos - (!eolpos); T.T_INTVAL (#1(Rational.fromDecimal(yytext^".(0)")), !lin, !col));

<INITIAL>"~" => (col := yypos - (!eolpos); T.T_ADDINV(!lin, !col));
<INITIAL>"inverse" => (col := yypos - (!eolpos); T.T_INVERSE(!lin, !col));
<INITIAL>"procedure" => (col := yypos - (!eolpos); T.T_PROCEDURE(!lin, !col));
<INITIAL>"make_rat" => (col := yypos - (!eolpos); T.T_MAKERAT(!lin, !col));
<INITIAL>"rat" => (col := yypos - (!eolpos); T.T_RATFROMINT(!lin, !col));

<INITIAL>"+" => (col := yypos - (!eolpos); T.T_INTADD(!lin, !col));
<INITIAL>"-" => (col := yypos - (!eolpos); T.T_INTSUB(!lin, !col));
<INITIAL>"*" => (col := yypos - (!eolpos); T.T_INTMUL(!lin, !col));
<INITIAL>"/" => (col := yypos - (!eolpos); T.T_INTDIV(!lin, !col));
<INITIAL>"%" => (col := yypos - (!eolpos); T.T_INTMOD(!lin, !col));


<INITIAL>[A-Za-z][A-Za-z0-9]* => (col:=yypos-(!eolpos);T.T_IDENT(yytext,!lin,!col));


<INITIAL>"<=" => (col := yypos - (!eolpos); T.T_LE(!lin, !col));
<INITIAL>">=" => (col := yypos - (!eolpos); T.T_GE(!lin, !col));
<INITIAL>"<>" => (col := yypos - (!eolpos); T.T_NEQ(!lin, !col));
<INITIAL>"<" => (col := yypos - (!eolpos); T.T_LT(!lin, !col));
<INITIAL>">" => (col := yypos - (!eolpos); T.T_GT(!lin, !col));
<INITIAL>"=" => (col := yypos - (!eolpos); T.T_EQ(!lin, !col));


<INITIAL>"," => (col := yypos - (!eolpos); T.T_COMMA(!lin, !col));
<INITIAL>";" => (col := yypos - (!eolpos); T.T_SEMI(!lin, !col));

<INITIAL>. => (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue());
