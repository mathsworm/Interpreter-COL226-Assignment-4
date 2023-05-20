signature Pi_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val T_PROCEDURE:  'a * 'a -> (svalue,'a) token
val T_ADDINV:  'a * 'a -> (svalue,'a) token
val T_INTVAL: (BigInt.bigint) *  'a * 'a -> (svalue,'a) token
val T_RATVAL: (Rational.rational) *  'a * 'a -> (svalue,'a) token
val T_RATFROMINT:  'a * 'a -> (svalue,'a) token
val T_MAKERAT:  'a * 'a -> (svalue,'a) token
val T_INVERSE:  'a * 'a -> (svalue,'a) token
val T_NEQ:  'a * 'a -> (svalue,'a) token
val T_EQ:  'a * 'a -> (svalue,'a) token
val T_GT:  'a * 'a -> (svalue,'a) token
val T_LT:  'a * 'a -> (svalue,'a) token
val T_GE:  'a * 'a -> (svalue,'a) token
val T_LE:  'a * 'a -> (svalue,'a) token
val T_FI:  'a * 'a -> (svalue,'a) token
val T_ELSE:  'a * 'a -> (svalue,'a) token
val T_THEN:  'a * 'a -> (svalue,'a) token
val T_IF:  'a * 'a -> (svalue,'a) token
val T_OD:  'a * 'a -> (svalue,'a) token
val T_DO:  'a * 'a -> (svalue,'a) token
val T_WHILE:  'a * 'a -> (svalue,'a) token
val T_PRINT:  'a * 'a -> (svalue,'a) token
val T_READ:  'a * 'a -> (svalue,'a) token
val T_CALL:  'a * 'a -> (svalue,'a) token
val T_ASSIGN:  'a * 'a -> (svalue,'a) token
val T_INTMOD:  'a * 'a -> (svalue,'a) token
val T_INTDIV:  'a * 'a -> (svalue,'a) token
val T_INTMUL:  'a * 'a -> (svalue,'a) token
val T_INTSUB:  'a * 'a -> (svalue,'a) token
val T_INTADD:  'a * 'a -> (svalue,'a) token
val T_RATDIV:  'a * 'a -> (svalue,'a) token
val T_RATMUL:  'a * 'a -> (svalue,'a) token
val T_RATSUB:  'a * 'a -> (svalue,'a) token
val T_RATADD:  'a * 'a -> (svalue,'a) token
val T_RCURLY:  'a * 'a -> (svalue,'a) token
val T_LCURLY:  'a * 'a -> (svalue,'a) token
val T_RPAREN:  'a * 'a -> (svalue,'a) token
val T_LPAREN:  'a * 'a -> (svalue,'a) token
val T_NOT:  'a * 'a -> (svalue,'a) token
val T_OR:  'a * 'a -> (svalue,'a) token
val T_AND:  'a * 'a -> (svalue,'a) token
val T_BOOLVAL: (bool) *  'a * 'a -> (svalue,'a) token
val T_IDENT: (string) *  'a * 'a -> (svalue,'a) token
val T_BOOL:  'a * 'a -> (svalue,'a) token
val T_INT:  'a * 'a -> (svalue,'a) token
val T_RAT:  'a * 'a -> (svalue,'a) token
val T_SEMI:  'a * 'a -> (svalue,'a) token
val T_COMMA:  'a * 'a -> (svalue,'a) token
end
signature Pi_LRVALS=
sig
structure Tokens : Pi_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
