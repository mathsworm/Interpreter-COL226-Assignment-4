structure DataTypes = 
struct
    datatype PROG = PROG of BLOCK

    and BLOCK = BLOCK of (DEC list) * (CMD list)
    (* should have a cmd list *)


    and DEC = BOOLEAN of string | INTEGER of string | RATIONAL of string | PROCEDURE of (string * BLOCK) 

    and CMD = ASSIGNCMD of (string * EXP) | CALLCMD of string | READCMD of string | PRINTCMD of EXP 
                | WHILECMD of (EXP * (CMD list)) | ITE of (EXP * (CMD list) * (CMD list))

    and EXP = NOT of EXP | AND of (EXP * EXP) | OR of (EXP * EXP) 
                | LE of (EXP * EXP) | LT of (EXP * EXP) | GT of (EXP * EXP) | GE of (EXP * EXP) | EQ of (EXP * EXP) | NEQ of (EXP * EXP) 
                | IDENT of string 
                | RATADD of (EXP * EXP) | RATSUB of (EXP * EXP) | RATMUL of (EXP * EXP) | RATDIV of (EXP * EXP) | INVERSE of (EXP) 
                | MAKERAT of (EXP * EXP) | RAT of (EXP) 
                | ADDINV of EXP 
                | INTADD of (EXP * EXP) | INTSUB of (EXP * EXP) | INTMUL of (EXP * EXP) | INTDIV of (EXP * EXP) | INTMOD of (EXP * EXP)
                | BOOLVAL of bool | INTVAL of BigInt.bigint | RATVAL of Rational.rational
end; 