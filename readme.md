# RationalPL0 Language

In this assignment, we have designed a programming language which supports basic data types like integers, rationals and booleans. One additional feature of the language is that the rationals and booleans can be of arbitrary length, which is inherited from the rational package which was implemented in assignment 3. 


The lexing and parsing phase of the compiler are completed with the help of ml-lex and ml-yacc. The grammar of the language is as follows (lowercase represents the non terminals and uppercase represents the terminals).


        prog : block 
        block : decl_seq cmd_seq 
        decl_seq : [var_decl] [proc_decl] 
        var_decl : [ratvar_decl] [intvar_decl] [boolvar_decl]  
        ratvar_decl : RAT idents_rat SEMI 
        intvar_decl : INT idents_int SEMI
        boolvar_decl : BOOL idents_bool SEMI 
        idents_rat : IDENT COMMA idents_rat | IDENT 
        idents_int : IDENT COMMA idents_int | IDENT 
        idents_bool : IDENT COMMA idents_bool | IDENT 
        proc_decl : proc_def SEMI proc_decl | proc_def SEMI 
        proc_def : PROCEDURE IDENT block 
        assign_cmd : IDENT ASSIGN exp
        call_cmd : CALL IDENT
        read_cmd : READ LPAREN IDENT RPAREN 
        princmd : PRINT LPAREN exp RPAREN 
        while_cmd : WHILE exp DO cmd_seq OD 
        if_cmd : IF exp THEN cmd_seq ELSE cmd_seq FI 
        cmd_seq : LCURLY cmds RCURLY 
        cmds : cmd SEMI cmds | eps
        cmd : assign_cmd | princmd | while_cmd | call_cmd | read_cmd | if_cmd 
        exp : IDENT | BOOLVAL | exp LE exp | exp GE exp | exp LT exp | exp GT exp | exp EQ exp 
            | exp NEQ exp | exp OR exp | exp AND exp | NOT exp | LPAREN exp RPAREN | exp INTADD exp 
            | exp INTSUB exp | exp INTMUL exp | exp INTDIV exp | exp INTMOD exp | ADDINV exp 
            | INTVAL | exp RATADD exp | exp RATSUB exp | exp RATMUL exp | exp RATDIV exp 
            | INVERSE exp | MAKERAT LPAREN exp COMMA exp RPAREN | RATFROMINT exp | RATVAL 


The features implemented in the language are:
- while 
- if - then - else
- prodedure definitions (parameterless functions)
- reading input from the command line
- printing of values


Instructions for use : 
- Open a terminal
- Run the command "sml run.sml"
- Use the execute function, specifying the input and output files

## Instructions for Use
1. Compile the SML file run.sml
2. Use the interpret function with two string arguments, the input and the output files to run the contents of the input file, and put the corresponding results in the output file.

## Implementation Overview 

### Phases
- Lexical analysis - The lexer first classifies the lexemes based on regexes.
- Parsing - The parser then creates an AST based on the nodes which are defined as in the datatypes file
- Type checking - Typechecking is done in at runtime using the checkBool, checkInt, checkRat functions in the compiler file. This is done during the actual traversal of the abstract syntax tree. 
- Then, the traversal of the abstract syntax tree is done. Computation of expressions is done using pattern matching and structural induction. Type checking is also done using pattern matching and structural induction. 
- Variables are stored in symbol tables. 


### Scoping
- Scoping is implemented using a list of symbol tables. The first element represents the variables declared in the current scope. To search for a variable, we must start searching from the head of the list and traverse backwards recursively, to ensure that we use the most recently declared instance of a variable. 
- Whenever we start the execution of a block, we append a new empty scope at the start of the list, and enter all information about fresh declarations in this new symbol table. 
- Whenever the execution of a  block ends, we must pop the most recent scope. This is to ensure that the variables local to that block do not get used elsewhere. 


### Design decisions 
- The make_rat function takes inputs within parantheses - make_rat(x,y) where x and y are both integer expressions.
- rational numbers are printed in decimal normal form in the output. 
- Each of the print statements give their output in a new line.
- There is no unary plus symbol in the grammar of the language (expressions like x := +2; don't make sense)
- Comments are not nested. This means that the expression (* hello *) is a comment in itself, and so is (* (* hello *), but (* (* hello *) *) isn't a comment, as the comment closes at the first occurence of *).
- For division and modulo operation on negative integers has the following conventions:
    a div b = sgn(a) * sgn(b) * (abs(a) div abs(b))
    a mod b = a - b * (a div b)

### Exceptions 
Exceptions are raised at runtime
- if we redeclare variables in the same scope. 
- if we assign values of the wrong type to a variable. 
- if we write type incompatible expressions. (for example 1 + 2.3(7) )
- if we try to assign values to undeclared variables. 
- if we try to call procedures which are not declared.

Apart from this, we can also get errors while creating the abstract syntax tree, which are given by the parser and the lexer. However, the parser tries to predict and correct the errors automatically (which could be a boon or a bane depending on the scenario). The parser raises precise errors specifying the line and column number in the file. 



### Example test cases

1. RECURSIVE FACTORIAL 
(to demonstrate that recursion works correctly)

        integer n,f;
        procedure factorial
        {
          if (n = 0) then {}
          else
          {
            f := f * n;
            n := n - 1;
            call factorial;
          }
          fi;
        };

        {
          read(n);
          f := 1;
          call factorial;
          print(f);
        }


2. TO DEMONSTRATE SCOPES

        integer a;
        procedure hello
        integer a;
        {
            a := 67;
            print(a);
        };  
        {
            a := 1;
            print (a);
            call hello;
            print(a);
        }

output:
        1
        67
        1


This lets us ensure that the variable a has the value 1 in the scope of the bottom block, and has the value 67 in the scope defined inside the procedure hello. When we return from the procedure, the outermost declaration of the variable a is the one on the top most line, which had the value 1, and hence 1 is printed.


Examples of incorrect syntax that raises errors:

        integer x;
        {
            x := tt;
        }
This raises the exception ErrorInEvaluation


        integer x;
        {
            x := 1 .+. 1;
        }
This also raises the exception ErrorInEvaluation


## Acknowledgements

The following sources have been used and consulted while doing the assignment, namely
* http://rogerprice.org/ug/ug.pdf
* Course Notes, COL 226, IIT Delhi
* https://github.com/ChinmayMittal/COL226/tree/main/Assignment%204
* Compilers - Principles, Techniques and Tools by Alfred V Aho et al

The code for the glue file and the cm file have been taken (almost verbatim) from the first resource. 
The semantics for the while language have been referred to from the course notes.
