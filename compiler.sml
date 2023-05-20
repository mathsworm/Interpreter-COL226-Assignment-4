(* compiler.sml *)
structure Pi :
sig val compile : string -> DataTypes.PROG
    val interpret : string * string -> unit
end =
struct
exception PiError;
fun compile (fileName) =
    let val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
            n => if TextIO.endOfStream inStream
                then ""
                else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
            print (fileName^"["^Int.toString line^":"
                ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = PiParser.parse
                (15,
                (PiParser.makeLexer grab fileName),
                printError,
                fileName)
            handle PiParser.ParseError => raise PiError;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in tree
    end





fun interpret(inp, out) = 
let


val os = TextIO.openOut out




fun getInput(message : string) =
   (print (message ^ "\n"); 
   let 
   val str= valOf(TextIO.inputLine TextIO.stdIn) 
   in 
   String.substring (str, 0, (String.size str)-1) 
   end)


val scope_list_rat = ref [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, Rational.rational) HashTable.hash_table] ;

fun enter_scope_rat () = scope_list_rat := [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, Rational.rational) HashTable.hash_table] @ (!scope_list_rat)

fun exit_scope_rat () = scope_list_rat := tl (!scope_list_rat)


val scope_list_int = ref [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, BigInt.bigint) HashTable.hash_table] ;

fun enter_scope_int () = scope_list_int := [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, BigInt.bigint) HashTable.hash_table] @ (!scope_list_int)

fun exit_scope_int () = scope_list_int := tl (!scope_list_int)


val scope_list_bool = ref [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, bool) HashTable.hash_table] ;

fun enter_scope_bool () = scope_list_bool := [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, bool) HashTable.hash_table] @ (!scope_list_bool)

fun exit_scope_bool () = scope_list_bool := tl (!scope_list_bool)


val scope_list_type = ref [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, string) HashTable.hash_table] ;

fun enter_scope_type () = scope_list_type := [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, string) HashTable.hash_table] @ (!scope_list_type)

fun exit_scope_type () = scope_list_type := tl (!scope_list_type)


val scope_list_proc = ref [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, DataTypes.BLOCK) HashTable.hash_table] ;

fun enter_scope_proc () = scope_list_proc := [HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") : (string, DataTypes.BLOCK) HashTable.hash_table] @ (!scope_list_proc)

fun exit_scope_proc () = scope_list_proc := tl (!scope_list_proc)


(* val prog_start = Pi.compile("test.rat") *)


exception ErrorInEvaluation
exception VariableRedeclared of string
exception ErrorInDeclaration
exception VariableUndeclared
exception TypeError
exception ProcedureRedeclared of string
exception ProcedureUndeclared


val symbol_table_type : (string, string) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found"); 

val symbol_table_procedure : (string, DataTypes.BLOCK) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found"); 

val symbol_table_int : (string, BigInt.bigint) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found"); 

val symbol_table_bool : (string, bool) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found"); 

val symbol_table_rat : (string, Rational.rational) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found"); 

(* fun get_type (ident : string) = 
    if(isSome (HashTable.find symbol_table_type ident)) 
    then 
        (HashTable.lookup symbol_table_type ident)
    else 
        raise VariableUndeclared;  *)



(* fun declare_procedure (ident : string, body : DataTypes.BLOCK) = 
    if isSome (HashTable.find symbol_table_procedure(ident)) 
    then 
        let 
        val var = print(ident)
        in 
        raise ProcedureRedeclared(ident) 
        end
    else 
        HashTable.insert symbol_table_procedure (ident, body);  *)


(* fun check_procedure (ident : string) =  
    if isSome (HashTable.find symbol_table_procedure(ident)) 
    then 
        ()
    else 
        raise ProcedureUndeclared;


fun get_value_procedure (ident : string) = 
    if isSome (HashTable.find symbol_table_procedure (ident)) then 
        (HashTable.lookup symbol_table_procedure ident)
    else 
        raise ProcedureUndeclared *)


(* fun declare_variable (ident : string, v_type : string) = 
        if isSome (HashTable.find symbol_table_type(ident)) 
        then 
                let 
                val var = print(ident)
                in 
                raise VariableRedeclared(ident) 
                end
        else 
                HashTable.insert symbol_table_type (ident, v_type);  *)



(* fun update_variable_int (ident : string, value : BigInt.bigint) = 
        if isSome (HashTable.find symbol_table_type (ident)) andalso isSome (HashTable.find symbol_table_int (ident)) 
        then 
            let 
                val _ = HashTable.remove symbol_table_int (ident)
            in
                HashTable.insert symbol_table_int (ident,value)
            end
        
        else if isSome (HashTable.find symbol_table_type (ident)) 
        then 
            HashTable.insert symbol_table_int (ident,value)
        else 
            raise VariableUndeclared; *)


(* fun get_value_int (ident : string) = 
    if isSome (HashTable.find symbol_table_int (ident)) then 
        (HashTable.lookup symbol_table_int ident)
    else 
        raise VariableUndeclared *)


(* fun update_variable_bool (ident : string, value : bool) = 
        if isSome (HashTable.find symbol_table_type (ident)) andalso isSome (HashTable.find symbol_table_bool (ident)) 
        then 
            let 
                val _ = HashTable.remove symbol_table_bool (ident)
            in
                HashTable.insert symbol_table_bool (ident,value)
            end
        
        else if isSome (HashTable.find symbol_table_type (ident)) 
        then 
            HashTable.insert symbol_table_bool (ident,value)
        else 
            raise VariableUndeclared; *)

(* fun get_value_bool (ident : string) = 
    if isSome (HashTable.find symbol_table_bool (ident)) then 
        (HashTable.lookup symbol_table_bool ident)
    else 
        raise VariableUndeclared *)




(* fun update_variable_rat (ident : string, value : Rational.rational) = 
        if isSome (HashTable.find symbol_table_type (ident)) andalso isSome (HashTable.find symbol_table_rat (ident)) 
        then 
            let 
                val _ = HashTable.remove symbol_table_rat (ident)
            in
                HashTable.insert symbol_table_rat (ident,value)
            end
        
        else if isSome (HashTable.find symbol_table_type (ident)) 
        then 
            HashTable.insert symbol_table_rat (ident,value)
        else 
            raise VariableUndeclared; *)


(* fun get_value_rat (ident : string) = 
    if isSome (HashTable.find symbol_table_rat (ident)) then 
        (HashTable.lookup symbol_table_rat ident)
    else 
        raise VariableUndeclared *)



fun get_value_helper (ident : string, l) =    
    if null l then raise VariableUndeclared
    else if isSome (HashTable.find (hd l) (ident)) then 
        (HashTable.lookup (hd l) ident)
    else 
        get_value_helper(ident, tl l)

fun get_value_rat (ident : string) = get_value_helper(ident, !scope_list_rat)
fun get_value_int (ident : string) = get_value_helper(ident, !scope_list_int)
fun get_value_bool (ident : string) = get_value_helper(ident, !scope_list_bool)
fun get_type (ident : string) = get_value_helper(ident, !scope_list_type)

fun get_value_procedure (ident : string) = get_value_helper(ident, !scope_list_proc)


fun declare_variable (ident : string, v_type : string) = 
        if isSome (HashTable.find (hd (!scope_list_type)) (ident)) 
        then 
                let 
                val var = print(ident)
                in 
                raise VariableRedeclared(ident) 
                end
        else 
                HashTable.insert (hd (!scope_list_type)) (ident, v_type); 


fun declare_procedure (ident : string, body : DataTypes.BLOCK) = 
        if isSome (HashTable.find (hd (!scope_list_proc)) (ident)) 
        then 
                let 
                val var = print(ident)
                in 
                raise VariableRedeclared(ident) 
                end
        else 
                HashTable.insert (hd (!scope_list_proc)) (ident, body); 


fun update_variable_helper (ident : string, v , l, type_scope) = 
    if null l then raise VariableUndeclared
    else 
        if isSome (HashTable.find (hd type_scope) (ident)) andalso isSome (HashTable.find (hd l) (ident)) 
        then 
            let 
                val _ = HashTable.remove (hd l) (ident)
            in
                HashTable.insert (hd l) (ident,v)
            end
        
        else if isSome (HashTable.find (hd type_scope) (ident)) 
        then 
            HashTable.insert (hd l) (ident,v)
        else 
            update_variable_helper(ident, v, tl l, tl type_scope)


fun update_variable_bool (ident, v) = update_variable_helper(ident, v, !scope_list_bool, !scope_list_type)

fun update_variable_rat (ident, v) = update_variable_helper(ident, v, !scope_list_rat, !scope_list_type)

fun update_variable_int (ident, v) = update_variable_helper(ident, v, !scope_list_int, !scope_list_type)



fun checkBool (ex : DataTypes.EXP) =   
case ex of 
    DataTypes.NOT(X) => checkBool(X)
    | DataTypes.AND(X,Y) => checkBool(X) andalso checkBool(Y)
    | DataTypes.OR(X,Y) => checkBool(X) andalso checkBool(Y)
    | DataTypes.LE(X,Y) => (checkInt(X) andalso checkInt(Y)) orelse (checkRat(X) andalso checkRat(Y))
    | DataTypes.LT(X,Y) => (checkInt(X) andalso checkInt(Y)) orelse (checkRat(X) andalso checkRat(Y))
    | DataTypes.GT(X,Y) => (checkInt(X) andalso checkInt(Y)) orelse (checkRat(X) andalso checkRat(Y))
    | DataTypes.GE(X,Y) => (checkInt(X) andalso checkInt(Y)) orelse (checkRat(X) andalso checkRat(Y))
    | DataTypes.EQ(X,Y) => (checkInt(X) andalso checkInt(Y)) orelse (checkRat(X) andalso checkRat(Y)) orelse (checkBool(X) andalso checkBool(Y))
    | DataTypes.NEQ(X,Y) => (checkInt(X) andalso checkInt(Y)) orelse (checkRat(X) andalso checkRat(Y)) orelse (checkBool(X) andalso checkBool(Y))
    | DataTypes.IDENT(X) => (get_type(X) = "boolean")
    | DataTypes.BOOLVAL(X) => true
    | _ => false


and checkInt (ex : DataTypes.EXP) = 
case ex of 
    DataTypes.IDENT(X) => (get_type(X) = "integer")
    | DataTypes.INTADD(X,Y) => checkInt(X) andalso checkInt(Y)
    | DataTypes.INTSUB(X,Y) => checkInt(X) andalso checkInt(Y)
    | DataTypes.INTMUL(X,Y) => checkInt(X) andalso checkInt(Y)
    | DataTypes.INTDIV(X,Y) => checkInt(X) andalso checkInt(Y)
    | DataTypes.INTMOD(X,Y) => checkInt(X) andalso checkInt(Y)
    | DataTypes.ADDINV(X) => checkInt(X)
    | DataTypes.INTVAL(X) => true
    | _ => false


and checkRat (ex : DataTypes.EXP) = 
case ex of 
    DataTypes.IDENT(X) => (get_type(X) = "rational")
    | DataTypes.RATADD(X,Y) => checkRat(X) andalso checkRat(Y)
    | DataTypes.RATSUB(X,Y) => checkRat(X) andalso checkRat(Y)
    | DataTypes.RATMUL(X,Y) => checkRat(X) andalso checkRat(Y)
    | DataTypes.RATDIV(X,Y) => checkRat(X) andalso checkRat(Y)
    | DataTypes.ADDINV(X) => checkRat(X)
    | DataTypes.MAKERAT(X,Y) => checkInt(X) andalso checkInt(Y)
    | DataTypes.INVERSE(X) => checkRat(X)
    | DataTypes.RAT(X) => checkInt(X)
    | DataTypes.RATVAL(X) => true
    | _ => false


fun evaluate_bool (ex : DataTypes.EXP) = 
case ex of 
    DataTypes.NOT(X) => not (evaluate_bool(X))
    | DataTypes.AND(X,Y) => evaluate_bool(X) andalso evaluate_bool(Y)
    | DataTypes.OR(X,Y) => evaluate_bool(X) orelse evaluate_bool(Y)
    | DataTypes.IDENT(X) => get_value_bool(X)
    | DataTypes.BOOLVAL(X) => X
    | DataTypes.EQ(X,Y) => if (checkInt(X) andalso checkInt(Y) andalso BigInt.equal(evaluate_int(X),evaluate_int(Y))) then true
                            else if (checkRat(X) andalso checkRat(Y) andalso Rational.equal(evaluate_rat(X),evaluate_rat(Y))) then true
                            else if (checkBool(X) andalso checkBool(Y)) then let val a = evaluate_bool X; val b = evaluate_bool Y; val z = (a orelse b) andalso not(a andalso b); in if z then false else true end
                            else false
    | DataTypes.NEQ(X,Y) => if (checkInt(X) andalso checkInt(Y) andalso BigInt.equal(evaluate_int(X),evaluate_int(Y))) then false
                            else if (checkRat(X) andalso checkRat(Y) andalso Rational.equal(evaluate_rat(X),evaluate_rat(Y))) then false
                            else if (checkBool(X) andalso checkBool(Y)) then let val a = evaluate_bool X; val b = evaluate_bool Y; val z = (a orelse b) andalso not(a andalso b); in if z then true else false end
                            else true   
    | DataTypes.LT(X,Y) => if (checkInt(X) andalso checkInt(Y) andalso BigInt.comp(evaluate_int(X),evaluate_int(Y))) then true
                            else if (checkRat(X) andalso checkRat(Y) andalso Rational.less(evaluate_rat(X),evaluate_rat(Y))) then true
                            else false
    | DataTypes.GT(X,Y) => if (checkInt(X) andalso checkInt(Y) andalso BigInt.comp(evaluate_int(Y),evaluate_int(X))) then true
                            else if (checkRat(X) andalso checkRat(Y) andalso Rational.less(evaluate_rat(Y),evaluate_rat(X))) then true
                            else false
    | DataTypes.GE(X,Y) => if (checkInt(X) andalso checkInt(Y) andalso(BigInt.comp(evaluate_int(Y),evaluate_int(X)) orelse BigInt.equal(evaluate_int(Y),evaluate_int(X))) ) then true
                            else if (checkRat(X) andalso checkRat(Y) andalso (Rational.less(evaluate_rat(Y),evaluate_rat(X)) orelse Rational.equal(evaluate_rat(Y),evaluate_rat(X)))) then true
                            else false
    | DataTypes.LE(X,Y) => if (checkInt(X) andalso checkInt(Y) andalso(BigInt.comp(evaluate_int(X),evaluate_int(Y)) orelse BigInt.equal(evaluate_int(Y),evaluate_int(X))) ) then true
                            else if (checkRat(X) andalso checkRat(Y) andalso (Rational.less(evaluate_rat(X),evaluate_rat(Y)) orelse Rational.equal(evaluate_rat(Y),evaluate_rat(X)))) then true
                            else false
    | _ => raise ErrorInEvaluation


and evaluate_int (ex : DataTypes.EXP) = 
case ex of 
    DataTypes.INTADD(X,Y) => BigInt.add (evaluate_int(X), evaluate_int(Y))
    | DataTypes.INTSUB(X,Y) => BigInt.subtract(evaluate_int(X), evaluate_int(Y))
    | DataTypes.INTMUL(X,Y) => BigInt.multiply(evaluate_int(X), evaluate_int(Y))
    | DataTypes.INTDIV(X,Y) => BigInt.divide(evaluate_int(X), evaluate_int(Y))
    | DataTypes.INTMOD(X,Y) => BigInt.modulo(evaluate_int(X), evaluate_int(Y))
    | DataTypes.ADDINV(X) => BigInt.multiply(evaluate_int(X), (true,[1]))
    | DataTypes.IDENT(X) => get_value_int(X)
    | DataTypes.INTVAL(X) => X
    | _ => raise ErrorInEvaluation


and evaluate_rat (ex : DataTypes.EXP) = 
case ex of 
    DataTypes.RATADD(X,Y) => Rational.add(evaluate_rat(X), evaluate_rat(Y))
    | DataTypes.RATSUB(X,Y) => Rational.subtract(evaluate_rat(X), evaluate_rat(Y))
    | DataTypes.RATMUL(X,Y) => Rational.multiply(evaluate_rat(X), evaluate_rat(Y))
    | DataTypes.RATDIV(X,Y) => valOf (Rational.divide(evaluate_rat(X), evaluate_rat(Y)))
    | DataTypes.RATVAL(X) => X 
    | DataTypes.ADDINV(X) => Rational.neg(evaluate_rat(X))
    | DataTypes.INVERSE(X) => valOf (Rational.inverse(evaluate_rat(X)))
    | DataTypes.RAT(X) => valOf (Rational.rat(evaluate_int(X)))
    | DataTypes.MAKERAT(X,Y) => valOf (Rational.make_rat(evaluate_int(X),evaluate_int(Y)))
    | DataTypes.IDENT(X) => get_value_rat(X)
    | _ => raise ErrorInEvaluation
    


fun process_cmd (cmd : DataTypes.CMD) =
case cmd of 

    DataTypes.ASSIGNCMD(A,B) => (
        if (checkBool B andalso (get_type(A) = "boolean")) then update_variable_bool(A,evaluate_bool(B))
        else if (checkInt B andalso (get_type(A) = "integer")) then update_variable_int(A,evaluate_int(B))
        else if (checkRat B andalso (get_type(A) = "rational")) then update_variable_rat(A,evaluate_rat(B))
        else raise ErrorInEvaluation
    )

    | DataTypes.PRINTCMD(A) => (
        if (checkBool(A)) then (if evaluate_bool A then TextIO.output (os, "tt") else TextIO.output (os, "ff"); TextIO.output (os, "\n")) 
        else if (checkInt(A)) then (TextIO.output (os, BigInt.print_bigint(evaluate_int(A))); TextIO.output (os, "\n")) 
        else if (checkRat(A)) then (TextIO.output (os, Rational.toDecimal(evaluate_rat(A))); TextIO.output (os, "\n")) 
        else raise ErrorInEvaluation
    ) 

    | DataTypes.WHILECMD(A,B) => 
        if not (checkBool(A)) then raise TypeError
        else
        let
        val truth = evaluate_bool(A);
        in
        if truth then 
            process_cmd_list(B@[DataTypes.WHILECMD(A,B)])
        else 
            ()
        end

    | DataTypes.ITE(C,A,B) => 
        if not (checkBool(C)) then raise TypeError
        else
        let
        val truth = evaluate_bool(C);
        in
        if truth then 
            process_cmd_list(A)
        else 
            process_cmd_list(B)
        end

    | DataTypes.CALLCMD(A) =>
        process_block(get_value_procedure A)
    

        (* if get_type(A) = "boolean" then 
            let 
            val _ = print("Input") 
            in 
            process_cmd(ASSIGNCMD(A,DataTypes.BOOLVAL(true)))
            end
        else raise TypeError; *)
    

    | DataTypes.READCMD(A) => 
        let
        val inp = getInput ("Input " ^ A ^ " : ")
        in
        if get_type (A) = "rational" then process_cmd(DataTypes.ASSIGNCMD(A,DataTypes.RATVAL(Rational.fromDecimal(inp))))
        else if get_type (A) = "integer" then process_cmd(DataTypes.ASSIGNCMD(A,DataTypes.INTVAL(#1 (Rational.fromDecimal(inp ^ ".(0)")))))
        else if get_type (A) = "boolean" andalso inp = "tt" then process_cmd(DataTypes.ASSIGNCMD(A,DataTypes.BOOLVAL(true)))
        else if get_type (A) = "boolean" andalso inp = "ff" then process_cmd(DataTypes.ASSIGNCMD(A,DataTypes.BOOLVAL(false)))
        else raise TypeError
        end

        (* | DataTypes.READCMD(A) => 
        let
        val inp = getInput ("Input" ^ A ^ " : ")
        in
        if get_type (A) = "rational" then update_variable_rat(A,Rational.fromDecimal(inp))
        else if get_type (A) = "integer" then update_variable_int(A,#1(Rational.fromDecimal(inp ^ ".(0)")))
        else if get_type (A) = "boolean" andalso inp = "tt" then update_variable_bool(A,true)
        else if get_type (A) = "boolean" andalso inp = "ff" then update_variable_bool(A,false)
        else raise TypeError
        end *)
    


and process_cmd_list (l) =
if null(l) then () else 
    let
    val _ = process_cmd (hd l)
    in 
    (* print("hello\n") ; *)
    process_cmd_list (tl l) 
    end 


and process_dec (dec) = 
    case dec of 
        DataTypes.BOOLEAN (X) => declare_variable(X, "boolean")
        | DataTypes.INTEGER (X) => declare_variable(X, "integer")
        | DataTypes.RATIONAL (X) => declare_variable(X, "rational")
        | DataTypes.PROCEDURE (A,B) => declare_procedure(A,B)


and process_dec_list [] = ()
| process_dec_list (h::t) = 
    let
    val _ = process_dec (h)
    in 
    process_dec_list (t)
    end 


and process_block (block) = 
    let
    val _ = enter_scope_rat();
    val _ = enter_scope_bool();
    val _ = enter_scope_int();
    val _ = enter_scope_type();
    val _ = enter_scope_proc();
    val DataTypes.BLOCK(runtime_dec_list, runtime_cmd_list) = block;
    val _ = process_dec_list (runtime_dec_list)
    val _ = process_cmd_list (runtime_cmd_list)
    val _ = exit_scope_rat();
    val _ = exit_scope_bool();
    val _ = exit_scope_int();
    val _ = exit_scope_type();
    val _ = exit_scope_proc();
    in
    ()
    end

and process_prog (prog) = 
    let
    val DataTypes.PROG(block) = prog
    val _ = process_block(block)
    in
    ()
    end



val prog_tree = compile inp
val _ = process_prog prog_tree



in
    TextIO.closeOut os 
end


end;