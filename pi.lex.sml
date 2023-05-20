functor PiLexFun(structure Tokens: Pi_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
Za | alpha | digit | z | A | COMMENT | INITIAL | tates
    structure UserDeclarations = 
      struct

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




      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (fileName:string)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (lin := !lin + 1; eolpos := yypos + size yytext; continue())
      end
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (lin := !lin + 1; eolpos := yypos + size yytext; continue())
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; continue()))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_RAT(!lin, !col)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_INT(!lin, !col)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_BOOL(!lin, !col)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_CALL(!lin, !col)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_READ(!lin, !col)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_PRINT(!lin, !col)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_WHILE(!lin, !col)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_DO(!lin, !col)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_OD(!lin, !col)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_IF(!lin, !col)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_THEN(!lin, !col)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_ELSE(!lin, !col)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_FI(!lin, !col)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_ASSIGN(!lin, !col)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_LPAREN(!lin, !col)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_RPAREN(!lin, !col)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_LCURLY(!lin, !col)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_RCURLY(!lin, !col)))
fun yyAction25 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col := yypos - (!eolpos); T.T_BOOLVAL(if yytext = "tt" then true else false, !lin, !col))
      end
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_NOT(!lin, !col)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_AND(!lin, !col)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_OR(!lin, !col)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_RATADD(!lin, !col)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_RATSUB(!lin, !col)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_RATMUL(!lin, !col)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_RATDIV(!lin, !col)))
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col := yypos - (!eolpos); T.T_RATVAL ((Rational.fromDecimal yytext), !lin, !col))
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col := yypos - (!eolpos); T.T_INTVAL (#1(Rational.fromDecimal(yytext^".(0)")), !lin, !col))
      end
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_ADDINV(!lin, !col)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_INVERSE(!lin, !col)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_PROCEDURE(!lin, !col)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_MAKERAT(!lin, !col)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_RATFROMINT(!lin, !col)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_INTADD(!lin, !col)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_INTSUB(!lin, !col)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_INTMUL(!lin, !col)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_INTDIV(!lin, !col)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_INTMOD(!lin, !col)))
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=yypos-(!eolpos);T.T_IDENT(yytext,!lin,!col))
      end
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_LE(!lin, !col)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_GE(!lin, !col)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_NEQ(!lin, !col)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_LT(!lin, !col)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_GT(!lin, !col)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_EQ(!lin, !col)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_COMMA(!lin, !col)))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := yypos - (!eolpos); T.T_SEMI(!lin, !col)))
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue())
      end
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yystuck(lastMatch)
            else if inp < #"*"
              then if inp = #")"
                  then yyQ50(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ49(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ49(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #")"
              then yystuck(lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ48(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ46(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ46(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ46(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ47(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ46(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ47(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ51(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction13(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ56(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"l"
              then yyQ55(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"i"
              then yyQ54(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"i"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"h"
              then yyQ53(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"h"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction25(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"n"
              then yyQ60(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"n"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ59(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ58(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"h"
                  then yyQ57(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"d"
              then yyQ64(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"d"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"b"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ63(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"l"
              then yyQ70(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"b"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ69(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"n"
              then yyQ68(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"n"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"o"
              then yyQ67(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"i"
              then yyQ66(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"i"
              then if inp <= #"`"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"t"
              then yyQ65(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"t"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ61(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ62(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ79(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"r"
              then yyQ78(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"r"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"u"
              then yyQ77(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"u"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"d"
              then yyQ76(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"d"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ75(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"c"
              then yyQ74(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"c"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"t"
              then yyQ81(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"t"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"n"
              then yyQ80(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"n"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ73(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"i"
                  then yyQ72(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"r"
              then yyQ71(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"r"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"d"
              then yyQ82(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"d"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"t"
              then yyQ89(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ88(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ87(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"`"
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ86(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ85(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"k"
              then yyQ84(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"k"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"b"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ83(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction36(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ97(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"s"
              then yyQ96(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"s"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"r"
              then yyQ95(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"r"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ94(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"r"
              then yyQ101(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"r"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ100(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"g"
              then yyQ99(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"g"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ98(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ93(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"t"
                  then yyQ92(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ91(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ90(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"l"
              then yyQ112(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"b"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ111(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"m"
              then yyQ110(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"m"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"i"
              then yyQ109(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"i"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"c"
              then yyQ108(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"c"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ107(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"D"
              then yyQ106(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"D"
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"m"
              then yyQ105(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"m"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"o"
              then yyQ104(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"f"
              then yyQ58(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"A"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction45(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                      else yyAction45(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"r"
              then yyQ103(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"i"
                  then yyQ102(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ115(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"s"
              then yyQ114(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"s"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"l"
              then yyQ113(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction14(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"o"
              then yyQ116(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"l"
              then yyQ119(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"l"
              then yyQ118(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"b"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ117(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"n"
              then yyQ125(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"n"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"b"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ124(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"e"
              then yyQ123(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"l"
              then yyQ122(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"o"
              then yyQ121(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction45(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction45(strm, yyNO_MATCH)
                      else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp = #"o"
              then yyQ120(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction45(strm, yyNO_MATCH)
                  else yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction45(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
                  else yyAction45(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ52(strm', yyMATCH(strm, yyAction45, yyNO_MATCH))
              else yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ126 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ126(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ128 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ127 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ128(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ127(strm', yyMATCH(strm, yyAction49, yyNO_MATCH))
                  else yyAction49(strm, yyNO_MATCH)
              else yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ129 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ129(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ46(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ47(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ134 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ133 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ134(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ135 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ132 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ135(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ136 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ131 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ136(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ137 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ130 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ137(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #","
              then yyAction54(strm, yyNO_MATCH)
            else if inp < #","
              then if inp = #")"
                  then yyAction54(strm, yyNO_MATCH)
                else if inp < #")"
                  then if inp = #"("
                      then yyQ48(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                      else yyAction54(strm, yyNO_MATCH)
                else if inp = #"*"
                  then yyQ130(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                  else yyQ131(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
            else if inp = #"/"
              then yyQ133(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
            else if inp < #"/"
              then if inp = #"-"
                  then yyQ132(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                  else yyAction54(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ46(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ138 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ138(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ139 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ139(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ140 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ140(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ140(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ140(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ140(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #")"
                  then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #")"
                  then if inp = #" "
                      then yyQ9(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #" "
                      then if inp = #"\v"
                          then yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ9(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                            else if inp = #"\n"
                              then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                              else yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #"\r"
                          then yyQ11(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"&"
                      then yyQ14(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"&"
                      then if inp = #"\""
                          then yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp < #"\""
                          then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #"%"
                          then yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"'"
                      then yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ15(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"0"
                  then yyQ23(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"-"
                      then yyQ20(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"-"
                      then if inp = #"+"
                          then yyQ18(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #"*"
                          then yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ19(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"."
                      then yyQ21(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ22(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"<"
                  then yyQ26(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"<"
                  then if inp = #":"
                      then yyQ24(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #";"
                      then yyQ25(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ23(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #">"
                  then yyQ28(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"="
                  then yyQ27(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"o"
              then yyQ37(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"e"
                  then yyQ33(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"e"
                  then if inp = #"b"
                      then yyQ30(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"b"
                      then if inp = #"["
                          then yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp < #"["
                          then yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #"a"
                          then yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"c"
                      then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ32(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"j"
                  then yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"j"
                  then if inp = #"g"
                      then yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"g"
                      then yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"i"
                      then yyQ35(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"m"
                  then yyQ36(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"w"
              then yyQ41(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"w"
              then if inp = #"s"
                  then yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"s"
                  then if inp = #"q"
                      then yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"p"
                      then yyQ38(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ39(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"t"
                  then yyQ40(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"}"
              then yyQ44(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"}"
              then if inp = #"{"
                  then yyQ42(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"|"
                  then yyQ43(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"~"
              then yyQ45(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ7(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyQ5(strm', lastMatch)
            else if inp < #"\r"
              then if inp = #"\n"
                  then yyQ4(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = #"*"
              then yyQ6(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of Za => yyQ0(!(yystrm), yyNO_MATCH)
    | alpha => yyQ0(!(yystrm), yyNO_MATCH)
    | digit => yyQ0(!(yystrm), yyNO_MATCH)
    | z => yyQ0(!(yystrm), yyNO_MATCH)
    | A => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
    | tates => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
