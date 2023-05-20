functor PiLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Pi_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* pi.yacc *)
open DataTypes

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\173\000\002\000\173\000\008\000\173\000\009\000\173\000\
\\012\000\173\000\015\000\173\000\016\000\173\000\017\000\173\000\
\\018\000\173\000\019\000\173\000\020\000\173\000\021\000\173\000\
\\022\000\173\000\023\000\173\000\029\000\173\000\032\000\173\000\
\\035\000\173\000\036\000\173\000\037\000\173\000\038\000\173\000\
\\039\000\173\000\040\000\173\000\000\000\
\\001\000\001\000\174\000\002\000\174\000\008\000\174\000\009\000\174\000\
\\012\000\174\000\015\000\174\000\016\000\174\000\017\000\174\000\
\\018\000\174\000\019\000\174\000\020\000\174\000\021\000\174\000\
\\022\000\174\000\023\000\174\000\029\000\174\000\032\000\174\000\
\\035\000\174\000\036\000\174\000\037\000\174\000\038\000\174\000\
\\039\000\174\000\040\000\174\000\000\000\
\\001\000\001\000\175\000\002\000\175\000\008\000\175\000\009\000\175\000\
\\012\000\175\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\085\000\020\000\084\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\175\000\032\000\175\000\
\\035\000\175\000\036\000\175\000\037\000\175\000\038\000\175\000\
\\039\000\175\000\040\000\175\000\000\000\
\\001\000\001\000\176\000\002\000\176\000\008\000\176\000\009\000\176\000\
\\012\000\176\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\085\000\020\000\084\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\176\000\032\000\176\000\
\\035\000\176\000\036\000\176\000\037\000\176\000\038\000\176\000\
\\039\000\176\000\040\000\176\000\000\000\
\\001\000\001\000\177\000\002\000\177\000\008\000\177\000\009\000\177\000\
\\012\000\177\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\085\000\020\000\084\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\177\000\032\000\177\000\
\\035\000\177\000\036\000\177\000\037\000\177\000\038\000\177\000\
\\039\000\177\000\040\000\177\000\000\000\
\\001\000\001\000\178\000\002\000\178\000\008\000\178\000\009\000\178\000\
\\012\000\178\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\085\000\020\000\084\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\178\000\032\000\178\000\
\\035\000\178\000\036\000\178\000\037\000\178\000\038\000\178\000\
\\039\000\178\000\040\000\178\000\000\000\
\\001\000\001\000\179\000\002\000\179\000\008\000\179\000\009\000\179\000\
\\012\000\179\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\085\000\020\000\084\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\179\000\032\000\179\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\179\000\040\000\179\000\000\000\
\\001\000\001\000\180\000\002\000\180\000\008\000\180\000\009\000\180\000\
\\012\000\180\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\085\000\020\000\084\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\180\000\032\000\180\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\180\000\040\000\180\000\000\000\
\\001\000\001\000\181\000\002\000\181\000\008\000\091\000\009\000\181\000\
\\012\000\181\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\085\000\020\000\084\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\181\000\032\000\181\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\075\000\040\000\074\000\000\000\
\\001\000\001\000\182\000\002\000\182\000\008\000\182\000\009\000\182\000\
\\012\000\182\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\085\000\020\000\084\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\182\000\032\000\182\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\075\000\040\000\074\000\000\000\
\\001\000\001\000\183\000\002\000\183\000\008\000\183\000\009\000\183\000\
\\012\000\183\000\015\000\183\000\016\000\183\000\017\000\183\000\
\\018\000\183\000\019\000\183\000\020\000\183\000\021\000\183\000\
\\022\000\183\000\023\000\183\000\029\000\183\000\032\000\183\000\
\\035\000\183\000\036\000\183\000\037\000\183\000\038\000\183\000\
\\039\000\183\000\040\000\183\000\000\000\
\\001\000\001\000\184\000\002\000\184\000\008\000\184\000\009\000\184\000\
\\012\000\184\000\015\000\184\000\016\000\184\000\017\000\184\000\
\\018\000\184\000\019\000\184\000\020\000\184\000\021\000\184\000\
\\022\000\184\000\023\000\184\000\029\000\184\000\032\000\184\000\
\\035\000\184\000\036\000\184\000\037\000\184\000\038\000\184\000\
\\039\000\184\000\040\000\184\000\000\000\
\\001\000\001\000\185\000\002\000\185\000\008\000\185\000\009\000\185\000\
\\012\000\185\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\185\000\020\000\185\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\185\000\032\000\185\000\
\\035\000\185\000\036\000\185\000\037\000\185\000\038\000\185\000\
\\039\000\185\000\040\000\185\000\000\000\
\\001\000\001\000\186\000\002\000\186\000\008\000\186\000\009\000\186\000\
\\012\000\186\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\186\000\020\000\186\000\021\000\083\000\
\\022\000\082\000\023\000\081\000\029\000\186\000\032\000\186\000\
\\035\000\186\000\036\000\186\000\037\000\186\000\038\000\186\000\
\\039\000\186\000\040\000\186\000\000\000\
\\001\000\001\000\187\000\002\000\187\000\008\000\187\000\009\000\187\000\
\\012\000\187\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\187\000\020\000\187\000\021\000\187\000\
\\022\000\187\000\023\000\187\000\029\000\187\000\032\000\187\000\
\\035\000\187\000\036\000\187\000\037\000\187\000\038\000\187\000\
\\039\000\187\000\040\000\187\000\000\000\
\\001\000\001\000\188\000\002\000\188\000\008\000\188\000\009\000\188\000\
\\012\000\188\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\188\000\020\000\188\000\021\000\188\000\
\\022\000\188\000\023\000\188\000\029\000\188\000\032\000\188\000\
\\035\000\188\000\036\000\188\000\037\000\188\000\038\000\188\000\
\\039\000\188\000\040\000\188\000\000\000\
\\001\000\001\000\189\000\002\000\189\000\008\000\189\000\009\000\189\000\
\\012\000\189\000\015\000\089\000\016\000\088\000\017\000\087\000\
\\018\000\086\000\019\000\189\000\020\000\189\000\021\000\189\000\
\\022\000\189\000\023\000\189\000\029\000\189\000\032\000\189\000\
\\035\000\189\000\036\000\189\000\037\000\189\000\038\000\189\000\
\\039\000\189\000\040\000\189\000\000\000\
\\001\000\001\000\190\000\002\000\190\000\008\000\190\000\009\000\190\000\
\\012\000\190\000\015\000\190\000\016\000\190\000\017\000\190\000\
\\018\000\190\000\019\000\190\000\020\000\190\000\021\000\190\000\
\\022\000\190\000\023\000\190\000\029\000\190\000\032\000\190\000\
\\035\000\190\000\036\000\190\000\037\000\190\000\038\000\190\000\
\\039\000\190\000\040\000\190\000\000\000\
\\001\000\001\000\191\000\002\000\191\000\008\000\191\000\009\000\191\000\
\\012\000\191\000\015\000\191\000\016\000\191\000\017\000\191\000\
\\018\000\191\000\019\000\191\000\020\000\191\000\021\000\191\000\
\\022\000\191\000\023\000\191\000\029\000\191\000\032\000\191\000\
\\035\000\191\000\036\000\191\000\037\000\191\000\038\000\191\000\
\\039\000\191\000\040\000\191\000\000\000\
\\001\000\001\000\192\000\002\000\192\000\008\000\192\000\009\000\192\000\
\\012\000\192\000\015\000\192\000\016\000\192\000\017\000\087\000\
\\018\000\086\000\019\000\192\000\020\000\192\000\021\000\192\000\
\\022\000\192\000\023\000\192\000\029\000\192\000\032\000\192\000\
\\035\000\192\000\036\000\192\000\037\000\192\000\038\000\192\000\
\\039\000\192\000\040\000\192\000\000\000\
\\001\000\001\000\193\000\002\000\193\000\008\000\193\000\009\000\193\000\
\\012\000\193\000\015\000\193\000\016\000\193\000\017\000\087\000\
\\018\000\086\000\019\000\193\000\020\000\193\000\021\000\193\000\
\\022\000\193\000\023\000\193\000\029\000\193\000\032\000\193\000\
\\035\000\193\000\036\000\193\000\037\000\193\000\038\000\193\000\
\\039\000\193\000\040\000\193\000\000\000\
\\001\000\001\000\194\000\002\000\194\000\008\000\194\000\009\000\194\000\
\\012\000\194\000\015\000\194\000\016\000\194\000\017\000\194\000\
\\018\000\194\000\019\000\194\000\020\000\194\000\021\000\194\000\
\\022\000\194\000\023\000\194\000\029\000\194\000\032\000\194\000\
\\035\000\194\000\036\000\194\000\037\000\194\000\038\000\194\000\
\\039\000\194\000\040\000\194\000\000\000\
\\001\000\001\000\195\000\002\000\195\000\008\000\195\000\009\000\195\000\
\\012\000\195\000\015\000\195\000\016\000\195\000\017\000\195\000\
\\018\000\195\000\019\000\195\000\020\000\195\000\021\000\195\000\
\\022\000\195\000\023\000\195\000\029\000\195\000\032\000\195\000\
\\035\000\195\000\036\000\195\000\037\000\195\000\038\000\195\000\
\\039\000\195\000\040\000\195\000\000\000\
\\001\000\001\000\196\000\002\000\196\000\008\000\196\000\009\000\196\000\
\\012\000\196\000\015\000\196\000\016\000\196\000\017\000\196\000\
\\018\000\196\000\019\000\196\000\020\000\196\000\021\000\196\000\
\\022\000\196\000\023\000\196\000\029\000\196\000\032\000\196\000\
\\035\000\196\000\036\000\196\000\037\000\196\000\038\000\196\000\
\\039\000\196\000\040\000\196\000\000\000\
\\001\000\001\000\197\000\002\000\197\000\008\000\197\000\009\000\197\000\
\\012\000\197\000\015\000\197\000\016\000\197\000\017\000\197\000\
\\018\000\197\000\019\000\197\000\020\000\197\000\021\000\197\000\
\\022\000\197\000\023\000\197\000\029\000\197\000\032\000\197\000\
\\035\000\197\000\036\000\197\000\037\000\197\000\038\000\197\000\
\\039\000\197\000\040\000\197\000\000\000\
\\001\000\001\000\198\000\002\000\198\000\008\000\198\000\009\000\198\000\
\\012\000\198\000\015\000\198\000\016\000\198\000\017\000\198\000\
\\018\000\198\000\019\000\198\000\020\000\198\000\021\000\198\000\
\\022\000\198\000\023\000\198\000\029\000\198\000\032\000\198\000\
\\035\000\198\000\036\000\198\000\037\000\198\000\038\000\198\000\
\\039\000\198\000\040\000\198\000\000\000\
\\001\000\001\000\199\000\002\000\199\000\008\000\199\000\009\000\199\000\
\\012\000\199\000\015\000\199\000\016\000\199\000\017\000\199\000\
\\018\000\199\000\019\000\199\000\020\000\199\000\021\000\199\000\
\\022\000\199\000\023\000\199\000\029\000\199\000\032\000\199\000\
\\035\000\199\000\036\000\199\000\037\000\199\000\038\000\199\000\
\\039\000\199\000\040\000\199\000\000\000\
\\001\000\001\000\047\000\002\000\154\000\000\000\
\\001\000\001\000\049\000\002\000\152\000\000\000\
\\001\000\001\000\051\000\002\000\150\000\000\000\
\\001\000\001\000\126\000\008\000\091\000\009\000\090\000\015\000\089\000\
\\016\000\088\000\017\000\087\000\018\000\086\000\019\000\085\000\
\\020\000\084\000\021\000\083\000\022\000\082\000\023\000\081\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\075\000\040\000\074\000\000\000\
\\001\000\002\000\134\000\048\000\134\000\000\000\
\\001\000\002\000\149\000\000\000\
\\001\000\002\000\151\000\000\000\
\\001\000\002\000\153\000\000\000\
\\001\000\002\000\157\000\000\000\
\\001\000\002\000\158\000\008\000\091\000\009\000\090\000\015\000\089\000\
\\016\000\088\000\017\000\087\000\018\000\086\000\019\000\085\000\
\\020\000\084\000\021\000\083\000\022\000\082\000\023\000\081\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\075\000\040\000\074\000\000\000\
\\001\000\002\000\159\000\000\000\
\\001\000\002\000\160\000\000\000\
\\001\000\002\000\161\000\000\000\
\\001\000\002\000\162\000\000\000\
\\001\000\002\000\163\000\000\000\
\\001\000\002\000\164\000\030\000\164\000\033\000\164\000\034\000\164\000\
\\048\000\164\000\000\000\
\\001\000\002\000\167\000\000\000\
\\001\000\002\000\168\000\000\000\
\\001\000\002\000\169\000\000\000\
\\001\000\002\000\170\000\000\000\
\\001\000\002\000\171\000\000\000\
\\001\000\002\000\172\000\000\000\
\\001\000\002\000\018\000\000\000\
\\001\000\002\000\046\000\000\000\
\\001\000\002\000\048\000\000\000\
\\001\000\002\000\050\000\000\000\
\\001\000\002\000\052\000\000\000\
\\001\000\003\000\014\000\004\000\013\000\005\000\012\000\013\000\138\000\
\\047\000\011\000\000\000\
\\001\000\004\000\146\000\005\000\146\000\013\000\146\000\047\000\146\000\000\000\
\\001\000\004\000\013\000\005\000\012\000\013\000\143\000\047\000\143\000\000\000\
\\001\000\005\000\147\000\013\000\147\000\047\000\147\000\000\000\
\\001\000\005\000\012\000\013\000\140\000\047\000\140\000\000\000\
\\001\000\005\000\012\000\013\000\144\000\047\000\144\000\000\000\
\\001\000\006\000\022\000\000\000\
\\001\000\006\000\024\000\000\000\
\\001\000\006\000\026\000\000\000\
\\001\000\006\000\028\000\000\000\
\\001\000\006\000\044\000\014\000\166\000\025\000\043\000\026\000\042\000\
\\027\000\041\000\028\000\040\000\031\000\039\000\000\000\
\\001\000\006\000\064\000\007\000\063\000\010\000\062\000\011\000\061\000\
\\041\000\060\000\042\000\059\000\043\000\058\000\044\000\057\000\
\\045\000\056\000\046\000\055\000\000\000\
\\001\000\006\000\068\000\000\000\
\\001\000\006\000\100\000\000\000\
\\001\000\008\000\091\000\009\000\090\000\012\000\121\000\015\000\089\000\
\\016\000\088\000\017\000\087\000\018\000\086\000\019\000\085\000\
\\020\000\084\000\021\000\083\000\022\000\082\000\023\000\081\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\075\000\040\000\074\000\000\000\
\\001\000\008\000\091\000\009\000\090\000\012\000\123\000\015\000\089\000\
\\016\000\088\000\017\000\087\000\018\000\086\000\019\000\085\000\
\\020\000\084\000\021\000\083\000\022\000\082\000\023\000\081\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\075\000\040\000\074\000\000\000\
\\001\000\008\000\091\000\009\000\090\000\012\000\131\000\015\000\089\000\
\\016\000\088\000\017\000\087\000\018\000\086\000\019\000\085\000\
\\020\000\084\000\021\000\083\000\022\000\082\000\023\000\081\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\075\000\040\000\074\000\000\000\
\\001\000\008\000\091\000\009\000\090\000\015\000\089\000\016\000\088\000\
\\017\000\087\000\018\000\086\000\019\000\085\000\020\000\084\000\
\\021\000\083\000\022\000\082\000\023\000\081\000\029\000\098\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\075\000\040\000\074\000\000\000\
\\001\000\008\000\091\000\009\000\090\000\015\000\089\000\016\000\088\000\
\\017\000\087\000\018\000\086\000\019\000\085\000\020\000\084\000\
\\021\000\083\000\022\000\082\000\023\000\081\000\032\000\080\000\
\\035\000\079\000\036\000\078\000\037\000\077\000\038\000\076\000\
\\039\000\075\000\040\000\074\000\000\000\
\\001\000\011\000\066\000\000\000\
\\001\000\011\000\067\000\000\000\
\\001\000\011\000\094\000\000\000\
\\001\000\012\000\124\000\000\000\
\\001\000\013\000\135\000\000\000\
\\001\000\013\000\136\000\047\000\011\000\000\000\
\\001\000\013\000\137\000\000\000\
\\001\000\013\000\139\000\047\000\139\000\000\000\
\\001\000\013\000\141\000\047\000\141\000\000\000\
\\001\000\013\000\142\000\047\000\142\000\000\000\
\\001\000\013\000\145\000\047\000\145\000\000\000\
\\001\000\013\000\148\000\047\000\148\000\000\000\
\\001\000\013\000\155\000\000\000\
\\001\000\013\000\156\000\047\000\011\000\000\000\
\\001\000\013\000\021\000\000\000\
\\001\000\014\000\165\000\000\000\
\\001\000\014\000\053\000\000\000\
\\001\000\024\000\069\000\000\000\
\\001\000\030\000\127\000\000\000\
\\001\000\033\000\125\000\000\000\
\\001\000\034\000\130\000\000\000\
\\001\000\048\000\000\000\000\000\
\\001\000\048\000\133\000\000\000\
\"
val actionRowNumbers =
"\054\000\059\000\083\000\056\000\
\\049\000\079\000\078\000\087\000\
\\095\000\060\000\061\000\062\000\
\\063\000\082\000\058\000\081\000\
\\086\000\077\000\031\000\064\000\
\\054\000\050\000\027\000\051\000\
\\028\000\052\000\029\000\080\000\
\\085\000\048\000\047\000\046\000\
\\044\000\045\000\043\000\053\000\
\\089\000\065\000\065\000\073\000\
\\074\000\066\000\090\000\035\000\
\\084\000\061\000\057\000\062\000\
\\055\000\063\000\064\000\042\000\
\\072\000\065\000\018\000\026\000\
\\065\000\075\000\065\000\065\000\
\\065\000\001\000\000\000\071\000\
\\065\000\067\000\037\000\065\000\
\\034\000\033\000\032\000\088\000\
\\065\000\065\000\065\000\065\000\
\\065\000\065\000\087\000\065\000\
\\065\000\065\000\065\000\065\000\
\\065\000\065\000\065\000\065\000\
\\065\000\065\000\017\000\025\000\
\\065\000\023\000\068\000\010\000\
\\087\000\069\000\076\000\036\000\
\\007\000\006\000\005\000\004\000\
\\003\000\002\000\092\000\016\000\
\\015\000\014\000\013\000\012\000\
\\022\000\021\000\020\000\019\000\
\\008\000\009\000\030\000\011\000\
\\091\000\039\000\038\000\087\000\
\\065\000\040\000\093\000\070\000\
\\041\000\024\000\094\000"
val gotoT =
"\
\\001\000\130\000\002\000\008\000\003\000\007\000\004\000\006\000\
\\005\000\005\000\006\000\004\000\007\000\003\000\008\000\002\000\
\\009\000\001\000\000\000\
\\008\000\013\000\000\000\
\\000\000\
\\008\000\015\000\009\000\014\000\000\000\
\\000\000\
\\000\000\
\\005\000\017\000\006\000\004\000\000\000\
\\013\000\018\000\000\000\
\\000\000\
\\000\000\
\\011\000\021\000\000\000\
\\012\000\023\000\000\000\
\\010\000\025\000\000\000\
\\000\000\
\\008\000\027\000\000\000\
\\000\000\
\\005\000\028\000\006\000\004\000\000\000\
\\000\000\
\\000\000\
\\014\000\036\000\015\000\035\000\016\000\034\000\018\000\033\000\
\\019\000\032\000\020\000\031\000\021\000\030\000\022\000\029\000\000\000\
\\002\000\043\000\003\000\007\000\004\000\006\000\005\000\005\000\
\\006\000\004\000\007\000\003\000\008\000\002\000\009\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\052\000\000\000\
\\017\000\063\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\068\000\000\000\
\\000\000\
\\012\000\069\000\000\000\
\\000\000\
\\010\000\070\000\000\000\
\\014\000\071\000\015\000\035\000\016\000\034\000\018\000\033\000\
\\019\000\032\000\020\000\031\000\021\000\030\000\022\000\029\000\000\000\
\\000\000\
\\000\000\
\\017\000\090\000\000\000\
\\000\000\
\\000\000\
\\017\000\091\000\000\000\
\\000\000\
\\017\000\093\000\000\000\
\\017\000\094\000\000\000\
\\017\000\095\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\097\000\000\000\
\\000\000\
\\000\000\
\\017\000\099\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\100\000\000\000\
\\017\000\101\000\000\000\
\\017\000\102\000\000\000\
\\017\000\103\000\000\000\
\\017\000\104\000\000\000\
\\017\000\105\000\000\000\
\\013\000\106\000\000\000\
\\017\000\107\000\000\000\
\\017\000\108\000\000\000\
\\017\000\109\000\000\000\
\\017\000\110\000\000\000\
\\017\000\111\000\000\000\
\\017\000\112\000\000\000\
\\017\000\113\000\000\000\
\\017\000\114\000\000\000\
\\017\000\115\000\000\000\
\\017\000\116\000\000\000\
\\017\000\117\000\000\000\
\\000\000\
\\000\000\
\\017\000\118\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\120\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\126\000\000\000\
\\017\000\127\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 131
val numrules = 67
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | T_INTVAL of unit ->  (BigInt.bigint)
 | T_RATVAL of unit ->  (Rational.rational)
 | T_BOOLVAL of unit ->  (bool) | T_IDENT of unit ->  (string)
 | if_cmd of unit ->  (CMD) | read_cmd of unit ->  (CMD)
 | call_cmd of unit ->  (CMD) | print_cmd of unit ->  (CMD)
 | while_cmd of unit ->  (CMD) | exp of unit ->  (EXP)
 | assign_cmd of unit ->  (CMD) | cmd of unit ->  (CMD)
 | cmds of unit ->  ( ( CMD list ) )
 | cmd_seq of unit ->  ( ( CMD list ) )
 | idents_int of unit ->  ( ( DEC list ) )
 | idents_bool of unit ->  ( ( DEC list ) )
 | idents_rat of unit ->  ( ( DEC list ) )
 | intvar_decl of unit ->  ( ( DEC list ) )
 | boolvar_decl of unit ->  ( ( DEC list ) )
 | ratvar_decl of unit ->  ( ( DEC list ) )
 | proc_def of unit ->  ( ( DEC ) )
 | proc_decl of unit ->  ( ( DEC list ) )
 | var_decl of unit ->  ( ( DEC list ) )
 | decl_seq of unit ->  ( ( DEC list ) ) | block of unit ->  (BLOCK)
 | prog of unit ->  (PROG)
end
type svalue = MlyValue.svalue
type result = PROG
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 4) => true | (T 3) => true | (T 2) => true | (T 30) => true | 
(T 31) => true | (T 32) => true | (T 33) => true | (T 27) => true | 
(T 28) => true | (T 29) => true | (T 25) => true | (T 26) => true | 
(T 24) => true | (T 40) => true | (T 41) => true | (T 42) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 47) => true | _ => false
val showTerminal =
fn (T 0) => "T_COMMA"
  | (T 1) => "T_SEMI"
  | (T 2) => "T_RAT"
  | (T 3) => "T_INT"
  | (T 4) => "T_BOOL"
  | (T 5) => "T_IDENT"
  | (T 6) => "T_BOOLVAL"
  | (T 7) => "T_AND"
  | (T 8) => "T_OR"
  | (T 9) => "T_NOT"
  | (T 10) => "T_LPAREN"
  | (T 11) => "T_RPAREN"
  | (T 12) => "T_LCURLY"
  | (T 13) => "T_RCURLY"
  | (T 14) => "T_RATADD"
  | (T 15) => "T_RATSUB"
  | (T 16) => "T_RATMUL"
  | (T 17) => "T_RATDIV"
  | (T 18) => "T_INTADD"
  | (T 19) => "T_INTSUB"
  | (T 20) => "T_INTMUL"
  | (T 21) => "T_INTDIV"
  | (T 22) => "T_INTMOD"
  | (T 23) => "T_ASSIGN"
  | (T 24) => "T_CALL"
  | (T 25) => "T_READ"
  | (T 26) => "T_PRINT"
  | (T 27) => "T_WHILE"
  | (T 28) => "T_DO"
  | (T 29) => "T_OD"
  | (T 30) => "T_IF"
  | (T 31) => "T_THEN"
  | (T 32) => "T_ELSE"
  | (T 33) => "T_FI"
  | (T 34) => "T_LE"
  | (T 35) => "T_GE"
  | (T 36) => "T_LT"
  | (T 37) => "T_GT"
  | (T 38) => "T_EQ"
  | (T 39) => "T_NEQ"
  | (T 40) => "T_INVERSE"
  | (T 41) => "T_MAKERAT"
  | (T 42) => "T_RATFROMINT"
  | (T 43) => "T_RATVAL"
  | (T 44) => "T_INTVAL"
  | (T 45) => "T_ADDINV"
  | (T 46) => "T_PROCEDURE"
  | (T 47) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 4) $$ (T 3) $$ (T 2) $$ 
(T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, block1left, block1right)) :: 
rest671)) => let val  result = MlyValue.prog (fn _ => let val  (block
 as block1) = block1 ()
 in (PROG (block))
end)
 in ( LrTable.NT 0, ( result, block1left, block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.cmd_seq cmd_seq1, _, cmd_seq1right)) :: ( _,
 ( MlyValue.decl_seq decl_seq1, decl_seq1left, _)) :: rest671)) => let
 val  result = MlyValue.block (fn _ => let val  (decl_seq as decl_seq1
) = decl_seq1 ()
 val  (cmd_seq as cmd_seq1) = cmd_seq1 ()
 in (BLOCK (decl_seq, cmd_seq))
end)
 in ( LrTable.NT 1, ( result, decl_seq1left, cmd_seq1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.proc_decl proc_decl1, _, proc_decl1right))
 :: ( _, ( MlyValue.var_decl var_decl1, var_decl1left, _)) :: rest671)
) => let val  result = MlyValue.decl_seq (fn _ => let val  (var_decl
 as var_decl1) = var_decl1 ()
 val  (proc_decl as proc_decl1) = proc_decl1 ()
 in (var_decl @ proc_decl)
end)
 in ( LrTable.NT 2, ( result, var_decl1left, proc_decl1right), rest671
)
end
|  ( 3, ( ( _, ( MlyValue.var_decl var_decl1, var_decl1left, 
var_decl1right)) :: rest671)) => let val  result = MlyValue.decl_seq
 (fn _ => let val  (var_decl as var_decl1) = var_decl1 ()
 in (var_decl)
end)
 in ( LrTable.NT 2, ( result, var_decl1left, var_decl1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.proc_decl proc_decl1, proc_decl1left, 
proc_decl1right)) :: rest671)) => let val  result = MlyValue.decl_seq
 (fn _ => let val  (proc_decl as proc_decl1) = proc_decl1 ()
 in (proc_decl)
end)
 in ( LrTable.NT 2, ( result, proc_decl1left, proc_decl1right), 
rest671)
end
|  ( 5, ( rest671)) => let val  result = MlyValue.decl_seq (fn _ => (
[]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( MlyValue.boolvar_decl boolvar_decl1, _, 
boolvar_decl1right)) :: ( _, ( MlyValue.intvar_decl intvar_decl1, _, _
)) :: ( _, ( MlyValue.ratvar_decl ratvar_decl1, ratvar_decl1left, _))
 :: rest671)) => let val  result = MlyValue.var_decl (fn _ => let val 
 (ratvar_decl as ratvar_decl1) = ratvar_decl1 ()
 val  (intvar_decl as intvar_decl1) = intvar_decl1 ()
 val  (boolvar_decl as boolvar_decl1) = boolvar_decl1 ()
 in (ratvar_decl @ intvar_decl @ boolvar_decl)
end)
 in ( LrTable.NT 3, ( result, ratvar_decl1left, boolvar_decl1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.intvar_decl intvar_decl1, _, 
intvar_decl1right)) :: ( _, ( MlyValue.ratvar_decl ratvar_decl1, 
ratvar_decl1left, _)) :: rest671)) => let val  result = 
MlyValue.var_decl (fn _ => let val  (ratvar_decl as ratvar_decl1) = 
ratvar_decl1 ()
 val  (intvar_decl as intvar_decl1) = intvar_decl1 ()
 in (ratvar_decl @ intvar_decl)
end)
 in ( LrTable.NT 3, ( result, ratvar_decl1left, intvar_decl1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.boolvar_decl boolvar_decl1, _, 
boolvar_decl1right)) :: ( _, ( MlyValue.ratvar_decl ratvar_decl1, 
ratvar_decl1left, _)) :: rest671)) => let val  result = 
MlyValue.var_decl (fn _ => let val  (ratvar_decl as ratvar_decl1) = 
ratvar_decl1 ()
 val  (boolvar_decl as boolvar_decl1) = boolvar_decl1 ()
 in (ratvar_decl @ boolvar_decl)
end)
 in ( LrTable.NT 3, ( result, ratvar_decl1left, boolvar_decl1right), 
rest671)
end
|  ( 9, ( ( _, ( MlyValue.boolvar_decl boolvar_decl1, _, 
boolvar_decl1right)) :: ( _, ( MlyValue.intvar_decl intvar_decl1, 
intvar_decl1left, _)) :: rest671)) => let val  result = 
MlyValue.var_decl (fn _ => let val  (intvar_decl as intvar_decl1) = 
intvar_decl1 ()
 val  (boolvar_decl as boolvar_decl1) = boolvar_decl1 ()
 in (intvar_decl @ boolvar_decl)
end)
 in ( LrTable.NT 3, ( result, intvar_decl1left, boolvar_decl1right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.ratvar_decl ratvar_decl1, ratvar_decl1left,
 ratvar_decl1right)) :: rest671)) => let val  result = 
MlyValue.var_decl (fn _ => let val  (ratvar_decl as ratvar_decl1) = 
ratvar_decl1 ()
 in (ratvar_decl)
end)
 in ( LrTable.NT 3, ( result, ratvar_decl1left, ratvar_decl1right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.intvar_decl intvar_decl1, intvar_decl1left,
 intvar_decl1right)) :: rest671)) => let val  result = 
MlyValue.var_decl (fn _ => let val  (intvar_decl as intvar_decl1) = 
intvar_decl1 ()
 in (intvar_decl)
end)
 in ( LrTable.NT 3, ( result, intvar_decl1left, intvar_decl1right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.boolvar_decl boolvar_decl1, 
boolvar_decl1left, boolvar_decl1right)) :: rest671)) => let val  
result = MlyValue.var_decl (fn _ => let val  (boolvar_decl as 
boolvar_decl1) = boolvar_decl1 ()
 in (boolvar_decl)
end)
 in ( LrTable.NT 3, ( result, boolvar_decl1left, boolvar_decl1right), 
rest671)
end
|  ( 13, ( ( _, ( _, _, T_SEMI1right)) :: ( _, ( MlyValue.idents_rat 
idents_rat1, _, _)) :: ( _, ( _, T_RAT1left, _)) :: rest671)) => let
 val  result = MlyValue.ratvar_decl (fn _ => let val  (idents_rat as 
idents_rat1) = idents_rat1 ()
 in (idents_rat)
end)
 in ( LrTable.NT 6, ( result, T_RAT1left, T_SEMI1right), rest671)
end
|  ( 14, ( ( _, ( _, _, T_SEMI1right)) :: ( _, ( MlyValue.idents_int 
idents_int1, _, _)) :: ( _, ( _, T_INT1left, _)) :: rest671)) => let
 val  result = MlyValue.intvar_decl (fn _ => let val  (idents_int as 
idents_int1) = idents_int1 ()
 in (idents_int)
end)
 in ( LrTable.NT 8, ( result, T_INT1left, T_SEMI1right), rest671)
end
|  ( 15, ( ( _, ( _, _, T_SEMI1right)) :: ( _, ( MlyValue.idents_bool 
idents_bool1, _, _)) :: ( _, ( _, T_BOOL1left, _)) :: rest671)) => let
 val  result = MlyValue.boolvar_decl (fn _ => let val  (idents_bool
 as idents_bool1) = idents_bool1 ()
 in (idents_bool)
end)
 in ( LrTable.NT 7, ( result, T_BOOL1left, T_SEMI1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.idents_rat idents_rat1, _, idents_rat1right
)) :: _ :: ( _, ( MlyValue.T_IDENT T_IDENT1, T_IDENT1left, _)) :: 
rest671)) => let val  result = MlyValue.idents_rat (fn _ => let val  (
T_IDENT as T_IDENT1) = T_IDENT1 ()
 val  (idents_rat as idents_rat1) = idents_rat1 ()
 in (RATIONAL(T_IDENT) :: idents_rat)
end)
 in ( LrTable.NT 9, ( result, T_IDENT1left, idents_rat1right), rest671
)
end
|  ( 17, ( ( _, ( MlyValue.T_IDENT T_IDENT1, T_IDENT1left, 
T_IDENT1right)) :: rest671)) => let val  result = MlyValue.idents_rat
 (fn _ => let val  (T_IDENT as T_IDENT1) = T_IDENT1 ()
 in ([RATIONAL(T_IDENT)])
end)
 in ( LrTable.NT 9, ( result, T_IDENT1left, T_IDENT1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.idents_int idents_int1, _, idents_int1right
)) :: _ :: ( _, ( MlyValue.T_IDENT T_IDENT1, T_IDENT1left, _)) :: 
rest671)) => let val  result = MlyValue.idents_int (fn _ => let val  (
T_IDENT as T_IDENT1) = T_IDENT1 ()
 val  (idents_int as idents_int1) = idents_int1 ()
 in (INTEGER(T_IDENT) :: idents_int)
end)
 in ( LrTable.NT 11, ( result, T_IDENT1left, idents_int1right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.T_IDENT T_IDENT1, T_IDENT1left, 
T_IDENT1right)) :: rest671)) => let val  result = MlyValue.idents_int
 (fn _ => let val  (T_IDENT as T_IDENT1) = T_IDENT1 ()
 in ([INTEGER(T_IDENT)])
end)
 in ( LrTable.NT 11, ( result, T_IDENT1left, T_IDENT1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.idents_bool idents_bool1, _, 
idents_bool1right)) :: _ :: ( _, ( MlyValue.T_IDENT T_IDENT1, 
T_IDENT1left, _)) :: rest671)) => let val  result = 
MlyValue.idents_bool (fn _ => let val  (T_IDENT as T_IDENT1) = 
T_IDENT1 ()
 val  (idents_bool as idents_bool1) = idents_bool1 ()
 in (BOOLEAN(T_IDENT) :: idents_bool)
end)
 in ( LrTable.NT 10, ( result, T_IDENT1left, idents_bool1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.T_IDENT T_IDENT1, T_IDENT1left, 
T_IDENT1right)) :: rest671)) => let val  result = MlyValue.idents_bool
 (fn _ => let val  (T_IDENT as T_IDENT1) = T_IDENT1 ()
 in ([BOOLEAN(T_IDENT)])
end)
 in ( LrTable.NT 10, ( result, T_IDENT1left, T_IDENT1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.proc_decl proc_decl1, _, proc_decl1right))
 :: _ :: ( _, ( MlyValue.proc_def proc_def1, proc_def1left, _)) :: 
rest671)) => let val  result = MlyValue.proc_decl (fn _ => let val  (
proc_def as proc_def1) = proc_def1 ()
 val  (proc_decl as proc_decl1) = proc_decl1 ()
 in (proc_def :: proc_decl)
end)
 in ( LrTable.NT 4, ( result, proc_def1left, proc_decl1right), rest671
)
end
|  ( 23, ( ( _, ( _, _, T_SEMI1right)) :: ( _, ( MlyValue.proc_def 
proc_def1, proc_def1left, _)) :: rest671)) => let val  result = 
MlyValue.proc_decl (fn _ => let val  (proc_def as proc_def1) = 
proc_def1 ()
 in ([proc_def])
end)
 in ( LrTable.NT 4, ( result, proc_def1left, T_SEMI1right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.block block1, _, block1right)) :: ( _, ( 
MlyValue.T_IDENT T_IDENT1, _, _)) :: ( _, ( _, T_PROCEDURE1left, _))
 :: rest671)) => let val  result = MlyValue.proc_def (fn _ => let val 
 (T_IDENT as T_IDENT1) = T_IDENT1 ()
 val  (block as block1) = block1 ()
 in (PROCEDURE(T_IDENT,block))
end)
 in ( LrTable.NT 5, ( result, T_PROCEDURE1left, block1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.T_IDENT T_IDENT1, T_IDENT1left, _)) :: rest671)) => let val  
result = MlyValue.assign_cmd (fn _ => let val  (T_IDENT as T_IDENT1) =
 T_IDENT1 ()
 val  (exp as exp1) = exp1 ()
 in (ASSIGNCMD(T_IDENT, exp))
end)
 in ( LrTable.NT 15, ( result, T_IDENT1left, exp1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.T_IDENT T_IDENT1, _, T_IDENT1right)) :: ( _
, ( _, T_CALL1left, _)) :: rest671)) => let val  result = 
MlyValue.call_cmd (fn _ => let val  (T_IDENT as T_IDENT1) = T_IDENT1
 ()
 in (CALLCMD(T_IDENT))
end)
 in ( LrTable.NT 19, ( result, T_CALL1left, T_IDENT1right), rest671)

end
|  ( 27, ( ( _, ( _, _, T_RPAREN1right)) :: ( _, ( MlyValue.T_IDENT 
T_IDENT1, _, _)) :: _ :: ( _, ( _, T_READ1left, _)) :: rest671)) =>
 let val  result = MlyValue.read_cmd (fn _ => let val  (T_IDENT as 
T_IDENT1) = T_IDENT1 ()
 in (READCMD(T_IDENT))
end)
 in ( LrTable.NT 20, ( result, T_READ1left, T_RPAREN1right), rest671)

end
|  ( 28, ( ( _, ( _, _, T_RPAREN1right)) :: ( _, ( MlyValue.exp exp1,
 _, _)) :: _ :: ( _, ( _, T_PRINT1left, _)) :: rest671)) => let val  
result = MlyValue.print_cmd (fn _ => let val  (exp as exp1) = exp1 ()
 in (PRINTCMD(exp))
end)
 in ( LrTable.NT 18, ( result, T_PRINT1left, T_RPAREN1right), rest671)

end
|  ( 29, ( ( _, ( _, _, T_OD1right)) :: ( _, ( MlyValue.cmd_seq 
cmd_seq1, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _,
 T_WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.while_cmd (fn _ => let val  (exp as exp1) = exp1 ()
 val  (cmd_seq as cmd_seq1) = cmd_seq1 ()
 in (WHILECMD(exp,cmd_seq))
end)
 in ( LrTable.NT 17, ( result, T_WHILE1left, T_OD1right), rest671)
end
|  ( 30, ( ( _, ( _, _, T_FI1right)) :: ( _, ( MlyValue.cmd_seq 
cmd_seq2, _, _)) :: _ :: ( _, ( MlyValue.cmd_seq cmd_seq1, _, _)) :: _
 :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, T_IF1left, _)) :: 
rest671)) => let val  result = MlyValue.if_cmd (fn _ => let val  (exp
 as exp1) = exp1 ()
 val  cmd_seq1 = cmd_seq1 ()
 val  cmd_seq2 = cmd_seq2 ()
 in (ITE(exp,cmd_seq1,cmd_seq2))
end)
 in ( LrTable.NT 21, ( result, T_IF1left, T_FI1right), rest671)
end
|  ( 31, ( ( _, ( _, _, T_RCURLY1right)) :: ( _, ( MlyValue.cmds cmds1
, _, _)) :: ( _, ( _, T_LCURLY1left, _)) :: rest671)) => let val  
result = MlyValue.cmd_seq (fn _ => let val  (cmds as cmds1) = cmds1 ()
 in (cmds)
end)
 in ( LrTable.NT 12, ( result, T_LCURLY1left, T_RCURLY1right), rest671
)
end
|  ( 32, ( ( _, ( MlyValue.cmds cmds1, _, cmds1right)) :: _ :: ( _, ( 
MlyValue.cmd cmd1, cmd1left, _)) :: rest671)) => let val  result = 
MlyValue.cmds (fn _ => let val  (cmd as cmd1) = cmd1 ()
 val  (cmds as cmds1) = cmds1 ()
 in (cmd :: cmds)
end)
 in ( LrTable.NT 13, ( result, cmd1left, cmds1right), rest671)
end
|  ( 33, ( rest671)) => let val  result = MlyValue.cmds (fn _ => ([]))
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 34, ( ( _, ( MlyValue.assign_cmd assign_cmd1, assign_cmd1left, 
assign_cmd1right)) :: rest671)) => let val  result = MlyValue.cmd (fn
 _ => let val  (assign_cmd as assign_cmd1) = assign_cmd1 ()
 in (assign_cmd)
end)
 in ( LrTable.NT 14, ( result, assign_cmd1left, assign_cmd1right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.print_cmd print_cmd1, print_cmd1left, 
print_cmd1right)) :: rest671)) => let val  result = MlyValue.cmd (fn _
 => let val  (print_cmd as print_cmd1) = print_cmd1 ()
 in (print_cmd)
end)
 in ( LrTable.NT 14, ( result, print_cmd1left, print_cmd1right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.while_cmd while_cmd1, while_cmd1left, 
while_cmd1right)) :: rest671)) => let val  result = MlyValue.cmd (fn _
 => let val  (while_cmd as while_cmd1) = while_cmd1 ()
 in (while_cmd)
end)
 in ( LrTable.NT 14, ( result, while_cmd1left, while_cmd1right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.call_cmd call_cmd1, call_cmd1left, 
call_cmd1right)) :: rest671)) => let val  result = MlyValue.cmd (fn _
 => let val  (call_cmd as call_cmd1) = call_cmd1 ()
 in (call_cmd)
end)
 in ( LrTable.NT 14, ( result, call_cmd1left, call_cmd1right), rest671
)
end
|  ( 38, ( ( _, ( MlyValue.read_cmd read_cmd1, read_cmd1left, 
read_cmd1right)) :: rest671)) => let val  result = MlyValue.cmd (fn _
 => let val  (read_cmd as read_cmd1) = read_cmd1 ()
 in (read_cmd)
end)
 in ( LrTable.NT 14, ( result, read_cmd1left, read_cmd1right), rest671
)
end
|  ( 39, ( ( _, ( MlyValue.if_cmd if_cmd1, if_cmd1left, if_cmd1right))
 :: rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (
if_cmd as if_cmd1) = if_cmd1 ()
 in (if_cmd)
end)
 in ( LrTable.NT 14, ( result, if_cmd1left, if_cmd1right), rest671)

end
|  ( 40, ( ( _, ( MlyValue.T_IDENT T_IDENT1, T_IDENT1left, 
T_IDENT1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (T_IDENT as T_IDENT1) = T_IDENT1 ()
 in (IDENT(T_IDENT))
end)
 in ( LrTable.NT 16, ( result, T_IDENT1left, T_IDENT1right), rest671)

end
|  ( 41, ( ( _, ( MlyValue.T_BOOLVAL T_BOOLVAL1, T_BOOLVAL1left, 
T_BOOLVAL1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (T_BOOLVAL as T_BOOLVAL1) = T_BOOLVAL1 ()
 in (BOOLVAL(T_BOOLVAL))
end)
 in ( LrTable.NT 16, ( result, T_BOOLVAL1left, T_BOOLVAL1right), 
rest671)
end
|  ( 42, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (LE(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (GE(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (LT(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (GT(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (EQ(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (NEQ(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (OR(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (AND(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
T_NOT1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (exp as exp1) = exp1 ()
 in (NOT(exp))
end)
 in ( LrTable.NT 16, ( result, T_NOT1left, exp1right), rest671)
end
|  ( 51, ( ( _, ( _, _, T_RPAREN1right)) :: ( _, ( MlyValue.exp exp1,
 _, _)) :: ( _, ( _, T_LPAREN1left, _)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 16, ( result, T_LPAREN1left, T_RPAREN1right), rest671
)
end
|  ( 52, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (INTADD(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (INTSUB(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (INTMUL(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (INTDIV(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (INTMOD(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
T_ADDINV1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn
 _ => let val  (exp as exp1) = exp1 ()
 in (ADDINV (exp))
end)
 in ( LrTable.NT 16, ( result, T_ADDINV1left, exp1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.T_INTVAL T_INTVAL1, T_INTVAL1left, 
T_INTVAL1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (T_INTVAL as T_INTVAL1) = T_INTVAL1 ()
 in (INTVAL(T_INTVAL))
end)
 in ( LrTable.NT 16, ( result, T_INTVAL1left, T_INTVAL1right), rest671
)
end
|  ( 59, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (RATADD(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (RATSUB(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (RATMUL(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (RATDIV(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
T_INVERSE1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn
 _ => let val  (exp as exp1) = exp1 ()
 in (INVERSE(exp))
end)
 in ( LrTable.NT 16, ( result, T_INVERSE1left, exp1right), rest671)

end
|  ( 64, ( ( _, ( _, _, T_RPAREN1right)) :: ( _, ( MlyValue.exp exp2,
 _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( _, 
T_MAKERAT1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn
 _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (MAKERAT(exp1,exp2))
end)
 in ( LrTable.NT 16, ( result, T_MAKERAT1left, T_RPAREN1right), 
rest671)
end
|  ( 65, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
T_RATFROMINT1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  (exp as exp1) = exp1 ()
 in (RAT(exp))
end)
 in ( LrTable.NT 16, ( result, T_RATFROMINT1left, exp1right), rest671)

end
|  ( 66, ( ( _, ( MlyValue.T_RATVAL T_RATVAL1, T_RATVAL1left, 
T_RATVAL1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (T_RATVAL as T_RATVAL1) = T_RATVAL1 ()
 in (RATVAL(T_RATVAL))
end)
 in ( LrTable.NT 16, ( result, T_RATVAL1left, T_RATVAL1right), rest671
)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Pi_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun T_COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun T_SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun T_RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun T_INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun T_BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun T_IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.T_IDENT (fn () => i),p1,p2))
fun T_BOOLVAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.T_BOOLVAL (fn () => i),p1,p2))
fun T_AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun T_OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun T_NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun T_LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun T_RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun T_LCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun T_RCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun T_RATADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun T_RATSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun T_RATMUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun T_RATDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun T_INTADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun T_INTSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun T_INTMUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun T_INTDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun T_INTMOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun T_ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun T_CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun T_READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun T_PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun T_WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun T_DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun T_OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun T_IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun T_THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun T_ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun T_FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun T_LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun T_GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun T_LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun T_GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun T_EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun T_NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun T_INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun T_MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun T_RATFROMINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun T_RATVAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.T_RATVAL (fn () => i),p1,p2))
fun T_INTVAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.T_INTVAL (fn () => i),p1,p2))
fun T_ADDINV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun T_PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
end
end
