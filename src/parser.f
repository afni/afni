      SUBROUTINE PARSER( C_EXPR , L_PRINT , NUM_CODE , C_CODE )
C
C  Parse the arithmetic expression in C_EXPR.  The code required to
C  evaluate the expression is returned in the first NUM_CODE entries
C  of the CHARACTER*8 array C_CODE. If NUM_CODE is returned as zero,
C  an error occurred. On input, L_PRINT determines whether or not to
C  print error messages.
C
C  Modified 02/17/89 by RWCox from APEVAL subroutine in APFORT, for PC.
C  Modified 06/29/89 by RWCox for Sun Fortran.
C  Modified 04/04/91 by RWCox to fix problem with -x**2 type operations.
C  Modified 11/20/96 by RWCox to try to control errors in evaluation.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IMPLICIT NONE
C
      CHARACTER*(*) C_EXPR
      LOGICAL       L_PRINT
      INTEGER       NUM_CODE
      CHARACTER*8   C_CODE(*)
C
C  Compilation, evaluation, and function stacks.
C
      INTEGER     NUM_CSTACK , NUM_FSTACK
      PARAMETER ( NUM_CSTACK = 2048 , NUM_FSTACK = 40 )
C
      INTEGER   N_CODE(NUM_CSTACK) , NCODE , NEXTCODE ,
     X          N_FUNC(NUM_FSTACK) , NFUNC
C
C  Random local stuff
C
      INTEGER       NLEN , NPOS , IPOS , NTOKEN , NUSED , NF , NARG ,
     X              NERR
      CHARACTER*10000 C_LOCAL
      CHARACTER*30  C_MESSAGE
      CHARACTER*1   C_CH
      REAL*8        VAL_TOKEN , R8_TOKEN
      CHARACTER*8   C8_TOKEN
      EQUIVALENCE   ( R8_TOKEN , C8_TOKEN )
C
      INTEGER     LC_TO_UC , LAST_NONBLANK
C
      PARAMETER ( LC_TO_UC = ICHAR('A') - ICHAR('a') )
C
      INCLUDE 'parser.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      NLEN = LAST_NONBLANK( C_EXPR )
      IF( NLEN .LE. 0 .OR. NLEN .GT. 9999 )THEN
C!no input, or too much
         NUM_CODE = 0
         GOTO 8000
      ENDIF
C
C  Copy input string to local, deleting blanks and converting case.
C
      NPOS = 0
      DO 10 IPOS=1,NLEN
         C_CH = C_EXPR(IPOS:IPOS)
         IF( C_CH .NE. ' ' )THEN
            IF( C_CH .GE. 'a' .AND. C_CH .LE. 'z' )
     X        C_CH = CHAR( ICHAR(C_CH) + LC_TO_UC )
C!convert case
            NPOS               = NPOS+1
            C_LOCAL(NPOS:NPOS) = C_CH
         ENDIF
10    CONTINUE
C!tack 1 blank at the end
      NLEN               = NPOS+1
      C_LOCAL(NLEN:NLEN) = ' '
C.......................................................................
C  This routine parses expressions according to the grammar:
C
C   EXPR  == E9 E8 E6 E4 $
C
C   E4    == <addop> E9 E8 E6 E4 | <null>
C
C   E6    == <mulop> E9 E8 E6 | <null>
C
C   E8    == <expop> E9 E8 | <null>
C
C   E9    == <number> | <function> ( E9 E8 E6 E4 ARGTL )
C            | ( E9 E8 E6 E4 ) | <addop> E9
C
C   ARGTL == , E9 E8 E6 E4 ARGTL | <null>
C
C   <addop>    is + or -
C   <mulop>    is * or /
C   <expop>    is **
C   <number>   is a literal number or a DCL variable
C   <function> is in the list C_FUNCNAME
C
C  The predictive parser described in Aho and Ullman, "Principles of
C  Compiler Design" on pages 185-191 for LL(1) grammars is used here,
C  with additions to perform the evaluation as the parsing proceeds.
C  These consist of adding code (NC_) to the compilation stack when an
C  expansion is made.  When the code is popped off the stack, it is
C  executed.
C
C  02/17/89:  Now, when code is popped off the stack, it is just
C             added to the output code list.
C.......................................................................
C  Prepare to process input string.  Initialize the stacks, etc.
C
C!start scan at 1st character
      NPOS      = 1
C!no function calls yet
      NFUNC     = 0
C!initial compile stack is E9 E8 E6 E4 $
      N_CODE(1) = NN_END
      N_CODE(2) = NN_E4
      N_CODE(3) = NN_E6
      N_CODE(4) = NN_E8
      N_CODE(5) = NN_E9
      NCODE     = 5
      NUM_CODE  = 0
C.......................................................................
C  1000 is the loop back point to process the next token in the input
C  string.
C
1000  CONTINUE
      CALL GET_TOKEN( C_LOCAL(NPOS:NLEN) , NTOKEN , VAL_TOKEN , NUSED )
C
      IF( NTOKEN .EQ. NT_ERROR )THEN
         NERR = 1
         C_MESSAGE = 'Can''t interpret symbol'
         GOTO 9000
C!error exit
      ENDIF
C
C  At 2000, process the next compile code until the token is used up.
C
2000  CONTINUE
      NEXTCODE = N_CODE(NCODE)
C
C  If next entry on the compile stack is an opcode, then apply it to
C  the evaluation stack.
C  02/17/89:  just add it to the output
C
      IF( NEXTCODE .GE. NC_BEG .AND. NEXTCODE .LE. NC_STOP )THEN
         NUM_CODE = NUM_CODE + 1
         CALL EXECUTE( NEXTCODE , C_CODE(NUM_CODE) )
         NCODE = NCODE - 1
C!remove opcode from compile stack
         GOTO 2000
C!loop back for next compile stack entry
      ENDIF
C
C  If next compile stack entry is a token itself, it must match the
C  new token from the input.
C
      IF( NEXTCODE .GE. NT_BEG .AND. NEXTCODE .LE. NT_STOP )THEN
         IF( NEXTCODE .EQ. NTOKEN )THEN
C!a match
            NCODE = NCODE - 1
C!pop token from compile stack
            GOTO 5000
C!loop back for next token
         ENDIF
         NERR = 2
         IF( NEXTCODE .EQ. NT_OPEN )THEN
            C_CH = '('
         ELSEIF( NEXTCODE .EQ. NT_CLOSE )THEN
            C_CH = ')'
         ELSEIF( NEXTCODE .EQ. NT_COMMA )THEN
            C_CH = ','
         ELSE
            C_CH = '?'
         ENDIF
         C_MESSAGE = 'Expected a "' // C_CH // '"'
         GOTO 9000
C!error exit
      ENDIF
C
C  Should have a nonterminal (NN) here.
C
      IF( NEXTCODE .LT. NN_BEG .OR. NEXTCODE .GT. NN_STOP )THEN
         NERR = 3
         C_MESSAGE = 'Internal parser error'
         GOTO 9000
C!error exit
      ENDIF
C
C  Expand the nonterminal appropriately, depending on the token.
C  If no legal expansion, then stop with an error.
C
C  TOKEN = end of string
C
      IF( NTOKEN .EQ. NT_EOS )THEN
         IF( NEXTCODE .EQ. NN_END )THEN
C!end of string = end of expr ==> compilation done
            GOTO 8000
C
         ELSEIF( NEXTCODE .EQ. NN_E8 .OR. NEXTCODE .EQ. NN_E6 .OR.
     X           NEXTCODE .EQ. NN_E4                              )THEN
            NCODE    = NCODE - 1
C!expand this to nothing
            GOTO 2000
C!and try this token again
         ENDIF
         NERR = 4
         C_MESSAGE = 'Unexpected end of input'
         GOTO 9000
C!error exit
      ENDIF
C
C  Check if end of input was expected but not encountered.
C
      IF( NEXTCODE .EQ. NN_END )THEN
         NERR = 15
         C_MESSAGE = 'Expected end of input'
         GOTO 9000
C!error exit
      ENDIF
C
C  TOKEN = number or symbol
C  02/17/89:  added NT_SYMBOL token type;  now, the code for
C             pushing the number or symbol onto the stack is
C             added to the output.
C
      IF( NTOKEN .EQ. NT_NUMBER .OR. NTOKEN .EQ. NT_SYMBOL )THEN
         IF( NEXTCODE .EQ. NN_E9 )THEN
C!only legal time for a number
            IF( NTOKEN .EQ. NT_NUMBER )THEN
               C_CODE(NUM_CODE+1) = 'PUSHNUM'
            ELSE
               C_CODE(NUM_CODE+1) = 'PUSHSYM'
            ENDIF
            R8_TOKEN           = VAL_TOKEN
            C_CODE(NUM_CODE+2) = C8_TOKEN
            NUM_CODE           = NUM_CODE + 2
            NCODE              = NCODE - 1
C!pop E9 from compile stack
            GOTO 5000
C!go to next token
         ENDIF
         NERR = 5
         C_MESSAGE = 'Expected an operator'
         GOTO 9000
C!error exit
      ENDIF
C
C  TOKEN = function call
C
      IF( NTOKEN .EQ. NT_FUNC )THEN
         IF( NEXTCODE .EQ. NN_E9 )THEN
C!only legal time for a function
C
            N_CODE(NCODE+7) = NT_OPEN
C!expand E9 into ( E9 E8 E6 E4 ARGTL ) <func>
            N_CODE(NCODE+6) = NN_E9
            N_CODE(NCODE+5) = NN_E8
            N_CODE(NCODE+4) = NN_E6
            N_CODE(NCODE+3) = NN_E4
            N_CODE(NCODE+2) = NN_ARGTL
            N_CODE(NCODE+1) = NT_CLOSE
            N_CODE(NCODE)   = NC_FUNC + INT(VAL_TOKEN)
            NCODE           = NCODE + 7
C
            NFUNC           = NFUNC + 2
C!setup function stack to check # arguments
            N_FUNC(NFUNC-1) = INT(VAL_TOKEN)
            N_FUNC(NFUNC)   = 0
            GOTO 5000
C!process next token
         ENDIF
         NERR = 6
         C_MESSAGE = 'Expected an operator'
         GOTO 9000
C!error exit
      ENDIF
C
C  TOKEN = addition operator
C
      IF( NTOKEN .EQ. NT_ADDOP )THEN
         IF( NEXTCODE .EQ. NN_E4 )THEN
C!expand E4 into E9 E8 E6 <binary addop> E4
            N_CODE(NCODE+4) = NN_E9
            N_CODE(NCODE+3) = NN_E8
            N_CODE(NCODE+2) = NN_E6
            IF( VAL_TOKEN .EQ. VT_PLUS )THEN
               N_CODE(NCODE+1) = NC_ADD
            ELSE
               N_CODE(NCODE+1) = NC_SUB
            ENDIF
            N_CODE(NCODE)   = NN_E4
            NCODE           = NCODE + 4
            GOTO 5000
C!process next token
C
         ELSEIF( NEXTCODE .EQ. NN_E6 .OR. NEXTCODE .EQ. NN_E8 )THEN
            NCODE = NCODE - 1
C!expand E6 or E8 to null and try again
            GOTO 2000
C
         ELSEIF( NEXTCODE .EQ. NN_E9 )THEN
C!unary + or -
            IF( VAL_TOKEN .EQ. VT_MINUS )THEN
C!expand E9 into E9 E8 <unary minus> if addop is - otherwise leave E9 alone
C [04/04/91 change:
C  used to expand to E9 <unary minus>, which makes -x**2 become (-x)**2]
               N_CODE(NCODE+2) = NN_E9
               N_CODE(NCODE+1) = NN_E8
               N_CODE(NCODE)   = NC_MINUS
               NCODE           = NCODE + 2
            ENDIF
            GOTO 5000
C!process next token
         ENDIF
         NERR = 7
         C_MESSAGE = 'Illegal arithmetic syntax'
         GOTO 9000
C!error exit
      ENDIF
C
C  TOKEN = multiplying operator
C
      IF( NTOKEN .EQ. NT_MULOP )THEN
         IF( NEXTCODE .EQ. NN_E6 )THEN
C!expand E6 into E9 E8 <operator> E6
            N_CODE(NCODE+3) = NN_E9
            N_CODE(NCODE+2) = NN_E8
            IF( VAL_TOKEN .EQ. VT_STAR )THEN
               N_CODE(NCODE+1) = NC_MUL
            ELSE
               N_CODE(NCODE+1) = NC_DIV
            ENDIF
            N_CODE(NCODE)   = NN_E6
            NCODE           = NCODE + 3
            GOTO 5000
C!next token
C
         ELSEIF( NEXTCODE .EQ. NN_E8 )THEN
C!expand E8 to null and try this token again
            NCODE = NCODE - 1
            GOTO 2000
         ENDIF
         NERR = 8
         C_MESSAGE = 'Illegal arithmetic syntax'
         GOTO 9000
C!error exit
      ENDIF
C
C  TOKEN = exponentiation operator
C
      IF( NTOKEN .EQ. NT_EXPOP )THEN
         IF( NEXTCODE .EQ. NN_E8 )THEN
C!expand E8 into E9 E8 <**>
            N_CODE(NCODE+2) = NN_E9
            N_CODE(NCODE+1) = NN_E8
            N_CODE(NCODE)   = NC_POWER
            NCODE           = NCODE + 2
            GOTO 5000
C!process next token
         ENDIF
         NERR = 9
         C_MESSAGE = 'Illegal arithmetic syntax'
         GOTO 9000
C!error exit
      ENDIF
C
C  TOKEN = comma
C
      IF( NTOKEN .EQ. NT_COMMA )THEN
         IF( NEXTCODE .EQ. NN_E4 .OR. NEXTCODE .EQ. NN_E6 .OR.
     X       NEXTCODE .EQ. NN_E8                              )THEN
C
            NCODE = NCODE - 1
C!pop this nonterminal and try this token again
            GOTO 2000
C
         ELSEIF( NEXTCODE .EQ. NN_ARGTL )THEN
C!expand ARGTL into E9 E8 E6 E4 ARGTL
            N_CODE(NCODE+4) = NN_E9
            N_CODE(NCODE+3) = NN_E8
            N_CODE(NCODE+2) = NN_E6
            N_CODE(NCODE+1) = NN_E4
            N_CODE(NCODE)   = NN_ARGTL
            NCODE           = NCODE + 4
C!add 1 to no. of args. encountered, and check if there are too many
            N_FUNC(NFUNC)   = N_FUNC(NFUNC) + 1
            NF              = N_FUNC(NFUNC-1)
            IF( N_FUNCARGS(NF) .LE. N_FUNC(NFUNC) .AND.
     X          N_FUNCARGS(NF) .GT. 0                  )THEN
               NERR = 12
               C_MESSAGE = 'Wrong number of arguments'
               GOTO 9000
C!error exit
            ENDIF
            GOTO 5000
C!process next token
         ENDIF
         NERR = 10
         C_MESSAGE = 'Expected an expression'
         GOTO 9000
C!error exit
      ENDIF
C
C  TOKEN = open parenthesis
C
      IF( NTOKEN .EQ. NT_OPEN )THEN
         IF( NEXTCODE .EQ. NN_E9 )THEN
C!expand E9 into E9 E8 E6 E4 )
            N_CODE(NCODE+4) = NN_E9
            N_CODE(NCODE+3) = NN_E8
            N_CODE(NCODE+2) = NN_E6
            N_CODE(NCODE+1) = NN_E4
            N_CODE(NCODE)   = NT_CLOSE
            NCODE           = NCODE + 4
            GOTO 5000
C!process next token
         ENDIF
         NERR = 11
         C_MESSAGE = 'Expected an operator'
         GOTO 9000
C!error exit
      ENDIF
C
C  TOKEN = close parenthesis
C
      IF( NTOKEN .EQ. NT_CLOSE )THEN
         IF( NEXTCODE .EQ. NN_E4 .OR. NEXTCODE .EQ. NN_E6 .OR.
     X       NEXTCODE .EQ. NN_E8                              )THEN
C
            NCODE = NCODE - 1
C!pop this nonterminal and try this token out on the next one
            GOTO 2000
C
         ELSEIF( NEXTCODE .EQ. NN_ARGTL )THEN
C!end of function call
C
            NARG  = N_FUNC(NFUNC) + 1
C!check # arguments
            NF    = N_FUNC(NFUNC-1)
            NFUNC = NFUNC - 2
            IF( N_FUNCARGS(NF) .LE. 0 )THEN
C!variable # of args ==> push number of args on stack (Feb 1997)
               C_CODE(NUM_CODE+1) = 'PUSHNUM'
               R8_TOKEN           = NARG
               C_CODE(NUM_CODE+2) = C8_TOKEN
               NUM_CODE           = NUM_CODE + 2
            ELSEIF( N_FUNCARGS(NF) .NE. NARG )THEN
C!illegal # of args
               NERR = 12
               C_MESSAGE = 'Wrong number of arguments'
               GOTO 9000
C!error exit
            ENDIF
C
            NCODE = NCODE - 1
C!pop this nonterminal and try to match the ) with the next compile stack entry
            GOTO 2000
         ENDIF
         NERR = 13
         C_MESSAGE = 'Expected an expression'
         GOTO 9000
C!error exit
      ENDIF
      NERR = 14
      C_MESSAGE = 'Internal parser error'
      GOTO 9000
C!error exit
C.......................................................................
C  At 5000, advance to the next token and loop back
C
5000  CONTINUE
      NPOS = NPOS + NUSED
      GOTO 1000
C.......................................................................
C  At 8000, exit
C
8000  CONTINUE
      RETURN
C.......................................................................
C  At 9000, error exit
C
9000  CONTINUE
      IF( L_PRINT )THEN
         IF( NUSED .LT. 1 ) NUSED = 1
         WRITE(*,9001) NERR , C_MESSAGE , C_LOCAL(1:NLEN) ,
     X                 ('.',NF=1,NPOS) , ('#',NF=1,NUSED)
9001     FORMAT(' PARSER error',I4,': ',A/ 1X,A/ 80A1 )
C
CCC         WRITE(*,9002) (N_CODE(NF),NF=NCODE,1,-1)
CCC9002     FORMAT(' Compile stack is (top down)' / 10(1X,I6) )
      ENDIF
C
      NUM_CODE = 0
      RETURN
      END
C
C
C
      SUBROUTINE EXECUTE( N_OPCODE , C_CODE )
C
C  Execute the opcode on the evaluation stack.  Note that no attempt is
C  made to intercept errors, such as divide by zero, ACOS(2), etc.
C
      IMPLICIT NONE
      INTEGER     N_OPCODE
      CHARACTER*8 C_CODE
C
      INCLUDE 'parser.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  Branch to special code for function evaluations
C
      IF( N_OPCODE .GE. NC_FUNC )GOTO 5000
C.......................................................................
      IF( N_OPCODE .EQ. NC_MINUS )THEN
C!unary minus, the only unary op.
         C_CODE = '--'
C
      ELSE
C!a binary operation
         IF( N_OPCODE .EQ. NC_ADD )THEN
C!add
            C_CODE = '+'
         ELSEIF( N_OPCODE .EQ. NC_SUB )THEN
C!subtract
            C_CODE = '-'
         ELSEIF( N_OPCODE .EQ. NC_MUL )THEN
C!multiply
            C_CODE = '*'
         ELSEIF( N_OPCODE .EQ. NC_DIV )THEN
C!divide
            C_CODE = '/'
         ELSEIF( N_OPCODE .EQ. NC_POWER )THEN
C!**
            C_CODE = '**'
         ENDIF
      ENDIF
      GOTO 8000
C.......................................................................
C  Function evaluation
C
5000  CONTINUE
      C_CODE = C_FUNCNAME(N_OPCODE-NC_FUNC)
C.......................................................................
8000  CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE GET_TOKEN( C_INPUT , NTYPE , VALUE , NUSED )
C
C  Return the 1st token in the input stream.
C
      IMPLICIT NONE
      CHARACTER*(*) C_INPUT
      INTEGER       NTYPE , NUSED
      REAL*8        VALUE
C
      INTEGER      NLEN , NPOS , IFUNC , IPOS , IO_CODE
      LOGICAL      L_ALPHABETIC , L_IDCHAR , L_NUMERIC
      CHARACTER*32 C_ID , C_VAL
      CHARACTER*1  C_C  , C_FIRST
C
      REAL*8      R8_VAL
      CHARACTER*8 C8_VAL
      EQUIVALENCE ( R8_VAL , C8_VAL )
C
      INCLUDE 'parser.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Statement function definitions
C
      L_ALPHABETIC(C_C) = C_C .GE. 'A' .AND. C_C .LE. 'Z'
C
      L_IDCHAR(C_C) = ( C_C .GE. 'A' .AND. C_C .LE. 'Z' ) .OR.
     X                ( C_C .GE. '0' .AND. C_C .LE. '9' ) .OR.
     X                  C_C .EQ. '_'                      .OR.
     X                  C_C .EQ. '$'
C
      L_NUMERIC(C_C) = C_C .GE. '0' .AND. C_C .LE. '9'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      NTYPE = NT_EOS
      NUSED = 0
      NLEN  = LEN(C_INPUT)
      IF( NLEN .LE. 0 )GOTO 8000
C
C  Process the simple cases 1st
C
      C_FIRST = C_INPUT(1:1)
C
      IF( C_FIRST .EQ. ' ' )GOTO 8000
C
      NUSED = 1
      IF( C_FIRST .EQ. '+' )THEN
         NTYPE = NT_ADDOP
         VALUE = VT_PLUS
      ELSEIF( C_FIRST .EQ. '-' )THEN
         NTYPE = NT_ADDOP
         VALUE = VT_MINUS
      ELSEIF( C_FIRST .EQ. '/' )THEN
         NTYPE = NT_MULOP
         VALUE = VT_SLASH
      ELSEIF( C_FIRST .EQ. '*' )THEN
         IF( C_INPUT(1:2) .EQ. '**' )THEN
            NTYPE = NT_EXPOP
            VALUE = VT_STARS
            NUSED = 2
         ELSE
            NTYPE = NT_MULOP
            VALUE = VT_STAR
         ENDIF
      ELSEIF( C_FIRST .EQ. '^' )THEN
         NTYPE = NT_EXPOP
         VALUE = VT_STARS
      ELSEIF( C_FIRST .EQ. '(' .OR. C_FIRST .EQ. '[' )THEN
         NTYPE = NT_OPEN
      ELSEIF( C_FIRST .EQ. ')' .OR. C_FIRST .EQ. ']' )THEN
         NTYPE = NT_CLOSE
      ELSEIF( C_FIRST .EQ. ',' )THEN
         NTYPE = NT_COMMA
      ENDIF
C
      IF( NTYPE .NE. NT_EOS )GOTO 8000
C!exit if above was successful
C.......................................................................
C  The only possibilities left are a variable name, a function name,
C  or a number.
C
      IF( L_ALPHABETIC(C_FIRST) )THEN
C!a name
C
         NPOS = 2
110      CONTINUE
         IF( .NOT. L_IDCHAR(C_INPUT(NPOS:NPOS)) )GOTO 120
            NPOS = NPOS+1
            GOTO 110
120      CONTINUE
         NPOS = NPOS-1
         C_ID = C_INPUT(1:NPOS)
C
C  The name is now in C_ID.  Check to see if it is a function name.
C
         IFUNC                  = 1
         C_FUNCNAME(NUM_FUNC+1) = C_ID
210      CONTINUE
         IF( .NOT. ( C_ID .NE. C_FUNCNAME(IFUNC) ) )GOTO 220
            IFUNC = IFUNC + 1
            GOTO 210
220      CONTINUE
         IF( IFUNC .LE. NUM_FUNC )THEN
C!it is a function
            NTYPE = NT_FUNC
            VALUE = IFUNC
            NUSED = NPOS
         ELSEIF( C_ID(1:NPOS) .EQ. 'PI' )THEN
C!symbolic pi
            NTYPE  = NT_NUMBER
            VALUE  = 3.1415 92653 58979 32D+00
            NUSED  = NPOS
         ELSE
C!must be a symbol
            NTYPE  = NT_SYMBOL
            C8_VAL = C_ID(1:NPOS)
            VALUE  = R8_VAL
            NUSED  = NPOS
         ENDIF
C.......................................................................
C  try for a number
C
      ELSEIF( L_NUMERIC(C_FIRST) .OR. C_FIRST .EQ. '.' )THEN
         NPOS = 2
310      CONTINUE
         IF( .NOT. L_NUMERIC(C_INPUT(NPOS:NPOS)) )GOTO 320
C!skip digits
            NPOS = NPOS+1
            GOTO 310
320      CONTINUE
         IF( C_FIRST .NE. '.' .AND. C_INPUT(NPOS:NPOS) .EQ. '.' )THEN
            NPOS = NPOS+1
410         CONTINUE
            IF( .NOT. L_NUMERIC(C_INPUT(NPOS:NPOS)) )GOTO 420
C!skip digits after decimal pt
               NPOS = NPOS+1
               GOTO 410
420         CONTINUE
         ENDIF
C!allow for exponent
         IF( C_INPUT(NPOS:NPOS) .EQ. 'E' .OR.
     X       C_INPUT(NPOS:NPOS) .EQ. 'D'     )THEN
            IPOS = NPOS+1
            IF( C_INPUT(IPOS:IPOS) .EQ. '+' .OR.
     X          C_INPUT(IPOS:IPOS) .EQ. '-'     )IPOS = IPOS+1
            IF( L_NUMERIC(C_INPUT(IPOS:IPOS)) )THEN
C!only if a digit follows the E can it be legal
               NPOS = IPOS
510            CONTINUE
               IF( .NOT. L_NUMERIC(C_INPUT(NPOS:NPOS)) )GOTO 520
                  NPOS = NPOS+1
                  GOTO 510
520            CONTINUE
            ENDIF
         ENDIF
         NPOS  = NPOS-1
C!number runs from position 1 to NPOS
         NUSED = NPOS
         IF( NPOS .LE. 9 )THEN
            WRITE(UNIT=C_VAL,FMT=5501) NPOS
5501        FORMAT('(F',I1,'.0)')
         ELSE
            WRITE(UNIT=C_VAL,FMT=5502) NPOS
5502        FORMAT('(F',I2,'.0)')
         ENDIF
         READ(UNIT=C_INPUT(1:NPOS),FMT=C_VAL,IOSTAT=IO_CODE) VALUE
C
CCC         WRITE(*,5509) C_INPUT(1:NPOS) , C_VAL , VALUE
CCC5509     FORMAT(
CCC     X     ' scanned text ',A/
CCC     X     ' using format ',A/
CCC     X     ' giving VALUE ',1PG14.7)
C
         IF( IO_CODE .EQ. 0 )THEN
            NTYPE = NT_NUMBER
         ELSE
            NTYPE = NT_ERROR
         ENDIF
C.......................................................................
C  If not a number, an error!
C
      ELSE
         NTYPE = NT_ERROR
         NUSED = 1
      ENDIF
C.......................................................................
8000  CONTINUE
      RETURN
      END
C
C
C
C(((....................................................................
      FUNCTION LAST_NONBLANK( CLINE )
C
C  Return the position of the last nonblank character in the input
C  character string.  CLINE is CHARACTER*(*).  Even if CLINE is all
C  blanks, LAST_NONBLANK will be returned as 1 so that operations of the
C  form CLINE(1:LAST_NONBLANK) won't be garbage.
C)))....................................................................
      CHARACTER*(*) CLINE
      INTEGER       LAST_NONBLANK
C
      INTEGER       NPOS
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  Start at the end and work backwards until a nonblank is found.
C  Loop back to 100 to check position # NPOS each time.
C
      NPOS = LEN( CLINE )
100   CONTINUE
C  quit if at the beginning
         IF( NPOS .LE. 1 )GOTO 200
C  quit if not a blank or a null
         IF( CLINE(NPOS:NPOS) .NE. ' '     .AND.
     .       CLINE(NPOS:NPOS) .NE. CHAR(0)       )GOTO 200
C  move back one position and try again
         NPOS = NPOS - 1
      GOTO 100
C.......................................................................
200   CONTINUE
      LAST_NONBLANK = NPOS
      RETURN
      END
C
C
C
      FUNCTION HASSYM( SYM , NUM_CODE , C_CODE )
      IMPLICIT NONE
C
      INTEGER HASSYM
      CHARACTER*8 SYM
      INTEGER     NUM_CODE
      CHARACTER*8 C_CODE(*)
C
      INTEGER NCODE
      CHARACTER*1 SSS
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      HASSYM = 0
      IF( NUM_CODE .LE. 0 ) RETURN
      SSS = SYM(1:1)
C
      DO 1000 NCODE=1,NUM_CODE
         IF( C_CODE(NCODE) .EQ. 'PUSHSYM' )THEN
            IF( C_CODE(NCODE+1)(1:1) .EQ. SSS )THEN
               HASSYM = 1
               RETURN
            ENDIF
         ENDIF
1000  CONTINUE
C
      RETURN
      END
C
C
C
      FUNCTION PAREVAL( NUM_CODE , C_CODE , R8VAL )
      IMPLICIT NONE
C
      REAL*8      PAREVAL
      INTEGER     NUM_CODE
      CHARACTER*8 C_CODE(*)
      REAL*8      R8VAL(*)
C
      INTEGER     NUM_ESTACK
      PARAMETER ( NUM_ESTACK = 128 )
      REAL*8  R8_EVAL(NUM_ESTACK)
C
      INTEGER NEVAL , NCODE , IALPHA , ITM,NTM
      CHARACTER*8 C8_VAL , CNCODE
      REAL*8      R8_VAL , X , Y
      EQUIVALENCE ( C8_VAL , R8_VAL )
C
C  Internal library functions
C
      REAL*8 QG , QGINV , BELL2 , RECT , STEP , BOOL ,
     X       LAND,LOR,LMOFN,MEDIAN , ZTONE , HMODE,LMODE,
     X       GRAN,URAN,IRAN,ERAN,LRAN , ORSTAT , TENT, MAD ,
     X       MEAN , STDEV , SEM , POSVAL , ZZMOD
      REAL*8 ARGMAX,ARGNUM , PAIRMX,PAIRMN , AMONGF, WITHINF
      REAL*8 MINABOVE , MAXBELOW, EXTREME, ABSEXTREME
      REAL*8 CHOOSE , LNCOSH , ACFWXM , GAMP,GAMQ
C
C  External library functions
C
      REAL*8 DAI,DBI , DGAMMA ,
     X       DBESI0,DBESI1 , DBESJ0,DBESJ1 , DBESK0,DBESK1 ,
     X       DBESY0,DBESY1 ,
     X       DERF,DERFC , CDF2ST , ST2CDF
C
C  Statistics functions (01 Mar 1999 - see parser_int.c)
C
      REAL * 8  FICOTP , FICOPT , FICOTZ ,
     X          FITTTP , FITTPT , FITTTZ ,
     X          FIFTTP , FIFTPT , FIFTTZ ,
     X          FIZTTP , FIZTPT , FIZTTZ ,
     X          FICTTP , FICTPT , FICTTZ ,
     X          FIBTTP , FIBTPT , FIBTTZ ,
     X          FIBNTP , FIBNPT , FIBNTZ , HRFBK4   , HRFBK5 ,
     X          FIGTTP , FIGTPT , FIGTTZ , RHDDC2   ,
     X          FIPTTP , FIPTPT , FIPTTZ , LEGENDRE , CBRTFF
C
      REAL*8 R2D , D2R
      PARAMETER ( R2D = 57.29577951308232D+00 ,
     X            D2R =  0.01745329251994D+00  )
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      IF( NUM_CODE .LE. 0 )THEN
         PAREVAL = 0.D+0
         GOTO 8000
      ENDIF
C-----------------------------------------------------------------------
      IALPHA = ICHAR('A') - 1
      NEVAL  = 0
      NCODE  = 0
C
1000  CONTINUE
         NCODE  = NCODE + 1
         CNCODE = C_CODE(NCODE)
C.......................................................................
         IF( CNCODE .EQ. 'PUSHSYM' )THEN
            NEVAL          = NEVAL + 1
            R8_EVAL(NEVAL) = R8VAL( ICHAR(C_CODE(NCODE+1)(1:1))-IALPHA )
            NCODE          = NCODE + 1
C.......................................................................
         ELSEIF( CNCODE .EQ. 'PUSHNUM' )THEN
            NEVAL          = NEVAL + 1
            C8_VAL         = C_CODE(NCODE+1)
            R8_EVAL(NEVAL) = R8_VAL
            NCODE          = NCODE + 1
C.......................................................................
         ELSEIF( CNCODE .EQ. '+' )THEN
            NEVAL          = NEVAL - 1
            R8_EVAL(NEVAL) = R8_EVAL(NEVAL) + R8_EVAL(NEVAL+1)
C.......................................................................
         ELSEIF( CNCODE .EQ. '-' )THEN
            NEVAL          = NEVAL - 1
            R8_EVAL(NEVAL) = R8_EVAL(NEVAL) - R8_EVAL(NEVAL+1)
C.......................................................................
         ELSEIF( CNCODE .EQ. '*' )THEN
            NEVAL          = NEVAL - 1
            R8_EVAL(NEVAL) = R8_EVAL(NEVAL) * R8_EVAL(NEVAL+1)
C.......................................................................
         ELSEIF( CNCODE .EQ. '/' )THEN
            NEVAL          = NEVAL - 1
            IF( R8_EVAL(NEVAL+1) .NE. 0.0D+0 )THEN
              R8_EVAL(NEVAL) = R8_EVAL(NEVAL) / R8_EVAL(NEVAL+1)
            ELSE
              R8_EVAL(NEVAL) = 0.0D+0
            ENDIF
C.......................................................................
         ELSEIF( CNCODE .EQ. '**' )THEN
            NEVAL          = NEVAL - 1
            IF( R8_EVAL(NEVAL) .GT. 0.0D+0            .OR.
     X            (R8_EVAL(NEVAL)   .NE. 0.D+00 .AND.
     X             R8_EVAL(NEVAL+1) .EQ. DINT(R8_EVAL(NEVAL+1))) )
     X
     X        R8_EVAL(NEVAL) = R8_EVAL(NEVAL) ** R8_EVAL(NEVAL+1)
C.......................................................................
         ELSEIF( CNCODE .EQ. '--' )THEN
            R8_EVAL(NEVAL) = -R8_EVAL(NEVAL)
C.......................................................................
         ELSEIF( CNCODE .EQ. 'SIN' )THEN
            R8_EVAL(NEVAL) = SIN( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'SIND' )THEN
            R8_EVAL(NEVAL) = SIN( D2R*R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'COS' )THEN
            R8_EVAL(NEVAL) = COS( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'COSD' )THEN
            R8_EVAL(NEVAL) = COS( D2R*R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'TAN' )THEN
            R8_EVAL(NEVAL) = TAN( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'TAND' )THEN
            R8_EVAL(NEVAL) = TAN( D2R*R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'SQRT' )THEN
            R8_EVAL(NEVAL) = SQRT(ABS(R8_EVAL(NEVAL)))
C.......................................................................
         ELSEIF( CNCODE .EQ. 'CBRT' )THEN
            R8_EVAL(NEVAL) = CBRTFF(R8_EVAL(NEVAL))
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ABS' )THEN
            R8_EVAL(NEVAL) = ABS( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'EXP' )THEN
            R8_EVAL(NEVAL) = EXP( MIN(87.5D+0,R8_EVAL(NEVAL)) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'LOG' )THEN
            IF( R8_EVAL(NEVAL) .NE. 0.D+0 )
     X        R8_EVAL(NEVAL) = LOG(ABS(R8_EVAL(NEVAL)))
C.......................................................................
         ELSEIF( CNCODE .EQ. 'LOG10' )THEN
            IF( R8_EVAL(NEVAL) .NE. 0.D+0 )
     X        R8_EVAL(NEVAL) = LOG10(ABS(R8_EVAL(NEVAL)))
C.......................................................................
         ELSEIF( CNCODE .EQ. 'INT' )THEN
            R8_EVAL(NEVAL) = DINT( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'MAX' )THEN
            NEVAL          = NEVAL - 1
            R8_EVAL(NEVAL) = MAX( R8_EVAL(NEVAL) , R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'MIN' )THEN
            NEVAL          = NEVAL - 1
            R8_EVAL(NEVAL) = MIN( R8_EVAL(NEVAL) , R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ASIN' )THEN
            IF( ABS(R8_EVAL(NEVAL)) .LE. 1.D+0 )
     X        R8_EVAL(NEVAL) = ASIN( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ACOS' )THEN
            IF( ABS(R8_EVAL(NEVAL)) .LE. 1.D+0 )
     X        R8_EVAL(NEVAL) = ACOS( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ATAN' )THEN
            R8_EVAL(NEVAL) = ATAN( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ATAN2')THEN
            NEVAL          = NEVAL - 1
            IF( R8_EVAL(NEVAL).NE.0.D+0.OR.R8_EVAL(NEVAL+1).NE.0.D+0)
     X        R8_EVAL(NEVAL) = ATAN2( R8_EVAL(NEVAL),R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'GRAN')THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = GRAN( R8_EVAL(NEVAL),R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'MOD')THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = ZZMOD( R8_EVAL(NEVAL),R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'URAN' )THEN
            R8_EVAL(NEVAL) = URAN( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'IRAN' )THEN
            R8_EVAL(NEVAL) = IRAN( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ERAN' )THEN
            R8_EVAL(NEVAL) = ERAN( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'LRAN' )THEN
            R8_EVAL(NEVAL) = LRAN( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'PLEG')THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = LEGENDRE( R8_EVAL(NEVAL),R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'HRFBK4')THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = HRFBK4( R8_EVAL(NEVAL),R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'HRFBK5')THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = HRFBK5( R8_EVAL(NEVAL),R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'RHDDC2')THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = RHDDC2( R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                                              R8_EVAL(NEVAL+2) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'SINH' )THEN
            IF( ABS(R8_EVAL(NEVAL)) .LT. 87.5 )
     X        R8_EVAL(NEVAL) = SINH( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'COSH' )THEN
            IF( ABS(R8_EVAL(NEVAL)) .LT. 87.5 )
     X        R8_EVAL(NEVAL) = COSH( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'LOGCOSH' )THEN
            IF( ABS(R8_EVAL(NEVAL)) .LT. 87.5 )
     X        R8_EVAL(NEVAL) = LNCOSH( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ACFWXM')THEN
            NEVAL = NEVAL - 3
            R8_EVAL(NEVAL) = ACFWXM( R8_EVAL(NEVAL)  ,R8_EVAL(NEVAL+1),
     X                               R8_EVAL(NEVAL+2),R8_EVAL(NEVAL+3))
C.......................................................................
         ELSEIF( CNCODE .EQ. 'GAMP')THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = GAMP( R8_EVAL(NEVAL)  ,R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'GAMQ')THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = GAMQ( R8_EVAL(NEVAL)  ,R8_EVAL(NEVAL+1) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'TANH' )THEN
            R8_EVAL(NEVAL) = TANH( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ASINH' )THEN
            X = ABS(R8_EVAL(NEVAL))
            IF( X .LE. 10.D0 )THEN
               Y = X + SQRT(X**2+1.D0)
            ELSE
               Y = X * ( 1.D0 + SQRT(1.D0+(1.D0/X)**2) )
            ENDIF
            Y = LOG(Y)
            IF( R8_EVAL(NEVAL) .LT. 0.D0 )THEN
               R8_EVAL(NEVAL) = -Y
            ELSE
               R8_EVAL(NEVAL) = Y
            ENDIF
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ACOSH' )THEN
            X = R8_EVAL(NEVAL)
            IF( X .GE. 1.D+0 )THEN
               IF( X .LE. 10.D0 )THEN
                  Y = X + SQRT(X**2-1.D0)
               ELSE
                  Y = X * ( 1.D0 + SQRT(1.D0-(1.D0/X)**2) )
               ENDIF
               R8_EVAL(NEVAL) = LOG(Y)
            ENDIF
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ATANH' )THEN
            X = R8_EVAL(NEVAL)
            IF( ABS(X) .LT. 1.D0 )
     X        R8_EVAL(NEVAL) = 0.5D0 * LOG( (1.D0+X)/(1.D0-X) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'AI' )THEN
            R8_EVAL(NEVAL) = DAI( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'BI' )THEN
            R8_EVAL(NEVAL) = DBI( R8_EVAL(NEVAL) , 1 )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ERF' )THEN
            R8_EVAL(NEVAL) = DERF( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'ERFC' )THEN
            R8_EVAL(NEVAL) = DERFC( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'GAMMA' )THEN
            R8_EVAL(NEVAL) = DGAMMA( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'I0' )THEN
            R8_EVAL(NEVAL) = DBESI0( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'I1' )THEN
            R8_EVAL(NEVAL) = DBESI1( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'J0' )THEN
            R8_EVAL(NEVAL) = DBESJ0( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'J1' )THEN
            R8_EVAL(NEVAL) = DBESJ1( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'K0' )THEN
            R8_EVAL(NEVAL) = DBESK0( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'K1' )THEN
            R8_EVAL(NEVAL) = DBESK1( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'Y0' )THEN
            R8_EVAL(NEVAL) = DBESY0( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'Y1' )THEN
            R8_EVAL(NEVAL) = DBESY1( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'QG' )THEN
            R8_EVAL(NEVAL) = QG( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'QGINV' )THEN
            R8_EVAL(NEVAL) = QGINV( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'BELL2' )THEN
            R8_EVAL(NEVAL) = BELL2( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'RECT' )THEN
            R8_EVAL(NEVAL) = RECT( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'STEP' )THEN
            R8_EVAL(NEVAL) = STEP( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'POSVAL' )THEN
            R8_EVAL(NEVAL) = POSVAL( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'TENT' )THEN
            R8_EVAL(NEVAL) = TENT( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'BOOL' )THEN
            R8_EVAL(NEVAL) = BOOL( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'ZTONE' )THEN
            R8_EVAL(NEVAL) = ZTONE( R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'CDF2STAT' )THEN
            NEVAL = NEVAL - 4
            R8_EVAL(NEVAL) = CDF2ST( R8_EVAL(NEVAL)   ,
     X                               R8_EVAL(NEVAL+1) ,
     X                               R8_EVAL(NEVAL+2) ,
     X                               R8_EVAL(NEVAL+3) ,
     X                               R8_EVAL(NEVAL+4)  )
         ELSEIF( CNCODE .EQ. 'STAT2CDF' )THEN
            NEVAL = NEVAL - 4
            R8_EVAL(NEVAL) = ST2CDF( R8_EVAL(NEVAL)   ,
     X                               R8_EVAL(NEVAL+1) ,
     X                               R8_EVAL(NEVAL+2) ,
     X                               R8_EVAL(NEVAL+3) ,
     X                               R8_EVAL(NEVAL+4)  )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'NOTZERO' )THEN
            R8_EVAL(NEVAL) = BOOL( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'ISZERO' .OR. CNCODE .EQ. 'NOT' )THEN
            R8_EVAL(NEVAL) = 1.D+0 - BOOL( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'EQUALS' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = 1.D+0-BOOL(R8_EVAL(NEVAL)-R8_EVAL(NEVAL+1))
         ELSEIF( CNCODE .EQ. 'ISPOSITI' )THEN
            R8_EVAL(NEVAL) = STEP( R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'ISNEGATI' )THEN
            R8_EVAL(NEVAL) = STEP( -R8_EVAL(NEVAL) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'AND'  )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = LAND( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'MEDIAN' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = MEDIAN( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'MAD' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = MAD( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'MEAN' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = MEAN( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'STDEV' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = STDEV( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'SEM' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = SEM( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'ORSTAT' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            NTM   = NTM - 1
            ITM   = R8_EVAL(NEVAL)
            R8_EVAL(NEVAL) = ORSTAT( ITM,NTM , R8_EVAL(NEVAL+1) )
         ELSEIF( CNCODE .EQ. 'HMODE' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = HMODE( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'LMODE' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = LMODE( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'OR'  )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = LOR( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'MOFN'  )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            NTM   = NTM - 1
            ITM   = R8_EVAL(NEVAL)
            R8_EVAL(NEVAL) = LMOFN( ITM,NTM , R8_EVAL(NEVAL+1) )
         ELSEIF( CNCODE .EQ. 'ASTEP' )THEN
            NEVAL = NEVAL - 1
            IF( ABS(R8_EVAL(NEVAL)) .GT. R8_EVAL(NEVAL+1) )THEN
               R8_EVAL(NEVAL) = 1.D+0
            ELSE
               R8_EVAL(NEVAL) = 0.D+0
            ENDIF
         ELSEIF( CNCODE .EQ. 'ARGMAX' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = ARGMAX( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'ARGNUM' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = ARGNUM( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'PAIRMAX' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = PAIRMX( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'PAIRMIN' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = PAIRMN( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'AMONGST' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = AMONGF( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'WITHIN' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = WITHINF( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'MINABOVE' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = MINABOVE( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'MAXBELOW' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = MAXBELOW( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'EXTREME' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = EXTREME( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'ABSEXTREME' )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            R8_EVAL(NEVAL) = ABSEXTREME( NTM , R8_EVAL(NEVAL) )
         ELSEIF( CNCODE .EQ. 'CHOOSE'  )THEN
            NTM   = R8_EVAL(NEVAL)
            NEVAL = NEVAL - NTM
            NTM   = NTM - 1
            ITM   = R8_EVAL(NEVAL)
            R8_EVAL(NEVAL) = CHOOSE( ITM,NTM , R8_EVAL(NEVAL+1) )
         ELSEIF( CNCODE .EQ. 'IFELSE' )THEN
            NEVAL = NEVAL - 2
            IF( R8_EVAL(NEVAL) .NE. 0.D+0 )THEN
                R8_EVAL(NEVAL) = R8_EVAL(NEVAL+1)
            ELSE
                R8_EVAL(NEVAL) = R8_EVAL(NEVAL+2)
            ENDIF
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FICO_T2P' )THEN
            NEVAL = NEVAL - 3
            R8_EVAL(NEVAL) = FICOTP(ABS(R8_EVAL(NEVAL)),
     X                              R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2),R8_EVAL(NEVAL+3) )
         ELSEIF( CNCODE .EQ. 'FICO_P2T' )THEN
            NEVAL = NEVAL - 3
            R8_EVAL(NEVAL) = FICOPT(R8_EVAL(NEVAL)  ,R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2),R8_EVAL(NEVAL+3) )
         ELSEIF( CNCODE .EQ. 'FICO_T2Z' )THEN
            NEVAL = NEVAL - 3
            R8_EVAL(NEVAL) = FICOTZ(R8_EVAL(NEVAL)  ,R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2),R8_EVAL(NEVAL+3) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FITT_T2P' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = FITTTP(ABS(R8_EVAL(NEVAL)),
     X                              R8_EVAL(NEVAL+1))
         ELSEIF( CNCODE .EQ. 'FITT_P2T' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = FITTPT(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1))
         ELSEIF( CNCODE .EQ. 'FITT_T2Z' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = FITTTZ(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1))
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIFT_T2P' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIFTTP(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
         ELSEIF( CNCODE .EQ. 'FIFT_P2T' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIFTPT(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
         ELSEIF( CNCODE .EQ. 'FIFT_T2Z' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIFTTZ(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIZT_T2P' )THEN
            R8_EVAL(NEVAL) = FIZTTP(ABS(R8_EVAL(NEVAL)))
         ELSEIF( CNCODE .EQ. 'FIZT_P2T' )THEN
            R8_EVAL(NEVAL) = FIZTPT(R8_EVAL(NEVAL))
         ELSEIF( CNCODE .EQ. 'FIZT_T2Z' )THEN
            R8_EVAL(NEVAL) = FIZTTZ(R8_EVAL(NEVAL))
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FICT_T2P' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = FICTTP(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1))
         ELSEIF( CNCODE .EQ. 'FICT_P2T' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = FICTPT(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1))
         ELSEIF( CNCODE .EQ. 'FICT_T2Z' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = FICTTZ(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1))
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIBT_T2P' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIBTTP(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
         ELSEIF( CNCODE .EQ. 'FIBT_P2T' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIBTPT(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
         ELSEIF( CNCODE .EQ. 'FIBT_T2Z' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIBTTZ(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIBN_T2P' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIBNTP(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
         ELSEIF( CNCODE .EQ. 'FIBN_P2T' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIBNPT(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
         ELSEIF( CNCODE .EQ. 'FIBN_T2Z' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIBNTZ(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIGT_T2P' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIGTTP(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
         ELSEIF( CNCODE .EQ. 'FIGT_P2T' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIGTPT(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
         ELSEIF( CNCODE .EQ. 'FIGT_T2Z' )THEN
            NEVAL = NEVAL - 2
            R8_EVAL(NEVAL) = FIGTTZ(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1),
     X                              R8_EVAL(NEVAL+2) )
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIPT_T2P' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = FIPTTP(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1))
         ELSEIF( CNCODE .EQ. 'FIPT_P2T' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = FIPTPT(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1))
         ELSEIF( CNCODE .EQ. 'FIPT_T2Z' )THEN
            NEVAL = NEVAL - 1
            R8_EVAL(NEVAL) = FIPTTZ(R8_EVAL(NEVAL),R8_EVAL(NEVAL+1))
C.......................................................................
         ENDIF
C.......................................................................
      IF( NCODE .LT. NUM_CODE )GOTO 1000
      PAREVAL = R8_EVAL(NEVAL)
C-----------------------------------------------------------------------
8000  CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE PAREVEC( NUM_CODE , C_CODE , VA, VB, VC, VD, VE,
     X                    VF, VG, VH, VI, VJ, VK, VL, VM, VN, VO,
     X                    VP, VQ, VR, VS, VT, VU, VV, VW, VX, VY, VZ,
     X                    LVEC, VOUT )
      IMPLICIT NONE
C
C  Vector version of PAREVAL, where VA..VZ with length LVEC
C  are supplied as vectors.
C  [Modified by Raoqiong Tong, August 1997]
C
      INTEGER     NUM_CODE , LVEC
      CHARACTER*8 C_CODE(*)
      INTEGER     NUM_ESTACK       , NVMAX
      PARAMETER ( NUM_ESTACK = 101 , NVMAX = 64 )
      REAL*8      VA(LVEC), VB(LVEC), VC(LVEC), VD(LVEC), VE(LVEC),
     X            VF(LVEC), VG(LVEC), VH(LVEC), VI(LVEC), VJ(LVEC),
     X            VK(LVEC), VL(LVEC), VM(LVEC), VN(LVEC), VO(LVEC),
     X            VP(LVEC), VQ(LVEC), VR(LVEC), VS(LVEC), VT(LVEC),
     X            VU(LVEC), VV(LVEC), VW(LVEC), VX(LVEC), VY(LVEC),
     X            VZ(LVEC), VOUT(LVEC)
C
      REAL*8  R8_EVAL(NVMAX,NUM_ESTACK) , R8VAL(NVMAX,26)
C
      INTEGER     NEVAL , NCODE , IALPHA , IV,IBV,IVBOT,IVTOP ,
     X            JF, NTM,ITM,JTM
      CHARACTER*8 C8_VAL , CNCODE , C2CODE
      REAL*8      R8_VAL , X , Y
      EQUIVALENCE ( C8_VAL , R8_VAL )
C
C  14 Jul 1998: add 1D array for stack copy
C
      REAL*8      SCOP(NUM_ESTACK)
C
C  Internal library functions
C
      REAL*8 QG , QGINV , BELL2 , RECT , STEP , BOOL , LAND,
     X       LOR, LMOFN , MEDIAN , ZTONE , HMODE , LMODE ,
     X       GRAN,URAN,IRAN,ERAN,LRAN , ORSTAT , TENT, MAD ,
     X       MEAN , STDEV , SEM , POSVAL , ZZMOD
      REAL*8 ARGMAX,ARGNUM , PAIRMX,PAIRMN, AMONGF, WITHINF
      REAL*8 MINABOVE , MAXBELOW, EXTREME, ABSEXTREME
      REAL*8 CHOOSE , LNCOSH , ACFWXM , GAMP,GAMQ
C
C  External library functions
C
      REAL*8 DAI,DBI , DGAMMA ,
     X       DBESI0,DBESI1 , DBESJ0,DBESJ1 , DBESK0,DBESK1 ,
     X       DBESY0,DBESY1 ,
     X       DERF,DERFC , CDF2ST , ST2CDF
C
C  Statistics functions (01 Mar 1999 - see parser_int.c)
C
      REAL * 8  FICOTP , FICOPT , FICOTZ ,
     X          FITTTP , FITTPT , FITTTZ ,
     X          FIFTTP , FIFTPT , FIFTTZ ,
     X          FIZTTP , FIZTPT , FIZTTZ ,
     X          FICTTP , FICTPT , FICTTZ ,
     X          FIBTTP , FIBTPT , FIBTTZ ,
     X          FIBNTP , FIBNPT , FIBNTZ , HRFBK4   , HRFBK5 ,
     X          FIGTTP , FIGTPT , FIGTTZ , RHDDC2   ,
     X          FIPTTP , FIPTPT , FIPTTZ , LEGENDRE , CBRTFF
C
      REAL*8 R2D , D2R
      PARAMETER ( R2D = 57.29577951308232D+00 ,
     X            D2R =  0.01745329251994D+00  )
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      IF( NUM_CODE.LE.0 .OR. LVEC.LE.0 )GOTO 8000
C
      IALPHA = ICHAR('A') - 1
C-----------------------------------------------------------------------
      DO 5000 IBV=0,LVEC-1,NVMAX
         IVBOT = IBV + 1
         IVTOP = IBV + NVMAX
         IF( IVTOP .GT. LVEC ) IVTOP = LVEC
C
ccc         WRITE(*,9802) IVBOT,IVTOP
ccc9802     FORMAT('   .. PAREVEC: loop from',I5,' to',I5)
C
         DO 100 IV=IVBOT,IVTOP
             R8VAL(IV-IBV, 1) = VA(IV)
100      CONTINUE
         DO 101 IV=IVBOT,IVTOP
             R8VAL(IV-IBV, 2) = VB(IV)
101      CONTINUE
         DO 102 IV=IVBOT,IVTOP
             R8VAL(IV-IBV, 3) = VC(IV)
102      CONTINUE
         DO 103 IV=IVBOT,IVTOP
             R8VAL(IV-IBV, 4) = VD(IV)
103      CONTINUE
         DO 104 IV=IVBOT,IVTOP
             R8VAL(IV-IBV, 5) = VE(IV)
104      CONTINUE
         DO 105 IV=IVBOT,IVTOP
             R8VAL(IV-IBV, 6) = VF(IV)
105      CONTINUE
         DO 106 IV=IVBOT,IVTOP
             R8VAL(IV-IBV, 7) = VG(IV)
106      CONTINUE
         DO 107 IV=IVBOT,IVTOP
             R8VAL(IV-IBV, 8) = VH(IV)
107      CONTINUE
         DO 108 IV=IVBOT,IVTOP
             R8VAL(IV-IBV, 9) = VI(IV)
108      CONTINUE
         DO 109 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,10) = VJ(IV)
109      CONTINUE
         DO 110 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,11) = VK(IV)
110      CONTINUE
         DO 111 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,12) = VL(IV)
111      CONTINUE
         DO 112 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,13) = VM(IV)
112      CONTINUE
         DO 113 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,14) = VN(IV)
113      CONTINUE
         DO 114 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,15) = VO(IV)
114      CONTINUE
         DO 115 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,16) = VP(IV)
115      CONTINUE
         DO 116 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,17) = VQ(IV)
116      CONTINUE
         DO 117 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,18) = VR(IV)
117      CONTINUE
         DO 118 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,19) = VS(IV)
118      CONTINUE
         DO 119 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,20) = VT(IV)
119      CONTINUE
         DO 120 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,21) = VU(IV)
120      CONTINUE
         DO 121 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,22) = VV(IV)
121      CONTINUE
         DO 122 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,23) = VW(IV)
122      CONTINUE
         DO 123 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,24) = VX(IV)
123      CONTINUE
         DO 124 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,25) = VY(IV)
124      CONTINUE
         DO 125 IV=IVBOT,IVTOP
             R8VAL(IV-IBV,26) = VZ(IV)
125      CONTINUE
C
         NEVAL  = 0
         NCODE  = 0
C
1000     CONTINUE
         NCODE  = NCODE + 1
         CNCODE = C_CODE(NCODE)
ccc         WRITE(*,9803) CNCODE
ccc9803     FORMAT('   .. PAREVEC: opcode=',A)
C.......................................................................
         IF( CNCODE .EQ. 'PUSHSYM' )THEN
            JF = ICHAR(C_CODE(NCODE+1)(1:1))-IALPHA
            IF( NCODE+2 .LE. NUM_CODE )THEN
               C2CODE = C_CODE(NCODE+2)
            ELSE
               C2CODE = 'q'
            ENDIF
            IF( C2CODE .EQ. '+' )THEN
               NCODE = NCODE + 2
               DO IV=IVBOT,IVTOP
                  R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                   + R8VAL(IV-IBV,JF)
               ENDDO
            ELSEIF( C2CODE .EQ. '-' )THEN
               NCODE = NCODE + 2
               DO IV=IVBOT,IVTOP
                  R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                   - R8VAL(IV-IBV,JF)
               ENDDO
            ELSEIF( C2CODE .EQ. '*' )THEN
               NCODE = NCODE + 2
               DO IV=IVBOT,IVTOP
                  R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                   * R8VAL(IV-IBV,JF)
               ENDDO
            ELSEIF( C2CODE .EQ. '/' )THEN
               NCODE = NCODE + 2
               DO IV=IVBOT,IVTOP
                  IF( R8VAL(IV-IBV,JF) .NE. 0.D+00 )THEN
                    R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                     / R8VAL(IV-IBV,JF)
                  ELSE
                    R8_EVAL(IV-IBV,NEVAL) = 0.D+00
                  ENDIF
               ENDDO
            ELSE
               NEVAL = NEVAL + 1
               NCODE = NCODE + 1
               DO IV=IVBOT,IVTOP
                  R8_EVAL(IV-IBV,NEVAL) = R8VAL(IV-IBV,JF)
               ENDDO
            ENDIF
C.......................................................................
         ELSEIF( CNCODE .EQ. 'PUSHNUM' )THEN
            C8_VAL = C_CODE(NCODE+1)
            IF( NCODE+2 .LE. NUM_CODE )THEN
               C2CODE = C_CODE(NCODE+2)
            ELSE
               C2CODE = 'q'
            ENDIF
            IF( C2CODE .EQ. '+' )THEN
               NCODE = NCODE + 2
               DO IV=IVBOT,IVTOP
                  R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                   + R8_VAL
               ENDDO
            ELSEIF( C2CODE .EQ. '-' )THEN
               NCODE = NCODE + 2
               DO IV=IVBOT,IVTOP
                  R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                   - R8_VAL
               ENDDO
            ELSEIF( C2CODE .EQ. '*' )THEN
               NCODE = NCODE + 2
               DO IV=IVBOT,IVTOP
                  R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                   * R8_VAL
               ENDDO
            ELSEIF( C2CODE .EQ. '/' )THEN
               NCODE = NCODE + 2
               IF( R8_VAL .NE. 0.D+00 )THEN
                  R8_VAL = 1.D+00 / R8_VAL
                  DO IV=IVBOT,IVTOP
                     R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                      * R8_VAL
                  ENDDO
               ELSE
                  DO IV=IVBOT,IVTOP
                     R8_EVAL(IV-IBV,NEVAL) =  0.D+00
                  ENDDO
               ENDIF
            ELSE
               NCODE  = NCODE + 1
               NEVAL  = NEVAL + 1
               DO IV=IVBOT,IVTOP
                  R8_EVAL(IV-IBV,NEVAL) = R8_VAL
               ENDDO
            ENDIF
C.......................................................................
         ELSEIF( CNCODE .EQ. '+' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                + R8_EVAL(IV-IBV,NEVAL+1)
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. '-' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                - R8_EVAL(IV-IBV,NEVAL+1)
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. '*' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                * R8_EVAL(IV-IBV,NEVAL+1)
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. '/' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               IF( R8_EVAL(IV-IBV,NEVAL+1) .NE. 0.0D+0 )THEN
                 R8_EVAL(IV-IBV,NEVAL) =  R8_EVAL(IV-IBV,NEVAL)
     X                                  / R8_EVAL(IV-IBV,NEVAL+1)
               ELSE
                 R8_EVAL(IV-IBV,NEVAL) = 0.D+00
               ENDIF
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. '**' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               IF( R8_EVAL(IV-IBV,NEVAL) .GT. 0.0D+0            .OR.
     X               (R8_EVAL(IV-IBV,NEVAL)   .NE. 0.D+00 .AND.
     X                R8_EVAL(IV-IBV,NEVAL+1) .EQ.
     X           DINT(R8_EVAL(IV-IBV,NEVAL+1))                 )     )
     X
     X         R8_EVAL(IV-IBV,NEVAL) =   R8_EVAL(IV-IBV,NEVAL)
     X                                ** R8_EVAL(IV-IBV,NEVAL+1)
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. '--' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = -R8_EVAL(IV-IBV,NEVAL)
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'SIN' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = SIN( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'SIND' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = SIN( D2R*R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'COS' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = COS( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'COSD' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = COS( D2R*R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'TAN' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = TAN( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'TAND' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = TAN( D2R*R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'SQRT' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = SQRT(ABS(R8_EVAL(IV-IBV,NEVAL)))
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'CBRT' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = CBRTFF(R8_EVAL(IV-IBV,NEVAL))
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ABS' )THEN
            DO IV=IVBOT,IVTOP
ccc               WRITE(*,9809) IV
ccc9809           FORMAT('     about to ABS #',I5)
               R8_EVAL(IV-IBV,NEVAL) = ABS( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'EXP' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) =
     X           EXP(MIN(87.5,R8_EVAL(IV-IBV,NEVAL)))
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'LOG' )THEN
            DO IV=IVBOT,IVTOP
               IF( R8_EVAL(IV-IBV,NEVAL) .NE. 0.D+0 )
     X           R8_EVAL(IV-IBV,NEVAL) = LOG(ABS(R8_EVAL(IV-IBV,NEVAL)))
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'LOG10' )THEN
            DO IV=IVBOT,IVTOP
               IF( R8_EVAL(IV-IBV,NEVAL) .NE. 0.D+0 )
     X         R8_EVAL(IV-IBV,NEVAL) = LOG10(ABS(R8_EVAL(IV-IBV,NEVAL)))
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'INT' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DINT( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'MAX' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = MAX( R8_EVAL(IV-IBV,NEVAL) ,
     X                                      R8_EVAL(IV-IBV,NEVAL+1) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'MIN' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = MIN( R8_EVAL(IV-IBV,NEVAL) ,
     X                                      R8_EVAL(IV-IBV,NEVAL+1) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ASIN' )THEN
            DO IV=IVBOT,IVTOP
               IF( ABS(R8_EVAL(IV-IBV,NEVAL)) .LE. 1.D+0 )
     X           R8_EVAL(IV-IBV,NEVAL) = ASIN( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ACOS' )THEN
            DO IV=IVBOT,IVTOP
               IF( ABS(R8_EVAL(IV-IBV,NEVAL)) .LE. 1.D+0 )
     X           R8_EVAL(IV-IBV,NEVAL) = ACOS( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ATAN' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = ATAN( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ATAN2')THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               IF( R8_EVAL(IV-IBV,NEVAL)  .NE.0.D+0 .OR.
     X             R8_EVAL(IV-IBV,NEVAL+1).NE.0.D+0      )
     X         R8_EVAL(IV-IBV,NEVAL) = ATAN2( R8_EVAL(IV-IBV,NEVAL) ,
     X                                        R8_EVAL(IV-IBV,NEVAL+1) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'GRAN')THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = GRAN( R8_EVAL(IV-IBV,NEVAL) ,
     X                                       R8_EVAL(IV-IBV,NEVAL+1) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'MOD')THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = ZZMOD( R8_EVAL(IV-IBV,NEVAL) ,
     X                                        R8_EVAL(IV-IBV,NEVAL+1) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'URAN')THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = URAN( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'IRAN')THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = IRAN( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ERAN')THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = ERAN( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'LRAN')THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = LRAN( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'PLEG')THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = LEGENDRE( R8_EVAL(IV-IBV,NEVAL) ,
     X                                         R8_EVAL(IV-IBV,NEVAL+1) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'HRFBK4')THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = HRFBK4( R8_EVAL(IV-IBV,NEVAL) ,
     X                                         R8_EVAL(IV-IBV,NEVAL+1) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'HRFBK5')THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = HRFBK5( R8_EVAL(IV-IBV,NEVAL) ,
     X                                         R8_EVAL(IV-IBV,NEVAL+1) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'RHDDC2')THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = RHDDC2( R8_EVAL(IV-IBV,NEVAL) ,
     X                                         R8_EVAL(IV-IBV,NEVAL+1) ,
     X                                         R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'SINH' )THEN
            DO IV=IVBOT,IVTOP
              IF( ABS(R8_EVAL(IV-IBV,NEVAL)) .LT. 87.5 )
     X          R8_EVAL(IV-IBV,NEVAL) = SINH( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'COSH' )THEN
            DO IV=IVBOT,IVTOP
              IF( ABS(R8_EVAL(IV-IBV,NEVAL)) .LT. 87.5 )
     X           R8_EVAL(IV-IBV,NEVAL) = COSH( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'LOGCOSH' )THEN
            DO IV=IVBOT,IVTOP
              IF( ABS(R8_EVAL(IV-IBV,NEVAL)) .LT. 87.5 )
     X           R8_EVAL(IV-IBV,NEVAL) = LNCOSH( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ACFWXM')THEN
            NEVAL = NEVAL - 3
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) =
     X         ACFWXM( R8_EVAL(IV-IBV,NEVAL)  ,R8_EVAL(IV-IBV,NEVAL+1),
     X                 R8_EVAL(IV-IBV,NEVAL+2),R8_EVAL(IV-IBV,NEVAL+3))
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'TANH' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = TANH( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ASINH' )THEN
            DO IV=IVBOT,IVTOP
               X = ABS(R8_EVAL(IV-IBV,NEVAL))
               IF( X .LE. 10.D0 )THEN
                  Y = X + SQRT(X**2+1.D0)
               ELSE
                  Y = X * ( 1.D0 + SQRT(1.D0+(1.D0/X)**2) )
               ENDIF
               Y = LOG(Y)
               IF( R8_EVAL(IV-IBV,NEVAL) .LT. 0.D0 )THEN
                  R8_EVAL(IV-IBV,NEVAL) = -Y
               ELSE
                  R8_EVAL(IV-IBV,NEVAL) = Y
               ENDIF
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ACOSH' )THEN
            DO IV=IVBOT,IVTOP
               X = R8_EVAL(IV-IBV,NEVAL)
               IF( X .GE. 1.D0 )THEN
                  IF( X .LE. 10.D0 )THEN
                     Y = X + SQRT(X**2-1.D0)
                  ELSE
                     Y = X * ( 1.D0 + SQRT(1.D0-(1.D0/X)**2) )
                  ENDIF
                  R8_EVAL(IV-IBV,NEVAL) = LOG(Y)
               ENDIF
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ATANH' )THEN
            DO IV=IVBOT,IVTOP
               X = R8_EVAL(IV-IBV,NEVAL)
               IF( ABS(X) .LT. 1.D0 )
     X         R8_EVAL(IV-IBV,NEVAL) = 0.5D0 * LOG( (1.D0+X)/(1.D0-X) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'AI' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DAI( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'BI' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DBI( R8_EVAL(IV-IBV,NEVAL) , 1 )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'ERF' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DERF( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'ERFC' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DERFC( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'GAMMA' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DGAMMA( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'I0' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DBESI0( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'I1' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DBESI1( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'J0' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DBESJ0( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'J1' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DBESJ1( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'K0' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DBESK0( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'K1' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DBESK1( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'Y0' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DBESY0( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'Y1' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = DBESY1( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'QG' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = QG( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'QGINV' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = QGINV( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'BELL2' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = BELL2( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'RECT' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = RECT( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'STEP' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = STEP( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'POSVAL' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = POSVAL( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'TENT' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = TENT( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'BOOL' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = BOOL( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'ZTONE' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = ZTONE( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'CDF2STAT' )THEN
            NEVAL = NEVAL - 4
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = CDF2ST( R8_EVAL(IV-IBV,NEVAL)   ,
     X                                        R8_EVAL(IV-IBV,NEVAL+1) ,
     X                                        R8_EVAL(IV-IBV,NEVAL+2) ,
     X                                        R8_EVAL(IV-IBV,NEVAL+3) ,
     X                                        R8_EVAL(IV-IBV,NEVAL+4)  )
            ENDDO
         ELSEIF( CNCODE .EQ. 'STAT2CDF' )THEN
            NEVAL = NEVAL - 4
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = ST2CDF( R8_EVAL(IV-IBV,NEVAL)   ,
     X                                        R8_EVAL(IV-IBV,NEVAL+1) ,
     X                                        R8_EVAL(IV-IBV,NEVAL+2) ,
     X                                        R8_EVAL(IV-IBV,NEVAL+3) ,
     X                                        R8_EVAL(IV-IBV,NEVAL+4)  )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'NOTZERO' )THEN
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = BOOL( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'ISZERO' .OR. CNCODE .EQ. 'NOT' )THEN
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = 1.D+0-BOOL(R8_EVAL(IV-IBV,NEVAL))
            ENDDO
         ELSEIF( CNCODE .EQ. 'EQUALS' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = 1.D+0
     X              -BOOL(R8_EVAL(IV-IBV,NEVAL)-R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
         ELSEIF( CNCODE .EQ. 'ISPOSITI' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = STEP( R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'ISNEGATI' )THEN
            DO IV=IVBOT,IVTOP
               R8_EVAL(IV-IBV,NEVAL) = STEP( -R8_EVAL(IV-IBV,NEVAL) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'AND'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = LAND( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'MEDIAN'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = MEDIAN( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'MAD'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = MAD( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'MEAN'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = MEAN( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'STDEV'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = STDEV( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'SEM'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = SEM( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'ORSTAT' )THEN
            NTM   = R8_EVAL(1,NEVAL)
            NEVAL = NEVAL - NTM
            NTM   = NTM - 1
            DO IV=IVBOT,IVTOP
               ITM   = R8_EVAL(IV-IBV,NEVAL)
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = ORSTAT(ITM,NTM,SCOP)
            ENDDO
         ELSEIF( CNCODE .EQ. 'HMODE'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = HMODE( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'LMODE'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = LMODE( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'OR'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = LOR( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'MOFN'  )THEN
            NTM   = R8_EVAL(1,NEVAL)
            NEVAL = NEVAL - NTM
            NTM   = NTM - 1
            DO IV=IVBOT,IVTOP
               ITM   = R8_EVAL(IV-IBV,NEVAL)
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = LMOFN(ITM,NTM,SCOP)
            ENDDO
         ELSEIF( CNCODE .EQ. 'ASTEP' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              IF( ABS(R8_EVAL(IV-IBV,NEVAL)) .GT.
     X                R8_EVAL(IV-IBV,NEVAL+1) )THEN
                  R8_EVAL(IV-IBV,NEVAL) = 1.D+0
               ELSE
                  R8_EVAL(IV-IBV,NEVAL) = 0.D+0
               ENDIF
            ENDDO
         ELSEIF( CNCODE .EQ. 'ARGMAX'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = ARGMAX( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'ARGNUM'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = ARGNUM( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'PAIRMAX'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = PAIRMX( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'PAIRMIN'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = PAIRMN( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'AMONGST'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = AMONGF( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'WITHIN'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = WITHINF( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'MINABOVE'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = MINABOVE( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'MAXBELOW'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = MAXBELOW( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'EXTREME'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = EXTREME( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'ABSEXTREME'  )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            DO IV=IVBOT,IVTOP
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM-1)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = ABSEXTREME( NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'CHOOSE' )THEN
            NTM   = R8_EVAL(1, NEVAL)
            NEVAL = NEVAL - NTM
            NTM   = NTM - 1
            DO IV=IVBOT,IVTOP
               ITM = R8_EVAL(IV-IBV,NEVAL)
               DO JTM=1,NTM
                  SCOP(JTM) = R8_EVAL(IV-IBV,NEVAL+JTM)
               ENDDO
               R8_EVAL(IV-IBV,NEVAL) = CHOOSE( ITM, NTM, SCOP )
            ENDDO
         ELSEIF( CNCODE .EQ. 'IFELSE' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
               IF( R8_EVAL(IV-IBV,NEVAL) .NE. 0.D+0 )THEN
                  R8_EVAL(IV-IBV,NEVAL) = R8_EVAL(IV-IBV,NEVAL+1)
               ELSE
                  R8_EVAL(IV-IBV,NEVAL) = R8_EVAL(IV-IBV,NEVAL+2)
               ENDIF
            ENDDO


C.......................................................................
         ELSEIF( CNCODE .EQ. 'FICO_T2P' )THEN
            NEVAL = NEVAL - 3
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FICOTP(ABS(R8_EVAL(IV-IBV,NEVAL)),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2),
     X                                       R8_EVAL(IV-IBV,NEVAL+3) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FICO_P2T' )THEN
            NEVAL = NEVAL - 3
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FICOPT(R8_EVAL(IV-IBV,NEVAL)  ,
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2),
     X                                       R8_EVAL(IV-IBV,NEVAL+3) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FICO_T2Z' )THEN
            NEVAL = NEVAL - 3
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FICOTZ(R8_EVAL(IV-IBV,NEVAL)  ,
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2),
     X                                       R8_EVAL(IV-IBV,NEVAL+3) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FITT_T2P' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FITTTP(ABS(R8_EVAL(IV-IBV,NEVAL)),
     X                                       R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
         ELSEIF( CNCODE .EQ. 'FITT_P2T' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FITTPT(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
         ELSEIF( CNCODE .EQ. 'FITT_T2Z' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FITTTZ(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIFT_T2P' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIFTTP(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                     R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIFT_P2T' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIFTPT(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                     R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIFT_T2Z' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIFTTZ(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                     R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIZT_T2P' )THEN
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIZTTP(ABS(R8_EVAL(IV-IBV,NEVAL)))
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIZT_P2T' )THEN
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIZTPT(R8_EVAL(IV-IBV,NEVAL))
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIZT_T2Z' )THEN
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIZTTZ(R8_EVAL(IV-IBV,NEVAL))
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FICT_T2P' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FICTTP(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
         ELSEIF( CNCODE .EQ. 'FICT_P2T' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FICTPT(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
         ELSEIF( CNCODE .EQ. 'FICT_T2Z' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FICTTZ(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIBT_T2P' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIBTTP(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIBT_P2T' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIBTPT(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIBT_T2Z' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIBTTZ(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIBN_T2P' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIBNTP(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIBN_P2T' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIBNPT(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIBN_T2Z' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIBNTZ(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIGT_T2P' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIGTTP(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIGT_P2T' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIGTPT(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIGT_T2Z' )THEN
            NEVAL = NEVAL - 2
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIGTTZ(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1),
     X                                       R8_EVAL(IV-IBV,NEVAL+2) )
            ENDDO
C.......................................................................
         ELSEIF( CNCODE .EQ. 'FIPT_T2P' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIPTTP(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIPT_P2T' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIPTPT(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
         ELSEIF( CNCODE .EQ. 'FIPT_T2Z' )THEN
            NEVAL = NEVAL - 1
            DO IV=IVBOT,IVTOP
              R8_EVAL(IV-IBV,NEVAL) = FIPTTZ(R8_EVAL(IV-IBV,NEVAL),
     X                                       R8_EVAL(IV-IBV,NEVAL+1))
            ENDDO
C.......................................................................
         ENDIF
C----------------------------------------------------------------------
         IF( NCODE .LT. NUM_CODE )GOTO 1000
C
         DO 4990 IV=IVBOT,IVTOP
            VOUT(IV) = R8_EVAL(IV-IBV,NEVAL)
4990     CONTINUE
C
5000  CONTINUE
C-----------------------------------------------------------------------
8000  CONTINUE
      RETURN
      END
C
C
C
      FUNCTION ZTONE( X )
      IMPLICIT REAL*8 (A-H,O-Z)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      IF( X .LE. 0.D+00 )THEN
         ZTONE = 0.D+00
      ELSEIF( X .GE. 1.0 )THEN
         ZTONE = 1.D+00
      ELSE
         Y = (0.5 * 3.1415926535897932D+00) * (1.6D+00 * X - 0.8D+00)
         ZTONE = 0.50212657D+00 * ( TANH(TAN(Y)) + 0.99576486D+00 )
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION QG( X )
C
C  Compute the reversed cdf of a Gaussian.
C
      IMPLICIT REAL*8 (A-H,O-Z)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      QG = 0.5D+0 * DERFC( X / 1.414213562373095D+0 )
      RETURN
      END
C
C
C
CCC The UNIF() function is now in parser_int.c,
CCC where it calls upon the C library to do the dirty work.
C
CCC      FUNCTION UNIF( XJUNK )
CCC      IMPLICIT REAL*8 (A-H,O-Z)
CCC      PARAMETER ( IA = 99992 , IB = 12345 , IT = 99991 )
CCC      PARAMETER ( F  = 1.00009D-05 )
CCC      DATA IX / 271 /
CCCC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCC      IX = MOD( IA*IX+IB , IT )
CCC      UNIF = F * IX
CCC      RETURN
CCC      END
C
C
C
CCC      FUNCTION UNIF( XJUNK )
CCC      IMPLICIT REAL*8 (A-H,O-Z)
CCCC
CCCC     FACTOR - INTEGER OF THE FORM 8*K+5 AS CLOSE AS POSSIBLE
CCCC              TO  2**26 * (SQRT(5)-1)/2     (GOLDEN SECTION)
CCCC     TWO28  = 2**28  (I.E. 28 SIGNIFICANT BITS FOR DEVIATES)
CCCC
CCC      PARAMETER ( FACTOR = 41475557.0D+00 , TWO28 = 268435456.0D+00 )
CCCC
CCC      DATA R / 0.D+00 /
CCCC
CCCC     RETURNS SAMPLE U FROM THE  0,1 -UNIFORM DISTRIBUTION
CCCC     BY A MULTIPLICATIVE CONGRUENTIAL GENERATOR OF THE FORM
CCCC        R := R * FACTOR (MOD 1) .
CCCC     IN THE FIRST CALL R IS INITIALIZED TO
CCCC        R := IR / 2**28 ,
CCCC     WHERE IR MUST BE OF THE FORM  IR = 4*K+1.
CCCC     THEN R ASSUMES ALL VALUES  0 < (4*K+1)/2**28 < 1 DURING
CCCC     A FULL PERIOD 2**26 OF SUNIF.
CCCC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCC
CCC      IF( R .EQ. 0.D+00 ) R = 4000001.D+00 / TWO28
CCC      R    = DMOD(R*FACTOR,1.0D+00)
CCC      UNIF = R
CCC      RETURN
CCC      END
C
C
C
      FUNCTION IRAN( TOP )
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 IRAN
C
C  Return an integer uniformly distributed among 0..TOP
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IRAN = DINT( (TOP+1.D+00)*UNIF(0.D+00) )
      RETURN
      END
C
C
C
      FUNCTION ERAN( TOP )
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  Return an exponentially distributed deviate: F(x) = 1-exp(-x/top)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
100   U1 = UNIF(0.D+00)
      IF( U1 .LE. 0.D+00 ) GOTO 100
      ERAN = -TOP*LOG(U1)
      RETURN
      END
C
C
C
      FUNCTION LRAN( TOP )
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 LRAN
C
C  Return a logistically distributed deviate: F(x) = 1/[1+exp(-x/top)]
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
100   U1 = UNIF(0.D+00)
      IF( U1 .LE. 0.D+00 .OR. U1 .GE. 1.D+00 ) GOTO 100
      LRAN = TOP*LOG( 1.D+00/U1 - 1.D+00 )
      RETURN
      END
C
C
C
      FUNCTION URAN( X )
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  Return a U(0,X) random variable.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      URAN = X * UNIF(0.D+00)
      RETURN
      END
C
C
C
      FUNCTION GRAN2( B , S )
      IMPLICIT REAL*8 (A-H,O-Z)
C
C  Compute a Gaussian random deviate with mean B and stdev S
C
      INTEGER IP
      DATA    IP / 0 /
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      IF( IP .EQ. 0 )THEN
100      U1 = UNIF(0.D+00)
         IF( U1 .LE. 0.D+00 ) GOTO 100
         U2 = UNIF(0.D+00)
         GRAN2 = B + S * SQRT(-2.0D+00*LOG(U1)) * SIN(6.2831853D+00*U2)
         IP    = 1
      ELSE
         GRAN2 = B + S * SQRT(-2.0D+00*LOG(U1)) * COS(6.2831853D+00*U2)
         IP    = 0
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION GRAN1( B , S )
      IMPLICIT REAL*8 (A-H,O-Z)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      G = -6.D+00 + UNIF(1.D+00) + UNIF(2.D+00) + UNIF(3.D+00)
     X            + UNIF(4.D+00) + UNIF(5.D+00) + UNIF(6.D+00)
     X            + UNIF(7.D+00) + UNIF(8.D+00) + UNIF(9.D+00)
     X            + UNIF(10.D+0) + UNIF(11.D+0) + UNIF(12.D+0)
      GRAN1 = B + S*G
      RETURN
      END
C
C
C
      FUNCTION GRAN( B , S )
      IMPLICIT REAL*8 (A-H,O-Z)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      UU = UNIF(0.0D+00)
      IF( UU .LE. 0.5D+00 )THEN
         GRAN = GRAN1(B,S)
      ELSE
         GRAN = GRAN2(B,S)
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION ZZMOD( A , B )
      IMPLICIT REAL*8 (A-H,O-Z)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      IF( B .NE. 0.0D+00 )THEN
        ZZMOD = A - B * DINT(A/B)
      ELSE
        ZZMOD = 0.0D+00
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION QGINV( P )
C
C  Return x such that Q(x)=P, for 0 < P < 1.  Q=reversed Gaussian cdf.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DP = P
      IF( DP .GT. 0.5D+0 )DP = 1.D+0 - DP
      IF( DP .LE. 0.D+0 )THEN
         DX = 13.D+00
         GOTO 8000
      ENDIF
C
C  Step 1:  use 26.2.23 from Abramowitz and Stegun
C
      DT = SQRT( -2.D+0 * LOG(DP) )
      DX = DT
     X     - ((.010328D+0*DT + .802853D+0)*DT + 2.525517D+0)
     X     /(((.001308D+0*DT + .189269D+0)*DT + 1.432788D+0)*DT + 1.D+0)
C
C  Step 2:  do 3 Newton steps to improve this
C
      DO 100 NEWT=1,3
         DQ  = 0.5D+0 * DERFC( DX / 1.414213562373095D+0 ) - DP
         DDQ = EXP( -0.5D+00 * DX * DX ) / 2.506628274631000D+0
         DX  = DX + DQ / DDQ
100   CONTINUE
C
8000  CONTINUE
      IF( P .GT. 0.5D+00 )THEN
        QGINV = -DX
      ELSE
        QGINV =  DX
      ENDIF
C
      RETURN
      END
C
C
C
      FUNCTION BELL2(X)
      REAL*8 BELL2 , X , AX
C...
      AX = ABS(X)
      IF( AX .LE. 0.5D+00 )THEN
         BELL2 = 1.0D+00 - 1.3333333333333333D+00 * AX*AX
      ELSEIF( AX .LE. 1.5D+00 )THEN
         BELL2 = 0.666666666666667D+00 * (1.5D+00 - AX)**2
      ELSE
         BELL2 = 0.0D+00
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION RECT(X)
      REAL*8 RECT , X , AX
      AX = ABS(X)
      IF( AX .LE. 0.5D+00 )THEN
         RECT = 1.0D+0
      ELSE
         RECT = 0.0D+0
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION STEP(X)
      REAL*8 STEP , X
      IF( X .LE. 0.D+0 )THEN
         STEP = 0.D+0
      ELSE
         STEP = 1.D+0
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION POSVAL(X)
      REAL*8 POSVAL , X
      IF( X .LE. 0.D+0 )THEN
         POSVAL = 0.D+0
      ELSE
         POSVAL = X
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION TENT(X)
      REAL*8 TENT , X , AX
      AX = ABS(X)
      IF( AX .GE. 1.D+0 )THEN
         TENT = 0.D+0
      ELSE
         TENT = 1.D+0 - AX
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION BOOL(X)
      REAL*8 BOOL , X
      IF( X .EQ. 0.D+0 )THEN
         BOOL = 0.D+0
      ELSE
         BOOL = 1.D+0
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION LAND(N,X)
      REAL*8  LAND,X(N)
      INTEGER N , I
      LAND = 0.D+0
      DO 100 I=1,N
         IF( X(I) .EQ. 0.D+0 ) RETURN
100   CONTINUE
      LAND = 1.D+0
      RETURN
      END
C
C
C
      SUBROUTINE BSORT(N,X)
      REAL*8 X(N) , TMP
      INTEGER N , I , IT
C------------------------------------  Bubble sort
50    IT = 0
      DO 100 I=2,N
         IF( X(I-1) .GT. X(I) )THEN
            TMP    = X(I)
            X(I)   = X(I-1)
            X(I-1) = TMP
            IT     = 1
         ENDIF
100   CONTINUE
      IF( IT .NE. 0 )GOTO 50
      RETURN
      END
C
C
C
      FUNCTION ORSTAT(M,N,X)
      REAL*8  ORSTAT,X(N)
      INTEGER M,N , I
C
      IF( N .LE. 1 )THEN
         ORSTAT = X(1)
         RETURN
      ENDIF
C
      I = M
      IF( I .LE. 0 )THEN
         I = 1
      ELSEIF( I .GT. N )THEN
         I = N
      ENDIF
      CALL BSORT(N,X)
      ORSTAT = X(I)
      RETURN
      END
C
C
C
      FUNCTION PAIRMX(N,X)
      REAL*8 PAIRMX, X(N),TT,PP
      INTEGER N,M,I
C
      IF( N .LE. 2 )THEN
        PAIRMX = X(2)
        RETURN
      ENDIF
C
      M  = N/2
      TT = X(1)
      PP = X(M+1)
      DO I=2,M
        IF( X(I) .GT. TT )THEN
          TT = X(I)
          PP = X(M+I)
        ENDIF
      ENDDO
      PAIRMX = PP
      RETURN
      END
C
C
C
      FUNCTION PAIRMN(N,X)
      REAL*8 PAIRMN, X(N),BB,PP
      INTEGER N,M,I
C
      IF( N .LE. 2 )THEN
        PAIRMN = X(2)
        RETURN
      ENDIF
C
      M  = N/2
      BB = X(1)
      PP = X(M+1)
      DO I=2,M
        IF( X(I) .LT. BB )THEN
          BB = X(I)
          PP = X(M+I)
        ENDIF
      ENDDO
      PAIRMN = PP
      RETURN
      END
C
C
C
      FUNCTION AMONGF(N,X)
      REAL*8 AMONGF , X(N)
      INTEGER N,I
      DO I=2,N
        IF( X(1) .EQ. X(I) )THEN
          AMONGF = 1.0D+00
          RETURN
        ENDIF
      ENDDO
      AMONGF = 0.0D+00
      RETURN
      END
C
C
C
      FUNCTION WITHINF(N,X)
      REAL*8 WITHINF , X(N)
      INTEGER N
C
      IF( N .LT. 1 )THEN
        WITHINF = 0.0D+00
        RETURN
      ENDIF
      IF( X(1) .LT. X(2) )THEN
         WITHINF = 0.0D+00
        RETURN
      ENDIF
      IF( X(1) .GT. X(3) )THEN
         WITHINF = 0.0D+00
        RETURN
      ENDIF
      WITHINF = 1.0D+00
      RETURN
      END
C
C
C
      FUNCTION MINABOVE(N,X)
      REAL*8 MINABOVE , X(N) , AAA , BBB
      INTEGER N,I
C
      IF( N .LT. 1 )THEN
        MINABOVE = 0.0D+00
        RETURN
      ENDIF
      AAA = X(1)
      IF( N .EQ. 1 )THEN
        MINABOVE = AAA
        RETURN
      ENDIF
      BBB = 1.0D+38
      DO I=2,N
        IF( X(I) .GT. AAA .AND. X(I) .LT. BBB ) BBB = X(I)
      ENDDO
      IF( BBB .EQ. 1.0D+38 ) BBB = AAA
      MINABOVE = BBB
      RETURN
      END
C
C
C
      FUNCTION MAXBELOW(N,X)
      REAL*8 MAXBELOW , X(N) , AAA , BBB
      INTEGER N,I
C
      IF( N .LT. 1 )THEN
        MAXBELOW = 0.0D+00
        RETURN
      ENDIF
      AAA = X(1)
      IF( N .EQ. 1 )THEN
        MAXBELOW = AAA
        RETURN
      ENDIF
      BBB = -1.0D+38
      DO I=2,N
        IF( X(I) .LT. AAA .AND. X(I) .GT. BBB ) BBB = X(I)
      ENDDO
      IF( BBB .EQ. -1.0D+38 ) BBB = AAA
      MAXBELOW = BBB
      RETURN
      END
C
C
C
      FUNCTION EXTREME(N,X)
      REAL*8 EXTREME , X(N) , AAA , BBB
      INTEGER N,I
C
      IF( N .LT. 1 )THEN
        EXTREME = 0.0D+00
        RETURN
      ENDIF
      AAA = X(1)
      IF( N .EQ. 1 )THEN
        EXTREME = AAA
        RETURN
      ENDIF
      BBB = 0.0
      DO I=1,N
        IF( ABS(X(I)) .GT. BBB ) BBB = X(I)
      ENDDO
      IF( BBB .EQ. 0.0 ) BBB = AAA
      EXTREME = BBB
      RETURN
      END
C
C
C
      FUNCTION ABSEXTREME(N,X)
      REAL*8 ABSEXTREME , X(N) , AAA , BBB
      INTEGER N,I
C
      IF( N .LT. 1 )THEN
        ABSEXTREME = 0.0D+00
        RETURN
      ENDIF
      AAA = X(1)
      IF( N .EQ. 1 )THEN
        ABSEXTREME = AAA
        RETURN
      ENDIF
      BBB = 0.0
      DO I=1,N
        IF( ABS(X(I)) .GT. BBB ) BBB = ABS(X(I))
      ENDDO
      IF( BBB .EQ. 0.0 ) BBB = AAA
      ABSEXTREME = BBB
      RETURN
      END
C
C
C
      FUNCTION CHOOSE(M,N,X)
      REAL*8  CHOOSE,X(N)
      INTEGER M,N , I
C
      IF( M .LT. 1 .OR. N .LT. M)THEN
         CHOOSE = 0.D+0
         RETURN
      ENDIF
      CHOOSE = X(M)
      RETURN
      END
C
C
C
      FUNCTION MEAN(N,X)
      REAL*8 MEAN , X(N) , TMP
      INTEGER N , IT
C
      IF( N .EQ. 1 )THEN
         MEAN = X(1)
         RETURN
      ELSEIF( N .EQ. 2 )THEN
         MEAN = 0.5D+0 * (X(1)+X(2))
         RETURN
      ENDIF
      TMP = 0.0D+0
      DO IT=1,N
        TMP = TMP + X(IT)
      ENDDO
      MEAN = TMP / N
      RETURN
      END
C
C
C
      FUNCTION STDEV(N,X)
      REAL*8 STDEV , X(N) , TMP , XBAR
      INTEGER N , IT
C
      IF( N .EQ. 1 )THEN
         STDEV = 0.0D+0
         RETURN
      ENDIF
      TMP = 0.0D+0
      DO IT=1,N
         TMP = TMP + X(IT)
      ENDDO
      XBAR = TMP / N
      TMP  = 0.D+0
      DO IT=1,N
         TMP = TMP + (X(IT)-XBAR)**2
      ENDDO
      STDEV = SQRT(TMP/(N-1.D+0))
      RETURN
      END
C
C
C
      FUNCTION SEM(N,X)
      REAL*8 SEM , X(N) , STDEV
      INTEGER N
C
      SEM = STDEV(N,X) / SQRT(N+0.000001D+0)
      RETURN
      END
C
C
C
      FUNCTION MEDIAN(N,X)
      REAL*8 MEDIAN , X(N) , TMP
      INTEGER N , IT
C
      IF( N .EQ. 1 )THEN
         MEDIAN = X(1)
         RETURN
      ELSEIF( N .EQ. 2 )THEN
         MEDIAN = 0.5D+00 * (X(1)+X(2))
         RETURN
      ELSEIF( N .EQ. 3 )THEN
         IF( X(1) .GT. X(2) )THEN
            TMP  = X(2)
            X(2) = X(1)
            X(1) = TMP
         ENDIF
         IF( X(1) .GT. X(3) )THEN
            MEDIAN = X(1)
         ELSEIF( X(2) .GT. X(3) )THEN
            MEDIAN = X(3)
         ELSE
            MEDIAN = X(2)
         ENDIF
         RETURN
      ENDIF
C
C---  sort it
C
      CALL BSORT(N,X)
C
C---  Even N --> average of middle 2
C---  Odd  N --> middle 1
C
      IT = N/2
      IF( 2*IT .EQ. N )THEN
         MEDIAN = 0.5D+00 * (X(IT)+X(IT+1))
      ELSE
         MEDIAN = X(IT+1)
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION MAD(N,X)
      REAL*8 MAD , X(N) , TMP , MEDIAN
      INTEGER N , IT
C
      IF( N .EQ. 1 )THEN
         MAD = 0.D+00
         RETURN
      ELSEIF( N .EQ. 2 )THEN
         MAD = 0.5D+00*ABS(X(1)-X(2))
         RETURN
      ENDIF
C
      TMP = MEDIAN(N,X)
      DO 100 IT=1,N
         X(IT) = ABS(X(IT)-TMP)
100   CONTINUE
      MAD = MEDIAN(N,X)
      RETURN
      END
C
C
C
      FUNCTION ARGMAX(N,X)
      REAL*8 ARGMAX , X(N) , TMP
      INTEGER N, I , IT , NZ
C
      TMP = X(1)
      IT  = 1
      NZ  = 0
      IF( TMP .EQ. 0.D+00 ) NZ = 1
      DO 100 I=2,N
        IF( X(I) .GT. TMP )THEN
          IT  = I
          TMP = X(I)
        ENDIF
        IF( X(I) .EQ. 0.D+00 ) NZ = NZ+1
100   CONTINUE
      IF( NZ .EQ. N )THEN
        ARGMAX = 0.D+00
      ELSE
        ARGMAX = IT
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION ARGNUM(N,X)
      REAL*8 ARGNUM , X(N)
      INTEGER N, I, NZ
C
      NZ = 0
      DO 100 I=1,N
        IF( X(I) .NE. 0.D+00 ) NZ = NZ+1
100   CONTINUE
      ARGNUM = NZ
      RETURN
      END
C
C
C
      FUNCTION HMODE(N,X)
      REAL*8 HMODE , VAL , VB , X(N)
      INTEGER N , I , IV  , IB
C
      IF( N .EQ. 1 )THEN
         HMODE = X(1)
         RETURN
      ENDIF
C
      CALL BSORT(N,X)
C
      VAL = X(1)
      IV  = 1
      IB  = 0
      DO 100 I=2,N
         IF( X(I) .NE. VAL )THEN
            IF( IV .GE. IB )THEN
               VB = VAL
               IB = IV
            ENDIF
            VAL = X(I)
            IV  = 1
         ELSE
            IV = IV+1
         ENDIF
100   CONTINUE
      IF( IV .GE. IB ) VB = VAL
      HMODE = VB
      RETURN
      END
C
C
C
      FUNCTION LMODE(N,X)
      REAL*8 LMODE , VAL , VB , X(N)
      INTEGER N , I , IV  , IB
C
      IF( N .EQ. 1 )THEN
         LMODE = X(1)
         RETURN
      ENDIF
C
      CALL BSORT(N,X)
C
      VAL = X(1)
      IV  = 1
      IB  = 0
      DO 100 I=2,N
         IF( X(I) .NE. VAL )THEN
            IF( IV .GT. IB )THEN
               VB = VAL
               IB = IV
            ENDIF
            VAL = X(I)
            IV  = 1
         ELSE
            IV = IV+1
         ENDIF
100   CONTINUE
      IF( IV .GT. IB ) VB = VAL
      LMODE = VB
      RETURN
      END
C
C
C
      FUNCTION LOR(N,X)
      REAL*8  LOR,X(N)
      INTEGER N , I
      LOR = 1.D+0
      DO 100 I=1,N
         IF( X(I) .NE. 0.D+0 ) RETURN
100   CONTINUE
      LOR = 0.D+0
      RETURN
      END
C
C
C
      FUNCTION LMOFN(M,N,X)
      REAL*8  LMOFN,X(N)
      INTEGER M,N , I,C
      C = 0
      DO 100 I=1,N
         IF( X(I) .NE. 0.D+0 ) C = C + 1
100   CONTINUE
      IF( C .GE. M )THEN
         LMOFN = 1.D+0
      ELSE
         LMOFN = 0.D+0
      ENDIF
      RETURN
      END
C
C
C
      FUNCTION LNCOSH(X)
      REAL*8 LNCOSH , X , AX
      AX = ABS(X)
      LNCOSH = AX + LOG(0.5D+0 + 0.5D+0*EXP(-2.D+0*AX))
      RETURN
      END
C
C
C
      REAL*8 FUNCTION  DAI( X )
      REAL*8 X
      CALL QQQERR
      DAI = 0.D+0
      RETURN
      END
      REAL*8 FUNCTION  DBI( X , I )
      REAL*8 X
      INTEGER I
      CALL QQQERR
      DBI = 0.D+0
      RETURN
      END
ccc      REAL*8 FUNCTION  DGAMMA( X )
ccc      REAL*8 X
ccc      CALL QQQERR
ccc      DGAMMA = 0.D+0
ccc      RETURN
ccc      END
      REAL*8 FUNCTION  DBESI0( X )
      REAL*8 X
      CALL QQQERR
      DBESI0 = 0.D+0
      RETURN
      END
      REAL*8 FUNCTION  DBESI1( X )
      REAL*8 X
      CALL QQQERR
      DBESI1 = 0.D+0
      RETURN
      END
ccc      REAL*8 FUNCTION  DBESJ0( X )
ccc      REAL*8 X
ccc      CALL QQQERR
ccc      END
ccc      REAL*8 FUNCTION  DBESJ1( X )
ccc      REAL*8 X
ccc      CALL QQQERR
ccc      END
      REAL*8 FUNCTION  DBESK0( X )
      REAL*8 X
      CALL QQQERR
      DBESK0 = 0.D+0
      RETURN
      END
      REAL*8 FUNCTION  DBESK1( X )
      REAL*8 X
      CALL QQQERR
      DBESK1 = 0.D+0
      RETURN
      END
ccc      REAL*8 FUNCTION  DBESY0( X )
ccc      REAL*8 X
ccc      CALL QQQERR
ccc      END
ccc      REAL*8 FUNCTION  DBESY1( X )
ccc      REAL*8 X
ccc      CALL QQQERR
ccc      END
ccc      REAL*8 FUNCTION  DERF( X )
ccc      REAL*8 X
ccc      CALL QQQERR
ccc      END
ccc      REAL*8 FUNCTION  DERFC( X )
ccc      REAL*8 X
ccc      CALL QQQERR
ccc      END
C
      SUBROUTINE QQQERR
      WRITE(*,999)
999   FORMAT('*** PARSER: unimplemented function ***')
      RETURN
      END
