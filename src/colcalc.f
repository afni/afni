      PROGRAM COLCALC
      CHARACTER*8 C_CODE(200,26) , C8
      REAL*8                       R8
      INTEGER     NUM_CODE(26)
      EQUIVALENCE (C8,R8)
      CHARACTER*666 C_EXPR
      REAL*8 R8VAL(26) , ROUT(9) , PAREVAL
      LOGICAL LSTOUT
C
      CHARACTER*8 C_CSTOP(200)
      INTEGER     N_CSTOP
      REAL*8      R_CSTOP
C
      EXTERNAL PAREVAL , INUMC , PARSER
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DO 90 I=1,26
         R8VAL(I) = 0.0D0
90    CONTINUE
C
      NOUT   = 1
      NCMAX  = 0
      IALPHA = ICHAR('A') - 1
C.......................................................................
100   CONTINUE
      WRITE(*,101) NOUT
101   FORMAT(' output',I1,'> '$)
      READ(*,111,END=1000,ERR=1000) C_EXPR
111   FORMAT(A)
      IF( C_EXPR .EQ. ' '    .OR. C_EXPR .EQ. 'end' .OR.
     X    C_EXPR .EQ. 'exit' .OR. C_EXPR .EQ. 'quit'    )GOTO 1000
C
      CALL PARSER( C_EXPR , .TRUE. , NUM_CODE(NOUT) , C_CODE(1,NOUT) )
C
      IF( NUM_CODE(NOUT) .LE. 0 )GOTO 100
C
C  find maximum symbol (column) reference
C
      DO 200 I=1,NUM_CODE(NOUT)-1
         IF( C_CODE(I,NOUT) .EQ. 'PUSHSYM' )THEN
            NCMAX = MAX( NCMAX , ICHAR(C_CODE(I+1,NOUT)(1:1))-IALPHA )
         ENDIF
200   CONTINUE
C
      NOUT = NOUT + 1
      IF( NOUT .LE. 9 )GOTO 100
C----------------------------------------------------------------------
1000  CONTINUE
      NOUT = NOUT - 1
      IF( NOUT .LE. 0 )THEN
        WRITE(*,1001)
1001    FORMAT(' Must enter at least one output column!')
        GOTO 9000
      ENDIF
C
1010  CONTINUE
      NCOL = 0
      WRITE(*,1002) 'input'
1002  FORMAT(' Enter ',A6,' filename           : '$)
      READ(*,111,END=9000,ERR=1010) C_EXPR
      IF( C_EXPR(1:1) .EQ. ' ' )GOTO 1030
C
      OPEN( UNIT=77,FILE=C_EXPR,
     X      FORM='FORMATTED',STATUS='OLD',IOSTAT=IERR)
      IF( IERR .NE. 0 )GOTO 1010
C
C  Find out how many columns of numbers are in this file by
C  reading the first line
C
      READ(77,111) C_EXPR
      REWIND( UNIT=77 )
      NCOL = INUMC( C_EXPR )
ccc      write(*,7707) ncol
ccc7707  format('inumc returns ',I5)
      IF( NCOL .LE. 0 )THEN
         WRITE(*,1019)
1019     FORMAT('*** cannot read numbers from that file ***' )
         CLOSE( UNIT=77 )
         GOTO 1010
      ENDIF
C
      IF( NCMAX .GT. NCOL )THEN
         WRITE(*,1029) NCMAX , NCOL
1029     FORMAT(/
     X     ' *** max column # in expressions =',I3 /
     X     ' ***              in input file  =',I3 /
     X     ' *** trailing columns set to zero ***' /)
      ENDIF
      NCOL = MIN( NCOL , NCMAX )
C
1030  CONTINUE
      IF( NCOL .EQ. 0 )THEN
         WRITE(*,1031)
1031     FORMAT(' OK, enter number of rows to run: '$)
         READ(*,*) NROW
         IF( NROW.LE.0 )GOTO 1030
      ELSE
         NROW = 999999
      ENDIF
C
      WRITE(*,1002) 'output'
      READ(*,111,END=9000,ERR=1030) C_EXPR
C
      LSTOUT = C_EXPR(1:1) .EQ. ' '
C
      IF( .NOT. LSTOUT )THEN
         OPEN( UNIT=78,FILE=C_EXPR,
     X         FORM='FORMATTED',STATUS='NEW',IOSTAT=IERR )
         IF( IERR .NE. 0 )GOTO 1030
      ENDIF
C.....................................................................
1050  CONTINUE
      N_CSTOP = 0
      WRITE(*,1051)
1051  FORMAT(' stopping expression (end < 0) : '$)
      READ(*,111,END=1090,ERR=1090) C_EXPR
      IF( C_EXPR .EQ. ' ' .OR. C_EXPR .EQ. '1' )GOTO 1090
      CALL PARSER( C_EXPR , .TRUE. , N_CSTOP , C_CSTOP )
      IF( N_CSTOP .LE. 0 )GOTO 1050
C.....................................................................
1090  CONTINUE
      IROW      = 0
1100  CONTINUE
      IROW      = IROW + 1
      R8VAL(26) = IROW
      IF( NCOL .GT. 0 )
     X  READ(77,*,END=9000,ERR=9000) ( R8VAL(I) , I=1,NCOL )
C
      DO 1200 I=1,NOUT
         ROUT(I) = PAREVAL( NUM_CODE(I) , C_CODE(1,I) , R8VAL )
1200  CONTINUE
      IF( N_CSTOP .GT. 0 )THEN
         R_CSTOP = PAREVAL( N_CSTOP , C_CSTOP , R8VAL )
         IF( R_CSTOP .LT. 0.D+00 )GOTO 9000
      ENDIF
C
      IF( LSTOUT )THEN
         WRITE(*,1201) ( ROUT(I) , I=1,NOUT )
      ELSE
         WRITE(78,1201) ( ROUT(I) , I=1,NOUT )
      ENDIF
1201  FORMAT(9(1X,1PG20.13))
C
      IF( NCOL.GT.0 .OR. IROW.LT.NROW )GOTO 1100
C.......................................................................
9000  CONTINUE
      END
C
C
C
      FUNCTION INUMC( CLINE )
      IMPLICIT NONE
C
C  Find how many columns there are in the string CLINE
C
      INTEGER INUMC
      CHARACTER*(*) CLINE
C
      INTEGER ITRY , I , IERR
      REAL*8  RVAL(26)
C
      REAL*8      TRAP                 , TOL
      PARAMETER ( TRAP = -987654.3D+21 , TOL = 1.D-11 )
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DO 100 ITRY=1,26
         DO 50 I=1,ITRY
            RVAL(I) = TRAP
50       CONTINUE
         READ(CLINE,*,IOSTAT=IERR) ( RVAL(I) , I=1,ITRY )
         IF( IERR .NE. 0 )GOTO 200
         IF( ABS(RVAL(ITRY)/TRAP-1.D+00) .LE. TOL )GOTO 200
100   CONTINUE
      ITRY = 27
C
200   CONTINUE
      INUMC = ITRY - 1
      RETURN
      END
