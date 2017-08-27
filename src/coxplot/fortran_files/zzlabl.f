C
C
C
      SUBROUTINE ZZLABL( VAL , COUT , NCHAR )
C
C  Generate a character string for a label for a linear axis in DRAXES
C.......................................................................
      CHARACTER*1  COUT(*)
      CHARACTER*10 BUF
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( VAL .EQ. 0. )THEN
         BUF = '0'
         NCH = 1
C.......................................................................
C   Intermediate values get an F format.
C
      ELSEIF( ABS(VAL) .GE. 0.01 .AND. ABS(VAL) .LE. 9999.99 )THEN
         WRITE( BUF , 101 ) VAL
101      FORMAT(F9.3)
C
C  Strip off leading blanks
C
         NBOT = 1
100      CONTINUE
            IF( BUF(NBOT:NBOT) .NE. ' ' )GOTO 200
            NBOT = NBOT+1
            IF( NBOT .LT. 9 )GOTO 100
200      CONTINUE
C
C  Strip off trailing zeroes
C
         NTOP = 9
300      CONTINUE
            IF( BUF(NTOP:NTOP) .NE. '0' )GOTO 400
            NTOP = NTOP-1
            IF( NTOP .GT. NBOT )GOTO 300
400      CONTINUE
C
C  Store desired part of string in first part of BUF
C
         NCH = NTOP - NBOT + 1
         BUF(1:NCH) = BUF(NBOT:NTOP)
C.......................................................................
C  Large or small values get an E format.
C
      ELSE
         WRITE( BUF , 301 ) VAL
301      FORMAT(1PE9.2)
         IF( BUF(1:1) .EQ. ' ' )THEN
            BUF(1:8) = BUF(2:9)
            NCH      = 8
         ELSE
            NCH = 9
         ENDIF
      ENDIF
C.......................................................................
      DO 900 N=1,NCH
         COUT(N) = BUF(N:N)
900   CONTINUE
      NCHAR = NCH
C
      RETURN
      END
