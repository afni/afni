      PROGRAM COLFIT
      IMPLICIT NONE
C
C  Program to do a nonlinear least squares fit of a two-column file in
C  the format
C
C    x1  y1
C    x2  y2
C    ..  ..
C    xN  yN
C
C  to a formula of the form  y(x) = expression(x,a,b,c,...), where
C  a, b, c, ... are parameters to be found.
C-----------------------------------------------------------------------
C  Common block XYDATA holds the data read from disk
C
      INTEGER     NPMAX
      PARAMETER ( NPMAX = 32768 )
      INTEGER NUMPTS
      REAL*8  XX(NPMAX) , YY(NPMAX)
      COMMON /XYDATA/ XX , YY , NUMPTS
C.......................................................................
C  Common block EXDATA holds the parsed expression for evaluation
C
      INTEGER     NCMAX
      PARAMETER ( NCMAX = 512 )
      CHARACTER*8 CCODE(NCMAX)
      INTEGER     NUMCOD , IVEVAR,NUMPAR,IFXVAR(26)
      COMMON /EXDATA/ CCODE , NUMCOD , IVEVAR,NUMPAR,IFXVAR
C.......................................................................
C  local variables
C
      CHARACTER*64  CFILE
      CHARACTER*128 CEXPR
      CHARACTER*1   CHANS
C
      INTEGER I , IERR , ICH , NUMCH , IUPPER,ILOWER ,
     X        IOPT,M,N,NPRINT,INFO , ISTART,IEND
      REAL*8  XIN , YIN , TOL
      LOGICAL LCHUSE(26)
C
      INTEGER     NWORK
      PARAMETER ( NWORK = 30*NPMAX )
      INTEGER IWORK(66)
      REAL*8  WORK(NPMAX,30) , XPAR(66) , FVEC(NPMAX) ,
     X        WAR1(30),WAR2(30),WAR3(30),WAR4(NPMAX)
C
      EXTERNAL FF , DNLS1E , DCOV
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C  Open data file
C
100   CONTINUE
         WRITE(*,101)
101      FORMAT(' Enter data file name : '$)
         READ(*,111) CFILE
111      FORMAT(A)
         IF( CFILE(1:1) .EQ. ' ' )GOTO 9900
C
         OPEN( UNIT = 1 ,
     X         FILE = CFILE ,
     X         FORM = 'FORMATTED' ,
     X       STATUS = 'OLD' ,
     X       IOSTAT = IERR             )
C
         IF( IERR .NE. 0 )THEN
            CLOSE( UNIT = 1 )
            GOTO 100
         ENDIF
C.......................................................................
C  Read data in
C
      WRITE(*,102)
102   FORMAT(' Start, stop indices in data file  [0,0=all] ? '$)
      READ(*,112) ISTART,IEND
112   FORMAT(2I10)
C
      DO 110 I=1,ISTART-1
         READ(1,*,IOSTAT=IERR) XIN,YIN
         IF( IERR .NE. 0 )THEN
            WRITE(*,119) I
119         FORMAT('   *** failure when skipping record ',I5)
            GOTO 9900
         ENDIF
110   CONTINUE
C
      IF( IEND .LE. ISTART )THEN
        IEND = NPMAX
      ELSE
        IEND = MIN( NPMAX , IEND-ISTART+1 )
      ENDIF
C
      I = 0
200   CONTINUE
         READ(1,*,IOSTAT=IERR) XIN,YIN
         IF( IERR .EQ. 0 )THEN
            I     = I + 1
            XX(I) = XIN
            YY(I) = YIN
            IF( I .LT. IEND )GOTO 200
         ENDIF
C
      CLOSE( UNIT = 1 )
      NUMPTS = I
      WRITE(*,201) NUMPTS
201   FORMAT('   --- read',I6,' data lines from file')
      IF( NUMPTS .LE. 1 )GOTO 9900
C.......................................................................
C  Read in expression that will be used to fit
C
300   CONTINUE
         WRITE(*,301)
301      FORMAT(/' Expression to fit 2nd column to 1st ? '$)
         READ(*,111,IOSTAT=IERR) CEXPR
         IF( IERR .NE. 0 )GOTO 9900
         CALL PARSER( CEXPR , .TRUE. , NUMCOD , CCODE )
         IF( NUMCOD .LE. 0 )GOTO 300
         IF( NUMCOD .GT. NCMAX )THEN
            WRITE(*,309)
309         FORMAT('   *** expression too complex!' )
            GOTO 9900
         ENDIF
C.......................................................................
C  Get the names of all the variables referred to in the expression
C
      DO 310 I=1,26
         LCHUSE(I) = .FALSE.
310   CONTINUE
C
      IUPPER = ICHAR('A') - 1
      ILOWER = ICHAR('a') - 1
      DO 320 I=1,NUMCOD
         IF( CCODE(I) .EQ. 'PUSHSYM' )THEN
            ICH         = ICHAR(CCODE(I+1)(1:1)) - IUPPER
            LCHUSE(ICH) = .TRUE.
         ENDIF
320   CONTINUE
C
      CFILE = ' '
      NUMCH = 0
      DO 330 I=1,26
         IF( LCHUSE(I) )THEN
            NUMCH = NUMCH + 1
            CFILE(2*NUMCH:2*NUMCH+1) = CHAR(I+IUPPER) // ' '
         ENDIF
330   CONTINUE
C
      IF( NUMCH .LE. 1 )THEN
         WRITE(*,338)
338      FORMAT('   *** not enough variable names in expression!')
         GOTO 300
      ENDIF
      IF( NUMCH .GT. NUMPTS )THEN
         WRITE(*,339)
339      FORMAT('   *** too many variable names in expression!')
         GOTO 300
      ENDIF
C.......................................................................
C  Find out which variable is the 1st data column
C
340   CONTINUE
         WRITE(*,341) CFILE(1:2*NUMCH)
341      FORMAT(
     X    ' Which variable represents 1st data column ? ' / 1X,A,' : '$)
         READ(*,111) CHANS
         IF( CHANS.GE.'A' .AND. CHANS.LE.'Z' )THEN
            ICH = ICHAR(CHANS) - IUPPER
         ELSEIF( CHANS.GE.'a' .AND. CHANS.LE.'z' )THEN
            ICH = ICHAR(CHANS) - ILOWER
         ELSE
            GOTO 340
         ENDIF
         IF( .NOT. LCHUSE(ICH) )GOTO 340
C.......................................................................
C  Assign 1st column variable name to VEctor VARiable, and
C  all others to be FiXed Variables;  get initial values for the latter
C
      IVEVAR = ICH
      NUMPAR = NUMCH - 1
      ICH    = 0
      DO 360 I=1,26
         IF( LCHUSE(I) .AND. I.NE.IVEVAR )THEN
            ICH = ICH + 1
            IFXVAR(ICH) = I
C
350         CONTINUE
               WRITE(*,351) CHAR(I+IUPPER)
351            FORMAT('  Initial value for parameter ',A,' ? '$)
               READ(*,*,IOSTAT=IERR) XPAR(ICH)
               IF( IERR .NE. 0 )GOTO 350
         ENDIF
360   CONTINUE
C.......................................................................
C  Call the fitting routine
C
      IOPT   = 1
      M      = NUMPTS
      N      = NUMPAR
      TOL    = 1.D-06
      NPRINT = 0
C
C     ************
      CALL DNLS1E( FF,IOPT , M,N , XPAR,FVEC , TOL,NPRINT , INFO ,
     X                                             IWORK,WORK,NWORK )
C     ************
C
C  Write results out:
C
      WRITE(*,501) 'DNLS1E' , INFO
501   FORMAT('   --- ',A,' exits with INFO =',I2)
      IF( INFO .EQ. 0 )GOTO 9900
C
      CALL DCOV( FF,IOPT , M,N , XPAR,FVEC , WORK,NPMAX , INFO ,
     X                                       WAR1,WAR2,WAR3,WAR4 )
C
      WRITE(*,501) 'DCOV' , INFO
      IF( INFO .EQ. 0 )GOTO 9900
C
      DO 510 I=1,NUMPAR
         IF( INFO .EQ. 1 )THEN
            XIN = SQRT( WORK(I,I) )
            WRITE(*,511) CHAR( IFXVAR(I)+IUPPER ) , XPAR(I) , XIN
511         FORMAT(3X,A,' -> ',1PG14.7,' +/- ',1PG14.7)
         ELSE
            WRITE(*,511) CHAR( IFXVAR(I)+IUPPER ) , XPAR(I)
         ENDIF
510   CONTINUE
C
      XIN = 0.D+00
      DO 520 I=1,NUMPTS
         XIN = XIN + ABS( FVEC(I) )
520   CONTINUE
      XIN = XIN / NUMPTS
      WRITE(*,521) XIN
521   FORMAT('   --- mean absolute deviation =',1PG10.3)
C.......................................................................
C  Write fit error curve to a file (if desired)
C
600   CONTINUE
      WRITE(*,601)
601   FORMAT(' Filename to save fit error curve in (blank=none) ? '$)
      READ(*,111) CFILE
      IF( CFILE(1:1) .NE. ' ' )THEN
         OPEN( UNIT = 1 ,
     X         FILE = CFILE ,
     X         FORM = 'FORMATTED' ,
     X       STATUS = 'UNKNOWN' ,
     X       IOSTAT = IERR        )
         IF( IERR .NE. 0 )GOTO 600
C
         DO 610 I=1,NUMPTS
            WRITE(1,611) XX(I) , FVEC(I)
611         FORMAT(1PG20.13,1X,1PG20.13)
610      CONTINUE
C
         CLOSE( UNIT = 1 )
      ENDIF
C-----------------------------------------------------------------------
9900  CONTINUE
      END
C
C
C
      SUBROUTINE FF( IFLAG , M , N , X , FVEC , FJAC , LDFJAC )
      IMPLICIT NONE
C
C  Routine supplied to DNLS1E to compute the functions we are
C  are trying to fit.
C
      INTEGER IFLAG , LDFJAC , M , N
      REAL*8  X(N) , FVEC(M) , FJAC
C.......................................................................
C  Common block XYDATA holds the data read from disk
C
      INTEGER     NPMAX
      PARAMETER ( NPMAX = 32768 )
      INTEGER NUMPTS
      REAL*8  XX(NPMAX) , YY(NPMAX)
      COMMON /XYDATA/ XX , YY , NUMPTS
C.......................................................................
C  Common block EXDATA holds the parsed expression for evaluation
C
      INTEGER     NCMAX
      PARAMETER ( NCMAX = 512 )
      CHARACTER*8 CCODE(NCMAX)
      INTEGER     NUMCOD , IVEVAR,NUMPAR,IFXVAR(26)
      COMMON /EXDATA/ CCODE , NUMCOD , IVEVAR,NUMPAR,IFXVAR
C.......................................................................
C  Local variables
C
      INTEGER I
      REAL*8  FIXVAR(26)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      IF( IFLAG .EQ. 0 )THEN
         WRITE(*,101) X
101      FORMAT(' --- FF X=',5(1X,1PG12.5) )
C.......................................................................
      ELSEIF( IFLAG .EQ. 1 )THEN
         DO 110 I=1,N
            FIXVAR( IFXVAR(I) ) = X(I)
110      CONTINUE
         CALL PAREVEC( NUMCOD , CCODE , FIXVAR ,
     X                 1,IVEVAR , M,M , XX , FVEC )
         DO 120 I=1,M
            FVEC(I) = FVEC(I) - YY(I)
120      CONTINUE
C.......................................................................
      ELSE
         WRITE(*,999) IFLAG
999      FORMAT('   *** FF has illegal IFLAG=',I5)
         STOP
      ENDIF
C
      RETURN
      END
