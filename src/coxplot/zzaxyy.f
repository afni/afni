C
C
C
      SUBROUTINE ZZAXYY( X , Y1 , Y2 , ISIDE , ILAB )
C
C  Draw an axis in the y-direction from (X,Y1) to (X,Y2)  [user coords]
C  with the specified divisions and tics.  If ISIDE is positive, the
C  tic marks appear in the +x direction and the labels in the -x
C  direction from the axis.  If ILAB = 0, no labels are drawn.
C.......................................................................
      CHARACTER*10 BUF10
      CHARACTER*1  BUF(10)
      EQUIVALENCE ( BUF10 , BUF(1) )
C
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( Y1 .EQ. Y2 )GOTO 8000
C
      YV1 = MIN( Y1 , Y2 )
      YV2 = MAX( Y1 , Y2 )
C
C  For log y-axis, must push lower value of Y down and upper value of
C  Y up to powers of 10.
C
      IF( IYCOOR .LT. 0 )THEN
         CALL ZZLGIN( YV1 , YV1 , NL1 )
         TEMP = YV2
         CALL ZZLGIN( TEMP , YV2 , NL2 )
         IF( YV2 .LE. 0.999*TEMP )THEN
            YV2 = 10.*YV2
            NL2 = NL2+1
         ENDIF
         NDEC = NL2 - NL1
         IF( NDEC .LE. 0 )GOTO 8000
      ENDIF
C.......................................................................
C  Convert to physical coordinates and plot axes
C
      YY1 = YV1
      YY2 = YV2
      XX  = X
      CALL ZZPHYS( TEMP , YY1 )
      CALL ZZPHYS( XX   , YY2 )
C
      IF( IYCOOR .GE. 0 )THEN
         CALL ZZLINY( XX , YY1,YY2 , MAJRY , ISIDE*TMAJY ,
     X                               MINRY , ISIDE*TMINY  )
      ELSE
         CALL ZZLOGY( XX , YY1,YY2 , NDEC , ISIDE*TMAJY , ISIDE*TMINY )
      ENDIF
C.......................................................................
C  Plot labels
C
      IF( ILAB .EQ. 0 )GOTO 8000
C
      IF( IYCOOR .GE. 0 )THEN
         NLAB = MAJRY
      ELSE
         NLAB = NDEC
      ENDIF
C
C  Calculate the max number of characters needed for labels into NSHIFT.
C
      IF( IYCOOR .LT. 0 )THEN
C  Log-axis:  1.E+x or 1.E+xx are the possibilities
         NPOWER = MAX( ABS(NL1) , ABS(NL2) )
         IF( NPOWER .LT. 10 )THEN
            NSHIFT = 5
         ELSE
            NSHIFT = 6
         ENDIF
      ELSE
C  Linear-axis:  calculate all labels and find the longest
         NSHIFT = 1
         DYV    = ( YV2 - YV1 ) / NLAB
         DO 50 IL=0,NLAB
            YV = YV1 + IL*DYV
            IF( ABS(YV) .LE. 1.E-5 * MIN(ABS(YV1),ABS(YV2)) )
     X        YV = 0.0
            CALL ZZLABL( YV , BUF , NCHAR )
            NSHIFT = MAX( NSHIFT , NCHAR )
50       CONTINUE
      ENDIF
C
      DYLAB = (YY2-YY1) / NLAB
      CSIZE = 0.0011 * ISIZY * ( XPGMAX - XPGMIN )
      XLAB  = XX - CSIZE * ISIDE * NSHIFT
      DYCSZ = 0.5 * CSIZE
      IF( DYLAB .LT. 0. )DYCSZ = -DYCSZ
C
      DO 100 IL=0,NLAB
         IF( IYCOOR .GE. 0 )THEN
            YV = YV1 + IL*DYV
            IF( ABS(YV) .LE. 1.E-5 * MIN(ABS(YV1),ABS(YV2)) )
     X        YV = 0.0
            CALL ZZLABL( YV , BUF , NCHAR )
         ELSE
            NPOWER = NL1 + IL
            IF( ABS(NPOWER) .LT. 10 )THEN
               WRITE( BUF10 , 101 ) NPOWER
101            FORMAT('1.E',I2)
               NCHAR = 5
            ELSE
               WRITE( BUF10 , 102 ) NPOWER
102            FORMAT('1.E',I3)
               NCHAR = 6
            ENDIF
            IF( BUF(4) .EQ. ' ' )BUF(4) = '+'
         ENDIF
         IF( IL .EQ. 0 )THEN
            YY = YY1 + DYCSZ
         ELSE
            YY = YY1 + IL*DYLAB
            IF( IL .EQ. NLAB )YY = YY - DYCSZ
         ENDIF
         CALL PWRITF( XLAB , YY , BUF , -NCHAR , ISIZY , 0 , -ISIDE )
100   CONTINUE
C.......................................................................
8000  CONTINUE
      RETURN
      END
