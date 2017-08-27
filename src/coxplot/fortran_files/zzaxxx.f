C=======================================================================
C  Routines that start with ZZ are internal utility routines:
C
C
C
      SUBROUTINE ZZAXXX( X1 , X2 , Y , ISIDE , ILAB )
C
C  Draw an axis in the x-direction from (X1,Y) to (X2,Y)  [user coords]
C  with the specified divisions and tics.  If ISIDE is positive, the
C  tic marks appear in the +y direction and the labels in the -y
C  direction from the axis.  If ILAB = 0, no labels are drawn.
C.......................................................................
      CHARACTER*10 BUF10
      CHARACTER*1  BUF(10)
      EQUIVALENCE ( BUF10 , BUF(1) )
C
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( X1 .EQ. X2 )GOTO 8000
C
      XV1 = MIN( X1 , X2 )
      XV2 = MAX( X1 , X2 )
C
C  For log x-axis, must push lower value of X down and upper value of
C  X up to powers of 10.
C
      IF( IXCOOR .LT. 0 )THEN
         CALL ZZLGIN( XV1 , XV1 , NL1 )
         TEMP = XV2
         CALL ZZLGIN( TEMP , XV2 , NL2 )
         IF( XV2 .LE. 0.999*TEMP )THEN
            XV2 = 10.*XV2
            NL2 = NL2+1
         ENDIF
         NDEC = NL2 - NL1
         IF( NDEC .LE. 0 )GOTO 8000
      ENDIF
C.......................................................................
C  Convert to physical coordinates and plot axes
C
      XX1 = XV1
      XX2 = XV2
      YY  = Y
      CALL ZZPHYS( XX1 , TEMP )
      CALL ZZPHYS( XX2 , YY   )
C
      IF( IXCOOR .GE. 0 )THEN
         CALL ZZLINX( XX1,XX2 , YY , MAJRX , ISIDE*TMAJX ,
     X                               MINRX , ISIDE*TMINX  )
      ELSE
         CALL ZZLOGX( XX1,XX2 , YY , NDEC , ISIDE*TMAJX , ISIDE*TMINX )
      ENDIF
C.......................................................................
C  Plot labels
C
      IF( ILAB .EQ. 0 )GOTO 8000
C
      IF( IXCOOR .GE. 0 )THEN
         NLAB = MAJRX
      ELSE
         NLAB = NDEC
      ENDIF
C
      DXLAB = (XX2-XX1) / NLAB
      YLAB  = YY - ISIDE * 0.0011 * ISIZX * ( XPGMAX - XPGMIN )
C
      IF( IXCOOR .GE. 0 )DXV = ( XV2 - XV1 ) / NLAB
C
      DO 100 IL=0,NLAB
         IF( IXCOOR .GE. 0 )THEN
            XV = XV1 + IL*DXV
            IF( ABS(XV) .LE. 1.E-5 * MIN(ABS(XV1),ABS(XV2)) )
     X        XV = 0.0
            CALL ZZLABL( XV , BUF , NCHAR )
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
         XX = XX1 + IL*DXLAB
         CALL PWRITF( XX , YLAB , BUF , -NCHAR , ISIZX , 0 , 0 )
100   CONTINUE
C.......................................................................
8000  CONTINUE
      RETURN
      END
