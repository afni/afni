C=======================================================================
C  Device independent setup routines:
C
C
C
      SUBROUTINE SET( XOBJ1,XOBJ2 , YOBJ1,YOBJ2 ,
     X                XSUB1,XSUB2 , YSUB1,YSUB2 , LTYPE )
C
C  Set the relationship between the physical space and the user space.
C.......................................................................
      INCLUDE 'plotpak.inc'
C
      INTEGER*2 IXC(4) , IYC(4)
      DATA      IXC / +1 , +1 , -1 , -1 /
      DATA      IYC / +1 , -1 , +1 , -1 /
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Check entry values for reasonableness.
C
      IF( XOBJ1 .LT. XPGMIN  .OR.  XOBJ1 .GE. XOBJ2  .OR.
     X    XOBJ2 .GT. XPGMAX  .OR.  XSUB1 .EQ. XSUB2  .OR.
     X    YOBJ1 .LT. YPGMIN  .OR.  YOBJ1 .GE. YOBJ2  .OR.
     X    YOBJ2 .GT. YPGMAX  .OR.  YSUB1 .EQ. YSUB2  .OR.
     X    LTYPE .LE. 0       .OR.  LTYPE .GT. 4          )GOTO 9000
C.......................................................................
      XBOT = XOBJ1
      YBOT = YOBJ1
      XTOP = XOBJ2
      YTOP = YOBJ2
C
      XMIN = XSUB1
      XMAX = XSUB2
      YMIN = YSUB1
      YMAX = YSUB2
C
      IXCOOR = IXC(LTYPE)
      IYCOOR = IYC(LTYPE)
C
      IF( IXCOOR .GE. 0 )THEN
         SXMIN = XSUB1
         SXMAX = XSUB2
      ELSE
         IF( XSUB1 .LE. 0.  .OR.  XSUB2 .LE. 0. )GOTO 9000
         SXMIN = ALOG10( XSUB1 )
         SXMAX = ALOG10( XSUB2 )
      ENDIF
C
      IF( IYCOOR .GE. 0 )THEN
         SYMIN = YSUB1
         SYMAX = YSUB2
      ELSE
         IF( YSUB1 .LE. 0.  .OR.  YSUB2 .LE. 0. )GOTO 9000
         SYMIN = ALOG10( YSUB1 )
         SYMAX = ALOG10( YSUB2 )
      ENDIF
C
C  Calculate the alpha and beta scaling factors to map user space
C  into physical space.
C
      ALPHXX = ( XTOP - XBOT ) / ( SXMAX - SXMIN )
      BETAXX = XBOT - ALPHXX * SXMIN
C
      ALPHYY = ( YTOP - YBOT ) / ( SYMAX - SYMIN )
      BETAYY = YBOT - ALPHYY * SYMIN
C
      RETURN
C.......................................................................
9000  CONTINUE
CCC      OPEN( 98 , FILE='PLOTPAK.ERR' , STATUS='NEW' )
CCC      WRITE(98,9001) XOBJ1,XOBJ2 , YOBJ1,YOBJ2 ,
CCC     X               XSUB1,XSUB2 , YSUB1,YSUB2 , LTYPE
9001  FORMAT(//
     X ' ********** Illegal parameters in SET **********' /
     X 4(1X,1PG12.5) / 4(1X,1PG12.5) , I6 )
ccc      CLOSE( 98 )
C
      WRITE(*,9001) XOBJ1,XOBJ2 , YOBJ1,YOBJ2 ,
     X              XSUB1,XSUB2 , YSUB1,YSUB2 , LTYPE
      STOP
      END
