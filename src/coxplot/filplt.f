C
C
C
      SUBROUTINE FILPLT( CFL )
C
C  Set the plotter to be echo of line coordinates to the file specified.
C
      CHARACTER*(*) CFL
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XPSCAL = 1.0
      YPSCAL = 1.0
      IFLIP  = 0
      NPLOTR = 3
      XPHMAX = 1.3
      YPHMAX = 1.0
      CALL SETFRM( 0.,XPHMAX , 0.,YPHMAX )
C
      CFILE = CFL
      OPEN( UNIT=99,FILE=CFILE,FORM='FORMATTED',STATUS='UNKNOWN' )
      REWIND( UNIT=99 )
C
      RETURN
      END
