C
C
C
      SUBROUTINE MEMPLT( ASPECT )
C
C  Set the plotter to be the "memplot" C routines
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XPSCAL = 1.0
      YPSCAL = 1.0
      IFLIP  = 0
      NPLOTR = 7
      IF( ASPECT .LE. 0.0 )THEN
         XPHMAX = 1.3
      ELSE
         XPHMAX = ASPECT
      ENDIF
      YPHMAX = 1.0
      CALL SETFRM( 0.,XPHMAX , 0.,YPHMAX )
C
      RETURN
      END
