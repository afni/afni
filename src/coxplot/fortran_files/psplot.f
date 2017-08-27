C
C
C
      SUBROUTINE PSPLOT( CFL )
C
C  Set the plotter to be PostScript code written to the given file
C
      CHARACTER*(*) CFL
C.......................................................................
      INCLUDE 'plotpak.inc'
C
      LOGICAL LFIRST
      SAVE    LFIRST
      DATA    LFIRST / .TRUE. /
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( .NOT. LFIRST )RETURN
C
      LFIRST = .FALSE.
      XPSCAL = 4096.0
      YPSCAL = 4096.0
      IFLIP  = 2
      NPLOTR = 6
      XPHMAX = 1.3
      YPHMAX = 1.0
      CALL SETFRM( 0.,XPHMAX , 0.,YPHMAX )
C
      CALL ZZPSOP( CFL )
C
      RETURN
      END
