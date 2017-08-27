C
C
C
      SUBROUTINE SETFRM( XOBJ1,XOBJ2 , YOBJ1,YOBJ2 )
C
C  This routine has no analog in NCAR.  It is called once before all
C  plots to set various parameters that define a "frame".
C
C  The entries are the minimum and maximum "physical" x and y values
C  allowed.  In NCAR they would always be 0,1,0,1.  Here they can be
C  anything to allow for various output device aspect ratios.  However,
C  for plots to look reasonable, 1 unit in the x-direction should be the
C  same physical size as 1 unit in the y-direction.
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Set size of physical page.
C
      XPGMIN = XOBJ1
      XPGMAX = XOBJ2
      YPGMIN = YOBJ1
      YPGMAX = YOBJ2
C
C  Initialize the clipping region in case SETW is never called.
C
      XCLBOT = XPGMIN
      XCLTOP = XPGMAX
      YCLBOT = YPGMIN
      YCLTOP = YPGMAX
C
C  Initialize various parameters in case SET is never called.
C  Physical coordinate range:
C
      XBOT = XPGMIN
      XTOP = XPGMAX
      YBOT = YPGMIN
      YTOP = YPGMAX
C
C  User coordinate range:
C
      XMIN = XPGMIN
      XMAX = XPGMAX
      YMIN = YPGMIN
      YMAX = YPGMAX
C
C  Last plotting location (lower left of page):
C
      XPHOLD = XPGMIN
      YPHOLD = YPGMIN
C
C  Axis types (linear):
C
      IXCOOR = +1
      IYCOOR = +1
C
C  Axis scalings from user to physical coordinates:
C
      ALPHXX = 1.0
      ALPHYY = 1.0
      BETAXX = 0.0
      BETAYY = 0.0
C
C  Grid parameters:
C
      TMAJX = 0.010 * ( XPGMAX - XPGMIN )
      TMINX = 0.6 * TMAJX
      TMAJY = TMAJX
      TMINY = TMINX
C
      MAJRX = 5
      MINRX = 10
      MAJRY = 5
      MINRY = 10
      ISIZX = 12
      ISIZY = 12
C
C  Dashed line type (solid)
C
      CALL SETLIN( 1 )
C
      RETURN
      END
