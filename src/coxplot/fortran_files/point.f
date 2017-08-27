C
C
C
      SUBROUTINE POINT( X , Y )
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XX = X
      YY = Y
      CALL ZZPHYS( XX , YY )
      XPHOLD = XX
      YPHOLD = YY
C
      IF( XX .GE. XCLBOT  .AND.  XX .LE. XCLTOP  .AND.
     X    YY .GE. YCLBOT  .AND.  YY .LE. YCLTOP       )
     X
     X  CALL PHDOT( XX ,YY )
c
      CALL PHDOT( XX ,YY )
C
      RETURN
      END
