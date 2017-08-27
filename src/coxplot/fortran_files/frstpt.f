C
C
C
      SUBROUTINE FRSTPT( X , Y )
C
C  Set the first point ("pen up move")
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XX = X
      YY = Y
      CALL ZZPHYS( XX , YY )
      XPHOLD = XX
      YPHOLD = YY
      RETURN
      END
