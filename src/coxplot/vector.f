C
C
C
      SUBROUTINE VECTOR( X , Y )
C
C  "Pen down move" (from last plotted location)
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XX = X
      YY = Y
      CALL ZZPHYS( XX , YY )
      CALL ZZLINE( XPHOLD , YPHOLD , XX , YY )
      RETURN
      END
