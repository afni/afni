C
C
C
      SUBROUTINE ZZPHYS( X , Y )
C
C  Convert user to physical coordinates.
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( IXCOOR .LT. 0 )X = ALOG10( ABS(X)+1.E-37 )
      X = ALPHXX * X + BETAXX
C
      IF( IYCOOR .LT. 0 )Y = ALOG10( ABS(Y)+1.E-37 )
      Y = ALPHYY * Y + BETAYY
C
      RETURN
      END
