C
C
C
      SUBROUTINE SETW( X1,Y1 , X2,Y2 )
C
C  Set the clipping rectangle (physical coords).
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( X1 .GE. X2 )THEN
         XCLBOT = XPGMIN
         XCLTOP = XPGMAX
      ELSE
         XCLBOT = MAX( X1 , XPGMIN )
         XCLTOP = MIN( X2 , XPGMAX )
      ENDIF
C
      IF( Y1 .GE. Y2 )THEN
         YCLBOT = YPGMIN
         YCLTOP = YPGMAX
      ELSE
         YCLBOT = MAX( Y1 , YPGMIN )
         YCLTOP = MIN( Y2 , YPGMAX )
      ENDIF
C
      RETURN
      END
