C
C
C
      SUBROUTINE PERIM( MBX , MLX , MBY , MLY )
C
C  Perimeter along the SET lines -- draw 4 axes -- no labels.
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      MAJRX = MBX
      MINRX = MLX
      MAJRY = MBY
      MINRY = MLY
C
C  Call perimeter routine with no labels.
C
      CALL ZZPERI( 0 )
      RETURN
      END
