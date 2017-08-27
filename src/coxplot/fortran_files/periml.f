C
C
C
      SUBROUTINE PERIML( MBX , MLX , MBY , MLY )
C
C  Perimeter along the SET lines -- draw 4 axes -- with labels.
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      MAJRX = MBX
      MINRX = MLX
      MAJRY = MBY
      MINRY = MLY
C
C  Call perimeter routine with labels.
C
      CALL ZZPERI( 1 )
      RETURN
      END
C
C
C
      SUBROUTINE PERIMM( MBX , MLX , MBY , MLY , ILAB )
C
C  Perimeter along the SET lines -- draw 4 axes -- maybe with labels
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      MAJRX = MBX
      MINRX = MLX
      MAJRY = MBY
      MINRY = MLY
      CALL ZZPERI( ILAB )
      RETURN
      END

