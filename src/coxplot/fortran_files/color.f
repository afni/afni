C
C
C
      SUBROUTINE COLOR( NCOL )
C
C  Set the color of subsequent lines
C.......................................................................
      INCLUDE 'plotpak.inc'
C
      REAL RGB(3,7)
      DATA RGB / 0.0 , 0.0 , 0.0 ,
     X           1.0 , 0.0 , 0.0 ,
     X           0.0 , 0.0 , 1.0 ,
     X           0.0 , 1.0 , 0.0 ,
     X           1.0 , 1.0 , 0.0 ,
     X           1.0 , 0.0 , 1.0 ,
     X           0.0 , 1.0 , 1.0  /
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ccc      IF( NPLOTR.EQ.1 .OR. NPLOTR.EQ.2 )THEN
ccc         IF( NCOL .LE. 1 .OR. NCOL .GT. 7 )THEN
ccc            CALL GCOLOR( 5Hclear )
ccc         ELSEIF( NCOL .EQ. 2 )THEN
ccc            CALL GCOLOR( 3Hred )
ccc         ELSEIF( NCOL .EQ. 3 )THEN
ccc            CALL GCOLOR( 4Hblue )
ccc         ELSEIF( NCOL .EQ. 4 )THEN
ccc            CALL GCOLOR( 5Hgreen )
ccc         ELSEIF( NCOL .EQ. 5 )THEN
ccc            CALL GCOLOR( 6Hyellow )
ccc         ELSEIF( NCOL .EQ. 6 )THEN
ccc            CALL GCOLOR( 7Hmagenta )
ccc         ELSEIF( NCOL .EQ. 7 )THEN
ccc            CALL GCOLOR( 4Hcyan )
ccc         ENDIF
C
ccc      IF( NPLOTR.EQ.5 )THEN
ccc         CALL ZZPCOL( NCOL )
ccc      ENDIF
C
ccc      IF( NPLOTR .EQ. 6 )THEN
ccc         IC = MAX( 1 , MIN( 7 , NCOL ) )
ccc         CALL ZZPSCO( RGB(1,IC) , RGB(2,IC) , RGB(3,IC) )
ccc      ENDIF

      IF( NPLOTR .EQ. 7 )THEN
         IC = MAX( 1 , MIN( 7 , NCOL ) )
         CALL ZZMPCO( RGB(1,IC) , RGB(2,IC) , RGB(3,IC) )
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE FCOLOR( CR,CG,CB )
C
C  Set the color of subsequent lines
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      IF( NPLOTR .EQ. 7 ) CALL ZZMPCO( CR,CG,CB )
      RETURN
      END
