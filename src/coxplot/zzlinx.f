C
C
C
      SUBROUTINE ZZLINX( X1 , X2 , Y , MAJRX , TMAJ , MINRX , TMIN )
C
C  Draw a linear axis from (X1,Y) to (X2,Y)  [physical coordinates],
C  with MAJRX major divisions (tic mark size = TMAJ) and MINRX minor
C  divisions (tic mark size = TMIN).
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL ZZLINE( X1,Y , X2,Y )
      IF( TMAJ .EQ. 0. .AND. TMIN .EQ. 0. )GOTO 8000
C
      YMAJR = Y + TMAJ
      YMINR = Y + TMIN
      DX    = (X2-X1) / (MAJRX*MINRX)
      XX    = X1
      CALL ZZLINE( XX,Y , XX,YMAJR )
      DO 200 IMAJ=1,MAJRX
         DO 100 IMIN=1,MINRX-1
            XX = XX + DX
            IF( TMIN .NE. 0. )CALL ZZLINE( XX,Y , XX,YMINR )
100      CONTINUE
         XX = XX + DX
         CALL ZZLINE( XX,Y , XX,YMAJR )
200   CONTINUE
C
8000  CONTINUE
      RETURN
      END
