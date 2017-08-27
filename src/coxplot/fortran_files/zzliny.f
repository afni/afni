C
C
C
      SUBROUTINE ZZLINY( X , Y1 , Y2 , MAJRY , TMAJ , MINRY , TMIN )
C
C  Draw a linear axis from (X,Y1) to (X,Y2)  [physical coordinates],
C  with MAJRY major divisions (tic mark size = TMAJ) and MINRY minor
C  divisions (tic mark size = TMIN).
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL ZZLINE( X,Y1 , X,Y2 )
      IF( TMAJ .EQ. 0. .AND. TMIN .EQ. 0. )GOTO 8000
C
      XMAJR = X + TMAJ
      XMINR = X + TMIN
      DY    = (Y2-Y1) / (MAJRY*MINRY)
      YY    = Y1
      CALL ZZLINE( X,YY , XMAJR,YY )
      DO 200 IMAJ=1,MAJRY
         DO 100 IMIN=1,MINRY-1
            YY = YY + DY
            IF( TMIN .NE. 0. )CALL ZZLINE( X,YY , XMINR,YY )
100      CONTINUE
         YY = YY + DY
         CALL ZZLINE( X,YY , XMAJR,YY )
200   CONTINUE
C
8000  CONTINUE
      RETURN
      END
