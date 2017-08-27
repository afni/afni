C
C
C
      SUBROUTINE ZZLOGY( X , Y1 , Y2 , NDEC , TMAJ , TMIN )
C
C  Draw a log axis from (X,Y1) to (X,Y2)  [physical coordinates],
C  with NDEC decades and the major and minor tic mark lengths in
C  TMAJ and TMIN, respectively.
C
C  This array is LOG10 of 2,3,...,9;  it is used to space the
C  minor divisions.
C
      REAL TMLOG(8)
      DATA TMLOG / .30103 , .47712 , .60206 , .69897 ,
     X             .77815 , .84509 , .90309 , .95424   /
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL ZZLINE( X,Y1 , X,Y2 )
      IF( TMAJ .EQ. 0. .OR. TMIN .EQ. 0. )GOTO 8000
      XMAJR = X + TMAJ
      XMINR = X + TMIN
      DYMAJ = (Y2-Y1) / NDEC
      YMAJR = Y1
      CALL ZZLINE( X,YMAJR , XMAJR,YMAJR )
C
      DO 200 IDEC=1,NDEC
         IF( TMIN .NE. 0. )THEN
            DO 100 IMIN=1,8
               YY = YMAJR + TMLOG(IMIN)*DYMAJ
               CALL ZZLINE( X,YY , XMINR,YY )
100         CONTINUE
         ENDIF
         YMAJR = YMAJR + DYMAJ
         CALL ZZLINE( X,YMAJR , XMAJR,YMAJR )
200   CONTINUE
C
8000  CONTINUE
      RETURN
      END
