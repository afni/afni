C
C
C
      SUBROUTINE ZZLOGX( X1 , X2 , Y , NDEC , TMAJ , TMIN )
C
C  Draw a log axis from (X1,Y) to (X2,Y)  [physical coordinates],
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
      CALL ZZLINE( X1,Y , X2,Y )
      IF( TMAJ .EQ. 0. .OR. TMIN .EQ. 0. )GOTO 8000
      YMAJR = Y + TMAJ
      YMINR = Y + TMIN
      DXMAJ = (X2-X1) / NDEC
      XMAJR = X1
      CALL ZZLINE( XMAJR,Y , XMAJR,YMAJR )
C
      DO 200 IDEC=1,NDEC
         IF( TMIN .NE. 0. )THEN
            DO 100 IMIN=1,8
               XX = XMAJR + TMLOG(IMIN)*DXMAJ
               CALL ZZLINE( XX,Y , XX,YMINR )
100         CONTINUE
         ENDIF
         XMAJR = XMAJR + DXMAJ
         CALL ZZLINE( XMAJR,Y , XMAJR,YMAJR )
200   CONTINUE
C
8000  CONTINUE
      RETURN
      END
