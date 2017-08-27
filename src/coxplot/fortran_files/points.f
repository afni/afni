C
C
C
      SUBROUTINE POINTS( X , Y , N , ICHAR , IPEN )
C
C  Note that ICHAR is ignored in this version (argument retained for
C  compatibility with NCAR).
C.......................................................................
      REAL X(N) , Y(N)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( IPEN .NE. 1 )THEN
         DO 100 I=1,N
            CALL POINT( X(I) , Y(I) )
100      CONTINUE
      ELSE
         CALL FRSTPT( X(1) , Y(1) )
         DO 200 I=2,N
            CALL VECTOR( X(I) , Y(I) )
200      CONTINUE
      ENDIF
      RETURN
      END
