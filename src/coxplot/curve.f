C=======================================================================
C  Device independent plotting routines:
C
C
C
      SUBROUTINE CURVE( X , Y , N )
C
C  Connect N points with N-1 straight lines
C
      REAL X(N) , Y(N)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL POINTS( X , Y , N , 0 , 1 )
      RETURN
      END
