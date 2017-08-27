C
C
C
      SUBROUTINE SETLIN( NTYPE )
C
C  Set default dash types
C    1 = solid
C    2 = long dash
C    3 = short
C    4 = long - short - short
C    5 = very short
C  Outside this range ==> solid
C
      PARAMETER ( NFMAX = 5 )
      INTEGER NDASH(NFMAX)
      REAL    XDASH(8,NFMAX)
C
      DATA NDASH / 1 , 2 , 2 , 6 , 2 /
C
      DATA XDASH / 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 ,
     X             .007,-.007, 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 ,
     X             .002,-.003, 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 ,
     X             .007,-.004,.002 ,-.004,.002 ,-.004, 1.0 , 1.0 ,
     X            .0002,-.005, 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0  /
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      ND = NTYPE
      IF( NTYPE.LE.0 .OR. NTYPE.GT.NFMAX )ND = 1
C
      CALL SETDSH( NDASH(ND) , XDASH(1,ND) )
      RETURN
      END
