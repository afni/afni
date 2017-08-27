C
C
C
      SUBROUTINE SETDSH( ND , XLD )
C
C  Set dash lengths (in internal units)
C
      REAL XLD(*)
      COMMON /ZZDASH/ NDASH , XLDASH(8) , XID
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      NDASH = MIN( ND , 8 )
      XID   = 0.0
      DO 10 I=1,NDASH
         XLDASH(I) = XLD(I)
10    CONTINUE
      RETURN
      END
