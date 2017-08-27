C
C
C
      SUBROUTINE PHLINE( X1,Y1 , X2,Y2 )
C
C  Draw a dashed line between 2 internal coordinate points;
C  replaces old PHLINE, which is now renamed ZZPHPH
C
      COMMON /ZZDASH/ NDASH , XLDASH(8) , XID
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      DX1 = X1
      DY1 = Y1
      DX2 = X2
      DY2 = Y2
C
      IF( NDASH .LE. 1 )THEN
         CALL ZZPHPH( DX1,DY1 , DX2,DY2 )
         GOTO 9000
      ENDIF
C
100   CONTINUE
      XYL = SQRT( (DX2-DX1)**2 + (DY2-DY1)**2 )
      IF( XYL .LE. 1.E-5 )GOTO 9000
      ID    = INT(XID) + 1
      XLEFT = (ID-XID) * ABS(XLDASH(ID))
      IF( XYL .LE. XLEFT )THEN
         IF( XLDASH(ID) .GT. 0.0 )CALL ZZPHPH( DX1,DY1 , DX2,DY2 )
         XID = XID + XYL/ABS(XLDASH(ID))
         GOTO 9000
      ELSE
         FAC = XLEFT / XYL
         DX3 = DX1 + FAC*(DX2-DX1)
         DY3 = DY1 + FAC*(DY2-DY1)
         IF( XLDASH(ID) .GT. 0.0 )CALL ZZPHPH( DX1,DY1 , DX3,DY3 )
         DX1 = DX3
         DY1 = DY3
         XID = MOD(ID,NDASH)
         GOTO 100
      ENDIF
C
9000  CONTINUE
      RETURN
      END
