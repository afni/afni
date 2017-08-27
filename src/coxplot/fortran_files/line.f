C
C
C
      SUBROUTINE LINE( X1,Y1 , X2,Y2 )
C
C  Draw one line between given user coordinates.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XX1 = X1
      YY1 = Y1
      CALL ZZPHYS( XX1 , YY1 )
      XX2 = X2
      YY2 = Y2
      CALL ZZPHYS( XX2 , YY2 )
C
      CALL ZZLINE( XX1,YY1 , XX2,YY2 )
C
      RETURN
      END
