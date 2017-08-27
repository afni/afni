C
C
C
      SUBROUTINE ZZLINE( X1,Y1 , X2,Y2 )
C
C  Draw a line between 2 physical coordinates points.
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XX1 = X1
      YY1 = Y1
      XX2 = X2
      YY2 = Y2
      CALL ZZCLIP( XX1,YY1 , XX2,YY2 )
      IF( XX1 .GE. XPGMIN )CALL PHLINE( XX1,YY1 , XX2,YY2 )
      XPHOLD = X2
      YPHOLD = Y2
C
      RETURN
      END
