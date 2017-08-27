C
C
C
      SUBROUTINE PWRIT( X , Y , CH , NCH , ISIZ , IOR , ICENT )
C
C  Additional options besides NCAR's choices:
C
C    NCH < 0    ==> use absolute coordinates rather than user coords.
C                   [this is because the use of integer absolute   ]
C                   [coordinates is not implemented in this package]
C
C    ABS(NCH) = 999 ==> find length of string by looking for a 0 byte.
C
C    ICENT = -2 ==> (X,Y) is lower left corner of string to plot.
C
      CHARACTER*1 CH(*)
C.......................................................................
      PARAMETER ( DG2RAD = .017453292 )
C
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Calculate character width in terms of 1/1000 of the x-width.
C
      ISIZE = ISIZ
      IF( ISIZE .LE. 0 )THEN
         ISIZE = 8
      ELSEIF( ISIZE .EQ. 1 )THEN
         ISIZE = 12
      ELSEIF( ISIZE .EQ. 2 )THEN
         ISIZE = 16
      ELSEIF( ISIZE .EQ. 3 )THEN
         ISIZE = 24
      ENDIF
C
      WIDTH = ISIZE * 0.001 * ( XPGMAX - XPGMIN )
C
C  Rotation/scaling factors for digitization.  Include factor of 1/6
C  to allow for digitization scale in ZZCHAR.
C
      OR = DG2RAD * IOR
      DX = WIDTH * COS(OR)
      DY = WIDTH * SIN(OR)
      CT = 0.1666667 * DX
      ST = 0.1666667 * DY
C
C  Starting location for first character.
C
      XX = X
      YY = Y
      IF( NCH .GT. 0 )CALL ZZPHYS( XX , YY )
C
C  Get no. of characters in string.  Special option 999 must be checked.
C
      NCHAR = ABS( NCH )
      IF( NCHAR .EQ. 999 )THEN
         DO 10 I=1,NCHAR
            IF( CH(I) .EQ. CHAR(0) )GOTO 20
10       CONTINUE
20       NCHAR = I-1
      ENDIF
C
C  If centering option is not lower-left corner, must calculate
C  location of lower left corner.
C
      IF( ICENT .NE. -2 )THEN
C  Move from center of character down to bottom (aspect ratio = 7/6)
         XX = XX + 0.5833333 * DY
         YY = YY - 0.5833333 * DX
         IF( ICENT .EQ. 0 )THEN
            XX = XX - 0.5*NCHAR*DX
            YY = YY - 0.5*NCHAR*DY
         ELSEIF( ICENT .EQ. +1 )THEN
            XX = XX - NCHAR*DX
            YY = YY - NCHAR*DY
         ENDIF
      ENDIF
C.......................................................................
      DO 100 I=1,NCHAR
         CALL ZZCHAR( CH(I) , XX , YY , CT , ST )
         XX = XX + DX
         YY = YY + DY
100   CONTINUE
C.......................................................................
      XPHOLD = XX
      YPHOLD = YY
      RETURN
      END
