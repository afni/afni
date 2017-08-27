C
C
C
      SUBROUTINE ZZPHPH( X1,Y1 , X2,Y2 )
C
C  Plot a physical coordinate line.
C
C  Everything in this package eventually funnels into this routine,
C  which depends on the hardware being drawn upon.  The device
C  is chosen by the NPLOTR variable (in 'plotpak.inc' COMMON /ZZPLTR/):
C
C    1 = Los Alamos CGS SunView window
C    2 = Los Alamos CGS Metafile
C    3 = Write all line coordinates to an ASCII file for later work
C    4 = Write all line coordinates to a Unix plot format file
C    5 = Use Microsoft GRAPHICS.LIB
C    6 = Write PostScript code to an output file
C    7 = Call the C "memplot" routine
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( IFLIP .EQ. 1 )THEN
C  Flipped coordinates
         PX1 = XPSCAL * ( YPHMAX - Y1 )
         PX2 = XPSCAL * ( YPHMAX - Y2 )
         PY1 = YPSCAL * ( XPHMAX - X1 )
         PY2 = YPSCAL * ( XPHMAX - X2 )
      ELSEIF( IFLIP .EQ. 2 )THEN
C  Flipped coordinates
         PX1 = XPSCAL * ( YPHMAX - Y1 )
         PX2 = XPSCAL * ( YPHMAX - Y2 )
         PY1 = XPSCAL * X1
         PY2 = XPSCAL * X2
      ELSE
C  Normal coordinates
         PX1 = XPSCAL * X1
         PX2 = XPSCAL * X2
         PY1 = YPSCAL * Y1
         PY2 = YPSCAL * Y2
      ENDIF
C
C  Plot it!
C
ccc      IF( NPLOTR.EQ.1 .OR. NPLOTR.EQ.2 )THEN
ccc         CALL GMOVA2( PX1 , PY1 )
ccc         CALL GLINA2( PX2 , PY2 )
ccc      ENDIF
C
ccc      IF( NPLOTR .EQ. 3 )THEN
ccc         WRITE(99,101) PX1,PY1,PX2,PY2
ccc101      FORMAT(4(F6.4,','))
ccc      ENDIF
C
ccc      IF( NPLOTR .EQ. 4 )THEN
ccc         CALL ZZUPLI( NINT(PX1),NINT(PY1) , NINT(PX2),NINT(PY2) )
ccc      ENDIF
C
ccc      IF( NPLOTR .EQ. 5 )THEN
ccc         CALL ZZPCLI( PX1,PY1 , PX2,PY2 )
ccc      ENDIF
C
ccc      IF( NPLOTR .EQ. 6 )THEN
ccc         CALL ZZPSLI( NINT(PX1),NINT(PY1) , NINT(PX2),NINT(PY2) )
ccc      ENDIF
C
      IF( NPLOTR .EQ. 7 )THEN
         CALL ZZMPLI( PX1,PY1 , PX2,PY2 )
      ENDIF
c
ccc      WRITE(*,999) PX1,PY1,PX2,PY2
ccc999   FORMAT('ZZPHPH:',4(1X,1PG10.3))
C
      RETURN
      END
