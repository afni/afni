C=======================================================================
C  Device dependent plotting routines:
C
C
C
      SUBROUTINE FRAME
C
C  Stop plotting in the current window (or frame).
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ccc      IF( NPLOTR.EQ.1 .OR. NPLOTR.EQ.2 )THEN
ccc         CALL GPAGE
ccc      ENDIF
C
ccc      IF( NPLOTR .EQ. 3 )THEN
ccc         CLOSE( UNIT=99 )
ccc      ENDIF
C
ccc      IF( NPLOTR .EQ. 4 )THEN
ccc         CALL ZZUPFR
ccc      ENDIF
C
ccc      IF( NPLOTR .EQ. 5 )THEN
ccc         CALL ZZPCFR
ccc      ENDIF
C
ccc      IF( NPLOTR .EQ. 6 )THEN
ccc         CALL ZZPSFR
ccc      ENDIF
C
      RETURN
      END
