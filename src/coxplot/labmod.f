C
C
C
      SUBROUTINE LABMOD( IFMTX , IFMTY , NUMX , NUMY ,
     X                   JSIZX , JSIZY , IXDEC , IYDEC , IXOR )
C
C  Modify the labels for the axes.  Note that only the JSIZX and JSIZY
C  arguments are used in this call.  The other arguments are retained
C  for compatibility with NCAR.
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ISIZX = JSIZX
      IF( ISIZX .LE. 0 )THEN
         ISIZX = 8
      ELSEIF( ISIZX .EQ. 1 )THEN
         ISIZX = 12
      ELSEIF( ISIZX .EQ. 2 )THEN
         ISIZX = 16
      ELSEIF( ISIZX .EQ. 3 )THEN
         ISIZX = 24
      ENDIF
C
      ISIZY = JSIZY
      IF( ISIZY .LE. 0 )THEN
         ISIZY = 8
      ELSEIF( ISIZY .EQ. 1 )THEN
         ISIZY = 12
      ELSEIF( ISIZY .EQ. 2 )THEN
         ISIZY = 16
      ELSEIF( ISIZY .EQ. 3 )THEN
         ISIZY = 24
      ENDIF
C
      RETURN
      END
