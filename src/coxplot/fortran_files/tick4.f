C
C
C
      SUBROUTINE TICK4( LMAJX , LMINX , LMAJY , LMINY )
C
C  Set the tick marks in units of 1/1000 the x-width
C.......................................................................
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SCALE = .001 * ( XPGMAX - XPGMIN )
      TMAJX = SCALE * LMAJX
      TMINX = SCALE * LMINX
      TMAJY = SCALE * LMAJY
      TMINY = SCALE * LMINY
      RETURN
      END
