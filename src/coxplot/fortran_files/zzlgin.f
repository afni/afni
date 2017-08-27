C
C
C
      SUBROUTINE ZZLGIN( XT , PWRTEN , NLOG )
C
C  Return PWRTEN and NTEN such that
C
C   PWRTEN .LE. XT .LT. 10*PWRTEN      AND    PWRTEN = 10**NLOG
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      XL = LOG10( XT ) + .00001
      NL = MAX( INT( XL ) , -36 )
      IF( XL .LT. 0. )NL = NL - 1
      PWRTEN = 10.0 ** NL
      NLOG   = NL
      RETURN
      END
