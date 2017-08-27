C
C
C
      SUBROUTINE ZZCLIP( X1IN , Y1IN , X2IN , Y2IN )
C
C  Clip the input line to the predefined plotting region.
C
C  INPUTS
C  ------
C  X1IN   = start X-coordinate of line  (physical coordinates)
C  Y1IN   = start Y-coordinate of line
C  X2IN   = end X-coordinate of line
C  Y2IN   = end Y-coordinate of line
C
C  OUTPUTS
C  -------
C  same as above but clipped so that the line fits into the plotting
C  region defined by calling SETW.
C
C  If the line falls entirely outside of the plotting region, then
C  X1IN is returned as -1.E+38.
C.......................................................................
      LOGICAL LINTER
C
      INCLUDE 'plotpak.inc'
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  Check to see if line can be thrown out as being totally out of
C  the plotting region.
C
      IF( MAX(X1IN,X2IN) .LT. XCLBOT .OR.
     X    MIN(X1IN,X2IN) .GT. XCLTOP .OR.
     X    MAX(Y1IN,Y2IN) .LT. YCLBOT .OR.
     X    MIN(Y1IN,Y2IN) .GT. YCLTOP     )THEN
C
         X1IN = -1.E+38
         GOTO 8000
      ENDIF
C
C  Copy input coordinates to local variables, making sure
C  that X1 .LE. X2 by interchanging the points if necessary.
C
      LINTER = X1IN .GT. X2IN
      IF( LINTER )THEN
         X1 = X2IN
         X2 = X1IN
         Y1 = Y2IN
         Y2 = Y1IN
      ELSE
         X1 = X1IN
         X2 = X2IN
         Y1 = Y1IN
         Y2 = Y2IN
      ENDIF
C
C  Clip line in X direction
C
      DX = X2 - X1
      IF( DX .GT. 0. )THEN
C  only clip if line has some X range
         SLOPE = (Y2-Y1)/DX
         IF( X1 .LT. XCLBOT )THEN
C  intercept of line at left side
            Y1 = Y1 + SLOPE*(XCLBOT-X1)
            X1 = XCLBOT
         ENDIF
         IF( X2 .GT. XCLTOP )THEN
C  intercept at right
            Y2 = Y2 + SLOPE*(XCLTOP-X2)
            X2 = XCLTOP
         ENDIF
      ENDIF
C
C  Check line again to see if it falls outside of plot region.
C
      IF( MAX(Y1,Y2) .LT. YCLBOT .OR. MIN(Y1,Y2) .GT. YCLTOP )THEN
         X1IN = -1.E+38
         GOTO 8000
      ENDIF
C
C  Clip Y-direction.  To do this, must have Y1 .LE. Y2
C
      IF( Y1 .GT. Y2 )THEN
         TEMP = X1
         X1   = X2
         X2   = TEMP
         TEMP = Y1
         Y1   = Y2
         Y2   = TEMP
C
         LINTER = .NOT. LINTER
      ENDIF
C
      DY = Y2 - Y1
      IF( DY .GT. 0. )THEN
C  only clip if line has some Y range
         SLOPE = (X2-X1)/DY
         IF( Y1 .LT. YCLBOT )THEN
C  intercept of line at bottom
            X1 = X1 + SLOPE*(YCLBOT-Y1)
            Y1 = YCLBOT
         ENDIF
         IF( Y2 .GT. YCLTOP )THEN
C  intercept at top
            X2 = X2 + SLOPE*(YCLTOP-Y2)
            Y2 = YCLTOP
         ENDIF
      ENDIF
C
C  Line is now guaranteed to be totally inside the plot region.
C  Copy local clipped coordinates to output values and return.
C  Note that we must restore points to original input order.
C
      IF( LINTER )THEN
         X1IN = X2
         X2IN = X1
         Y1IN = Y2
         Y2IN = Y1
      ELSE
         X1IN = X1
         Y1IN = Y1
         X2IN = X2
         Y2IN = Y2
      ENDIF
C
8000  CONTINUE
      RETURN
      END
