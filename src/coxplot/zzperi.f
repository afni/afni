C
C
C
      SUBROUTINE ZZPERI( ILAB )
C
C  Do the perimeter axes.
C  ILAB = 0  --> no labels on axes
C       = 1  --> labels on x and y
C       = 2  --> labels on x only
C       = 3  --> labels on y only
C.......................................................................
      INCLUDE 'plotpak.inc'
      INTEGER XLAB , YLAB
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF( XMIN .LT. XMAX )THEN
         ISIDEY = 1
         SXMIN  = XMIN
         SXMAX  = XMAX
      ELSE
         ISIDEY = -1
         SXMIN  = XMAX
         SXMAX  = XMIN
      ENDIF
C
      IF( YMIN .LT. YMAX )THEN
         ISIDEX = 1
         SYMIN  = YMIN
         SYMAX  = YMAX
      ELSE
         ISIDEX = -1
         SYMIN  = YMAX
         SYMAX  = YMIN
      ENDIF
C
      XLAB = 0
      YLAB = 0
      IF( ILAB .EQ. 1 .OR. ILAB .EQ. 2 ) XLAB = 1
      IF( ILAB .EQ. 1 .OR. ILAB .EQ. 3 ) YLAB = 1
C
      CALL ZZAXXX( SXMIN , SXMAX , SYMIN ,  ISIDEX , XLAB )
      CALL ZZAXXX( SXMIN , SXMAX , SYMAX , -ISIDEX , 0    )
C
      CALL ZZAXYY( SXMIN , SYMIN , SYMAX ,  ISIDEY , YLAB )
      CALL ZZAXYY( SXMAX , SYMIN , SYMAX , -ISIDEY , 0    )
C
      RETURN
      END
