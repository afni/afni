      SUBROUTINE AAAPLT
C=======================================================================
C  PLOTPAK by Bob Cox
C
C  A library of low-level NCAR compatible routines for my personal use
C  on systems without NCAR.
C
C  Routines that depend on specific hardware/software features can be
C  found by searching for the variable NPLOTR, which determines which
C  device is being plotted upon.
C=======================================================================
C  Plotting devices supported in this version (LANL):
C     Sunview CGS window:  CALL SUNPLT
C     CGS metafile:        CALL METPLT
C     output lines drawn
C       to ASCII data file: CALL FILPLT( 'filename' )  [one frame only!]
C     output lines drawn
C       to Unix plot file:  CALL UNIXPL( 'filename' )
C                           if filename starts with a '|' pipe character,
C                           the output will be piped to the shell command
C                           that follows;  '|sunplot' is a good try.
C
C  Devices cannot be mixed in a single computer run!  For correct
C  termination of the SUNPLT and METPLT options, the last plot call
C  in your code should be CALL GDONE, which will terminate CGS.
C  CALL FRAME will terminate the FILPLT option and close the file;
C  it merely writes an "end-of-frame" mark on the UNIXPL file.
C  The UNIXPL file can be displayed at any time by executing
C  CALL UPLSHO [if it is an actual file, and not a pipe].
C
C  Both file output options write to logical unit 99, so you can't
C  use that in your code.
C
C  At present there is no external software that supports FILPLT output.
C.......................................................................
C  Jan 1993: adapted for Microsoft Fortran
C    use GRAPHICS.LIB to plot:  CALL PCPLOT
C.......................................................................
C  June 1993: adapted to produce PostScript output
C    CALL PSPLOT( 'filename' )
C.......................................................................
C  Aug 1998: adapted to interface with the C "memplot" routines.
C=======================================================================
      RETURN
      END
