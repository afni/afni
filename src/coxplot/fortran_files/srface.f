C=======================================================================
C
C
C
      SUBROUTINE SRFACE (X,Y,Z,M,MX,NX,NY,S,STEREO)
C
C  Surface plotting package from NCAR -- the only high level NCAR
C  routine in this library at present (Aug 17, 1990).
C
ccc      DIMENSION       X(NX)      ,Y(NY)      ,Z(MX,NY)   ,M(2,NX,NY) ,
ccc     1                S(6)
      DIMENSION       X(NX)      ,Y(NY)      ,
     x                Z(MX,NY)    ,M(2,NX,NY) ,
     1                S(6)
      DIMENSION       MXS(2)     ,MXF(2)     ,MXJ(2)     ,MYS(2)     ,
     1                MYF(2)     ,MYJ(2)
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN      ,YYMAX      ,
     1                ZZMIN      ,ZZMAX      ,DELCRT     ,EYEX       ,
     2                EYEY       ,EYEZ
      COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX       ,
     1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT     ,
     2                NCLA       ,THETA      ,HSKIRT     ,CHI        ,
     3                CLO        ,CINC       ,ISPVAL
      DATA JF,IF,LY,LX,ICNST/1,1,2,2,0/
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL SRFABD
      CALL SET(0.,1.,0.,1.,1.,1024.,1.,1024.,1)
      BIGEST = 1.E+38
CCC      BIGEST = R1MACH(2)
      MMXX = MX
      NNXX = NX
      NNYY = NY
      STER = STEREO
      NXP1 = NNXX+1
      NYP1 = NNYY+1
      NLA = NCLA
      NSPVAL = ISPVAL
      NDRZ = IDRZ
      IF (IDRZ .NE. 0)
     1    CALL CLSET (Z,MMXX,NNXX,NNYY,CHI,CLO,CINC,NLA,40,CL,NCL,
     2                ICNST,IOFFP,SPVAL,BIGEST)
      IF (IDRZ .NE. 0) NDRZ = 1-ICNST
      STHETA = SIN(STER*THETA)
      CTHETA = COS(STER*THETA)
      RX = S(1)-S(4)
      RY = S(2)-S(5)
      RZ = S(3)-S(6)
      D1 = SQRT(RX*RX+RY*RY+RZ*RZ)
      D2 = SQRT(RX*RX+RY*RY)
      DX = 0.
      DY = 0.
      IF (STEREO .EQ. 0.) GO TO  20
      D1 = D1*STEREO*THETA
      IF (D2 .GT. 0.) GO TO  10
      DX = D1
      GO TO  20
   10 AGL = ATAN2(RX,-RY)
      DX = D1*COS(AGL)
      DY = D1*SIN(AGL)
   20 IROT = IROTS
      NPIC = 1
      IF (STER .NE. 0.) NPIC = 2
      FACT = 1.
      IF (NRSWT .NE. 0) FACT = RZERO/D1
      IF (ISTP.EQ.0 .AND. STER.NE.0.) IROT = 1
      DO 570 IPIC=1,NPIC
         NUPPER = IUPPER
         IF (IFR .LT. 0) CALL FRAME
C
C SET UP MAPING FROM FLOATING POINT 3-SPACE TO CRT SPACE.
C
         SIGN1 = IPIC*2-3
         EYEX = S(1)+SIGN1*DX
         POIX = S(4)+SIGN1*DX
         EYEY = S(2)+SIGN1*DY
         POIY = S(5)+SIGN1*DY
         EYEZ = S(3)
         POIZ = S(6)
         LL = 0
         XEYE = EYEX
         YEYE = EYEY
         ZEYE = EYEZ
         CALL TRN32S (POIX,POIY,POIZ,XEYE,YEYE,ZEYE,0)
         LL = IPIC+2*ISTP+3
         IF (STER .EQ. 0.) LL = 1
         IF (NRSWT .NE. 0) GO TO 100
         XXMIN = X(1)
         XXMAX = X(NNXX)
         YYMIN = Y(1)
         YYMAX = Y(NNYY)
         UMIN = BIGEST
         VMIN = BIGEST
         ZZMIN = BIGEST
         UMAX = -UMIN
         VMAX = -VMIN
         ZZMAX = -ZZMIN
         DO  40 J=1,NNYY
            DO  30 I=1,NNXX
               ZZ = Z(I,J)
               IF (IOFFP.EQ.1 .AND. ZZ.EQ.SPVAL) GO TO  30
               ZZMAX = AMAX1(ZZMAX,ZZ)
               ZZMIN = AMIN1(ZZMIN,ZZ)
               CALL TRN32S (X(I),Y(J),Z(I,J),UT,VT,DUMMY,1)
               UMAX = AMAX1(UMAX,UT)
               UMIN = AMIN1(UMIN,UT)
               VMAX = AMAX1(VMAX,VT)
               VMIN = AMIN1(VMIN,VT)
   30       CONTINUE
   40    CONTINUE
         IF (ISKIRT .NE. 1) GO TO  70
         NXSTP = NNXX-1
         NYSTP = NNYY-1
         DO  60 J=1,NNYY,NYSTP
            DO  50 I=1,NNXX,NXSTP
               CALL TRN32S (X(I),Y(J),HSKIRT,UT,VT,DUMMY,1)
               UMAX = AMAX1(UMAX,UT)
               UMIN = AMIN1(UMIN,UT)
               VMAX = AMAX1(VMAX,VT)
               VMIN = AMIN1(VMIN,VT)
   50       CONTINUE
   60    CONTINUE
   70    CONTINUE
         WIDTH = UMAX-UMIN
         HIGHT = VMAX-VMIN
         DIF = .5*(WIDTH-HIGHT)
         IF (DIF)  80,100, 90
   80    UMIN = UMIN+DIF
         UMAX = UMAX-DIF
         GO TO 100
   90    VMIN = VMIN-DIF
         VMAX = VMAX+DIF
  100    XEYE = EYEX
         YEYE = EYEY
         ZEYE = EYEZ
         CALL TRN32S (POIX,POIY,POIZ,XEYE,YEYE,ZEYE,0)
         DO 120 J=1,NNYY
            DO 110 I=1,NNXX
               CALL TRN32S (X(I),Y(J),Z(I,J),UT,VT,DUMMY,1)
               M(1,I,J) = UT
               M(2,I,J) = VT
  110       CONTINUE
  120    CONTINUE
C
C INITIALIZE UPPER AND LOWER VISIBILITY ARRAYS
C
         DO 130 K=1,1024
            LIMU(K) = 0
            LIML(K) = 1024
  130    CONTINUE
C
C FIND ORDER TO DRAW LINES
C
         NXPASS = 1
         IF (S(1) .GE. X(NNXX)) GO TO 160
         IF (S(1) .LE. X(1)) GO TO 170
         DO 140 I=2,NNXX
            LX = I
            IF (S(1) .LE. X(I)) GO TO 150
  140    CONTINUE
  150    MXS(1) = LX-1
         MXJ(1) = -1
         MXF(1) = 1
         MXS(2) = LX
         MXJ(2) = 1
         MXF(2) = NNXX
         NXPASS = 2
         GO TO 180
  160    MXS(1) = NNXX
         MXJ(1) = -1
         MXF(1) = 1
         GO TO 180
  170    MXS(1) = 1
         MXJ(1) = 1
         MXF(1) = NNXX
  180    NYPASS = 1
         IF (S(2) .GE. Y(NNYY)) GO TO 210
         IF (S(2) .LE. Y(1)) GO TO 220
         DO 190 J=2,NNYY
            LY = J
            IF (S(2) .LE. Y(J)) GO TO 200
  190    CONTINUE
  200    MYS(1) = LY-1
         MYJ(1) = -1
         MYF(1) = 1
         MYS(2) = LY
         MYJ(2) = 1
         MYF(2) = NNYY
         NYPASS = 2
         GO TO 230
  210    MYS(1) = NNYY
         MYJ(1) = -1
         MYF(1) = 1
         GO TO 230
  220    MYS(1) = 1
         MYJ(1) = 1
         MYF(1) = NNYY
C
C PUT ON SKIRT ON FRONT SIDE IF WANTED
C
  230    IF (NXPASS.EQ.2 .AND. NYPASS.EQ.2) GO TO 490
         IF (ISKIRT .EQ. 0) GO TO 290
         IN = MXS(1)
         IF = MXF(1)
         JN = MYS(1)
         JF = MYF(1)
         IF (NYPASS .NE. 1) GO TO 260
         CALL TRN32S (X(1),Y(JN),HSKIRT,UX1,VX1,DUMMY,1)
         CALL TRN32S (X(NNXX),Y(JN),HSKIRT,UX2,VX2,DUMMY,1)
         QU = (UX2-UX1)/(X(NNXX)-X(1))
         QV = (VX2-VX1)/(X(NNXX)-X(1))
         YNOW = Y(JN)
         DO 240 I=1,NNXX
            CALL TRN32S (X(I),YNOW,HSKIRT,RU,RV,DUMMY,1)
            CALL DRAWS (IFIX(RU),IFIX(RV),M(1,I,JN),M(2,I,JN),1,0)
  240    CONTINUE
         CALL DRAWS (IFIX(UX1),IFIX(VX1),IFIX(UX2),IFIX(VX2),1,1)
         IF (IDRY .NE. 0) GO TO 260
         DO 250 I=2,NNXX
            CALL DRAWS (M(1,I-1,JN),M(2,I-1,JN),M(1,I,JN),M(2,I,JN),1,1)
  250    CONTINUE
  260    IF (NXPASS .NE. 1) GO TO 290
         CALL TRN32S (X(IN),Y(1),HSKIRT,UY1,VY1,DUMMY,1)
         CALL TRN32S (X(IN),Y(NNYY),HSKIRT,UY2,VY2,DUMMY,1)
         QU = (UY2-UY1)/(Y(NNYY)-Y(1))
         QV = (VY2-VY1)/(Y(NNYY)-Y(1))
         XNOW = X(IN)
         DO 270 J=1,NNYY
            CALL TRN32S (XNOW,Y(J),HSKIRT,RU,RV,DUMMY,1)
            CALL DRAWS (IFIX(RU),IFIX(RV),M(1,IN,J),M(2,IN,J),1,0)
  270    CONTINUE
         CALL DRAWS (IFIX(UY1),IFIX(VY1),IFIX(UY2),IFIX(VY2),1,1)
         IF (IDRX .NE. 0) GO TO 290
         DO 280 J=2,NNYY
            CALL DRAWS (M(1,IN,J-1),M(2,IN,J-1),M(1,IN,J),M(2,IN,J),1,1)
  280    CONTINUE
C
C PICK PROPER ALGORITHM
C
  290    LI = MXJ(1)
         MI = MXS(1)-LI
         NI = IABS(MI-MXF(1))
         LJ = MYJ(1)
         MJ = MYS(1)-LJ
         NJ = IABS(MJ-MYF(1))
C
C WHEN LINE OF SIGHT IS NEARER TO PARALLEL TO THE X AXIS,
C HAVE J LOOP OUTER-MOST, OTHERWISE HAVE I LOOP OUTER-MOST.
C
         IF (ABS(RX) .LE. ABS(RY)) GO TO 360
         IF (ISKIRT.NE.0 .OR. NYPASS.NE.1) GO TO 310
         I = MXS(1)
         DO 300 J=2,NNYY
            CALL DRAWS (M(1,I,J-1),M(2,I,J-1),M(1,I,J),M(2,I,J),0,1)
  300    CONTINUE
  310    DO 350 II=1,NNXX
            I = MI+II*LI
            IPLI = I+LI
            IF (NYPASS .EQ. 1) GO TO 320
            K = MYS(1)
            L = MYS(2)
            IF (IDRX .NE. 0)
     1          CALL DRAWS (M(1,I,K),M(2,I,K),M(1,I,L),M(2,I,L),1,1)
            IF (NDRZ.NE.0 .AND. II.NE.NI)
     1          CALL CTCELL (Z,MMXX,NNXX,NNYY,M,MIN0(I,I+LI),K)
  320       DO 340 JPASS=1,NYPASS
               LJ = MYJ(JPASS)
               MJ = MYS(JPASS)-LJ
               NJ = IABS(MJ-MYF(JPASS))
               DO 330 JJ=1,NJ
                  J = MJ+JJ*LJ
                  JPLJ = J+LJ
                  IF (IDRX.NE.0 .AND. JJ.NE.NJ)
     1                CALL DRAWS (M(1,I,J),M(2,I,J),M(1,I,JPLJ),
     2                            M(2,I,JPLJ),1,1)
                  IF (I.NE.MXF(1) .AND. IDRY.NE.0)
     1                CALL DRAWS (M(1,IPLI,J),M(2,IPLI,J),M(1,I,J),
     2                            M(2,I,J),1,1)
                  IF (NDRZ.NE.0 .AND. JJ.NE.NJ .AND. II.NE.NNXX)
     1                CALL CTCELL (Z,MMXX,NNXX,NNYY,M,MIN0(I,I+LI),
     2                             MIN0(J,J+LJ))
  330          CONTINUE
  340       CONTINUE
  350    CONTINUE
         GO TO 430
  360    IF (ISKIRT.NE.0 .OR. NXPASS.NE.1) GO TO 380
         J = MYS(1)
         DO 370 I=2,NNXX
            CALL DRAWS (M(1,I-1,J),M(2,I-1,J),M(1,I,J),M(2,I,J),0,1)
  370    CONTINUE
  380    DO 420 JJ=1,NNYY
            J = MJ+JJ*LJ
            JPLJ = J+LJ
            IF (NXPASS .EQ. 1) GO TO 390
            K = MXS(1)
            L = MXS(2)
            IF (IDRY .NE. 0)
     1          CALL DRAWS (M(1,K,J),M(2,K,J),M(1,L,J),M(2,L,J),1,1)
            IF (NDRZ.NE.0 .AND. JJ.NE.NJ)
     1          CALL CTCELL (Z,MMXX,NNXX,NNYY,M,K,MIN0(J,J+LJ))
  390       DO 410 IPASS=1,NXPASS
               LI = MXJ(IPASS)
               MI = MXS(IPASS)-LI
               NI = IABS(MI-MXF(IPASS))
               DO 400 II=1,NI
                  I = MI+II*LI
                  IPLI = I+LI
                  IF (IDRY.NE.0 .AND. II.NE.NI)
     1                CALL DRAWS (M(1,I,J),M(2,I,J),M(1,IPLI,J),
     2                            M(2,IPLI,J),1,1)
                  IF (J.NE.MYF(1) .AND. IDRX.NE.0)
     1                CALL DRAWS (M(1,I,JPLJ),M(2,I,JPLJ),M(1,I,J),
     2                            M(2,I,J),1,1)
                  IF (NDRZ.NE.0 .AND. II.NE.NI .AND. JJ.NE.NNYY)
     1                CALL CTCELL (Z,MMXX,NNXX,NNYY,M,MIN0(I,I+LI),
     2                             MIN0(J,J+LJ))
  400          CONTINUE
  410       CONTINUE
  420    CONTINUE
  430    IF (ISKIRT .EQ. 0) GO TO 520
C
C FIX UP IF SKIRT IS USED WITH LINES ONE WAY.
C
         IF (IDRX .NE. 0) GO TO 460
         DO 450 IPASS=1,NXPASS
            IF (NXPASS .EQ. 2) IF = 1+(IPASS-1)*(NNXX-1)
            DO 440 J=2,NNYY
               CALL DRAWS (M(1,IF,J-1),M(2,IF,J-1),M(1,IF,J),M(2,IF,J),
     1                     1,0)
  440       CONTINUE
  450    CONTINUE
  460    IF (IDRY .NE. 0) GO TO 520
         DO 480 JPASS=1,NYPASS
            IF (NYPASS .EQ. 2) JF = 1+(JPASS-1)*(NNYY-1)
            DO 470 I=2,NNXX
               CALL DRAWS (M(1,I-1,JF),M(2,I-1,JF),M(1,I,JF),M(2,I,JF),
     1                     1,0)
  470       CONTINUE
  480    CONTINUE
         GO TO 520
C
C ALL VISIBLE IF VIEWED FROM DIRECTLY ABOVE OR BELOW.
C
  490    IF (NUPPER.GT.0 .AND. S(3).LT.S(6)) GO TO 520
         IF (NUPPER.LT.0 .AND. S(3).GT.S(6)) GO TO 520
         NUPPER = 1
         IF (S(3) .LT. S(6)) NUPPER = -1
         DO 510 I=1,NNXX
            DO 500 J=1,NNYY
               IF (IDRX.NE.0 .AND. J.NE.NNYY)
     1             CALL DRAWS (M(1,I,J),M(2,I,J),M(1,I,J+1),M(2,I,J+1),
     2                         1,0)
               IF (IDRY.NE.0 .AND. I.NE.NNXX)
     1             CALL DRAWS (M(1,I,J),M(2,I,J),M(1,I+1,J),M(2,I+1,J),
     2                         1,0)
               IF (IDRZ.NE.0 .AND. I.NE.NNXX .AND. J.NE.NNYY)
     1             CALL CTCELL (Z,MMXX,NNXX,NNYY,M,I,J)
  500       CONTINUE
  510    CONTINUE
  520    IF (STER .EQ. 0.) GO TO 560
         IF (ISTP) 540,530,550
  530    CALL FRAME
  540    CALL FRAME
         GO TO 570
  550    IF (IPIC .NE. 2) GO TO 570
  560    IF (IFR .GT. 0) CALL FRAME
  570 CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE SRFPL( N , PX , PY )
      DIMENSION PX(2) , PY(2)
      CALL LINE( PX(1),PY(1) , PX(2),PY(2) )
      RETURN
      END
C
C
C
      SUBROUTINE CLSET (Z,MX,NX,NY,CHI,CLO,CINC,NLA,NLM,CL,NCL,ICNST,
     1                  IOFFP,SPVAL,BIGEST)
ccc      DIMENSION       Z(MX,NY)   ,CL(NLM)
      DIMENSION       Z(MX,NY)  ,CL(NLM)
      DATA KK/0/
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CLSET PUTS THE VALUS OF THE CONTOUR LEVELS IN CL
C
      ICNST = 0
      GLO = CLO
      HA = CHI
      FANC = CINC
      CRAT = NLA
      IF (HA-GLO)  10, 20, 50
   10 GLO = HA
      HA = CLO
      GO TO  50
   20 GLO = BIGEST
      HA = -GLO
      DO  40 J=1,NY
         DO  30 I=1,NX
            IF (IOFFP.EQ.1 .AND. Z(I,J).EQ.SPVAL) GO TO  30
            GLO = AMIN1(Z(I,J),GLO)
            HA = AMAX1(Z(I,J),HA)
   30    CONTINUE
   40 CONTINUE
   50 IF (FANC)  60, 70, 90
   60 CRAT = -FANC
   70 FANC = (HA-GLO)/CRAT
      IF (FANC) 140,140, 80
   80 P = 10.**(IFIX(ALOG10(FANC)+500.)-500)
      FANC = AINT(FANC/P)*P
   90 IF (CHI-CLO) 110,100,110
  100 GLO = AINT(GLO/FANC)*FANC
      HA = AINT(HA/FANC)*FANC
  110 DO 120 K=1,NLM
         CC = GLO+FLOAT(K-1)*FANC
         IF (CC .GT. HA) GO TO 130
         KK = K
         CL(K) = CC
  120 CONTINUE
  130 NCL = KK
      RETURN
  140 ICNST = 1
      RETURN
      END
C
C
C
      SUBROUTINE CTCELL (Z,MX,NX,NY,M,I0,J0)
C
CCC      DIMENSION       Z(MX,NY)   ,M(2,NX,NY)
      DIMENSION       Z(MX,NY)   ,M(2,NX,NY)
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
C
      LOGICAL LCOLOR
      DATA IDUB/0/
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      R(HO,HU) = (HO-CV)/(HO-HU)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      I1 = I0
      I1P1 = I1+1
      J1 = J0
      J1P1 = J1+1
      H1 = Z(I1,J1)
      H2 = Z(I1,J1P1)
      H3 = Z(I1P1,J1P1)
      H4 = Z(I1P1,J1)
      IF (IOFFP .NE. 1) GO TO  10
      IF (H1.EQ.SPVAL .OR. H2.EQ.SPVAL .OR. H3.EQ.SPVAL .OR.
     1    H4.EQ.SPVAL) RETURN
   10 IF (AMIN1(H1,H2,H3,H4) .GT. CL(NCL)) RETURN
C
      LCOLOR = .FALSE.
      DO 110 K=1,NCL
C
C FOR EACH CONTOUR LEVEL, DESIDE WHICH OF THE 16 BASIC SIT-
C UATIONS EXISTS, THEN INTERPOLATE IN TWO-SPACE TO FIND THE
C END POINTS OF THE CONTOUR LINE SEGMENT WITHIN THIS CELL.
C
         CV = CL(K)
         K1 = (IFIX(SIGN(1.,H1-CV))+1)/2
         K2 = (IFIX(SIGN(1.,H2-CV))+1)/2
         K3 = (IFIX(SIGN(1.,H3-CV))+1)/2
         K4 = (IFIX(SIGN(1.,H4-CV))+1)/2
         JUMP = 1+K1+K2*2+K3*4+K4*8
C
C  17/Apr/91:  plot contours in different colors
C
         IF( JUMP.GT.1 .AND. JUMP.LT.16 )CALL COLOR( MOD(K,6)+2 )
C
         GO TO (120, 30, 50, 60, 70, 20, 80, 90, 90, 80,
     1           40, 70, 60, 50, 30,110),JUMP
C
   20    IDUB = 1
   30    RA = R(H1,H2)
         MUA = FLOAT(M(1,I1,J1))+RA*FLOAT(M(1,I1,J1P1)-M(1,I1,J1))
         MVA = FLOAT(M(2,I1,J1))+RA*FLOAT(M(2,I1,J1P1)-M(2,I1,J1))
         RB = R(H1,H4)
         MUB = FLOAT(M(1,I1,J1))+RB*FLOAT(M(1,I1P1,J1)-M(1,I1,J1))
         MVB = FLOAT(M(2,I1,J1))+RB*FLOAT(M(2,I1P1,J1)-M(2,I1,J1))
         GO TO 100
   40    IDUB = -1
   50    RA = R(H2,H1)
         MUA = FLOAT(M(1,I1,J1P1))+RA*FLOAT(M(1,I1,J1)-M(1,I1,J1P1))
         MVA = FLOAT(M(2,I1,J1P1))+RA*FLOAT(M(2,I1,J1)-M(2,I1,J1P1))
         RB = R(H2,H3)
         MUB = FLOAT(M(1,I1,J1P1))+RB*FLOAT(M(1,I1P1,J1P1)-M(1,I1,J1P1))
         MVB = FLOAT(M(2,I1,J1P1))+RB*FLOAT(M(2,I1P1,J1P1)-M(2,I1,J1P1))
         GO TO 100
   60    RA = R(H2,H3)
         MUA = FLOAT(M(1,I1,J1P1))+RA*FLOAT(M(1,I1P1,J1P1)-M(1,I1,J1P1))
         MVA = FLOAT(M(2,I1,J1P1))+RA*FLOAT(M(2,I1P1,J1P1)-M(2,I1,J1P1))
         RB = R(H1,H4)
         MUB = FLOAT(M(1,I1,J1))+RB*FLOAT(M(1,I1P1,J1)-M(1,I1,J1))
         MVB = FLOAT(M(2,I1,J1))+RB*FLOAT(M(2,I1P1,J1)-M(2,I1,J1))
         GO TO 100
   70    RA = R(H3,H2)
         MUA = FLOAT(M(1,I1P1,J1P1))+
     1         RA*FLOAT(M(1,I1,J1P1)-M(1,I1P1,J1P1))
         MVA = FLOAT(M(2,I1P1,J1P1))+
     1         RA*FLOAT(M(2,I1,J1P1)-M(2,I1P1,J1P1))
         RB = R(H3,H4)
         MUB = FLOAT(M(1,I1P1,J1P1))+
     1         RB*FLOAT(M(1,I1P1,J1)-M(1,I1P1,J1P1))
         MVB = FLOAT(M(2,I1P1,J1P1))+
     1         RB*FLOAT(M(2,I1P1,J1)-M(2,I1P1,J1P1))
         IDUB = 0
         GO TO 100
   80    RA = R(H2,H1)
         MUA = FLOAT(M(1,I1,J1P1))+RA*FLOAT(M(1,I1,J1)-M(1,I1,J1P1))
         MVA = FLOAT(M(2,I1,J1P1))+RA*FLOAT(M(2,I1,J1)-M(2,I1,J1P1))
         RB = R(H3,H4)
         MUB = FLOAT(M(1,I1P1,J1P1))+
     1         RB*FLOAT(M(1,I1P1,J1)-M(1,I1P1,J1P1))
         MVB = FLOAT(M(2,I1P1,J1P1))+
     1         RB*FLOAT(M(2,I1P1,J1)-M(2,I1P1,J1P1))
         GO TO 100
   90    RA = R(H4,H1)
         MUA = FLOAT(M(1,I1P1,J1))+RA*FLOAT(M(1,I1,J1)-M(1,I1P1,J1))
         MVA = FLOAT(M(2,I1P1,J1))+RA*FLOAT(M(2,I1,J1)-M(2,I1P1,J1))
         RB = R(H4,H3)
         MUB = FLOAT(M(1,I1P1,J1))+RB*FLOAT(M(1,I1P1,J1P1)-M(1,I1P1,J1))
         MVB = FLOAT(M(2,I1P1,J1))+RB*FLOAT(M(2,I1P1,J1P1)-M(2,I1P1,J1))
         IDUB = 0
  100    CALL DRAWS (MUA,MVA,MUB,MVB,1,0)
         LCOLOR = .TRUE.
         IF (IDUB)  90,110, 70
  110 CONTINUE
C
  120 CONTINUE
      IF( LCOLOR )CALL COLOR( 1 )
      RETURN
      END
C
C
C
      SUBROUTINE DRAWS (MX1,MY1,MX2,MY2,IDRAW,IMARK)
C
C THIS ROUTINE DRAWS THE VISIBLE PART OF THE LINE CONNECTING
C (MX1,MY1) AND (MX2,MY2).  IF IDRAW .NE. 0, THE LINE IS DRAWN.
C IF IMARK .NE. 0, THE VISIBILITY ARRAY IS MARKED.
C
      LOGICAL         VIS1       ,VIS2
      DIMENSION       PXS(2)     ,PYS(2)
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      DATA STEEP/5./
      DATA MX,MY/0,0/
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C MAKE LINE LEFT TO RIGHT.
C
      MMX1 = MX1
      MMY1 = MY1
      MMX2 = MX2
      MMY2 = MY2
      IF (MMX1.EQ.NSPVAL .OR. MMX2.EQ.NSPVAL) RETURN
      IF (MMX1 .GT. MMX2) GO TO  10
      NX1 = MMX1
      NY1 = MMY1
      NX2 = MMX2
      NY2 = MMY2
      GO TO  20
   10 NX1 = MMX2
      NY1 = MMY2
      NX2 = MMX1
      NY2 = MMY1
   20 IF (NUPPER .LT. 0) GO TO 180
C
C CHECK UPPER VISIBILITY.
C
      VIS1 = NY1 .GE. (LIMU(NX1)-1)
      VIS2 = NY2 .GE. (LIMU(NX2)-1)
C
C VIS1 AND VIS2 TRUE MEANS VISIBLE.
C
      IF (VIS1 .AND. VIS2) GO TO 120
C
C VIS1 AND VIS2 FALSE MEANS INVISIBLE.
C
      IF (.NOT.(VIS1 .OR. VIS2)) GO TO 180
C
C FIND CHANGE POINT.
C
      IF (NX1 .EQ. NX2) GO TO 110
      DY = FLOAT(NY2-NY1)/FLOAT(NX2-NX1)
      NX1P1 = NX1+1
      FNY1 = NY1
      IF (VIS1) GO TO  60
      DO  30 K=NX1P1,NX2
         MX = K
         MY = FNY1+FLOAT(K-NX1)*DY
         IF (MY .GT. LIMU(K)) GO TO  40
   30 CONTINUE
   40 IF (ABS(DY) .GE. STEEP) GO TO  90
   50 NX1 = MX
      NY1 = MY
      GO TO 120
   60 DO  70 K=NX1P1,NX2
         MX = K
         MY = FNY1+FLOAT(K-NX1)*DY
         IF (MY .LE. LIMU(K)) GO TO  80
   70 CONTINUE
   80 IF (ABS(DY) .GE. STEEP) GO TO 100
      NX2 = MX-1
      NY2 = MY
      GO TO 120
   90 IF (LIMU(MX) .EQ. 0) GO TO  50
      NX1 = MX
      NY1 = LIMU(NX1)
      GO TO 120
  100 NX2 = MX-1
      NY2 = LIMU(NX2)
      GO TO 120
  110 IF (VIS1) NY2 = MIN0(LIMU(NX1),LIMU(NX2))
      IF (VIS2) NY1 = MIN0(LIMU(NX1),LIMU(NX2))
  120 IF (IDRAW .EQ. 0) GO TO 150
C
C DRAW VISIBLE PART OF LINE.
C
      IF (IROT) 130,140,130
  130 CONTINUE
      PXS(1) = FLOAT(NY1)
      PXS(2) = FLOAT(NY2)
      PYS(1) = FLOAT(1024-NX1)
      PYS(2) = FLOAT(1024-NX2)
      CALL SRFPL (2,PXS,PYS)
      GO TO 150
  140 CONTINUE
      PXS(1) = FLOAT(NX1)
      PXS(2) = FLOAT(NX2)
      PYS(1) = FLOAT(NY1)
      PYS(2) = FLOAT(NY2)
      CALL SRFPL (2,PXS,PYS)
  150 IF (IMARK .EQ. 0) GO TO 180
      IF (NX1 .EQ. NX2) GO TO 170
      DY = FLOAT(NY2-NY1)/FLOAT(NX2-NX1)
      FNY1 = NY1
      DO 160 K=NX1,NX2
         LTEMP = FNY1+FLOAT(K-NX1)*DY
         IF (LTEMP .GT. LIMU(K)) LIMU(K) = LTEMP
  160 CONTINUE
      GO TO 180
  170 LTEMP = MAX0(NY1,NY2)
      IF (LTEMP .GT. LIMU(NX1)) LIMU(NX1) = LTEMP
  180 IF (NUPPER) 190,190,370
C
C SAME IDEA AS ABOVE, BUT FOR LOWER SIDE.
C
  190 IF (MMX1 .GT. MMX2) GO TO 200
      NX1 = MMX1
      NY1 = MMY1
      NX2 = MMX2
      NY2 = MMY2
      GO TO 210
  200 NX1 = MMX2
      NY1 = MMY2
      NX2 = MMX1
      NY2 = MMY1
  210 VIS1 = NY1 .LE. (LIML(NX1)+1)
      VIS2 = NY2 .LE. (LIML(NX2)+1)
      IF (VIS1 .AND. VIS2) GO TO 310
      IF (.NOT.(VIS1 .OR. VIS2)) GO TO 370
      IF (NX1 .EQ. NX2) GO TO 300
      DY = FLOAT(NY2-NY1)/FLOAT(NX2-NX1)
      NX1P1 = NX1+1
      FNY1 = NY1
      IF (VIS1) GO TO 250
      DO 220 K=NX1P1,NX2
         MX = K
         MY = FNY1+FLOAT(K-NX1)*DY
         IF (MY .LT. LIML(K)) GO TO 230
  220 CONTINUE
  230 IF (ABS(DY) .GE. STEEP) GO TO 280
  240 NX1 = MX
      NY1 = MY
      GO TO 310
  250 DO 260 K=NX1P1,NX2
         MX = K
         MY = FNY1+FLOAT(K-NX1)*DY
         IF (MY .GE. LIML(K)) GO TO 270
  260 CONTINUE
  270 IF (ABS(DY) .GE. STEEP) GO TO 290
      NX2 = MX-1
      NY2 = MY
      GO TO 310
  280 IF (LIML(MX) .EQ. 1024) GO TO 240
      NX1 = MX
      NY1 = LIML(NX1)
      GO TO 310
  290 NX2 = MX-1
      NY2 = LIML(NX2)
      GO TO 310
  300 IF (VIS1) NY2 = MAX0(LIML(NX1),LIML(NX2))
      IF (VIS2) NY1 = MAX0(LIML(NX1),LIML(NX2))
  310 IF (IDRAW .EQ. 0) GO TO 340
      IF (IROT) 320,330,320
  320 CONTINUE
      PXS(1) = FLOAT(NY1)
      PXS(2) = FLOAT(NY2)
      PYS(1) = FLOAT(1024-NX1)
      PYS(2) = FLOAT(1024-NX2)
      CALL SRFPL (2,PXS,PYS)
      GO TO 340
  330 CONTINUE
      PXS(1) = FLOAT(NX1)
      PXS(2) = FLOAT(NX2)
      PYS(1) = FLOAT(NY1)
      PYS(2) = FLOAT(NY2)
      CALL SRFPL (2,PXS,PYS)
  340 IF (IMARK .EQ. 0) GO TO 370
      IF (NX1 .EQ. NX2) GO TO 360
      DY = FLOAT(NY2-NY1)/FLOAT(NX2-NX1)
      FNY1 = NY1
      DO 350 K=NX1,NX2
         LTEMP = FNY1+FLOAT(K-NX1)*DY
         IF (LTEMP .LT. LIML(K)) LIML(K) = LTEMP
  350 CONTINUE
      RETURN
  360 LTEMP = MIN0(NY1,NY2)
      IF (LTEMP .LT. LIML(NX1)) LIML(NX1) = LTEMP
  370 RETURN
      END
C
C
C
      SUBROUTINE SETR (XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,R0)
C
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN      ,YYMAX      ,
     1                ZZMIN      ,ZZMAX      ,DELCRT     ,EYEX       ,
     2                EYEY       ,EYEZ
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF (R0)  10, 10, 20
   10 NRSWT = 0
      RETURN
   20 NRSWT = 1
      XXMIN = XMIN
      XXMAX = XMAX
      YYMIN = YMIN
      YYMAX = YMAX
      ZZMIN = ZMIN
      ZZMAX = ZMAX
      RZERO = R0
      LL = 0
      XAT = (XXMAX+XXMIN)*.5
      YAT = (YYMAX+YYMIN)*.5
      ZAT = (ZZMAX+ZZMIN)*.5
      ALPHA = -(YYMIN-YAT)/(XXMIN-XAT)
      YEYE = -RZERO/SQRT(1.+ALPHA*ALPHA)
      XEYE = YEYE*ALPHA
      YEYE = YEYE+YAT
      XEYE = XEYE+XAT
      ZEYE = ZAT
      CALL TRN32S (XAT,YAT,ZAT,XEYE,YEYE,ZEYE,0)
      XMN = XXMIN
      XMX = XXMAX
      YMN = YYMIN
      YMX = YYMAX
      ZMN = ZZMIN
      ZMX = ZZMAX
      CALL TRN32S (XMN,YMN,ZAT,UMN,DUMMY,DUMMIE,1)
      CALL TRN32S (XMX,YMN,ZMN,DUMMY,VMN,DUMMIE,1)
      CALL TRN32S (XMX,YMX,ZAT,UMX,DUMMY,DUMMIE,1)
      CALL TRN32S (XMX,YMN,ZMX,DUMMY,VMX,DUMMIE,1)
      UMIN = UMN
      UMAX = UMX
      VMIN = VMN
      VMAX = VMX
      BIGD = SQRT((XXMAX-XXMIN)**2+(YYMAX-YYMIN)**2+(ZZMAX-ZZMIN)**2)*.5
      RETURN
      END
C
C
C
      SUBROUTINE TRN32S (X,Y,Z,XT,YT,ZT,IFLAG)
C
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN      ,YYMAX      ,
     1                ZZMIN      ,ZZMAX      ,DELCRT     ,EYEX       ,
     2                EYEY       ,EYEZ
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      DIMENSION       NLU(7)     ,NRU(7)     ,NBV(7)     ,NTV(7)
C
C PICTURE CORNER COORDINATES FOR LL=1
C
      DATA NLU(1),NRU(1),NBV(1),NTV(1)/  10,1014,  10,1014/
C
C PICTURE CORNER COORDINATES FOR LL=2
C
      DATA NLU(2),NRU(2),NBV(2),NTV(2)/  10, 924,  50, 964/
C
C PICTURE CORNER COORDINATES FOR LL=3
C
      DATA NLU(3),NRU(3),NBV(3),NTV(3)/ 100,1014,  50, 964/
C
C PICTURE CORNER COORDINATES FOR LL=4
C
      DATA NLU(4),NRU(4),NBV(4),NTV(4)/  10,1014,  10,1014/
C
C PICTURE CORNER COORDINATES FOR LL=5
C
      DATA NLU(5),NRU(5),NBV(5),NTV(5)/  10,1014,  10,1014/
C
C PICTURE CORNER COORDINATES FOR LL=6
C
      DATA NLU(6),NRU(6),NBV(6),NTV(6)/  10, 512, 256, 758/
C
C PICTURE CORNER COORDINATES FOR LL=7
C
      DATA NLU(7),NRU(7),NBV(7),NTV(7)/ 512,1014, 256, 758/
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C STORE THE PARAMETERS OF THE SET32 CALL FOR USE WHEN
C TRN32 IS CALLED.
C
      IF (IFLAG)  40, 10, 40
   10 CONTINUE
      ASSIGN  60 TO JUMP3
      IF (IOFFP .EQ. 1) ASSIGN  50 TO JUMP3
      AX = X
      AY = Y
      AZ = Z
      EX = XT
      EY = YT
      EZ = ZT
C
C AS MUCH COMPUTATION AS POSSIBLE IS DONE DURING EXECUTION
C THIS ROUTINE WHEN IFLAG=0 BECAUSE CALLS IN THAT MODE ARE INFREQUENT.
C
      DX = AX-EX
      DY = AY-EY
      DZ = AZ-EZ
      D = SQRT(DX*DX+DY*DY+DZ*DZ)
      COSAL = DX/D
      COSBE = DY/D
      COSGA = DZ/D
      SINGA = SQRT(1.-COSGA*COSGA)
      ASSIGN 120 TO JUMP2
      IF (LL .EQ. 0) GO TO  20
      ASSIGN 100 TO JUMP2
      DELCRT = NRU(LL)-NLU(LL)
      U0 = UMIN
      V0 = VMIN
      U1 = NLU(LL)
      V1 = NBV(LL)
      U2 = NRU(LL)-NLU(LL)
      V2 = NTV(LL)-NBV(LL)
      U3 = U2/(UMAX-UMIN)
      V3 = V2/(VMAX-VMIN)
      U4 = NRU(LL)
      V4 = NTV(LL)
      IF (NRSWT .EQ. 0) GO TO  20
      U0 = -BIGD
      V0 = -BIGD
      U3 = U2/(2.*BIGD)
      V3 = V2/(2.*BIGD)
C
C THE 3-SPACE POINT LOOKED AT IS TRANSFORMED INTO (0,0) OF
C THE 2-SPACE.  THE 3-SPACE Z AXIS IS TRANSFORMED INTO THE
C 2-SPACE Y AXIS.  IF THE LINE OF SIGHT IS CLOSE TO PARALLEL
C TO THE 3-SPACE Z AXIS, THE 3-SPACE Y AXIS IS CHOSEN (IN-
C STEAD OF THE 3-SPACE Z AXIS) TO BE TRANSFORMED INTO THE
C 2-SPACE Y AXIS.
C
   20 IF (SINGA .LT. 0.0001) GO TO  30
      R = 1./SINGA
      ASSIGN  70 TO JUMP
      RETURN
   30 SINBE = SQRT(1.-COSBE*COSBE)
      R = 1./SINBE
      ASSIGN  80 TO JUMP
      RETURN
   40 CONTINUE
      XX = X
      YY = Y
      ZZ = Z
      GO TO JUMP3,( 50, 60)
   50 IF (ZZ .EQ. SPVAL) GO TO 110
   60 Q = D/((XX-EX)*COSAL+(YY-EY)*COSBE+(ZZ-EZ)*COSGA)
      GO TO JUMP,( 70, 80)
   70 XX = ((EX+Q*(XX-EX)-AX)*COSBE-(EY+Q*(YY-EY)-AY)*COSAL)*R
      YY = (EZ+Q*(ZZ-EZ)-AZ)*R
      GO TO  90
   80 XX = ((EZ+Q*(ZZ-EZ)-AZ)*COSAL-(EX+Q*(XX-EX)-AX)*COSGA)*R
      YY = (EY+Q*(YY-EY)-AY)*R
   90 GO TO JUMP2,(100,120)
  100 XX = AMIN1(U4,AMAX1(U1,U1+U3*(FACT*XX-U0)))
      YY = AMIN1(V4,AMAX1(V1,V1+V3*(FACT*YY-V0)))
      GO TO 120
  110 XX = NSPVAL
      YY = NSPVAL
C
  120 XT = XX
      YT = YY
      RETURN
      END
C
ccc      BLOCKDATA SRFABD
      SUBROUTINE SRFABD
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /SRFIP1/ IFR        ,ISTP       ,IROTS      ,IDRX       ,
     1                IDRY       ,IDRZ       ,IUPPER     ,ISKIRT     ,
     2                NCLA       ,THETA      ,HSKIRT     ,CHI        ,
     3                CLO        ,CINC       ,ISPVAL
C
C  INITIALIZATION OF INTERNAL PARAMETERS
C
      DATA ISPVAL/-999/
      DATA IFR,ISTP,IROTS,IDRX,IDRY,IDRZ,IUPPER,ISKIRT,NCLA/
     1       1,   0,    0,   1,   1,   0,     0,     0,   6/
      DATA THETA,HSKIRT,CHI,CLO,CINC/
     1       .02,    0., 0., 0.,  0./
      DATA NRSWT/0/
      DATA IOFFP,SPVAL/0,0.0/
C
      RETURN
      END
