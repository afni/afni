#define THD_BN_CMTOP


#if 0
   #define THD_BN_XCM    0.0   /* center of mass of output dataset goes here */
   #define THD_BN_YCM   20.0

   #ifdef THD_BN_CMTOP
   # define THD_BN_ZCM  20.0
   #else
   # define THD_BN_ZCM   0.0
   #endif
   #define THD_BN_XORG -83.0   /* the box for the master dataset grid */
   #define THD_BN_YORG -89.0
   #define THD_BN_ZORG -82.0
   #define THD_BN_DXYZ   1.0
   #define THD_BN_NX   ((int)(167.0/THD_BN_DXYZ))
   #define THD_BN_NY   ((int)(212.0/THD_BN_DXYZ))
   #define THD_BN_NZ   ((int)(175.0/THD_BN_DXYZ))
   #define THD_BN_ZHEIGHT 170.0   /* height of box, from top slice */
#endif

