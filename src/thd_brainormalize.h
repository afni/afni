#define CMTOP

#define XCM    0.0   /* center of mass of output dataset goes here */
#define YCM   20.0

#ifdef CMTOP
# define ZCM  20.0
#else
# define ZCM   0.0
#endif

#define XORG -83.0   /* the box for the master dataset grid */
#define YORG -89.0
#define ZORG -82.0
#define NX   167
#define NY   212
#define NZ   175
#define DXYZ   1.0

#define ZHEIGHT 170.0   /* height of box, from top slice */

