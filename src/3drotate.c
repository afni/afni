/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "thd_shear3d.h"

/*-- prototypes for funcs at end of file --*/

static void rotate_stdin_points( THD_dfvec3, THD_dmat33, int,THD_dfvec3 ) ;

/*------------------------------------------------------------------------------*/

#define MATVEC_DICOM 1
#define MATVEC_ORDER 2

static int verb=0 ;

/*-- 19 Jun 2001 stuff --*/

#define MODE_DFILE   1
#define MODE_1DFILE  2
MRI_IMAGE * get_dfile_params( char * fname , int mode ) ;

/*------------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset=NULL ;
   char * new_prefix = "rota" , * cpt ;
   float dx=0 , dy=0 , dz=0 ;
   int   ax1=0,ax2=1,ax3=2 , adx,ady,adz ;
   char  cdx,cdy,cdz ;
   float th1=0.0,th2=0.0,th3=0.0 ;
   int iopt , nvox , rotarg=-1 , dcode=-1 , ival,nval , ihand ;
   float * fvol ;
   double cputim ;
   int clipit=1 ;  /* 11 Apr 2000 and 16 Apr 2002 */
   float cbot,ctop ;

   int matvec=0 ;    /* 19 July 2000 */
   THD_dmat33 rmat , pp,ppt ;
   THD_dfvec3 tvec ;

   int dopoints=0 , doorigin=0 ;    /* 21 Nov 2000 */
   double xo=0.0 , yo=0.0 , zo=0.0 ;

   THD_3dim_dataset *rotpar_dset=NULL , *gridpar_dset=NULL ;  /* 07 Feb 2001 */

   int zpad=0 ;      /* 05 Feb 2001 */

   char *dname=NULL ;  /* 19 Jun 2001 */
   int   dmode=0 ;
   MRI_IMAGE *dim=NULL ;
   float     *dar=NULL ;
   int       ndar=0    ;
   int       skipit=0  ;

   /*-------------------------------*/

   LOAD_DIAG_DMAT(rmat,1.0,1.0,1.0) ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ){
      printf(
       "Usage: 3drotate [options] dataset\n"
       "Rotates and/or translates all bricks from an AFNI dataset.\n"
       "'dataset' may contain a sub-brick selector list.\n"
       "\n"
       "GENERIC OPTIONS:\n"
       "  -prefix fname    = Sets the output dataset prefix name to be 'fname'\n"
       "  -verbose         = Prints out progress reports (to stderr)\n"
       "\n"
       "OPTIONS TO SPECIFY THE ROTATION/TRANSLATION:\n"
       "-------------------------------------------\n"
       "*** METHOD 1 = direct specification:\n"
       "At most one of these shift options can be used:\n"
       "  -ashift dx dy dz = Shifts the dataset 'dx' mm in the x-direction, etc.,\n"
       "                       AFTER rotation.\n"
       "  -bshift dx dy dz = Shifts the dataset 'dx' mm in the x-direction, etc.,\n"
       "                       BEFORE rotation.\n"
       "    The shift distances by default are along the (x,y,z) axes of the dataset\n"
       "    storage directions (see the output of '3dinfo dataset').  To specify them\n"
       "    anatomically, you can suffix a distance with one of the symbols\n"
       "    'R', 'L', 'A', 'P', 'I', and 'S', meaning 'Right', 'Left', 'Anterior',\n"
       "    'Posterior', 'Inferior', and 'Superior', respectively.\n"
       "\n"
       "  -rotate th1 th2 th3\n"
       "    Specifies the 3D rotation to be composed of 3 planar rotations:\n"
       "       1) 'th1' degrees about the 1st axis,           followed by\n"
       "       2) 'th2' degrees about the (rotated) 2nd axis, followed by\n"
       "       3) 'th3' degrees about the (doubly rotated) 3rd axis.\n"
       "    Which axes are used for these rotations is specified by placing\n"
       "    one of the symbols 'R', 'L', 'A', 'P', 'I', and 'S' at the end\n"
       "    of each angle (e.g., '10.7A').  These symbols denote rotation\n"
       "    about the 'Right-to-Left', 'Left-to-Right', 'Anterior-to-Posterior',\n"
       "    'Posterior-to-Anterior', 'Inferior-to-Superior', and\n"
       "    'Superior-to-Inferior' axes, respectively.  A positive rotation is\n"
       "    defined by the right-hand rule.\n"
       "\n"
       "*** METHOD 2 = copy from output of 3dvolreg:\n"
       "  -rotparent rset\n"
       "    Specifies that the rotation and translation should be taken from the\n"
       "    first 3dvolreg transformation found in the header of dataset 'rset'.\n"
       "  -gridparent gset\n"
       "    Specifies that the output dataset of 3drotate should be shifted to\n"
       "    match the grid of dataset 'gset'.  Can only be used with -rotparent.\n"
       "    This dataset should be one this is properly aligned with 'rset' when\n"
       "    overlaid in AFNI.\n"
       "  * If -rotparent is used, then don't use -matvec, -rotate, or -[ab]shift.\n"
       "  * If 'gset' has a different number of slices than the input dataset,\n"
       "    then the output dataset will be zero-padded in the slice direction\n"
       "    to match 'gset'.\n"
       "  * These options are intended to be used to align datasets between sessions:\n"
       "     S1 = SPGR from session 1    E1 = EPI from session 1\n"
       "     S2 = SPGR from session 2    E2 = EPI from session 2\n"
       " 3dvolreg -twopass -twodup -base S1+orig -prefix S2reg S2+orig\n"
       " 3drotate -rotparent S2reg+orig -gridparent E1+orig -prefix E2reg E2+orig\n"
       "     The result will have E2reg rotated from E2 in the same way that S2reg\n"
       "     was from S2, and also shifted/padded (as needed) to overlap with E1.\n"
       "\n"
       "*** METHOD 3 = give the transformation matrix/vector directly:\n"
       "  -matvec_dicom mfile\n"
       "  -matvec_order mfile\n"
       "    Specifies that the rotation and translation should be read from file\n"
       "    'mfile', which should be in the format\n"
       "           u11 u12 u13 v1\n"
       "           u21 u22 u23 v2\n"
       "           u31 u32 u33 u3\n"
       "    where each 'uij' and 'vi' is a number.  The 3x3 matrix [uij] is the\n"
       "    orthogonal matrix of the rotation, and the 3-vector [vi] is the -ashift\n"
       "    vector of the translation.\n"
       "\n"
       "*** METHOD 4 = copy the transformation from 3dTagalign:\n"
       "  -matvec_dset mset\n"
       "    Specifies that the rotation and translation should be read from\n"
       "    the .HEAD file of dataset 'mset', which was created by program\n"
       "    3dTagalign.\n"
       "  * If -matvec_dicom is used, the matrix and vector are given in Dicom\n"
       "     coordinate order (+x=L, +y=P, +z=S).  This is the option to use\n"
       "     if mfile is generated using 3dTagalign -matvec mfile.\n"
       "  * If -matvec_order is used, the the matrix and vector are given in the\n"
       "     coordinate order of the dataset axes, whatever they may be.\n"
       "  * You can't mix -matvec_* options with -rotate and -*shift.\n"
       "\n"
       "*** METHOD 5 = input rotation+shift parameters from an ASCII file:\n"
       "  -dfile dname  *OR*  -1Dfile dname\n"
       "    With these methods, the movement parameters for each sub-brick\n"
       "    of the input dataset are read from the file 'dname'.  This file\n"
       "    should consist of columns of numbers in ASCII format.  Six (6)\n"
       "    numbers are read from each line of the input file.  If the\n"
       "    '-dfile' option is used, each line of the input should be at\n"
       "    least 7 numbers, and be of the form\n"
       "      ignored roll pitch yaw dS dL dP\n"
       "    If the '-1Dfile' option is used, then each line of the input\n"
       "    should be at least 6 numbers, and be of the form\n"
       "      roll pitch yaw dS dL dP\n"
       "          (These are the forms output by the '-dfile' and\n"
       "           '-1Dfile' options of program 3dvolreg; see that\n"
       "           program's -help output for the hideous details.)\n"
       "    The n-th sub-brick of the input dataset will be transformed\n"
       "    using the parameters from the n-th line of the dname file.\n"
       "    If the dname file doesn't contain as many lines as the\n"
       "    input dataset has sub-bricks, then the last dname line will\n"
       "    be used for all subsequent sub-bricks.  Excess columns or\n"
       "    rows will be ignored.\n"
       "  N.B.: Rotation is always about the center of the volume.\n"
       "          If the parameters are derived from a 3dvolreg run\n"
       "          on a dataset with a different center in xyz-space,\n"
       "          the results may not be what you want!\n"
       "  N.B.: You can't use -dfile/-1Dfile with -points (infra).\n"
       "\n"
       "POINTS OPTIONS (instead of datasets):\n"
       "------------------------------------\n"
       " -points\n"
       " -origin xo yo zo\n"
       "   These options specify that instead of rotating a dataset, you will\n"
       "   be rotating a set of (x,y,z) points.  The points are read from stdin.\n"
       "   * If -origin is given, the point (xo,yo,zo) is used as the center for\n"
       "     the rotation.\n"
       "   * If -origin is NOT given, and a dataset is given at the end of the\n"
       "     command line, then the center of the dataset brick is used as\n"
       "     (xo,yo,zo).  The dataset will NOT be rotated if -points is given.\n"
       "   * If -origin is NOT given, and NO dataset is given at the end of the\n"
       "     command line, then xo=yo=zo=0 is assumed.  You probably don't\n"
       "     want this.\n"
       "   * (x,y,z) points are read from stdin as 3 ASCII-formatted numbers per\n"
       "     line, as in 3dUndump.  Any succeeding numbers on input lines will\n"
       "     be copied to the output, which will be written to stdout.\n"
       "   * The input (x,y,z) coordinates are taken in the same order as the\n"
       "     axes of the input dataset.  If there is no input dataset, then\n"
       "       negative x = R  positive x = L  }\n"
       "       negative y = A  positive y = P  } e.g., the DICOM order\n"
       "       negative z = I  positive z = S  }\n"
       "     One way to dump some (x,y,z) coordinates from a dataset is:\n"
       "\n"
       "      3dmaskdump -mask something+tlrc -o xyzfilename -noijk\n"
       "                 '3dcalc( -a dset+tlrc -expr x -datum float )'\n"
       "                 '3dcalc( -a dset+tlrc -expr y -datum float )'\n"
       "                 '3dcalc( -a dset+tlrc -expr z -datum float )'\n"
       "\n"
       "     (All of this should be on one command line.)\n"
       "============================================================================\n"
       "\n"
       "Example: 3drotate -prefix Elvis -bshift 10S 0 0 -rotate 30R 0 0 Sinatra+orig\n"
       "\n"
       "This will shift the input 10 mm in the superior direction, followed by a 30\n"
       "degree rotation about the Right-to-Left axis (i.e., nod the head forward).\n"
       "\n"
       "============================================================================\n"
       "Algorithm: The rotation+shift is decomposed into 4 1D shearing operations\n"
       "           (a 3D generalization of Paeth's algorithm).  The interpolation\n"
       "           (i.e., resampling) method used for these shears can be controlled\n"
       "           by the following options:\n"
       "\n"
       " -Fourier = Use a Fourier method (the default: most accurate; slowest).\n"
       " -NN      = Use the nearest neighbor method.\n"
       " -linear  = Use linear (1st order polynomial) interpolation (least accurate).\n"
       " -cubic   = Use the cubic (3rd order) Lagrange polynomial method.\n"
       " -quintic = Use the quintic (5th order) Lagrange polynomial method.\n"
       " -heptic  = Use the heptic (7th order) Lagrange polynomial method.\n"
       "\n"
       " -Fourier_nopad = Use the Fourier method WITHOUT padding\n"
       "                * If you don't mind - or even want - the wraparound effect\n"
       "                * Works best if dataset grid size is a power of 2, possibly\n"
       "                  times powers of 3 and 5, in all directions being altered.\n"
       "                * The main use would seem to be to un-wraparound poorly\n"
       "                  reconstructed images, by using a shift; for example:\n"
       "                   3drotate -ashift 30A 0 0 -Fourier_nopad -prefix Anew A+orig\n"
       "                * This option is also available in the Nudge Dataset plugin.\n"
       "\n"
       " -clipit  = Clip results to input brick range [now the default].\n"
       " -noclip  = Don't clip results to input brick range.\n"
       "\n"
       " -zpad n  = Zeropad around the edges by 'n' voxels during rotations\n"
       "              (these edge values will be stripped off in the output)\n"
       "        N.B.: Unlike to3d, in this program '-zpad' adds zeros in\n"
       "               all directions.\n"
       "        N.B.: The environment variable AFNI_ROTA_ZPAD can be used\n"
       "               to set a nonzero default value for this parameter.\n"
      ) ;

      printf("\n" MASTER_SHORTHELP_STRING ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3drotate main"); machdep(); PRINT_VERSION("3drotate"); AUTHOR("RW Cox");

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3drotate",argc,argv) ;

#define ERREX(str) (fprintf(stderr,"*** %s\n",str),exit(1))

   iopt = 1 ;
   while( iopt < argc && argv[iopt][0] == '-' ){

      if( strncmp(argv[iopt],"-zpad",5) == 0 ){     /* 05 Feb 2001 */
         if( zpad > 0 )
            fprintf(stderr,"+++ WARNING: second -zpad option!\n") ;
         zpad = (int) strtod( argv[++iopt] , NULL ) ;
         if( zpad < 0 ){
            fprintf(stderr,"*** ERROR: Can't use -zpad %d\n",zpad) ;
            exit(1) ;
         }
         THD_rota_setpad(zpad,zpad,zpad) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-points",7) == 0 ){   /* 21 Nov 2000 */
        dopoints = 1 ;
        iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-origin",7) == 0 ){   /* 21 Nov 2000 */
        xo = strtod( argv[++iopt] , NULL ) ;
        yo = strtod( argv[++iopt] , NULL ) ;
        zo = strtod( argv[++iopt] , NULL ) ;
        doorigin = 1 ;
        iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-rotpar",7) == 0 ){  /* 07 Feb 2001 */

         ATR_float *atr ;

         if( rotpar_dset != NULL )
            ERREX("*** Can't use 2 -rotparent options!") ;
         if( matvec )
            ERREX("*** Can't combine -rotparent with -matvec!") ;
         if( dcode > 0 || rotarg > 0 )
            ERREX("*** Can't use -rotparent with -shift or -rotate options!") ;
         if( dname != NULL )
            ERREX("*** Can't use -rotparent with -dfile or -1Dfile options!") ;

         rotpar_dset = THD_open_one_dataset( argv[++iopt] ) ;
         if( rotpar_dset == NULL )
            ERREX("*** Can't open -rotparent dataset!\n") ;

         atr = THD_find_float_atr( rotpar_dset->dblk , "VOLREG_MATVEC_000000" ) ;
         if( atr == NULL || atr->nfl < 12 )
            ERREX("*** -rotparent dataset doesn't have VOLREG attributes!?") ;

         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-gridpar",7) == 0 ){  /* 07 Feb 2001 */

         if( gridpar_dset != NULL )
            ERREX("*** Can't use -2 -gridparent options!") ;

         gridpar_dset = THD_open_one_dataset( argv[++iopt] ) ;
         if( gridpar_dset == NULL )
            ERREX("*** Can't open -gridparent dataset!\n") ;

         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-matvec_",8) == 0 ){  /* 19 Jul 2000 */

         MRI_IMAGE *matim ; float *matar , sum ;

         if( matvec )
           ERREX("*** Can't use 2 -matvec options!") ;
         if( dcode > 0 || rotarg > 0 )
           ERREX("*** Can't use -matvec with -shift or -rotate options!") ;
         if( rotpar_dset != NULL )
           ERREX("*** Can't use -matvec with -rotparent option!") ;
         if( dname != NULL )
           ERREX("*** Can't use -matvec with -dfile or -1Dfile options!") ;

         if( strcmp(argv[iopt],"-matvec_order") == 0 ) matvec = MATVEC_ORDER ;
         else                                          matvec = MATVEC_DICOM ;

         if( strcmp(argv[iopt],"-matvec_dset") == 0){   /* 20 July 2000 */
            THD_3dim_dataset * mvset ; ATR_float * atr ;

            mvset = THD_open_dataset( argv[++iopt] ) ;
            if( mvset == NULL ) ERREX("*** Can't read -matvec_dset dataset!") ;
            atr = THD_find_float_atr( mvset->dblk , "TAGALIGN_MATVEC" ) ;
            if( atr == NULL || atr->nfl < 12 )
              ERREX("*** -matvec_dset doesn't have matrix+vector in .HEAD!") ;
            matar = atr->fl ;
            LOAD_DMAT(rmat,matar[0],matar[1],matar[2],
                           matar[4],matar[5],matar[6],
                           matar[8],matar[9],matar[10] ) ;
            LOAD_DFVEC3(tvec,matar[3],matar[7],matar[11]) ;
            DSET_delete(mvset) ;
         } else {
            matim = mri_read_ascii( argv[++iopt] ) ;
            if( matim == NULL ) ERREX("Can't read -matvec file!") ;
            if( matim->nx != 4 || matim->ny != 3 ) ERREX("-matvec file not 4x3!") ;

            matar = MRI_FLOAT_PTR(matim) ;
            LOAD_DMAT(rmat,matar[0],matar[1],matar[2],
                          matar[4],matar[5],matar[6],
                          matar[8],matar[9],matar[10] ) ;
            LOAD_DFVEC3(tvec,matar[3],matar[7],matar[11]) ;

            mri_free(matim) ;
         }

         /* check if matrix is approximately orthogonal */
         /* [will be orthogonalized in rot_to_shear_matvec() in thd_shear3d.c] */

         pp = TRANSPOSE_DMAT(rmat) ; pp = DMAT_MUL(pp,rmat) ;
         sum = fabs(pp.mat[0][0]-1.0)+fabs(pp.mat[1][0])    +fabs(pp.mat[2][0])
              +fabs(pp.mat[0][1])    +fabs(pp.mat[1][1]-1.0)+fabs(pp.mat[2][1])
              +fabs(pp.mat[0][2])    +fabs(pp.mat[1][2])    +fabs(pp.mat[2][2]-1.0);
         if( sum > 0.01 ) ERREX("-matvec matrix not orthogonal!") ;

         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-clipit",4) == 0 ){  /* 11 Apr 2000 */
         fprintf(stderr,"++ Notice: -clipit is now the default\n") ;
         clipit = 1 ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-noclip",4) == 0 ){  /* 16 Apr 2002 */
         clipit = 0 ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-prefix",4) == 0 ){
         new_prefix = argv[++iopt] ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-verbose",5) == 0 ){
         verb = 1 ;
         iopt++ ; continue ;
      }

      if( strcmp(argv[iopt],"-Fourier_nopad") == 0 ){   /* 13 May 2003 */
         THD_rota_method( MRI_FOURIER_NOPAD ) ;
         iopt++ ; continue ;
      }
      if( strncmp(argv[iopt],"-Fourier",4) == 0 || strncmp(argv[iopt],"-fourier",4) == 0 ){
         THD_rota_method( MRI_FOURIER ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-cubic",4) == 0 || strncmp(argv[iopt],"-Cubic",4) == 0 ){
         THD_rota_method( MRI_CUBIC ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-quintic",4) == 0 || strncmp(argv[iopt],"-Quintic",4) == 0 ){
         THD_rota_method( MRI_QUINTIC ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-heptic",4) == 0 || strncmp(argv[iopt],"-Heptic",4) == 0 ){
         THD_rota_method( MRI_HEPTIC ) ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-linear",4) == 0 || strncmp(argv[iopt],"-Linear",4) == 0 ){
         THD_rota_method( MRI_LINEAR ) ; clipit = 0 ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-nn",3) == 0 || strncmp(argv[iopt],"-NN",4) == 0 ){
         THD_rota_method( MRI_NN ) ; clipit = 0 ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-ashift",4) == 0 ){
         if( matvec    ) ERREX("*** Can't use -ashift with -matvec!") ;
         if( dcode > 0 ){fprintf(stderr,"*** Can't use 2 shift options!\n");exit(1);}
         if( rotpar_dset != NULL )
            ERREX("*** Can't use -ashift with -rotparent!") ;
         if( dname != NULL )
            ERREX("*** Can't use -ashift with -dfile or -1Dfile options!") ;
         dx = strtod( argv[++iopt] , &cpt ) ; cdx = *cpt ;
         dy = strtod( argv[++iopt] , &cpt ) ; cdy = *cpt ;
         dz = strtod( argv[++iopt] , &cpt ) ; cdz = *cpt ;
         dcode = DELTA_AFTER ;
         iopt++ ; continue ;
      }

      if( strncmp(argv[iopt],"-bshift",4) == 0 ){
         if( matvec    ) ERREX("*** Can't use -bshift with -matvec!") ;
         if( dcode > 0 ){fprintf(stderr,"*** Can't use 2 shift options!\n");exit(1);}
         if( rotpar_dset != NULL )
            ERREX("*** Can't use -bshift with -rotparent!") ;
         if( dname != NULL )
            ERREX("*** Can't use -bshift with -dfile or -1Dfile options!") ;
         dx = strtod( argv[++iopt] , &cpt ) ; cdx = *cpt ;
         dy = strtod( argv[++iopt] , &cpt ) ; cdy = *cpt ;
         dz = strtod( argv[++iopt] , &cpt ) ; cdz = *cpt ;
         dcode = DELTA_BEFORE ;
         iopt++ ; continue ;
      }

#if 0
      if( strncmp(argv[iopt],"-cshift",4) == 0 ){
         if( dcode > 0 ){fprintf(stderr,"*** Can't use 2 shift options!\n");exit(1);}
         dx = strtod( argv[++iopt] , &cpt ) ; cdx = *cpt ;
         dy = strtod( argv[++iopt] , &cpt ) ; cdy = *cpt ;
         dz = strtod( argv[++iopt] , &cpt ) ; cdz = *cpt ;
         dcode = DELTA_FIXED ;
         iopt++ ; continue ;
      }
#endif

      if( strncmp(argv[iopt],"-rotate",4) == 0 ){
         if( matvec     ) ERREX("*** Can't use -rotate with -matvec!") ;
         if( rotarg > 0 ) ERREX("*** Can't have 2 -rotate options!\n") ;
         if( rotpar_dset != NULL )
            ERREX("*** Can't use -rotate with -rotparent!") ;
         if( dname != NULL )
            ERREX("*** Can't use -rotate with -dfile or -1Dfile options!") ;
         rotarg = iopt ;  /* save and process later */
         iopt += 4 ; continue ;
      }

      /*-- 19 Jun 2001 --*/

      if( strcmp(argv[iopt],"-dfile")==0 || strcmp(argv[iopt],"-1Dfile")==0 ){

         if( dname != NULL )
            ERREX("*** Can't use 2 -dfile/-1Dfile options!") ;
         if( matvec )
            ERREX("*** Can't combine -dfile/-1Dfile with -matvec!") ;
         if( dcode > 0 || rotarg > 0 )
            ERREX("*** Can't use -dfile/-1Dfile with -shift or -rotate options!") ;
         if( rotpar_dset != NULL )
            ERREX("*** Can't use -dfile/-1Dfile with -rotparent!") ;

              if( strcmp(argv[iopt],"-dfile") ==0 ) dmode = MODE_DFILE  ;
         else if( strcmp(argv[iopt],"-1Dfile")==0 ) dmode = MODE_1DFILE ;

         dname = argv[++iopt] ;

         dim = get_dfile_params( dname , dmode ) ;
         if( dim == NULL )
            ERREX("*** Error with -dfile or -1Dfile!") ;

         dar  = MRI_FLOAT_PTR(dim) ; /* 6 x ndar array */
         ndar = dim->ny ;
         iopt++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[iopt]) ; exit(1) ;
   }

   /*-- check for legal combinations --*/

   if( gridpar_dset != NULL && rotpar_dset == NULL ){
      fprintf(stderr,"+++ WARNING: -gridparent means nothing without -rotparent!\n");
      DSET_delete( gridpar_dset ) ;
      gridpar_dset = NULL ;
   }

   if( gridpar_dset != NULL && dopoints ){
      fprintf(stderr,"+++ WARNING: -gridparent means nothing with -points!\n") ;
      DSET_delete( gridpar_dset ) ;
      gridpar_dset = NULL ;
   }

   if( doorigin && !dopoints ){
      fprintf(stderr,"+++ WARNING: -origin means nothing without -points!\n") ;
   }

   if( dar != NULL && dopoints )
     ERREX("*** Can't combine -dfile/-1Dfile with -points!") ;

   if( matvec==0 && dcode<0 && rotarg<0 && rotpar_dset==NULL && dar==NULL )
     ERREX("Don't you want to do anything [no -rotate,-shift,-matvec,-rotparent,-dfile]?");

   /** read input dataset */

   if( iopt >= argc && !dopoints ) ERREX("*** No input dataset?") ;

   if( iopt < argc ){
      dset = THD_open_dataset( argv[iopt] ) ;
      if( dset == NULL ){
         fprintf(stderr,"*** Cannot open dataset %s!\n",argv[iopt]); exit(1);
      }
   } else {
      dset = EDIT_empty_copy(NULL) ;  /* 21 Nov 2000: need a fake dataset */
      if( !doorigin )
         fprintf(stderr,"+++ WARNING: no -origin and no input dataset on command line!\n") ;
   }

   if( dopoints && !doorigin ){
      xo = dset->daxes->xxorg + 0.5*(dset->daxes->nxx - 1)*dset->daxes->xxdel ;
      yo = dset->daxes->yyorg + 0.5*(dset->daxes->nyy - 1)*dset->daxes->yydel ;
      zo = dset->daxes->zzorg + 0.5*(dset->daxes->nzz - 1)*dset->daxes->zzdel ;
      fprintf(stderr,"+++ Using -origin %g %g %g\n",xo,yo,zo) ;
   }

   /* now can process rotation arguments */

   ihand = THD_handedness(dset) ;

   if( rotarg > 0 ){
      int neg ;
      iopt = rotarg ;

      th1 = (PI/180.0) * strtod( argv[++iopt] , &cpt ) ;
      switch( *cpt ){
         default: ERROR_exit("Illegal code after th1 in -rotate\n");

         case '\0': case 'x': case 'X': ax1 = 0 ; neg = 0 ; break ;
                    case 'y': case 'Y': ax1 = 1 ; neg = 0 ; break ;
                    case 'z': case 'Z': ax1 = 2 ; neg = 0 ; break ;

         case 'A': case 'P':
         case 'R': case 'L':
         case 'I': case 'S': ax1 = THD_axcode(dset,*cpt) ;
                             neg = (ax1 < 0) ;
                             ax1 = abs(ax1) - 1 ; break ;
      }
      if( neg ) th1 = -th1 ;

      th2 = (PI/180.0) * strtod( argv[++iopt] , &cpt ) ;
      switch( *cpt ){
         default: ERROR_exit("Illegal code after th2 in -rotate\n");

                    case 'x': case 'X': ax2 = 0 ; break ;
         case '\0': case 'y': case 'Y': ax2 = 1 ; break ;
                    case 'z': case 'Z': ax2 = 2 ; break ;

         case 'A': case 'P':
         case 'R': case 'L':
         case 'I': case 'S': ax2 = THD_axcode(dset,*cpt) ;
                             neg = (ax2 < 0) ;
                             ax2 = abs(ax2) - 1 ; break ;
      }
      if( neg ) th2 = -th2 ;

      th3 = (PI/180.0) * strtod( argv[++iopt] , &cpt ) ;
      switch( *cpt ){
         default: ERROR_exit("Illegal code after th3 in -rotate\n");

                    case 'x': case 'X': ax3 = 0 ; break ;
                    case 'y': case 'Y': ax3 = 1 ; break ;
         case '\0': case 'z': case 'Z': ax3 = 2 ; break ;

         case 'A': case 'P':
         case 'R': case 'L':
         case 'I': case 'S': ax3 = THD_axcode(dset,*cpt) ;
                             neg = (ax3 < 0) ;
                             ax3 = abs(ax3) - 1 ; break ;
      }
      if( neg ) th3 = -th3 ;

      if( th1 == 0.0 && th2 == 0.0 && th3 == 0.0 )
        INFO_message("All angles after -rotate are 0!") ;

      if( ax1 < 0 || ax1 > 2 || ax2 < 0 || ax2 > 2 || ax3 < 0 || ax3 > 2 )
        ERROR_exit("Can't understand axes codes in -rotate!") ;

      if( ihand < 0 ){ th1 = -th1 ; th2 = -th2 ; th3 = -th3 ; }

#if 0
fprintf(stderr,"ihand=%d th1=%g th2=%g th3=%g\n",ihand,th1,th2,th3);
fprintf(stderr,"ax1=%d ax2=%d ax3=%d\n",ax1,ax2,ax3) ;
#endif

      /* notice the minus signs on the angles ------+ */
      /*                          |        |        | */
      if( dopoints )          /*  V        V        V */
        rmat = rot_to_matrix( ax1,-th1,ax2,-th2,ax3,-th3 ) ; /* 21 Nov 2000 */
   }

   /* may need to process shift arguments as well */

   if( dcode > 0 && (cdx != '\0' || cdy != '\0' || cdz != '\0') ){
     float qdx=0 , qdy=0 , qdz=0 ;

     adx = THD_axcode(dset,cdx) ;
     if( adx > -99 || dx != 0.0 ){      /* 29 Jan 2001: skip if purely 0 */
       switch( adx ){
         case  1: qdx = -dx ; break ;
         default:
         case -1: qdx =  dx ; break ;
         case  2: qdy = -dx ; break ;
         case -2: qdy =  dx ; break ;
         case  3: qdz = -dx ; break ;
         case -3: qdz =  dx ; break ;
       }
     }

     ady = THD_axcode(dset,cdy) ;
     if( ady > -99 || dy != 0.0 ){      /* 29 Jan 2001 */
       switch( ady ){
         case  1: qdx = -dy ; break ;
         case -1: qdx =  dy ; break ;
         case  2: qdy = -dy ; break ;
         default:
         case -2: qdy =  dy ; break ;
         case  3: qdz = -dy ; break ;
         case -3: qdz =  dy ; break ;
       }
     }

     adz = THD_axcode(dset,cdz) ;
     if( adz > -99 || dz != 0.0 ){      /* 29 Jan 2001 */
       switch( adz ){
         case  1: qdx = -dz ; break ;
         case -1: qdx =  dz ; break ;
         case  2: qdy = -dz ; break ;
         case -2: qdy =  dz ; break ;
         case  3: qdz = -dz ; break ;
         default:
         case -3: qdz =  dz ; break ;
       }
     }

     if( verb )
       fprintf(stderr,"++ Shifting parameters:\n"
                      "++   direction codes: adx=%d ady=%d adz=%d\n"
                      "++   input values:     dx=%g  dy=%g  dz=%g\n"
                      "++   output values:   qdx=%g qdy=%g qdz=%g\n" ,
               adx,ady,adz , dx,dy,dz , qdx,qdy,qdz ) ;

     dx = qdx ; dy = qdy ; dz = qdz ;
   }

   /*- 19 July 2000: now can deal with -matvec_dicom case -*/

   if( matvec == MATVEC_DICOM ){           /* convert matrix/vector  */
     pp   = DBLE_mat_to_dicomm( dset ) ;   /* to dataset coord order */
     ppt  = TRANSPOSE_DMAT(pp);            /* from the DICOM order!  */
     rmat = DMAT_MUL(ppt,rmat); rmat = DMAT_MUL(rmat,pp);
     tvec = DMATVEC(ppt,tvec);
   }

   /*-- 07 Feb 2001: deal with -rotparent and -gridparent case
                     [very similar to -matvec_dicom, actually] --*/

   if( gridpar_dset != NULL ){
      int mm , nz_gp , nz_ds ;

      /* check for compatibility! */

      mm = THD_dataset_mismatch( gridpar_dset , dset ) ;
      if( mm & (MISMATCH_DELTA | MISMATCH_ORIENT) ){
         fprintf(stderr,"*** Fatal Error:\n"
                        "*** -gridparent dataset and input dataset don't\n"
                        "*** match in grid spacing and/or orientation!\n"  ) ;
         exit(1) ;
      }

      if( DSET_NX(gridpar_dset) != DSET_NX(dset) ||
          DSET_NY(gridpar_dset) != DSET_NY(dset)   ){

         fprintf(stderr,"*** Fatal Error:\n"
                        "*** -gridparent and input datasets\n"
                        "*** don't match in x,y dimensions!\n" ) ;
         exit(1) ;
      }

      /* check for zero padding */

      nz_gp = DSET_NZ(gridpar_dset) ; nz_ds = DSET_NZ(dset) ;

      if( nz_gp < nz_ds ){
         fprintf(stderr,"*** Fatal Error:\n"
                        "*** -gridparent has fewer slices than input dataset!\n") ;
         exit(1) ;
      }

      if( nz_gp > nz_ds ){
         int npad1 = (nz_gp - nz_ds) / 2 ;
         int npad2 = (nz_gp - nz_ds) - npad1 ;
         int add_I=0, add_S=0, add_A=0, add_P=0, add_L=0, add_R=0 ;
         THD_3dim_dataset * pset ;
         char *sp1,*sp2 ;

         /* where to add slices? and how many? */

         switch( dset->daxes->zzorient ){
            case ORI_R2L_TYPE:
            case ORI_L2R_TYPE: add_R=npad1; add_L=npad2; sp1="R"; sp2="L"; break;

            case ORI_P2A_TYPE:
            case ORI_A2P_TYPE: add_A=npad1; add_P=npad2; sp1="A"; sp2="P"; break;

            case ORI_I2S_TYPE:
            case ORI_S2I_TYPE: add_I=npad1; add_S=npad2; sp1="I"; sp2="S"; break;
         }

         /* add them on */

         if( verb )
            fprintf(stderr,"+++ Zero padding to match -gridparent: -%s %d  -%s %d\n",
                    sp1,npad1,sp2,npad2 ) ;

         pset = THD_zeropad( dset,
                             add_I,add_S,add_A,add_P,add_L,add_R,
                             "Elvis" , ZPAD_PURGE ) ;

         if( pset == NULL ){
            fprintf(stderr,"*** Fatal Error:\n"
                           "*** Can't properly zeropad input dataset!\n" ) ;
            exit(1) ;
         }

         /* toss input datset, replace with padded one */

         DSET_delete(dset) ; dset = pset ;
      }
   }

   if( rotpar_dset != NULL ){  /* compute transformation from -rotparent */
      ATR_float * atr ;
      float * matar , sum ;
      THD_fvec3 fv ;
      THD_dfvec3 dv,ev,qv , cv_e2, cv_e1, cv_s1, cv_s2 ;

      /* load transformation from rotparent */

      atr = THD_find_float_atr( rotpar_dset->dblk , "VOLREG_MATVEC_000000" ) ;
      matar = atr->fl ;
      LOAD_DMAT(rmat,matar[0],matar[1],matar[2],
                     matar[4],matar[5],matar[6],
                     matar[8],matar[9],matar[10] ) ;
      LOAD_DFVEC3(tvec,matar[3],matar[7],matar[11]) ;

      /* check if matrix is orthogonal */

      pp = TRANSPOSE_DMAT(rmat) ; pp = DMAT_MUL(pp,rmat) ;
      sum = fabs(pp.mat[0][0]-1.0)+fabs(pp.mat[1][0])    +fabs(pp.mat[2][0])
           +fabs(pp.mat[0][1])    +fabs(pp.mat[1][1]-1.0)+fabs(pp.mat[2][1])
           +fabs(pp.mat[0][2])    +fabs(pp.mat[1][2])    +fabs(pp.mat[2][2]-1.0);
      if( sum > 0.01 ) ERREX("-rotparent matrix not orthogonal!") ;

      /* must alter shift [tvec] to allow for differing
         coordinates in the rotparent, gridparent, and input datasets */

      /* cv_e2 = center of input dataset [Dicom coordinates] */

      fv = THD_dataset_center( dset ) ;       /* dataset coords  */
      FVEC3_TO_DFVEC3( fv , cv_e2 ) ;         /* convert to double */

      /* cv_e1 = center of gridparent */

      if( gridpar_dset != NULL ){
         fv = THD_dataset_center( gridpar_dset ) ;
         FVEC3_TO_DFVEC3( fv , cv_e1 ) ;
      } else {
         cv_e1 = cv_e2 ;  /* what else to do? */
      }

      /* cv_s2 = center of rotation in rotparent */

      atr = THD_find_float_atr( rotpar_dset->dblk , "VOLREG_CENTER_OLD" ) ;
      LOAD_DFVEC3( cv_s2 , atr->fl[0] , atr->fl[1] , atr->fl[2] ) ;

      /* cv_s1 = center of base dataset for rotparent */

      atr = THD_find_float_atr( rotpar_dset->dblk , "VOLREG_CENTER_BASE" ) ;
      LOAD_DFVEC3( cv_s1 , atr->fl[0] , atr->fl[1] , atr->fl[2] ) ;

      /* compute extra shift due to difference in
         center of rotation between rotparent and input dataset,
         then add in shifts caused by -twodup for rotparent and input */

      dv = SUB_DFVEC3( cv_e2 , cv_s2 ) ;
      ev = DMATVEC( rmat , dv ) ;         /* R[E2-S2]         */

      dv = ev ;  /* vestige of a stupid bug, since fixed */

      ev = SUB_DFVEC3( cv_e1 , cv_s1 ) ;  /* E1-S1            */

      qv = SUB_DFVEC3( dv , ev ) ;        /* R[E2-S2] + S1-E1 */

      tvec = ADD_DFVEC3( tvec , qv ) ;    /* shifted translation vector */

      /* convert transformation from Dicom to dataset coords */

      pp   = DBLE_mat_to_dicomm( dset ) ;
      ppt  = TRANSPOSE_DMAT(pp);
      rmat = DMAT_MUL(ppt,rmat); rmat = DMAT_MUL(rmat,pp); tvec = DMATVEC(ppt,tvec);

      /* modify origin of output dataset to match -gridparent */

      if( gridpar_dset != NULL ){
         dset->daxes->xxorg = gridpar_dset->daxes->xxorg ;
         dset->daxes->yyorg = gridpar_dset->daxes->yyorg ;
         dset->daxes->zzorg = gridpar_dset->daxes->zzorg ;

         /* 12 Feb 2001: adjust origin of time-offsets as well */

         if( dset->taxis != NULL && dset->taxis->nsl > 0 ){
            dset->taxis->zorg_sl = dset->daxes->zzorg ;
         }
      }

      matvec = MATVEC_ORDER ;  /* flag that transform comes from rmat/tvec */
   }

   /*-- 21 Nov 2000: read (x,y,z) points from stdin, process them, quit --*/

   if( dopoints ){
     THD_dfvec3 xyzorg ;
     if( !matvec ) LOAD_DFVEC3( tvec , dx,dy,dz ) ;
     if( dcode < 0 ) dcode = DELTA_AFTER ;
     LOAD_DFVEC3(xyzorg,xo,yo,zo) ;
     rotate_stdin_points( xyzorg , rmat , dcode,tvec ) ;  /* at end of file */
     exit(0) ;
   }

   /*-- 12 Feb 2001: adjust time-offsets for slice direction shifts --*/

   if( dset->taxis != NULL && dset->taxis->nsl > 0 ){
     int ndz ;
     int kk,jj , nsl = dset->taxis->nsl ;

     if( matvec )
       ndz = (int) rint( tvec.xyz[2] / fabs(dset->daxes->zzdel) ) ; /* shift */
     else
       ndz = (int) rint( dz / fabs(dset->daxes->xxdel) ) ;

     if( ndz != 0 ){
       float * tsl = (float *)malloc(sizeof(float)*nsl) ;
       for( kk=0 ; kk < nsl ; kk ++ ){
         jj = kk - ndz ;
         if( jj < 0 || jj >= nsl ) tsl[kk] = 0.0 ;
         else                      tsl[kk] = dset->taxis->toff_sl[jj] ;
       }
       EDIT_dset_items( dset , ADN_toff_sl , tsl , ADN_none ) ;
       free(tsl) ;
       if( verb )
         fprintf(stderr,"+++ adjusting time-offsets by %d slices\n",ndz) ;
     }
   }

   /*- read dataset, prepare to process it, write back out (with new name) -*/

   DSET_mallocize(dset) ;
   DSET_load(dset) ;
   dset->dblk->diskptr->storage_mode = STORAGE_BY_BRICK ; /* 14 Jan 2004 */

   dset->idcode = MCW_new_idcode() ;  /* 08 Jun 1999 - is a new dataset */
   EDIT_dset_items( dset ,
                       ADN_prefix , new_prefix ,
                       ADN_label1 , new_prefix ,
                    ADN_none ) ;
   if( THD_deathcon() && THD_is_file(dset->dblk->diskptr->header_name) ){
     fprintf(stderr,
             "** ERROR: Output file %s already exists -- cannot continue!\n",
             dset->dblk->diskptr->header_name ) ;
     exit(1) ;
   }

   /* old history is already in the dataset */

   tross_Make_History( "3drotate" , argc,argv , dset ) ;

   nvox = DSET_NVOX(dset) ;
   fvol = (float *) malloc( sizeof(float) * nvox ) ;

   nval = DSET_NVALS(dset) ;
   if( verb ){
     fprintf(stderr,"+++ %d sub-bricks: ",nval) ;
     cputim = COX_cpu_time() ;
   }

   /* 03 May 2005: save rotation center */

   { THD_fvec3 cv ;
     cv = THD_dataset_center( dset ) ;
     THD_set_float_atr( dset->dblk , "ROTATE_CENTER_OLD"  , 3 , cv.xyz ) ;
     THD_set_float_atr( dset->dblk , "ROTATE_CENTER_BASE" , 3 , cv.xyz ) ;
   }

   /*-- loop over all sub-bricks: copy into fvol, rotate fvol, copy back --*/

   for( ival=0 ; ival < nval ; ival++ ){

     if( verb ) fprintf(stderr,"%d",ival) ;

     EDIT_coerce_type( nvox ,
                       DSET_BRICK_TYPE(dset,ival),DSET_ARRAY(dset,ival) ,
                       MRI_float,fvol ) ;

     if( verb ) fprintf(stderr,".") ;

     if( clipit ){                                 /* 11 Apr 2000 */
       register int ii ; register float bb,tt ;
       bb = tt = fvol[0] ;
       for( ii=1 ; ii < nvox ; ii++ ){
               if( fvol[ii] < bb ) bb = fvol[ii] ;
          else if( fvol[ii] > tt ) tt = fvol[ii] ;
       }
       cbot = bb ; ctop = tt ;
     }

     /* 19 Jun 2001: get matrix/vector from rot params in file */

     skipit = 0 ;
     if( dar != NULL ){
       THD_dvecmat dvm ; char rotcom[256] ; int jj=ival ;
       static int ndar_over=0 ;

       if( jj >= ndar ){
         jj = ndar-1 ;
         if( ndar_over == 0 )
           WARNING_message("from brick %d on, using last line (%d) from %s\n",
                           jj , ndar-1 , dname ) ;
         ndar_over++ ;
       }

       sprintf(rotcom,"-rotate %.4fI %.4fR %.4fA -ashift %.4fS %.4fL %.4fP",
               dar[0+6*jj] , dar[1+6*jj] , dar[2+6*jj] ,
               dar[3+6*jj] , dar[4+6*jj] , dar[5+6*jj]  ) ;

       dvm = THD_rotcom_to_matvec( dset , rotcom ) ; /* thd_rotangles.c */

       rmat = dvm.mm ; tvec = dvm.vv ; matvec = MATVEC_ORDER ;

       skipit = (dar[0+6*jj]==0.0 && dar[1+6*jj]==0.0 && dar[2+6*jj]==0.0 &&
                 dar[3+6*jj]==0.0 && dar[4+6*jj]==0.0 && dar[5+6*jj]==0.0  );

       if( !skipit ){
         skipit = ( fabs(rmat.mat[0][0]-1.0) < 0.00001 ) &&
                  ( fabs(rmat.mat[1][1]-1.0) < 0.00001 ) &&
                  ( fabs(rmat.mat[2][2]-1.0) < 0.00001 ) &&
                  ( fabs(tvec.xyz[0])        < 0.001   ) &&
                  ( fabs(tvec.xyz[1])        < 0.001   ) &&
                  ( fabs(tvec.xyz[2])        < 0.001   )    ;
       }

       if( skipit ) fprintf(stderr,"[Matrix near identity, skipping]");
     }

     /** carry out the rotation **/

     if( !skipit ){
      if( matvec ){
       THD_rota_vol_matvec( DSET_NX(dset) , DSET_NY(dset) , DSET_NZ(dset) ,
                            fabs(DSET_DX(dset)) ,
                             fabs(DSET_DY(dset)) ,
                              fabs(DSET_DZ(dset)) ,
                            fvol , rmat , tvec ) ;
      } else {
       THD_rota_vol( DSET_NX(dset) , DSET_NY(dset) , DSET_NZ(dset) ,
                     fabs(DSET_DX(dset)),
                      fabs(DSET_DY(dset)),
                       fabs(DSET_DZ(dset)), fvol ,
                     ax1,th1, ax2,th2, ax3,th3, dcode,dx,dy,dz ) ;

       /* 02 May 2005: save matrix+vector for recording below */

       rmat = rot_to_matrix( ax1,-th1,ax2,-th2,ax3,-th3 ) ;
       LOAD_DFVEC3( tvec , dx,dy,dz ) ;
       if( dcode == DELTA_BEFORE ) tvec = DMATVEC(rmat,tvec) ;
      }
     }

     /** 02 May 2005: record operation in header **/

     { float matar[12] ;
       THD_dmat33 drmat ;
       THD_dfvec3 dvec ;
       char anam[64] ;

       /* must transform [rmat,tvec] from dataset to DICOM coords */
       /* (the inverse of the DICOM to dataset transforms earlier) */

       pp    = DBLE_mat_to_dicomm(dset) ; ppt = TRANSPOSE_DMAT(pp) ;
       drmat = DMAT_MUL(pp,rmat) ; drmat = DMAT_MUL(drmat,ppt);
       dvec  = DMATVEC (pp,tvec) ;

       UNLOAD_DMAT(drmat,matar[0],matar[1],matar[2],
                         matar[4],matar[5],matar[6],
                         matar[8],matar[9],matar[10] ) ;
       UNLOAD_DFVEC3(dvec,matar[3],matar[7],matar[11]) ;
       sprintf(anam,"ROTATE_MATVEC_%06d",ival) ;
       THD_set_float_atr( dset->dblk , anam , 12 , matar ) ;
     }

     if( verb ) fprintf(stderr,".") ;

     if( clipit ){                                 /* 11 Apr 2000 */
       register int ii ; register float bb,tt ;
       bb = cbot ; tt = ctop ;
       for( ii=0 ; ii < nvox ; ii++ ){
              if( fvol[ii] < bb ) fvol[ii] = bb ;
         else if( fvol[ii] > tt ) fvol[ii] = tt ;
       }
     }

     EDIT_coerce_type( nvox , MRI_float,fvol ,
                              DSET_BRICK_TYPE(dset,ival),DSET_ARRAY(dset,ival) ) ;

   } /* end of loop over sub-bricks */

   if( verb ){
     cputim = COX_cpu_time() - cputim ;
     fprintf(stderr,"\n+++ CPU time=%10.3g s" , cputim) ;
     if( nval > 1 ) fprintf(stderr,"  [= %10.3g s/sub-brick]" , cputim/nval) ;
     fprintf(stderr,"\n+++ Writing dataset to disk in %s", DSET_BRIKNAME(dset) ) ;
   }

   dset->dblk->master_nvals = 0 ;  /* 11 Apr 2000 hack */
   DSET_write(dset) ;
   if( verb ) fprintf(stderr,"\n") ;
   exit(0) ;
}

/*=================================================================================*/

#include <ctype.h>

#define NBUF 1024  /* line buffer size */

#define DUPOUT(n) fprintf(fpout,"%s",linbuf+n)

static void rotate_stdin_points( THD_dfvec3 xyzorg, THD_dmat33 rmat,
                                                    int dcode, THD_dfvec3 tvec )
{
   char linbuf[NBUF] , *cp ;
   FILE *fpin=stdin , *fpout=stdout ;
   int ii , kk , nbuf , ll , nn , ld ;
   double xx,yy,zz ;
   THD_dfvec3 xyz ;

   if( verb ){
      DUMP_DMAT33("Rotation",rmat) ;
   }

   /** rmat = DMAT_INV(rmat) ; **/

   /*-- loop over input lines --*/

   ll = ld = 0 ;
   while(1){
      ll++ ;                               /* line count */
      cp = fgets( linbuf , NBUF , fpin ) ; /* read the line */
      if( cp == NULL ) break ;             /* end of file => end of loop */
      kk = strlen(linbuf) ;
      if( kk == 0 ) continue ;             /* empty line => get next line */

      /* find 1st nonblank */

      for( ii=0 ; ii < kk && isspace(linbuf[ii]) ; ii++ ) ;     /* nada */
      if( ii == kk ||                                           /* all blanks */
          (linbuf[ii] == '/' && linbuf[ii+1] == '/') ){         /* or comment */

         DUPOUT(0) ; continue ;
      }

      /* scan line for data */

      nn = sscanf(linbuf+ii , "%lf%lf%lf%n" , &xx,&yy,&zz,&nbuf ) ;
      if( nn < 3 ){
         fprintf(stderr,"+++ WARNING: input line %d was incomplete\n",ll) ;
         continue ;
      }
      nbuf += ii ;  /* position of next character after zz */

      /* process vector */

      LOAD_DFVEC3(xyz , xx,yy,zz) ;
      xyz = SUB_DFVEC3(xyz,xyzorg) ;
      if( dcode == DELTA_BEFORE ) xyz = ADD_DFVEC3(xyz,tvec) ;
      xyz = DMATVEC(rmat,xyz) ;
      if( dcode == DELTA_AFTER ) xyz = ADD_DFVEC3(xyz,tvec) ;
      xyz = ADD_DFVEC3(xyz,xyzorg) ;

      fprintf(fpout,"%g %g %g%s",xyz.xyz[0],xyz.xyz[1],xyz.xyz[2],linbuf+nbuf) ;
      ld++ ;

   } /* end of loop over input lines */

   if( verb )
      fprintf(stderr,"-points: read %d lines, wrote %d lines\n",ll-1,ld) ;
}

/*-----------------------------------------------------------------------
   19 Jun 2001:
     Get a 6 x N image of the rotation parameters
-------------------------------------------------------------------------*/

MRI_IMAGE * get_dfile_params( char * fname , int mode )
{
   MRI_IMAGE *outim , *flim ;
   float     *oar   , *far ;
   int nx,ny , ii,jj ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   flim = mri_read_ascii( fname ) ;
   if( flim == NULL ) return NULL ;

   nx = flim->nx ; ny = flim->ny ; far = MRI_FLOAT_PTR(flim) ;

   if( mode == MODE_DFILE ){  /* skip 1st element of each row */
      if( nx < 7 ){
         fprintf(stderr,"** -dfile %s has too few columns!\n",fname) ;
         mri_free(flim) ; return NULL ;
      }
      outim = mri_new( 6 , ny , MRI_float ) ;
      oar   = MRI_FLOAT_PTR(outim) ;

      for( jj=0 ; jj < ny ; jj++ ){
         oar[0+6*jj] = far[1+nx*jj] ;
         oar[1+6*jj] = far[2+nx*jj] ;
         oar[2+6*jj] = far[3+nx*jj] ;
         oar[3+6*jj] = far[4+nx*jj] ;
         oar[4+6*jj] = far[5+nx*jj] ;
         oar[5+6*jj] = far[6+nx*jj] ;
      }

      mri_free(flim) ;

   } else if( mode == MODE_1DFILE ){  /* first 6 elements of each row */

      if( nx < 6 ){
         fprintf(stderr,"** -1Dfile %s has too few columns!\n",fname) ;
         mri_free(flim) ; return NULL ;
      } else if( nx == 6 ){
         outim = flim ;
      } else {
         outim = mri_new( 6 , ny , MRI_float ) ;
         oar   = MRI_FLOAT_PTR(outim) ;
         for( jj=0 ; jj < ny ; jj++ ){
            oar[0+6*jj] = far[0+nx*jj] ;
            oar[1+6*jj] = far[1+nx*jj] ;
            oar[2+6*jj] = far[2+nx*jj] ;
            oar[3+6*jj] = far[3+nx*jj] ;
            oar[4+6*jj] = far[4+nx*jj] ;
            oar[5+6*jj] = far[5+nx*jj] ;
         }
         mri_free(flim) ;
      }

   } else {
      fprintf(stderr,"** get_dfile_params: illegal mode=%d\n",mode) ;
      mri_free(flim) ; return NULL ;
   }

   return outim ;
}
