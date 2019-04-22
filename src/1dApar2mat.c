#include "mrilib.h"
#include "mri_genalign.c"

int main( int argc , char *argv[] )
{
   mat44 qmat ;
   int npar ; float parvec[12] ;
   int iarg=1 ;

   if( argc < 2 || strncasecmp(argv[1],"-h",2) == 0 ){
     printf("\n"
            "Usage:  1dApar2mat dx dy dz a1 a2 a3 sx sy sz hx hy hz\n"
            "\n"
            "* This program computes the affine transformation matrix\n"
            "  from the set of 3dAllineate parameters.\n"
            "\n"
            "* The parameters are, in order\n"
            "    x-shift          in mm\n"
            "    y-shift          in mm\n"
            "    z-shift          in mm\n"
            "    z-angle (roll)   in degrees\n"
            "    x-angle (pitch)  in degrees\n"
            "    y-angle (yaw)    in degrees\n"
            "    x-scale          unitless\n"
            "    y-scale          unitless\n"
            "    z-scale          unitless\n"
            "    y/x-shear        unitless\n"
            "    z/x-shear        unitless\n"
            "    z/y-shear        unitless\n"
            "\n"
            "* Parameters omitted from the end get their default values\n"
            "  (0 except for scales, which default to 1).\n"
            "\n"
            "* Example:\n"
            "    1dApar2mat 0 0 0 3 2 7\n"
            "  to get a pure rotation matrix\n"
            "\n"
            "* This program has no options.\n"
            "\n"
      ) ;
      exit(0) ;
   }

   for( npar=0 ; iarg < argc && npar < 12; iarg++,npar++ ){
     parvec[npar] = (float)strtod(argv[iarg],NULL) ;
   }
   qmat = GA_setup_affine( npar , parvec ) ;
   DUMP_MAT44("matrix",qmat) ;
   exit(0) ;
}
