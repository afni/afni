#include "mrilib.h"
#include "mri_genalign.c"

int main( int argc , char *argv[] )
{
   mat44 qmat ;
   int npar ; float parvec[12] ;
   int iarg=1 ;
   char lab[2048] ;

   /*--- help the pitiful luser ---*/

   if( argc < 2 || strncasecmp(argv[1],"-h",2) == 0 ){
     printf("\n"
            "Usage:  1dApar2mat dx dy dz a1 a2 a3 sx sy sz hx hy hz\n"
            "\n"
            "* This program computes the affine transformation matrix\n"
            "  from the set of 3dAllineate parameters.\n"
            "\n"
            "* One use for this program is to take a set of parameters\n"
            "  from '3dAllineate -1Dparam_save', alter them in some way,\n"
            "  and re-compute the corresponding matrix. For example,\n"
            "  compute the full affine transform with 12 parameters,\n"
            "  but then omit the final 6 parameters to see what the\n"
            "  'pure' shift+rotation matrix looks like.\n"
            "\n"
            "* The parameters are, in order on the command line\n"
            "  (the same order as output by 3dAllineate):\n"
            "    x-shift          in mm\n"
            "    y-shift          in mm\n"
            "    z-shift          in mm\n"
            "    z-angle (roll)   in degrees\n"
            "    x-angle (pitch)  in degrees\n"
            "    y-angle (yaw)    in degrees\n"
            "    x-scale          unitless factor, in [0.10,10.0]\n"
            "    y-scale          unitless factor, in [0.10,10.0]\n"
            "    z-scale          unitless factor, in [0.10,10.0]\n"
            "    y/x-shear        unitless factor, in [-0.3333,0.3333]\n"
            "    z/x-shear        unitless factor, in [-0.3333,0.3333]\n"
            "    z/y-shear        unitless factor, in [-0.3333,0.3333]\n"
            "\n"
            "* Parameters omitted from the end of the command line get their\n"
            "  default values (0 except for scales, which default to 1).\n"
            "\n"
            "* Legal scale and shear factors have limited ranges, as\n"
            "  described above. An input value outside the given range\n"
            "  will be reset to the default value for that factor (1 or 0).\n"
            "\n"
            "* Example:\n"
            "    1dApar2mat 0 1 2 3 4 5\n"
            "  to get a rotation matrix with some shifts; the output is:\n"
            "  # mat44 1dApar2mat 0 1 2 3 4 5 :\n"
            "        0.994511      0.058208     -0.086943       0.000000\n"
            "       -0.052208      0.996197      0.069756       1.000000\n"
            "        0.090673     -0.064834      0.993768       2.000000\n"
            "  If you wish to capture this matrix all on one line, you can\n"
            "  use various Unix shell tricks, as in\n"
            "    echo `1dApar2mat 0 1 2 3 4 5 | 1dcat stdin:`\n"
            "  This 12-numbers-in-one-line is the format output by '-1Dmatrix_save'\n"
            "  in 3dAllineate and 3dvolreg.\n"
            "\n"
            "* This program has no options. Love it or leave it.\n"
            "\n"
            "* Author: Zhark the Most Affine and Sublime - April 2019\n"
            "\n"
      ) ;
      exit(0) ;
   }

   /* initialize parvec (not really necessary) */

   for( npar=0 ; npar < 3 ; npar++ ){
     parvec[0+npar] = parvec[3+npar] = parvec[9+npar] = 0.0f ;
     parvec[6+npar] = 1.0f ; /* scale factors */
   }

   /* get parvec from command line, and make an informative label */

   strcpy(lab,"1dApar2mat ") ;
   for( npar=0 ; iarg < argc && npar < 12; iarg++,npar++ ){
     parvec[npar] = (float)strtod(argv[iarg],NULL) ;
     strcat(lab,argv[iarg]) ; strcat(lab," ") ;
   }

   /* do the work */

   qmat = GA_setup_affine( npar , parvec ) ;

   /* dump the work */

   DUMP_MAT44(lab,qmat) ;
   exit(0) ;
}
