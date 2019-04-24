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
            "Usage: 1dApar2mat dx dy dz a1 a2 a3 sx sy sz hx hy hz\n"
            "\n"
            "* This program computes the affine transformation matrix\n"
            "  from the set of 3dAllineate parameters.\n"
            "\n"
            "* The result is printed to stdout, and can be captured\n"
            "  by Unix shell redirection (e.g., '|', '>', '>>', etc.).\n"
            "  See the EXAMPLE, far below.\n"
            "\n"
            "* One use for 1dApar2mat is to take a set of parameters\n"
            "  from '3dAllineate -1Dparam_save', alter them in some way,\n"
            "  and re-compute the corresponding matrix. For example,\n"
            "  compute the full affine transform with 12 parameters,\n"
            "  but then omit the final 6 parameters to see what the\n"
            "  'pure' shift+rotation matrix looks like.\n"
            "\n"
            "* The 12 parameters are, in the order used on the 1dApar2mat command line\n"
            "  (the same order as output by 3dAllineate):\n"
            "   x-shift         in mm\n"
            "   y-shift         in mm\n"
            "   z-shift         in mm\n"
            "   z-angle (roll)  in degrees (not radians!)\n"
            "   x-angle (pitch) in degrees\n"
            "   y-angle (yaw)   in degrees\n"
            "   x-scale         unitless factor, in [0.10,10.0]\n"
            "   y-scale         unitless factor, in [0.10,10.0]\n"
            "   z-scale         unitless factor, in [0.10,10.0]\n"
            "   y/x-shear       unitless factor, in [-0.3333,0.3333]\n"
            "   z/x-shear       unitless factor, in [-0.3333,0.3333]\n"
            "   z/y-shear       unitless factor, in [-0.3333,0.3333]\n"
            "\n"
            "* Parameters omitted from the end of the command line get their\n"
            "  default values (0 except for scales, which default to 1).\n"
            "\n"
            "* At least 1 parameter must be given, or you get this help message :)\n"
            "  The minimum command line is\n"
            "   1dApar2mat 0\n"
            "  which will output the identity matrix.\n"
            "\n"
            "* Legal scale and shear factors have limited ranges, as\n"
            "  described above. An input value outside the given range\n"
            "  will be reset to the default value for that factor (1 or 0).\n"
            "\n"
            "* UNUSUAL SPECIAL CASES:\n"
            "   If you used 3dAllineate with any of the options described\n"
            "   under 'CHANGING THE ORDER OF MATRIX APPLICATION' or you\n"
            "   used the '-EPI' option, then the order of parameters inside\n"
            "   3dAllineate will no longer be the same as the parameter order\n"
            "   in 1dApar2mat. In such a situation, the matrix output by\n"
            "   this program will NOT agree with that output by 3dAllineate\n"
            "   for the same set of parameter numbers :(\n"
            "\n"
            "* EXAMPLE:\n"
            "   1dApar2mat 0 1 2 3 4 5\n"
            "  to get a rotation matrix with some shifts; the output is:\n"
            "  # mat44 1dApar2mat 0 1 2 3 4 5 :\n"
            "        0.994511      0.058208     -0.086943       0.000000\n"
            "       -0.052208      0.996197      0.069756       1.000000\n"
            "        0.090673     -0.064834      0.993768       2.000000\n"
            "  If you wish to capture this matrix all on one line, you can\n"
            "  combine various Unix shell and command tricks/tools, as in\n"
            "   echo `1dApar2mat 0 1 2 3 4 5 | tail -3` > Fred.aff12.1D\n"
            "  This 12-numbers-in-one-line is the format output by '-1Dmatrix_save'\n"
            "  in 3dAllineate and 3dvolreg.\n"
            "\n"
            "* FANCY EXAMPLE:\n"
            "  Tricksy command line stuff to compute the inverse of a matrix\n"
            "    set fred = `1dApar2mat 0 0 0 3 4 5 1 1 1 0.2 0.1 0.2 | tail -3`\n"
            "    cat_matvec `echo $fred | sed -e 's/ /,/g' -e 's/^/MATRIX('/`')' -I\n"
            "\n"
            "* ALSO SEE: Programs cat_matvec and 1dmatcalc for doing\n"
            "            simple matrix arithmetic on such files.\n"
            "\n"
            "* OPTIONS: This program has no options. Love it or leave it :)\n"
            "\n"
            "* AUTHOR: Zhark the Most Affine and Sublime - April 2019\n"
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
