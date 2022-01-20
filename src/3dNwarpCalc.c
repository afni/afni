#if 1 /*######################################################################*/
#include <stdlib.h>
#include <stdio.h>

int main( int argc , char *argv[] )  /* Obituary: 27 Apr 2021 */
{
   printf("\n"
    "*******************************************************************\n"
    "Program 3dNwarpCalc has been retired, and is no longer available :(\n"
    "*******************************************************************\n"
   ) ;
   exit(0) ;
}

#else /*######################################################################*/
#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#include "mri_genalign.c"
#include "mri_genalign_util.c"
#include "mri_nwarp.c"

/*----------------------------------------------------------------------------*/

void NWC_help(void)
{
   printf(
    "Usage: 3dNwarpCalc [options] expression\n"
    "------\n"
    " * This program performs calculations on 3D warps defined on a grid.\n"
    "\n"
    " * The fundamental idea is a 'stack' of warps, with operators being\n"
    "    applied to the top element(s) of the stack.\n"
    "   ++ If you don't know what a computer science 'stack' is, see\n"
    "      http://en.wikipedia.org/wiki/Stack_(data_structure)\n"
    "   ++ Also see 1dmatcalc for a similar implementation of a stack\n"
    "      of matrix operations.\n"
    "   ++ In the explanations below, the stack will be denoted as\n"
    "        [ A B C ... ]\n"
    "      where A is the top element, B the next element, etc.\n"
    "      Operations take place using the top one or two elements.\n"
    "\n"
    " * The expression is a single string enclosed in quotes ' or \",\n"
    "    with operators separated by spaces.  See EXAMPLES below.\n"
    "   ++ The expression is the last thing on the command line!\n"
    "   ++ For scripting convenience, you can actually break the\n"
    "      expression into multiple strings (after all the '-' options),\n"
    "      and they will be re-assembled into one big string for\n"
    "      parsing and processing.\n"
    "\n"
    "** Note that to get any output, you will have to use the '&write'\n"
    "    operator at least once.  Otherwise, the program computes stuff\n"
    "    and then just throws it away.  (Fun perhaps, but useless.)\n"
    "\n"
    " * To actually use a 3D warp to transform a dataset, you must run the\n"
    "    program 3dNwarpApply:\n"
    "   ++ Note that the warp used in 3dNwarpApply does not need to be on the\n"
    "      same grid as the dataset being transformed.  You can define a warp\n"
    "      on a high-resolution anatomical grid and apply it to a low-resolution\n"
    "      functional dataset, for example -- 3dNwarpApply will figure it out.\n"
    "   ++ On the other hand, all the warps in 3dNwarpCalc must be defined on\n"
    "      the same spatial grid!\n"
    "   ++ (LATER) You can use the &apply command to transform a 3D dataset\n"
    "      from within 3dNwarpCalc, if you don't need the special capabilities\n"
    "      of 3dNwarpApply.\n"
    "\n"
    " * Operations such as &invert and &sqrt may produce artifacts and be\n"
    "    inaccurate near the edges of the 3D grid, since they might require\n"
    "    extrapolating the warp to the outside of the grid in places, where\n"
    "    there is no information.\n"
    "\n"
    " * For convenience, you can break the expression up into multiple strings\n"
    "    (after all the options), and they will be re-assembled into the single\n"
    "    expression string the controlling C function needs to work.\n"
    "   ++ But note that since the '&' and '()' characters are special to the shell\n"
    "      you have to put the expression string(s) inside single quote ' or double\n"
    "      quote \" pairs, or else ugly things will happen.\n"
    "      (Not as ugly as having a hippopotamus step on your head, but almost.)\n"
    "\n"
    "OPTIONS\n"
    "-------\n"
    " -interp iii   == 'iii' is the interpolation mode:\n"
    "                  ++ Modes allowed are a subset of those in 3dAllineate:\n"
    "                       linear  quintic  wsinc5\n"
    "                  ++ The default interpolation mode is 'quintic'.\n"
    "                  ++ 'linear' is much faster but less accurate.\n"
    "                  ++ 'wsinc5' is much slower but more accurate.\n"
    "\n"
    " -ainterp jjj  == 'jjj' is the interpolation mode for the '&apply' operation.\n"
    "                  ++ Modes allowed here are\n"
    "                       NN linear cubic quintic wsinc5\n"
    "                  ++ If this option isn't given, then the value from '-interp'\n"
    "                     is used (which should be good enough for government work).\n"
    "\n"
    " -verb         == print (to stderr) various fun messages along the road\n"
    "                  ++ A second '-verb' gives you even more fun!\n"
    "\n"
    "BUT WHERE DO WARPS COME FROM, MOMMY?\n"
    "------------------------------------\n"
    " * The program 3dAllineate with the -nwarp_save option will save a\n"
    "    displacement representation of a nonlinear warp to a 3D dataset\n"
    "    with 3 sub-bricks (1 for each of x, y, and z).\n"
    "\n"
    " * The contents of these sub-bricks are the displacments of each voxel in mm.\n"
    "   ++ The identity warp would be all zero, for example.\n"
    "\n"
    " * An input warp dataset can contain extra sub-bricks -- only the first 3\n"
    "    are used.\n"
    "\n"
#if 0
    " * Warp datasets output by this program have a 4th sub-brick, labeled\n"
    "    'hexvol', which contains the volume of each distorted hexahedron in\n"
    "    the grid.  This sub-brick is NOT used in any application of the\n"
    "    warp, such as 3dNwarpApply or further runs of 3dNwarpCalc, but can\n"
    "    help you understand how much distortion is present in the warp.\n"
    "   ++ If there are any negative values in the hexvol sub-brick, this\n"
    "      indicates that something bad happened in the warp calculation.\n"
#else
    " * If you want the volume distortion at each voxel, use the program\n"
    "   3dNwarpFuncs.\n"
#endif
    "\n"
    "OPERATORS\n"
    "---------\n"
    " * In the explanations below, the single character 'x' represents a 3D\n"
    "    coordinate vector, and a capital letter such as 'A' represents a\n"
    "    whole 3D warp function, whose output at a particular location is 'A(x)'.\n"
    " * You can replace the '&' character that starts a command with '%%' or '@',\n"
    "    if that is more convenient for you.\n"
    " * Operator names are not case sensitive: &INVERT is the same as &invert.\n"
    "\n"
    "&readnwarp(FF) == Read a 3D warp from a file and place it on top of the stack.\n"
    "  *OR*             The input file should be a 3D dataset with 3 sub-bricks\n"
    "&readwarp(FF)      (volumes) storing the xyz displacments of each grid point.\n"
    "\n"
    "&identwarp(FF) == Create an identity warp (all displacements 0) on the grid\n"
    "                   of a 3D dataset specified by the filename 'FF'.\n"
    "                  ++ This operation is to be used to create a starting point\n"
    "                     for calculations that otherwise do not involve a warp\n"
    "                     defined on a grid, such a polynomial warps.\n"
    "                  ++ The actual data in 'FF' is ignored by '&identwarp'; only the\n"
    "                     3D grid definition in the header is actually needed.\n"
    "       ----**==>> ++ Either '&identwarp' or '&readnwarp' should be the first\n"
    "                     operation, in order to define the grid for all subsequent\n"
    "                     calculations.\n"
    "\n"
    "&readpoly(FF)  == The input is a text file with one line of numbers\n"
    "                   specifying a warp as a polynomial, as output from\n"
    "                   '3dAllineate -1Dparam_save'.\n"
    "                  ++ The count of values determines the type of warp:\n"
    "                         12 ==> affine (shifts+angles+scales+shears)\n"
    "                         64 ==> cubic (3rd order) polyomial\n"
    "                        172 ==> quintic (5th order) polynomial\n"
    "                        364 ==> heptic (7th order) polynomial\n"
    "                        664 ==> nonic (9th order) polynomial\n"
    "                  ++ Any other count of values on the single input line is\n"
    "                     illegal, unconstitutional, against the laws of God,\n"
    "                     fattening, and will make you get red pimples on your nose.\n"
    "                  ++ The parameters could come, most probably, from using\n"
    "                     3dAllineate with the '-1Dparam_save' and '-nwarp' options.\n"
    "\n"
    "&read4x4(FF)   == Read an affine 4x4 transform matrix directly; the input\n"
    "                   file should contain 12 numbers in the order:\n"
    "                     r11 r12 r13 r14 r21 r22 r23 r24 r31 r32 r33 r34\n"
    "                   which will be organized into the 3D transformation matrix:\n"
    "                      r11   r12   r13   r14\n"
    "                      r21   r22   r23   r24\n"
    "                      r31   r32   r33   r34\n"
    "                      0.0   0.0   0.0   1.0\n"
    "                 ++ This matrix defines the transformation from input spatial\n"
    "                    DICOM coordinates (x,y,z) to output coordinates, in mm.\n"
    "                 ++ One way to get this matrix is via '3dAllineate -1Dmatrix_save'.\n"
    "                 ++ This matrix should have non-zero determinant!\n"
    "\n"
    "&write(FF)     == Write the 3D warp on the top of the stack to a file.\n"
    "                   The output file is always in a 3D nwarp (dataset) format\n"
    "                   -- NEVER a matrix or polynomial.\n"
    "\n"
    "&dup           == Push the duplicate of the top of the stack onto the stack:\n"
    "                   [ A B C ... ] goes to [ A A B C ... ]  after &dup.\n"
    "\n"
    "&swap          == Interchange the top two elements of the stack:\n"
    "                   [ A B C ... ] goes to [ B A C ... ]    after &swap\n"
    "                  ++ You can swap other elements of the stack by using\n"
    "                     indexes in the form '&swap(p,q)' where 'p' and 'q'\n"
    "                     are distinct non-negative integers indicating depth\n"
    "                     into the stack; '&swap' is equivalent to '&swap(0,1)'.\n"
    "\n"
    "&pop           == Remove (and delete) the top element from the stack:\n"
    "                   [ A B C ... ] goes to [ B C ... ]      after &pop\n"
    "\n"
    "&compose       == If the stack is [ A(x) B(x) C(x) ... ], compute the warp\n"
    "  *OR*             B(A(x)) and replace these top 2 elements with the result:\n"
    "&mult              [ A B C ... ] goes to [ B(A(x)) C(x) ... ] after &compose\n"
    "                  ++ If you wanted to compute A(B(x)), then you would use the\n"
    "                     operator combination '&swap &compose'.\n"
    "\n"
    "&invert        == Replace top element of the stack with its inverse:\n"
    "                   the warp J(x) such that A(J(x)) = x.\n"
    "                  ++ Inversion is done via a functional iteration:\n"
    "                       Jnew(x) = Jold( 2*x - A(Jold(x)) )\n"
    "                     which requires 1 warp composition and 1 warp interpolation\n"
    "                     for each step.\n"
    "                  ++ &invert and &invsqrt (and thus &sqrt) are slow operations\n"
    "                     due to the iterative nature of the calculations.\n"
#ifdef USE_OMP
    "                  ++ Multiple CPUS (via OpenMP) are used to help speed up\n"
    "                     these functions.\n"
#else
    "                  ++ On a system with OpenMP enabled, multiple CPUs would\n"
    "                     be used to speed up these functions.  However, this\n"
    "                     binary copy of 3dNwarpCalc has not been compiled with\n"
    "                     OpenMP (alas).\n"
#endif
    "                  ++ The '-verb' option to 3dNwarpCalc will show you the\n"
    "                     progress of the iterations for &invert and &invsqrt.\n"
    "\n"
    "&sqrt          == Replace top element of the stack with its 'square root':\n"
    "                   the warp Q(x) such that Q(Q(x)) = A(x).\n"
    "                  ++ NOTE: not all warps have square roots, so this operation\n"
    "                     is not guaranteed to work.  Be careful out there.\n"
    "                  ++ Nor is the square root of a nonlinear operator guaranteed\n"
    "                     to be unique!\n"
#ifndef USE_SQRTPAIR
    "                  ++ The basic algorithm used computes the inverse of Q(x),\n"
    "                     as in &invsqrt, so an extra warp inversion is required\n"
    "                     at the end here, so this operation is slower than &invsqrt.\n"
#endif
    "\n"
    "&invsqrt       == Replace the top element of the stack with the inverse of\n"
    "                   its square root: the warp R(x) such that A(R(R(x)) = x.\n"
    "                  ++ '&sqrtinv' is a synonym for this operation, since I always\n"
    "                     have trouble remembering which one is correct-imundo-ific.\n"
#ifndef USE_SQRTPAIR
    "                  ++ This operation is based on the functional iteration\n"
    "                       Rnew(x) = Rold( 1.5*x - 0.5*A(Rold(Rold(x))) )\n"
    "                     which is adapted from the Schulz iteration for matrix\n"
    "                     square roots, and requires 2 warp compositions and 1 warp\n"
    "                     interpolation for each step.\n"
#else
    "                  ++ This operation is based on a functional iteration\n"
    "                     adapted from the Denman-Beavers method for computing\n"
    "                     the square root of a matrix:\n"
    "                       initialize Y(x) = A(x) and Z(x) = x; then iterate\n"
    "                         Ynew(x) = 0.5*(Yold(x)+inv(Zold(x)))\n"
    "                         Znew(x) = 0.5*(Zold(x)+inv(Yold(x)))\n"
    "                       which converges to Y=sqrt(A) and Z=invsqrt(A).\n"
    "                  ++ For speed, these square root iterations are always done\n"
    "                     with linear interpolation, no matter what '-interp' is.\n"
#endif
    "\n"
    "&sqrtpair      == Compute both &sqrtinv and &sqrt, and leave both of them\n"
    "                  on the stack -- &sqrt on top, &sqrtinv 'below' it.\n"
    "\n"
    "&sqr           == Replace the top element of the stack with its 'square':\n"
    "                   the warp S(x) = A(A(x)).  Equivalent to '&dup &compose'.\n"
    "                  ++ To compute the fourth power of a warp: '&sqr &sqr'\n"
    "                  ++ To compute the third power of a warp:  '&dup &sqr &compose'\n"
    "                  ++ '&square' is a synonym for this operation.\n"
    "\n"
    "&scale(a)      == Scale the top-of-stack warp displacements by numerical\n"
    "                   factor 'a' in all 3 dimensions.\n"
    "                  ++ NOTE: this might make the warp non-invertible, (e.g., give\n"
    "                     negative results in 'hexvol') for large enough 'a'.\n"
    "                     Proceed at your own risk!\n"
    "                  ++ If a=0, then the result is the identity warp, since\n"
    "                     all the displacements are now 0.\n"
    "                  ++ The case a=-1 is NOT the inverse warp!\n"
    "\n"
    "&sum           == Add the displacements of the two warps on the stack,\n"
    "                   then replace BOTH of them with the result.\n"
    "                  ++ You can do something like '&sum(0.5,0.5)' to average\n"
    "                     the displacements, or '&sum(1,-1)' to difference them.\n"
    "                     In this case, the first value scales the displacements\n"
    "                     of the stack's top warp, and the second value scales the\n"
    "                     displacements of the stack's second warp.\n"
    "                  ++ NOTE: you can produce a non-invertible warp this way!\n"
    "\n"
    "&apply(DD,PP)  == Apply the 3D warp at the top of the stack to a dataset\n"
    "                   whose name is given by the 'DD' argument, to produce\n"
    "                   a dataset whose prefix is given by the 'PP' argument.\n"
    "                 ++ This operation does not affect the stack of warps.\n"
    "                 ++ &apply is provided to make your life simpler and happier :-)\n"
    "                 ++ &apply is like 3dNwarpApply with the output dataset PP\n"
    "                    always being on the same grid as the input dataset DD.\n"
    "                 ++ The grid of dataset DD does NOT have to be the same as the\n"
    "                    grid defining the warp defined on the stack.  If needed,\n"
    "                    the warp will be interpolated to be used with DD.\n"
    "                 ++ Program 3dNwarpApply provides more options to control\n"
    "                    the way that a warp is applied to a dataset; for example,\n"
    "                    to control the output grid spacing.\n"
    "\n"
    "EXAMPLES\n"
    "--------\n"
    "** Read a warp from a dataset, invert it, save the inverse.\n"
    "\n"
    " 3dNwarpCalc '&readnwarp(Warp+tlrc.HEAD) &invert &write(WarpInv)'\n"
    "\n"
    "** Do the same, but also compute the composition of the warp with the inverse,\n"
    "   and save that -- ideally, the output warp displacements would be identically\n"
    "   zero (i.e., the identity warp), and the 'hexvol' entries would be constant\n"
    "   and equal to the voxel volume.\n"
    "\n"
    " 3dNwarpCalc -verb '&readnwarp(Warp+tlrc.HEAD) &dup &invert' \\\n"
    "             '&write(WarpInv) &compose &write(WarpOut)'\n"
    "\n"
    "** Read in a warp, compute its inverse square root, then square that, and compose\n"
    "   the result with the original warp -- the result should be the identity warp\n"
    "   (i.e., all zero displacments) -- except for numerical errors, of course.\n"
    "\n"
    " 3dNwarpCalc '&readnwarp(Warp+tlrc.HEAD) &dup &invsqrt &sqr &compose &write(WarpOut)'\n"
   ) ;

   printf(
    "\n"
    "AUTHOR -- RWCox -- August 2011\n"
   ) ;

   PRINT_AFNI_OMP_USAGE("3dNwarpCalc",NULL) ; PRINT_COMPILE_DATE ;
   exit(0) ;
}

/*----------------------------------------------------------------------------*/
/* This program is basically a wrapper for function NwarpCalcRPN() */
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , verb=0 , interp_code=MRI_QUINTIC , ainterp_code=-666 ;
   char *expr ; int nexpr , narg ;
   THD_3dim_dataset *oset ;

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) NWC_help() ;

   mainENTRY("3dNwarpCalc"); machdep();
   AFNI_logger("3dNwarpCalc",argc,argv);
   PRINT_VERSION("3dNwarpCalc"); AUTHOR("Bob the Warped");
   (void)COX_clock_time() ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-NN") == 0 || strncasecmp(argv[iarg],"-nearest",6) == 0 ){
       WARNING_message("NN interpolation not legal here -- switched to linear") ;
       interp_code = MRI_LINEAR ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-linear",4)==0 || strncasecmp(argv[iarg],"-trilinear",6)==0 ){
       interp_code = MRI_LINEAR ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-cubic",4)==0 || strncasecmp(argv[iarg],"-tricubic",6)==0 ){
       WARNING_message("cubic interplation not legal here -- switched to quintic") ;
       interp_code = MRI_QUINTIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-quintic",4)==0 || strncasecmp(argv[iarg],"-triquintic",6)==0 ){
       interp_code = MRI_QUINTIC ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-wsinc",5) == 0 ){
       interp_code = MRI_WSINC5 ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-interp",5)==0 ){
       char *inam ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       inam = argv[iarg] ; if( *inam == '-' ) inam++ ;
       if( strcasecmp(inam,"NN")==0 || strncasecmp(inam,"nearest",5)==0 ){
         WARNING_message("NN interpolation not legal here -- changed to linear") ;
         interp_code = MRI_LINEAR ;
       } else if( strncasecmp(inam,"linear",3)==0 || strncasecmp(inam,"trilinear",5)==0 ){
         interp_code = MRI_LINEAR ;
       } else if( strncasecmp(inam,"cubic",3)==0 || strncasecmp(inam,"tricubic",5)==0 ){
         WARNING_message("cubic interplation not legal here -- changed to quintic") ;
         interp_code = MRI_QUINTIC ;
       } else if( strncasecmp(inam,"quintic",3)==0 || strncasecmp(inam,"triquintic",5)==0 ){
         interp_code = MRI_QUINTIC ;
       } else if( strncasecmp(inam,"wsinc",4)==0 ){
         interp_code = MRI_WSINC5 ;
       } else {
         ERROR_exit("Unknown code '%s' after '%s' :-(",argv[iarg],argv[iarg-1]) ;
       }
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-ainterp",6)==0 ){
       char *inam ;
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       inam = argv[iarg] ; if( *inam == '-' ) inam++ ;
       if( strcasecmp(inam,"NN")==0 || strncasecmp(inam,"nearest",5)==0 ){
         ainterp_code = MRI_NN ;
       } else if( strncasecmp(inam,"linear",3)==0 || strncasecmp(inam,"trilinear",5)==0 ){
         ainterp_code = MRI_LINEAR ;
       } else if( strncasecmp(inam,"cubic",3)==0 || strncasecmp(inam,"tricubic",5)==0 ){
         ainterp_code = MRI_CUBIC ;
       } else if( strncasecmp(inam,"quintic",3)==0 || strncasecmp(inam,"triquintic",5)==0 ){
         ainterp_code = MRI_QUINTIC ;
       } else if( strncasecmp(inam,"wsinc",4)==0 ){
         ainterp_code = MRI_WSINC5 ;
       } else {
         ERROR_exit("Unknown code '%s' after '%s' :-(",argv[iarg],argv[iarg-1]) ;
       }
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     /*---------------*/

     ERROR_message("Bizarre and Unknown option '%s' :-(",argv[iarg]) ;
     suggest_best_prog_option(argv[0],argv[iarg]) ;
     exit(1) ;

   }

   if( iarg >= argc ) ERROR_exit("No command line expression :-(") ;

   /*--- Assemble all remaining args into the expression ---*/

   expr = strdup(argv[iarg++]) ;
   for( ; iarg < argc ; iarg++ ){
     nexpr = strlen(expr) ;
     narg  = strlen(argv[iarg]) ; if( narg == 0 ) continue ;
     expr  = (char *)realloc( expr , sizeof(char)*(nexpr+narg+4) ) ;
     strcat(expr," ") ; strcat(expr,argv[iarg]) ;
   }

   if( ainterp_code < 0 ) ainterp_code = interp_code ;

   /*--- All the work is done herein ---*/

   NwarpCalcRPN_verb(verb) ;

   oset = NwarpCalcRPN( expr , NULL , interp_code , ainterp_code ) ;

   /*--- run away screaming into the night, never to be seen again ---*/

   free(expr) ; if( oset != NULL ) DSET_delete(oset) ;

   INFO_message("total CPU time = %.1f sec  Elapsed = %.1f\n",
                COX_cpu_time() , COX_clock_time() ) ;

   exit(0) ;
}
#endif /*#####################################################################*/
