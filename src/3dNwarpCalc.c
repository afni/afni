#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
#include <omp.h>
#include "mri_genalign.c"
#include "mri_genalign_util.c"
#endif

#include "mri_nwarp.c"

/*----------------------------------------------------------------------------*/

void NWC_help(void)
{
   printf(
    "Usage: 3dNwarpCalc [options] expression\n"
    "------\n"
    " * This program performs calculations on 3D warps defined on a grid.\n"
    "   ++ Or it WILL, if I ever get it working.\n"
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
    " * The expression is a single string enclosed in quotes (' or \"),\n"
    "    with operators separated by spaces.  See EXAMPLES below.\n"
    "** Note that to get any output, you will have to use the '&write'\n"
    "    operator at least once.\n"
    " * To actually use a 3D warp to transform a dataset, you must run the\n"
    "    program 3dNwarpApply:\n"
    "   ++ Note that the warp used in 3dNwarpApply does not need to be on the\n"
    "      same grid as the dataset being transformed.  You can define a warp\n"
    "      on a high-resolution anatomical grid and apply it to a low-resolution\n"
    "      functional dataset, for example -- 3dNwarpApply will figure it out.\n"
    "\n"
    "OPTIONS\n"
    "-------\n"
    " -interp iii   == 'iii' is the interpolation mode:\n"
    "                  ++ Modes allowed are a subset of those in 3dAllineate:\n"
    "                       linear  quintic  wsinc5\n"
    "                  ++ 'wsinc5' is the most accurate\n"
    "                     -- and the slowest\n"
    "                     -- and the default, if you don't make a choice\n"
    "\n"
    " -verb         == print (to stderr) various fun messages along the road\n"
    "\n"
    "BUT WHERE DO WARPS COME FROM, MOMMY?\n"
    "------------------------------------\n"
    " * The program 3dAllineate with the -nwarp_save option will save a\n"
    "    displacement representation of a nonlinear warp to a 3D dataset\n"
    "    with 3 sub-bricks (1 for each of xyz).\n"
    "\n"
    "OPERATORS\n"
    "---------\n"
    "&readnwarp(FF) == Read a 3D warp from a file and place it on top of the stack.\n"
    "                   The input file should be a 3D dataset with 3 sub-bricks\n"
    "                   (volumes) storing the xyz displacments of each grid point.\n"
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
    "                  ++ The count of values determines\the type of warp:\n"
    "                         12 ==> affine (shifts+angles+scales+shears)\n"
    "                         64 ==> cubic (3rd order) polyomial\n"
    "                        172 ==> quintic (5th order) polynomial\n"
    "                        364 ==> heptic (7th order) polynomial\n"
    "                        664 ==> nonic (9th order) polynomial\n"
    "                  ++ Any other count of values on the single input line is\n"
    "                     illegal, unconstitutional, against the laws of God,\n"
    "                     fattening, and will make you get red pimples on your nose.\n"
    "                  ++ The parameters could come, for example, from using\n"
    "                     3dAllineate with the '-1Dparam_save' and '-nwarp' options.\n"
    "\n"
    "&read4x4(FF)   == Read an affine 4x4 transform matrix directly; the input\n"
    "                   file should contain 12 numbers, which will be organized\n"
    "                   into the 3D transformation matrix as so:\n"
    "                      r11   r12   r13   r14\n"
    "                      r21   r22   r23   r24\n"
    "                      r31   r32   r33   r34\n"
    "                      0.0   0.0   0.0   1.0\n"
    "               ++++ NOT YET IMPLEMENTED\n"
    "\n"
    "&write(FF)     == Write the 3D warp on the top of the stack to a file.\n"
    "                   The output file is always in a 3D nwarp (dataset) format\n"
    "                   -- never a matrix or polynomial.\n"
    "\n"
    "&dup           == Push the duplicate of the top of the stack onto the stack:\n"
    "                   [ A B C ... ] goes to [ A A B C ... ]  after &dup.\n"
    "\n"
    "&swap          == Interchange the top two elements of the stack:\n"
    "                   [ A B C ... ] goes to [ B A C ... ]    after &swap\n"
    "\n"
    "&pop           == Remove (and delete) the top element from the stack:\n"
    "                   [ A B C ... ] goes to [ B C ... ]      after &pop\n"
    "\n"
    "&compose       == If the stack is [ A(x) B(x) C(x) ... ], compute the warp\n"
    "                   B(A(x)) and replace these top 2 elements with the result:\n"
    "                   [ A B C ... ] goes to [ B(A(x)) C(x) ... ] after &compose\n"
    "\n"
    "&invert        == Replace top element of the stack with its inverse:\n"
    "                   the warp J(x) such that A(J(x)) = x.\n"
    "\n"
    "&sqrt          == Replace top element of the stack with its 'square root':\n"
    "                   the warp Q(x) such that Q(Q(x)) = A(x).\n"
    "                  ++ NOTE: not all warps have square roots, so this operation\n"
    "                     is not guaranteed to work.  Be careful out there.\n"
    "               ++++ NOT YET IMPLEMENTED\n"
    "\n"
    "&invsqrt       == Replace the top element of the stack with the inverse of\n"
    "                   its square root: the warp R(x) such that A(R(R(x)) = x.\n"
    "                  ++ '&sqrtinv' is a synonym for this operation, since I\n"
    "                     have trouble remembering which one is correct-imundo.\n"
    "               ++++ NOT YET IMPLEMENTED\n"
    "\n"
    "&sqrtboth      == Replace the top element of the stack by TWO new elements:\n"
    "                   the square root Q(x) and the inverse square root R(x):\n"
    "                   [ A B C ... ] goes to [ Q R B C ... ]\n"
    "               ++++ NOT YET IMPLEMENTED\n"
    "\n"
    "&sqr           == Replace the top element of the stack with its 'square':\n"
    "                   the warp S(x) = A(A(x)).  Equivalent to '&dup &compose'.\n"
    "\n"
    "&scale(a)      == Scale the top-of-stack warp displacements by numerical\n"
    "                   factor 'a' in all 3 dimensions.\n"
    "                  ++ NOTE: this might make the warp non-invertible, for\n"
    "                     large enough 'a'.  Proceed at your own risk.\n"
    "                  ++ If a=0, then the result is the identity warp, since\n"
    "                     all the displacements are 0.\n"
    "\n"
    "EXAMPLES\n"
    "--------\n"
    "Nuthin yet :-(\n"
   ) ;

   exit(0) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , verb=0 , interp_code=MRI_WSINC5 ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) NWC_help() ;

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-NN") == 0 || strncasecmp(argv[iarg],"-nearest",6) == 0 ){
       ERROR_exit("NN interpolation not legal here") ;
     }
     if( strncasecmp(argv[iarg],"-linear",4)==0 || strncasecmp(argv[iarg],"-trilinear",6)==0 ){
       interp_code = MRI_LINEAR ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-cubic",4)==0 || strncasecmp(argv[iarg],"-tricubic",6)==0 ){
       ERROR_exit("cubic interplation not legal here") ;
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
       if( strcasecmp(inam,"NN")==0 || strncasecmp(inam,"nearest",5)==0 )
         ERROR_exit("NN interpolation not legal here") ;
       else
       if( strncasecmp(inam,"linear",3)==0 || strncasecmp(inam,"trilinear",5)==0 )
         interp_code = MRI_LINEAR ;
       else
       if( strncasecmp(inam,"cubic",3)==0 || strncasecmp(inam,"tricubic",5)==0 )
         ERROR_exit("cubic interplation not legal here") ;
       else
       if( strncasecmp(inam,"quintic",3)==0 || strncasecmp(inam,"triquintic",5)==0 )
         interp_code = MRI_QUINTIC ;
       else
       if( strncasecmp(inam,"WSINC",5)==0 )
         interp_code = MRI_WSINC5 ;
       else
         ERROR_exit("Unknown code '%s' after '%s' :-(",argv[iarg],argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       NwarpCalcRPN_verb(1) ; iarg++ ; continue ;
     }

     /*---------------*/

     ERROR_exit("Unknown, Illegal, and Fattening option '%s' :-( :-(",argv[iarg]) ;
   }

   if( iarg >= argc ) ERROR_exit("No command line expression :-(") ;

   /*--- All the work is done herein ---*/

   (void)NwarpCalcRPN( argv[iarg] , NULL , interp_code ) ;

   /*--- run away screaming into the night ---*/

   exit(0) ;
}
