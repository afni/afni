#include "mrilib.h"
#include "r_new_resam_dset.h"

#ifdef USE_OMP
#include <omp.h>
#include "mri_genalign_util.c"
#endif

/*----------------------------------------------------------------------------*/

void NWC_help(void)
{
   printf(
    "Usage: 3dNwarpCalc [options] expression\n"
    "\n"
    " * This program performs calculations on 3D warps defined on a grid.\n"
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
    "\n"
    "OPTIONS\n"
    "-------\n"
    " -verb         == print (to stderr) various messages along the way\n"
    " -interp iii   == 'iii' is the interpolation method;\n"
    "                  ++ Default interpolation mode is 'wsinc5'\n"
    "                  ++ Modes are the same as in 3dAllineate:\n"
    "                       NN  linear  cubic  quintic  wsinc5\n"
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
    "&readpoly(FF)  == The input is a text file with a single line of numbers\n"
    "                   specifying a warp as a polynomial, as output from\n"
    "                   3dAllineate -1Dparam_save; the count of values determines\n"
    "                   the type of warp:\n"
    "                         12 ==> affine (shifts+angles+scales+shears)\n"
    "                         64 ==> cubic (3rd order) polyomial\n"
    "                        172 ==> quintic (5th order) polynomial\n"
    "                        364 ==> heptic (7th order) polynomial\n"
    "                        664 ==> nonic (9th order) polynomial\n"
    "                   Any other count of values on the input line is illegal,\n"
    "                   unconstitutional, against the laws of God, and fattening.\n"
    "&read4x4(FF)   == Read an affine 4x4 transform matrix directly; the input\n"
    "                   file should contain 12 numbers, which will be organized\n"
    "                   into the 3D transformation matrix as so:\n"
    "                      r11   r12   r13   r14\n"
    "                      r21   r22   r23   r24\n"
    "                      r31   r32   r33   r34\n"
    "                      0.0   0.0   0.0   1.0\n"
    "&write(FF)     == Write the 3D warp on the top of the stack to a file.\n"
    "                   The output file is always in a 3D nwarp format --\n"
    "                   never a matrix or polynomial.\n"
    "&dup           == Push the duplicate of the top of the stack onto the stack:\n"
    "                   [ A B C ... ] goes to [ A A B C ... ]  after &dup.\n"
    "&swap          == Interchange the top two elements of the stack:\n"
    "                   [ A B C ... ] goes to [ B A C ... ]    after &swap\n"
    "&pop           == Remove the top element from the stack:\n"
    "                   [ A B C ... ] goes to [ B C ... ]      after &pop\n"
    "&compose       == If the stack is [ A(x) B(x) C(x) ... ], compute the warp\n"
    "                   B(A(x)) and replace these top 2 elements with the result:\n"
    "                   [ A B C ] goes to [ B(A(x)) C(x) ... ] after &compose\n"
    "&invert        == Replace top element of the stack with its inverse.\n"
    "&sqrt          == Replace top element of the stack with its 'square root':\n"
    "                   the warp Q(x) such that Q(Q(x)) = A(x).\n"
    "&scale(a)      == Scale the warp displacements by numerical factor 'a'\n"
    "                   in all 3 dimensions.\n"
    "                  ++ NOTE: this might make the warp non-invertible, for\n"
    "                     large enough 'a'.  Proceed at your own risk.\n"
    "\n"
    "EXAMPLES\n"
    "--------\n"

   ) ;

   exit(0) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , verb=0 , interp_code=MRI_WSINC5 ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) NWC_help() ;

   exit(0) ;
}
