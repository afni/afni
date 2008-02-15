#include "mrilib.h"

int main( int argc , char *argv[] )
{
   int iarg ;
   THD_3dim_dataset *rhset ;
   XtPointer_array *dsar ;
   int ntime , nvar ;

   /*------- help the pitifully ignorant user? -------*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dTfitter [options]\n"
      "At each voxels, assembles and solves a set of linear equations.\n"
      "Output is a bucket dataset with the parameters at each voxel.\n"
      "\n"
      "Options:\n"
      "  -RHS rset = Specifies the right-hand-side 3D+time dataset.\n"
      "\n"
      "  -LHS lset = Specifies a column (or columns) of the left-hand-side matrix.\n"
      "             * More than one 'lset' can follow the '-LHS' option, but each\n"
      "               input filename must NOT start with the '-' character!\n"
      "             * Each 'lset' can be a 3D+time dataset, or a 1D file.\n"
      "             * Columns are assembled in the order given on the command line,\n"
      "               which means that parameters will be output in that order!\n"
      "\n"
      "  -lsqfit   = Solve equations via least squares [the default].\n"
      "\n"
      "  -l1fit    = Solve equations via least sum of absolute residuals.\n"
      "             * You can follow this option with a list of parameter\n"
      "               indexes to indicate that some parameters should be\n"
      "               constrained in the solution, as in\n"
      "                 -l1fit 1 -3\n"
      "               which indicates that parameter #1 (from the first -LHS)\n"
      "               must be non-negative, and that parameter #3 must be\n"
      "               non-positive.  Parameter #2 is unconstrained.\n"
      "             * Constraints are not available with '-lsqfit'.\n"
      "\n"
      "  -prefix p = Prefix for the output dataset filename.\n"
      "             * Which is always in float format.\n"
      "\n"
      "-- RWCox -- Feb 2008\n" 
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

}
