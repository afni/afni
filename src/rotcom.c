#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_dvecmat dvm ;
   THD_3dim_dataset *dset ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: rotcom '-rotate aaI bbR ccA -ashift ddS eeL ffP' [dataset]\n"
            "\n"
            "Prints to stdout the 4x3 transformation matrix+vector that would be\n"
            "applied by 3drotate to the given dataset.\n"
            "\n"
            "The -rotate and -ashift options combined must be input inside single\n"
            "quotes (i.e., as one long command string):\n"
            " * These options follow the same form as specified by '3drotate -help'.\n"
            " * That is, if you include the '-rotate' component, it must be followed\n"
            "   by 3 angles.\n"
            " * If you include the '-ashift' component, it must be followed by 3 shifts;\n"
            " * For example, if you only want to shift in the 'I' direction, you could use\n"
            "     '-ashift 10I 0 0'.\n"
            " * If you only want to rotate about the 'I' direction, you could use\n"
            "     '-rotate 10I 0R 0A'.\n"
            "\n"
            "Note that the coordinate order for the matrix and vector is that of\n"
            "the dataset, which can be determined from program 3dinfo.  This is the\n"
            "only function of the 'dataset' command line argument.\n"
            "\n"
            "If no dataset is given, the coordinate order is 'RAI', which means:\n"
            "    -x = Right      [and so +x = Left     ]\n"
            "    -y = Anterior   [    so +y = Posterior]\n"
            "    -z = Inferior   [    so +z = Superior ]\n"
            "For example, the output of command\n"
            "   rotcom '-rotate 10I 0R 0A'\n"
            "is the 3 lines below:\n"
            "0.984808 -0.173648  0.000000  0.000\n"
            "0.173648  0.984808  0.000000  0.000\n"
            "0.000000  0.000000  1.000000  0.000\n"
            "\n"
            "-- RWCox - Nov 2002\n"
           ) ;
     exit(0) ;
   }

   if( argc > 2 ){
     dset = THD_open_dataset( argv[2] ) ;
     if( !ISVALID_DSET(dset) ){
       fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[2]); exit(1);
     }
   } else {
     dset = EDIT_empty_copy(NULL) ;
   }

   dvm = THD_rotcom_to_matvec( dset , argv[1] ) ;

   printf("%9.6f %9.6f %9.6f  %.3f\n"
          "%9.6f %9.6f %9.6f  %.3f\n"
          "%9.6f %9.6f %9.6f  %.3f\n" ,

     dvm.mm.mat[0][0] , dvm.mm.mat[0][1] , dvm.mm.mat[0][2] , dvm.vv.xyz[0] ,
     dvm.mm.mat[1][0] , dvm.mm.mat[1][1] , dvm.mm.mat[1][2] , dvm.vv.xyz[1] ,
     dvm.mm.mat[2][0] , dvm.mm.mat[2][1] , dvm.mm.mat[2][2] , dvm.vv.xyz[2]   ) ;

   exit(0) ;
}
