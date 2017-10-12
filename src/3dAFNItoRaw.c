/******************************* 3dAFNItoRaw **************************************/
#include "mrilib.h"

static char outfname[THD_MAX_PREFIX] = "rawxyz.dat";

/*! convert three sub-briks to a raw dataset with consecutive triplets */
int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset;  /* input dataset */
   int nopt, nxyz;
   int force_float = 0;
   float voxval;
   int ival;
   short sval;
   double dval;
   byte bval;
   void *voxptr;
   int i,j;
   FILE *fout;
   double fac;
   MRI_IMAGE *data_im = NULL;
   MRI_TYPE out_kind = MRI_float;
   int statcode;

   /*----- Read command line -----*/

WARNING_message("This program (3dAFNItoRaw) is old, not maintained, and probably useless!") ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNItoRaw [options] dataset\n"
             "Convert an AFNI brik file with multiple sub-briks to a raw file with\n"
             "  each sub-brik voxel concatenated voxel-wise.\n"
             "For example, a dataset with 3 sub-briks X,Y,Z with elements x1,x2,x3,...,xn,\n"
	     "  y1,y2,y3,...,yn and z1,z2,z3,...,zn will be converted to a raw dataset with\n"
             "  elements x1,y1,z1, x2,y2,z2, x3,y3,z3, ..., xn,yn,zn \n"
             "The dataset is kept in the original data format (float/short/int)\n"
             "Options:\n"
             "  -output / -prefix = name of the output file (not an AFNI dataset prefix)\n"
             "    the default output name will be rawxyz.dat\n\n"
             "  -datum float = force floating point output. Floating point forced if any\n"
             "    sub-brik scale factors not equal to 1.\n\n"
	     );
     printf("\n" MASTER_SHORTHELP_STRING ) ;
      PRINT_COMPILE_DATE; exit(0) ;
   }

   mainENTRY("3dAFNItoRaw main"); machdep(); AFNI_logger("3dAFNItoRaw",argc,argv);
   PRINT_VERSION("3dAFNItoRaw") ;
   nopt = 1;
   while( nopt < argc && argv[nopt][0] == '-' ){
    if( (strcmp(argv[nopt],"-output") == 0 ) || (strcmp(argv[nopt],"-prefix")==0))
      {
       if (++nopt >= argc)
	 {
	  fprintf (stderr, "*** Error - output / prefix needs an argument!\n");
	  exit (1);
	 }
       MCW_strncpy (outfname, argv[nopt], THD_MAX_PREFIX);	/* change name from default prefix */
       if (!THD_filename_ok (outfname))
	 {
	  fprintf (stderr, "*** Error - %s is not a valid output name!\n", outfname);
	  exit (1);
	 }
       if (THD_is_file(outfname))
        {
	  fprintf (stderr, "*** Error - %s already exists!\n", outfname);
	  exit (1);
	 }
       fout = fopen (outfname, "w") ;
       if( fout == NULL ){
	  fprintf (stderr, "*** Error - can not create %s for some reason!\n", outfname);
	  exit (1);
	 }
       nopt++; continue;
      }
    if (strcmp(argv[nopt],"-datum")==0) {
       if (strcmp(argv[++nopt],"float")==0)
          force_float = 1;
       else {
	  fprintf (stderr, \
            "*** Error - can only specify float; otherwise, original format assumed!\n");
	  exit (1);
	 }
       nopt++; continue;
      }
     fprintf(stderr, "*** Error - unknown option %s\n", argv[nopt]);
     exit(1);
   }
 
   old_dset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(old_dset,argv[nopt]);

   /* expect at least 2 values per voxel - otherwise what's the point? */
   if (DSET_NVALS (old_dset) < 2){
      fprintf (stderr, "*** Error - Dataset must have at least two sub-briks.\n");
      exit (1);
   }

   nxyz = DSET_NVOX(old_dset) ; /* number of voxels in dataset */

   fout = fopen (outfname, "w") ;
   if( fout == NULL ){
      fprintf (stderr, "*** Error - can not create %s for some reason!\n", outfname);
      exit (1);
   }
 
   if(force_float==0) {
     for(j=0;j<DSET_NVALS(old_dset);j++) {
         fac = DSET_BRICK_FACTOR(old_dset, j); /* get scale factor for each sub-brik*/
         if(fac==0.0) fac=1.0;
         if(fac!=1.0) {
	   force_float = 1;                /* if even one scale factor != 1, put output in float */
           break;
         }
     }
   }
 

   DSET_mallocize (old_dset);
   DSET_load (old_dset);	                /* load dataset */

   for(i=0;i<nxyz;i++) {                   /* for every voxel in each sub-brik */
     for(j=0;j<DSET_NVALS(old_dset);j++) {  /*   for each sub-brik */
       data_im = DSET_BRICK (old_dset, j);	/* set pointer to the jth sub-brik of the dataset */
       fac = DSET_BRICK_FACTOR(old_dset, j); /* get scale factor for each sub-brik*/
       if(fac==0.0) fac=1.0;
              switch( data_im->kind ){
               case MRI_short:{
                  short *ar = mri_data_pointer(data_im) ;
                  voxval = ar[i];
               }
               break ;

               case MRI_byte:{
                  byte *ar = mri_data_pointer(data_im) ;
                  voxval = ar[i];
               }
               break ;

               case MRI_float:{
                  float *ar = mri_data_pointer(data_im) ;
                  voxval = ar[i];
               }
               break ;

              case MRI_double:{
                  double *ar = mri_data_pointer(data_im) ;
                  voxval = ar[i];
               }
               break ;

              case MRI_int:{
                  int *ar = mri_data_pointer(data_im) ;
                  voxval = ar[i];
               }
               break ;

              case MRI_complex:{
                  complex *ar = mri_data_pointer(data_im) ;
                  voxval = CABS(ar[i]);
               }
               break ;

              case MRI_rgb:{
                  byte *ar = mri_data_pointer(data_im) ;
                  voxval = 0.299*ar[3*i]+0.587*ar[3*i+1]+0.114*ar[3*i+2];
               }
               break ;

	      default:                          /* unknown type */
		 voxval = 0.0;                   /* ignore this voxel */
                 fprintf(stderr,"Unknown type, %s, in sub-brik %d\n", MRI_TYPE_name[data_im->kind], i);
	       break;
            }

       voxval = voxval * fac;

       if(force_float!=0)
         out_kind = MRI_float;
       else
         out_kind = data_im->kind;
       
       switch(out_kind) {
          case MRI_short:{
             sval  = voxval;
             voxptr = &sval;
          }
          break ;

          case MRI_byte:{
             bval  = voxval;
             voxptr = &sval;
          }
          break ;

          case MRI_float:{
             voxptr = &voxval;
          }
          break ;

          case MRI_double:{
             dval = voxval;
             voxptr = &dval;
          }
          break ;

          case MRI_int:{
             ival = voxval;
             voxptr = &ival;
          }
          break ;
 
          default:                          /* unknown type */
              fprintf(stderr,"Can't handle type, %s, in sub-brik %d\n",\
                MRI_TYPE_name[data_im->kind], j);
              fclose(fout);
              exit(1);
	  break;
       }             

       statcode = fwrite(voxptr, sizeof(out_kind), 1,fout);  /* write one voxel value at a time */
       if(statcode!=1) {
	 fprintf(stderr, "*** Error - writing output file!\n");
         fclose(fout);
         exit(1);
       }
     }
   }

   fprintf(stderr, "File output format is %s\n", MRI_TYPE_name[out_kind]);
   fclose(fout);
   exit(0);
}
