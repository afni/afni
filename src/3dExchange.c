/*********************** 3dExchange.c **********************************************/
/* Author: Daniel Glen - May 09 2019 */
#include "mrilib.h"

static float find_float_in_list(float voxval, 
                         float *srclist, float *replacelist, int nx);

void Exchange_help(void) {
      printf(
"Usage: 3dExchange [-prefix PREFIX] <-input DATASET>\n"
"\n"
"Replaces voxel values using mapping file with two columns of numbers\n"
"with the first column of the input value and the second has the output value\n"
"\n"
"  -input DATASET :               Input dataset\n"
"                                  Acceptable data types are:\n"
"                                  byte, short, and floats.\n"
"  -map MAPCOLS.1D :               Mapping columns - input is first column\n"
"                                  output is second column\n"
"  -prefix PREFIX: Output prefix\n"
"\n"
"  -ver = print author and version info\n"
"  -help = print this help screen\n"
         ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      PRINT_COMPILE_DATE ;
      return; 
} 

/*! Replace a voxel's value by the value's exchange in the entire set of input datasets */
int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset=NULL, *new_dset=NULL; /*input and output datasets*/
   MRI_IMAGE *data_im = NULL, *out_data_im = NULL;
   MRI_IMAGE *mapping_1D = NULL;
   
   int nopt=0, nbriks=0, ib=0;
   int i,k, nvox=0, nmaps=0, nx=0, ny=0;
   float fac, voxval, prev_voxval=-9999.0, prev_replace=-9999.0 ;
   short *sar=NULL, *outsar;
   byte *bar=NULL, *outbar;
   float *far=NULL, *outfar, *mapptr, *srclist, *replacelist;
   char *mapstr=NULL;
   char *prefix=NULL;
   char stmp[THD_MAX_PREFIX+1]={""}; 
   byte badsb_type=0;
   void *var=NULL;
   int floatize = 0;

   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      Exchange_help ();
      exit(0) ;
   }

   mainENTRY("3dExchange main"); machdep(); AFNI_logger("3dExchange",argc,argv);
   nopt = 1 ;
   
   while( nopt < argc && argv[nopt][0] == '-' ){
      if( strcmp(argv[nopt],"-ver") == 0 ){
         PRINT_VERSION("3dExchange"); AUTHOR("Daniel Glen");
         exit(0);
      }

      if( strcmp(argv[nopt],"-help") == 0 ){
         Exchange_help();
         exit(0) ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         ++nopt;
         if (nopt>=argc) {
            fprintf(stderr,"**ERROR: Need string after -prefix\n");
            exit(1);
         }
         prefix = argv[nopt] ;
         ++nopt; continue;
      }
      if( strcmp(argv[nopt],"-input") == 0 ){
         ++nopt; nbriks=0;
         dset = THD_open_dataset( argv[nopt] );
         if( !ISVALID_DSET(dset) ){
            fprintf(stderr,"**ERROR: can't open dataset %s\n",argv[nopt]) ;
            exit(1);
         }
         ++nopt; ++nbriks; 
         continue;
      }

      if( strcmp(argv[nopt],"-map") == 0 ){
         ++nopt; nmaps=0;
         mapstr = argv[nopt];
         ++nopt; ++nmaps; 
         continue;
      }

      if( strcmp(argv[nopt],"-float") == 0 ){
         floatize = 1;
         ++nopt; 
         continue;
      }
      
      ERROR_exit( " Error - unknown option %s", argv[nopt]);
   } 
   if (nopt < argc) {
      ERROR_exit( " Error unexplained trailing option: %s\n", argv[nopt]);
   }

   if (!dset) ERROR_exit("Error - no dataset given!");
   if (!mapstr) ERROR_exit("Error - no mapping file given!");

   /* check out the 1D mapping file */
   mapping_1D = mri_read_1D (mapstr);
   if (mapping_1D == NULL)   {
      ERROR_exit("Error reading mapping file. These should be 2 columns of numbers");
    }

   nx = mapping_1D->nx; ny = mapping_1D->ny;
   mapptr = MRI_FLOAT_PTR (mapping_1D);
   /* I think this is the way 1D files are stored in memory */
   srclist = mapptr;
   replacelist = mapptr+nx;
   
   if (ny < 2)
    {
      mri_free (mapping_1D);
      ERROR_message("Error - Not enough columns in mapping file");
      ERROR_exit(" %d rows and %d columns found in mapping file", nx, ny);
    }

   DSET_mallocize (dset);
   DSET_load (dset);	                /* load dataset */




   if (prefix) {
 	 /* usual case - assign a prefix */ 
		snprintf(stmp, sizeof(char)*THD_MAX_PREFIX, 
				 "%s", prefix);
   } else {  /* no prefix given, so use default name */
	 snprintf(stmp, sizeof(char)*THD_MAX_PREFIX, 
			  "exchange.%.900s", DSET_PREFIX(new_dset));
   }

   new_dset = EDIT_full_copy( dset, prefix ) ; /* make a copy of its header */
      
   tross_Copy_History( dset , new_dset ) ;

   /* if data type has not been selected by user */
   if(floatize) EDIT_floatize_dataset( new_dset );

   /* Exchange data across all the subbricks of the input dataset */
   for(i=0;i<dset->dblk->nvals; i++) {  /* for each sub-brik in dataset */
      data_im = DSET_BRICK (dset, i);   /* set pointer to the ith sub-brik of the dataset */
      out_data_im = DSET_BRICK(new_dset, i);
      fac = DSET_BRICK_FACTOR(dset, i); /* get scale factor for each sub-brik*/
      if(fac==0.0) fac=1.0;
      nvox = data_im->nvox;             /* number of voxels in the sub-brik */

      outfar = outsar = outbar = mri_data_pointer(out_data_im);

      badsb_type = 0;
      switch(data_im->kind){
           case MRI_short:{
              sar = mri_data_pointer(data_im) ;
              var = sar;
           }
           break ;
    
           case MRI_byte:{
              bar = mri_data_pointer(data_im) ;
              var = bar;
           }
           break ;
    
           case MRI_float:{
              far = mri_data_pointer(data_im) ;
              var = far;
           }
           break ;

          default:                          /* unknown type */
              WARNING_message("Unknown type, %s, in sub-brik %d",
                              MRI_TYPE_name[data_im->kind], i);
              badsb_type = 1;
           break;
        }
       if (var==NULL){
            ERROR_exit("Can not read dataset data");
       }
          switch( data_im->kind ){
             case MRI_short:{
               for(k=0;k<nvox;k++) {
                  voxval = (float) sar[k];
                  if(fac!=1.0 && voxval != 0.0) voxval = voxval*fac;
                  /* for efficiency, don't search if same swap as previous voxel*/
                  if(voxval==prev_voxval) {
					  if (floatize)
					     outfar[k] = prev_replace;
					  else 
					     outsar[k] = (short) prev_replace;
                  } 
                  else {
					 if(floatize) {
					    outfar[k] = find_float_in_list(voxval, srclist, replacelist, nx);
                        prev_replace = outfar[k];
				     }
					 else{
                        outsar[k] = (short) (find_float_in_list(voxval, srclist, replacelist, nx)/fac);
                        prev_replace = outsar[k];
                     }
                     prev_voxval = voxval;
                  }
               }
               break ;
              }

             case MRI_float:{
               for(k=0;k<nvox;k++) {
                  voxval = far[k];
                  if(fac!=1.0 && voxval != 0.0) voxval = voxval*fac;
                  /* for efficiency, don't search if same swap as previous voxel*/
                  if(voxval==prev_voxval) outfar[k] = prev_replace;
                  else {
                     outfar[k] = find_float_in_list(voxval, srclist, replacelist, nx);
                     prev_voxval = voxval;
                     prev_replace = outfar[k];
                  }
               }
               break ;
              }

             case MRI_byte:{
               for(k=0;k<nvox;k++) {
                  voxval = (float) bar[k];
                  if(fac!=1.0 && voxval != 0.0) voxval = voxval*fac;
                  /* for efficiency, don't search if same swap as previous voxel*/
                  if(voxval==prev_voxval) {
					  if (floatize)
					     outfar[k] = prev_replace;
					  else 
					     outbar[k] = (byte) prev_replace;
                  } 
                  else {
					 if(floatize) {
					    outfar[k] = find_float_in_list(voxval, srclist, replacelist, nx);
                        prev_replace = outfar[k];
				     }
					 else{
                        outbar[k] = (byte) (find_float_in_list(voxval, srclist, replacelist, nx)/fac);
                        prev_replace = outbar[k];
                     }
                     prev_voxval = voxval;
                  }
               }
               break ;
              }

             default:                          /* unknown type */
              WARNING_message("Unknown type, %s, in sub-brik %d",
              MRI_TYPE_name[data_im->kind], i);
              break;
        }

      /* change storage mode, this way prefix will determine
         format of output dset */
      new_dset->dblk->diskptr->storage_mode = STORAGE_BY_BRICK;
     
      
      tross_Make_History( "3dExchange" , argc, argv , new_dset ) ;

      if (!DSET_write( new_dset )) {
         ERROR_message("Failed to write %s", stmp);
         exit(1);  
      } else {
         WROTE_DSET(new_dset); 
      }
   }
   
   exit(0);
}

/* return floating point value in replacelist wherever 
 * voxval matches value in srclist */
static float find_float_in_list(float voxval, 
                         float *srclist, float *replacelist, int nx)
{
   int i; 
   for(i=0;i<nx;i++){
      if(voxval==srclist[i])
          return(replacelist[i]);    
   }
   /* return the original value if not in list - could change this to 
    * some other behavior */
   return(voxval);
}

