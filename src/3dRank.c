/*********************** 3dRank.c **********************************************/
/* Author: Ziad Saad May 2009 */
#include "mrilib.h"

extern int * UniqueInt (int *y, int ysz, int *kunq, int Sorted );

void Rank_help(void) {
      printf(
         "Usage: 3dRank [-prefix PREFIX] <-input DATASET1 [DATASET2 ...]>\n"
         "\n"
         "Replaces voxel values by their rank in the set of\n"
         "values collected over all voxels in all input datasets\n"
         "If you input one dataset, the output should be identical\n"
         "to the -1rank option in 3dmerge\n"
         "\n"
         "  -input DATASET1 [DATASET2 ...]: Input datasets.\n"
         "                                  Acceptable data types are:\n"
         "                                  byte, short, and floats.\n"
         "  -prefix PREFIX: Ouput prefix.\n"
         "                  If you have multiple datasets on input\n"
         "                  the prefix is preceded by r00., r01., etc.\n"
         "                  If no prefix is given, the default is \n"
         "                  rank.DATASET1, rank.DATASET2, etc.\n"
         "                  In addition to the ranked volume, a rank map\n"
         "                  1D file is created. It shows the mapping from \n"
         "                  the rank to the input dataset value \n"
         "  -ver = print author and version info\n"
         "  -help = print this help screen\n"
         ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      PRINT_COMPILE_DATE ;
      return; 
} 



/*! Replace a voxel's value by the value's rank in the entire set of input datasets */
int main( int argc , char * argv[] )
{
   THD_3dim_dataset ** dsets_in = NULL;  /* input and output datasets */
   int nopt=0, nbriks=0, nsubbriks=0, ib=0, isb=0;
   byte *cmask=NULL;
   int *all_uniques=NULL, **uniques=NULL, *final_unq=NULL, *N_uniques=NULL;
   int N_final_unq=0, iun=0, total_unq=0;
   int *rmap=NULL;
   int imax=0, iunq=0, ii=0;
   long int off=0;
   char *prefix=NULL;
   char stmp[THD_MAX_PREFIX+1]={""}; 
   FILE *fout=NULL;

   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      Rank_help ();
      exit(0) ;
   }

   mainENTRY("3dRank main"); machdep(); AFNI_logger("3dRank",argc,argv);
   nopt = 1 ;
   
   while( nopt < argc && argv[nopt][0] == '-' ){
      if( strcmp(argv[nopt],"-ver") == 0 ){
         PRINT_VERSION("3dRank"); AUTHOR("Ziad Saad");
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-help") == 0 ){
         Rank_help();
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
         dsets_in = (THD_3dim_dataset**)
                        calloc(argc-nopt+1, sizeof(THD_3dim_dataset*));
         ++nopt; nbriks=0;
         while (nopt < argc ) {
            dsets_in[nbriks] = THD_open_dataset( argv[nopt] );
            if( !ISVALID_DSET(dsets_in[nbriks]) ){
              fprintf(stderr,"**ERROR: can't open dataset %s\n",argv[nopt]) ;
              exit(1);
            }
            ++nopt; ++nbriks; 
         }
         continue;
      }
      
      ERROR_exit( " Error - unknown option %s", argv[nopt]);
   }
   /* some checks and inits*/
   nsubbriks = 0;
   for (ib = 0; ib<nbriks; ++ib) {
      for (isb=0; isb<DSET_NVALS(dsets_in[ib]); ++isb) {
         if (  DBLK_BRICK_TYPE( dsets_in[ib]->dblk, isb ) != MRI_byte ||
               DBLK_BRICK_TYPE( dsets_in[ib]->dblk, isb ) != MRI_short ) {
            /* do not complain, NIFTI int dsets are stored as float in AFNI
            WARNING_message("One of the input dset is not of an integral"
                            "data type.");       */
         }
         ++nsubbriks;
      }
   }
   
   /* Now get unique arrays */
   uniques = (int **)calloc(nsubbriks, sizeof(int*));
   N_uniques = (int *)calloc(nsubbriks, sizeof(int));
   total_unq = 0;
   iun = 0;
   for (ib = 0; ib<nbriks; ++ib) {
      DSET_load(dsets_in[ib]);
      for (isb=0; isb<DSET_NVALS(dsets_in[ib]); ++isb) {
         uniques[iun] = THD_unique_vals(dsets_in[ib], isb,
                                        &(N_uniques[iun]), cmask);
         total_unq += N_uniques[iun]; 
         ++iun;
      }
   }
   
   /* put all the arrays together and get the unique of the uniques */
   all_uniques = (int *)calloc(total_unq, sizeof(int));
   off=0;
   for (iun=0; iun<nsubbriks; ++iun) {
      memcpy(all_uniques+off, uniques[iun], N_uniques[iun]*sizeof(int));
      off += N_uniques[iun];
   }
   
   /* free intermediate unique arrays */
   for (iun=0; iun<nsubbriks; ++iun) {
      free(uniques[iun]);
   }
   free(uniques); uniques=NULL;
   free(N_uniques); N_uniques=NULL;
   
   
   /* get unique of catenated array */
   final_unq = UniqueInt (all_uniques, total_unq, &N_final_unq, 0 );
   free(all_uniques); all_uniques=NULL;
  
   /* get the maximum integer in the unique array */
   imax = 0;
   for (iunq=0; iunq<N_final_unq; ++iunq) {
      if (final_unq[iunq] > imax) imax = final_unq[iunq]; 
   }
   
   if (prefix) {
      snprintf(stmp, sizeof(char)*THD_MAX_PREFIX, 
               "%s.rankmap.1D", prefix);
   } else {
      if (nbriks == 1) {
        snprintf(stmp, sizeof(char)*THD_MAX_PREFIX, 
                  "%s.rankmap.1D", DSET_PREFIX(dsets_in[0]));
      } else { 
         snprintf(stmp, sizeof(char)*THD_MAX_PREFIX, 
                  "%s+.rankmap.1D", DSET_PREFIX(dsets_in[0]));
      }
   }
      
   if (stmp[0]) {
      if ((fout = fopen(stmp,"w"))) {
         fprintf(fout, "#Rank Map (%d unique values)\n", N_final_unq);
         fprintf(fout, "#Col. 0: Rank\n");
         fprintf(fout, "#Col. 1: Input Dset Value\n");
      }
   } 

   rmap = (int *)calloc(imax+1, sizeof(int));
   for (iunq=0; iunq<N_final_unq; ++iunq) {
      rmap[final_unq[iunq]] = iunq; 
      if (fout) fprintf(fout, "%d   %d\n", iunq, final_unq[iunq]);
   }
   fclose(fout); fout=NULL;

   /* now cycle over all dsets and replace their voxel values with rank */
   for (ib = 0; ib<nbriks; ++ib) {
      for (isb=0; isb<DSET_NVALS(dsets_in[ib]); ++isb) {
         EDIT_BRICK_LABEL(  dsets_in[ib],isb, "rank" ) ;
         EDIT_BRICK_TO_NOSTAT(  dsets_in[ib],isb ) ;
         EDIT_BRICK_FACTOR( dsets_in[ib],isb, 0.0);/* no factors for rank*/
         switch (DSET_BRICK_TYPE(dsets_in[ib],isb) ){
            default:
               fprintf(stderr,
                        "** Bad dset type for unique operation.\n"
                        "Only Byte, Short, and float dsets are allowed.\n");
               break ; /* this should not happen here, 
                        so don't bother returning*/
            case MRI_short:{
               short *mar = (short *) DSET_ARRAY(dsets_in[ib],isb) ;
               if (imax >  MRI_TYPE_maxval[MRI_short]) {
                  WARNING_message("Maximum rank value of %d is\n"
                                  "than maximum value for dset datatype of %d\n",
                                  imax, MRI_TYPE_maxval[MRI_short]);
               }
               for( ii=0 ; ii < DSET_NVOX(dsets_in[ib]) ; ii++ )
                  if (!cmask || cmask[ii]) 
                     mar[ii] = (short)(rmap[(int)mar[ii]]); 
                  else mar[ii] = 0;
            }
            break ;
            case MRI_byte:{
               byte *mar ;
               if (imax >  MRI_TYPE_maxval[MRI_short]) {
                  WARNING_message("Maximum rank value of %d is\n"
                                  "than maximum value for dset datatype of %d\n",
                                  imax, MRI_TYPE_maxval[MRI_byte]);
               }
               mar = (byte *) DSET_ARRAY(dsets_in[ib],isb) ;
               for( ii=0 ; ii < DSET_NVOX(dsets_in[ib]) ; ii++ )
                  if (!cmask || cmask[ii]) 
                     mar[ii] = (byte)(rmap[(int)mar[ii]]); 
                  else mar[ii] = 0;
            }
            break ;
            case MRI_float:{
               float *mar = (float *) DSET_ARRAY(dsets_in[ib],isb) ;
               for( ii=0 ; ii < DSET_NVOX(dsets_in[ib]) ; ii++ )
                  if (!cmask || cmask[ii]) 
                     mar[ii] = (float)(rmap[(int)mar[ii]]); 
                  else mar[ii] = 0;
            }
            break ;

         }
      }
      
      /* update range, etc. */
      THD_load_statistics(dsets_in[ib]);
      
      /* Now write the bricks */
      if (prefix) {
         if (nbriks == 1) { 
            snprintf(stmp, sizeof(char)*THD_MAX_PREFIX, 
                     "%s", prefix);
         } else {
            snprintf(stmp, sizeof(char)*THD_MAX_PREFIX, 
                     "r%02d.%s", ib, prefix);
         }
      } else {
         snprintf(stmp, sizeof(char)*THD_MAX_PREFIX, 
                  "rank.%s", DSET_PREFIX(dsets_in[ib]));
      }
      EDIT_dset_items( dsets_in[ib] ,
                       ADN_prefix   , stmp ,
                       ADN_none ) ;
      
      /* change storage mode, this way prefix will determine
         format of output dset */
      dsets_in[ib]->dblk->diskptr->storage_mode = STORAGE_BY_BRICK;
      
      tross_Make_History( "3dRank" , argc, argv , dsets_in[ib] ) ;
      if (!THD_write_3dim_dataset( NULL, stmp, dsets_in[ib],True )) {
         ERROR_message("Failed to write");
         exit(1);  
      } else {
         WROTE_DSET(dsets_in[ib]); 
         DSET_deletepp(dsets_in[ib]);
      }
   }
    
   free(rmap); rmap=NULL;
   free(final_unq);  final_unq=NULL;
   
   exit(0);
}

