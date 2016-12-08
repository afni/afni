/*
3dTsplit4D.c
This is a quick program to split a 3d+time dataset into multiple
single 3D files.  Mostly to facilitate transferring data into MATLAB toolboxes.
It can also be useful for deal with SPM and FSL programs.  

Author: Peter J. Molfese, Haskins Laboratories/UConn/Yale
Contact: Peter.Molfese@yale.edu

January 5, 2016 - Initial Version
*/



#include "mrilib.h"

int help_3dTsplit4D( )
{
   printf(
      "USAGE: 3dTsplit4D [options] dataset\n\n"
      "This program converts a 3D+time dataset into multiple 3D single-brick\n"
      "files.  The main purpose of this is to accelerate the process of\n"
      "export AFNI/NIFTI datasets if you have the unfortunate need to work\n"
      "with Some other PrograM that doesn't like datasets in the pseudo-4D\n"
      "nature that AFNI knows and loves.\n"
      "\n"
      " -as_type       : leave output as original type\n"
      " -prefix PREFIX : Prefix of the output dataset \n"
      "                  Numbers will be added after the prefix to denote\n"
      "                  prior sub-brick.\n"
      "\n\n"
      "Authored by: Peter Molfese, UConn"
      );

   PRINT_COMPILE_DATE; 
   return(0);
}

/* a few updates    8 Dec 2016 [rickr] */

int main( int argc, char *argv[] )
{
   THD_3dim_dataset *iset, *oset;
   float ffac;
   int   iarg=1, kk, nval, ndigits, freq;
   int   datum=MRI_float, as_type=0;
   char *prefix = "SPLIT";
   char *sub_prefix, newlabel[32];
   MRI_IMAGE *inImage=NULL, *outImage;
   
   if( argc < 2 || strcmp(argv[1], "-help") == 0 )
   {
      help_3dTsplit4D( );
      exit(0);
   }
   
   mainENTRY("3dTsplit4D"); 
   machdep();
   
   for( iarg=1; iarg < argc && argv[iarg][0] == '-'; iarg++ )
   {
      if( strcmp( argv[iarg], "-prefix") == 0 )
      {
         prefix = argv[++iarg];
         if( !THD_filename_ok(prefix) )
            ERROR_exit("bad -prefix value");
      } else if( strcmp( argv[iarg], "-as_type") == 0 ) {
         as_type = 1;
      } else {
         ERROR_exit("unknown option %s", argv[iarg]);
      }
   }
   
   printf("Prefix set to: %s\n", prefix);
   
   /* Begin reading dataset, error checking like a good programmer */
   iset = THD_open_dataset( argv[iarg] );
   CHECK_OPEN_ERROR( iset, argv[iarg] );
   THD_force_malloc_type( iset->dblk , DATABLOCK_MEM_MALLOC ) ;
   DSET_load(iset);
   CHECK_LOAD_ERROR(iset);
   
   //begin looping through!
   
   nval=DSET_NVALS(iset);
   INFO_message("Dataset read...\n");
   INFO_message("Number of Sub-bricks: %d\n", nval);

   if( nval == 0 ) ERROR_exit("no volumes to output?");

   /* how many digits do we need for the prefix trailer?   8 Dec 2016 */
   ndigits = (int)(log(nval)/log(10)+1);

   /* allocate sub_prefix */
   kk = strlen(prefix) + ndigits + 4; /* full length of prefix, plus 2 extra */
   sub_prefix = (char *)malloc(kk*sizeof(char));
   if( ! sub_prefix ) ERROR_exit("failed to alloc %d bytes for prefix", kk);

   oset = EDIT_empty_copy( iset ); //Easy to just copy!
   THD_force_malloc_type( oset->dblk , DATABLOCK_MEM_MALLOC ) ;

   printf("++ writing:");

   for( kk=0 ; kk < nval ; kk++ )
   {
   
      /* MODIFY to make single brik output */
      /* ALSO NEED TO CHANGE PREFIX EACH TIME! */
      
      sprintf(sub_prefix, "%s.%0*d", prefix, ndigits, kk);
      //sub_prefix = strncat( prefix, itoa(kk), 10 );

      // INFO_message("File Saved: %s", sub_prefix);
      if( kk == 0 || kk == (nval-1) )
         printf(" %s ", sub_prefix);
      else
         putchar('.');
      
      if( as_type ) datum = DSET_BRICK_TYPE(iset, kk);
      else          datum = MRI_float;

      EDIT_dset_items( oset, 
         ADN_datum_all , datum ,
         ADN_prefix , sub_prefix ,
         ADN_ntt, 1 ,
         ADN_nvals, 1, ADN_none);

      if( DSET_HAS_LABEL(iset, kk) ) {
         EDIT_dset_items( oset, 
             ADN_brick_label_one, DSET_BRICK_LABEL(iset, kk),
             ADN_none );
      } else {
         sprintf(newlabel, "%.16s[%d]", DSET_PREFIX(iset), kk);
         EDIT_dset_items( oset, 
             ADN_brick_label_one, newlabel,
             ADN_none );
      }
      
      /* either pass the data along or make a float version */
      if( as_type ) {
         EDIT_substitute_brick( oset, 0, datum, DSET_ARRAY(iset, kk));
         DSET_BRICK_FACTOR(oset,0) = DSET_BRICK_FACTOR(iset, kk);
      } else {
         inImage = THD_extract_float_brick(kk, iset);
         if( inImage == NULL)
            ERROR_exit("Failed to convert sub-brick %d of input dataset...",kk);

         EDIT_substitute_brick( oset, 0, MRI_float, MRI_FLOAT_PTR(inImage) );
      }
      
      DSET_write( oset );
      if( ! THD_is_file(DSET_BRIKNAME(oset)) ) 
         ERROR_exit("failed to write dataset %s", sub_prefix);
      //WROTE_DSET( oset );
      
      DSET_unload_one(iset, kk);

      /* for output, just clear borrowed pointer (being naughty?) */
      DSET_BRICK(oset, 0) = NULL;

      if( inImage) mri_clear_and_free( inImage );
   }//for
   
   printf("\n...Done\n");
      
   exit(0);
}
