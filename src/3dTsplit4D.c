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
      "examples:\n"
      "\n"
      "   1. Write the 152 time point dataset, epi_r1+orig, to 152 single\n"
      "      volume datasets, out/epi.000+orig ... epi.151+orig.\n"
      "\n"
      "         mkdir out\n"
      "         3dTsplit4D -prefix out/epi epi_r1+orig\n"
      "\n"
      "   2. Do the same thing, but write to 152 NIFTI volume datasets,\n"
      "      out/epi.000.nii ... out/epi.151.nii.  Include .nii in -prefix.\n"
      "\n"
      "         mkdir out\n"
      "         3dTsplit4D -prefix out/epi.nii epi_r1+orig\n"
      "\n"
      "   3. Convert an AFNI stats dataset (betas, t-stats, F-stats) into\n"
      "      a set of NIFTI volume datasets, including the volume labels\n"
      "      in the file names.\n"
      "\n"
      "         3dTsplit4D -prefix stats.FT.nii -label_prefix stats.FT+tlrc\n"
      "\n"
      " -prefix PREFIX : Prefix of the output datasets\n"
      "                  Numbers will be added after the prefix to denote\n"
      "                  prior sub-brick.\n"
      " -digits DIGITS : number of digits to use for output filenames\n"
      " -keep_datum    : output uses original datum (no conversion to float)\n"
      " -label_prefix  : include volume label in each output prefix\n"
      "\n\n"
      "Authored by: Peter Molfese, UConn"
      );

   PRINT_COMPILE_DATE; 
   return(0);
}

/* create an output prefix, the length of which might grow with label
 *
 *    prelim   : initial prefix
 *    ndigits  : number of digits used for zero-padded index in the name
 *    index    : index will be the second part of the name
 *    label    : if set, include the label string, with space or #
 *               replaced by _
 *    ext      : if set, include .ext
 *
 * If prelim is NULL, free local memory and return NULL.
 *
 * return a stactic char pointer, that should not be freed
 *                                     [12 Apr 2024 rickr]
 */
char * make_output_prefix(char *prelim, int ndigits, int index,
                          char *label, char *ext)
{
   static char *sprefix=NULL, *slabel=NULL;
   static int   splen  =0,     sllen=0;
   int          lablen=0, extlen=0;   /* label and extension lengths */
   char        *lp;                   /* for modifying label */
   int          cp, osize;

   /* if prelim is NULL, it is a request to free memory and return */
   if ( !prelim ) {
      free(sprefix); free(slabel);
      splen = sllen = 0;
      return NULL;  /* a happy NULL, in this case */
   }

   /* start by checking sizes */
   if( label ) lablen = strlen(label);
   if( ext )   extlen = strlen(ext);

   /* account for possible: prefix, digits, label, extension, separators */
   osize = strlen(prelim) + ndigits + lablen + extlen + 8;

   /* if we need more memory, allocate or grow it */
   if( osize > splen ) {
      sprefix = (char *)realloc(sprefix, osize*sizeof(char));
      if( ! sprefix ) ERROR_exit("failed to alloc %d bytes for prefix", osize);
      splen = osize;
   }

   /* and for any label (since it may be modified) */
   if( lablen > sllen ) {
      slabel = (char *)realloc(slabel, lablen*sizeof(char));
      if( ! slabel ) ERROR_exit("failed to alloc %d bytes for label", lablen);
      sllen = lablen;
   }

   /* start with prefix and digits */
   sprintf(sprefix, "%s.%0*d", prelim, ndigits, index);

   /* if there is a label, add it, replacing space or # with _ */
   if( label ) {
      /* copy and modify */
      strcpy(slabel, label);
      for(cp=0, lp=slabel; cp < lablen; cp++, lp++)
        if( *lp == ' ' || *lp == '#' )
          *lp = '_';
      
      /* and append .slabel to the final prefix */
      strcat(sprefix, ".");
      strcat(sprefix, slabel);
   }

   /* if there is a non-AFNI extension, use it       9 Dec 2016 [rickr] */
   if( ext ) {
      strcat(sprefix, ".");
      strcat(sprefix, ext);
   }

   /* and return sprefix as the new prefix */
   return sprefix;
}

/* a few updates    8 Dec 2016 [rickr] */

int main( int argc, char *argv[] )
{
   THD_3dim_dataset *iset, *oset;
   int   iarg=1, kk, nval;
   int   datum=MRI_float, keep_datum=0, ndigits=0, smode;
   int   do_label_prefix=0;
   char *prefix = "SPLIT";
   char *sub_prefix, newlabel[32];
   char *precopy=NULL, *exten=NULL;  /* copied prefix and any needed ext */
   MRI_IMAGE *inImage=NULL;
   
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
      } else if( strcmp( argv[iarg], "-digits") == 0 )
      {
         ndigits = atoi(argv[++iarg]);
         if( ndigits <= 0 )
            ERROR_exit("bad -digits '%s'", argv[iarg-1]);
      } else if( strcmp( argv[iarg], "-keep_datum") == 0 ) {
         keep_datum = 1;
      } else if( strcmp( argv[iarg], "-label_prefix") == 0 ) {
         do_label_prefix = 1;
      } else {
         ERROR_exit("unknown option %s", argv[iarg]);
      }
   }
   
   INFO_message("Prefix set to: %s\n", prefix);
   
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
   if( ndigits == 0 )
      ndigits = (int)(log(nval)/log(10)+1);

   /* prep for a new prefix which can be altered */
   /* (precopy and exten will be used for actual output prefix) */
   precopy = nifti_strdup(prefix);
   exten = NULL;
   smode = storage_mode_from_filename(prefix);
   if( has_known_non_afni_extension(precopy)
        && is_writable_storage_mode(smode) ) {
      exten = find_filename_extension(precopy);
      /* if found, terminate actual prefix, and point exten past '.' */
      if( exten && exten > precopy )
         *exten++ = '\0';
      INFO_message("Using new prefix for non-AFNI write: %s\n", precopy);
   }

   oset = EDIT_empty_copy( iset ); //Easy to just copy!
   THD_force_malloc_type( oset->dblk , DATABLOCK_MEM_MALLOC ) ;

   printf("++ writing:");

   for( kk=0 ; kk < nval ; kk++ )
   {
      /* MODIFY to make single brik output */

      /* create an output prefix */
      sub_prefix = make_output_prefix(
                        precopy, ndigits, kk,
                        (do_label_prefix && DSET_HAS_LABEL(iset, kk)) ?
                           DSET_BRICK_LABEL(iset, kk) : NULL,
                        exten);
      
      // INFO_message("File Saved: %s", sub_prefix);
      if( kk == 0 || kk == (nval-1) )
         printf(" %s ", sub_prefix);
      else
         putchar('.');
      
      if( keep_datum ) datum = DSET_BRICK_TYPE(iset, kk);
      else             datum = MRI_float;

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
      if( keep_datum ) {
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
   }  /* for each volume */
   

   /* and free the allocated memory */
   make_output_prefix(NULL, 0, 0, NULL, NULL);

   printf("\n...Done\n");

   exit(0);
}
