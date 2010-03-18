#include "mrilib.h"

typedef signed char sbyte ;  /* for -byte */

/*--------------------------------------------------------------------------*/

char * get_surf_param(char *sname, char *parname)
{
   char com[1024]={""};
   char buf[1024]={""};
   char *out=NULL;

   FILE *output = NULL;

   sprintf(com,"\\SurfInfo -quiet -sep ';' -%s -i %s", parname, sname);

   if (!(output = popen (com, "r"))) {
      return(out);
   }

   out = fgets(buf, sizeof(buf), output);
   pclose(output);

   if (0) {
      fprintf(stderr,"Command: %s\n"
                     "ss: %s\n",
                     com, out?out:"NULL");
   }

   return(out);
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset **inset ; int ndset,ids ;
   char *prefix      = "Group"           ;
   char *suffix_head = ".grpincorr.niml" ;
   char *suffix_data = ".grpincorr.data" ;
   char *hfname , *dfname ;
   char atrib[2048] , *buf , *gstr ;
   byte *mask=NULL ;
   int mask_nx,mask_ny,mask_nz,nmask , nx,ny,nz,nvox ;
   NI_element *nel ;
   int nvec , *nvals , *ivec=NULL , iv,kk,nvv , nopt , do_delete=0 ;
   float *fac , *fv , val,top ;
   NI_float_array *facar ; NI_int_array *nvar ;
   short *sv ; sbyte *bv ; MRI_vectim *mv ; FILE *fp ; long long fsize ;
   int atim,btim,ctim ;
   int LRpairs = 0, Ns[2]={-1,-1}, Nv[2]={-1,-1}, Nm[2]={0, 0};
   int do_byte = 1 ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
 "Usage: 3dSetupGroupInCorr [options] dataset dataset ...\n"
 "\n"
 "This program is used to pre-process a collection of AFNI\n"
 "3D+time datasets for use with Group InstaCorr (3dGroupInCorr).\n"
 "\n"
 "* All the datasets input here will be treated as one sample\n"
 "  for the t-test performed in 3dGroupInCorr.  If you are going\n"
 "  to do a 2-sample t-test, then you will need to run this\n"
 "  program twice, once for each collection of datasets\n"
 "  (e.g., once for 'control subjects' and once for 'patients').\n"
 "\n"
 "* All datasets must have the same grid layout, since 3dGroupInCorr\n"
 "  will do voxel-by-voxel comparisons.  Usually, this means that\n"
 "  the datasets have been transformed to a standard space; for\n"
 "  example, using the @auto_tlrc script.\n"
 "\n"
 "* All the datasets use the same mask -- only voxels inside\n"
 "  this mask will be stored and processed.  If you do not give the\n"
 "  '-mask' option, then all voxels will be processed -- not usually\n"
 "  a good idea, since non-brain voxels will use up a LOT of memory\n"
 "  and CPU time in 3dGroupInCorr.\n"
 "  ++ If you use '-mask', you MUST use the same mask dataset\n"
 "     in all runs of 3dSetupGroupInCorr that will be input\n"
 "     at the same time to 3dGroupInCorr -- otherwise, the\n"
 "     computations in that program will make no sense AT ALL!\n"
 "  ++ This requirement is why there is no '-automask' option.\n"
 "\n"
 "* However, the datasets do NOT all have to have the same number\n"
 "  of time points or time spacing.  But each dataset must have\n"
 "  at least 9 points along the time axis!\n"
 "\n"
 "* The only pre-processing herein for each time series is to L2\n"
 "  normalize it (sum of squares = 1) and scale it to 8-bit bytes\n"
 "  (or to 16-bit shorts).\n"
 "  ++ You almost certainly want to use 3dBandpass and/or some other\n"
 "     code to pre-process the datasets BEFORE input to this program.\n"
 "  ++ See the SAMPLE SCRIPT below for a semi-reasonable way to\n"
 "     pre-process a collection of datasets for 3dGroupInCorr.\n"
 "\n"
 "* The outputs from this program are 2 files:\n"
 "  ++ PREFIX.grpincorr.niml is a text file containing the header\n"
 "     information that describes the data file.  This file is input\n"
 "     to 3dGroupInCorr to define one sample in the t-test.\n"
 "  ++ PREFIX.grpincorr.data is the data file, which contains\n"
 "     all the time series (in the mask) from all the datasets.\n"
 "  ++ The data file will usually be huge (gigabytes, perhaps).\n"
 "     You need to be sure you have enough disk space.\n"
 "  ++ If the output files already exist when you run this program,\n"
 "     then 3dSetupGroupInCorr will exit without processing the datasets!\n"
 "\n"
 "* See the help for 3dGroupInCorr for information on running that program.\n"
 "* The PDF file\n"
 "  http://afni.nimh.nih.gov/pub/dist/edu/latest/afni_handouts/instastuff.pdf\n"
 "  also has some information on the Group InstaCorr process (as well as all\n"
 "  the other 'Insta' functions added to AFNI).\n"
 "\n"
 "-------\n"
 "OPTIONS\n"
 "-------\n"
 "  -mask mset     = Mask dataset [highly recommended for volumetric e data!]\n"
 "\n"
 "  -prefix PREFIX = Set prefix name of output dataset\n"
 "\n"
 "  -short         = Store data as 16-bit shorts [used to be the default]\n"
 "\n"
 "  -byte          = Store data as 8-bit bytes rather than 16-bit shorts.\n"
 "                 ++ This will save memory in 3dGroupInCorr (and disk space),\n"
 "                    which can be important when using large collections of\n"
 "                    datasets.  Results will be very slightly less accurate\n"
 "                    than with '-short', but you'll have a hard time finding\n"
 "                    any place where this matters.\n"
 "                 ++ This is now the default [08 Feb 2010].\n"
 "\n"
 "  -DELETE        = Delete input datasets from disk after\n"
 "                   processing them one at a time into the\n"
 "                   output data file -- this very highly\n"
 "                   destructive option is intended to let\n"
 "                   you save disk space, if absolutely\n"
 "                   necessary.  *** BE CAREFUL OUT THERE! ***\n"
 "                 ++ If you are setting up for 3dGroupInCorr\n"
 "                    in a script that first uses 3dBandpass\n"
 "                    to filter the datasets, and then uses this\n"
 "                    program to finish the setup, then you\n"
 "                    COULD use '-DELETE' to remove the\n"
 "                    temporary 3dBandpass outputs as soon\n"
 "                    as they are no longer needed.\n"
 "\n"
"    Variations for surface-based data:\n"
"    ----------------------------------\n"
"    If you are working with one surface, no special options are needed.\n"
"    However, it is often the case that you want to perform correlations\n"
"    on both hemispheres. So in that case, you'll want to provide volume\n"
"    pairs (Left Hemi data, Right Hemi data). To help reduce the risk of\n"
"    user errors (the only kind we know of), you should also provide the\n"
"    domain parents for each of the hemispheres.\n"
"       -LRpairs L_SURF R_SURF: This option sets the domains for the left\n"
"                               and right hemisphere surfaces, and \n"
"                               indicates that the datasets to follow\n"
"                               are arranged in (Left, Right) pairs.\n"

/* No masking yet for surface-based
"                               This arrangement also applies to the mask\n"
"                               option which would then take a (Left, Right)\n"
"                               mask pair.\n"
*/

 "-------------\n"
 "SAMPLE SCRIPT  (tcsh syntax)\n"
 "-------------\n"
 "* Assume datasets are named in the following scheme (sub01, sub02, ...)\n"
 " ++ T1-weighted anatomical  = sub01_anat+orig\n"
 " ++ Resting state EPI       = sub01_rest+orig\n"
 " ++ Standard space template = ~/abin/MNI_avg152T1+tlrc\n"
 "\n"
 "#!/bin/tcsh\n"
 "\n"
 "# MNI-ize each subject's anat, then EPIs (at 2 mm resolution)\n"
 "\n"
 "cp -f ~/abin/MNI_avg152T1+tlrc.* .\n"
 "foreach fred ( sub*_anat+orig.HEAD )\n"
 "  set sub = `basename $fred _anat+orig.HEAD`\n"
 "  @auto_tlrc -base MNI_avg152T1+tlrc.HEAD -input $fred\n"
 "  adwarp -apar ${sub}_anat+tlrc.HEAD -dpar ${sub}_rest+orig.HEAD \\\n"
 "         -resam Cu -dxyz 2.0\n"
 "  3dAutomask -dilate 1 -prefix ${sub}_amask ${sub}_rest+tlrc.HEAD\n"
 "end\n"
 "\n"
 "# Combine individual EPI automasks into a group mask\n"
 "\n"
 "3dMean -datum float -prefix ALL_amaskFULL *_amask+tlrc.HEAD\n"
 "3dcalc -datum byte -prefix ALL_amask5050 -a ALL_amaskFULL+tlrc -expr 'step(a-0.499)'\n"
 "/bin/rm -f *_amask+tlrc.*\n"
 "\n"
 "# Bandpass and blur each dataset inside the group mask\n"
 "# (skip first 4 time points, and also remove global signal)\n"
 "\n"
 "foreach fred ( sub*_rest+tlrc.HEAD )\n"
 "  set sub = `basename $fred _rest+tlrc.HEAD`\n"
 "  3dmaskave -mask ALL_amask5050+tlrc -quiet $fred'[4..$]' > ${sub}_GS.1D\n"
 "  3dBandpass -mask ALL_amask5050+tlrc -blur 6.0 -band 0.01 0.10 -prefix ${sub}_BP\\\n"
 "             -input $fred'[4..$]' -ort ${sub}_GS.1D\n"
 "end\n"
 "/bin/rm -f *_GS.1D\n"
 "\n"
 "# Extract data for 3dGroupInCorr\n"
 "\n"
 "3dSetupGroupInCorr -mask ALL_amask5050 -prefix ALLshort -short *_BP+tlrc.HEAD\n"
 "\n"
 "# OR\n"
 "\n"
 "3dSetupGroupInCorr -mask ALL_amask5050 -prefix ALLbyte -byte *_BP+tlrc.HEAD\n"
 "\n"
 "/bin/rm -f *_BP+tlrc.*\n"
 "\n"
 "### At this point you could run (in 2 separate terminal windows)\n"
 "###   afni -niml MNI_avg152T1+tlrc\n"
 "###   3dGroupInCorr -setA ALLbyte.grpincorr.niml -verb\n"
 "### And away we go ....\n"
 "\n"
 "------------------\n"
 "CREDITS (or blame)\n"
 "------------------\n"
 "* Written by RWCox, 31 December 2009.\n"
 "* With a little help from my friends: Alex Martin, Steve Gotts, Ziad Saad.\n"
 "* With encouragement from MMK.\n"
 "\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dSetupGroupInCorr"); machdep();
   AFNI_logger("3dSetupGroupInCorr",argc,argv);
   PRINT_VERSION("3dSetupGroupInCorr"); AUTHOR("RW Cox");

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-byte") == 0 ){
       do_byte = 1 ; nopt++ ; continue ;
     }
     if( strcmp(argv[nopt],"-short") == 0 ){
       do_byte = 0 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-DELETE") == 0 ){
       do_delete = 1 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -prefix!") ;
       prefix = argv[nopt] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix option!") ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++nopt >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[nopt] ) ;
       CHECK_OPEN_ERROR(mset,argv[nopt]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL )
         ERROR_exit("Can't make mask from dataset '%s'",argv[nopt]) ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < 1 ) ERROR_exit("Mask is too small to process") ;
       nopt++ ; continue ;
     }

     if ( strcmp(argv[nopt],"-LRpairs") == 0 ){
       char *eo=NULL, *ss=NULL;
       int isrf=0;
       LRpairs = 1;
       if( nopt+2 >= argc ) ERROR_exit("Need 2 arguments after '-LRpairs'") ;
       for (isrf=0; isrf<2; ++isrf) {
          Ns[isrf] = strtod(argv[nopt+isrf+1], &eo);
          if (eo == argv[nopt+isrf+1] || Ns[isrf] <= 0) {
            fprintf(stderr,"Have surface %s\n", argv[nopt+isrf+1]);
            if (!(ss = get_surf_param(argv[nopt+isrf+1], "N_Node"))) {
               ERROR_exit("Failed to get info on '%s'",argv[nopt+isrf+1]);
            }
            Ns[isrf] = (int)strtod(ss, NULL);
          }
          fprintf(stderr,"Number of nodes in surf[%d]: %d\n", isrf, Ns[isrf]);
       }
       fprintf(stderr,"Assuming full masks.\n");
       Nm[0] = Ns[0];
       Nm[1] = Ns[1];

       nopt += 2;

       nopt++ ; continue ;
     }
     ERROR_exit("Unknown option: '%s'",argv[nopt]) ;
   }

   if (LRpairs && mask) {
      ERROR_exit("Mask is not yet compatible with -LRpairs");
      /* You should get two masks in that case, pass both to
         THD_2dset_to_vectim and be sure to set Nm[0] and Nm[1]
         according to the number of non zero values in each
         of them */
   }

   /*-- don't clobber existing files --*/

   hfname = (char *)malloc(strlen(prefix)+32) ;    /* header filename */
   dfname = (char *)malloc(strlen(prefix)+32) ;    /* data filename   */
   strcpy(hfname,prefix) ; strcat(hfname,suffix_head) ;
   strcpy(dfname,prefix) ; strcat(dfname,suffix_data) ;
   if( THD_is_file(hfname) )
     ERROR_exit("Output header file '%s' already exists",hfname) ;
   if( THD_is_file(dfname) )
     ERROR_exit("Output data file '%s' already exists",dfname) ;

   /*-- read input dataset headers --*/

   ndset = argc - nopt ;
   if( ndset < 2 )
     ERROR_exit("Must have at least 2 datasets on the command line!") ;
   if (LRpairs && (ndset % 2)) {
     ERROR_exit("With -LRpairs, you must have an even "
                "number of datasets on the command line!") ;
   }
   inset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*ndset) ;
   nvals = (int   *)malloc(sizeof(int)  *ndset) ; /* # time points per dataset */
   fac   = (float *)malloc(sizeof(float)*ndset) ; /* scale factor per dataset */

   for( ids=0 ; ids < ndset ; ids++ ){
     inset[ids] = THD_open_dataset(argv[nopt+ids]) ;    /* read header */
     CHECK_OPEN_ERROR(inset[ids],argv[nopt+ids]) ;      /* fail ==> bail */
     nvals[ids] = DSET_NVALS(inset[ids]) ;              /* save # time points */
     if (!LRpairs) {
      if( ids > 0 && !EQUIV_GRIDS(inset[0],inset[ids]) ) /* check for errors */
         ERROR_exit("Dataset grid %s doesn't match %s" ,
                     argv[nopt+ids] , argv[nopt]         ) ;
     } else {
      if( ids > 0 && !EQUIV_GRIDS(inset[ids%2],inset[ids]) )/*check for errors*/
         ERROR_exit("Dataset grid %s doesn't match %s" ,
                     argv[nopt+ids] , argv[nopt+ids%2]         ) ;
     }
     if( nvals[ids] < 9 )
       ERROR_exit("Dataset %s has only %d time points!?" ,
                  argv[nopt+ids] , nvals[ids] ) ;
   }

   if (!LRpairs) {
      nx = DSET_NX(inset[0]) ;
   } else {
      nx = DSET_NX(inset[0]) + DSET_NX(inset[1]);
   }
   nz = DSET_NZ(inset[0]) ;
   ny = DSET_NY(inset[0]) ;
   nvox = nx*ny*nz ;
   if( nvox < 2 ) ERROR_exit("Only 1 voxel in datasets?!") ;

   /*-- check or create mask --*/

   if( mask != NULL ){
     if( mask_nx != nx || mask_ny != ny || mask_nz != nz )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset(mask,1,sizeof(byte)*nvox) ;
     INFO_message("No mask ==> processing all %d voxels",nvox);
   }

   /* load index vector (if needed) */

   nvec = nmask ;
   if( nvec < nvox ){
     ivec = (int *)calloc(sizeof(int),nmask) ;
     for( iv=kk=0 ; kk < nvox ; kk++ ) if( mask[kk] ) ivec[iv++] = kk ;
   }

   /* open output data file */

   fp = fopen( dfname , "w" ) ;
   if( fp == NULL ) ERROR_exit("Can't open file '%s' for output",dfname) ;

   /*--- loop over datasets, convert to bytes/shorts, write out ---*/

   if (!LRpairs) {
      gstr = strdup( EDIT_get_geometry_string(inset[0]) ) ;
   } else {
      /* fake the number of Z voxels */
      int nxt=DSET_NX(inset[0]);
      DSET_NX(inset[0]) = nx;
      gstr = strdup( EDIT_get_geometry_string(inset[0]) ) ;
      DSET_NX(inset[0]) = nxt;
   }

   atim = NI_clock_time() ;

   if (LRpairs) {
      /* first do some checks */
      for (ids=0; ids < ndset; ++ids) {
         if (ids<2) {
            Nv[ids] = DSET_NVOX(inset[ids]);
         }
         if (DSET_NVOX(inset[ids]) > Ns[ids%2] ) {
             fprintf(stderr,"\n** Error Dataset %s:\n"
                            "Nvox %d >  %d(max nodes in surf[%d])\n",
                            DSET_BRIKNAME(inset[ids]),
                            DSET_NVOX(inset[ids]),Ns[ids%2], ids%2);
            fclose(fp) ; remove(dfname) ;
            ERROR_exit("Mismatch between data and LRpairs parameters") ;
         }
         if (DSET_NVOX(inset[ids]) != Nv[ids%2]) {
             fprintf(stderr,"\n** Error Dataset %s:\n"
                            "Novx %d != %d Nvox in dataset %s\n",
                            DSET_BRIKNAME(inset[ids]),
                            DSET_NVOX(inset[ids]), Nv[ids%2],
                              argv[nopt+ids%2]);
            fclose(fp) ; remove(dfname) ;
            ERROR_exit("Mismatch in input data") ;
         }
      }
      /* now do the work */
      ids = 0;
      while( ids < ndset ){
        fprintf(stderr,"++ Dataset %s:",DSET_BRIKNAME(inset[ids])) ;
        btim = NI_clock_time() ;

        /* extract all time series in the mask, as floats */

        mv = THD_2dset_to_vectim( inset[ids], NULL ,
                                  inset[ids+1], NULL ,
                                  0 );

        if( mv == NULL ){
          fclose(fp) ; remove(dfname) ; ERROR_exit("Can't load dataset!?") ;
        }
        ctim = NI_clock_time() ;
        fprintf(stderr," input =%5d ms;",ctim-btim) ;
        btim = ctim ;

        /* save memory by removing the two input dataset */

        THD_delete_3dim_dataset( inset[ids] , (Boolean)do_delete ) ;
        inset[ids] = NULL ;
        THD_delete_3dim_dataset( inset[ids+1] , (Boolean)do_delete ) ;
        inset[ids+1] = NULL ;

        THD_vectim_normalize( mv ) ; /* L2 normalize each time series */

        /* find largest absolute value over all vectors */
        nvv = mv->nvec * mv->nvals ; top = 0.0f ; fv = mv->fvec ;
        for( kk=0 ; kk < nvv ; kk++ ){
          val = fabsf(fv[kk]) ; if( val > top ) top = val ;
        }

        if( top == 0.0f ){
          fclose(fp) ; remove(dfname) ; ERROR_exit("Dataset is all zero?!") ;
        }

        /* scale to bytes/shorts to save disk and memory space */

        if( do_byte ){
          top = 127.4f / top ; fac[ids/2] = 1.0f / top ;  /* save scale factor */
          bv = (sbyte *)malloc(sizeof(sbyte)*nvv) ;       /* output array */
          for( kk=0 ; kk < nvv ; kk++ ) bv[kk] = (sbyte)rintf(top*fv[kk]) ;
        } else {
          top = 32766.0f / top ; fac[ids/2] = 1.0f / top ;  /* save scale factor */
          sv = (short *)malloc(sizeof(short)*nvv) ;       /* output array */
          for( kk=0 ; kk < nvv ; kk++ ) sv[kk] = (short)rintf(top*fv[kk]) ;
        }
        VECTIM_destroy(mv) ;

        ctim = NI_clock_time() ;
        fprintf(stderr," normalize =%5d ms;",ctim-btim) ;
        btim = ctim ;

        /* write output array */

        if( do_byte ) kk = fwrite( bv , sizeof(sbyte) , nvv , fp ) ;
        else          kk = fwrite( sv , sizeof(short) , nvv , fp ) ;
        if( kk < nvv ){
          fclose(fp) ; remove(dfname) ;
          ERROR_exit("Write to '%s' failed -- disk full? permission?",dfname) ;
        }
        if( do_byte ) free(bv) ;
        else          free(sv) ;  /* toss this trash */
        fflush(fp) ;

        ctim = NI_clock_time() ;
        fprintf(stderr," output =%5d ms\n",ctim-btim) ;

        ids += 2;
      } /* end of dataset pairs loop */
      ndset /= 2;
   } else {
      for( ids=0 ; ids < ndset ; ids++ ){
        fprintf(stderr,"++ Dataset %s:",DSET_BRIKNAME(inset[ids])) ;
        btim = NI_clock_time() ;

        /* extract all time series in the mask, as floats */
        mv = THD_dset_to_vectim( inset[ids] , mask , 0 ) ;
        if( mv == NULL ){
          fclose(fp) ; remove(dfname) ; ERROR_exit("Can't load dataset!?") ;
        }
        ctim = NI_clock_time() ;
        fprintf(stderr," input =%5d ms;",ctim-btim) ;
        btim = ctim ;

        /* save memory by removing this input dataset */

        THD_delete_3dim_dataset( inset[ids] , (Boolean)do_delete ) ;
        inset[ids] = NULL ;

        THD_vectim_normalize( mv ) ; /* L2 normalize each time series */

        /* find largest absolute value over all vectors */

        nvv = mv->nvec * mv->nvals ; top = 0.0f ; fv = mv->fvec ;
        for( kk=0 ; kk < nvv ; kk++ ){
          val = fabsf(fv[kk]) ; if( val > top ) top = val ;
        }

        if( top == 0.0f ){
          fclose(fp) ; remove(dfname) ; ERROR_exit("Dataset is all zero?!") ;
        }

        /* scale to bytes/shorts to save disk and memory space */

        if( do_byte ){
          top = 127.4f / top ; fac[ids] = 1.0f / top ;  /* save scale factor */
          bv = (sbyte *)malloc(sizeof(sbyte)*nvv) ;     /* output array */
          for( kk=0 ; kk < nvv ; kk++ ) bv[kk] = (sbyte)rintf(top*fv[kk]) ;
        } else {
          top = 32766.0f / top ; fac[ids] = 1.0f / top ;  /* save scale factor */
          sv = (short *)malloc(sizeof(short)*nvv) ;       /* output array */
          for( kk=0 ; kk < nvv ; kk++ ) sv[kk] = (short)rintf(top*fv[kk]) ;
        }
        VECTIM_destroy(mv) ;

        ctim = NI_clock_time() ;
        fprintf(stderr," normalize =%5d ms;",ctim-btim) ;
        btim = ctim ;

        /* write output array */

        if( do_byte ) kk = fwrite( bv , sizeof(sbyte) , nvv , fp ) ;
        else          kk = fwrite( sv , sizeof(short) , nvv , fp ) ;
        if( kk < nvv ){
          fclose(fp) ; remove(dfname) ;
          ERROR_exit("Write to '%s' failed -- disk full? permission?",dfname) ;
        }
        if( do_byte ) free(bv) ;
        else          free(sv) ;  /* toss this trash */
        fflush(fp) ;

        ctim = NI_clock_time() ;
        fprintf(stderr," output =%5d ms\n",ctim-btim) ;

      } /* end of dataset loop */
   }
   /*--- close data file, print some helpful info ---*/

   fclose(fp) ;
   fsize = THD_filesize(dfname) ;
   INFO_message("Wrote data file %s = %s bytes (about %s)",
                dfname , commaized_integer_string(fsize) ,
                         approximate_number_string((double)fsize) ) ;

   ctim = NI_clock_time() - atim ;
   ININFO_message("Total elapsed time = %d ms = %.2f min",ctim,ctim/(60000.0f)) ;

   /*--- create NIML data element to describe what's in the data file ---*/

   /* if have a mask, then must write data indicating which voxels are in use */
   /* if no mask, then don't write that data, since we can generate as needed */

   if( ivec != NULL ){
     nel = NI_new_data_element( "3dGroupInCorr" , nvec ) ;
     NI_add_column( nel , NI_INT , ivec ) ;
   } else {
     nel = NI_new_data_element( "3dGroupInCorr" , 0 ) ;
   }

   sprintf(atrib,"%d",nvec) ;
   NI_set_attribute( nel , "nvec" , atrib ) ;       /* # time series vectors */

   sprintf(atrib,"%d",ndset) ;
   NI_set_attribute( nel , "ndset" , atrib ) ;                 /* # datasets */

   NI_set_attribute( nel , "datum" ,
                     (do_byte) ? "byte" : "short" ) ; /* type of data stored */

   nvar = (NI_int_array *)malloc(sizeof(NI_int_array)) ;
   nvar->num = ndset ; nvar->ar = nvals ;
   buf = NI_encode_int_list( nvar , "," ) ;
   NI_set_attribute( nel , "nvals" , buf ) ;            /* # TRs per dataset */

   facar = (NI_float_array *)malloc(sizeof(NI_float_array)) ;
   facar->num = ndset ; facar->ar = fac ;
   buf = NI_encode_float_list( facar , "," ) ;
   NI_set_attribute( nel , "fac" , buf ) ;       /* scale factor per dataset */

                                                     /* describe 3D geometry */
   NI_set_attribute( nel , "geometry" , gstr ) ;      /* of what we're about */

   NI_set_attribute( nel , "datafile" , dfname ) ;         /* data file name */

   if (LRpairs) {
      char ss[256]={""};

      sprintf(ss,"%d, %d", Ns[0], Ns[1]);
      NI_set_attribute( nel, "LRpair_nnode", ss);

      sprintf(ss,"%d, %d", Nm[0], Nm[1]);
      NI_set_attribute( nel, "LRpair_ninmask", ss);
   }

   /*--- write header file ---*/

   kk = NI_write_element_tofile( hfname , nel , NI_BINARY_MODE ) ;
   if( kk < 0 ){     /* should probably never happen */
     remove(dfname) ;
     ERROR_exit("Failed to write head file %s; deleted data file %s" ,
                hfname , dfname ) ;
   }

   INFO_message("Wrote head file %s",hfname) ;
   exit(0) ;
}
