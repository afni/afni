#include "mrilib.h"

#ifdef USE_OMP
#include "mri_nstats.c"
#endif

/* #defines moved to editvol.h */

/*-----------------------------------------------------------------------*/

int LS_decode_parameters(char *str, float *params)
{
   int iparams=-1;
   int nc=0, k, icol[10], stp=0, ncol = 0, ii;
   char strbuf[256];
   char *ce=NULL;

   ENTRY("LS_decode_parameters");

   if (!str || !params) RETURN(iparams);

   iparams = 0;

   #if 1 /* clumsy but more flexible. */
   /* find my ':' */
   nc = strlen(str);
   ncol = 0;
   k = 0;
   while (k<nc) {
      if (str[k] == ':') {icol[ncol] = k; ++ncol; }
      ++k;
   }
   if (!ncol) RETURN(iparams);

   for (k=0; k<ncol; ++k) {
      if (k<ncol-1) { stp = icol[k+1]-1; }
      else { stp = nc; }
      for (ii=0; ii<stp-icol[k]; ++ii) strbuf[ii]=str[icol[k]+ii+1];
      strbuf[stp-icol[k]] = '\0';
      /* fprintf(stderr, ">>>%s<<<\n", strbuf); */
      ++iparams; params[iparams] = strtod(strbuf, NULL);
   }
   #else /* would skip cases like :: which is kinda like :0.0: */
   ce = strtok(str,":");
   while (ce = strtok(NULL,":")) {
      ++iparams; params[iparams] = strtod(ce, NULL);
      /* fprintf(stderr, ">>>%s<<<\n", ce); */
   }
   #endif
   params[0] = iparams;

   RETURN(iparams);
}

/*-----------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset ;
   int ncode=0 , code[MAX_NCODE] , iarg=1 , ii ;
   float codeparams[MAX_NCODE][MAX_CODE_PARAMS+1];
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   char *prefix="./localstat" ;
   int ntype=0 ; float na=0.0f,nb=0.0f,nc=0.0f ;
   int do_fwhm=0 , verb=1 ;
   int npv = -1;
   int ipv;
   int datum = MRI_float;

   /*---- for the clueless who wish to become clued-in ----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dLocalstat [options] dataset\n"
      "\n"
      "This program computes statistics at each voxel, based on a\n"
      "local neighborhood of that voxel.\n"
      " - The neighborhood is defined by the '-nbhd' option.\n"
      " - Statistics to be calculated are defined by the '-stat' option(s).\n"
      "\n"
      "OPTIONS\n"
      "-------\n"
      " -nbhd 'nnn' = The string 'nnn' defines the region around each\n"
      "               voxel that will be extracted for the statistics\n"
      "               calculation.  The format of the 'nnn' string are:\n"
      "               * 'SPHERE(r)' where 'r' is the radius in mm;\n"
      "                 the neighborhood is all voxels whose center-to-\n"
      "                 center distance is less than or equal to 'r'.\n"
      "                 ** A negative value for 'r' means that the region\n"
      "                    is calculated using voxel indexes rather than\n"
      "                    voxel dimensions; that is, the neighborhood\n"
      "                    region is a \"sphere\" in voxel indexes of\n"
      "                    \"radius\" abs(r).\n"
      "               * 'RECT(a,b,c)' is a rectangular block which\n"
      "                 proceeds plus-or-minus 'a' mm in the x-direction,\n"
      "                 'b' mm in the y-direction, and 'c' mm in the\n"
      "                 z-direction.  The correspondence between the\n"
      "                 dataset xyz axes and the actual spatial orientation\n"
      "                 can be determined by using program 3dinfo.\n"
      "                 ** A negative value for 'a' means that the region\n"
      "                    extends plus-and-minus abs(a) voxels in the\n"
      "                    x-direction, rather than plus-and-minus a mm.\n"
      "                    Mutatis mutandum for negative 'b' and/or 'c'.\n"
      "               * 'RHDD(a)' where 'a' is the size parameter in mm;\n"
      "                 this is Kepler's rhombic dodecahedron [volume=2*a^3].\n"
      "               * 'TOHD(a)' where 'a' is the size parameter in mm;\n"
      "                 this is a truncated octahedron. [volume=4*a^3]\n"
      "                 ** This is the polyhedral shape that tiles space\n"
      "                    and is the most 'sphere-like'.\n"
      "               * If no '-nbhd' option is given, the region extracted\n"
      "                 will just be the voxel and its 6 nearest neighbors.\n"
      "               * Voxels not in the mask (if any) or outside the\n"
      "                 dataset volume will not be used.  This means that\n"
      "                 different output voxels will have different numbers\n"
      "                 of input voxels that went into calculating their\n"
      "                 statistics.  The 'num' statistic can be used to\n"
      "                 get this count on a per-voxel basis, if you need it.\n"
      "\n"
      " -stat sss   = Compute the statistic named 'sss' on the values\n"
      "               extracted from the region around each voxel:\n"
      "               * mean   = average of the values\n"
      "               * stdev  = standard deviation\n"
      "               * var    = variance (stdev*stdev)\n"
      "               * cvar   = coefficient of variation = stdev/fabs(mean)\n"
      "               * median = median of the values\n"
      "               * MAD    = median absolute deviation\n"
      "               * min    = minimum\n"
      "               * max    = maximum\n"
      "               * absmax = maximum of the absolute values\n"
      "               * num    = number of the values in the region:\n"
      "                          with the use of -mask or -automask,\n"
      "                          the size of the region around any given\n"
      "                          voxel will vary; this option lets you\n"
      "                          map that size.  It may be useful if you\n"
      "                          plan to compute a t-statistic (say) from\n"
      "                          the mean and stdev outputs.\n"
      "               * sum    = sum of the values in the region:\n"
      "               * FWHM   = compute (like 3dFWHM) image smoothness\n"
      "                          inside each voxel's neighborhood.  Results\n"
      "                          are in 3 sub-bricks: FWHMx, FHWMy, and FWHMz.\n"
      "                          Places where an output is -1 are locations\n"
      "                          where the FWHM value could not be computed\n"
      "                          (e.g., outside the mask).\n"
      "               * FWHMbar= Compute just the average of the 3 FWHM values\n"
      "                          (normally would NOT do this with FWHM also).\n"
      "               * perc:P0:P1:Pstep = \n"
      "                          Compute percentiles between P0 and P1 with a \n"
      "                          step of Pstep.\n"
      "                          Default P1 is equal to P0 and default P2 = 1\n"
      "               * ALL    = all of the above, in that order \n"
      "                         (except for FWHMbar and perc).\n"
      "               More than one '-stat' option can be used.\n"
      "\n"
      " -mask mset  = Read in dataset 'mset' and use the nonzero voxels\n"
      "               therein as a mask.  Voxels NOT in the mask will\n"
      "               not be used in the neighborhood of any voxel. Also,\n"
      "               a voxel NOT in the mask will have its statistic(s)\n"
      "               computed as zero (0).\n"
      " -automask   = Compute the mask as in program 3dAutomask.\n"
      "               -mask and -automask are mutually exclusive: that is,\n"
      "               you can only specify one mask.\n"
      "\n"
      " -use_nonmask = Just above, I said that voxels NOT in the mask will\n"
      "                not have their local statistics computed.  This option\n"
      "                will make it so that voxels not in the mask WILL have\n"
      "                their local statistics computed from all voxels in\n"
      "                their neighborhood that ARE in the mask.\n"
      "               * You could use '-use_nonmask' to compute the average\n"
      "                 local white matter time series, for example, even at\n"
      "                 non-WM voxels.\n"
      "\n"
      " -prefix ppp = Use string 'ppp' as the prefix for the output dataset.\n"
      "               The output dataset is normally stored as floats.\n"
      "\n"
      " -datum type = Coerce the output data to be stored as the given type, \n"
      "               which may be byte, short, or float.\n"
      "               Default is float\n"
      "\n"
      " -quiet      = Stop the highly informative progress reports.\n"
      "\n"
      "Author: RWCox - August 2005.  Instigator: ZSSaad.\n"
     ) ;
     PRINT_AFNI_OMP_USAGE("3dLocalstat",NULL) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
   enable_mcw_malloc() ;
#endif

   PRINT_VERSION("3dLocalstat"); mainENTRY("3dLocalstat main"); machdep();
   AFNI_logger("3dLocalstat",argc,argv); AUTHOR("Emperor Zhark");

   /* initialize codeparams */
   for (ii=0; ii<MAX_NCODE; ++ii) codeparams[ii][0] = -1.0;

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strncmp(argv[iarg],"-q",2) == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }

     /**** -datum type ****/

     if( strncasecmp(argv[iarg],"-datum",6) == 0 ){
        if( ++iarg >= argc )
          ERROR_exit("need an argument after -datum!\n") ;
        if( strcasecmp(argv[iarg],"short") == 0 ){
           datum = MRI_short ;
        } else if( strcasecmp(argv[iarg],"float") == 0 ){
           datum = MRI_float ;
        } else if( strcasecmp(argv[iarg],"byte") == 0 ){
           datum = MRI_byte ;
        } else {
            ERROR_exit("-datum of type '%s' not supported in 3dLocalstat!\n",
                        argv[iarg]) ;
        }
        iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-use_nonmask") == 0 ){        /* 13 Jul 2009 */
       SetSearchAboutMaskedVoxel(1) ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL )
         ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       if( verb ) INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 2 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ;
       iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-stat") == 0 ){
       char *cpt ;
       float strt, stp, jmp;
       int iizz;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-stat'") ;

       cpt = argv[iarg] ; if( *cpt == '-' ) cpt++ ;
            if( strcasecmp(cpt,"mean")  == 0 ) code[ncode++] = NSTAT_MEAN  ;
       else if( strcasecmp(cpt,"stdev") == 0 ) code[ncode++] = NSTAT_SIGMA ;
       else if( strcasecmp(cpt,"var")   == 0 ) code[ncode++] = NSTAT_VAR   ;
       else if( strcasecmp(cpt,"cvar")  == 0 ) code[ncode++] = NSTAT_CVAR  ;
       else if( strcasecmp(cpt,"median")== 0 ) code[ncode++] = NSTAT_MEDIAN;
       else if( strcasecmp(cpt,"MAD")   == 0 ) code[ncode++] = NSTAT_MAD   ;
       else if( strcasecmp(cpt,"min")   == 0 ) code[ncode++] = NSTAT_MIN   ;
       else if( strcasecmp(cpt,"max")   == 0 ) code[ncode++] = NSTAT_MAX   ;
       else if( strcasecmp(cpt,"absmax")== 0 ) code[ncode++] = NSTAT_ABSMAX;
       else if( strcasecmp(cpt,"num")   == 0 ) code[ncode++] = NSTAT_NUM   ;
       else if( strcasecmp(cpt,"sum")   == 0 ) code[ncode++] = NSTAT_SUM   ;
       else if( strcasecmp(cpt,"fwhm")  == 0 ){code[ncode++] = NSTAT_FWHMx ;
                                               code[ncode++] = NSTAT_FWHMy ;
                                               code[ncode++] = NSTAT_FWHMz ;
                                               do_fwhm++                   ;}
       else if( strncasecmp(cpt,"perc",4) == 0) {
         /* How many you say? */
         if (LS_decode_parameters(cpt, codeparams[ncode]) <= 0) {
            ERROR_exit("Need params with perc stat");
         }
         strt = codeparams[ncode][1]; stp = strt; jmp = 1.0;
         if ((int)codeparams[ncode][0] == 2) {
            stp = codeparams[ncode][2]; if (stp == 0) stp = strt;
         }else if ((int)codeparams[ncode][0] == 3) {
            stp = codeparams[ncode][2]; jmp = codeparams[ncode][3];
         }
         if (jmp == 0.0) jmp = 1.0;
         npv = ceil((stp - strt)/jmp);
         if (npv > MAX_CODE_PARAMS) {
            ERROR_exit( "A max of %d percentiles allowed. You have %d\n",
                        MAX_CODE_PARAMS, npv);
         }
         ipv = 1;
         codeparams[ncode][1] = strt;
         while (codeparams[ncode][ipv]+jmp <= stp) {
            codeparams[ncode][ipv+1] = codeparams[ncode][ipv]+jmp; ++ipv;
         }
         codeparams[ncode][0] = ipv;
         iizz = (int)(codeparams[ncode][0]);
         /* fprintf( stderr,
                     "Have %d percentiles (coded with %d) "
                     "starting at code index %d\n",
                      iizz, NSTAT_PERCENTILE, ncode); */
         for (ipv=0; ipv<iizz; ++ipv)  code[ncode++] = NSTAT_PERCENTILE;
       }
       else if( strcasecmp(cpt,"fwhmbar")==0 ) code[ncode++] = NSTAT_FWHMbar;
       else if( strcasecmp(cpt,"ALL")   == 0 ){
         code[ncode++] = NSTAT_MEAN  ; code[ncode++] = NSTAT_SIGMA ;
         code[ncode++] = NSTAT_VAR   ; code[ncode++] = NSTAT_CVAR  ;
         code[ncode++] = NSTAT_MEDIAN; code[ncode++] = NSTAT_MAD   ;
         code[ncode++] = NSTAT_MIN   ; code[ncode++] = NSTAT_MAX   ;
         code[ncode++] = NSTAT_ABSMAX; code[ncode++] = NSTAT_NUM   ;
         code[ncode++] = NSTAT_SUM   ;
         code[ncode++] = NSTAT_FWHMx ; code[ncode++] = NSTAT_FWHMy ;
         code[ncode++] = NSTAT_FWHMz ; do_fwhm++ ;
       }
       else
         ERROR_exit("-stat '%s' is an unknown statistic type",argv[iarg]) ;

       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nbhd") == 0 ){
       char *cpt ;
       if( ntype  >  0    ) ERROR_exit("Can't have 2 '-nbhd' options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-nbhd'") ;

       cpt = argv[iarg] ;
       if( strncasecmp(cpt,"SPHERE",6) == 0 ){
         sscanf( cpt+7 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a SPHERE of radius 0") ;
         ntype = NTYPE_SPHERE ;
       } else if( strncasecmp(cpt,"RECT",4) == 0 ){
         sscanf( cpt+5 , "%f,%f,%f" , &na,&nb,&nc ) ;
         if( na == 0.0f && nb == 0.0f && nc == 0.0f )
           ERROR_exit("'RECT(0,0,0)' is not a legal neighborhood") ;
         ntype = NTYPE_RECT ;
       } else if( strncasecmp(cpt,"RHDD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a RHDD of radius 0") ;
         ntype = NTYPE_RHDD ;
       } else if( strncasecmp(cpt,"TOHD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a TOHD of radius 0") ;
         ntype = NTYPE_TOHD ;
       } else {
         ERROR_exit("Unknown -nbhd shape: '%s'",cpt) ;
       }
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*---- check for stupid user inputs ----*/

   if( ncode <= 0 ) ERROR_exit("No '-stat' options given?");

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     int mmm ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     mmm = THD_countmask( DSET_NVOX(inset) , mask ) ;
     if( verb ) INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 2 ) ERROR_exit("Automask is too small to process") ;
   }

   /*---- create neighborhood -----*/

   if( ntype <= 0 ){         /* default neighborhood */
     ntype = NTYPE_SPHERE ; na = -1.01f ;
     if( verb ) INFO_message("Using default neighborhood = self + 6 neighbors") ;
   }

   switch( ntype ){
     default:
       ERROR_exit("WTF?  ntype=%d",ntype) ;  /* should not happen */

     case NTYPE_SPHERE:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_spheremask( dx,dy,dz , na ) ;
     }
     break ;

     case NTYPE_RECT:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = 1.0f; na = -na; } else dx = fabsf(DSET_DX(inset));
       if( nb < 0.0f ){ dy = 1.0f; nb = -nb; } else dy = fabsf(DSET_DY(inset));
       if( nc < 0.0f ){ dz = 1.0f; nc = -nc; } else dz = fabsf(DSET_DZ(inset));
       nbhd = MCW_rectmask( dx,dy,dz , na,nb,nc ) ;
     }
     break ;

     case NTYPE_RHDD:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_rhddmask( dx,dy,dz , na ) ;
     }
     break ;

     case NTYPE_TOHD:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_tohdmask( dx,dy,dz , na ) ;
     }
     break ;
   }

   if( verb ) INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;

   if( do_fwhm && nbhd->num_pt < 19 )
     ERROR_exit("FWHM requires neighborhood of at least 19 voxels!");

   /*---- actually do some work for a change ----*/

   THD_localstat_verb(verb) ;
   THD_localstat_datum(datum);
   outset = THD_localstat( inset , mask , nbhd , ncode , code, codeparams ) ;

   DSET_unload(inset) ;

   if( outset == NULL ) ERROR_exit("Function THD_localstat() fails?!") ;

   /*---- save resulting dataset ----*/

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalstat" , argc,argv , outset ) ;

   { char *lcode[MAX_NCODE] , lll[MAX_NCODE] , *slcode, pcode[MAX_NCODE];
     int ipv = 0;
     lcode[NSTAT_MEAN]   = "MEAN" ; lcode[NSTAT_SIGMA]  = "SIGMA"  ;
     lcode[NSTAT_CVAR]   = "CVAR" ; lcode[NSTAT_MEDIAN] = "MEDIAN" ;
     lcode[NSTAT_MAD]    = "MAD"  ; lcode[NSTAT_MAX]    = "MAX"    ;
     lcode[NSTAT_MIN]    = "MIN"  ; lcode[NSTAT_ABSMAX] = "ABSMAX" ;
     lcode[NSTAT_VAR]    = "VAR"  ; lcode[NSTAT_NUM]    = "NUM"    ;
     lcode[NSTAT_FWHMx]  = "FWHMx"; lcode[NSTAT_PERCENTILE] = "PERC";
     lcode[NSTAT_FWHMy]  = "FWHMy"; lcode[NSTAT_SUM]    = "SUM"    ;
     lcode[NSTAT_FWHMz]  = "FWHMz";
     if( DSET_NVALS(inset) == 1 ){
       ii=0;
       while(ii < DSET_NVALS(outset)) {
         if (code[ii%ncode] == NSTAT_PERCENTILE) {
            if (ipv < 0) ipv = ii%ncode;
            sprintf(pcode,"perc:%.2f", codeparams[ipv][1+ii%ncode-ipv]);
            slcode = pcode;
         } else {
            ipv = -1;
            slcode = lcode[code[ii%ncode]];
         }
         /*fprintf(stderr,"CODE %d: %s\n", ii, slcode);*/
         EDIT_dset_items( outset ,
                            ADN_brick_label_one+ii , slcode ,
                          ADN_none ) ;
         ++ii;
      }

     } else {
       for( ii=0 ; ii < DSET_NVALS(outset) ; ii++ ){
         if (code[ii%ncode] == NSTAT_PERCENTILE) {
            if (ipv < 0) ipv = ii%ncode;
            sprintf(pcode,"perc:%.2f", codeparams[ipv][1+ii%ncode-ipv]);
            slcode = pcode;
         } else {
            ipv = -1;
            slcode = lcode[code[ii%ncode]];
         }
         sprintf(lll,"%s[%d]",slcode,(ii/ncode)) ;
         /* fprintf(stderr,"CODE sb%d: %s\n", ii, lll); */
         EDIT_dset_items( outset , ADN_brick_label_one+ii,lll, ADN_none ) ;
       }
     }
   }

   DSET_write( outset ) ;
   if( verb ) WROTE_DSET( outset ) ;
   exit(0) ;
}
