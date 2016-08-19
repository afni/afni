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

void usage_3dLocalstat(int detail) 
{
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
"                 ** The distances are computed in 3 dimensions,\n"
"                    so a SPHERE(1) on a 1mm3 grid gives a 7 voxel-\n"
"                    neighborhood - the center voxel and the six\n"
"                    facing voxels, 4 in plane and 2 above and below.\n"
"                    A SPHERE(1.42) contains 19 voxels, the center voxel\n"
"                    with the 8 others in plane, and the 5 above and\n"
"                    below (all voxels sharing an edge with the center)\n"
"                    A SPHERE(1.74) contains 27 voxels, all voxels\n"
"                    sharing a face, edge or corner with the center\n"
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
"                 ** Note the a,b,c are not the full dimensions of\n"
"                    of the block. They are radially used - effectively\n"
"                    half the dimension of a side. So if one wanted to\n"
"                    compute a 5-slice projection on a 1mm3 volume,\n"
"                    then a RECT(0,0,2) would be appropriate, and \n"
"                    the program would report 6 voxels used in the mask\n"
"                    Any dimension less than a voxel will avoid\n"
"                    voxels in that direction.\n"
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
"               * mode   = mode\n"
"               * nzmode = non-zero mode\n"
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
"                          are in 3 sub-bricks: FWHMx, FWHMy, and FWHMz.\n"
"                          Places where an output is -1 are locations\n"
"                          where the FWHM value could not be computed\n"
"                          (e.g., outside the mask).\n"
"               * FWHMbar= Compute just the average of the 3 FWHM values\n"
"                          (normally would NOT do this with FWHM also).\n"
#if 0
"               * FWHMbar12 = Compute the average of the 3 FWHM values,\n"
"                             but using the '-2difMAD' method of 3dFWHMx.\n"
#endif
"               * perc:P0:P1:Pstep = \n"
"                          Compute percentiles between P0 and P1 with a \n"
"                          step of Pstep.\n"
"                          Default P1 is equal to P0 and default P2 = 1\n"
"               * rank   = rank of the voxel's intensity\n"
"               * frank  = rank / number of voxels in neighborhood\n"
"               * P2skew = Pearson's second skewness coefficient\n"
"                           3 * (mean - median) / stdev \n"
"               * ALL    = all of the above, in that order \n"
"                         (except for FWHMbar and perc).\n"
"               * mMP2s  = Exactly the same output as:\n"
"                          -stat median -stat MAD -stat P2skew\n"
"                          but it a little faster\n"
"               * mmMP2s  = Exactly the same output as:\n"
"                       -stat mean -stat median -stat MAD -stat P2skew\n"
"               * diffs   = Compute differences between central voxel\n"
"                           and all neighbors. Values output are the \n"
"                           average difference, followed by the min and max\n"
"                           differences.\n"
"               * list    = Just output the voxel values in the neighborhood\n"
"                           The order in which the neighbors are listed \n"
"                           depends on the neighborhood selected. Only\n"
"                           SPHERE results in a neighborhood list sorted by\n"
"                           the distance from the center.\n"
"                           Regardless of the neighborhood however, the first\n"
"                           value should always be that of the central voxel.\n"
"               * hist:MIN:MAX:N[:IGN] = Compute the histogram in the voxel's\n"
"                           neighborhood. You must specify the min, max, and \n"
"                           the number of bins in the histogram. You can also\n"
"                           ignore values outside the [min max] range by \n"
"                           setting IGN to 1. IGN = 0 by default.\n"
"                           The histograms are scaled by the number \n"
"                           of values that went into the histogram.\n"
"                           That would be the number of non-masked voxels\n"
"                           in the neighborhood if outliers are NOT\n"
"                           ignored (default).\n" 
"                       For histograms of labeled datasets, use 3dLocalHistog\n" 
"\n"
"               More than one '-stat' option can be used.\n"
"\n"
" -mask mset  = Read in dataset 'mset' and use the nonzero voxels\n"
"               therein as a mask.  Voxels NOT in the mask will\n"
"               not be used in the neighborhood of any voxel. Also,\n"
"               a voxel NOT in the mask will have its statistic(s)\n"
"               computed as zero (0) -- usually (cf. supra).\n"
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
" -label_ext LABEXT = Append '.LABEXT' to each sub-brick label \n"
"\n"
" -reduce_grid Rx [Ry Rz] = Compute output on a grid that is \n"
"                           reduced by a factor of Rx Ry Rz in\n"
"                           the X, Y, and Z directions of the \n"
"                           input dset. This option speeds up \n"
"                           computations at the expense of \n"
"                           resolution. You should only use it\n"
"                           when the nbhd is quite large with \n"
"                           respect to the input's resolution,\n"
"                           and the resultant stats are expected\n"
"                           to be smooth. \n"
"                           You can either set Rx, or Rx Ry and Rz.\n"
"                           If you only specify Rx the same value\n"
"                           is applied to Ry and Rz.\n"
"\n"
" -reduce_restore_grid Rx [Ry Rz] = Like reduce_grid, but also resample\n"
"                                   output back to input grid.\n"
" -reduce_max_vox MAX_VOX = Like -reduce_restore_grid, but automatically\n"
"                           set Rx Ry Rz so that the computation grid is\n"
"                           at a resolution of nbhd/MAX_VOX voxels.\n"
" -grid_rmode RESAM = Interpolant to use when resampling the output with\n"
"                     reduce_restore_grid option. The resampling method\n"        "                     string RESAM should come from the set \n"
"                     {'NN', 'Li', 'Cu', 'Bk'}.  These stand for\n"
"                     'Nearest Neighbor', 'Linear', 'Cubic'\n"
"                     and 'Blocky' interpolation, respectively.\n"
"                     Default is Linear\n"
" -quiet      = Stop the highly informative progress reports.\n"
" -verb       = a little more verbose.\n"
" -proceed_small_N = Do not crash if neighborhood is too small for \n"
"                    certain estimates.\n"
"\n"
"Author: RWCox - August 2005.  Instigator: ZSSaad.\n"
     ) ;
     PRINT_AFNI_OMP_USAGE("3dLocalstat",NULL) ;
     PRINT_COMPILE_DATE ;
   return;
}
/*-----------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   char allstats[] = { "mean; stdev; var; cvar; median; MAD; P2skew;"
                       "kurt; min; max; absmax; num; nznum; fnznum;"
                       "sum; rank; frank; fwhm; diffs; adiffs; mMP2s;"
                       "mmMP2s; list; hist; perc; fwhmbar; fwhmbar12;"
                       "mode;nzmode;ALL;" };
   THD_3dim_dataset *inset=NULL , *outset ;
   int ncode=0 , code[MAX_NCODE] , iarg=1 , ii ;
   float codeparams[MAX_NCODE][MAX_CODE_PARAMS+1], 
         redx[3]={0.0, 0.0, 0.0}, mxvx=0.0;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0 , automask=0 ;
   char *prefix="./localstat", *lab_ext=NULL ;
   int ntype=0 ; float na=0.0f,nb=0.0f,nc=0.0f ;
   int do_fwhm=0 , verb=1 , shootmyfoot = 0;
   int npv = -1;
   int ipv, restore_grid=0, resam_mode=resam_str2mode("Linear");
   int datum = MRI_float;
   
   /*---- for the clueless who wish to become clued-in ----*/

   if( argc == 1){ usage_3dLocalstat(1); exit(0); } /* Bob's help shortcut */

   /*---- official startup ---*/

#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
   enable_mcw_malloc() ;
#endif

   PRINT_VERSION("3dLocalstat"); mainENTRY("3dLocalstat main"); machdep();
   AFNI_logger("3dLocalstat",argc,argv); AUTHOR("Emperor Zhark");
   THD_check_AFNI_version("3dLocalstat") ;
   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   /* initialize codeparams */
   for (ii=0; ii<MAX_NCODE; ++ii) codeparams[ii][0] = -1.0;

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){
     if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0){
        usage_3dLocalstat(strlen(argv[iarg])>3 ? 2:1);
        exit(0);
     }
      
     if( strncmp(argv[iarg],"-q",2) == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-verb") == 0 ){
       verb = 2 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-proceed_small_N") == 0 ){
       shootmyfoot = 1 ; iarg++ ; continue ;
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

     if( strncasecmp(argv[iarg],"-float",6) == 0 ){
       datum = MRI_float ; iarg++ ; continue ;
     }
     if( strncasecmp(argv[iarg],"-short",6) == 0 ){
       datum = MRI_short ; iarg++ ; continue ;
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

     if( strcmp(argv[iarg],"-label_ext") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-label_ext'") ;
       lab_ext = argv[iarg];
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
     
     /* Some -stat options must be processed after input and neighborhoods
        are generated, so brief check here followed by processing below */
     if( strcmp(argv[iarg],"-stat") == 0 ){
       char *cpt, padname[128]={""};
       
       int iizz;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-stat'") ;

       cpt = argv[iarg] ; if( *cpt == '-' ) cpt++ ; 
       snprintf(padname,126,"%s;", cpt);
       if ( strncmp(cpt,"perc:",5) && strncmp(cpt,"hist:",5) &&
           !strcasestr(allstats, padname)) {
         ERROR_exit("-stat '%s' is an unknown statistic type",padname) ;
       }
       ncode = 1; /* Have something, set fully later */
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
     
     if( strcmp(argv[iarg],"-reduce_grid") == 0 || 
         strcmp(argv[iarg],"-reduce_restore_grid") == 0 ){
       if (strcmp(argv[iarg],"-reduce_restore_grid") == 0) restore_grid = 1;
       
       if( ++iarg >= argc ) 
         ERROR_exit( "Need 1 or 3 arguments after '-reduce_grid' "
                     "or '-reduce_restore_grid'") ;

       redx[0] = (float)strtod(argv[iarg],NULL); redx[2] = redx[1] = redx[0]; 
       if( iarg+2 < argc && *(argv[iarg+1]) != '-' && *(argv[iarg+2]) != '-') { 
         redx[1] = (float)strtod(argv[++iarg],NULL);
         redx[2] = (float)strtod(argv[++iarg],NULL); 
       }
       if (redx[0] < 1.0 || redx[1] < 1.0 || redx[2] < 1.0) {
         ERROR_exit("Bad values for -reduce_grid %f %f %f \n"
                    "All values must be >= 1.0\n",
                     redx[0], redx[1], redx[2]);
       }
       iarg++ ; continue ;
     }
     
     if( strcmp(argv[iarg],"-reduce_max_vox") == 0) {
      if( ++iarg >= argc ) 
         ERROR_exit( "Need 1 argument after '-reduce_max_vox' ") ;
      mxvx = (float)strtod(argv[iarg],NULL);
      restore_grid = 1;
      iarg++ ; continue ;
     }     
     
     if( strcmp(argv[iarg],"-grid_rmode") == 0) {
        if( ++iarg >= argc ) ERROR_exit("Need argument after '-grid_rmode'") ;
        if ( ( (resam_mode = resam_str2mode(argv[iarg]) ) < 0 ) ||
             (  resam_mode > LAST_RESAM_TYPE ) )
             ERROR_exit("invalid resample mode <%s>\n", argv[iarg] );
        iarg++ ; continue ;
     }
     

      ERROR_message("** 3dLocalstat: Illegal option: '%s'",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1) ;

   } /*--- end of loop over options ---*/

   if (argc < 2) {
      ERROR_message("Too few options. Try -help for details.\n");
      exit(1);
   }  
   
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
   
   if (mxvx) {
      redx[0] = redx[1] = redx[2] = 1.0;
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
       if (mxvx) {
         if (na/dx > mxvx) redx[0] = na / (mxvx * dx);
         if (na/dy > mxvx) redx[1] = na / (mxvx * dy);
         if (na/dz > mxvx) redx[2] = na / (mxvx * dz);
       }
     }
     break ;

     case NTYPE_RECT:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = 1.0f; na = -na; } else dx = fabsf(DSET_DX(inset));
       if( nb < 0.0f ){ dy = 1.0f; nb = -nb; } else dy = fabsf(DSET_DY(inset));
       if( nc < 0.0f ){ dz = 1.0f; nc = -nc; } else dz = fabsf(DSET_DZ(inset));
       nbhd = MCW_rectmask( dx,dy,dz , na,nb,nc ) ;
       if (mxvx) {
         if (na/dx > mxvx) redx[0] = na / (mxvx * dx);
         if (nb/dy > mxvx) redx[1] = nb / (mxvx * dy);
         if (nc/dz > mxvx) redx[2] = nc / (mxvx * dz);
       }
     }
     break ;

     case NTYPE_RHDD:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_rhddmask( dx,dy,dz , na ) ;
       if (mxvx) {
         if (na/dx > mxvx) redx[0] = na / (mxvx * dx);
         if (na/dy > mxvx) redx[1] = na / (mxvx * dy);
         if (na/dz > mxvx) redx[2] = na / (mxvx * dz);
       }
     }
     break ;

     case NTYPE_TOHD:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_tohdmask( dx,dy,dz , na ) ;
       if (mxvx) {
         if (na/dx > mxvx) redx[0] = na / (mxvx * dx); 
         if (nb/dy > mxvx) redx[1] = nb / (mxvx * dy); 
         if (nc/dz > mxvx) redx[2] = nc / (mxvx * dz);
       }
     }
     break ;
   }

   if( verb ) INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;
   if (verb > 1) {
      MCW_showmask (nbhd, NULL, NULL, stderr);
   }
   if( verb && redx[0] > 0.0) 
      INFO_message("Computing grid reduction: %f %f %f (%f)\n", 
                     redx[0], redx[1], redx[2], mxvx);
   
   /* Now figure out what is needed for holding output */
   ncode = 0; iarg = 1;
   while( iarg < argc ){
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
       else if( strcasecmp(cpt,"mode")  == 0 ) code[ncode++] = NSTAT_MODE  ;
       else if( strcasecmp(cpt,"nzmode") == 0) code[ncode++] = NSTAT_NZMODE  ;
       else if( strcasecmp(cpt,"P2skew")== 0 ) code[ncode++] = NSTAT_P2SKEW;
       else if( strcasecmp(cpt,"kurt")  == 0 ) code[ncode++] = NSTAT_KURT  ;
       else if( strcasecmp(cpt,"min")   == 0 ) code[ncode++] = NSTAT_MIN   ;
       else if( strcasecmp(cpt,"max")   == 0 ) code[ncode++] = NSTAT_MAX   ;
       else if( strcasecmp(cpt,"absmax")== 0 ) code[ncode++] = NSTAT_ABSMAX;
       else if( strcasecmp(cpt,"num")   == 0 ) code[ncode++] = NSTAT_NUM   ;
       else if( strcasecmp(cpt,"nznum") == 0 ) code[ncode++] = NSTAT_NZNUM ;
       else if( strcasecmp(cpt,"fnznum")== 0 ) code[ncode++] = NSTAT_FNZNUM;
       else if( strcasecmp(cpt,"sum")   == 0 ) code[ncode++] = NSTAT_SUM   ;
       else if( strcasecmp(cpt,"rank")  == 0 ) code[ncode++] = NSTAT_RANK  ;
       else if( strcasecmp(cpt,"frank") == 0 ) code[ncode++] = NSTAT_FRANK ;
       else if( strcasecmp(cpt,"fwhm")  == 0 ){code[ncode++] = NSTAT_FWHMx ;
                                               code[ncode++] = NSTAT_FWHMy ;
                                               code[ncode++] = NSTAT_FWHMz ;
                                               do_fwhm++                   ;}
       else if( strcasecmp(cpt,"diffs") == 0 ){code[ncode++] = NSTAT_diffs0;
                                               code[ncode++] = NSTAT_diffs1;
                                               code[ncode++] = NSTAT_diffs2;}
       else if( strcasecmp(cpt,"adiffs") == 0){code[ncode++] = NSTAT_adiffs0;
                                               code[ncode++] = NSTAT_adiffs1;
                                               code[ncode++] = NSTAT_adiffs2;}
       else if( strcasecmp(cpt,"mMP2s") == 0 ){code[ncode++] = NSTAT_mMP2s0;
                                               code[ncode++] = NSTAT_mMP2s1;
                                               code[ncode++] = NSTAT_mMP2s2;}
       else if( strcasecmp(cpt,"mmMP2s") == 0 ){code[ncode++] = NSTAT_mmMP2s0;
                                                code[ncode++] = NSTAT_mmMP2s1;
                                                code[ncode++] = NSTAT_mmMP2s2;
                                                code[ncode++] = NSTAT_mmMP2s3;}
       else if( strcasecmp(cpt,"list") == 0) {
          if (!nbhd) {
            ERROR_exit("Must specify neighborhood before list "
                       "stat is processed");
          }
          for (ipv=0; ipv<nbhd->num_pt; ++ipv)  code[ncode++] = NSTAT_LIST;
       } 
       else if( strncasecmp(cpt,"hist",4) == 0) {
          /* How many you say? */
         if (LS_decode_parameters(cpt, codeparams[ncode]) <= 0) {
            ERROR_exit("Need params with hist stat (e.g. hist:0:4.5:100)");
         }
         if (codeparams[ncode][0] != 3 && codeparams[ncode][0] != 4) {
            ERROR_exit("Need 3 or 4 params with hist stat");
         }
         if (codeparams[ncode][1]>=codeparams[ncode][2]) {
            ERROR_exit("Need min >= max!");
         }
         if (codeparams[ncode][3]< 3) {
            ERROR_exit("A histogram with just %d bins? I'm hurting here!"
                       , codeparams[ncode][3]);
         }
         if (codeparams[ncode][0] == 3) {
             codeparams[ncode][4] = 0; /* Don't ignore outliers */
             codeparams[ncode][0] = 4;
         }
         /*
         fprintf(stderr,
          "codeparams[%d][..]=[npar=%d min=%f max=%f N=%d ignore_outliers=%d]\n",
                  ncode, (int)codeparams[ncode][0],      codeparams[ncode][1], 
                              codeparams[ncode][2], (int)codeparams[ncode][3],
                         (int)codeparams[ncode][4]); */
         stp = (int)codeparams[ncode][3];
         for (ipv=0; ipv<stp; ++ipv) code[ncode++] = NSTAT_HIST;
       }else if( strncasecmp(cpt,"perc",4) == 0) {
         /* How many you say? */
         if (LS_decode_parameters(cpt, codeparams[ncode]) <= 0) {
            ERROR_exit("Need params with perc stat (e.g. perc:10:20:5");
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
       else if( strcasecmp(cpt,"fwhmbar"  )==0 ) code[ncode++] = NSTAT_FWHMbar;
       else if( strcasecmp(cpt,"fwhmbar12")==0 ) code[ncode++] = NSTAT_FWHMbar12;
       else if( strcasecmp(cpt,"ALL")   == 0 ){
         code[ncode++] = NSTAT_MEAN  ; code[ncode++] = NSTAT_SIGMA ;
         code[ncode++] = NSTAT_VAR   ; code[ncode++] = NSTAT_CVAR  ;
         code[ncode++] = NSTAT_MEDIAN; code[ncode++] = NSTAT_MAD   ;
         code[ncode++] = NSTAT_MIN   ; code[ncode++] = NSTAT_MAX   ;
         code[ncode++] = NSTAT_ABSMAX; code[ncode++] = NSTAT_NUM   ;
         code[ncode++] = NSTAT_SUM   ; code[ncode++] = NSTAT_MODE   ;
         code[ncode++] = NSTAT_FWHMx ; code[ncode++] = NSTAT_FWHMy ;
         code[ncode++] = NSTAT_FWHMz ; do_fwhm++ ;
         code[ncode++] = NSTAT_RANK  ; code[ncode++] = NSTAT_FRANK ; 
         code[ncode++] = NSTAT_P2SKEW; code[ncode++] = NSTAT_KURT  ;
         code[ncode++] = NSTAT_NZNUM ; code[ncode++] = NSTAT_FNZNUM;
       }
       else
         ERROR_exit("-stat '%s' is an unknown statistic type",argv[iarg]) ;
      }
      iarg++ ; continue ;
   }
   if( ncode <= 0 ) ERROR_exit("No '-stat' options given???");

    
   if( !shootmyfoot && do_fwhm && nbhd->num_pt < 19 )
     ERROR_exit("FWHM requires neighborhood of at least 19 voxels!");

   /*---- actually do some work for a change ----*/

   THD_localstat_verb(verb) ;
   THD_localstat_datum(datum);

   outset = THD_localstat(inset , mask , nbhd , ncode , code, codeparams, 
                          redx, restore_grid == 1 ? resam_mode : -1);
   if( outset == NULL ) ERROR_exit("Function THD_localstat() fails?!") ;

   DSET_unload(inset) ;
   /*---- save resulting dataset ----*/

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalstat" , argc,argv , outset ) ;

   { char *lcode[MAX_NCODE] , lll[1024] , *slcode, pcode[MAX_NCODE];
     int ipv = -1, ineighb = -1, ihist = -1;
     double W=0.0;
     lcode[NSTAT_MEAN]    = "MEAN" ;   lcode[NSTAT_SIGMA]      = "SIGMA"  ;
     lcode[NSTAT_CVAR]    = "CVAR" ;   lcode[NSTAT_MEDIAN]     = "MEDIAN" ;
     lcode[NSTAT_MAD]     = "MAD"  ;   lcode[NSTAT_MAX]        = "MAX"    ;
     lcode[NSTAT_MIN]     = "MIN"  ;   lcode[NSTAT_ABSMAX]     = "ABSMAX" ;
     lcode[NSTAT_VAR]     = "VAR"  ;   lcode[NSTAT_NUM]        = "NUM"    ;
     lcode[NSTAT_FWHMx]   = "FWHMx";   lcode[NSTAT_PERCENTILE] = "PERC";
     lcode[NSTAT_FWHMy]   = "FWHMy";   lcode[NSTAT_SUM]        = "SUM"    ;
     lcode[NSTAT_FWHMz]   = "FWHMz";   lcode[NSTAT_FWHMbar]    = "FWHMavg"; 
     lcode[NSTAT_RANK]    = "RANK" ;   lcode[NSTAT_FRANK]      = "FRANK";
     lcode[NSTAT_P2SKEW]  = "P2skew";  lcode[NSTAT_KURT]       = "KURT"; 
     lcode[NSTAT_mMP2s0]  = "MEDIAN";  lcode[NSTAT_mMP2s1]     = "MAD";
     lcode[NSTAT_mMP2s2]  = "P2skew";  lcode[NSTAT_mmMP2s0]    = "MEAN";
     lcode[NSTAT_mmMP2s1] = "MEDIAN";  lcode[NSTAT_mmMP2s2]    = "MAD";
     lcode[NSTAT_mmMP2s3] = "P2skew";  lcode[NSTAT_FWHMbar12]  = "FWHMbar12";
     lcode[NSTAT_NZNUM]   = "NZNUM" ;  lcode[NSTAT_FNZNUM]     = "FNZNUM" ;
     lcode[NSTAT_diffs0]  = "AvgDif";  lcode[NSTAT_diffs1]     = "MinDif";
                                       lcode[NSTAT_diffs2]     = "MaxDif"; 
     lcode[NSTAT_adiffs0] = "Avg|Dif|";lcode[NSTAT_adiffs1]    = "Min|Dif|";
                                       lcode[NSTAT_adiffs2]    = "Max|Dif|";
     lcode[NSTAT_LIST]    = "list";    lcode[NSTAT_HIST]       = "hist"; 
     lcode[NSTAT_MODE]    = "MODE";    lcode[NSTAT_NZMODE]     = "NZMODE";
     
     if( DSET_NVALS(inset) == 1 ){
       ii=0;
       while(ii < DSET_NVALS(outset)) {
         if (code[ii%ncode] == NSTAT_PERCENTILE) {
            ihist = -1;
            ineighb = -1;
            if (ipv < 0) ipv = ii%ncode;
            sprintf(pcode,"perc:%.2f", codeparams[ipv][1+ii%ncode-ipv]);
            slcode = pcode;
         } else if (code[ii%ncode] == NSTAT_LIST) {
            ipv = -1;
            ihist = -1;
            if (ineighb < 0) ineighb = 0;
            else ++ineighb;
            sprintf(pcode,"list:%04d", ineighb);
            slcode = pcode;
         } else if (code[ii%ncode] == NSTAT_HIST) {
            ipv = -1;
            ineighb = -1;
            if (ihist < 0) {
               ihist = 0;
               W = (codeparams[ii%ncode][2] - codeparams[ii%ncode][1])
                                                   / codeparams[ii%ncode][3];
            } else ++ihist;
            sprintf(pcode,"pdf:%.5f", W*ihist);
            slcode = pcode;
         } else {
            ipv = -1;
            ihist = -1;
            ineighb = -1;
            slcode = lcode[code[ii%ncode]];
         }
         /*fprintf(stderr,"CODE %d: %s\n", ii, slcode);*/
         if (lab_ext) {
            snprintf(lll,1010,"%s.%s",slcode,lab_ext) ;
         } else {
            snprintf(lll,1010,"%s",slcode) ;
         }
         EDIT_dset_items( outset ,
                            ADN_brick_label_one+ii , lll ,
                          ADN_none ) ;
         ++ii;
      }

     } else {
       for( ii=0 ; ii < DSET_NVALS(outset) ; ii++ ){
         if (code[ii%ncode] == NSTAT_PERCENTILE) {
            ihist = -1;
            ineighb = -1;
            if (ipv < 0) ipv = ii%ncode;
            sprintf(pcode,"perc:%.2f", codeparams[ipv][1+ii%ncode-ipv]);
            slcode = pcode;
         } else if (code[ii%ncode] == NSTAT_LIST) {
            ipv = -1;
            ihist = -1;
            if (ineighb < 0) ineighb = 0;
            else ++ineighb;
            sprintf(pcode,"list:%04d", ineighb);
            slcode = pcode;
         } else if (code[ii%ncode] == NSTAT_HIST) {
            ipv = -1;
            ineighb = -1;
            if (ihist < 0) {
               ihist = 0;
               W = (codeparams[ii%ncode][2] - codeparams[ii%ncode][1])
                                                   / codeparams[ii%ncode][3];
            } else ++ihist;
            sprintf(pcode,"pdf:%.5f", W*ihist);
            slcode = pcode;
         } else {
            ipv = -1;
            ihist = -1;
            ineighb = -1;
            slcode = lcode[code[ii%ncode]];
         }
         if (lab_ext) {
            snprintf(lll,1010,"%s[%d].%s",slcode,(ii/ncode),lab_ext) ;
         } else {
            snprintf(lll,1010,"%s[%d]",slcode,(ii/ncode)) ;
         }
         /* fprintf(stderr,"CODE sb%d: %s\n", ii, lll); */
         EDIT_dset_items( outset , ADN_brick_label_one+ii,lll, ADN_none ) ;
       }
     }
   }

   DSET_write( outset ) ;
   if( verb ) WROTE_DSET( outset ) ;
   exit(0) ;
}
