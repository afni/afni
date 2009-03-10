#include "mrilib.h"

#define MAX_NCODE 666

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *jnset=NULL , *outset , *wset=NULL;
   int ncode=0 , code[MAX_NCODE] , iarg=1 , ii ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   char *prefix="./localstat" ;
   int ntype=0 ; float na=0.0f,nb=0.0f,nc=0.0f ;
   double hist_pow=0.3333333 ; int hist_nbin=0 ;
   float hbot1=1.0f , htop1=-1.0f ; int hbot1_perc=0, htop1_perc=0 ;
   float hbot2=1.0f , htop2=-1.0f ; int hbot2_perc=0, htop2_perc=0 ;

   /*---- for the clueless who wish to become clued-in ----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dLocalBistat [options] dataset1 dataset2\n"
      "\n"
      "This program computes statistics between 2 datasets,\n"
      "at each voxel, based on a local neighborhood of that voxel.\n"
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
      "               * 'RHDD(r)' is a rhombic dodecahedron of 'radius' r.\n"
      "               * 'TOHD(r)' is a truncated octahedron of 'radius' r.\n"
      "\n"
      " -stat sss   = Compute the statistic named 'sss' on the values\n"
      "               extracted from the region around each voxel:\n"
      "               * pearson  = Pearson correlation coefficient\n"
      "               * spearman = Spearman correlation coefficient\n"
      "               * quadrant = Quadrant correlation coefficient\n"
      "               * mutinfo  = Mutual Information\n"
      "               * normuti  = Normalized Mutual Information\n"
      "               * jointent = Joint entropy\n"
      "               * hellinger= Hellinger metric\n"
      "               * crU      = Correlation ratio (Unsymmetric)\n"
      "               * crM      = Correlation ratio (symmetrized by Multiplication)\n"
      "               * crA      = Correlation ratio (symmetrized by Addition)\n"
      "               * num    = number of the values in the region:\n"
      "                          with the use of -mask or -automask,\n"
      "                          the size of the region around any given\n"
      "                          voxel will vary; this option lets you\n"
      "                          map that size.\n"
      "               * ncd    = Normalized Compression Distance (zlib)\n"
      "               * ALL    = all of the above, in that order\n"
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
      " -weight ws  = Use dataset 'ws' as a weight.  Only applies to 'pearson'.\n"
      "\n"
      " -prefix ppp = Use string 'ppp' as the prefix for the output dataset.\n"
      "               The output dataset is always stored as floats.\n"
      "\n"
      "ADVANCED OPTIONS\n"
      "----------------\n"
      " -histpow pp   = By default, the number of bins in the histogram used\n"
      "                 for calculating the Hellinger, Mutual Information, NCD,\n"
      "                 and Correlation Ratio statistics is n^(1/3), where n\n"
      "                 is the number of data points in the -nbhd mask.  You\n"
      "                 can change that exponent to 'pp' with this option.\n"
      " -histbin nn   = Or you can just set the number of bins directly to 'nn'.\n"
      " -hclip1 a b   = Clip dataset1 to lie between values 'a' and 'b'.  If 'a'\n"
      "                 and 'b' end in '%%', then these values are percentage\n"
      "                 points on the cumulative histogram.\n"
      " -hclip2 a b   = Similar to '-hclip1' for dataset2.\n"
      "\n"
      "-----------------------------\n"
      "Author: RWCox - October 2006.\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dLocalstat"); mainENTRY("3dLocalstat main"); machdep();

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-hclip1") == 0 ){
       char *cpt1, *cpt2 ;
       if( ++iarg >= argc-1 ) ERROR_exit("need 2 arguments after -hclip1") ;
       hbot1 = (float)strtod(argv[iarg],&cpt1) ; iarg++ ;
       htop1 = (float)strtod(argv[iarg],&cpt2) ;
       if( hbot1 >= htop1 ) ERROR_exit("illegal values after -hclip1") ;
       if( *cpt1 == '%' ){
         hbot1_perc = 1 ; hbot1 = (int)rint((double)hbot1) ;
         if( hbot1 < 0 || hbot1 > 99 ) ERROR_exit("illegal bot percentage after -hclip1") ;
       }
       if( *cpt2 == '%' ){
         htop1_perc = 1 ; htop1 = (int)rint((double)htop1) ;
         if( htop1 < 1 || htop1 > 100 ) ERROR_exit("illegal top percentage after -hclip1") ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-hclip2") == 0 ){
       char *cpt1, *cpt2 ;
       if( ++iarg >= argc-1 ) ERROR_exit("need 2 arguments after -hclip2") ;
       hbot2 = (float)strtod(argv[iarg],&cpt1) ; iarg++ ;
       htop2 = (float)strtod(argv[iarg],&cpt2) ;
       if( hbot2 >= htop2 ) ERROR_exit("illegal values after -hclip2") ;
       if( *cpt1 == '%' ){
         hbot2_perc = 1 ; hbot2 = (int)rint((double)hbot2) ;
         if( hbot2 < 0 || hbot2 > 99 ) ERROR_exit("illegal bot percentage after -hclip2") ;
       }
       if( *cpt2 == '%' ){
         htop2_perc = 1 ; htop2 = (int)rint((double)htop2) ;
         if( htop2 < 1 || htop2 > 100 ) ERROR_exit("illegal top percentage after -hclip2") ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-histpow") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       hist_pow = strtod(argv[iarg],NULL) ;
       if( hist_pow <= 0.0 || hist_pow > 0.5 ){
         WARNING_message("Illegal value after -histpow"); hist_pow = 0.33333;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-histbin") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s'!",argv[iarg-1]) ;
       hist_nbin = (int)strtod(argv[iarg],NULL) ;
       if( hist_nbin <= 1 ) WARNING_message("Illegal value after -histbin");
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Bad name after '-prefix'") ;
       iarg++ ; continue ;
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
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 2 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-weight") == 0 ){  /* 14 Aug 2007 */
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-weight'") ;
       if( wset != NULL ) ERROR_exit("Can't have two weight inputs") ;
       wset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(wset,argv[iarg]) ;
       DSET_load(wset) ; CHECK_LOAD_ERROR(wset) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-stat") == 0 ){
       char *cpt ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-stat'") ;
       cpt = argv[iarg] ; if( *cpt == '-' ) cpt++ ;
            if( strcasecmp(cpt,"pearson")  == 0 ) code[ncode++] = NBISTAT_PEARSON_CORR ;
       else if( strcasecmp(cpt,"spearman") == 0 ) code[ncode++] = NBISTAT_SPEARMAN_CORR;
       else if( strcasecmp(cpt,"quadrant") == 0 ) code[ncode++] = NBISTAT_QUADRANT_CORR;
       else if( strcasecmp(cpt,"mutinfo")  == 0 ) code[ncode++] = NBISTAT_MUTUAL_INFO  ;
       else if( strcasecmp(cpt,"normuti")  == 0 ) code[ncode++] = NBISTAT_NORMUT_INFO  ;
       else if( strcasecmp(cpt,"jointent") == 0 ) code[ncode++] = NBISTAT_JOINT_ENTROPY;
       else if( strcasecmp(cpt,"hellinger")== 0 ) code[ncode++] = NBISTAT_HELLINGER    ;
       else if( strcasecmp(cpt,"crU")      == 0 ) code[ncode++] = NBISTAT_CORR_RATIO_U ;
       else if( strcasecmp(cpt,"crM")      == 0 ) code[ncode++] = NBISTAT_CORR_RATIO_M ;
       else if( strcasecmp(cpt,"crA")      == 0 ) code[ncode++] = NBISTAT_CORR_RATIO_A ;
       else if( strcasecmp(cpt,"num")      == 0 ) code[ncode++] = NBISTAT_NUM          ;
       else if( strcasecmp(cpt,"ncd")      == 0 ) code[ncode++] = NBISTAT_NCD          ;
       else if( strcasecmp(cpt,"ALL")      == 0 ){
          code[ncode++] = NBISTAT_PEARSON_CORR ; code[ncode++] = NBISTAT_SPEARMAN_CORR;
          code[ncode++] = NBISTAT_QUADRANT_CORR; code[ncode++] = NBISTAT_MUTUAL_INFO  ;
          code[ncode++] = NBISTAT_NORMUT_INFO  ; code[ncode++] = NBISTAT_JOINT_ENTROPY;
          code[ncode++] = NBISTAT_HELLINGER    ; code[ncode++] = NBISTAT_CORR_RATIO_U ;
          code[ncode++] = NBISTAT_CORR_RATIO_M ; code[ncode++] = NBISTAT_CORR_RATIO_A ;
          code[ncode++] = NBISTAT_NUM          ; code[ncode++] = NBISTAT_NCD          ;
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
       } else if( strncasecmp(cpt,"RHDD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a RHDD of radius 0") ;
         ntype = NTYPE_RHDD ;
       } else if( strncasecmp(cpt,"TOHD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a TOHD of radius 0") ;
         ntype = NTYPE_TOHD ;
       } else if( strncasecmp(cpt,"RECT",4) == 0 ){
         sscanf( cpt+5 , "%f,%f,%f" , &na,&nb,&nc ) ;
         if( na == 0.0f && nb == 0.0f && nc == 0.0f )
           ERROR_exit("'RECT(0,0,0)' is not a legal neighborhood") ;
         ntype = NTYPE_RECT ;
       } else {
           ERROR_exit("Unknown -nbhd shape: '%s'",cpt) ;
       }
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*---- check for stupid user inputs ----*/

   if( ncode <= 0 ) ERROR_exit("No '-stat' options given?") ;
   if( ntype <= 0 ) ERROR_exit("No '-nbhd' option given?!") ;

   /*---- deal with input datasets ----*/

   if( iarg > argc-1 ) ERROR_exit("No first input dataset on command line?") ;
   inset = THD_open_dataset( argv[iarg] ) ; CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   iarg++ ;
   if( iarg > argc-1 ) ERROR_exit("No second input dataset on command line?") ;
   jnset = THD_open_dataset( argv[iarg] ) ; CHECK_OPEN_ERROR(jnset,argv[iarg]) ;
   if( jnset == NULL  ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
   if( DSET_NVOX(jnset)  != DSET_NVOX(inset) )
     ERROR_exit("Input datasets have different numbers of voxels!?");
   if( DSET_NVALS(jnset) != DSET_NVALS(inset)  )
     ERROR_exit("Input datasets have different numbers of sub-bricks!?");

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   DSET_load(jnset) ; CHECK_LOAD_ERROR(jnset) ;

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     int mmm , nvox ; byte *jask ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset #1?") ;
     jask = THD_automask( jnset ) ;
     if( jask == NULL )
       ERROR_message("Can't create -automask from input dataset #2?") ;
     nvox = DSET_NVOX(inset) ;
     for( ii=0 ; ii < nvox ; ii++ ) mask[ii] = (mask[ii] || jask[ii]) ;
     free(jask) ;
     mmm = THD_countmask( nvox , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 11 ) ERROR_exit("Automask is too small to process") ;
   }

   if( wset != NULL ){ /* 14 Aug 2007 */
     MRI_IMAGE *wim ; float *war ;
     if( DSET_NVOX(wset) != DSET_NVOX(inset) )
       ERROR_exit("-weight dataset mismatch with input datasets!") ;
     wim = mri_scale_to_float( DSET_BRICK_FACTOR(wset,0), DSET_BRICK(wset,0) );
     war = MRI_FLOAT_PTR(wim) ;
     DSET_delete(wset) ;
     if( mask != NULL ){
       int nvox = DSET_NVOX(inset) ;
       for( ii=0 ; ii < nvox ; ii++ ) if( !mask[ii] ) war[ii] = 0.0f ;
     }
     mri_bistat_setweight( wim ) ; mri_free(wim) ;
   }

   /*---- create neighborhood -----*/

   switch( ntype ){
     default:
       ERROR_exit("WTF?  ntype=%d",ntype) ;

     case NTYPE_SPHERE:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_spheremask( dx,dy,dz , na ) ;
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

     case NTYPE_RECT:{
       float dx , dy , dz ;
       if( na < 0.0f ){ dx = 1.0f; na = -na; } else dx = fabsf(DSET_DX(inset));
       if( nb < 0.0f ){ dy = 1.0f; nb = -nb; } else dy = fabsf(DSET_DY(inset));
       if( nc < 0.0f ){ dz = 1.0f; nc = -nc; } else dz = fabsf(DSET_DZ(inset));
       nbhd = MCW_rectmask( dx,dy,dz , na,nb,nc ) ;
     }
     break ;
   }

   INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;

   if( hist_nbin <= 1 ) hist_nbin = (int)pow((double)nbhd->num_pt,hist_pow) ;
   if( hist_nbin <= 1 ) hist_nbin = 2 ;
   INFO_message("2D histogram size = %d",hist_nbin) ;
   set_2Dhist_hbin( hist_nbin ) ;

   if( hbot1_perc || htop1_perc ){
     MRI_IMAGE *fim ; float perc[101] ;
     fim = THD_median_brick(inset) ;
     mri_percents(fim,100,perc) ; mri_free(fim) ;
     if( hbot1_perc ) hbot1 = perc[(int)hbot1] ;
     if( htop1_perc ) htop1 = perc[(int)htop1] ;
     INFO_message("Clipping dataset '%s' between %g and %g" ,
                  DSET_BRIKNAME(inset),hbot1,htop1 ) ;
   }
   if( hbot2_perc || htop2_perc ){
     MRI_IMAGE *fim ; float perc[101] ;
     fim = THD_median_brick(jnset) ;
     mri_percents(fim,100,perc) ; mri_free(fim) ;
     if( hbot2_perc ) hbot2 = perc[(int)hbot2] ;
     if( htop2_perc ) htop2 = perc[(int)htop2] ;
     INFO_message("Clipping dataset '%s' between %g and %g" ,
                  DSET_BRIKNAME(jnset),hbot2,htop2 ) ;
   }
   mri_nbistat_setclip( hbot1,htop1 , hbot2,htop2 ) ;

   /*---- actually do some work for a change ----*/

   THD_localbistat_verb(1) ;
   outset = THD_localbistat( inset,jnset , mask , nbhd , ncode , code ) ;

   DSET_unload(inset) ; DSET_unload(jnset) ;

   if( outset == NULL ) ERROR_exit("Function THD_localbistat() fails?!") ;

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalBistat" , argc,argv , outset ) ;

#if 1
   { char *lcode[66] , lll[66] ;
     lcode[0] = "Rank cor" ;
     lcode[1] = "Quad cor" ;
     lcode[2] = "Pear cor" ;
     lcode[3] = "MI" ;
     lcode[4] = "NMI" ;
     lcode[5] = "Jnt Entropy" ;
     lcode[6] = "Hlngr metric" ;
     lcode[7] = "CorRat Sym*" ;
     lcode[8] = "CorRat Sym+" ;
     lcode[9] = "CorRatUnsym" ;
     lcode[10]= "Number" ;
     lcode[11]= "NCD" ;
     if( DSET_NVALS(inset) == 1 ){
       for( ii=0 ; ii < DSET_NVALS(outset) ; ii++ )
         EDIT_dset_items( outset ,
                            ADN_brick_label_one+ii, lcode[code[ii%ncode]-NBISTAT_BASE],
                          ADN_none ) ;
     } else {
       for( ii=0 ; ii < DSET_NVALS(outset) ; ii++ ){
         sprintf(lll,"%s[%d]",lcode[code[ii%ncode]-NBISTAT_BASE],(ii/ncode)) ;
         EDIT_dset_items( outset , ADN_brick_label_one+ii,lll, ADN_none ) ;
       }
     }
   }
#endif

   DSET_write( outset ) ;
   WROTE_DSET( outset ) ;
   exit(0) ;
}
