#include "mrilib.h"

#define MAX_NCODE 666

#define NTYPE_SPHERE 1
#define NTYPE_RECT   2

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset ;
   int ncode=0 , code[MAX_NCODE] , iarg=1 , ii ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   char *prefix="./localstat" ;
   int ntype=0 ; float na=0.0f,nb=0.0f,nc=0.0f ;

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
      "               * If no '-nbhd' option is given, the region extracted\n"
      "                 will just be the voxel and its 6 nearest neighbors.\n"
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
      "\n"
      " -prefix ppp = Use string 'ppp' as the prefix for the output dataset.\n"
      "               The output dataset is always stored as floats.\n"
      "\n"
      "Author: RWCox - August 2005.  Instigator: ZSSaad.\n"
     ) ;
     exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dLocalstat"); mainENTRY("3dLocalstat main"); machdep();

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] ) ;
       if( inset == NULL  ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       if( mset == NULL ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       DSET_load(mset) ;
       if( !DSET_LOADED(mset) ) ERROR_exit("Can't load dataset '%s'",argv[iarg]) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 1.0f,-1.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
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
       else if( strcasecmp(cpt,"ALL")   == 0 ){
         code[ncode++] = NSTAT_MEAN  ; code[ncode++] = NSTAT_SIGMA ;
         code[ncode++] = NSTAT_VAR   ; code[ncode++] = NSTAT_CVAR  ;
         code[ncode++] = NSTAT_MEDIAN; code[ncode++] = NSTAT_MAD   ;
         code[ncode++] = NSTAT_MIN   ; code[ncode++] = NSTAT_MAX   ;
         code[ncode++] = NSTAT_ABSMAX; code[ncode++] = NSTAT_NUM   ;
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
       } else {
           ERROR_exit("Unknown -nbhd shape: '%s'",cpt) ;
       }
       iarg++ ; continue ;
     }

     ERROR_exit("Uknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*---- check for stupid user inputs ----*/

   if( ncode <= 0 ) ERROR_exit("No '-stat' options given?") ;

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     if( inset == NULL  ) ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
   }

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) )
     ERROR_exit("Can't load input dataset '%s' from disk") ;

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
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 2 ) ERROR_exit("Automask is too small to process") ;
   }

   /*---- create neighborhood -----*/

   if( ntype <= 0 ){         /* default neighborhood */
     ntype = NTYPE_SPHERE ; na = -1.01f ;
     INFO_message("Using default neighborhood = self + 6 neighbors") ;
   }

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

   /*---- actually do some work for a change ----*/

   THD_localstat_verb(1) ;
   outset = THD_localstat( inset , mask , nbhd , ncode , code ) ;

   DSET_unload(inset) ;

   if( outset == NULL ) ERROR_exit("Function THD_localstat() fails?!") ;

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalstat" , argc,argv , outset ) ;

   { char *lcode[66] , lll[66] ;
     lcode[NSTAT_MEAN]   = "MEAN" ; lcode[NSTAT_SIGMA]  = "SIGMA"  ;
     lcode[NSTAT_CVAR]   = "CVAR" ; lcode[NSTAT_MEDIAN] = "MEDIAN" ;
     lcode[NSTAT_MAD]    = "MAD"  ; lcode[NSTAT_MAX]    = "MAX"    ;
     lcode[NSTAT_MIN]    = "MIN"  ; lcode[NSTAT_ABSMAX] = "ABSMAX" ;
     lcode[NSTAT_VAR]    = "VAR"  ; lcode[NSTAT_NUM]    = "NUM"    ;
     if( DSET_NVALS(inset) == 1 ){
       for( ii=0 ; ii < DSET_NVALS(outset) ; ii++ )
         EDIT_dset_items( outset ,
                            ADN_brick_label_one+ii , lcode[code[ii%ncode]] ,
                          ADN_none ) ;
     } else {
       for( ii=0 ; ii < DSET_NVALS(outset) ; ii++ ){
         sprintf(lll,"%s[%d]",lcode[code[ii%ncode]],(ii/ncode)) ;
         EDIT_dset_items( outset , ADN_brick_label_one+ii,lll, ADN_none ) ;
       }
     }
   }

   DSET_write( outset ) ;
   WROTE_DSET( outset ) ;
   exit(0) ;
}
