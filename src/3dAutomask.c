#include "mrilib.h"

#undef ALLOW_FILLIN  /* 28 May 2002 */


int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset , *mset=NULL, *masked_dset ;
   char *amprefix = "automask" ;
   char *prefix = NULL;
   byte *mask ;
   int iarg=1 , fillin=0 , nmask,nfill , dilate=0 , dd  , erode = 0;
   int dilate_flag = 0, erode_flag = 0;
   float SIhh=0.0 ;        /* 06 Mar 2003 */
   int   SIax=0 , SIbot=0,SItop=0 ;
   int   verb=1 ;
   float clfrac=0.5 ;      /* 20 Mar 2006 */
   int peels=1, nbhrs=17 ; /* 24 Oct 2006 */
   byte NN = 2;             /* edge neighbors 23 Dec 2019 DRG */
   byte new_NN = 0;        /* flag user settings for neighbors and NNx*/
   int new_nbhrs = 0;
   int max_nbhrs = 18;     /* maximum neighbors for NN2 */
   int apply_mask = 0;     /* 17 Nov 2009 */
   char *apply_prefix = NULL, *depthprefix=NULL;
   short *depth=NULL, dodepth=0;      /* 02 March 2010 ZSS */ 


   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dAutomask [options] dataset\n"
 "Input dataset is EPI 3D+time, or a skull-stripped anatomical.\n"
 "Output dataset is a brain-only mask dataset.\n"
 "\n"
 "This program by itself does NOT do 'skull-stripping'.  Use\n"
 "program 3dSkullStrip for that purpose!\n"
 "\n"
 "Method:\n"
 " + Uses 3dClipLevel algorithm to find clipping level.\n"
 " + Keeps only the largest connected component of the\n"
 "   supra-threshold voxels, after an erosion/dilation step.\n"
 " + Writes result as a 'fim' type of functional dataset,\n"
 "   which will be 1 inside the mask and 0 outside the mask.\n"
 "\n"
 "Options:\n"
 "--------\n"
 "  -prefix ppp = Write mask into dataset with prefix 'ppp'.\n"
 "                 [Default == 'automask']\n"
 "\n"
 "  -apply_prefix ppp = Apply mask to input dataset and save\n"
 "                masked dataset. If an apply_prefix is given\n"
 "                and not the usual prefix, the only output\n"
 "                will be the applied dataset\n" 
 "\n"
 "  -clfrac cc  = Set the 'clip level fraction' to 'cc', which\n"
 "                 must be a number between 0.1 and 0.9.\n"
 "                 A small 'cc' means to make the initial threshold\n"
 "                 for clipping (a la 3dClipLevel) smaller, which\n"
 "                 will tend to make the mask larger.  [default=0.5]\n"
 "\n"
 "  -nograd     = The program uses a 'gradual' clip level by default.\n"
 "                 To use a fixed clip level, use '-nograd'.\n"
 "                 [Change to gradual clip level made 24 Oct 2006.]\n"
 "\n"
 "  -peels pp   = Peel (erode) the mask 'pp' times, \n"
"                  then unpeel (dilate). Using NN2 neighborhoods,\n"
 "                 clips off protuberances less than 2*pp voxels\n"
 "                 thick. Turn off by setting to 0. [Default == 1]\n"
 "\n"
 "  -NN1 -NN2 -NN3 = Erode and dilate using different neighbor definitions\n"
 "                 NN1=faces, NN2=edges, NN3= corners [Default=NN2]\n"
 "                 Applies to erode and dilate options, if present.\n"    
 "                 Note the default peeling processes still use NN2\n"
 "                 unless the peels are set to 0\n"
 "\n" 
 "  -nbhrs nn   = Define the number of neighbors needed for a voxel\n"
 "                 NOT to be eroded.  The 18 nearest neighbors in\n"
 "                 the 3D lattice are used, so 'nn' should be between\n"
 "                 6 and 26. [Default == 17]\n"
 "\n"
 "  -q          = Don't write progress messages (i.e., be quiet).\n"
 "\n"
 "  -eclip      = After creating the mask, remove exterior\n"
 "                 voxels below the clip threshold.\n"
 "\n"
 "  -dilate nd  = Dilate the mask outwards 'nd' times.\n"
 "\n"
 "  -erode ne   = Erode the mask inwards 'ne' times.\n"
#ifdef ALLOW_FILLIN
 "\n"
 "  -fillin nnn = Fill in holes inside the mask of width up\n"
 "                 to 'nnn' voxels. [Default == 0 == no fillin]\n"
#endif
 "\n"
 "  -SI hh      = After creating the mask, find the most superior\n"
 "                 voxel, then zero out everything more than 'hh'\n"
 "                 millimeters inferior to that.  hh=130 seems to\n"
 "                 be decent (i.e., for Homo Sapiens brains).\n"
 "\n"
 "  -depth DEP  = Produce a dataset (DEP) that shows how many peel \n"
 "                operations it takes to get to a voxel in the mask.\n"
 "                The higher the number, the deeper a voxel is located \n"
 "                in the mask. Note this uses the NN1,2,3 neighborhoods\n"
 "                above with a default of 2 for edge-sharing neighbors\n"
#ifdef ALLOW_FILLIN
"          None of -peels, -dilate, -fillin, or -erode affect this option.\n" 
#else
"          None of -peels, -dilate, or -erode affect this option.\n" 
#endif
            ) ;

      printf(
       "--------------------------------------------------------------------\n"
       "How to make an edge-of-brain mask from an anatomical volume:\n"
       "* 3dSkullStrip to create a brain-only dataset; say, Astrip+orig\n"
       "* 3dAutomask -prefix Amask Astrip+orig\n"
       "* Create a mask of edge-only voxels via\n"
       "   3dcalc -a Amask+orig -b a+i -c a-i -d a+j -e a-j -f a+k -g a-k \\\n"
       "          -expr 'ispositive(a)*amongst(0,b,c,d,e,f,g)' -prefix Aedge\n"
       "  which will be 1 at all voxels in the brain mask that have a\n"
       "  nearest neighbor that is NOT in the brain mask.\n"
       "* cf. '3dcalc -help' DIFFERENTIAL SUBSCRIPTS for information\n"
       "  on the 'a+i' et cetera inputs used above.\n"
       "* In regions where the brain mask is 'stair-stepping', then the\n"
       "  voxels buried inside the corner of the steps probably won't\n"
       "  show up in this edge mask:\n"
       "     ...00000000...\n"
       "     ...aaa00000...\n"
       "     ...bbbaa000...\n"
       "     ...bbbbbaa0...\n"
       "  Only the 'a' voxels are in this edge mask, and the 'b' voxels\n"
       "  down in the corners won't show up, because they only touch a\n"
       "  0 voxel on a corner, not face-on.  Depending on your use for\n"
       "  the edge mask, this effect may or may not be a problem.\n"
       "--------------------------------------------------------------------\n"
      ) ;

      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dAutomask main"); machdep(); AFNI_logger("3dAutomask",argc,argv);
   PRINT_VERSION("3dAutomask") ; AUTHOR("Emperor Zhark") ;

   /*-- options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-peel",5) == 0 ){           /* 24 Oct 2006 */
        peels = (int)strtod( argv[++iarg] , NULL ) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-NN1") == 0 ){           /* 23 Dec 2019 */
        new_NN = 1 ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-NN2") == 0 ){
        new_NN = 2 ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-NN3") == 0 ){
        new_NN = 3 ;
        iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-nbhr",5) == 0 ){           /* 24 Oct 2006 */
        nbhrs = (int)strtod( argv[++iarg] , NULL ) ;
        iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-nograd",5) == 0 ){
        THD_automask_set_gradualize(0) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-clfrac") == 0 || strcmp(argv[iarg],"-mfrac") == 0 ){    /* 20 Mar 2006 */
        clfrac = strtod( argv[++iarg] , NULL ) ;
        if( clfrac < 0.1f || clfrac > 0.9f )
          ERROR_exit("-clfrac value %f is illegal!",clfrac) ;
        THD_automask_set_clipfrac(clfrac) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-SI") == 0 ){        /* 06 Mar 2003 */
        SIhh = strtod( argv[++iarg] , NULL ) ;
        if( SIhh <= 0.0 )
          ERROR_exit("-SI value %f is illegal!\n",SIhh) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-eclip") == 0 ){     /* 28 Oct 2003 */
        THD_automask_extclip(1) ;
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-depth") == 0 ){
         depthprefix = argv[++iarg] ;
         if( !THD_filename_ok(depthprefix) )
           ERROR_exit("-depth %s is illegal!\n",depthprefix) ;
         dodepth = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-q") == 0 ){     /* 28 Oct 2003 */
        verb = 0 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) )
           ERROR_exit("-prefix %s is illegal!\n",prefix) ;
         iarg++ ; continue ;
      }

#ifdef ALLOW_FILLIN
      if( strcmp(argv[iarg],"-fillin") == 0 ){
         fillin = strtol( argv[++iarg] , NULL , 10 ) ;
         if( fillin <  0 )
           ERROR_exit("-fillin %s is illegal!\n",argv[iarg]) ;
         else if( fillin > 0 )
           fillin = (fillin+2) / 2 ;
         iarg++ ; continue ;
      }
#endif

      if( strcmp(argv[iarg],"-dilate") == 0 ){
         dilate = strtol( argv[++iarg] , NULL , 10 ) ;
         dilate_flag = 1;
         if( dilate < 0 )
           ERROR_exit("-dilate %s is illegal!\n",argv[iarg]);
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-erode") == 0 ){
         erode = strtol( argv[++iarg] , NULL , 10 ) ;
         erode_flag = 1;
         if( erode < 0 )
           ERROR_exit("-erode %s is illegal!\n",argv[iarg]);
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-apply_prefix") == 0 ){
         apply_prefix = argv[++iarg] ;
         if( !THD_filename_ok(apply_prefix) )
           ERROR_exit("-apply_prefix %s is illegal!\n",apply_prefix) ;
         apply_mask = 1;
         iarg++ ; continue ;
      }

      ERROR_exit("ILLEGAL option: %s\n",argv[iarg]) ;
   }

   // nearest neighbor mode changes override neighbor choices
   if(new_NN)
     NN = new_NN;
 
   switch(NN){
       case(1):
          max_nbhrs = 6;
          break;
       case(3):
          max_nbhrs = 26;
          break; 

       case(2):
       default:
          max_nbhrs = 18;
   }
   
   if(new_NN) /* keep historic default of 17 for NN2
                 unless changed via NNx or nbhrs options */
      nbhrs = max_nbhrs;

   /*  check for valid number of neighbors */
   if(new_nbhrs){
      if((new_nbhrs< max_nbhrs) && (new_nbhrs>0))  
         nbhrs = new_nbhrs;
      else
         ERROR_exit("-nbhrs %d is illegal (choose between 1 and %d)!\n",
           new_nbhrs, max_nbhrs);
   }

   /* set default number of peels (erosions/dilations combo, neighbors) */
   THD_automask_set_peelcounts(peels,nbhrs) ;

   if((dilate_flag+erode_flag)>1)
     WARNING_message("Combining dilate,erode options is like peels option");

   /*-- read data --*/

   dset = THD_open_dataset(argv[iarg]); CHECK_OPEN_ERROR(dset,argv[iarg]);
   if( DSET_BRICK_TYPE(dset,0) != MRI_short &&
       DSET_BRICK_TYPE(dset,0) != MRI_byte  &&
       DSET_BRICK_TYPE(dset,0) != MRI_float   ){
      ERROR_exit("Illegal dataset datum type\n") ;
   }
   if( verb ) INFO_message("Loading dataset %s\n",argv[iarg]) ;
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   /*** do all the real work now ***/

   if( verb ) INFO_message("Forming automask\n") ;
   if( verb ) THD_automask_verbose(1) ;
   mask = THD_automask( dset ) ;
   if( mask == NULL )
     ERROR_exit("Mask creation fails for unknown reasons!\n");

   if (dodepth) {       /* ZSS March 02 2010 */
      if (!(depth = THD_mask_depth( DSET_NX(dset), 
                                    DSET_NY(dset),  
                                    DSET_NZ(dset), mask, 1, NULL, NN))) {
         ERROR_exit("Failed to get depth vector!\n");
      }
   }

   /* 30 Aug 2002 (modified 05 Mar 2003 to do fillin, etc, after dilation) */
   if(dilate||erode) {
     int ii,nx,ny,nz , nmm ;

     nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nz = DSET_NZ(dset) ;
     nmm = 1 ;
     ii  = rint(0.032*nx) ; nmm = MAX(nmm,ii) ;
     ii  = rint(0.032*ny) ; nmm = MAX(nmm,ii) ;
     ii  = rint(0.032*nz) ; nmm = MAX(nmm,ii) ;

     if( verb && dilate) INFO_message("Dilating automask\n") ;
     for( dd=0 ; dd < dilate ; dd++ ){
       THD_mask_dilate           ( nx,ny,nz , mask, 3, NN ) ;
       THD_mask_fillin_completely( nx,ny,nz , mask, nmm ) ;
     }

     /* 3 May 2006 - drg- eroding option added */
     if( verb && erode) INFO_message("Eroding automask\n") ;
     for( dd=0 ; dd < erode ; dd++ ){
       THD_mask_erode           ( nx,ny,nz , mask, 0, NN) ;
       THD_mask_fillin_completely( nx,ny,nz , mask, nmm ) ;
     }

     if(dilate||erode) {
       nmm = nx*ny*nz ;
       for( ii=0 ; ii < nmm ; ii++ ) mask[ii] = !mask[ii] ;
       THD_mask_clust( nx,ny,nz, mask ) ;
       for( ii=0 ; ii < nmm ; ii++ ) mask[ii] = !mask[ii] ;
     }
   }

   /* 18 Apr 2002: print voxel count */

   nmask = THD_countmask( DSET_NVOX(dset) , mask ) ;
   if( verb ) INFO_message("%d voxels in the mask [out of %d: %.2f%%]\n",
                 nmask,DSET_NVOX(dset), (100.0*nmask)/DSET_NVOX(dset) ) ;
   if( nmask == 0 )
      ERROR_exit("No voxels? Quitting without saving mask\n");

   /* 18 Apr 2002: maybe fill in voxels */

#ifdef ALLOW_FILLIN
   if( fillin > 0 ){
     nfill = THD_mask_fillin_completely(
                 DSET_NX(dset),DSET_NY(dset),DSET_NZ(dset), mask, fillin ) ;
     if( verb ) INFO_message("%d voxels filled in; %d voxels total\n",
                             nfill,nfill+nmask ) ;
   }
#endif

   /** 04 Jun 2002: print cut plane report **/

   { int nx=DSET_NX(dset), ny=DSET_NY(dset), nz=DSET_NZ(dset), nxy=nx*ny ;
     int ii,jj,kk ;

#if 0
     { int xm=-1,xp=-1,ym=-1,yp=-1,zm=-1,zp=-1 ;
       THD_autobbox( dset , &xm,&xp , &ym,&yp , &zm,&zp, NULL ) ;
       INFO_message("Auto bbox: x=%d..%d  y=%d..%d  z=%d..%d\n",
                     xm,xp,ym,yp,zm,zp ) ;
     }
#endif

     for( ii=0 ; ii < nx ; ii++ )
       for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP5 ;
     CP5: if( verb )
           INFO_message("first %3d x-planes are zero [from %c]\n",
                        ii,ORIENT_tinystr[dset->daxes->xxorient][0]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->xxorient][0] == 'S' ){
       SIax = 1 ; SIbot = ii + (int)(SIhh/fabs(DSET_DX(dset))+0.5) ; SItop = nx-1 ;
     }

     for( ii=nx-1 ; ii >= 0 ; ii-- )
       for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP6 ;
     CP6: if( verb )
           INFO_message("last  %3d x-planes are zero [from %c]\n",
                        nx-1-ii,ORIENT_tinystr[dset->daxes->xxorient][1]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->xxorient][1] == 'S' ){
       SIax = 1 ; SIbot = 0 ; SItop = ii - (int)(SIhh/fabs(DSET_DX(dset))+0.5) ;
     }

     for( jj=0 ; jj < ny ; jj++ )
       for( kk=0 ; kk < nz ; kk++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP3 ;
     CP3: if( verb )
           INFO_message("first %3d y-planes are zero [from %c]\n",
                        jj,ORIENT_tinystr[dset->daxes->yyorient][0]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->yyorient][0] == 'S' ){
       SIax = 2 ; SIbot = jj + (int)(SIhh/fabs(DSET_DY(dset))+0.5) ; SItop = ny-1 ;
     }

     for( jj=ny-1 ; jj >= 0 ; jj-- )
       for( kk=0 ; kk < nz ; kk++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP4 ;
     CP4: if( verb )
           INFO_message("last  %3d y-planes are zero [from %c]\n",
                        ny-1-jj,ORIENT_tinystr[dset->daxes->yyorient][1]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->yyorient][1] == 'S' ){
       SIax = 2 ; SIbot = 0 ; SItop = jj - (int)(SIhh/fabs(DSET_DY(dset))+0.5) ;
     }

     for( kk=0 ; kk < nz ; kk++ )
       for( jj=0 ; jj < ny ; jj++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP1 ;
     CP1: if( verb )
           INFO_message("first %3d z-planes are zero [from %c]\n",
                        kk,ORIENT_tinystr[dset->daxes->zzorient][0]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->zzorient][0] == 'S' ){
       SIax = 3 ; SIbot = kk + (int)(SIhh/fabs(DSET_DZ(dset))+0.5) ; SItop = nz-1 ;
     }

     for( kk=nz-1 ; kk >= 0 ; kk-- )
       for( jj=0 ; jj < ny ; jj++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP2 ;
     CP2: if( verb )
           INFO_message("last  %3d z-planes are zero [from %c]\n",
                        nz-1-kk,ORIENT_tinystr[dset->daxes->zzorient][1]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->zzorient][1] == 'S' ){
       SIax = 3 ; SIbot = 0 ; SItop = kk - (int)(SIhh/fabs(DSET_DZ(dset))+0.5) ;
     }

     /* 06 Mar 2003: cut off stuff below SIhh mm from most Superior point */

     if( SIax > 0 && SIbot <= SItop ){
       char *cax="xyz" ;
       if( verb )
         INFO_message("SI clipping mask along axis %c from %d..%d\n" ,
                      cax[SIax-1] , SIbot,SItop ) ;
       switch( SIax ){
         case 1:
           for( ii=SIbot ; ii <= SItop ; ii++ )
             for( kk=0 ; kk < nz ; kk++ )
               for( jj=0 ; jj < ny ; jj++ ) mask[ii+jj*nx+kk*nxy] = 0 ;
         break ;
         case 2:
           for( jj=SIbot ; jj <= SItop ; jj++ )
             for( kk=0 ; kk < nz ; kk++ )
               for( ii=0 ; ii < nx ; ii++ ) mask[ii+jj*nx+kk*nxy] = 0 ;
         break ;
         case 3:
           for( kk=SIbot ; kk <= SItop ; kk++ )
             for( jj=0 ; jj < ny ; jj++ )
               for( ii=0 ; ii < nx ; ii++ ) mask[ii+jj*nx+kk*nxy] = 0 ;
         break ;
       }
       nmask = THD_countmask( DSET_NVOX(dset) , mask ) ;
       if( verb )
         INFO_message("%d voxels left [out of %d]\n",nmask,DSET_NVOX(dset)) ;
     }
   }


   /* create output dataset */
   if(prefix==NULL) {
      if(apply_prefix == NULL)
         prefix = amprefix;
   }

   if(prefix) {
      mset = EDIT_empty_copy( dset ) ;
      EDIT_dset_items( mset ,
                         ADN_prefix     , prefix   ,
                         ADN_datum_all  , MRI_byte ,
                         ADN_nvals      , 1        ,
                         ADN_ntt        , 0        ,
                         ADN_type       , HEAD_FUNC_TYPE ,
                         ADN_func_type  , FUNC_FIM_TYPE ,
                       ADN_none ) ;
      EDIT_substitute_brick( mset , 0 , MRI_byte , mask ) ;

      /* 16 Apr 2002: make history */

      tross_Copy_History( dset , mset ) ;
      tross_Make_History( "3dAutomask", argc,argv, mset ) ;

      DSET_write( mset ) ;
      if( verb ) WROTE_DSET(mset) ;
   }

   if (apply_mask) {
      if(verb) INFO_message("applying mask to original data\n");
      masked_dset = thd_apply_mask(dset, mask, apply_prefix);
      if(masked_dset){
         if(verb) INFO_message("Writing masked data\n");
         tross_Copy_History( dset , masked_dset ) ;
         tross_Make_History( "3dAutomask", argc,argv, masked_dset ) ;
         DSET_write(masked_dset);
         if( verb ) WROTE_DSET(masked_dset) ;
         DSET_unload( masked_dset ) ;  /* don't need data any more */
      }
      else {
         ERROR_exit("Could not apply mask to dataset");
      }
   }
   
   if (dodepth) {             /* ZSS March 02 2010 */
      if(verb) INFO_message("Writing depth dataset\n");
      if (mset) DSET_delete(mset); mset = NULL;
      mset = EDIT_empty_copy( dset ) ;
      EDIT_dset_items( mset ,
                         ADN_prefix     , depthprefix   ,
                         ADN_datum_all  , MRI_short ,
                         ADN_nvals      , 1        ,
                         ADN_ntt        , 0        ,
                         ADN_type       , HEAD_FUNC_TYPE ,
                         ADN_func_type  , FUNC_FIM_TYPE ,
                       ADN_none ) ;
      EDIT_substitute_brick( mset , 0 , MRI_short , depth ) ;

      /* 16 Apr 2002: make history */

      tross_Copy_History( dset , mset ) ;
      tross_Make_History( "3dAutomask", argc,argv, mset ) ;

      DSET_write( mset ) ;
      if( verb ) WROTE_DSET(mset) ;
      
      DSET_delete(mset); mset = NULL;
   }

   DSET_unload( dset ) ;  /* don't need data any more */

   if( verb ) INFO_message("CPU time = %f sec\n",COX_cpu_time()) ;

   exit(0) ;
}
