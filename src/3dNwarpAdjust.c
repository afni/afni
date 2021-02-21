#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#include "mri_genalign_util.c"
#include "mri_genalign.c"
#include "mri_nwarp.c"
#include "thd_conformist.c"

/*----------------------------------------------------------------------------*/
/* This program's function is probably limited to its usage in @toMNI_Qwarpar */
/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nwset=0,nsset=0 ; THD_3dim_dataset **dset_nwarp=NULL , **dset_src=NULL ;
   char *prefix = "AdjMean" ;
   int iarg , ii,kk , verb=1 , iv ;
   THD_3dim_dataset *dset_sbar=NULL , *dset_wbar , *dset_twarp ;
   int nx=0,ny=0,nz=0,nxyz=0, nxs=0,nys=0,nzs=0,nxyzs=0;
   IndexWarp3D *AA,*BB , *WWbin ;
   float *sbar=NULL , fac , Anorm,Bnorm ;
   int *ijkpad=NULL ;

   /**----------------------------------------------------------------------*/
   /**----------------- Help the pitifully ignorant user? -----------------**/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dNwarpAdjust [options]\n"
      "\n"
      "This program takes as input a bunch of 3D warps, averages them,\n"
      "and computes the inverse of this average warp.  It then composes\n"
      "each input warp with this inverse average to 'adjust' the set of\n"
      "warps.  Optionally, it can also read in a set of 1-brick datasets\n"
      "corresponding to the input warps, and warp each of them, and average\n"
      "those.\n"
      "\n"
      "           Input warps: Wi(x) for i=1..N\n"
      "          Average warp: Wbar(x) = mean of the displacements in Wi(x)\n"
      "       Inverse average: Wbin(x) = inverse of Wbar(x)\n"
      "        Adjusted warps: Ai(x) = Wi(Wbin(x))\n"
      "\n"
      "       Source datasets: Di(x) for i=1..N\n"
      "   Output mean dataset: average of Di(Ai(x))\n"
      "\n"
      "The logic behind this arcane necromancy is the following sophistry:\n"
      "\n"
      "   We use 3dQwarp to warp each Di(x) to match a template T(x), giving\n"
      "   warp Wi(x) such that Di(Wi(x)) matches T(x).  Now we want to average\n"
      "   these warped Di datasets to create a new template; say\n"
      "     B(x) = average of Di(Wi(x))\n"
      "   But the warps might be biased (e.g., have net shrinkage of the volumes).\n"
      "   So we compute the average warp Wbar(x), and its inverse Wbin(x), and then\n"
      "   instead we want to use as the new template B(Wbin(x)), which will 'put back'\n"
      "   each x to a bias-corrected location.  So then we have\n"
      "     B(Wbin(x)) = average of Di(Wi(Wbin(x)))\n"
      "   which is where the 'adjusted warp' Ai(x) = Wi(Wbin(x)) comes from.\n"
      "\n"
      "All these calculcations could be done with other programs and a script,\n"
      "but the goal of this program is to make them faster and simpler to combine.\n"
      "It is intended to be used in an incremental template-building script, and\n"
      "probably has no other utility (cf. the script @toMNI_Qwarpar).\n"
      "\n"
      "OPTIONS:\n"
      "--------\n"
      " -nwarp  w1 w2 ... = List of input 3D warp datasets (at least 5).\n"
      "                     The list ends when a command line argument starts\n"
      "                     with a '-' or the command line itself ends.\n"
      "                     * This 'option' is REQUIRED!\n"
      "                -->>** Each input warp is adjusted, and the altered warp\n"
      "                       over-writes the input dataset. (Therefore, there is\n"
      "                       no reason to run 3dNwarpAdjust twice over the same\n"
      "                       collection of warp datasets!)\n"
      "                     * These input warps do not have to be defined on\n"
      "                       exactly the same grids, but the grids must be\n"
      "                       'conformant' -- that is, they have to have the\n"
      "                       the same orientation and grid spacings.  Warps\n"
      "                       will be extended to match the minimum containing\n"
      "                       3D rectangular grid, as needed.\n"
      "\n"
      " -source d1 d2 ... = List of input 3D datasets to be warped by the adjusted\n"
      "                     warp datasets.  There must be exactly as many of these\n"
      "                     datasets as there are input warps.\n"
      "                     * This option is NOT required.\n"
      "                     * These datasets will NOT be altered by this program.\n"
      "                     * These datasets DO have to be on the same 3D grid\n"
      "                       (so they can be averaged after warping).\n"
      "\n"
      " -prefix ppp       = Use 'ppp' for the prefix of the output mean dataset.\n"
      "                     (Only needed if the '-source' option is also given.)\n"
      "                     The output dataset will be on the common grid shared\n"
      "                     by the source datasets.\n"
     ) ;

     PRINT_AFNI_OMP_USAGE("3dNwarpAdjust",NULL) ; PRINT_COMPILE_DATE ;
     exit(0) ;
   }

   /**--- bookkeeping and marketing ---**/

   mainENTRY("3dNwarpAdjust"); machdep();
   AFNI_logger("3dNwarpAdjust",argc,argv);
   PRINT_VERSION("3dNwarpAdjust"); AUTHOR("Zhark the Warped");
   (void)COX_clock_time() ;

   /**--- process command line options ---**/

   iarg = 1 ; THD_load_no_mmap() ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     /*---------------*/

     if( strcasecmp(argv[iarg],"-quiet") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     /*---------------*/

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       if( strcasecmp(argv[iarg],"NULL") == 0 )
         ERROR_exit("can't use filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       else
         prefix = strdup(argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-nwarp") == 0 || strcasecmp(argv[iarg],"-warp") == 0 ){
       int pad_xm,pad_xp,pad_ym,pad_yp,pad_zm,pad_zp , npad ;
       if( nwset > 0 ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       for( kk=iarg ; kk < argc && argv[kk][0] != '-' ; kk++,nwset++ ) ; /*nada*/
       if( nwset < 5 ) ERROR_exit("Need at least 5 datasets after '%s'",argv[iarg-1]) ;
       dset_nwarp = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*nwset) ;
       for( kk=0 ; kk < nwset ; kk++ ){
         dset_nwarp[kk] = THD_open_dataset( argv[iarg+kk] ) ; DSET_COPYOVER_REAL(dset_nwarp[kk]) ;
         if( dset_nwarp[kk] == NULL )
           ERROR_exit("can't open warp dataset '%s' :-(",argv[iarg+kk]);
         if( DSET_NVALS(dset_nwarp[kk]) < 3 ) ERROR_exit("dataset '%s' isn't a 3D warp",argv[iarg+kk]);
#if 0
         if( kk == 0 ){
           nx = DSET_NX(dset_nwarp[0]); ny = DSET_NY(dset_nwarp[0]); nz = DSET_NZ(dset_nwarp[0]); nxyz = nx*ny*nz;
         } else if( DSET_NX(dset_nwarp[kk]) != nx ||
                    DSET_NY(dset_nwarp[kk]) != ny ||
                    DSET_NZ(dset_nwarp[kk]) != nz   ){
           ERROR_exit("warp dataset '%s' doesn't match with grid size %dx%dx%d",argv[iarg+kk],nx,ny,nz) ;
         }
#endif
       }
       ijkpad = (int *)calloc(sizeof(int),6*nwset) ;
       kk = THD_conformist( nwset , dset_nwarp , CONFORM_NOREFIT , ijkpad ) ;
       if( kk < 0 )
         ERROR_exit("warp datasets grid do not conform to one another") ;
       for( npad=kk=0 ; kk < nwset ; kk++ ){
         pad_xm = ijkpad[6*kk+0] ; pad_xp = ijkpad[6*kk+1] ;
         pad_ym = ijkpad[6*kk+2] ; pad_yp = ijkpad[6*kk+3] ;
         pad_zm = ijkpad[6*kk+4] ; pad_zp = ijkpad[6*kk+5] ;
         if( pad_xm > 0 || pad_xp > 0 || pad_ym > 0 || pad_yp > 0 || pad_zm > 0 || pad_zp > 0 ){
           THD_3dim_dataset *qset ;
           if( verb ) INFO_message("extending input warp #%d: %d %d %d %d %d %d",
                                    kk , pad_xm,pad_xp,pad_ym,pad_yp,pad_zm,pad_zp ) ;
           qset = THD_nwarp_extend( dset_nwarp[kk] ,
                                    pad_xm,pad_xp,pad_ym,pad_yp,pad_zm,pad_zp ) ;
           if( qset == NULL )
             ERROR_exit("Cannot extend warp dataset %s to match containing grid",
                        DSET_HEADNAME(dset_nwarp[kk])) ;
           EDIT_dset_items( qset , ADN_prefix , DSET_prefix_noext(dset_nwarp[kk]) , ADN_none ) ;
           DSET_delete(dset_nwarp[kk]) ; DSET_lock(qset) ; dset_nwarp[kk] = qset ; npad++ ;
         }
       }
       nx = DSET_NX(dset_nwarp[0]); ny = DSET_NY(dset_nwarp[0]); nz = DSET_NZ(dset_nwarp[0]); nxyz = nx*ny*nz;
       if( npad == 0 && verb )
         INFO_message("All %d input warp datasets matched grid %dx%dx%d",nwset,nx,ny,nz) ;
       else
         INFO_message("%d input warp dataset%s (out of %d) %s padded to match grid %dx%dx%d",
                      npad  , (npad > 1) ? "s"    : "\0"  ,
                      nwset , (npad > 1) ? "were" : "was" ,
                      nx,ny,nz ) ;
       iarg += nwset ; continue ;
     }

     /*---------------*/

     if( strcasecmp(argv[iarg],"-source") == 0 ){
       if( nsset > 0 ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       for( kk=iarg ; kk < argc && argv[kk][0] != '-' ; kk++,nsset++ ) ; /*nada*/
       if( nsset < 5 ) ERROR_exit("Need at least 5 datasets after '%s'",argv[iarg-1]) ;
       dset_src = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*nsset) ;
       for( kk=0 ; kk < nsset ; kk++ ){
         dset_src[kk] = THD_open_dataset( argv[iarg+kk] ) ; DSET_COPYOVER_REAL(dset_src[kk]) ;
         if( dset_src[kk] == NULL )
           ERROR_exit("can't open warp dataset '%s' :-(",argv[iarg+kk]);
         if( DSET_NVALS(dset_src[kk]) > 1 ) ERROR_exit("dataset '%s' has more than 1 sub-brick",argv[iarg+kk]);
         if( kk == 0 ){
           nxs = DSET_NX(dset_src[0]); nys = DSET_NY(dset_src[0]); nzs = DSET_NZ(dset_src[0]); nxyzs = nxs*nys*nzs;
         } else if( DSET_NX(dset_src[kk]) != nxs ||
                    DSET_NY(dset_src[kk]) != nys ||
                    DSET_NZ(dset_src[kk]) != nzs   ){
           ERROR_exit("source dataset '%s' doesn't match with grid size %dx%dx%d",argv[iarg+kk],nxs,nys,nzs) ;
         }
       }
       iarg += nsset ; continue ;
     }

     /*---------------*/

     ERROR_message("Weird and Unknown option '%s' :-( :-( :-(",argv[iarg]) ;
     suggest_best_prog_option(argv[0],argv[iarg]) ;
     exit(1) ;

   }

   Hverb = (verb > 0) ;  /* for IW3D_invert */

   /*-------- check inputs to see if the user is completely demented ---------*/

   if( dset_nwarp == NULL )
     ERROR_exit("No -nwarp option?  Did you bother to read the -help?") ;

   if( nsset > 0 && nsset != nwset )
     ERROR_exit("Number of -source datasets %d doesn't match number of -nwarp datasets %d",nsset,nwset) ;


   if( verb && nsset > 0 && ( nx != nxs || ny != nys || nz != nzs ) )
     INFO_message("warp grid = %dx%dx%d is bigger than source grid = %dx%dx%d (this is NOT a problem)",
                  nx,ny,nz , nxs,nys,nzs ) ;

   /*--- the actual work (bow your head in reverence) ---*/

   /* mean of input displacements */

   if( verb ) fprintf(stderr,"++ Computing mean warp") ;

   dset_wbar = THD_mean_dataset( nwset , dset_nwarp , 0,2 , verb ) ;

   if( verb ) fprintf(stderr,"\n") ;

   if( dset_wbar == NULL )
     ERROR_exit("Can't create mean -nwarp dataset :-( ??") ;

   /* convert to an index warp and invert that */

   if( verb ) fprintf(stderr,"++ Inverting mean warp") ;

   AA    = IW3D_from_dataset( dset_wbar , 0,0 ); Anorm = IW3D_normL1(AA   ,NULL);
   WWbin = IW3D_invert( AA, NULL , MRI_WSINC5 ); Bnorm = IW3D_normL1(WWbin,NULL);
   IW3D_destroy( AA ) ;

   if( verb ){
     fprintf(stderr,"\n") ;
     ININFO_message("Mean warp L1 norm = %.2f voxel displacement",Anorm) ;
     ININFO_message("Inverse   L1 norm = %.2f voxel displacement",Bnorm) ;
   }

   /* now adjust and re-write all input warps */

   THD_set_quiet_overwrite(1) ;
   THD_force_ok_overwrite(1) ;
   putenv("AFNI_WSINC5_SILENT=YES") ;

   if( dset_src != NULL ){
     INFO_message("Creating average dataset in memory") ;
     dset_sbar = EDIT_empty_copy( dset_src[0] ) ;
     EDIT_dset_items( dset_sbar ,
                        ADN_prefix , prefix ,
                        ADN_brick_fac , NULL ,
                      ADN_none ) ;
     sbar = (float *)calloc(sizeof(float),nxyzs) ;
     EDIT_substitute_brick( dset_sbar , 0 , MRI_float , sbar ) ;
   }

   if( verb == 1 ) fprintf(stderr,"++ Adjusting") ;
   else if( verb > 1 ) INFO_message("========== Beginning adjustment process ==========") ;

   for( kk=0 ; kk < nwset ; kk++ ){
     if( verb > 1 )
       INFO_message("convert dataset warp #%d to index warp",kk) ;
     AA = IW3D_from_dataset( dset_nwarp[kk] , 0,0 ) ;
     if( AA == NULL ) continue ;  /* should not happen */
     if( verb > 1 ) ININFO_message("  compose with mean inverse") ;
     BB = IW3D_compose( AA , WWbin , MRI_WSINC5 ) ;
     IW3D_destroy(AA) ;
     if( verb > 1 ) ININFO_message("  convert back to dataset warp") ;
     dset_twarp = IW3D_to_dataset( BB , "WeLoveTheLeader" ) ;
     IW3D_destroy(BB) ;

     if( verb == 1 ) fprintf(stderr,".") ;

     if( verb > 1 ) ININFO_message("  load input dataset warp again (just to be safe)") ;
     DSET_load( dset_nwarp[kk] ) ;
     if( verb > 1 ) ININFO_message("  substitute brick #0") ;
     EDIT_substitute_brick( dset_nwarp[kk] , 0 , MRI_float , DSET_ARRAY(dset_twarp,0) ) ;
     if( verb > 1 ) ININFO_message("  substitute brick #1") ;
     EDIT_substitute_brick( dset_nwarp[kk] , 1 , MRI_float , DSET_ARRAY(dset_twarp,1) ) ;
     if( verb > 1 ) ININFO_message("  substitute brick #2") ;
     EDIT_substitute_brick( dset_nwarp[kk] , 2 , MRI_float , DSET_ARRAY(dset_twarp,2) ) ;
     if( verb > 1 ) ININFO_message("  delete temporary warp dataset") ;
     DSET_NULL_ARRAY(dset_twarp,0) ;
     DSET_NULL_ARRAY(dset_twarp,1) ;
     DSET_NULL_ARRAY(dset_twarp,2) ; DSET_delete(dset_twarp) ;
     tross_Make_History( "3dNwarpAdjust" , argc,argv , dset_nwarp[kk] ) ;
     if( verb > 1 ) ININFO_message("  write out adjusted warp dataset") ;
     DSET_write( dset_nwarp[kk] ) ;

     if( dset_src != NULL ){
       THD_3dim_dataset *dset_www ; float *bb ;
       if( verb > 1 ) ININFO_message("  re-warp from source") ;
       dset_www = THD_nwarp_dataset( dset_nwarp[kk] , dset_src[kk] , NULL ,
                                     "BondJamesBond" , MRI_WSINC5 , MRI_WSINC5 , 0.0f,1.0f,1,NULL ) ;
       bb = (float *)DSET_ARRAY(dset_www,0) ;
       if( verb > 1 ) ININFO_message("  add to mean") ;
       for( ii=0 ; ii < nxyzs ; ii++ ) sbar[ii] += bb[ii] ;
       if( verb > 1 ) ININFO_message("  delete www dataset") ;
       DSET_delete(dset_www) ;
       if( verb > 1 ) ININFO_message("  delete src dataset") ;
       DSET_delete(dset_src[kk]) ;
     }

     if( verb > 1 ) ININFO_message("  delete warp dataset from memory") ;
     DSET_delete( dset_nwarp[kk] ) ;
   }

   if( verb == 1 ) fprintf(stderr,"\n") ;

   if( dset_sbar != NULL ){
     fac = 1.0f / nwset ;
     for( ii=0 ; ii < nxyzs ; ii++ ) sbar[ii] *= fac ;
     DSET_write(dset_sbar) ;
     if( verb ) WROTE_DSET(dset_sbar) ;
     DSET_delete(dset_sbar) ;
   }

   if( verb ) INFO_message("total CPU time = %.1f sec  Elapsed = %.1f\n",
                           COX_cpu_time() , COX_clock_time() ) ;
   exit(0) ;
}
