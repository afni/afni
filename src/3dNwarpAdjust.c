#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#include "mri_genalign_util.c"
#include "mri_genalign.c"
#include "mri_nwarp.c"

/* prototypes */

int THD_conformant_dataxes( THD_dataxes *ax , THD_dataxes *bx ) ;
THD_dataxes * THD_superset_dataxes( THD_dataxes *ax , THD_dataxes *bx ) ;

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nwset=0,nsset=0 ; THD_3dim_dataset **dset_nwarp=NULL , **dset_src=NULL ;
   char *prefix = "AdjMean" ;
   int iarg , ii,kk , verb=1 , iv ;
   THD_3dim_dataset *dset_sbar=NULL , *dset_wbar , *dset_twarp ;
   int nx=0,ny=0,nz=0,nxyz=0, nxs=0,nys=0,nzs=0;
   IndexWarp3D *AA,*BB , *WWbin ;
   float *sbar=NULL , fac , Anorm,Bnorm ;

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
      "probably has no other utility.\n"
      "\n"
      "OPTIONS:\n"
      "--------\n"
      " -nwarp  w1 w2 ... = List of input 3D warp datasets (at least 5).\n"
      "                     The list ends when a command line argument starts\n"
      "                     with a '-' or the command line itself ends.\n"
      "                     * This 'option' is required!\n"
      "                -->>** Each input warp is adjusted, and the altered warp\n"
      "                       over-writes the input dataset.\n"
      "\n"
      " -source d1 d2 ... = List of input 3D datasets to be warped.  There must\n"
      "                     be exactly as many of these datasets as there are\n"
      "                     input warps.\n"
      "                     * This option is not required.\n"
      "                     * These datasets will NOT be altered by this program.\n"
      "\n"
      " -prefix ppp       = Use 'ppp' for the prefix of the output mean dataset.\n"
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
       if( nwset > 0 ) ERROR_exit("Can't have multiple %s options :-(",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("No argument after '%s' :-(",argv[iarg-1]) ;
       for( kk=iarg ; kk < argc && argv[kk][0] != '-' ; kk++,nwset++ ) ; /*nada*/
       if( nwset < 5 ) ERROR_exit("Need at least 5 datasets after '%s'",argv[iarg-1]) ;
       dset_nwarp = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*nwset) ;
       for( kk=0 ; kk < nwset ; kk++ ){
         dset_nwarp[kk] = THD_open_dataset( argv[iarg+kk] ) ;
         if( dset_nwarp[kk] == NULL )
           ERROR_exit("can't open warp dataset '%s' :-(",argv[iarg+kk]);
         if( DSET_NVALS(dset_nwarp[kk]) < 3 ) ERROR_exit("dataset '%s' isn't a 3D warp",argv[iarg+kk]);
         if( kk == 0 ){
           nx = DSET_NX(dset_nwarp[0]); ny = DSET_NY(dset_nwarp[0]); nz = DSET_NZ(dset_nwarp[0]); nxyz = nx*ny*nz;
         } else if( DSET_NX(dset_nwarp[kk]) != nx ||
                    DSET_NY(dset_nwarp[kk]) != ny ||
                    DSET_NZ(dset_nwarp[kk]) != nz   ){
           ERROR_exit("warp dataset '%s' doesn't match with grid size %dx%dx%d",argv[iarg+kk],nx,ny,nz) ;
         }
       }
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
         dset_src[kk] = THD_open_dataset( argv[iarg+kk] ) ;
         if( dset_src[kk] == NULL )
           ERROR_exit("can't open warp dataset '%s' :-(",argv[iarg+kk]);
         if( DSET_NVALS(dset_src[kk]) > 1 ) ERROR_exit("dataset '%s' has more than 1 sub-brick",argv[iarg+kk]);
         if( kk == 0 ){
           nxs = DSET_NX(dset_src[0]); nys = DSET_NY(dset_src[0]); nzs = DSET_NZ(dset_src[0]);
         } else if( DSET_NX(dset_src[kk]) != nxs ||
                    DSET_NY(dset_src[kk]) != nys ||
                    DSET_NZ(dset_src[kk]) != nzs   ){
           ERROR_exit("source dataset '%s' doesn't match with grid size %dx%dx%d",argv[iarg+kk],nxs,nys,nzs) ;
         }
       }
       iarg += nsset ; continue ;
     }

     /*---------------*/

     ERROR_exit("Unknown, Illegal, and Fattening option '%s' :-( :-( :-(",argv[iarg]) ;
   }

   /*-------- check inputs to see if the user is completely demented ---------*/

   if( dset_nwarp == NULL )
     ERROR_exit("No -nwarp option?  Did you bother to read the -help?") ;

   if( nsset > 0 && nsset != nwset )
     ERROR_exit("Number of -source datasets %d doesn't match number of -nwarp datasets %d",nsset,nwset) ;

   if( nsset > 0 && ( nxs != nx || nys != ny || nzs != nz ) )
     ERROR_exit("-source datasets grid %dx%dx%d doesn't match -nwarp grid %dx%dx%d",
                nxs,nys,nzs , nx,ny,nz ) ;

   /*--- the actual work (bow your head in reverence) ---*/

   /* mean of input displacements */

   if( verb ) fprintf(stderr,"++ Mean warp") ;

   dset_wbar = THD_mean_dataset( nwset , dset_nwarp , 0,2 , verb ) ;

   if( verb ) fprintf(stderr,"\n") ;

   if( dset_wbar == NULL )
     ERROR_exit("Can't create mean -nwarp dataset :-( ??") ;

   /* convert to an index warp and invert that */

   if( verb ) fprintf(stderr,"++ Invert mean warp") ;

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
     dset_sbar = EDIT_empty_copy( dset_src[0] ) ;
     EDIT_dset_items( dset_sbar ,
                        ADN_prefix , prefix ,
                        ADN_brick_fac , NULL ,
                      ADN_none ) ;
     sbar = (float *)calloc(sizeof(float),nxyz) ;
     EDIT_substitute_brick( dset_sbar , 0 , MRI_float , sbar ) ;
   }

   if( verb ) fprintf(stderr,"++ Adjusting") ;

   for( kk=0 ; kk < nwset ; kk++ ){
     AA = IW3D_from_dataset( dset_nwarp[kk] , 0,0 ) ;
     if( AA == NULL ) continue ;  /* should not happen */
     BB = IW3D_compose( AA , WWbin , MRI_WSINC5 ) ;
     IW3D_destroy(AA) ;
     dset_twarp = IW3D_to_dataset( BB , "WeLoveTheLeader" ) ;
     IW3D_destroy(BB) ;

     if( verb ) fprintf(stderr,".") ;

     DSET_load( dset_nwarp[kk] ) ;
     EDIT_substitute_brick( dset_nwarp[kk] , 0 , MRI_float , DSET_ARRAY(dset_twarp,0) ) ;
     EDIT_substitute_brick( dset_nwarp[kk] , 1 , MRI_float , DSET_ARRAY(dset_twarp,1) ) ;
     EDIT_substitute_brick( dset_nwarp[kk] , 2 , MRI_float , DSET_ARRAY(dset_twarp,2) ) ;
     DSET_NULL_ARRAY(dset_twarp,0) ;
     DSET_NULL_ARRAY(dset_twarp,1) ;
     DSET_NULL_ARRAY(dset_twarp,2) ; DSET_delete(dset_twarp) ;
     tross_Make_History( "3dNwarpAdjust" , argc,argv , dset_nwarp[kk] ) ;
     DSET_write( dset_nwarp[kk] ) ;

     if( dset_src != NULL ){
       THD_3dim_dataset *dset_www ; float *bb ;
       dset_www = THD_nwarp_dataset( dset_nwarp[kk] , dset_src[kk] , NULL ,
                                     "BondJamesBond" , MRI_WSINC5 , MRI_WSINC5 , 0.0f,1.0f,1,NULL ) ;
       bb = (float *)DSET_ARRAY(dset_www,0) ;
       for( ii=0 ; ii < nxyz ; ii++ ) sbar[ii] += bb[ii] ;
       DSET_delete(dset_www) ; DSET_delete(dset_src[kk]) ;
     }

     DSET_delete( dset_nwarp[kk] ) ;
   }

   if( verb ) fprintf(stderr,"\n") ;

   fac = 1.0f / nwset ;
   for( ii=0 ; ii < nxyz ; ii++ ) sbar[ii] *= fac ;

   DSET_write(dset_sbar) ;
   if( verb ) WROTE_DSET(dset_sbar) ;
   if( verb ) INFO_message("total CPU time = %.1f sec  Elapsed = %.1f\n",
                           COX_cpu_time() , COX_clock_time() ) ;
   exit(0) ;
}

/*----------------------------------------------------------------------------*/

int THD_conformant_dataxes( THD_dataxes *ax , THD_dataxes *bx )
{
   float xo,yo,zo ;

   if( ax->xxorient != bx->xxorient ||
       ax->yyorient != bx->yyorient ||
       ax->zzorient != bx->zzorient   ) return 0 ;

   if( fabsf(ax->xxdel-bx->xxdel) > 0.001f ) return 0 ;
   if( fabsf(ax->yydel-bx->yydel) > 0.001f ) return 0 ;
   if( fabsf(ax->zzdel-bx->zzdel) > 0.001f ) return 0 ;

   xo = (ax->xxorg - bx->xxorg) / ax->xxdel ;
   yo = (ax->yyorg - bx->yyorg) / ax->yydel ;
   zo = (ax->zzorg - bx->zzorg) / ax->zzdel ;

   if( fabsf(xo-rintf(xo)) > 0.01f ||
       fabsf(yo-rintf(yo)) > 0.01f ||
       fabsf(zo-rintf(zo)) > 0.01f   ) return 0 ;

   return 1 ;
}

/*----------------------------------------------------------------------------*/

THD_dataxes * THD_superset_dataxes( THD_dataxes *ax , THD_dataxes *bx )
{
   THD_dataxes *cx ;
   float dx,dy,dz , axo,ayo,azo , bxo,byo,bzo , ae,be ;
   int nxa,nya,nza , nxb,nyb,nzb , ndif ;
   float cxo,cyo,czo ;
   int   nxc,nyc,nzc ;

   if( !THD_conformant_dataxes(ax,bx) ) return NULL ;

   /* create new dataxes as copy of first one */

   cx = myXtNew(THD_dataxes) ; *cx = *ax ;
   cx->parent = NULL ;
   if( EQUIV_DATAXES(ax,bx) ) return cx ;

   /* load some variables from the input structs */

   dx  = ax->xxdel ; dy  = ax->yydel ; dz  = ax->zzdel ;  /* same for ax & bx */
   axo = ax->xxorg ; ayo = ax->yyorg ; azo = ax->zzorg ;  /* ax origins */
   bxo = bx->xxorg ; byo = bx->yyorg ; bzo = bx->zzorg ;  /* bx origins */
   nxa = ax->nxx   ; nya = ax->nyy   ; nza = ax->nzz   ;  /* ax grid lengths */
   nxb = bx->nxx   ; nyb = bx->nyy   ; nzb = bx->nzz   ;  /* bx grid lengths */

   /* extend origins to the outermost */

   ndif = (int)rintf((axo-bxo)/dx) ; cxo = (ndif <= 0) ? axo : bxo ;
   ndif = (int)rintf((ayo-byo)/dy) ; cyo = (ndif <= 0) ? ayo : byo ;
   ndif = (int)rintf((azo-bzo)/dz) ; czo = (ndif <= 0) ? azo : bzo ;

   cx->xxorg = cxo ; cx->yyorg = cyo ; cx->zzorg = czo ;

   /* extend grid lengths to the outermost */

   ae = axo + nxa*dx ; be = bxo + nxb*dx ;
   ndif = (int)rintf((ae-be)/dx) ; if( ndif < 0 ) ae = be ;
   nxc  = (int)rintf((ae-cxo)/dx) ;

   ae = ayo + nya*dy ; be = byo + nyb*dy ;
   ndif = (int)rintf((ae-be)/dy) ; if( ndif < 0 ) ae = be ;
   nyc  = (int)rintf((ae-cyo)/dy) ;

   ae = azo + nza*dz ; be = bzo + nzb*dz ;
   ndif = (int)rintf((ae-be)/dz) ; if( ndif < 0 ) ae = be ;
   nzc  = (int)rintf((ae-czo)/dz) ;

   cx->nxx   = nxc ; cx->nyy   = nyc ; cx->nzz   = nzc ;

#if 0
fprintf(stderr,"\n") ;
INFO_message("ax: nxyz=%d %d %d  org=%g %g %g  del=%g %g %g",ax->nxx,ax->nyy,ax->nzz,ax->xxorg,ax->yyorg,ax->zzorg,ax->xxdel,ax->yydel,ax->zzdel) ;
INFO_message("bx: nxyz=%d %d %d  org=%g %g %g  del=%g %g %g",bx->nxx,bx->nyy,bx->nzz,bx->xxorg,bx->yyorg,bx->zzorg,bx->xxdel,bx->yydel,bx->zzdel) ;
INFO_message("cx: nxyz=%d %d %d  org=%g %g %g  del=%g %g %g",cx->nxx,cx->nyy,cx->nzz,cx->xxorg,cx->yyorg,cx->zzorg,cx->xxdel,cx->yydel,cx->zzdel) ;
#endif

   /* fix the matrices etc (probably not needed) */

   LOAD_ZERO_MAT(cx->to_dicomm) ;
   THD_daxes_to_mat44(cx) ;
   THD_set_daxes_bbox(cx) ;
   cx->ijk_to_dicom_real = cx->ijk_to_dicom ;

   return cx ;
}
