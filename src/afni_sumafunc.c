#undef MAIN

#include "afni.h"

/*-----------------------------------------------------------------------*/
/*! Create a nodal color overlay from a voxel map.
    - Return value is number of nodes overlaid
    -  0 return ==> no overlay
    - -1 return ==> some error (e.g., no surface nodes on this dataset)
    - *map is set to a newly malloc()-ed array (if return > 0)
    - *nvused is set to the number of functional dataset voxels used to
        make the map (e.g., those that got some color)
    - im3d->anat_now->su_vnlist->nvox will have the total number of
       functional dataset voxels that intersected the surface

    Sample usage:
    - SUMA_irgba *map ;
    - int        nmap ;
    - nmap = AFNI_vnlist_func_overlay( im3d , &map ) ;
    -      if( nmap <  0 ){ ** error ** }
    - else if( nmap == 0 ){ ** nothing to show ** }
    - else                { ** show map[0..nmap-1] ** }
-------------------------------------------------------------------------*/

int AFNI_vnlist_func_overlay( Three_D_View *im3d, SUMA_irgba **map, int *nvused )
{
   MRI_IMAGE *im_thr , *im_fim ;
   short fim_ovc[NPANE_MAX+1] ;
   byte  ovc_r[NPANE_MAX+1], ovc_g[NPANE_MAX+1], ovc_b[NPANE_MAX+1] ;
   int ii,jj,nn , lp , num_lp , function_type , fdset_type , ival ;
   float scale_factor , scale_thr=1.0 , scale_fim=1.0 ;
   MCW_pbar * pbar ;
   Boolean have_thr ;
   int     simult_thr , need_thr ;
   THD_3dim_dataset *adset , *fdset ;
   SUMA_irgba *mmm ;
   SUMA_ixyz  *ixyz ;
   int nvox,nnod,nout , *numnod , *voxijk , *nlist ;
   int *vlist ;
   int nvout ;   /* 13 Mar 2002 */

ENTRY("AFNI_vnlist_func_overlay") ;

   /* check inputs for goodness */

   if( map == NULL || !IM3D_VALID(im3d) ) RETURN(-1) ; /* that was easy */

   if( nvused != NULL ) *nvused = 0 ;      /* default return value here */

   /* check datasets for goodness */

   adset = im3d->anat_now ;                    /* anat dataset */
   if( adset == NULL          ||               /* must have surface */
       adset->su_surf == NULL   ) RETURN(-1) ;

   fdset = im3d->fim_now  ; if( fdset == NULL ) RETURN(-1) ;

   /* figure out what we are showing from the func dataset */

   function_type = im3d->vinfo->showfunc_type ;
   fdset_type    = fdset->func_type ;
   have_thr      = FUNC_HAVE_THR( fdset_type ) ;

   if( ISFUNCBUCKET(fdset) )
      ival = im3d->vinfo->thr_index ;
   else
      ival = FUNC_ival_thr[fdset_type] ;    /* sub-brick for threshold */

   if( function_type == SHOWFUNC_THR && !have_thr ) RETURN(-1) ;

   /* get the component images */

   need_thr = have_thr && ( function_type == SHOWFUNC_THR ||      /* 10 Dec 1997 */
                            im3d->vinfo->func_threshold > 0.0 ) ;

   if( need_thr ) im_thr = DSET_BRICK(fdset,ival) ;
   else           im_thr = NULL ;

   have_thr = (im_thr != NULL) ;

   if( have_thr ){
     scale_thr = DSET_BRICK_FACTOR(fdset,ival) ;
     if( scale_thr == 0.0 ) scale_thr = 1.0 ;
   }

   if( function_type == SHOWFUNC_FIM ){
      int ind ;

      if( fdset_type == FUNC_FIM_TYPE ){   /* Mar 1997: allow for 3D+t FIM */
         ind = im3d->vinfo->time_index ;
         if( ind >= DSET_NUM_TIMES(fdset) )
            ind = DSET_NUM_TIMES(fdset) - 1 ;
      } else {
         if( ISFUNCBUCKET(fdset) )         /* 30 Nov 1997 */
            ind = im3d->vinfo->fim_index ;
         else
            ind = FUNC_ival_fim[fdset_type] ;
      }
      im_fim       = DSET_BRICK(fdset,ind) ;
      scale_factor = im3d->vinfo->fim_range ;
      if( scale_factor == 0.0 ) scale_factor = im3d->vinfo->fim_autorange ;

      scale_fim = DSET_BRICK_FACTOR(fdset,ind) ;
      if( scale_fim == 0.0 ) scale_fim = 1.0 ;

   } else {
      im_fim = im_thr ;
      scale_factor = im3d->vinfo->fim_range ;
      if( scale_factor == 0.0 ) scale_factor = im3d->vinfo->fim_autorange ;
      scale_fim = scale_thr ;
   }

   /* if component images not good, quit now */

   if( im_fim == NULL ) RETURN(-1) ;

   if( !AFNI_GOOD_FUNC_DTYPE(im_fim->kind) ||
       ( im_thr != NULL && !AFNI_GOOD_FUNC_DTYPE(im_thr->kind) ) ){

      RETURN(-1) ;
   }

   /* maybe need to build a voxel-to-node list for func dataset */

   if( adset->su_vnlist == NULL ||
       !EQUIV_DATAXES(adset->su_vnlist->dset->daxes,fdset->daxes) ){

     if( adset->su_vnlist != NULL )
        SUMA_destroy_vnlist( adset->su_vnlist ) ;

     adset->su_vnlist = SUMA_make_vnlist( adset->su_surf , fdset ) ;
     if( adset->su_vnlist == NULL ) RETURN(-1) ;
   }

   /* create array of voxel indexes (vlist);
      will put in there the voxels that are above threshold */

   nvox   = adset->su_vnlist->nvox   ; if( nvox < 1 ) RETURN(0);
   voxijk = adset->su_vnlist->voxijk ;  /* list of voxels with surface nodes */
   numnod = adset->su_vnlist->numnod ;  /* number of nodes in each voxel */

   nnod = adset->su_surf->num_ixyz   ; if( nnod < 1 ) RETURN(0);
   ixyz = adset->su_surf->ixyz ;

#if 0
fprintf(stderr,"AFNI_vnlist_func_overlay: nvox=%d nnod=%d\n",nvox,nnod) ;
#endif

   /** if don't have threshold, will process all voxels for color **/

   DSET_load(fdset) ;  /* just in case isn't in memory yet */
   if( !DSET_LOADED(fdset) ) RETURN(-1) ;

   if( im_thr == NULL ){

      vlist = voxijk ;  /* list of voxels to process */
      nout  = nnod   ;  /* number of output nodes */

   /** if have threshold, cut out voxels below threshold (set vlist[]=-1) **/

   } else {

     vlist = (int *) malloc(sizeof(int)*nvox) ;    /* copy voxel list */
     memcpy( vlist , voxijk , sizeof(int)*nvox ) ;

     switch( im_thr->kind ){
       case MRI_short:{
         short thresh = im3d->vinfo->func_threshold
                      * im3d->vinfo->func_thresh_top / scale_thr ;
         short *ar_thr = MRI_SHORT_PTR(im_thr) ;
         for( ii=0 ; ii < nvox ; ii++ ){  /* voxel cutting time */
           jj = vlist[ii] ;               /* actual voxel index in func brick */
           if( ar_thr[jj] > -thresh && ar_thr[jj] < thresh ) vlist[ii] = -1 ;
         }
       }
       break ;

       case MRI_byte:{
         byte thresh = im3d->vinfo->func_threshold
                     * im3d->vinfo->func_thresh_top / scale_thr ;
         byte *ar_thr = MRI_BYTE_PTR(im_thr) ;
         for( ii=0 ; ii < nvox ; ii++ ){
           jj = vlist[ii] ;
           if( ar_thr[ii] < thresh ) vlist[ii] = -1 ;
         }
       }
       break ;

       case MRI_float:{
          float thresh = im3d->vinfo->func_threshold
                       * im3d->vinfo->func_thresh_top / scale_thr ;
          float *ar_thr = MRI_FLOAT_PTR(im_thr) ;
          for( ii=0 ; ii < nvox ; ii++ ){
            jj = vlist[ii] ;
            if( ar_thr[jj] > -thresh && ar_thr[jj] < thresh ) vlist[ii] = -1 ;
          }
       }
       break ;
     } /* end of switch on threshold sub-brick type */

     /* count surviving voxels; exit if there aren't any */

     for( jj=ii=0 ; ii < nvox ; ii++ )
        if( vlist[ii] >= 0 ) jj++ ;
#if 0
fprintf(stderr,"Number functional voxels above threshold = %d\n",jj) ;
#endif
     if( jj == 0 ){ free(vlist) ; RETURN(0) ; }

     /* count output nodes */

     for( nout=ii=0 ; ii < nvox ; ii++ )
        if( vlist[ii] >= 0 ) nout += numnod[ii] ;

#if 0
fprintf(stderr,"Number of colored nodes in voxels = %d\n",nout) ;
#endif

   } /* end of if on existence of threshold sub-brick */

   /*** allocate output structure (maybe too big, but will clip it later) ***/

   mmm = (SUMA_irgba *) malloc( sizeof(SUMA_irgba) * nout ) ;

   /*** set overlay colors ***/

   pbar   = im3d->vwid->func->inten_pbar ;
   num_lp = pbar->num_panes ;

   for( lp=0 ; lp < num_lp ; lp++ )       /* overlay color indexes */
      fim_ovc[lp] = pbar->ov_index[lp] ;  /* run from top of pbar down */

   /* overlay color index for values below bottom of pbar */

   fim_ovc[num_lp] = (im3d->vinfo->use_posfunc) ? (0) : (fim_ovc[num_lp-1]) ;

   /* get the actual RGB colors of each pane on the pbar */

   for( lp=0 ; lp <= num_lp ; lp++ ){
      ovc_r[lp] = DCOV_REDBYTE  (im3d->dc,fim_ovc[lp]) ;
      ovc_g[lp] = DCOV_GREENBYTE(im3d->dc,fim_ovc[lp]) ;
      ovc_b[lp] = DCOV_BLUEBYTE (im3d->dc,fim_ovc[lp]) ;
   }

   /** process im_fim into overlay, depending on data type **/

   switch( im_fim->kind ){

      default: nvout = nout = 0 ; break ;   /* should never happen */

      case MRI_short:{
         short * ar_fim = MRI_SHORT_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;
         byte r,g,b ;

#if 0
fprintf(stderr,"scale_factor=%f\n",scale_factor) ;
#endif

         for( lp=0 ; lp < num_lp ; lp++ ) /* thresholds for each pane */
           fim_thr[lp] = (scale_factor/scale_fim) * pbar->pval[lp+1] ;
#if 0
for(lp=0;lp<num_lp;lp++)
fprintf(stderr,"  fim_thr[%d]=%f\n",lp,fim_thr[lp]) ;
#endif

         nvout = nout = 0 ;                           /* num output nodes */
         for( ii=0 ; ii < nvox ; ii++ ){
            jj = vlist[ii] ; if( jj < 0 ) continue ;  /* skip voxel? */
            /* find pane this voxel is in */
            for( lp=0; lp < num_lp && ar_fim[jj] < fim_thr[lp]; lp++ ) ; /*nada*/
            if( fim_ovc[lp] == 0 ) continue ;         /* uncolored pane */
            r = ovc_r[lp]; g = ovc_g[lp]; b = ovc_b[lp];
            nlist = adset->su_vnlist->nlist[ii] ;     /* list of nodes */
            for( nn=0 ; nn < numnod[ii] ; nn++ ){     /* loop over nodes */
               mmm[nout].id = ixyz[nlist[nn]].id ;
               mmm[nout].r  = r ; mmm[nout].g = g ;
               mmm[nout].b  = b ; mmm[nout].a = 255 ; nout++ ;
#if 0
fprintf(stderr,"voxel=%d node index=%d ID=%d rgb=%d %d %d (%02x %02x %02x)\n",
        jj,ii,ixyz[nlist[nn]].id,r,g,b,r,g,b ) ;
#endif
            }
            nvout++ ;                           /* number of voxels used */
         }
      }
      break ;

      case MRI_byte:{
         byte * ar_fim = MRI_BYTE_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;
         byte r,g,b ;

         for( lp=0 ; lp < num_lp ; lp++ )
           if( pbar->pval[lp+1] <= 0.0 )
             fim_thr[lp] = 0 ;
           else
             fim_thr[lp] = (scale_factor/scale_fim) * pbar->pval[lp+1] ;

         nvout = nout = 0 ;                           /* num output nodes */
         for( ii=0 ; ii < nvox ; ii++ ){
            jj = vlist[ii] ; if( jj < 0 ) continue ;  /* skip voxel? */
            /* find pane this voxel is in */
            for( lp=0; lp < num_lp && ar_fim[jj] < fim_thr[lp]; lp++ ) ; /*nada*/
            if( fim_ovc[lp] == 0 ) continue ;         /* uncolored pane */
            r = ovc_r[lp]; g = ovc_g[lp]; b = ovc_b[lp];
            nlist = adset->su_vnlist->nlist[ii] ;     /* list of nodes */
            for( nn=0 ; nn < numnod[ii] ; nn++ ){     /* loop over nodes */
               mmm[nout].id = ixyz[nlist[nn]].id ;
               mmm[nout].r  = r ; mmm[nout].g = g ;
               mmm[nout].b  = b ; mmm[nout].a = 255 ; nout++ ;
            }
            nvout++ ;                           /* number of voxels used */
         }
      }
      break ;

      case MRI_float:{
         float * ar_fim = MRI_FLOAT_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;
         byte r,g,b ;

         for( lp=0 ; lp < num_lp ; lp++ )
            fim_thr[lp] = (scale_factor/scale_fim) * pbar->pval[lp+1] ;

         nvout = nout = 0 ;                           /* num output nodes */
         for( ii=0 ; ii < nvox ; ii++ ){
            jj = vlist[ii] ; if( jj < 0 ) continue ;  /* skip voxel? */
            /* find pane this voxel is in */
            for( lp=0; lp < num_lp && ar_fim[jj] < fim_thr[lp]; lp++ ) ; /*nada*/
            if( fim_ovc[lp] == 0 ) continue ;         /* uncolored pane */
            r = ovc_r[lp]; g = ovc_g[lp]; b = ovc_b[lp];
            nlist = adset->su_vnlist->nlist[ii] ;     /* list of nodes */
            for( nn=0 ; nn < numnod[ii] ; nn++ ){     /* loop over nodes */
               mmm[nout].id = ixyz[nlist[nn]].id ;
               mmm[nout].r  = r ; mmm[nout].g = g ;
               mmm[nout].b  = b ; mmm[nout].a = 255 ; nout++ ;
            }
            nvout++ ;                           /* number of voxels used */
         }
      }
      break ;
   }

   /** finished: clean up and exit **/

   if( vlist != voxijk ) free(vlist) ;  /* toss trash, if it is trash */

   if( nout == 0 ){ free(mmm); RETURN(0); }  /* no overlay? */

   *map = (SUMA_irgba *) realloc( mmm , sizeof(SUMA_irgba)*nout ) ;

   if( nvused != NULL ) *nvused = nvout ;    /* 13 Mar 2002 */

   RETURN(nout) ;
}
