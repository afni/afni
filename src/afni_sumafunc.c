#undef MAIN

#include "afni.h"

/*-----------------------------------------------------------------------*/
/*! Create a nodal color overlay from a voxel map.
    - Input ks is surface index to use from anat dataset [12 Dec 2002]
    - Return value is number of nodes overlaid:
    -  0 return ==> no overlay
    - -1 return ==> some error (e.g., no surface nodes on this dataset)
    - *map is set to a newly malloc()-ed array (if return > 0)
    - *nvused is set to the number of functional dataset voxels used to
        make the map (e.g., those that got some color)
    - im3d->anat_now->su_vnlist->nvox will have the total number of
       functional dataset voxels that intersected the surface

    Sample usage:
    - SUMA_irgba *map ;
    - int        nmap , nvox ;
    - nmap = AFNI_vnlist_func_overlay( im3d , 0 , &map , &nvox ) ;
    -      if( nmap <  0 ){ ** error ** }
    - else if( nmap == 0 ){ ** nothing to show ** }
    - else                { ** show map[0..nmap-1] ** }

    Much of this function is a 3D-ification of AFNI_func_overlay().
-------------------------------------------------------------------------*/

int AFNI_vnlist_func_overlay( Three_D_View *im3d, int ks ,
                              SUMA_irgba **map, int *nvused )
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

   int bm , zbot=0 , kk ;        /* 02 Feb 2003: colorscale stuff */
   float fbot=0.0,ftop=0.0,ffac=0.0 , val ;
   rgbyte *cmap=NULL ;

ENTRY("AFNI_vnlist_func_overlay") ;

   /* check inputs for goodness */

   if( map == NULL || !IM3D_VALID(im3d) ) RETURN(-1) ; /* that was easy */

   if( nvused != NULL ) *nvused = 0 ;  /* set default return value here */

   /* check datasets for goodness */
   /* 12 Dec 2002: ks is now input (we used to compute it) */

   adset = im3d->anat_now ;            /* anat dataset must */
   if( adset              == NULL ||   /* have surface #ks  */
       adset->su_num      == 0    ||
       ks                 <  0    ||
       adset->su_num      <= ks   ||
       adset->su_surf[ks] == NULL   ) RETURN(-1) ;

   fdset = im3d->fim_now ; if( fdset == NULL ) RETURN(-1) ;

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

   need_thr = have_thr && ( function_type == SHOWFUNC_THR ||
                            im3d->vinfo->func_threshold > 0.0 ) ;

   if( need_thr ) im_thr = DSET_BRICK(fdset,ival) ;
   else           im_thr = NULL ;

   have_thr = (im_thr != NULL) ;

   if( have_thr ){
     scale_thr = DSET_BRICK_FACTOR(fdset,ival) ;
     if( scale_thr == 0.0 ) scale_thr = 1.0 ;
   }

   if( function_type == SHOWFUNC_FIM || !have_thr ){
     int ind ;

     /* figure out which sub-brick to show (= ind) */

     if( fdset_type == FUNC_FIM_TYPE ){   /* allow for 3D+t FIM */
       ind = im3d->vinfo->time_index ;
       if( ind >= DSET_NUM_TIMES(fdset) ) /* be careful */
         ind = DSET_NUM_TIMES(fdset) - 1 ;
     } else {
       if( ISFUNCBUCKET(fdset) )          /* allow for func bucket */
         ind = im3d->vinfo->fim_index ;
       else
         ind = FUNC_ival_fim[fdset_type] ;
     }
     im_fim       = DSET_BRICK(fdset,ind) ;  /* the sub-brick to show */
     scale_factor = im3d->vinfo->fim_range ;
     if( scale_factor == 0.0 ) scale_factor = im3d->vinfo->fim_autorange ;

     scale_fim = DSET_BRICK_FACTOR(fdset,ind) ;
     if( scale_fim == 0.0 ) scale_fim = 1.0 ;

   } else {                            /* show the threshold sub-brick */
     im_fim = im_thr ;
     scale_factor = im3d->vinfo->fim_range ;
     if( scale_factor == 0.0 ) scale_factor = im3d->vinfo->fim_autorange ;
     scale_fim = scale_thr ;
   }

   /* if component images not good, quit now */

   if( im_fim == NULL ) RETURN(-1) ;  /* no function!? */

   if( !AFNI_GOOD_FUNC_DTYPE(im_fim->kind) ||
       ( im_thr != NULL && !AFNI_GOOD_FUNC_DTYPE(im_thr->kind) ) ){

      RETURN(-1) ;  /* bad function - no soup for you */
   }

   /* maybe need to build a voxel-to-node list for func dataset  */
   /* (this is where the 3D-to-2D mapping is encapsulated, Ziad) */

   if( adset->su_vnlist[ks] == NULL ||
       !EQUIV_DATAXES(adset->su_vnlist[ks]->dset->daxes,fdset->daxes) ){

     /* if have an old one
        (that doesn't match current dataset grid),
        then delete it from the Universe           */

     if( adset->su_vnlist[ks] != NULL )
        SUMA_destroy_vnlist( adset->su_vnlist[ks] ) ;

     /* make the new list */

     adset->su_vnlist[ks] = SUMA_make_vnlist( adset->su_surf[ks] , fdset ) ;
     if( adset->su_vnlist[ks] == NULL ) RETURN(-1) ;
   }

   /* create array of voxel indexes (vlist);
      will put in there the voxels that are above threshold */

   nvox   = adset->su_vnlist[ks]->nvox  ; if( nvox < 1 ) RETURN(0);
   voxijk = adset->su_vnlist[ks]->voxijk; /* list of voxels with surface nodes */
   numnod = adset->su_vnlist[ks]->numnod; /* number of nodes in each voxel */

   nnod = adset->su_surf[ks]->num_ixyz  ; if( nnod < 1 ) RETURN(0);
   ixyz = adset->su_surf[ks]->ixyz      ; /* array of surface nodes */

#if 0
fprintf(stderr,"AFNI_vnlist_func_overlay: nvox=%d nnod=%d\n",nvox,nnod) ;
#endif

   DSET_load(fdset) ;                     /* if isn't in memory now */
   if( !DSET_LOADED(fdset) ) RETURN(-1) ; /* what the hell?         */

   /** if don't have threshold, will process all voxels for color **/

   if( im_thr == NULL ){

     vlist = voxijk ;  /* list of voxels to process = all voxels */
     nout  = nnod   ;  /* number of output nodes */

   /** if have threshold, cut out voxels below threshold (set vlist[]=-1) **/

   } else {

     vlist = (int *) malloc(sizeof(int)*nvox) ;    /* copy voxel list */
     memcpy( vlist , voxijk , sizeof(int)*nvox ) ; /* then prune it */

     switch( im_thr->kind ){
       case MRI_short:{
         int thresh = im3d->vinfo->func_threshold
                    * im3d->vinfo->func_thresh_top / scale_thr ;
         short *ar_thr = MRI_SHORT_PTR(im_thr) ;
         for( ii=0 ; ii < nvox ; ii++ ){  /* voxel cutting time */
           jj = vlist[ii] ;               /* actual voxel index in func brick */
           if( ar_thr[jj] > -thresh && ar_thr[jj] < thresh ) vlist[ii] = -1 ;
         }
       }
       break ;

       case MRI_byte:{
         int thresh = im3d->vinfo->func_threshold
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

     /* count output nodes inside each surviving voxel */

     for( nout=ii=0 ; ii < nvox ; ii++ )
        if( vlist[ii] >= 0 ) nout += numnod[ii] ;

#if 0
fprintf(stderr,"Number of colored nodes in voxels = %d\n",nout) ;
#endif

   } /* end of if on existence of threshold sub-brick */

   /** allocate output structure (maybe too big, but will clip it later) **/

   mmm = (SUMA_irgba *) malloc( sizeof(SUMA_irgba) * nout ) ;

   /** set overlay colors **/

   pbar   = im3d->vwid->func->inten_pbar ;
   num_lp = pbar->num_panes ;
   bm     = pbar->bigmode ;              /* 02 Feb 2003 */

   if( !bm ){                              /* indexed colors */
     for( lp=0 ; lp < num_lp ; lp++ )      /* overlay color indexes */
       fim_ovc[lp] = pbar->ov_index[lp] ;  /* run from top of pbar down */

     /* overlay color index for values below bottom of pbar */

     fim_ovc[num_lp] = (im3d->vinfo->use_posfunc) ? (0) : (fim_ovc[num_lp-1]) ;

     /* get the actual RGB colors of each pane on the pbar */

     for( lp=0 ; lp <= num_lp ; lp++ ){
       ovc_r[lp] = DCOV_REDBYTE  (im3d->dc,fim_ovc[lp]) ;
       ovc_g[lp] = DCOV_GREENBYTE(im3d->dc,fim_ovc[lp]) ;
       ovc_b[lp] = DCOV_BLUEBYTE (im3d->dc,fim_ovc[lp]) ;
     }

   } else {                                /* colorscale colors - 02 Feb 2003 */
     fbot = (scale_factor/scale_fim)*pbar->bigbot ;
     ftop = (scale_factor/scale_fim)*pbar->bigtop ;
     ffac = NPANE_BIG / (ftop-fbot) ;
     cmap = pbar->bigcolor ;
     zbot = (fbot == 0.0) ;
   }

   /** process im_fim into overlay, depending on data type **/

   switch( im_fim->kind ){

      default: nvout = nout = 0 ; break ;   /* should never happen! */

      case MRI_rgb:{                        /* 17 Apr 2002 */
        byte *ar_fim = MRI_RGB_PTR(im_fim); /* colors direct from fim */
        byte r,g,b ;

        nvout = nout = 0 ;                  /* num output nodes & voxels */
        for( ii=0 ; ii < nvox ; ii++ ){
          jj = vlist[ii] ; if( jj < 0 ) continue ;   /* skip voxel? */
          r = ar_fim[3*jj]; g = ar_fim[3*jj+1]; b = ar_fim[3*jj+2];
          if( r == 0 && g ==0 && b == 0 ) continue ; /* uncolored */
          nlist = adset->su_vnlist[ks]->nlist[ii] ;  /* list of nodes */
          for( nn=0 ; nn < numnod[ii] ; nn++ ){      /* loop over nodes */
            mmm[nout].id = ixyz[nlist[nn]].id ;
            mmm[nout].r  = r ; mmm[nout].g = g ;
            mmm[nout].b  = b ; mmm[nout].a = 255 ; nout++ ;
          }
          nvout++ ;                           /* number of voxels used */
        }
      }
      break ;

      case MRI_short:{
        short * ar_fim = MRI_SHORT_PTR(im_fim) ;
        float fim_thr[NPANE_MAX] ;
        byte r,g,b ;

        if( !bm ){                         /* indexed colors from panes */
          for( lp=0 ; lp < num_lp ; lp++ ) /* thresholds for each pane */
            fim_thr[lp] = (scale_factor/scale_fim) * pbar->pval[lp+1] ;
        }

        nvout = nout = 0 ;                   /* num output nodes & voxels */
        for( ii=0 ; ii < nvox ; ii++ ){
          jj = vlist[ii] ; if( jj < 0 ) continue ;  /* skip voxel? */
          if( ar_fim[jj] == 0 )         continue ;  /* no func? */
          if( !bm ){              /* find pane this voxel is in */
            for( lp=0; lp < num_lp && ar_fim[jj] < fim_thr[lp]; lp++ ) ; /*nada*/
            if( fim_ovc[lp] == 0 ) continue ;         /* uncolored pane */
            r = ovc_r[lp]; g = ovc_g[lp]; b = ovc_b[lp];
          } else {                /* colorscale - 02 Feb 2003 */
            if( zbot && ar_fim[jj] < 0 ) continue ;
            val = ffac*(ftop-ar_fim[jj]) ;
            if( val < 0.0 ) val = 0.0;
            kk = (int)(val+0.49); if( kk >= NPANE_BIG ) kk = NPANE_BIG-1;
            r = cmap[kk].r; g = cmap[kk].g; b = cmap[kk].b;
            if( r == 0 && g ==0 && b == 0 ) continue ; /* black == uncolored */
          }
          nlist = adset->su_vnlist[ks]->nlist[ii] ; /* list of nodes */
          for( nn=0 ; nn < numnod[ii] ; nn++ ){     /* loop over nodes */
            mmm[nout].id = ixyz[nlist[nn]].id ;
            mmm[nout].r  = r ; mmm[nout].g = g ;
            mmm[nout].b  = b ; mmm[nout].a = 255 ; nout++ ;
          }
          nvout++ ;                           /* number of voxels used */
        }
      }
      break ;

      case MRI_byte:{
        byte * ar_fim = MRI_BYTE_PTR(im_fim) ;
        float fim_thr[NPANE_MAX] ;
        byte r,g,b ;

        if( !bm ){                         /* indexed colors from panes */
          for( lp=0 ; lp < num_lp ; lp++ )
            if( pbar->pval[lp+1] <= 0.0 )
              fim_thr[lp] = 0 ;
            else
              fim_thr[lp] = (scale_factor/scale_fim) * pbar->pval[lp+1] ;
        }

        nvout = nout = 0 ;                          /* num output nodes */
        for( ii=0 ; ii < nvox ; ii++ ){
          jj = vlist[ii] ; if( jj < 0 ) continue ;  /* skip voxel? */
          if( ar_fim[jj] == 0 )         continue ;  /* no func? */
          if( !bm ){              /* find pane this voxel is in */
            for( lp=0; lp < num_lp && ar_fim[jj] < fim_thr[lp]; lp++ ) ; /*nada*/
            if( fim_ovc[lp] == 0 ) continue ;         /* uncolored pane */
            r = ovc_r[lp]; g = ovc_g[lp]; b = ovc_b[lp];
          } else {                /* colorscale - 02 Feb 2003 */
            val = ffac*(ftop-ar_fim[jj]) ;
            if( val < 0.0 ) val = 0.0;
            kk = (int)(val+0.49); if( kk >= NPANE_BIG ) kk = NPANE_BIG-1;
            r = cmap[kk].r; g = cmap[kk].g; b = cmap[kk].b;
            if( r == 0 && g ==0 && b == 0 ) continue ; /* black == uncolored */
          }
          nlist = adset->su_vnlist[ks]->nlist[ii] ; /* list of nodes */
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

        if( !bm ){                         /* indexed colors from panes */
          for( lp=0 ; lp < num_lp ; lp++ )
            fim_thr[lp] = (scale_factor/scale_fim) * pbar->pval[lp+1] ;
        }

        nvout = nout = 0 ;                          /* num output nodes */
        for( ii=0 ; ii < nvox ; ii++ ){
          jj = vlist[ii] ; if( jj < 0 ) continue ;  /* skip voxel? */
          if( ar_fim[jj] == 0.0 )       continue ;  /* no func? */
          if( !bm ){              /* find pane this voxel is in */
            for( lp=0; lp < num_lp && ar_fim[jj] < fim_thr[lp]; lp++ ) ; /*nada*/
            if( fim_ovc[lp] == 0 ) continue ;         /* uncolored pane */
            r = ovc_r[lp]; g = ovc_g[lp]; b = ovc_b[lp];
          } else {                /* colorscale - 02 Feb 2003 */
            if( zbot && ar_fim[jj] < 0.0 ) continue ;
            val = ffac*(ftop-ar_fim[jj]) ;
            if( val < 0.0 ) val = 0.0;
            kk = (int)(val+0.49); if( kk >= NPANE_BIG ) kk = NPANE_BIG-1;
            r = cmap[kk].r; g = cmap[kk].g; b = cmap[kk].b;
            if( r == 0 && g ==0 && b == 0 ) continue ; /* black == uncolored */
          }
          nlist = adset->su_vnlist[ks]->nlist[ii] ; /* list of nodes */
          for( nn=0 ; nn < numnod[ii] ; nn++ ){     /* loop over nodes */
            mmm[nout].id = ixyz[nlist[nn]].id ;
            mmm[nout].r  = r ; mmm[nout].g = g ;
            mmm[nout].b  = b ; mmm[nout].a = 255 ; nout++ ;
          }
          nvout++ ;                           /* number of voxels used */
        }
      }
      break ;

   } /* end of switch on fim data type */

   /** finished: clean up and exit **/

   if( vlist != voxijk ) free(vlist) ;  /* toss trash, if it is trash */

   if( nout == 0 ){ free(mmm); RETURN(0); }  /* no overlay? */

   /* map gets the array of node IDs + colors */

   *map = (SUMA_irgba *) realloc( mmm , sizeof(SUMA_irgba)*nout ) ;

   /* nvused gets the number of voxels used */

   if( nvused != NULL ) *nvused = nvout ;    /* 13 Mar 2002 */

   RETURN(nout) ;  /* number of entries in map */
}

/*-----------------------------------------------------------------------*/
/*! Find node in surface closest to given DICOM vector, with limitation
    that node's x is in range xbot..xtop, etc.  Return value is node
    index into ixyz array (not necessarily node ID), -1 if none is found.
-------------------------------------------------------------------------*/

int AFNI_find_closest_node( int num_ixyz , SUMA_ixyz *ixyz ,
                            float xtarg, float ytarg, float ztarg,
                            float xbot , float xtop ,
                            float ybot , float ytop ,
                            float zbot , float ztop  )
{
   int ii ,      ibest=-1 ;
   float x,y,z , dbest, d ;

ENTRY("AFNI_find_closest_node") ;

   if( num_ixyz <= 0 || ixyz == NULL ) RETURN(-1) ;  /* bad inputs */

   /* if search ranges are incoherent, make them very wide */

   if( xbot >= xtop ){ xbot = -WAY_BIG; xtop = WAY_BIG; }
   if( ybot >= ytop ){ ybot = -WAY_BIG; ytop = WAY_BIG; }
   if( zbot >= ztop ){ zbot = -WAY_BIG; ztop = WAY_BIG; }

   for( ii=0 ; ii < num_ixyz ; ii++ ){
     x = ixyz[ii].x; y = ixyz[ii].y; z = ixyz[ii].z;
     if( x < xbot || x > xtop ||
         y < ybot || y > ytop ||
         z < zbot || z > ztop   ) continue ;  /* outside box */

     d = (xtarg-x)*(xtarg-x) + (ytarg-y)*(ytarg-y) + (ztarg-z)*(ztarg-z) ;
     if( ibest < 0 || d < dbest ){ ibest = ii; dbest = d; }
   }

   RETURN(ibest) ;
}

/*---------------------------------------------------------------------------*/
/*-------- Stuff below here is for surface control panel [19 Aug 2002] ------*/

static void AFNI_surf_done_CB( Widget,XtPointer,XtPointer ) ;
static void AFNI_surf_redraw_CB( MCW_arrowval *,XtPointer ) ;
static AFNI_make_surface_widgets( Three_D_View *, int ) ;
static void AFNI_surf_bbox_CB( Widget,XtPointer,XtPointer ) ; /* 19 Feb 2003 */

/*---------------------------------------------------------------------------*/
/*! Make the widgets for one row of the surface control panel.
    The row itself will not be managed at this time; that comes later. */

#define MAKE_SURF_ROW(ii)                                          \
 do{ Widget rc ; char *str[1]={"abcdefghijklmn: "} ;               \
     rc = swid->surf_rc[ii] =                                      \
         XtVaCreateWidget(                                         \
           "dialog" , xmRowColumnWidgetClass , swid->rowcol ,      \
              XmNpacking      , XmPACK_TIGHT ,                     \
              XmNorientation  , XmHORIZONTAL   ,                   \
              XmNtraversalOn , False ,                             \
           NULL ) ;                                                \
     swid->surf_bbox[ii] = new_MCW_bbox( rc , 1 , str ,            \
                             MCW_BB_check, MCW_BB_noframe,         \
                             AFNI_surf_bbox_CB , im3d ) ;          \
     MCW_set_bbox( swid->surf_bbox[ii] , 1 ) ;                     \
     MCW_reghelp_children( swid->surf_bbox[ii]->wrowcol ,          \
                           "Use this toggle to turn the\n"         \
                           "overlay drawing for this surface\n"    \
                           "off and back on."                  ) ; \
     swid->surf_node_av[ii] = new_MCW_colormenu( rc ,              \
                               "Nodes" , im3d->dc ,                \
                               0 , im3d->dc->ovc->ncol_ov-1 , 0 ,  \
                               AFNI_surf_redraw_CB , im3d ) ;      \
     swid->surf_line_av[ii] = new_MCW_colormenu( rc ,              \
                               "Lines" , im3d->dc ,                \
                               0 , im3d->dc->ovc->ncol_ov-1 , 1 ,  \
                               AFNI_surf_redraw_CB , im3d ) ;      \
     MCW_reghint_children( swid->surf_node_av[ii]->wrowcol ,       \
                           "Color of node boxes" ) ;               \
     MCW_reghelp_children( swid->surf_node_av[ii]->wrowcol ,       \
                           "If this is not 'none', then\n"         \
                           "a box will be drawn around the\n"      \
                           "location of each surface node\n"       \
                           "inside the slice volume."        ) ;   \
     MCW_reghint_children( swid->surf_line_av[ii]->wrowcol ,       \
                           "Color of triangle lines" ) ;           \
     MCW_reghelp_children( swid->surf_line_av[ii]->wrowcol ,       \
                           "If this is not 'none', then\n"         \
                           "line segments will be drawn for\n"     \
                           "the intersection of each surface\n"    \
                           "triangle with the slice plane."    ) ; \
  } while(0)

/*------------------------------------------------------------------------*/
/*! Make surface widgets for an AFNI controller [19 Aug 2002].
--------------------------------------------------------------------------*/

static AFNI_make_surface_widgets( Three_D_View *im3d, int num )
{
   AFNI_surface_widgets *swid ;
   Widget ww , rc ;
   XmString xstr ;
   char str[32] ;
   int ii ;

   im3d->vwid->view->swid = swid = myXtNew( AFNI_surface_widgets ) ;

   /* shell to hold it all */

   sprintf(str,"AFNI Surface Controls %s",AFNI_controller_label(im3d)) ;

   swid->wtop = XtVaAppCreateShell(
                  "AFNI" , "AFNI" ,
                   topLevelShellWidgetClass , im3d->dc->display ,
                   XmNallowShellResize   , True ,
                   XmNtitle              , str ,
                   XmNmappedWhenManaged  , False ,              /* manage manually */
                   XmNdeleteResponse     , XmDO_NOTHING ,       /* deletion handled below */
                NULL ) ;
   DC_yokify( swid->wtop , im3d->dc ) ;

   XtVaSetValues( swid->wtop ,
                     XmNmwmDecorations , MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE ,
                  NULL ) ;

   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           swid->wtop ,
           XmInternAtom( im3d->dc->display , "WM_DELETE_WINDOW" , False ) ,
           AFNI_surf_done_CB , im3d ) ;

   /* vertical rowcol to hold it all */

   swid->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , swid->wtop ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , False ,
         NULL ) ;

   /* top label to say what dataset we are dealing with */

   xstr = XmStringCreateLtoR( "xxxxxxxxxAxxxxxxxxxAxxxxxxxxxAxxxxxxxxxAxxxxxxxxxA [x] " ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   swid->top_lab = XtVaCreateManagedWidget(
                    "dialog" , xmLabelWidgetClass , swid->rowcol ,
                       XmNrecomputeSize , False ,
                       XmNlabelString , xstr ,
                       XmNtraversalOn , False ,
                    NULL ) ;
   XmStringFree(xstr) ;

   /* Separator from other widgets */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,swid->rowcol,
                                      XmNseparatorType   , XmSHADOW_ETCHED_IN ,
                                      XmNshadowThickness , 5 ,
                                   NULL ) ;

   /* horiz rowcol for top controls [23 Feb 2003] */

   rc = XtVaCreateWidget(
          "dialog" , xmRowColumnWidgetClass , swid->rowcol ,
             XmNpacking      , XmPACK_TIGHT ,
             XmNorientation  , XmHORIZONTAL   ,
             XmNtraversalOn , False ,
          NULL ) ;

   /* boxsize control [23 Feb 2003] */

   swid->boxsize_av = new_MCW_optmenu( rc , "BoxSize" ,
                                       1,19,2,0 ,
                                       AFNI_surf_redraw_CB , im3d ,
                                       NULL , NULL ) ;
   MCW_reghint_children( swid->boxsize_av->wrowcol ,
                         "Size of boxes drawn at nodes" ) ;
   MCW_reghelp_children( swid->boxsize_av->wrowcol ,
                         "This sets the size of the\n"
                         "boxes used to draw the\n"
                         "surface nodes that are\n"
                         "in the current slice volume." ) ;

   /* linewidth control [23 Feb 2003] */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass, rc ,
                                      XmNorientation   , XmVERTICAL    ,
                                      XmNseparatorType , XmDOUBLE_LINE ,
                                   NULL ) ;

   swid->linewidth_av = new_MCW_optmenu( rc , "LineWidth" ,
                                         0,19,0,0 ,
                                         AFNI_surf_redraw_CB , im3d ,
                                         NULL , NULL ) ;
   MCW_reghint_children( swid->linewidth_av->wrowcol ,
                         "Width of lines drawn for surface" ) ;
   MCW_reghelp_children( swid->linewidth_av->wrowcol ,
                         "This sets the thickness of\n"
                         "the lines used to draw the\n"
                         "intersection of the surface\n"
                         "with the slice plane."         ) ;


   /* Done button */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass, rc ,
                                      XmNorientation   , XmVERTICAL    ,
                                      XmNseparatorType , XmDOUBLE_LINE ,
                                   NULL ) ;

   xstr = XmStringCreateLtoR( "Done" , XmFONTLIST_DEFAULT_TAG ) ;
   swid->done_pb =
     XtVaCreateManagedWidget(
           "dialog" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , False ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( swid->done_pb, XmNactivateCallback, AFNI_surf_done_CB, im3d );
   MCW_set_widget_bg( swid->done_pb, MCW_hotcolor(swid->done_pb), 0 ) ;
   MCW_register_hint( swid->done_pb, "Close window" ) ;

   XtManageChild(rc) ;

   /* Separator from other widgets */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,swid->rowcol,
                                      XmNseparatorType   , XmSHADOW_ETCHED_IN ,
                                      XmNshadowThickness , 5 ,
                                   NULL ) ;

   /* Now create rows of widgets to control the surfaces */

   swid->nall = num ;
   swid->nrow = 0 ;     /* none are managed at this time */

   swid->surf_rc      = (Widget *)        XtCalloc( num , sizeof(Widget)         ) ;
   swid->surf_bbox    = (MCW_bbox **)     XtCalloc( num , sizeof(MCW_bbox *)     ) ;
   swid->surf_node_av = (MCW_arrowval **) XtCalloc( num , sizeof(MCW_arrowval *) ) ;
   swid->surf_line_av = (MCW_arrowval **) XtCalloc( num , sizeof(MCW_arrowval *) ) ;

   for( ii=0 ; ii < num ; ii++ ){
      MAKE_SURF_ROW(ii) ;
   }

   XtManageChild(swid->rowcol) ;
   XtRealizeWidget(swid->wtop) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/
/*! Update surface widgets for this controller. */

void AFNI_update_surface_widgets( Three_D_View *im3d )
{
   AFNI_surface_widgets *swid ;
   int num , ii , nwid,nall ;
   char str[64] , nam[THD_MAX_NAME] , *tnam ;

ENTRY("AFNI_update_surface_widgets") ;

   if( !IM3D_OPEN(im3d) || im3d->anat_now == NULL ) EXRETURN ;

   num  = im3d->anat_now->su_num ;  /* # of surfaces */
   swid = im3d->vwid->view->swid ;

   SENSITIZE( im3d->vwid->view->choose_surf_pb , (Boolean)(num > 0) ) ;

   if( swid == NULL ) EXRETURN ;

   /* put dataset label in top of panel */

   strcpy( nam , im3d->anat_now->dblk->diskptr->directory_name ) ;
   strcat( nam , im3d->anat_now->dblk->diskptr->filecode ) ;
   tnam = THD_trailname(nam,SESSTRAIL+1) ;
   ii = strlen(tnam) ; if( ii > 50 ) tnam += (ii-50) ;
   sprintf(str ,"%-.50s %s" , tnam, AFNI_controller_label(im3d) ) ;
   MCW_set_widget_label( swid->top_lab , str ) ;

   /* make more widget rows? (1 per surface is needed) */

   if( swid->nall < num ){
     swid->surf_rc      = (Widget *)        XtRealloc( (char *)swid->surf_rc     ,num*sizeof(Widget)         );
     swid->surf_bbox    = (MCW_bbox **)     XtRealloc( (char *)swid->surf_bbox   ,num*sizeof(MCW_bbox *)     );
     swid->surf_node_av = (MCW_arrowval **) XtRealloc( (char *)swid->surf_node_av,num*sizeof(MCW_arrowval *) );
     swid->surf_line_av = (MCW_arrowval **) XtRealloc( (char *)swid->surf_line_av,num*sizeof(MCW_arrowval *) );
     for( ii=swid->nall ; ii < num ; ii++ ){
       MAKE_SURF_ROW(ii) ;
     }
     swid->nall = num ;
   }

   /* map or unmap widget rows? (1 per surface is needed) */

   if( swid->nrow < num ){
     for( ii=swid->nrow ; ii < num ; ii++ )
       XtManageChild( swid->surf_rc[ii] ) ;
   } else if( swid->nrow > num ){
      for( ii=num ; ii < swid->nrow ; ii++ )
       XtUnmanageChild( swid->surf_rc[ii] ) ;
   }
   swid->nrow = num ;  /* # of managed rows */

   /* change labels for each row */

   for( ii=0 ; ii < num ; ii++ ){
     sprintf(str,"%-14.14s: ",im3d->anat_now->su_surf[ii]->label) ;
     MCW_set_widget_label( swid->surf_bbox[ii]->wbut[0] , str ) ;

     sprintf(str,"%d Nodes; %d Triangles",               /* 20 Aug 2002:  */
             im3d->anat_now->su_surf[ii]->num_ixyz ,     /* put a hint    */
             im3d->anat_now->su_surf[ii]->num_ijk   ) ;  /* on each label */
     MCW_register_hint( swid->surf_bbox[ii]->wbut[0] , str ) ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------------*/
/*! Update all surface widgets everywhere that touch this dataset. */

void AFNI_update_all_surface_widgets( THD_3dim_dataset *dset )
{
   int ii ;
   Three_D_View *im3d ;
ENTRY("AFNI_update_all_surface_widgets") ;
   if( !ISVALID_DSET(dset) ) EXRETURN ;
   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
     im3d = GLOBAL_library.controllers[ii] ;
     if( IM3D_OPEN(im3d) && im3d->anat_now == dset )
       AFNI_update_surface_widgets( im3d ) ;
   }
   EXRETURN ;
}

/*------------------------------------------------------------------------*/
/*! Callback for Switch Surface pushbutton [19 Aug 2002].
--------------------------------------------------------------------------*/

void AFNI_choose_surface_CB( Widget w , XtPointer cd, XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   AFNI_surface_widgets *swid ;
   int num , ii , nwid,nall ;

ENTRY("AFNI_choose_surface_CB") ;

   if( !IM3D_OPEN(im3d) || im3d->anat_now == NULL ) EXRETURN ;

   num  = im3d->anat_now->su_num ;
   swid = im3d->vwid->view->swid ;

   /* no surfaces ==> nothing to do */

   if( num == 0 ) EXRETURN ;

   /* make widgets? */

   if( swid == NULL ){
     AFNI_make_surface_widgets(im3d,num) ;
     swid = im3d->vwid->view->swid ;
   }

   /* make control panel visible, if it isn't already */

   XtMapWidget( swid->wtop ) ;
   XRaiseWindow( XtDisplay(swid->wtop), XtWindow(swid->wtop) ) ;

   /* put proper labels on widgets, etc. */

   AFNI_update_surface_widgets(im3d) ;

   /* wait for user to press some button */

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Callback for "Done" button for surface control panel. */

static void AFNI_surf_done_CB( Widget w , XtPointer cd, XtPointer cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   AFNI_surface_widgets *swid ;

ENTRY("AFNI_surf_done_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   swid = im3d->vwid->view->swid ;
   if( swid != NULL ) XtUnmapWidget( swid->wtop ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Callback for press of an arrowval on the surface controls.
   All this does is to redraw the images, since that is where
   the actual info will be grabbed from the swid arrowvals.  */

static void AFNI_surf_redraw_CB( MCW_arrowval * av , XtPointer cd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   AFNI_surface_widgets *swid ;

ENTRY("AFNI_surf_redraw_CB") ;

   if( im3d == NULL ) EXRETURN ;
   swid = im3d->vwid->view->swid ;
   if( swid == NULL ) EXRETURN ;  /* should be impossible */

   if( !IM3D_OPEN(im3d) ){
     XtUnmapWidget( swid->wtop ) ;
     EXRETURN ;
   }

   AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Callback for press of a toggle button on the surface controls.
   All this does is to force a redraw of the images.               */

static void AFNI_surf_bbox_CB( Widget w , XtPointer cd , XtPointer qd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   AFNI_surface_widgets *swid ;

ENTRY("AFNI_surf_bbox_CB") ;

   if( im3d == NULL ) EXRETURN ;
   swid = im3d->vwid->view->swid ;
   if( swid == NULL ) EXRETURN ;  /* should be impossible */

   if( !IM3D_OPEN(im3d) ){
     XtUnmapWidget( swid->wtop ) ;
     EXRETURN ;
   }

   AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;

   EXRETURN ;
}
