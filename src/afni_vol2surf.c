/*----------------------------------------------------------------------
 * afni_vol2surf.c   - afni's interface to the vol2surf library
 *
 * AFNI_vol2surf_func_overlay() - similar to AFNI_vnlist_func_overlay()
 *----------------------------------------------------------------------
 */

/*----------------------------------------------------------------------
 * R. Reynolds    September, 2004
 *
 * history:
 *
 * 08 Oct 2004 [rickr]:
 *   - AFNI_vol2surf_func_overlay() has new params, surfA,surfB,use_defaults
 *   - pass use_defaults to afni_vol2surf()
 * 25 Oct 2004 [rickr]:
 *   - accept Rdata and Rthr pointers, for optionally returning the data
 *     and global threshold
 *   - make threshold masks absolute
 * 09 Aug 2006 [rickr]:
 *  - set the surface volume dataset in the global v2s_plugin_opts struct
 *  - also store the index and threshold value of the threshold sub-brick
 *----------------------------------------------------------------------
 */

#include "afni.h"
#include "vol2surf.h"

static int map_v2s_results(v2s_results *res, Three_D_View *im3d,
			   SUMA_irgba **map, int debug, int dnode );
/*-----------------------------------------------------------------------*/
/*! Create a nodal color overlay from a voxel map.
    - Surface index(es) come from global v2s_plugin_opts struct
    - Return value is number of nodes overlaid:
      -  0 return ==> no overlay was computed
      - -1 return ==> some error (e.g., no surface nodes on this dataset)
      = positive  ==> can use *map
    - *map is set to a newly malloc()-ed array (if return > 0)
    - if (Rdata) return data from results structure
    - if (Rthr) return overlay threshold from afni

    Sample usage:
    - SUMA_irgba * map;
    - int          nmap;
    - nmap = AFNI_vnlist_func_overlay( im3d, &map );
    -      if( nmap <  0 ){ ** error ** }
    - else if( nmap == 0 ){ ** nothing to show ** }
    - else                { ** show map[0..nmap-1] ** }

    * based on AFNI_vnlist_func_overlay()
-------------------------------------------------------------------------*/
int AFNI_vol2surf_func_overlay(Three_D_View *im3d, SUMA_irgba **map,
         int surfA, int surfB, int use_defaults, float ** Rdata, float * Rthr )
{
    THD_3dim_dataset * oset;		/* overlay dataset */
    THD_slist_find     find;
    THD_session      * ss;
    SUMA_surface     * sA, * sB;
    MRI_IMAGE        * im_oim;
    v2s_plugin_opts  * go;
    v2s_results      * results;
    byte             * cmask;
    int                nout, oind, debug;

ENTRY("AFNI_vol2surf_func_overlay") ;

    /* check inputs */
    if ( map == NULL || !IM3D_VALID(im3d) ) RETURN(-1);

    go = &gv2s_plug_opts;
    if ( ! use_defaults && ! go->ready ) RETURN(-1);

    debug = go->sopt.debug;	    /* because I'm lazy */

    ss = im3d->ss_now;              /* session must have needed surface(s) */
    if( ss                 == NULL      ||
        ss->su_num         <= 0         ||
        surfA              <  0         ||
        ss->su_num         <= surfA     ||
        ss->su_num         <= surfB     ||
        ss->su_surf[surfA] == NULL      ||
        (surfB >= 0 && ss->su_surf[surfB] == NULL) )
    {
	if ( debug > 1 )
	{
	    if ( !ss ) fprintf(stderr,"** v2s: NULL session\n");
	    else
		fprintf(stderr,"** v2s: bad session data:\n"
			"   su_num, surfA, surfB = %d, %d, %d\n",
			ss->su_num, surfA, surfB);
	}
	RETURN(-1);
    }

    /* init return values */
    if ( Rdata ) *Rdata = NULL;
    if ( Rthr )  *Rthr  = 0.0;

    /* set surface pointers */
    sA = ss->su_surf[surfA];
    sB = ( surfB >= 0 ) ? ss->su_surf[surfB] : NULL;

    /* store the surface volume pointer            9 Aug 2006 [rickr] */
    find = PLUTO_dset_finder(sA->idcode_dset);
    go->sv_dset = find.dset;

    if ( debug )
    {
	fprintf(stderr,"++ v2s overlay: sa,sb = %d,%d\n", surfA, surfB);
	if ( debug > 1 )
	    fprintf(stderr,"  surfA is %s, surfB is %s\n",
		sA->label[0] ? sA->label : "<no label>",
		sB ? (sB->label[0] ? sB->label : "<no label>") : "<not used>");
    }

    /*-------------------- overlay image --------------------*/
    oset = im3d->fim_now;
    oind = im3d->vinfo->fim_index;  /* overlay sub-brick index */

    if( oset == NULL )
    {
	if ( debug > 1 ) fprintf(stderr,"** v2s: no overlay dset\n");
	RETURN(-1);
    }
    im_oim = DSET_BRICK(oset,oind);

    if( ! im_oim || ! AFNI_GOOD_FUNC_DTYPE(im_oim->kind) )
    {
	if ( debug > 1 ) fprintf(stderr,"** v2s: no overlay image\n");
	RETURN(-1);
    }
    DSET_load(oset);			/* to be sure */
    if( !DSET_LOADED(oset) ) RETURN(-1);

    /*-------------------- mask from threshold --------------------*/
    go->gpt_index = -1; go->gpt_thresh = 0.0;   /* init threshold options */
    cmask = NULL;

    /* if a new overlay has been created from clustering, get it as a mask
       (for now - since it's in MRI_IMAGE, and we can't pass to v2s)
                                                       11 Jan 2008 [rickr] */

    if( debug ) {
        fprintf(stderr,"-- CLUST: ");
        if( ! im3d->vinfo )
            fprintf(stderr,"no vinfo to cluster on\n");
        else if( ! oset->dblk->vedim )
            fprintf(stderr,"no dblk->vedim to cluster on\n");
        else {
            fprintf(stderr,"fim_index %d, thr_index %d, VEDIT_IVAL %d\n",
                   im3d->vinfo->fim_index,
                   im3d->vinfo->thr_index,
                   DSET_VEDIT_IVAL(oset));
            fprintf(stderr,"   (kind %d, brick type %d)\n",
                   oset->dblk->vedim->kind, DSET_BRICK_TYPE(oset,oind));
        }
    }

    if( im3d->vinfo && oset->dblk->vedim                      &&
            (im3d->vinfo->fim_index == DSET_VEDIT_IVAL(oset)) &&
            (oset->dblk->vedim->kind == DSET_BRICK_TYPE(oset,oind)) ) {
        cmask = mri_to_bytemask(oset->dblk->vedim, 1.0, 0.0);
        if( debug > 1 ) fprintf(stderr,"++ applying mask from edited image\n");
    }
    else if( im3d->vinfo->func_threshold > 0.0 &&
             im3d->vinfo->thr_onoff )	            /* then want a threshold */
    {
	MRI_IMAGE * im_thr;
        float       thresh;
	int         tind = im3d->vinfo->thr_index;

	im_thr = DSET_BRICK(oset,tind);

 	/* note real threshold */
        thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top;

        /* maybe we want to return this */
        if ( Rthr ) *Rthr = thresh;

	if( im_thr && !AFNI_GOOD_FUNC_DTYPE(im_thr->kind) )
	    im_thr = NULL;

	/* create the mask, if this fails, just continue... */
	if( im_thr != NULL )
	{
	    int nset;
	    if ( debug > 1 )
		fprintf(stderr,"++ mask from index %d and thresh %f\n",
			tind,thresh);
	    nset = thd_mask_from_brick(oset, tind, thresh, &cmask, 1);
	    if ( debug > 1 )
	    {
		if ( ! cmask )
		    fprintf(stderr,"-- no mask created\n");
		else
		    fprintf(stderr,"++ mask has %d set voxels\n", nset);
	    }
	}
	else if ( debug > 1 )
	    fprintf(stderr,"-- no threshold mask\n");

        /* note mask options for v2s command output  9 Aug 2006 [rickr] */
        if( cmask ) { go->gpt_index = tind; go->gpt_thresh = thresh; }
    } else      /* then make the mask from non-zero overlay values */
        cmask = THD_makemask(oset, oind, 1.0, 0.0);

    /*-------------------- vol2surf computation --------------------*/
    results = afni_vol2surf(oset, oind, sA, sB, cmask, use_defaults);

    if ( cmask ) free(cmask);	/* we're done with the mask */
    if ( ! results )
    {
	if ( debug ) fprintf(stderr,"-- vol2surf failure\n");
	RETURN(-1);		/* failure? */
    }

    /*-------------------- lose old vnlist --------------------*/
    /* vol2surf has no method for counting voxels (right now)  */
    if( sA->vn )
    {
	if ( debug > 1 ) fprintf(stderr,"-- destroying vnlist\n");
	SUMA_destroy_vnlist( sA->vn ) ;
	sA->vn = NULL;
    }

    /*-------------------- set overlay colors --------------------*/
    nout = map_v2s_results(results, im3d, map, debug, go->sopt.dnode);

    /* before free()ing results, check whether we want to return the values */
    if ( Rdata )
    {
	*Rdata = results->vals[0];
	results->vals[0] = NULL;   /* do not let free_v2s_results() free it */
    }

    free_v2s_results(results);

    if ( debug > 1 ) fprintf(stderr,"++ map_v2s_results: nout = %d\n", nout);

    /* check failure */
    if ( ! *map ) RETURN(-1);

    RETURN(nout);
}

/*! given vol2surf results, fill SUMA_irgba struct */
static int map_v2s_results(v2s_results *res, Three_D_View *im3d,
			   SUMA_irgba **map, int debug, int dnode )
{
    SUMA_irgba * mptr;
    MCW_pbar   * pbar;
    rgbyte     * cmap;
    float      * result_vals;
    float        bbot, btop, fac, fval;
    float        pane_scale = 1.0;
    byte         r, g, b;
    int          nindex, node, npanes, ival;
    int          map_index = 0;  /* counter for results */

ENTRY("map_v2s_results");

    /*-------------------- create output node list --------------------*/
    /* from malloc    12 Feb 2009 [lesstif patrol] */
    mptr = (SUMA_irgba *) calloc(res->nused, sizeof(SUMA_irgba));
    if ( ! mptr )
    {
	fprintf(stderr,"** malloc failure: %d irgba's\n", res->nused);
	RETURN(-1);
    }
    *map = mptr;  /* and store the address */

    pbar       = im3d->vwid->func->inten_pbar;
    npanes     = pbar->num_panes;
    pane_scale = im3d->vinfo->fim_range;
    if ( pane_scale == 0.0 ) pane_scale = im3d->vinfo->fim_autorange;
    if ( pane_scale == 0.0 ) pane_scale = 1.0;

    if ( debug > 1 )
	fprintf(stderr,"+d mvr: npanes = %d, pane_scale = %f\n",
		pbar->bigmode ? NPANE_BIG : npanes, pane_scale);

    result_vals = res->vals[0];	/* for typing and potential speed */

    if( pbar->bigmode )		/* colorscale */
    {
	int zbot;

	bbot = pane_scale*pbar->bigbot;
	btop = pane_scale*pbar->bigtop;
	fac  = NPANE_BIG / (btop-bbot);
	cmap = pbar->bigcolor;
	zbot = (bbot == 0.0);

	if ( debug > 1 )
	    fprintf(stderr,"+d bigmode: bbot,btop,fac, zbot = %f,%f,%f, %d\n",
		    bbot, btop, fac, zbot);

	for ( nindex = 0; nindex < res->nused; nindex++ )
	{
	    if ( res->nodes ) node = res->nodes[nindex];
	    else              node = nindex;

	    /* note value, and check to ignore */
	    fval = result_vals[nindex];

	    if ( debug > 1 && node == dnode )
		fprintf(stderr, "+d dnode %d, fval %f\n", dnode, fval);

	    if ( zbot && fval < 0 ) continue;

	    /* note the color panel index, and bound it in [0,NPANE_BIG-1] */
            if( fval >= btop ) ival = 0;        /* guard against overflow */
            else if ( fval <= bbot ) ival = NPANE_BIG - 1;
            else ival = (int)(fac * (btop - fval) + 0.49);

	    if ( ival < 0 ) ival = 0;
	    if ( ival >= NPANE_BIG ) ival = NPANE_BIG - 1;

	    /* get color map, and check if non-zero */
	    r = cmap[ival].r;  g = cmap[ival].g;  b = cmap[ival].b;

	    if ( debug > 1 && node == dnode )
		fprintf(stderr, "+d pane, r,g,b = %d, %d,%d,%d\n",ival,r,g,b);

	    if ( r == 0 && g == 0 && b == 0 ) continue;

	    /* we are set, fill the SUMA_irgba struct  */
	    mptr->id = res->nodes[nindex];
	    mptr->r = r;  mptr->g = g;  mptr->b = b;  mptr->a = 255;

	    /* increment pointer and counter (okay, so counter is unneeded) */
	    mptr++;  map_index++;
	}
    }
    else        /* indexed colors */
    {
	float othr[NPANE_MAX];	/* threshold */
	short ovc[NPANE_MAX+1];	/* color */
	byte  ovc_r[NPANE_MAX+1], ovc_g[NPANE_MAX+1], ovc_b[NPANE_MAX+1];

	/* set the overlay color indices */
	for( ival=0 ; ival < npanes ; ival++ )
	    ovc[ival] = pbar->ov_index[ival];   /* from top of pbar down */
	ovc[npanes] = im3d->vinfo->use_posfunc ? 0 : ovc[npanes-1];

	/* get the actual RGB colors of each pane on the pbar */
        for( ival=0 ; ival <= npanes ; ival++ ) /* include npanes 27 Aug 2007 */
	{
	    ovc_r[ival] = DCOV_REDBYTE  (im3d->dc,ovc[ival]);
	    ovc_g[ival] = DCOV_GREENBYTE(im3d->dc,ovc[ival]);
	    ovc_b[ival] = DCOV_BLUEBYTE (im3d->dc,ovc[ival]);
	}

	/* compute the thresholds */
	for( ival=0 ; ival < npanes ; ival++ )
	    othr[ival] = pane_scale * pbar->pval[ival+1];

	if ( debug > 2 )
	{
	    for( ival=0 ; ival <= npanes ; ival++ )
		fprintf(stderr,"+d pane #%2d, ovc = %d, othr = %f\n",
			ival, ovc[ival], othr[ival]);
	}

	for ( nindex = 0; nindex < res->nused; nindex++ )
	{
	    if ( res->nodes ) node = res->nodes[nindex];
	    else              node = nindex;

	    /* note value, and check to ignore */
	    fval = result_vals[nindex];

	    if ( debug > 1 && node == dnode )
		fprintf(stderr, "+d dnode %d, fval %f\n", dnode, fval);

	    if ( fval == 0.0 ) continue;

	    for ( ival = 0; ival < npanes && fval < othr[ival]; ival++ )
		;
	    if ( ovc[ival] == 0 ) continue;  /* no color in this pane */
	    r = ovc_r[ival];  g = ovc_g[ival];  b = ovc_b[ival]; 

            /* we are set, fill the SUMA_irgba struct  */
            mptr->id = res->nodes[nindex];
            mptr->r = r;  mptr->g = g;  mptr->b = b;  mptr->a = 255;

	    if ( debug > 1 && node == dnode )
		fprintf(stderr, "+d pane, r,g,b = %d, %d,%d,%d\n",ival,r,g,b);

	    if ( r == 0 && g == 0 && b == 0 ) continue;

            /* increment pointer and counter (okay, so counter is unneeded) */
            mptr++;  map_index++;
	}
   }

    if ( debug > 0 )
	fprintf(stderr,"+d mvr v2s map size = %d\n", map_index);

   /* forfeit unused memory */
   *map = (SUMA_irgba *)realloc(*map, sizeof(SUMA_irgba)*map_index);
   if ( ! *map )
   {
	fprintf(stderr,"** mvr: failed realloc of map, %d irgba's\n",map_index);
	map_index = -1;
   }

   RETURN(map_index) ;  /* number of entries in map */
}


/*-----------------------------------------------------------------------*/
/*! Return the vol2surf time series data at a single node.
 *
 *      sess    : session to get surfaces from (e.g. im3d->ss_now)
 *      dset    : dataset to get values from (e.g. im3d->anat_now)
 *      surf    : surface index (will use LDP of this surface)
 *                              (e.g. in {0..from im3d->ss_now->su_num-1})
 *      node    : node index in surface
 *
 *  Note: surf/node might come from AFNI_get_xhair_node(), say.
 *
 *  returns: a pointer to the float data or NULL on failure.
 *
 *  Array will be of length DSET_NVALS(dset), and should be free'd by
 *  the calling function.
 * ----------------------------------------------------------------------*/
float * AFNI_v2s_node_timeseries(THD_session * sess, THD_3dim_dataset * dset,
                                 int surfA, int surfB, int node, int use_v2s)
{
   v2s_results     * results = NULL;
   v2s_opts_t        sopt;
   SUMA_surface    * sA, * sB;
   float           * ts = NULL;  /* time series to return */
   int               ind, len, verb = gv2s_plug_opts.sopt.debug;

   ENTRY("AFNI_v2s_node_timeseries");

   if( !sess || !dset || surfA < 0 ){
      fprintf(stderr,"** v2s_node_ts - bad inputs: %p,%p,%d\n",sess,dset,surfA);
      RETURN(NULL);
   }

   sA = (surfA >= 0) ? sess->su_surf[surfA] : NULL;
   sB = (surfB >= 0) ? sess->su_surf[surfB] : NULL;

   /* check and apply the node index */
   if( node < 0 || node >= sA->num_ixyz ) {
      fprintf(stderr,"** v2s_NTS: node %d outside [0,%d)\n",node,sA->num_ixyz);
      RETURN(NULL);
   } else if( verb > 1 )
      fprintf(stderr,"-- v2s_NTS: getting time series at node %d\n", node);

   /* fill options struct (from defaults or plugin opts) */
   if( use_v2s ) {
      /* is using plugin, nuke any gp_index, filenames or structs */
      sopt = gv2s_plug_opts.sopt;

      sopt.gp_index = -1;  /* get all sub-bricks */
      sopt.outfile_1D = sopt.outfile_niml = sopt.segc_file = NULL;
      memset(&sopt.cmd, 0, sizeof(v2s_cmd_t));
      memset(&sopt.oob, 0, sizeof(v2s_oob_t));
      memset(&sopt.oom, 0, sizeof(v2s_oob_t));
   } else
      v2s_fill_sopt_default(&sopt, sB ? 2 : 1);

   sopt.skip_cols = V2S_SKIP_ALL ^ V2S_SKIP_VALS;  /* get all data */
   sopt.first_node = sopt.last_node = node;        /* get only 1 node */
   if( verb > 2 ) sopt.dnode = node;               /* maybe make verbose */

   /* get the results */
   results = opt_vol2surf(dset, &sopt, sA, sB, NULL);
   if( !results ) RETURN(NULL);

   /* extract single time series */
   len = DSET_NVALS(dset);
   if( len != results->nlab ){
      fprintf(stderr,"** v2s_NTS: nvals != nlab (%d, %d)\n",len,results->nlab);
      free_v2s_results(results); RETURN(NULL);
   }

   ts = (float *)malloc(len*sizeof(float));
   if( !ts ) {
      fprintf(stderr,"** v2s_NTS: failed to alloc %d floats\n", len);
      free_v2s_results(results); RETURN(NULL);
   }

   /* actually fill the array, woohoo! */
   if(verb > 3) fprintf(stderr,"-- v2s_NTS: copying %d vals for return\n",len);
   for( ind = 0; ind < len; ind++ )
      ts[ind] = results->vals[ind][0];

   if(verb > 3) fprintf(stderr,"-- v2s_NTS: freeing results... ");
   free_v2s_results(results);

   if(verb > 3) fprintf(stderr,"done\n");
   RETURN(ts);
}

