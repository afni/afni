
/*----------------------------------------------------------------------
 * Exported functions - these are available for use in other functions
 *
 *     r_new_resam_dset            : reorient and/or resamle a dataset
 *     r_fill_resampled_data_brick : warp a dataset into memory
 *     r_dxyz_mod_dataxes          : modify THD_dataxes struct per dxyz
 *----------------------------------------------------------------------
*/

/*----------------------------------------------------------------------
 * history:
 *
 * 2003.01.15
 *   - do not add any history in this library routine
 *
 * 2002.07.29
 *   - if we master, be sure output has same view type
 *
 * 2004.07.26
 *   - added sublist parameter to r_new_resam_dset, as a list of
 *     sub-bricks to resample (all, if NULL)
 *----------------------------------------------------------------------
*/

#include "mrilib.h"
#include "r_misc.h"
#include "r_new_resam_dset.h"

/* local #defines and function definitions */

static int apply_dataxes     ( THD_3dim_dataset * dset, THD_dataxes * dax );
static int apply_orientation ( THD_3dim_dataset * dset, FD_brick * fdb,
			       char orient[] );
static int valid_resam_inputs( THD_3dim_dataset * , THD_3dim_dataset *,
			       double, double, double, char [], int );

#define LR_RESAM_IN_FAIL	-1
#define LR_RESAM_IN_NONE	 0
#define LR_RESAM_IN_REORIENT	 1
#define LR_RESAM_IN_RESAM	 2

static char * this_file = "r_new_resam_dset.c";

/*----------------------------------------------------------------------
 * r_new_resam_dset - create a new dataset by resampling an existing one
 *
 * inputs:
 *   - din	: input dataset
 *   - min	: master dataset - overrides dxyz and orient input
 *   - dx,dy,dz : new deltas
 *   - orient   : new orientation string
 *   - resam    : resampling mode
 *   - sublist  : list of sub-bricks to resample (all if NULL)
 *                {first element of sublist is the number of sub-bricks}
 *
 * output       : a pointer to the resulting THD_3dim_dataset
 *                (or NULL, on failure)
 *
 * notes:
 *   - if (min != NULL)          , dx, dy, dz and orient are ignored
 *   - if (dx == dy == dz == 0.0), no resampling will be done
 *   - if (orient == NULL),        no reorienting will be done
 *----------------------------------------------------------------------
*/
THD_3dim_dataset * r_new_resam_dset
    (
	THD_3dim_dataset * din,		/* original dataset to resample */
	THD_3dim_dataset * min,		/* master dataset to align to   */
	double             dx,		/* delta for new x-axis         */
	double             dy,		/* delta for new y-axis         */
	double             dz,		/* delta for new z-axis         */
	char               orient [],   /* new orientation code         */
	int                resam,	/* mode to resample with        */
	int              * sublist      /* list of sub-bricks to resam  */
    )
{
    THD_3dim_dataset * dout;
    THD_3dim_dataset * dtmp;
    THD_dataxes        new_daxes;
    FD_brick         * fdb = NULL;
    int                work;
    char             * l_orient, l_orient_s[4] = "";

    /* if we have a master, do not rely on orient - use local memory */
    if ( min ) l_orient = l_orient_s;
    else       l_orient = orient;

    work = valid_resam_inputs( din, min, dx, dy, dz, l_orient, resam );

    if ( work == LR_RESAM_IN_FAIL )
        return NULL;

    if ( sublist )
	dtmp = THD_copy_dset_subs(din, sublist);
    else
	dtmp = din;

    dout = EDIT_empty_copy( dtmp );		/* create new brick */
    if( ! ISVALID_3DIM_DATASET( dout ) )
    {
	fprintf( stderr, "ERROR: <%s> - failed to duplicate datset at %p\n",
		 this_file, din );
	return NULL;
    }

    /* give junk name */
    EDIT_dset_items(dout, ADN_prefix, "junk_resample", ADN_none );

    /* possibly update view type to that of the master */
    if ( min                                  &&
         (dout->view_type != min->view_type)  &&
	 (dout->dblk && dout->dblk->diskptr) )
    {
	dout->view_type = min->view_type;
	THD_init_diskptr_names( dout->dblk->diskptr, NULL, NULL, NULL,
                                min->view_type, True );
    }


    /* re-orient the brick? */
    if ( work & LR_RESAM_IN_REORIENT )
    {
	if ( ( fdb = THD_oriented_brick( dout, l_orient ) ) == NULL )
	{
	    fprintf( stderr, "<%s>: failed to create THD_brick with orient "
		     "string <%.6s>\n", this_file, l_orient );
	    THD_delete_3dim_dataset( dout, FALSE );
	    return NULL;
	}

	if ( apply_orientation( dout, fdb, l_orient ) != 0 )
	{
	    THD_delete_3dim_dataset( dout, FALSE );
	    return NULL;
	}
    }

    new_daxes = *dout->daxes;		     /* in case we don't resample */

    /* resample the brick? */
    if ( work & LR_RESAM_IN_RESAM )
    {
	/*-- given dx, dy and dz, create a new THD_dataxes structure ---- */
	new_daxes.type = DATAXES_TYPE;

	if ( min )   /* if we have a master, just copy the dataxes struct */
	    new_daxes = *min->daxes;
        else if ( r_dxyz_mod_dataxes(dx, dy, dz, dout->daxes, &new_daxes) != 0 )
	{
	    THD_delete_3dim_dataset( dout, FALSE );
	    return NULL;
	}

	if ( apply_dataxes( dout, &new_daxes ) != 0 )
	{
	    THD_delete_3dim_dataset( dout, FALSE );
	    return NULL;
	}
    }

    /* needed by THD_load_datablock() */
    dout->warp_parent        = dtmp;
    dout->warp_parent_idcode = dtmp->idcode;	/* needed for HEAD write */

    /* needed to warp from parent */
    dout->wod_flag        = True;		/* mark for WOD          */
    dout->vox_warp->type  = ILLEGAL_TYPE;	/* mark fo recomputation */
    *dout->wod_daxes      = new_daxes;		/* used for actual warp  */

    dout->daxes->parent   = (XtPointer)dout;	/* parent is new dset    */

    if ( r_fill_resampled_data_brick( dout, resam ) != 0 )
    {
	THD_delete_3dim_dataset( dout, FALSE );
	dout = NULL;
    }

    if ( sublist ) DSET_delete(dtmp);

    return dout;
}


/*----------------------------------------------------------------------
 * r_fill_resampled_data_brick - get the data brick into memory
 *
 * return FAIL or OK (0)
 *                                  - see adwarp_refashion_dataset
 *----------------------------------------------------------------------
*/
int r_fill_resampled_data_brick( THD_3dim_dataset * dset, int resam )
{
    THD_diskptr * diskptr = dset->dblk->diskptr;
    MRI_IMAGE   * im;
    char        * newdata, * dptr;
    float         bfac;
    int           ival, dsize;
    int           nx, ny, nz, nxy, nxyz, nv;
    int           slice;

    if ( DSET_LOADED(dset) )
    {
	fprintf( stderr, "error <%s>: trying to fill pre-loaded dataset\n",
		 this_file );
	return FAIL;
    }

    DSET_lock(dset);	      /* since it will just sit in memory for now */

    /* a basic warp is needed if header is written out - PLUTO_add_dset() */
    dset->warp  = myXtNew( THD_warp );
    *dset->warp = IDENTITY_WARP;
    ADDTO_KILL( dset->kl, dset->warp );

    diskptr->byte_order   = mri_short_order();
    diskptr->storage_mode = STORAGE_BY_BRICK;

    /* note new dimensions */
    nx = dset->daxes->nxx;  ny = dset->daxes->nyy;  nz = dset->daxes->nzz;
    nv = diskptr->nvals;

    nxy  = nx * ny;
    nxyz = nx * ny * nz;

    /* recompute sub-bricks */
    for ( ival = 0; ival < nv; ival++ )            /* for each sub-brick */
    {
	/* first create memory to deposit the slices into */
	dsize = mri_datum_size( DSET_BRICK_TYPE(dset, ival) );

        if ( (newdata = (char *)malloc( nxyz * dsize )) == NULL )
	{
	    fprintf( stderr, "r frdb: alloc failure: %d bytes!\n",
                     nxyz * dsize );
	    return FAIL;
	}

	dptr = newdata;			  /* we will copy images at dptr */

	/* force return of unscaled slices for output - reset fac at end */
        bfac = DBLK_BRICK_FACTOR(dset->dblk,ival);
        DBLK_BRICK_FACTOR(dset->dblk,ival) = 0.0;

	/* for each slice, insert it into memory */
	for ( slice = 0; slice < nz; slice++)
	{
	    im = AFNI_dataset_slice( dset, 3, slice, ival, resam );
	    if ( im == NULL )
	    {
		fprintf( stderr, "r_fill_resampled_data_brick: failure to "
			 "compute dataset slice %d\n", slice );
		free( newdata );
		return FAIL;
	    }

	    memcpy( (void *)dptr, mri_data_pointer(im), nxy*dsize );
	    mri_free( im );
	    dptr += nxy*dsize;
	}

        DBLK_BRICK_FACTOR(dset->dblk,ival) = bfac;
	/* we now have the raw brick data, so insert it into the brick */
	EDIT_substitute_brick(dset, ival, DSET_BRICK_TYPE(dset,ival),
			      (void *)newdata);
    }

    dset->dblk->malloc_type = DATABLOCK_MEM_MALLOC;
    dset->wod_flag = False;		/* since data is now in memory */

    /* recompute statistics */
    THD_load_statistics( dset );

    return 0;   /* OK */
}

/*----------------------------------------------------------------------
 * Fill in a THD_dataxes structure to match the 3D box of an existing
 * one, but where dx, dy and dz are modified.
 *
 * This is identical to THD_edit_dataxes(), except that the requirement
 * for cubical voxels has been lifted.
 *----------------------------------------------------------------------
*/
int r_dxyz_mod_dataxes( double dx, double dy, double dz,
                        THD_dataxes * daxin, THD_dataxes * daxout
                      )
{
    double    rex, rey, rez;
    double    lxx, lyy, lzz;
    int       ret_val;

    if ( ! ISVALID_DATAXES( daxin ) || ! ISVALID_DATAXES( daxout ) )
	return -1;

    *daxout = *daxin;    /* start with a copy */

    if ( dx <= 0.0 || dy <= 0.0 || dz <= 0.0 )
	return -1;       /* having duplicated the structures */

    rex = (daxout->xxdel > 0) ? dx : -dx;   /* signed voxel sizes */
    rey = (daxout->yydel > 0) ? dy : -dy;
    rez = (daxout->zzdel > 0) ? dz : -dz;

    lxx = daxin->nxx * daxin->xxdel;          /* signed lengths of data box */
    lyy = daxin->nyy * daxin->yydel;
    lzz = daxin->nzz * daxin->zzdel;

    daxout->nxx = (int)( lxx/rex + 0.499 );     /* so this is > 0 */
    daxout->nyy = (int)( lyy/rey + 0.499 );
    daxout->nzz = (int)( lzz/rez + 0.499 );

    /* go from old edge to old center, then back out to new edge */
    daxout->xxorg = daxin->xxorg + 0.5*(lxx - daxin->xxdel)
				 - 0.5*(daxout->nxx - 1)*rex;

    daxout->yyorg = daxin->yyorg + 0.5*(lyy - daxin->yydel)
				 - 0.5*(daxout->nyy - 1)*rey;

    daxout->zzorg = daxin->zzorg + 0.5*(lzz - daxin->zzdel)
				 - 0.5*(daxout->nzz - 1)*rez;

    /* dave new dimensions */
    daxout->xxdel = rex;
    daxout->yydel = rey;
    daxout->zzdel = rez;

    /* create a new bounding box                        */
    /* (note that xxdel<0 implies we must swap min/max) */
    daxout->xxmin = daxout->xxorg;
    daxout->xxmax = daxout->xxorg + (daxout->nxx-1)*daxout->xxdel;
    if ( daxout->xxmin > daxout->xxmax )
    {
        double tmp    = daxout->xxmin;
        daxout->xxmin = daxout->xxmax;
        daxout->xxmax = tmp;
    }

    daxout->yymin = daxout->yyorg;
    daxout->yymax = daxout->yyorg + (daxout->nyy-1)*daxout->yydel;
    if ( daxout->yymin > daxout->yymax )
    {
        double tmp    = daxout->yymin;
        daxout->yymin = daxout->yymax;
        daxout->yymax = tmp;
    }

    daxout->zzmin = daxout->zzorg;
    daxout->zzmax = daxout->zzorg + (daxout->nzz-1)*daxout->zzdel;
    if ( daxout->zzmin > daxout->zzmax )
    {
        double tmp    = daxout->zzmin;
        daxout->zzmin = daxout->zzmax;
        daxout->zzmax = tmp;
    }

#ifdef EXTEND_BBOX
    daxout->xxmin -= 0.5 * daxout->xxdel;
    daxout->xxmax += 0.5 * daxout->xxdel;
    daxout->yymin -= 0.5 * daxout->yydel;
    daxout->yymax += 0.5 * daxout->yydel;
    daxout->zzmin -= 0.5 * daxout->zzdel;
    daxout->zzmax += 0.5 * daxout->zzdel;
#endif

    return ret_val = 0;
}


/*----------------------------------------------------------------------
 * validate an orientation string
 *
 * use explicit boolean return values
 *----------------------------------------------------------------------
*/
Boolean r_is_valid_orient_str ( char ostr [] )
{
    int o1, o2, o3;

    if ( ostr == NULL )
	return False;

    o1 = ORCODE(toupper(ostr[0]));
    o2 = ORCODE(toupper(ostr[1]));
    o3 = ORCODE(toupper(ostr[2]));

    if ( ( o1 != ILLEGAL_TYPE ) &&
	 ( o2 != ILLEGAL_TYPE ) &&
	 ( o3 != ILLEGAL_TYPE ) &&
	   OR3OK(o1,o2,o3) )
	return True;
    else
	return False;
}


/*----------------------------------------------------------------------
 * given an orientation string, fill an orientation code vector
 *
 * return 0 on success
 *----------------------------------------------------------------------
*/
int r_orient_str2vec ( char ostr [], THD_ivec3 * ovec )
{
    int o1, o2, o3;

    if ( !ostr || !ovec )
    {
	fprintf( stderr, "%s: r_orient_str2vec - invalid parameter pair "
		 "(%p,%p)\n", this_file, ostr, ovec );
	return -1;
    }

    ovec->ijk[0] = o1 = ORCODE(toupper(ostr[0]));
    ovec->ijk[1] = o2 = ORCODE(toupper(ostr[1]));
    ovec->ijk[2] = o3 = ORCODE(toupper(ostr[2]));

    if ( ( o1 == ILLEGAL_TYPE ) ||
	 ( o2 == ILLEGAL_TYPE ) ||
	 ( o3 == ILLEGAL_TYPE ) ||
	 ( !OR3OK(o1,o2,o3) ) )
    {
	fprintf( stderr, "%s: r_orient_str2vec - bad ostr <%.4s>\n",
		 this_file, ostr );
	return -2;
    }

    return 0;
}


/*----------------------------------------------------------------------
 * validate inputs to driving function
 *
 * return one of:
 *
 *    LR_RESAM_IN_NONE      : no changes from original
 *    LR_RESAM_IN_REORIENT  : do a re-orientation
 *    LR_RESAM_IN_RESAM     : resample dataset
 *    LR_RESAM_IN_FAIL      : bad inputs
 *----------------------------------------------------------------------
*/
static int valid_resam_inputs( THD_3dim_dataset * dset, THD_3dim_dataset * mset,
			       double dx, double dy, double dz,
			       char orient [], int resam )
{
    int ret_val = LR_RESAM_IN_NONE;

    if( ! ISVALID_3DIM_DATASET(dset) )
    {
	fprintf( stderr, "ERROR: <%s> - invalid input dataset\n", this_file );
	return LR_RESAM_IN_FAIL;
    }

    if( mset ) /* behold, the master! */
    {
	/* validate mset and its thd_dataxes structure */
        if ( ! ISVALID_3DIM_DATASET(mset) || !ISVALID_DATAXES(mset->daxes) )
	{
	    fprintf( stderr, "ERROR: <%s> - invalid master dataset\n",
                     this_file );
	    return LR_RESAM_IN_FAIL;
        }

	/* we should have orientation memory with a master */
	if ( ! orient )
	{
	    fprintf( stderr, "ERROR: <%s> - orientation memory should "
		     "come with master\n", this_file );
	    return LR_RESAM_IN_FAIL;
	}

	/* we have a master - get orientation now, dxyz will come later */
	orient[0] = ORIENT_typestr[mset->daxes->xxorient][0];
	orient[1] = ORIENT_typestr[mset->daxes->yyorient][0];
	orient[2] = ORIENT_typestr[mset->daxes->zzorient][0];
	orient[3] = '\0';

	return LR_RESAM_IN_RESAM | LR_RESAM_IN_REORIENT; /* we're good to go */
    }

    /* so no master dataset, validate dxyz and orientation */

    /* validate resampling */
    if ( dx == 0.0 && dy == 0.0 && dz == 0.0 )        /* no resampling */
        ret_val &= ~LR_RESAM_IN_RESAM;
    else if ( dx <= 0.0 || dy <= 0.0 || dz <= 0.0 )
    {
	fprintf( stderr, "ERROR: <%s> - invalid (dx,dy,dz) = (%f,%f,%f)\n",
		 this_file, dx, dy, dz );
	return LR_RESAM_IN_FAIL;
    }
    else
	ret_val |= LR_RESAM_IN_RESAM;

    /* validate the resampling mode - this is required in any case */
    if ( resam < 0 || resam > LAST_RESAM_TYPE )
    {
        fprintf( stderr, "ERROR: <%s> - invalid resample mode of %d\n",
                 this_file, resam );
        return LR_RESAM_IN_FAIL;
    }

    /* validate orientation */
    if ( orient == NULL )
          ret_val &= ~LR_RESAM_IN_REORIENT;
    else if ( r_is_valid_orient_str ( orient ) == False )
    {
	fprintf( stderr, "ERROR: <%s> - invalid orientation string <%.6s>\n",
		 this_file, orient );
	return LR_RESAM_IN_FAIL;
    }
    else
	ret_val |= LR_RESAM_IN_REORIENT;

    return ret_val;
}


/*----------------------------------------------------------------------
 * Apply orientation and delta values to the dataset.
 *
 * return 0 on success
 *----------------------------------------------------------------------
*/
static int apply_orientation( THD_3dim_dataset * dset, FD_brick * fdb,
			      char orient[] )
{
    THD_dataxes * daxp = dset->daxes;
    THD_ivec3     ivnxyz, ivorient;
    THD_fvec3     fvdel, fvorg;
    float         org4[4], del4[4];
    int		  aa1, aa2, aa3;
    int		  a1, a2, a3;
    int           ret_val;

    LOAD_IVEC3(ivnxyz, fdb->n1, fdb->n2, fdb->n3);

    if ( (ret_val = r_orient_str2vec(orient, &ivorient)) != 0 )
	return ret_val;

    LOAD_FVEC3( fvdel,
	ORIENT_sign[ivorient.ijk[0]]=='+' ? fdb->del1 : -fdb->del1,
	ORIENT_sign[ivorient.ijk[1]]=='+' ? fdb->del2 : -fdb->del2,
	ORIENT_sign[ivorient.ijk[2]]=='+' ? fdb->del3 : -fdb->del3 );

    /* use original org and delta with permuted index to get new orgs */
    org4[1] = daxp->xxorg;  org4[2] = daxp->yyorg;  org4[3] = daxp->zzorg;
    del4[1] = daxp->xxdel;  del4[2] = daxp->yydel;  del4[3] = daxp->zzdel;

    a1  = fdb->a123.ijk[0];  a2 = fdb->a123.ijk[1];  a3 = fdb->a123.ijk[2];
    aa1 = abs(a1);          aa2 = abs(a2);          aa3 = abs(a3);

    LOAD_FVEC3( fvorg,
		(a1 > 0) ? org4[aa1] : org4[aa1]+(fdb->n1-1)*del4[aa1],
		(a2 > 0) ? org4[aa2] : org4[aa2]+(fdb->n2-1)*del4[aa2],
		(a3 > 0) ? org4[aa3] : org4[aa3]+(fdb->n3-1)*del4[aa3] );

    /* now update the current brick with our new orientation data */
    EDIT_dset_items( dset,
			ADN_nxyz,      ivnxyz,
			ADN_xyzdel,    fvdel,
			ADN_xyzorg,    fvorg,
			ADN_xyzorient, ivorient,
		     ADN_none );
    return 0;
}


/*----------------------------------------------------------------------
 * Apply new dataxes structure to dataset and prepare for warp speed.
 *
 * return 0 on success
 *----------------------------------------------------------------------
*/
static int apply_dataxes( THD_3dim_dataset * dset, THD_dataxes * dax )
{
    THD_ivec3     ivnxyz;
    THD_fvec3     fvdel, fvorg;

    LOAD_IVEC3( ivnxyz, dax->nxx, dax->nyy, dax->nzz );
    LOAD_FVEC3( fvorg, dax->xxorg, dax->yyorg, dax->zzorg );
    LOAD_FVEC3( fvdel, dax->xxdel, dax->yydel, dax->zzdel );

    EDIT_dset_items( dset,
			ADN_nxyz,   ivnxyz,
			ADN_xyzdel, fvdel,
			ADN_xyzorg, fvorg,
		     ADN_none );

    /* prepare the dataset for warping */
    *dset->wod_daxes         = *dax;		/* used for actual warp  */
    *dset->daxes             = *dax;

    return 0;
}

