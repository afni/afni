
#include "mrilib.h"
#include "r_idisp.h"

/*----------------------------------------------------------------------
 * The following routines are to display data structures.
 * The functions are of the form r_idisp_XXX, meaning :
 *     "Information Display of data structure XXX"
 *
 * Existing structures are:
 *   3ddata.h:
 *      - FD_brick
 *      - THD_3dim_dataset
 *      - THD_dataxes
 *      - THD_datablock
 *      - THD_diskptr
 *
 *   mrilib.h:
 *      - MRI_IMAGE
 *      - MRI_IMARR
 *
 *   cox_render.h:
 *      - CREN_stuff
 *
 *   raw data types:
 *      - mat33d
 *      - mat33f
 *      - vec3d
 *      - vec3i
 *
 * Return values:
 *   0    : success
 *   else : some error
 *----------------------------------------------------------------------
**/


/*----------------------------------------------------------------------
 *                new structs - local
 *----------------------------------------------------------------------
*/

int r_idisp_vec3d( char * info, double * vec )
{
    if ( info )
        fputs( info, stdout );

    if ( vec == NULL )
    {
        printf( "r_idisp_vec3d: vec == NULL" );
        return -1;
    }

    printf( "THD_dvec3 vector at %p: <%f, %f, %f>\n",
            vec, vec[0], vec[1], vec[2] );

    return 0;
}

/*----------------------------------------------------------------------
 *                mrilib.h, 3ddata.h
 *----------------------------------------------------------------------
*/

int r_idisp_fd_brick( char * info, FD_brick * bp )
{
    if ( info )
        fputs( info, stdout );

    if ( bp == NULL )
    {
	printf( "r_idisp_fd_brick: bp == NULL\n" );
	return -1;
    }

    printf( "FD_brick structure at %p :\n", bp );
    r_idisp_vec3i( "   FD->nxyz : ", bp->nxyz.ijk );
    r_idisp_vec3i( "   FD->sxyz : ", bp->sxyz.ijk );
    r_idisp_vec3i( "   FD->a123 : ", bp->a123.ijk );
    printf( "   (n1, d1, e1) = (%d, %d, %d)\n"
            "   (n2, d2, e2) = (%d, %d, %d)\n"
            "   (n3, d3)     = (%d, %d)\n"
            "   start        = %d\n"
            "   (del1, del2, del3) = (%8.3f, %8.3f, %8.3f)\n"
            "   dset               = %p\n"
	    "   resam_code         = %d\n"
            "   thr_resam_code     = %d\n"
            "   namecode           = <%s>\n"
	    "   parent             = %p\n",
            bp->n1, bp->d1, bp->e1, bp->n2, bp->d2, bp->e2,
            bp->n3, bp->d3, bp->start,
            bp->del1, bp->del2, bp->del3,
            bp->dset, bp->resam_code, bp->thr_resam_code,
            bp->namecode, bp->parent );

    return 0;
}

int r_idisp_vec3i( char * info, int * vec )
{
    if ( info )
        fputs( info, stdout );

    if ( vec == NULL )
    {
	printf( "r_idisp_vec3i: vec == NULL\n" );
        return -1;
    }

    printf( "int vec3 at %p: <%d, %d, %d>\n",
            vec, vec[0], vec[1], vec[2] );

    return 0;
}

int r_idisp_mri_image( char * info, MRI_IMAGE * ip )
{
    if ( info )
        fputs( info, stdout );

    if ( ip == NULL )
    {
	printf( "r_idisp_mri_image: ip == NULL\n" );
	return -1;
    }

    printf( "r_idisp_mri_image structure at %p :\n"
            "   nx = %d, ny = %d, nz = %d\n"
            "   nt = %d, nu = %d, nv = %d, nw = %d\n"
            "   nxy = %d, nxyz = %d, nxyzt = %d\n"
            "   nvox = %d, pixel_size = %d\n"
            "   kind = %d, im = %p, name = %s\n"
            "   dx = %7.3f, dy = %7.3f, dz = %7.3f, dt = %7.3f\n"
            "   du = %7.3f, dv = %7.3f, dw = %7.3f\n"
            "   xo = %7.3f, yo = %7.3f, zo = %7.3f, to = %7.3f\n"
            "   uo = %7.3f, vo = %7.3f, wo = %7.3f\n"
            "   was_swapped = %d\n",
            ip,
            ip->nx, ip->ny, ip->nz, ip->nt, ip->nu, ip->nv, ip->nw,
            ip->nxy, ip->nxyz, ip->nxyzt, ip->nvox, ip->pixel_size,
            (int)ip->kind, ip->im.byte_data, ip->name,
            ip->dx, ip->dy, ip->dz, ip->dt, ip->du, ip->dv, ip->dw,
            ip->xo, ip->yo, ip->zo, ip->to, ip->uo, ip->vo, ip->wo,
            ip->was_swapped );

    return 0;
}

int r_idisp_mri_imarr( char * info, MRI_IMARR * ip, int images )
{
    int c;

    if ( info )
        fputs( info, stdout );

    if ( ip == NULL )
    {
	printf( "r_idisp_mri_imarr: ip == NULL\n" );
	return -1;
    }

    printf( "r_idisp_mri_imarr structure at %p :\n"
            "      num = %d, nall (mem) = %d\n",
            ip, ip->num, ip->nall );

    if ( images )
    {
	for ( c = 0; c < ip->num; c++ )
        {
	    r_idisp_mri_image( NULL, ip->imarr[c] );
	}
    }

    return 0;
}

int r_idisp_mat33f( char * info, float mat[3][3] )
{
    if ( info )
        fputs( info, stdout );

    if ( mat == NULL )
    {
	printf( "r_idisp_mat33f: mat == NULL\n" );
	return -1;
    }

    printf( "mat33 float structure at %p :\n"
            "      %7.3f   %7.3f   %7.3f\n"
            "      %7.3f   %7.3f   %7.3f\n"
            "      %7.3f   %7.3f   %7.3f\n",
            mat,
            mat[0][0], mat[0][1], mat[0][2],
            mat[1][0], mat[1][1], mat[1][2],
            mat[2][0], mat[2][1], mat[2][2]
          );
    return 0;
}

int r_idisp_mat33d( char * info, double mat[3][3] )
{
    if ( info )
        fputs( info, stdout );

    if ( mat == NULL )
    {
	printf( "r_idisp_mat33d: mat == NULL\n" );
	return -1;
    }

    printf( "mat33 double structure at %p :\n"
            "      %7.3f   %7.3f   %7.3f\n"
            "      %7.3f   %7.3f   %7.3f\n"
            "      %7.3f   %7.3f   %7.3f\n",
            mat,
            mat[0][0], mat[0][1], mat[0][2],
            mat[1][0], mat[1][1], mat[1][2],
            mat[2][0], mat[2][1], mat[2][2]
          );
    return 0;
}

int r_idisp_thd_3dim_dataset( char * info, THD_3dim_dataset * dp )
{
    if ( info )
        fputs( info, stdout );

    if ( dp == NULL )
    {
	printf( "r_idisp_thd_3dim_dataset: dp == NULL\n" );
	return -1;
    }

    printf( "THD_3dim_dataset struct at %p\n"
            "   type         : %d\n"
            "   view_type    : %d\n"
            "   func_type    : %d\n"
            "   -------------------\n"
            "   dblk         : %p\n"
            "   daxes        : %p\n"
            "   wod_daxes    : %p\n"
            "   wod_flag     :   %d\n"
            "   -------------------\n"
            "   taxis        : %p\n"
            "   markers      : %p\n"
            "   warp_parent  : %p\n"
            "   warp         : %p\n"
            "   vox_warp     : %p\n"
            "   -------------------\n"
            "   anat_parent  : %p\n"
            "   stats        : %p\n"
            "   pts          : %p\n"
            "   pts_original :   %d\n"
            "   death_mark   :   %d\n"
            "   -------------------\n"
#ifndef OMIT_DATASET_IDCODES
            "   idcode.str              : <%s>\n"
            "   idcode.date             : <%s>\n"
            "   anat_parent_idcode.str  : <%s>\n"
            "   anat_parent_idcode.date : <%s>\n"
            "   warp_parent_idcode.str  : <%s>\n"
            "   warp_parent_idcode.date : <%s>\n"
            "   -------------------\n"
#endif
            "   keywords  : %p\n"
            "   tagset    : %p\n"
            "   -------------------\n"
            "   kl.num    :   %d\n"
            "   kl.nalloc :   %d\n"
            "   kl.kill   : %p\n"
            "   parent    : %p\n"
            "   -------------------\n"
            "   su_surf   : %p\n"
            "   su_sname  : <%s>\n"
            "   su_vmap   : %p\n"
            "   su_vnlist : %p\n"
            "-------------------------------------------\n",
	    dp, dp->type, dp->view_type, dp->func_type,
            dp->dblk, dp->daxes, dp->wod_daxes, dp->wod_flag,
            dp->taxis, dp->markers, dp->warp_parent, dp->warp, dp->vox_warp,
	    dp->anat_parent, dp->stats, dp->pts,
            dp->pts_original, dp->death_mark,
#ifndef OMIT_DATASET_IDCODES
            dp->idcode.str, dp->idcode.date,
            dp->anat_parent_idcode.str, dp->anat_parent_idcode.date,
            dp->warp_parent_idcode.str, dp->warp_parent_idcode.date,
#endif
            dp->keywords, dp->tagset,
	    dp->kl.num, dp->kl.nalloc, dp->kl.kill, dp->parent,
	    dp->su_surf, dp->su_sname, dp->su_vmap, dp->su_vnlist
          );

    return 0;
}


int r_idisp_thd_diskptr( char * info, THD_diskptr * dp )
{
    if ( info )
        fputs( info, stdout );

    if ( dp == NULL )
    {
	printf( "r_idisp_thd_diskptr: dp == NULL\n" );
	return -1;
    }

    printf( "THD_diskptr structure at %p\n"
            "   type (%2d)      : %d\n"
            "   rank (3)       : %d\n"
            "   dimsizes       : (%d,%d,%d)\n"
            "   storage_mode   : %d\n"
            "   byte_order     : %d\n"
            "   --------------------------------------\n"
            "   prefix         : %.60s\n"
            "   viewcode       : %.60s\n"
            "   filecode       : %.60s\n"
            "   --------------------------------------\n"
            "   directory_name : %.80s\n"
            "   header_name    : %.80s\n"
            "   brick_name     : %.80s\n"
            "   --------------------------------------\n",
	    dp, DISKPTR_TYPE, dp->type, dp->rank,
	    dp->dimsizes[0], dp->dimsizes[1], dp->dimsizes[2],
	    dp->storage_mode, dp->byte_order,
            dp->prefix, dp->viewcode, dp->filecode,
	    dp->directory_name, dp->header_name, dp->brick_name
	  );
}


int r_idisp_thd_datablock( char * info, THD_datablock * dp )
{
    if ( info )
        fputs( info, stdout );

    if ( dp == NULL )
    {
	printf( "r_idisp_thd_datablock: dp == NULL\n" );
	return -1;
    }

    printf( "THD_datablock structure at %p\n"
            "   type        : %d\n"
            "   nvals       : %d\n"
            "   brick       : %p\n"
            "   brick_fac   : %p\n"
            "   brick_bytes : %p\n",
	    dp, dp->type, dp->nvals, dp->brick,
            dp->brick_fac, dp->brick_bytes );

    if ( dp->nvals > 0 )
    {
	int c;

	printf( 
	  "   ----------------------------------------\n"
	  "   sub   fac        brick_bytes   brick_lab\n"
	  "   ---   ---        -----------   ---------\n" );
	for ( c = 0; c < dp->nvals; c++ )
	{
	    printf( "   %3d   ", c );

	    if ( dp->brick_fac )
		printf( "%f  ", dp->brick_fac[c] );
	    else
		printf( "         " );

	    if ( dp->brick_bytes )
		printf( " %10d    ", dp->brick_bytes[c] );
	    else
		printf( "%15s", "" );

	    if ( dp->brick_lab )
		printf( "%s\n", dp->brick_lab[c] );
	    else
		printf( "\n" );
	}
    }

    printf( "   --------------------------------------\n"
            "   brick_keywords : %p\n"
            "   brick_statcode : %p\n"
            "   brick_stataux  : %p\n"
            "   --------------------------------------\n"
	    "   total_bytes    : %d\n"
	    "   malloc_type    : %d\n"
	    "   locked         : %d\n"
            "   --------------------------------------\n"
	    "   master_nvals   : %d\n"
	    "   master_ival    : %p\n"
	    "   master_bytes   : %p\n"
	    "   master_bot     : %f\n"
	    "   master_top     : %f\n"
            "   --------------------------------------\n"
	    "   diskptr        : %p\n"
	    "   natr           : %d\n"
	    "   natr_alloc     : %d\n"
	    "   atr            : %p\n"
            "   --------------------------------------\n"
	    "   kl.num         : %d\n"
	    "   kl.nalloc      : %d\n"
	    "   kl.kill        : %p\n"
	    "   parent         : %p\n"
            "-----------------------------------------\n",
	    dp->brick_keywords, dp->brick_statcode, dp->brick_stataux,
	    dp->total_bytes, dp->malloc_type, dp->locked,
	    dp->master_nvals, dp->master_ival, dp->master_bytes,
	    dp->master_bot, dp->master_top,
	    dp->diskptr, dp->natr, dp->natr_alloc, dp->atr,
	    dp->kl.num, dp->kl.nalloc, dp->kl.kill, dp->parent );

    return 0;
}


int r_idisp_thd_dataxes( char * info, THD_dataxes * dp )
{
    if ( info )
        fputs( info, stdout );

    if ( dp == NULL )
    {
	printf( "r_idisp_thd_dataxes: dp == NULL\n" );
	return -1;
    }

    printf( "THD_dataxes structure at %p\n"
            "   type                  : %d\n"
            "   (nxx, nyy, nzz)       : (%d, %d, %d)\n"
            "   (xxorg, yyorg, zzorg) : (%8.3f, %8.3f, %8.3f)\n"
            "   (xxdel, yydel, zzdel) : (%8.3f, %8.3f, %8.3f)\n"
            "   (xxmin, yymin, zzmin) : (%8.3f, %8.3f, %8.3f)\n"
            "   (xxmax, yymax, zzmax) : (%8.3f, %8.3f, %8.3f)\n"
            "   (xxorient,yyorient,zzorient) : (%d, %d, %d)\n"
	    "   parent                : %p\n"
            "   -----------------------\n",
            dp,
            dp->type, dp->nxx, dp->nyy, dp->nzz,
            dp->xxorg, dp->yyorg, dp->zzorg, dp->xxdel, dp->yydel, dp->zzdel,
            dp->xxmin, dp->yymin, dp->zzmin, dp->xxmax, dp->yymax, dp->zzmax,
            dp->xxorient, dp->yyorient, dp->zzorient, dp->parent
          );

    r_idisp_mat33f( "   to_dicomm[3][3] : ", dp->to_dicomm.mat );

    printf( "-------------------------------------------\n ");

    return 0;
}


/*----------------------------------------------------------------------
 *                cox_render.h
 *----------------------------------------------------------------------
*/

#ifdef _COX_RENDER_HEADER_        /* be sure we can compile this */

int r_idisp_cren_stuff( char * info, CREN_stuff * cp )
{
    if ( info )
        fputs( info, stdout );

    if ( cp == NULL )
    {
	printf( "r_idisp_thd_dataxes: cp == NULL\n" );
	return -1;
    }

    printf( "CREN_stuff structure at %p\n"
            "   type = %d, nx = %d, ny = %d, nz = %d\n"
            "   dx = %7.3f, dy = %7.3f, dz = %7.3f\n"
            "   vox = %p, vtm = %p\n"
            "   nrgb = %d, opargb = %7.3f, min_opacity = %7.3f\n"
            /* rmap, gmap, bmap, imap,   opamap */
            "   ax1 = %d, ax2 = %d, ac3 = %d\n"
            "   th1 = %7.3f, th2 = %7.3f, th3 = %7.3f\n"
            /* THD_mat33 skewmat */
            "   newvox = %d, newopa = %d, newangles = %d\n"
            "   renmode = %d, intmode = %d\n",
            cp,
            cp->type, cp->nx, cp->ny, cp->nz, cp->dx, cp->dy, cp->dz,
            cp->vox, cp->vtm,
            cp->nrgb, cp->opargb, cp->min_opacity,
            cp->ax1, cp->ax2, cp->ax3, cp->th1, cp->th2, cp->th3,
            cp->newvox, cp->newopa, cp->newangles, cp->renmode, cp->intmode
          );

    return 0;
}

#endif  /* _COX_RENDER_HEADER_ */
