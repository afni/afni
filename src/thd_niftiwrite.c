#include "mrilib.h"
#include "thd_niftiwrite.h"

/* prototypes */

nifti_image * populate_nifti_image(THD_3dim_dataset *dset, niftiwr_opts_t options) ;

void nifti_set_afni_extension(THD_3dim_dataset *dset,nifti_image *nim) ;

/*******************************************************************/
/*!  Write an AFNI dataset as a NIFTI file.
     - fname = NIFTI filename
     - dset = AFNI dataset
     - flags = logical OR of various masks:

   Return value is 1 if went OK, 0 if not.
-------------------------------------------------------------------*/

int THD_write_nifti( THD_3dim_dataset *dset, niftiwr_opts_t options )
{
  nifti_image * nim ;
  nifti_brick_list nbl ;
  int ii ;
  char * fname ;

ENTRY("THD_write_nifti") ;

  nifti_set_debug_level(options.debug_level) ;

   /*-- check inputs for goodness --*/

  fname = nifti_strdup(options.infile_name );

  if( !THD_filename_ok(fname) || fname[0] == '-' ){
    fprintf(stderr,"** ERROR: Illegal filename for NIFTI output: %s\n",
      (fname != NULL) ? fname : "(null)" ) ;
    RETURN(0) ;
  }

  if( !ISVALID_DSET(dset) ){
    fprintf(stderr,
         "** ERROR: Illegal input dataset for NIFTI output: %s\n",
         fname ) ;
    RETURN(0) ;
  }

  /*-- load dataset from disk, if need be --*/

  DSET_load(dset) ;
  if( !DSET_LOADED(dset) ){
    fprintf(stderr,
            "** ERROR: Can't write NIFTI file since dataset isn't loaded: %s\n", fname) ;
    RETURN(0) ;
  }

  nim = populate_nifti_image(dset,options) ;

  /*-- construct filename --*/

   nim->fname = malloc( strlen(fname)+16 ) ;
   nim->iname = malloc( strlen(fname)+16 ) ;
   strcpy(nim->fname,fname) ;
   strcpy(nim->iname,fname) ;

  /*-- construct nifti_brick_list of pointers to data briks */

  if( options.debug_level > 2 ) nifti_image_infodump(nim) ;
  nbl.bricks = (void **) malloc ( DSET_NVALS(dset) * sizeof(void*) ) ;
  nbl.nbricks = DSET_NVALS(dset) ;
  nbl.bsize = DSET_BRICK_BYTES(dset,0) ;
  for (ii = 0 ; ii < DSET_NVALS(dset) ; ii++ ) {
    nbl.bricks[ii] = DSET_ARRAY(dset,ii) ;
  }

  /*-- use handy-dandy library function to write out data */

  nifti_set_afni_extension( dset , nim ) ;  /* 09 May 2005 - RWCox */

  nifti_image_write_bricks (nim, &nbl ) ;
  RETURN(1) ;
}

/*******************************************************************/

nifti_image * populate_nifti_image(THD_3dim_dataset *dset, niftiwr_opts_t options)
{
  int nparam, type0 , ii , jj, val;
  int nif_x_axnum, nif_y_axnum, nif_z_axnum ;
  int slast, sfirst ;
  int pattern_unknown = 0 ;
  nifti_image * nim ;
  char axcode[3], axsign[3] ;
  float axstep[3] , axstart[3] ;
  int   axnum[3] ;
  float fac0 ;
  float dumqx, dumqy, dumqz, dumdx, dumdy, dumdz ;
  float pixdimfac[4] ;
  float odd_del, even_del, tmp_float ;
  int even_parity_sign = 0 ;

ENTRY("populate_nifti_image") ;
  /*-- create nifti_image structure --*/

  nim = (nifti_image *) calloc( 1 , sizeof(nifti_image) ) ;
  if( !nim ) {
    fprintf(stderr, "** ERROR: failed to allocate nifti image\n");
    RETURN(0) ;
  }

  /*-- calculate and set ndim and intents --*/
  /* cases to allow for:
              1. 3d+time dataset
              2. 3d func bucket
                 (not going to handle this currently, may extend later)
                 -- RWC: modified so that 'u' dimension is for buckets
              3. 3d single brick
              4. 2d and 1d spatial
                 (if 2d or 1d spatial + time, treat as #1)
              5. Don't know of any vectors in AFNI, so won't do those either
              6. Single 3d (or 2d or 1d) functional brik  */

  if (dset->dblk->nvals > 1) {
    STATUS("4D dataset") ;
    nim->ndim = (dset->taxis != NULL) ? 4 : 5 ;  /* RWC: bucket stored as 5th dimen */

    /*-- check sub-bricks for uniformity in type and scale --*/

    type0 = DSET_BRICK_TYPE(dset,0) ;
    fac0  = DSET_BRICK_FACTOR(dset,0) ;

    for( ii=1 ; ii < DSET_NVALS(dset) ; ii++ ){
      if( DSET_BRICK_TYPE(dset,ii) != type0){
        fprintf(stderr,
        "** ERROR: CANNOT WRITE NIFTI FILE; BRICK DATA TYPES NOT CONSISTENT\n") ;
        RETURN(0);
      } else if( DSET_BRICK_FACTOR(dset,ii) != fac0) {
        fprintf(stderr,
        "** ERROR: CANNOT WRITE NIFTI FILE; BRICK FACTORS NOT CONSISTENT\n") ;
        fprintf(stderr,
        "RICH HAMMETT SHOULD FIX THIS REAL SOON NOW\n") ;
        RETURN(0);
      }
    }
  } else {  /* we only have one brick */
    STATUS("3D dataset") ;
    if( options.debug_level > 1 ) fprintf(stderr,"ONLY ONE BRICK!!!\n") ;
    type0 = DSET_BRICK_TYPE(dset,0);
    fac0  = DSET_BRICK_FACTOR(dset,0) ;
    if (ISFUNC(dset)) {
      STATUS("functional dataset") ;
      if( options.debug_level > 1 )
        fprintf(stderr,"ONLY ONE BRICK, AND IT'S FUNCTIONAL!!!\n") ;
      nim->intent_code = DSET_BRICK_STATCODE(dset,0);
      if (nim->intent_code < 0) nim->intent_code = dset->func_type ;
      if (nim->intent_code < 0) nim->intent_code = NIFTI_INTENT_NONE ;
      if( options.debug_level > 1 )
        fprintf(stderr,"ONLY ONE BRICK, AND ITS FUNCTIONAL STAT CODE IS %d !!!\n",nim->intent_code) ;
      if(PRINT_TRACING){
        char str[256]; sprintf(str,"intent_code = %d",nim->intent_code);STATUS(str);
      }
      if (nim->intent_code > -1) {
        nparam = FUNC_need_stat_aux[nim->intent_code];
        if (nparam >= 1) nim->intent_p1 = DSET_BRICK_STATPAR(dset,0,1);
        if (nparam >= 2) nim->intent_p2 = DSET_BRICK_STATPAR(dset,0,2);
        if (nparam == 3) nim->intent_p3 = DSET_BRICK_STATPAR(dset,0,3);
      }
    }
    if (dset->daxes->nzz > 1) {
      nim->ndim = 3 ;
    } else if (dset->daxes->nyy > 1) {
      nim->ndim = 2 ;
    } else {
      nim->ndim = 1;
    }
  }


  /*-- set datatype, size, etc. --*/

  STATUS("set datatype") ;
  switch(type0) {
    case MRI_byte:
      nim->datatype = DT_UNSIGNED_CHAR;
      nim->nbyper = 1 ;
      break;
    case MRI_short:
      nim->datatype = DT_SIGNED_SHORT;
      nim->nbyper = 2 ;
      break;
    case MRI_int:
      nim->datatype = DT_SIGNED_INT;
      nim->nbyper = 4 ;
      break;
    case MRI_float:
      nim->datatype = DT_FLOAT;
      nim->nbyper = 4 ;
      break;
    case MRI_double:
      nim->datatype = DT_DOUBLE;
      nim->nbyper = 8 ;
      break;
    case MRI_complex:
      nim->datatype = DT_COMPLEX;
      nim->nbyper = 8 ;
      break;
    case MRI_rgb:
      nim->datatype = DT_RGB24;
      nim->nbyper = 3 ;
      break;
    case MRI_rgba:
      fprintf(stderr,
               "** ERROR: Can't write NIFTI file since dataset is RGBA: %s\n",
               options.infile_name) ;
      RETURN(0) ;
      break;
    default:
      fprintf(stderr,
               "** ERROR: Can't write NIFTI file since datatype is unknown: %s\n",
               options.infile_name) ;
      RETURN(0) ;
      break;
  }

  /*-- scaling --*/

  nim->scl_slope = fac0 ;
  nim->scl_inter = 0 ;

  /*-- spatial transforms --*/

  STATUS("set orientation") ;

  axcode[0] = ORIENT_xyz[ dset->daxes->xxorient ] ; axnum[0] = dset->daxes->nxx ;
  axcode[1] = ORIENT_xyz[ dset->daxes->yyorient ] ; axnum[1] = dset->daxes->nyy ;
  axcode[2] = ORIENT_xyz[ dset->daxes->zzorient ] ; axnum[2] = dset->daxes->nzz ;

  axsign[0] = ORIENT_sign[ dset->daxes->xxorient ] ;
  axsign[1] = ORIENT_sign[ dset->daxes->yyorient ] ;
  axsign[2] = ORIENT_sign[ dset->daxes->zzorient ] ;

  axstep[0] = dset->daxes->xxdel ; axstart[0] = dset->daxes->xxorg ;
  axstep[1] = dset->daxes->yydel ; axstart[1] = dset->daxes->yyorg ;
  axstep[2] = dset->daxes->zzdel ; axstart[2] = dset->daxes->zzorg ;

  for (ii = 0 ; ii < 3 ; ii++ ) {
    if (axcode[ii] == 'x') {
      nif_x_axnum = ii ;
    } else if (axcode[ii] == 'y') {
      nif_y_axnum = ii ;
    } else nif_z_axnum = ii ;
  }

  nim->qto_xyz.m[0][0] = nim->qto_xyz.m[0][1] = nim->qto_xyz.m[0][2] =
  nim->qto_xyz.m[1][0] = nim->qto_xyz.m[1][1] = nim->qto_xyz.m[1][2] =
  nim->qto_xyz.m[2][0] = nim->qto_xyz.m[2][1] = nim->qto_xyz.m[2][2] = 0.0 ;

  /*-- set voxel and time deltas and units --*/

  nim->dx = nim->pixdim[1] = fabs ( axstep[0] ) ;
  nim->dy = nim->pixdim[2] = fabs ( axstep[1] ) ;
  nim->dz = nim->pixdim[3] = fabs ( axstep[2] ) ;

  nim->du = nim->pixdim[5] = 0 ;
  nim->dv = nim->pixdim[6] = 0 ;
  nim->dw = nim->pixdim[7] = 0 ;

#if 0
  val = (axsign[nif_x_axnum] == '+')  ? -1 : 1 ;
  nim->qto_xyz.m[0][nif_x_axnum] = val  * nim->pixdim[nif_x_axnum + 1];
  val = (axsign[nif_y_axnum] == '+')  ? -1 : 1 ;
  nim->qto_xyz.m[1][nif_y_axnum] = val  * nim->pixdim[nif_y_axnum + 1];
  val = (axsign[nif_x_axnum] == '-')  ? -1 : 1 ;
  nim->qto_xyz.m[2][nif_z_axnum] = val  * nim->pixdim[nif_z_axnum + 1];
#else
  nim->qto_xyz.m[0][nif_x_axnum] = - axstep[nif_x_axnum];
  nim->qto_xyz.m[1][nif_y_axnum] = - axstep[nif_y_axnum];
  nim->qto_xyz.m[2][nif_z_axnum] =   axstep[nif_z_axnum];
#endif

  /* nifti origin stuff */

#if 0
  nim->qoffset_x =  axstart[nif_x_axnum] ;
  if (axsign[nif_x_axnum] == '+') nim->qoffset_x = - nim->qoffset_x ;
  nim->qoffset_y =  axstart[nif_y_axnum];
  if (axsign[nif_y_axnum] == '+') nim->qoffset_y = - nim->qoffset_y ;
  nim->qoffset_z =  axstart[nif_z_axnum];
  if (axsign[nif_z_axnum] == '-') nim->qoffset_z = - nim->qoffset_z ;
  nim->qoffset_x =  -axstart[nif_x_axnum] ;
  nim->qoffset_y =  -axstart[nif_y_axnum];
  nim->qoffset_z =  axstart[nif_z_axnum];
#else
  nim->qoffset_x =  -axstart[0] ;
  nim->qoffset_y =  -axstart[1];
  nim->qoffset_z =  axstart[2];
#endif

  nim->qto_xyz.m[0][3] = nim->qoffset_x ;
  nim->qto_xyz.m[1][3] = nim->qoffset_y ;
  nim->qto_xyz.m[2][3] = nim->qoffset_z ;

  /*-- from the above info, calculate the quaternion qform --*/

  STATUS("set quaternion") ;

  nifti_mat44_to_quatern( nim->qto_xyz , &nim->quatern_b, &nim->quatern_c, &nim->quatern_d,
                    &dumqx, &dumqy, &dumqz, &dumdx, &dumdy, &dumdz, &nim->qfac ) ;

  /*-- from the same above info, set the sform matrix to equal the qform --*/
  /* KRH 7/6/05 - using sform to duplicate qform for
                           interoperability with FSL                       */

  for ( ii = 0 ; ii < 3 ; ii++ ) {
    for ( jj = 0 ; jj < 4 ; jj++ ) {
      nim->sto_xyz.m[ii][jj] = nim->qto_xyz.m[ii][jj] ;
    }
  }

  /*-- verify dummy quaternion parameters --*/

  if( options.debug_level > 2 )
    fprintf(stderr,"++ Quaternion check:\n"
          "%f , %f\n %f , %f\n %f , %f\n %f , %f\n %f , %f\n %f , %f\n; %f\n",
           nim->qoffset_x, dumqx , nim->qoffset_y, dumqy , nim->qoffset_z, dumqz ,
           nim->dx, dumdx , nim->dy, dumdy , nim->dz, dumdz, nim->qfac ) ;

  /*-- calculate inverse qform            --*/

  nim->qto_ijk = nifti_mat44_inverse( nim->qto_xyz ) ;

  /*-- set dimensions of grid array --*/

  nim->nt = nim->nu = nim->nv = nim->nw = 1 ;
  nim->nx = axnum[0] ;
  nim->ny = axnum[1] ;
  nim->nz = axnum[2] ;

  if (dset->taxis == NULL) {
    nim->nu = DSET_NVALS(dset) ;   /* RWC: bucket is 5th dimension */
  } else {
    nim->nt = DSET_NUM_TIMES(dset) ;  /* time is 4th dimension */
  }

  if ( nim->nt > 1){
    float TR = dset->taxis->ttdel ;
    if( DSET_TIMEUNITS(dset) == UNITS_MSEC_TYPE ) TR *= 0.001; /* 10 May 2005 */
    nim->dt = nim->pixdim[4] = TR ;
  }

  nim->dim[0] = nim->ndim;
  nim->dim[1] = nim->nx;
  nim->dim[2] = nim->ny;
  nim->dim[3] = nim->nz;
  nim->dim[4] = nim->nt;  /* RWC: at most one of nt and nu is > 1 */
  nim->dim[5] = nim->nu;
  nim->dim[6] = nim->nv;
  nim->dim[7] = nim->nw;

  nim->nvox = nim->nx * nim->ny * nim->nz * nim->nt
                                * nim->nu * nim->nv * nim->nw ;

  /*-- slice timing --*/

  nim->freq_dim = nim->phase_dim = 0 ;
  if (dset->taxis != NULL) {  /* if time axis exists */
    nim->slice_dim = 3 ;
    nim->slice_duration = 0 ;
    nim->slice_start = 0 ;
    nim->slice_end = nim->nz - 1;
    nim->toffset =  DSET_TIMEORIGIN(dset);

    /*-- this bit assumes that afni slice timing offsets  *
     *-- are created starting from zero and including all *
     *-- slices initially.  They may later be modified by *
     *-- zero padding at either end.  No other            *
     *-- modifications are intentionally accepted right now. */

    if (DSET_NUM_TTOFF(dset) > 0 ) { /* if time offset exists */

      /*-- Find first and last non-zero element */
#define MYEPSILON 0.00001
#define MYFPEQ(a, b) (fabs((a) - (b)) < MYEPSILON)

      for (ii = 0 ; ii < nim->nz ; ii++ ) {
        if (!MYFPEQ(dset->taxis->toff_sl[ii],0.0)) break ;
      }
      sfirst = ii ;
      for (ii = nim->nz - 1 ; ii >= sfirst ; ii-- ) {
        if (!MYFPEQ(dset->taxis->toff_sl[ii],0.0)) break ;
      }
      slast = ii ;

      if (slast == sfirst) {
        /* only zero or one non-zero elements  */
        pattern_unknown = 1 ;
      } else { /* if there is more than one non-zero slice time */

      /*-- Calc deltas between offsets in above range.     */
      /*-- Special case for only two elements!!!!!!!!!!!!!!*/

        if ( slast - sfirst == 1 ) {
          if (dset->taxis->toff_sl[slast] > dset->taxis->toff_sl[sfirst] ) {
            nim->slice_code = NIFTI_SLICE_SEQ_INC ;
          } else {
            nim->slice_code = NIFTI_SLICE_SEQ_DEC ;
          }
        } else { /* if there are more than two slice timing offsets */

      /*-- While storing in two variables, delta_even and  *
       *-- delta_odd, check to see if all evens are equal  *
       *-- and all odds are equal. Fail if not.            */

          odd_del = dset->taxis->toff_sl[sfirst +  1] -
                            dset->taxis->toff_sl[sfirst] ;
          even_del = dset->taxis->toff_sl[sfirst +  2] -
                       dset->taxis->toff_sl[sfirst +  1];
          for (ii = sfirst + 2 ; ii < slast - 1 ; ii += 2 ) {
            if (MYFPEQ(odd_del, dset->taxis->toff_sl[ii + 1] - dset->taxis->toff_sl[ii])) {
              if (MYFPEQ(even_del, dset->taxis->toff_sl[ii +  2] - dset->taxis->toff_sl[ii +  1])) {
                continue;
              }
            }
            pattern_unknown = 1 ;
            break ;
          }

          if (ii == slast - 1 ) {
            if (!MYFPEQ(odd_del, dset->taxis->toff_sl[ii + 1] - dset->taxis->toff_sl[ii])) {
              pattern_unknown = 1 ;
            }
          } else {
            tmp_float = (dset->taxis->toff_sl[ii] - dset->taxis->toff_sl[ii - 1]) / odd_del ;
            even_parity_sign = tmp_float / fabs (tmp_float) ;
          }

      /*-- If evens equal odds, it's NIFTI_SLICE_SEQ_INC (if *
       *-- positive) or NIFTI_SLICE_SEQ_DEC (if negative)    */
          if (!pattern_unknown ) {
            if (MYFPEQ(odd_del, even_del)) {
              if (odd_del > 0) nim->slice_code = NIFTI_SLICE_SEQ_INC ;
                else nim->slice_code = NIFTI_SLICE_SEQ_INC ;
            } else {

      /*-- Else if they ARE of opposite sign, then the           *
       *-- order is NIFTI_SLICE_ALT_INC if the sum of delta_odd  *
       *-- and delta_even is positive, and NIFTI_SLICE_ALT_DEC   *
       *-- if negative.                                          */

              if ((odd_del * even_del < 0 ) && (even_parity_sign != -1)) {
                if (odd_del + even_del > 0 ) {
                  nim->slice_code = NIFTI_SLICE_ALT_INC ;
                } else {
                  if (odd_del + even_del < 0 ) {
                    nim->slice_code = NIFTI_SLICE_ALT_DEC ;
                  } else {
                    pattern_unknown = 1 ;
                  }
                }
              } else {
                pattern_unknown = 1 ;
              }
            }
          }
        } /* if there are more than two slice timing offsets */
      } /* if there is more than one non-zero slice time */
    } else { /* time offset not exists */
      pattern_unknown = 1 ;
    }

  /*-- Now store slice_start and slice_end from the position *
   *-- of the first and last non-zero elements above.  IFF   *
   *-- we have removed at least one zero from the beginning  *
   *-- of an INC order or the end of a DEC order, then       *
   *-- shift slice_start or slice end to add exactly one zero*
   *-- back to the appropriate end.                          */
  /*-- If we've done all of this, we might as well populate  *
   *-- slice_duration as well.  It is the absolute value of  *
   *-- the delta for the sequential cases, or the sum of the *
   *-- two deltas for the alternating cases.                 */

    if (!pattern_unknown ) {
      switch (nim->slice_code) {
        case NIFTI_SLICE_SEQ_INC:
          nim->slice_duration = odd_del ;
          if (sfirst > 0) sfirst-- ;
          break ;
        case NIFTI_SLICE_ALT_INC:
          nim->slice_duration = fabs (odd_del + even_del ) ;
          if (sfirst > 0) sfirst-- ;
          break ;
        case NIFTI_SLICE_SEQ_DEC:
          nim->slice_duration = fabs (odd_del) ;
          if (slast < nim->nz - 1) slast++ ;
          break ;
        case NIFTI_SLICE_ALT_DEC:
          nim->slice_duration = fabs (odd_del + even_del ) ;
          if (slast < nim->nz - 1) slast++ ;
          break ;
        default: /* sanity check */
          fprintf(stderr,
          "++ ERROR: CANNOT WRITE NIFTI FILE; LOGIC BORKED IN SLICE TIMING\n") ;
          fprintf(stderr, "RICH HAMMETT SHOULD FIX THIS REAL SOON NOW\n") ;
          RETURN(0);
      }

      nim->slice_start = sfirst ;
      nim->slice_end = slast ;

    } else {
      nim->slice_code = NIFTI_SLICE_UNKNOWN ;
      nim->slice_start = 0 ;
      nim->slice_end = 0 ;
    } /* end the if !pattern_unknown final assignment section */

    nim->time_units = NIFTI_UNITS_SEC ;

  } else { /* if time axis not exists */
    nim->slice_dim = 0 ;
    nim->time_units = NIFTI_UNITS_UNKNOWN ;
  }

  /*-- byte order --*/

  nim->byteorder = nifti_short_order() ;

  /* KRH 7/25/05 modified to note talairach view into NIfTI file */

  if ( dset->view_type == VIEW_TALAIRACH_TYPE ) {
    nim->qform_code = NIFTI_XFORM_TALAIRACH ;
  } else {
    nim->qform_code = NIFTI_XFORM_SCANNER_ANAT ;
  }
  nim->sform_code = nim->qform_code ; /* KRH 7/6/05 - using */
           /* sform to duplicate qform for interoperability with FSL */


  /*-- odds and ends that are constant for AFNI files --*/
  nim->cal_min = nim->cal_max = 0 ;
  nim->nifti_type = 1 ;
  nim->xyz_units = NIFTI_UNITS_MM ;
  nim->num_ext = 0;
  nim->ext_list = NULL ;
  nim->iname_offset = 352 ; /* until extensions are added */
  nim->data = NULL ;

  RETURN(nim) ;
}

/*-------------------------------------------------------------------*/
/*! List of dataset attributes NOT to save in a NIfTI-1.1 file. -----*/

static char *badlist[] = {
     "IDCODE_STRING"      ,   /* this goes in the NI_group header */
     "DATASET_RANK"       ,
     "DATASET_DIMENSIONS" ,
     "TYPESTRING"         ,
     "SCENE_DATA"         ,
     "ORIENT_SPECIFIC"    ,
     "ORIGIN"             ,
     "DELTA"              ,
     "TAXIS_NUMS"         ,
     "TAXIS_FLOATS"       ,
     "TAXIS_OFFSETS"      ,
     "BYTEORDER_STRING"   ,
     "BRICK_TYPES"        ,
     "BRICK_FLOAT_FACS"   ,
     "STAT_AUX"           ,
     "LABEL_1"            ,
     "LABEL_2"            ,
     "DATASET_NAME"       ,
 NULL } ;

/*-------------------------------------------------------------------*/
/*! Create the AFNI extension string for a NIfTI-1.1 file, and insert
    this metadata into the nifti_image struct for output to disk.
    - If something bad happens, fails silently
    - 09 May 2005 - RWCox
---------------------------------------------------------------------*/

void nifti_set_afni_extension( THD_3dim_dataset *dset , nifti_image *nim )
{
   THD_datablock *blk ;
   NI_group      *ngr ;
   NI_element    *nel ;
   NI_stream      ns  ;
   char *rhs , buf[128] ;
   int ii,bb , npart,*bpart ;

   if( nim == NULL                     ) return ;  /* stupid or evil caller */
   if( AFNI_yesenv("AFNI_NIFTI_NOEXT") ) return ;  /* not allowed */

   /** write all dataset 'attributes' into a NIML group */

   ngr = THD_nimlize_dsetatr( dset ) ;
   if( ngr == NULL ) return ;            /* bad */
   NI_rename_group( ngr , "AFNI_attributes" ) ;

   /* 12 May 2005: add a signature to check the file on input to AFNI */

   sprintf(buf,"%d,%d,%d,%d,%d,%d" ,
           nim->nx, nim->ny, nim->nz, nim->nt, nim->nu, nim->datatype ) ;
   NI_set_attribute( ngr , "NIfTI_nums" , buf ) ;

   /** now, scan attribute elements in the group, and mark some
       of them as being useless or redundant in the NIfTI world **/

   npart = ngr->part_num ;
   bpart = (int *)calloc(sizeof(int),npart) ;
   for( ii=0 ; ii < npart ; ii++ ){
     if( ngr->part_typ[ii] != NI_ELEMENT_TYPE ) continue ;
     nel = (NI_element *) ngr->part[ii] ;
     if( strcmp(nel->name,"AFNI_atr") != 0 )    continue ;
     rhs = NI_get_attribute( nel , "AFNI_name" ) ;
     if( rhs == NULL )                          continue ;

     for( bb=0 ; badlist[bb] != NULL ; bb++ )
       if( strcmp(rhs,badlist[bb]) == 0 ){ bpart[ii] = 1; break; }
   }

   /** remove marked attributes from the NIML group **/

   for( ii=npart-1 ; ii >= 0 ; ii-- ){
     if( bpart[ii] )
       NI_remove_from_group( ngr , ngr->part[ii] ) ;
   }
   free((void *)bpart) ;  /* done with this */
   if( ngr->part_num <= 0 ){ NI_free_element(ngr); return; }

   /** format into a character string to put in the NIfTI-1.1 extension **/

   ns = NI_stream_open( "str:" , "w" ) ;
   NI_stream_writestring( ns , "<?xml version='1.0' ?>\n" ) ;
   NI_write_element( ns , ngr , NI_TEXT_MODE ) ;
   rhs = NI_stream_getbuf( ns ) ;

   /** write contents of the string into the nifti_image struct **/

   nifti_add_extension( nim , rhs , strlen(rhs)+1 , NIFTI_ECODE_AFNI ) ;

   NI_stream_close(ns) ;   /* frees the string buffer, too */
   NI_free_element(ngr) ;  /* done with this trashola */
   return ;
}
