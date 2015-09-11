#include "mrilib.h"
#include "thd_niftiwrite.h"

/* prototypes */

nifti_image * populate_nifti_image(THD_3dim_dataset *dset, niftiwr_opts_t options) ;

void nifti_set_afni_extension(THD_3dim_dataset *dset,nifti_image *nim) ;

static int get_slice_timing_pattern( float * times, int len, float * delta );
static int needs_conversion_to_float(THD_3dim_dataset *dset, int warn);
static int space_to_NIFTI_code(THD_3dim_dataset *dset);
extern int THD_space_code(char *space);

/*******************************************************************/
/*!  Write an AFNI dataset as a NIfTI file.
     - dset  = AFNI dataset
     - options = structure with options to control the output

   Return value is 1 if went OK, 0 if not.
-------------------------------------------------------------------*/

int THD_write_nifti( THD_3dim_dataset *din, niftiwr_opts_t options )
{
  THD_3dim_dataset * dset=NULL;
  nifti_image *nim ;
  nifti_brick_list nbl ;
  int ii ;
  char *fname , *cpt ;

ENTRY("THD_write_nifti") ;

  nifti_set_debug_level(options.debug_level) ;

   /*-- check inputs for goodness --*/

  fname = nifti_strdup(options.infile_name);

  if( !THD_filename_ok(fname) || fname[0] == '-' ){
    ERROR_message("Illegal filename for NIfTI output: %s\n",
                  (fname != NULL) ? fname : "(null)" ) ;
    RETURN(0) ;
  }

  /* if we need a float dataset, make one (insted of failing)
   * (wasteful, but simple and effective)  6 Sep 2012 [rickr] */
  if( needs_conversion_to_float(din, 1) ) {
     dset = EDIT_full_copy(din, NULL);
     EDIT_floatize_dataset(dset);
     tross_Copy_History(din, dset); /* ZSS May 23 2013 */

     if( ! ISVALID_DSET(dset) ) {
       ERROR_message("failed to copy dset for NIfTI write\n");
       RETURN(0);
     }
  } else dset = din;

  if( !ISVALID_DSET(dset) ){
    ERROR_message("Illegal input dataset for NIfTI output: %s\n", fname) ;
    RETURN(0) ;
  }

  /*-- load dataset from disk, if need be --*/

  DSET_load(dset) ;
  if( !DSET_LOADED(dset) ){
    ERROR_message(
            "Can't write NIfTI file since dataset isn't loaded: %s\n", fname) ;
    RETURN(0) ;
  }

  nim = populate_nifti_image(dset,options) ;
  if( !nim ) RETURN(0) ;   /* catch failure    6 Apr 2006 [rickr] */

  /*-- construct filename --*/

  nim->fname = malloc( strlen(fname)+16 ) ;
  nim->iname = malloc( strlen(fname)+16 ) ;
  strcpy(nim->fname,fname) ;
  strcpy(nim->iname,fname) ;

  /* 11 Oct 2005: Allow for .hdr/.img file outputs -- RWCox */

  cpt = nifti_find_file_extension( nim->iname ) ;
  if( cpt != NULL && strcmp(cpt,".hdr") == 0 ){
    nim->nifti_type = 2 ;   /* indicate 2 file output    */
    memcpy(cpt,".img",4) ;  /* convert .hdr name to .img */
  }

  /*-- construct nifti_brick_list of pointers to data briks */

  if( options.debug_level > 2 ) nifti_image_infodump(nim) ;
  nbl.bricks = (void **) malloc ( DSET_NVALS(dset) * sizeof(void*) ) ;
  nbl.nbricks = DSET_NVALS(dset) ;
  nbl.bsize = DSET_BRICK_BYTES(dset,0) ;
  for (ii = 0 ; ii < DSET_NVALS(dset) ; ii++ )
    nbl.bricks[ii] = DSET_ARRAY(dset,ii) ;

  /*-- 13 Mar 2006: check disk space --*/

  { FILE *fp = fopen(nim->fname,"ab") ;
    int mm   = THD_freemegabytes(nim->fname) ;
    int rr   = (int)(dset->dblk->total_bytes/(1024*1024)) ;
    if( fp != NULL ) fclose(fp) ;
    if( mm >= 0 && mm <= rr )
      WARNING_message("Disk space: writing dataset %s (%d MB),"
                      " but only %d free MB on disk"                   ,
              nim->fname , rr , mm ) ;
  }

  /*-- use handy-dandy library function to write out data */

  nifti_set_afni_extension( dset , nim ) ;  /* 09 May 2005 - RWCox */

  nifti_image_write_bricks (nim, &nbl ) ;

  /* if we made a float copy, nuke it */
  if( dset != din ) THD_delete_3dim_dataset(dset, True) ;

  RETURN(1) ;
}

/* if the dataset has inconsistent types or scale factors, then it needs
 * to be converted fo floats in order to write        6 Sep 2012 [rickr] */
static int needs_conversion_to_float(THD_3dim_dataset *dset, int warn)
{
  float fac0;                 /* was int, ick!   11 Sep 2015 [rickr] */
  int   type0, ii;

  ENTRY("needs_conversion_to_float");

  if( ! ISVALID_DSET(dset) )   RETURN(0);

  if( dset->dblk->nvals <= 1 ) RETURN(0);       /* nothing can vary */

  type0 = DSET_BRICK_TYPE(dset,0) ;
  fac0  = DSET_BRICK_FACTOR(dset,0) ;

  for( ii=1 ; ii < DSET_NVALS(dset) ; ii++ ){
     if( DSET_BRICK_TYPE(dset,ii) != type0) {
        if( warn )
          WARNING_message("varying brick types, writing NIfTI as float");
        RETURN(1);
     }

     if( DSET_BRICK_FACTOR(dset,ii) != fac0) {
        if( warn )
          WARNING_message("varying brick factors, writing NIfTI as float");
        RETURN(1);
     }
  }

  RETURN(0);
}


/*******************************************************************/

nifti_image * populate_nifti_image(THD_3dim_dataset *dset, niftiwr_opts_t options)
{
  int nparam, type0 , ii , jj;
  int nif_x_axnum=0, nif_y_axnum=0, nif_z_axnum=0;
  int slast, sfirst ;
  int pattern, tlen ;
  nifti_image *nim ;
  char axcode[3], axsign[3] ;
  float axstep[3] , axstart[3] ;
  int   axnum[3] ;
  float fac0 ;
  double dumqx, dumqy, dumqz, dumdx, dumdy, dumdz ;
  float *tlist, fsdur;

ENTRY("populate_nifti_image") ;
  /*-- create nifti_image structure --*/

  nim = (nifti_image *) calloc( 1 , sizeof(nifti_image) ) ;
  if( !nim ) {
    fprintf(stderr, "** ERROR: failed to allocate nifti image\n");
    RETURN(NULL) ;
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
        "** ERROR: CANNOT WRITE NIfTI FILE; BRICK DATA TYPES NOT CONSISTENT\n") ;
        RETURN(NULL);
      } else if( DSET_BRICK_FACTOR(dset,ii) != fac0) {
        fprintf(stderr,
        "** ERROR: CANNOT WRITE NIfTI FILE; BRICK FACTORS NOT CONSISTENT\n") ;
        fprintf(stderr,
        "   (consider transforming to a float dataset before performing\n"
        "    this operation, or consider '3dAFNItoNIFTI -float')\n"
        ) ;
        RETURN(NULL);
      }
    }
  } else {  /* we only have one brick */
    STATUS("3D dataset") ;
    if( options.debug_level > 1 ) fprintf(stderr,"-- PNI: write one brick\n") ;
    type0 = DSET_BRICK_TYPE(dset,0);
    fac0  = DSET_BRICK_FACTOR(dset,0) ;
    if (ISFUNC(dset)) {
      STATUS("functional dataset") ;
      if( options.debug_level > 1 )
        fprintf(stderr,"-- PNI: functional brick\n") ;
      nim->intent_code = DSET_BRICK_STATCODE(dset,0);
      if (nim->intent_code < 0) nim->intent_code = dset->func_type ;
      if (nim->intent_code < 0) nim->intent_code = NIFTI_INTENT_NONE ;
      /* 3dbucket func_type=FUNC_BUCK_TYPE becomes NIFTI_INTENT_NORMAL, which
         AFNI whines about...  3dD uses 'none' for stat type on betas, so
         stick with that                        6 Jul 2013 [rickr] */
      if (nim->intent_code >= FUNC_BUCK_TYPE)
        nim->intent_code = NIFTI_INTENT_NONE ;
      if( options.debug_level > 1 )
        fprintf(stderr,"-- PNI: stat code = %d !!!\n",nim->intent_code) ;
      if(PRINT_TRACING){
        char str[256]; sprintf(str,"intent_code = %d",nim->intent_code);STATUS(str);
      }
      if (nim->intent_code > -1) {
        nparam = FUNC_need_stat_aux[nim->intent_code];
        /* statpars should be 0-based             20 Oct 2010 [rickr] */
        if (nparam >= 1) nim->intent_p1 = DSET_BRICK_STATPAR(dset,0,0);
        if (nparam >= 2) nim->intent_p2 = DSET_BRICK_STATPAR(dset,0,1);
        if (nparam == 3) nim->intent_p3 = DSET_BRICK_STATPAR(dset,0,2);
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
               "** ERROR: Can't write NIfTI file since dataset is RGBA: %s\n",
               options.infile_name) ;
      RETURN(NULL) ;
      break;
    default:
      fprintf(stderr,
               "** ERROR: Can't write NIfTI file since datatype is unknown: %s\n",
               options.infile_name) ;
      RETURN(NULL) ;
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
#endif

  nim->qoffset_x =  -axstart[nif_x_axnum] ;
  nim->qoffset_y =  -axstart[nif_y_axnum];
  nim->qoffset_z =  axstart[nif_z_axnum];

#if 0
  nim->qoffset_x =  -axstart[0] ;
  nim->qoffset_y =  -axstart[1];
  nim->qoffset_z =  axstart[2];
#endif

  nim->qto_xyz.m[0][3] = nim->qoffset_x ;
  nim->qto_xyz.m[1][3] = nim->qoffset_y ;
  nim->qto_xyz.m[2][3] = nim->qoffset_z ;


  /*-- from the same above info, set the sform matrix to equal the qform --*/
  /* KRH 7/6/05 - using sform to duplicate qform for
                           interoperability with FSL                       */
  /* update with oblique transformation if available DRG 24 May 2007 */
  /* check for valid transformation matrix */
  if(!ISVALID_MAT44(dset->daxes->ijk_to_dicom_real)) {
     nim->sto_xyz = nim->qto_xyz; /* copy qform to sform */
  }
  else {
      /* fill in sform with AFNI daxes transformation matrix */
      /* n2   10 Jul, 2015 [rickr] */
      nifti_mat44_to_dmat44(&dset->daxes->ijk_to_dicom_real, &nim->sto_xyz);
     /* negate first two rows of sform for NIFTI - LPI standard versus
                                            AFNI RAI "DICOM" standard */
     for( ii = 0; ii < 2; ii++) {
	for (jj = 0 ; jj < 4; jj++) {
            nim->sto_xyz.m[ii][jj] = -(nim->sto_xyz.m[ii][jj]);
	}
     }
     /* update qform too with struct copy from sform*/
     nim->qto_xyz= nim->sto_xyz ;

  }

  /*-- from the above info, calculate the quaternion qform --*/

  STATUS("set quaternion") ;

  /* n2   10 Jul, 2015 [rickr] */
  nifti_dmat44_to_quatern( nim->qto_xyz ,
                          &nim->quatern_b, &nim->quatern_c, &nim->quatern_d,
                          &dumqx, &dumqy, &dumqz, &dumdx, &dumdy, &dumdz,
                          &nim->qfac ) ;

  /*-- verify dummy quaternion parameters --*/

  if( options.debug_level > 2 )
    /* n2   10 Jul, 2015 [rickr] */
    fprintf(stderr,"++ Quaternion check:\n"
          "%f , %f\n %f , %f\n %f , %f\n %f , %f\n %f , %f\n %f , %f\n; %f\n",
           nim->qoffset_x, dumqx, nim->qoffset_y,dumqy, nim->qoffset_z, dumqz ,
           nim->dx, dumdx , nim->dy, dumdy , nim->dz, dumdz, nim->qfac ) ;

  /*-- calculate inverse qform            --*/

  /* n2   10 Jul, 2015 [rickr] */
  nim->qto_ijk = nifti_dmat44_inverse( nim->qto_xyz ) ;

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

      tlist = dset->taxis->toff_sl;
      for (ii = 0 ; ii < nim->nz ; ii++ ) {
        if (!MYFPEQ(tlist[ii],0.0)) break ;
      }
      sfirst = ii ;
      for (ii = nim->nz - 1 ; ii >= sfirst ; ii-- ) {
        if (!MYFPEQ(tlist[ii],0.0)) break ;
      }
      slast = ii ;

      /* pattern check re-written to deal with including zeros */
      /* on either end                     14 Jun 2006 [rickr] */

      pattern = NIFTI_SLICE_UNKNOWN;

      /* do we have all zeros? */
      if( sfirst == slast && MYFPEQ(tlist[sfirst],0.0) ) {
         nim->slice_duration = 0.0;
      } else { /* see if there is a known pattern in the list */
         tlen = slast-sfirst+2;

         /* try including leading adjacent zero in the pattern, first */
         if( sfirst > 0 ) {
            /* n2   10 Jul, 2015 [rickr] */
            pattern = get_slice_timing_pattern(tlist+sfirst-1, tlen, &fsdur);
            nim->slice_duration = (double)fsdur;
            if( pattern != NIFTI_SLICE_UNKNOWN ) sfirst--;
         }

         /* try including trailing adjacent zero in the pattern, next */
         if( pattern == NIFTI_SLICE_UNKNOWN && slast < nim->nz-1 ) {
            pattern = get_slice_timing_pattern(tlist+sfirst, tlen, &fsdur);
            nim->slice_duration = (double)fsdur;
            if( pattern != NIFTI_SLICE_UNKNOWN ) slast++;
         }

         /* if no pattern yet, try list without zeros */
         if( pattern == NIFTI_SLICE_UNKNOWN ) {
            pattern = get_slice_timing_pattern(tlist+sfirst, tlen-1, &fsdur);
            nim->slice_duration = (double)fsdur;
         }

         if( pattern == NIFTI_SLICE_UNKNOWN ) {
            nim->slice_code = pattern ;
            nim->slice_start = 0 ;
            nim->slice_end = 0 ;
            nim->slice_duration = 0.0 ;
         } else {
            nim->slice_start = sfirst ;
            nim->slice_end = slast ;
            nim->slice_code = pattern;
         }

         if( options.debug_level > 1)
            fprintf(stderr,"+d timing pattern '%s', slice %d to %d, stime %f\n",
               nifti_slice_string(pattern), sfirst, slast, nim->slice_duration);
      }

      /* if toffset is 0 and the timing patter is known and the minimum
       * slice offset is positive, the toffset to that minimum
       *                                        12 Oct 2007 [rickr] */
      if( nim->toffset == 0.0 && nim->slice_code != NIFTI_SLICE_UNKNOWN ){
         float tmin = tlist[0];
         for (ii = 1 ; ii < nim->nz ; ii++ )
            if( tlist[ii] < tmin ) tmin = tlist[ii] ;
         if( tmin > 0.0 ) nim->toffset = tmin ;
      }
    }

    nim->time_units = NIFTI_UNITS_SEC ;

  } else { /* if time axis not exists */
    nim->slice_dim = 0 ;
    nim->time_units = NIFTI_UNITS_UNKNOWN ;
  }

  /*-- byte order --*/

  nim->byteorder = nifti_short_order() ;

  /* KRH 7/25/05 modified to note talairach view into NIfTI file */

  nim->qform_code = space_to_NIFTI_code(dset);

#if 0
  if ( dset->view_type == VIEW_TALAIRACH_TYPE ) {
    nim->qform_code = NIFTI_XFORM_TALAIRACH ;
  } else {
    nim->qform_code = NIFTI_XFORM_SCANNER_ANAT ;
  }
#endif

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

   /* n2   10 Jul, 2015 [rickr] */
   sprintf(buf,"%ld,%ld,%ld,%ld,%ld,%d" ,
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


/*! given a list of floats, detect any slice timing pattern
 *                                                14 Jun 2006 [rickr]
 *
 *    - if a pattern is found and delta is set, return the time delta
 *    - return one of:
 *        NIFTI_SLICE_UNKNOWN,
 *        NIFTI_SLICE_SEQ_INC,  NIFTI_SLICE_SEQ_DEC,
 *        NIFTI_SLICE_ALT_INC,  NIFTI_SLICE_ALT_DEC,
 *        NIFTI_SLICE_ALT_INC2, NIFTI_SLICE_ALT_DEC2,
 */
static int get_slice_timing_pattern( float * times, int len, float * delta )
{
   float * flist, diff;
   int   * ilist;
   int     c, index, pattern;

ENTRY("get_slice_timing_pattern");

   if( delta ) *delta = 0.0;  /* init, in case of early return */
   if( ! times || len < 2 ) RETURN(NIFTI_SLICE_UNKNOWN);

   /* if the length is very short, deal with it separately */
   if( len == 2 ){
      if( delta ) *delta = fabs(times[1]-times[0]);
      if( times[1] > times[0] ) RETURN(NIFTI_SLICE_SEQ_INC);
      else                      RETURN(NIFTI_SLICE_SEQ_DEC);
   }

   /*** sort the list, and look for a linear pattern ***/

   /* duplicate list */
   flist = (float *)malloc(len * sizeof(float));
   ilist = (int   *)malloc(len * sizeof(int));
   if(!flist || !ilist) {   /* yeah, lazy... */
      ERROR_message(" GSTP: cannot dupe timing list\n");
      RETURN(NIFTI_SLICE_UNKNOWN);
   }
   memcpy(flist, times, len*sizeof(float));
   for(c = 0; c < len; c++) ilist[c] = c;  /* init ilist with current indices */

   /* sort flist, with ilist returning original indices */
   qsort_floatint(len, flist, ilist);

   /* and check for a fixed difference */
   diff = flist[1] - flist[0];
   pattern = 1;
   for( c = 1; c < len-1; c++ )
     if( !MYFPEQ(diff, (flist[c+1]-flist[c])) ) { pattern = 0; break; }

   /* if no pattern, just return failure */
   if( !pattern ) {
      free(flist);  free(ilist);  RETURN(NIFTI_SLICE_UNKNOWN);
   }

   /* we have linear offsets, now see if the slices match a known pattern */
   /* repeatedly: init to a pattern, and see if it fails */
   
   /* SEQ_INC  (0,1,2,3...,l-1) */
   pattern = NIFTI_SLICE_SEQ_INC;  index = 0;
   for( c = 0; c < len; c++ ) {
      if( ilist[c] != index ) { pattern = NIFTI_SLICE_UNKNOWN;  break; }
      index++;
   }

   if( pattern == NIFTI_SLICE_UNKNOWN ) { /* (l-1,l-2,...2,1,0) */
       pattern = NIFTI_SLICE_SEQ_DEC;  index = len-1;
       for( c = 0; c < len; c++ ) {
          if( ilist[c] != index ) { pattern = NIFTI_SLICE_UNKNOWN;  break; }
          index--;
       }
   }

   if( pattern == NIFTI_SLICE_UNKNOWN ) { /* (0,2,4,6,...,1,3,5,...) */
       pattern = NIFTI_SLICE_ALT_INC;  index = 0;
       for( c = 0; c < len; c++ ) {
          if( ilist[c] != index ) { pattern = NIFTI_SLICE_UNKNOWN;  break; }
          index += 2;  if( index >= len ) index = 1;  /* so no parity issue */
       }
   }

   if( pattern == NIFTI_SLICE_UNKNOWN ) { /* (l-1,l-3,...1/0,l-2,l-4,...,0/1) */
       pattern = NIFTI_SLICE_ALT_DEC;  index = len-1;
       for( c = 0; c < len; c++ ) {
          if( ilist[c] != index ) { pattern = NIFTI_SLICE_UNKNOWN;  break; }
          index -= 2;  if( index < 0 ) index = len-2;
       }
   }

   if( pattern == NIFTI_SLICE_UNKNOWN ) { /* (1,3,5,...,0,2,4...) */
       pattern = NIFTI_SLICE_ALT_INC2;  index = 1;
       for( c = 0; c < len; c++ ) {
          if( ilist[c] != index ) { pattern = NIFTI_SLICE_UNKNOWN;  break; }
          index += 2;  if( index >= len ) index = 0;
       }
   }

   if( pattern == NIFTI_SLICE_UNKNOWN ) { /* (l-2,l-4,...4,2,0,l-1,...,5,3,1) */
       pattern = NIFTI_SLICE_ALT_DEC2;  index = len-2;
       for( c = 0; c < len; c++ ) {
          if( ilist[c] != index ) { pattern = NIFTI_SLICE_UNKNOWN;  break; }
          index -= 2;  if( index < 0 ) index = len-1;
       }
   }

   if( delta && pattern != NIFTI_SLICE_UNKNOWN ) *delta = diff;

   /** done, whatever the case may be **/
   free(flist);  free(ilist);
   RETURN(pattern);
}

/* set NIFTI sform code  based on atlas space */
static int space_to_NIFTI_code(THD_3dim_dataset *dset)
{
    char *genspc = NULL;
    /* several changes for generic spaces and defaults 05/02/2012 -mod drg */
    /* use generic space or space of dataset to choose xform code */
    genspc = THD_get_generic_space(dset);
    if(genspc == NULL)
       return(NIFTI_XFORM_SCANNER_ANAT);

    if (strcmp(genspc,"TLRC") == 0) {
      return(NIFTI_XFORM_TALAIRACH);
    }
    if (strcmp(genspc,"MNI") == 0) {
      return(NIFTI_XFORM_MNI_152);
    }
    /* call MNI_ANAT as aligned to something else*/
    if (strcmp(genspc,"MNI_ANAT") == 0) {
      return(NIFTI_XFORM_ALIGNED_ANAT);
    }
    if((strcmp(genspc,"ORIG") == 0) ||
       (strcmp(genspc,"ACPC") == 0)){
      return(NIFTI_XFORM_SCANNER_ANAT);
    }
    /* make catch-all for other spaces as an aligned space
       alternative is use SCANNER_ANAT again or UNKNOWN */
    return(NIFTI_XFORM_ALIGNED_ANAT);
}
