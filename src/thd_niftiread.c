#include "mrilib.h"

#ifndef DONT_INCLUDE_ANALYZE_STRUCT
#define DONT_INCLUDE_ANALYZE_STRUCT
#endif
#include "nifti2_io.h"   /** will include nifti1.h **/
static void NIFTI_code_to_space(int code,THD_3dim_dataset *dset);
static int NIFTI_code_to_view(int code);
static int NIFTI_default_view();
extern char *THD_get_space(THD_3dim_dataset *dset);

/*******************************************************************/
/********** 26 Aug 2003: read a NIFTI-1 file as a dataset **********/
/*******************************************************************/

THD_3dim_dataset * THD_open_nifti( char *pathname )
{
   THD_3dim_dataset *dset=NULL ;
   nifti_image *nim ;
   int ntt , nbuc , nvals ;
   int use_qform = 0 , use_sform = 0, form_code = 0 ;
   int statcode = 0 , datum , iview , ibr ;
   int scale_data = 0 ;  /* flag based on scl_slope and inter  20 Jun 2008 */
   int xform_data = 0;
   THD_ivec3 orixyz , nxyz ;
   THD_fvec3 dxyz , orgxyz ;
   THD_mat33 R ;
   mat44 ijk_to_dicom44 ;
   char *ppp , prefix[THD_MAX_PREFIX] ;
   char form_priority = 'S' ;             /* 23 Mar 2006 */
   static int n_xform_warn=0;
   
ENTRY("THD_open_nifti") ;

   /*-- open input file --*/

   { /* set the nifti_io debug level       8 Apr 2005 [rickr] */
      char * ept = my_getenv("AFNI_NIFTI_DEBUG");
      if( ept != NULL ) nifti_set_debug_level(atoi(ept));
   }

   nifti_set_alter_cifti(1) ;  /* if CIFTI, shift dims   23 Jul 2015 [rickr] */
   nim = nifti_image_read( pathname, 0 ) ;

   if( nim == NULL || nim->nifti_type == 0 ) RETURN(NULL) ;

   /*-- extract some useful AFNI-ish information from the nim struct --*/

   /* we must have at least 2 spatial dimensions */

   /* this should be okay                 11 Jun 2007 */
   /* if( nim->nx < 2 || nim->ny < 2 ) RETURN(NULL) ; */

   /* 4th dimension = time; 5th dimension = bucket:
      these are mutually exclusive in AFNI at present */

   ntt = nim->nt ; nbuc = nim->nu ;

   /* nt and nu might be 0 now (see irritating niftilib 1.17 update)  */
   /* so ensure that ntt and nbuc are positive    02 Mar 2006 [rickr] */

   if( ntt  <= 0 ) ntt  = 1;
   if( nbuc <= 0 ) nbuc = 1;

   if( nim->nz <= 0 ) nim->nz = 1 ;  /* 03 Mar 2006: RWCox */

   if( ntt > 1 && nbuc > 1 ){
     fprintf(stderr,
             "** AFNI can't deal with 5 dimensional NIfTI(%s)\n",
             pathname ) ;
     RETURN(NULL) ;
   }

   nvals = MAX(ntt,nbuc) ;

   /* collapse higher-dimensional datasets    23 Jul 2015 [rickr] */
   /* (this includes CIFTI)                                       */
   if( nim->nv > 1 ) nvals *= nim->nv;
   if( nim->nw > 1 ) nvals *= nim->nw;
   if( ntt > 1 ) ntt = nvals;
   else          nbuc = nvals;

   /* determine type of dataset values:
      if we are scaling, or if the data type in the NIfTI file
      is something AFNI can't handle, then the result will be floats */

   /* do not scale if slope is 0 or if slope is 1 and inter is 0 */
   if( !isfinite(nim->scl_slope) || !isfinite(nim->scl_inter) ){
      fprintf(stderr,"** bad scl_slope and inter = %f, %f, ignoring...\n",
              nim->scl_slope, nim->scl_inter);
   } else {
       scale_data = nim->scl_slope != 0.0 &&
                        (nim->scl_slope != 1.0 || nim->scl_inter != 0.0) ;
   }
   { char *eee = getenv("AFNI_NIFTI_SCALE") ;
     if( eee != NULL && toupper(*eee) == 'N' ) scale_data = 0 ;
   }

   switch( nim->datatype ){
     default:
       fprintf(stderr,
               "** AFNI can't handle NIFTI datatype=%d (%s) in file %s\n",
               nim->datatype, nifti_datatype_string(nim->datatype), pathname );
       RETURN(NULL) ;
     break ;

     case DT_UINT8:     datum = scale_data ? MRI_float : MRI_byte  ;
                        xform_data = scale_data;
                        break ;
     case DT_INT16:     datum = scale_data ? MRI_float : MRI_short ;
                        xform_data = scale_data;
                        break ;
     case DT_FLOAT32:   datum = MRI_float   ; break ;
     case DT_COMPLEX64: datum = MRI_complex ; break ;
     case DT_RGB24:     datum = MRI_rgb     ; break ;

     case DT_INT8:      /* NIfTI-1 data types that AFNI can't handle directly */
     case DT_UINT16:
     case DT_INT32:
     case DT_UINT32:
     case DT_FLOAT64:   datum = MRI_float ; xform_data = 1 ; break ;

#if 0
     case DT_COMPLEX128:  /* this case would be too much like real work */
       fprintf(stderr,
               "** AFNI convert NIFTI_datatype=%d (%s) in file %s to COMPLEX64\n",
               nim->datatype, nifti_datatype_string(nim->datatype), pathname );
       datum = MRI_complex ;
     break ;
#endif
   }

   if( xform_data && !AFNI_noenv("AFNI_NIFTI_TYPE_WARN")) {
      if (!n_xform_warn || AFNI_yesenv("AFNI_NIFTI_TYPE_WARN")) {/* ZSS 04/11 */
         fprintf(stderr,
             "** AFNI converts NIFTI_datatype=%d (%s) in file %s to FLOAT32\n",
             nim->datatype, nifti_datatype_string(nim->datatype), pathname );
         if (!AFNI_yesenv("AFNI_NIFTI_TYPE_WARN")) {
            fprintf(stderr,
               "     Warnings of this type will be muted for this session.\n"
      "     Set AFNI_NIFTI_TYPE_WARN to YES to see them all, NO to see none.\n");
         }
      }
      ++n_xform_warn;
   }
   /* check for statistics code */

   if( nim->intent_code >= NIFTI_FIRST_STATCODE &&
       nim->intent_code <= NIFTI_LAST_STATCODE    ){

     if( nim->intent_code > FUNC_PT_TYPE ){
       fprintf(stderr,
               "** AFNI doesn't understand NIFTI statistic type %d (%s) in file %s\n",
               nim->intent_code , nifti_intent_string(nim->intent_code) , pathname ) ;
     } else {
       statcode = nim->intent_code ;
       if( nbuc > 1 ){
         fprintf(stderr,
                 "** AFNI doesn't support NIFTI voxel-dependent statistic parameters"
                 " in file %s\n" , pathname ) ;
         statcode = 0 ;
       }
     }
   }

   /* 23 Mar 2006: set qform or sform as having priority -- RWCox */

   ppp = my_getenv("NIFTI_FORM_PRIORITY") ;
   if( ppp == NULL ) ppp = getenv("AFNI_FORM_PRIORITY") ;
   if( ppp == NULL ) ppp = getenv("AFNI_NIFTI_PRIORITY") ;
   if( ppp == NULL ) ppp = getenv("AFNI_NIFTI_FORM") ;
   if( ppp == NULL ) ppp = getenv("AFNI_NIFTI_FORM_PRIORITY") ;
   if( ppp != NULL ){
     char fp = toupper(*ppp) ;
     if( fp == 'S' || fp == 'Q' ) form_priority = fp ;
     else WARNING_message("Illegal NIFTI_FORM_PRIORITY='%s'",ppp) ;
   }

   /** 24 Mar 2006: check determs of qform and sform, if have both **/

   if( nim->qform_code > 0 && nim->sform_code > 0 ){
     float qdet , sdet ;
     LOAD_MAT(R, nim->qto_xyz.m[0][0] ,
                 nim->qto_xyz.m[0][1] ,
                 nim->qto_xyz.m[0][2] ,
                 nim->qto_xyz.m[1][0] ,
                 nim->qto_xyz.m[1][1] ,
                 nim->qto_xyz.m[1][2] ,
                 nim->qto_xyz.m[2][0] ,
                 nim->qto_xyz.m[2][1] ,
                 nim->qto_xyz.m[2][2]  ) ; qdet = MAT_DET(R) ;

     LOAD_MAT(R, nim->sto_xyz.m[0][0] ,
                 nim->sto_xyz.m[0][1] ,
                 nim->sto_xyz.m[0][2] ,
                 nim->sto_xyz.m[1][0] ,
                 nim->sto_xyz.m[1][1] ,
                 nim->sto_xyz.m[1][2] ,
                 nim->sto_xyz.m[2][0] ,
                 nim->sto_xyz.m[2][1] ,
                 nim->sto_xyz.m[2][2]  ) ; sdet = MAT_DET(R) ;

     if( qdet*sdet < 0.0f )
       WARNING_message("NIfTI('%s'): Qform/Sform handedness differ; %c wins!",
                       pathname , form_priority ) ;
   }

   /* KRH 07/11/05 -- adding ability to choose spatial transform
      from the options of qform, sform, bothform, or noform.

      If qform is present, it will be used.

      If qform is absent, but sform present, then the sform
        will be modified to be an orthogonal rotation and used.

      If both qform and sform are absent, then we will have
        an error.

      Previously assumed qform present.  */

   /* 23 Mar 2006: use form_priority to choose between them */

   if ((nim->qform_code > 0) && (nim->sform_code > 0) ) {
     if( form_priority == 'Q' )   { use_qform = 1 ; use_sform = 0 ; }
     else                         { use_qform = 0 ; use_sform = 1 ; }
   } else if (nim->qform_code > 0){ use_qform = 1 ; use_sform = 0 ; }
     else if (nim->sform_code > 0){ use_qform = 0 ; use_sform = 1 ; }
     else {
                                    use_qform = 0 ; use_sform = 0 ;
     WARNING_message(
      "NO spatial transform (neither qform nor sform), in NIfTI file '%s'" ,
      pathname ) ;
   }

   /** now take NIfTI-1.1 coords and transform to AFNI codes **/

   if (use_qform) {

     float orgx, orgy, orgz ;

     form_code = nim->qform_code;

     /* determine orientation from the qto_xyz matrix,
      which transforms (i,j,k) voxel indexes to (x,y,z) LPI coordinates */

     LOAD_MAT(R, -nim->qto_xyz.m[0][0] ,  /* negate x and y   */
                 -nim->qto_xyz.m[0][1] ,  /* coefficients,    */
                 -nim->qto_xyz.m[0][2] ,  /* since AFNI works */
                 -nim->qto_xyz.m[1][0] ,  /* with RAI coords, */
                 -nim->qto_xyz.m[1][1] ,  /* but NIFTI uses   */
                 -nim->qto_xyz.m[1][2] ,  /* LPI coordinates. */
                  nim->qto_xyz.m[2][0] ,  /* [Which is my own] */
                  nim->qto_xyz.m[2][1] ,  /* [damn fault!!!!!] */
                  nim->qto_xyz.m[2][2]  ) ;

     LOAD_MAT44(ijk_to_dicom44,
                 -nim->qto_xyz.m[0][0] ,  /* negate x and y   */
                 -nim->qto_xyz.m[0][1] ,  /* coefficients,    */
                 -nim->qto_xyz.m[0][2] ,  /* since AFNI works */
                 -nim->qto_xyz.m[0][3] ,
                 -nim->qto_xyz.m[1][0] ,  /* with RAI coords, */
                 -nim->qto_xyz.m[1][1] ,  /* but NIFTI uses   */
                 -nim->qto_xyz.m[1][2] ,  /* LPI coordinates. */
                 -nim->qto_xyz.m[1][3] ,
                  nim->qto_xyz.m[2][0] ,  /* [Which is my own] */
                  nim->qto_xyz.m[2][1] ,  /* [damn fault!!!!!] */
                  nim->qto_xyz.m[2][2] ,  
                  nim->qto_xyz.m[2][3] ) ;

     orixyz = THD_matrix_to_orientation( R ) ;   /* compute orientation codes */

     iview = NIFTI_code_to_view(nim->qform_code);

     /* load the offsets and the grid spacings */

     if (ORIENT_xyz[orixyz.ijk[0]] == 'z' )  {
       orgx = nim->qto_xyz.m[ORIENT_xyzint[orixyz.ijk[0]] - 1][3] ;
     } else {
       orgx = - nim->qto_xyz.m[ORIENT_xyzint[orixyz.ijk[0]] - 1][3] ;
     }

     if (ORIENT_xyz[orixyz.ijk[1]] == 'z' )  {
       orgy = nim->qto_xyz.m[ORIENT_xyzint[orixyz.ijk[1]] - 1][3] ;
     } else {
       orgy = - nim->qto_xyz.m[ORIENT_xyzint[orixyz.ijk[1]] - 1][3] ;
     }

     if (ORIENT_xyz[orixyz.ijk[2]] == 'z' )  {
       orgz = nim->qto_xyz.m[ORIENT_xyzint[orixyz.ijk[2]] - 1][3] ;
     } else {
       orgz = - nim->qto_xyz.m[ORIENT_xyzint[orixyz.ijk[2]] - 1][3] ;
     }


     LOAD_FVEC3( orgxyz ,  orgx ,
                           orgy ,
                           orgz  ) ;
#if 0
     LOAD_FVEC3( orgxyz , -nim->qto_xyz.m[0][3] ,    /* again, negate  */
                        -nim->qto_xyz.m[1][3] ,    /* x and y coords */
                         nim->qto_xyz.m[2][3]  ) ;
#endif

     /* AFNI space units are always mm */

     if( nim->xyz_units == NIFTI_UNITS_METER ){
       nim->dx *= 1000.0 ; nim->dy *= 1000.0 ; nim->dz *= 1000.0 ;
     } else if(  nim->xyz_units == NIFTI_UNITS_MICRON ){
       nim->dx *= 0.001  ; nim->dy *= 0.001  ; nim->dz *= 0.001  ;
     }

     LOAD_FVEC3( dxyz , (ORIENT_sign[orixyz.ijk[0]]=='+') ? nim->dx : -nim->dx ,
                        (ORIENT_sign[orixyz.ijk[1]]=='+') ? nim->dy : -nim->dy ,
                        (ORIENT_sign[orixyz.ijk[2]]=='+') ? nim->dz : -nim->dz  ) ;

   } else if (use_sform) {

     int orimap[7] = { 6 , 1 , 0 , 2 , 3 , 4 , 5 } ;
     int oritmp[3] ;
     float dxtmp, dytmp, dztmp ;
     float xmax, ymax, zmax ;
     float orgx, orgy, orgz ;
     float fig_merit, ang_merit ;

     form_code = nim->sform_code;

     /* convert sform to nifti orientation codes */

     /* n2   10 Jul, 2015 [rickr] */
     nifti_dmat44_to_orientation(nim->sto_xyz,
                                 &oritmp[0], &oritmp[1], &oritmp[2] ) ;

     /* convert nifti orientation codes to AFNI codes and store in vector */

     LOAD_IVEC3( orixyz , orimap[oritmp[0]] ,
                          orimap[oritmp[1]] ,
                          orimap[oritmp[2]] ) ;

     /* assume original view if there's no talairach id present */
     iview = NIFTI_code_to_view(nim->sform_code);

     /* load the offsets and the grid spacings */

     if (ORIENT_xyz[orixyz.ijk[0]] == 'z' )  {
       orgx = nim->sto_xyz.m[ORIENT_xyzint[orixyz.ijk[0]] - 1][3] ;
     } else {
       orgx = - nim->sto_xyz.m[ORIENT_xyzint[orixyz.ijk[0]] - 1][3] ;
     }

     if (ORIENT_xyz[orixyz.ijk[1]] == 'z' )  {
       orgy = nim->sto_xyz.m[ORIENT_xyzint[orixyz.ijk[1]] - 1][3] ;
     } else {
       orgy = - nim->sto_xyz.m[ORIENT_xyzint[orixyz.ijk[1]] - 1][3] ;
     }

     if (ORIENT_xyz[orixyz.ijk[2]] == 'z' )  {
       orgz = nim->sto_xyz.m[ORIENT_xyzint[orixyz.ijk[2]] - 1][3] ;
     } else {
       orgz = - nim->sto_xyz.m[ORIENT_xyzint[orixyz.ijk[2]] - 1][3] ;
     }


     LOAD_FVEC3( orgxyz ,  orgx ,
                           orgy ,
                           orgz  ) ;

#if 0
     LOAD_FVEC3( orgxyz , -nim->sto_xyz.m[0][3] ,    /* again, negate  */
                          -nim->sto_xyz.m[1][3] ,    /* x and y coords */
                           nim->sto_xyz.m[2][3] ) ;
#endif

#define MAXNUM(a,b) ( (a) > (b) ? (a):(b))
#define MAX3(a,b,c) ( (MAXNUM(a,b)) > (MAXNUM(a,c)) ? (MAXNUM(a,b)):(MAXNUM(a,c)))
#define MINNUM(a,b) ( (a) < (b) ? (a):(b))
#define MIN3(a,b,c) ( (MINNUM(a,b)) < (MINNUM(a,c)) ? (MINNUM(a,b)):(MINNUM(a,c)))

     dxtmp = sqrt ( nim->sto_xyz.m[0][0] * nim->sto_xyz.m[0][0] +
                    nim->sto_xyz.m[1][0] * nim->sto_xyz.m[1][0] +
                    nim->sto_xyz.m[2][0] * nim->sto_xyz.m[2][0] ) ;

     xmax = MAX3(fabs(nim->sto_xyz.m[0][0]),fabs(nim->sto_xyz.m[1][0]),fabs(nim->sto_xyz.m[2][0])) / dxtmp ;

     dytmp = sqrt ( nim->sto_xyz.m[0][1] * nim->sto_xyz.m[0][1] +
                    nim->sto_xyz.m[1][1] * nim->sto_xyz.m[1][1] +
                    nim->sto_xyz.m[2][1] * nim->sto_xyz.m[2][1] ) ;

     ymax = MAX3(fabs(nim->sto_xyz.m[0][1]),fabs(nim->sto_xyz.m[1][1]),fabs(nim->sto_xyz.m[2][1])) / dytmp ;

     dztmp = sqrt ( nim->sto_xyz.m[0][2] * nim->sto_xyz.m[0][2] +
                    nim->sto_xyz.m[1][2] * nim->sto_xyz.m[1][2] +
                    nim->sto_xyz.m[2][2] * nim->sto_xyz.m[2][2] ) ;

     zmax = MAX3(fabs(nim->sto_xyz.m[0][2]),fabs(nim->sto_xyz.m[1][2]),fabs(nim->sto_xyz.m[2][2])) / dztmp ;

     fig_merit = MIN3(xmax,ymax,zmax) ;
     ang_merit = acos (fig_merit) * 180.0 / 3.141592653 ;
#if 0
     if (fabs(ang_merit) > .01) {
       WARNING_message (
         "qform not present in:\n"
         "   '%s'\n"
         "  oblique sform used, and the worst axis is\n"
         "  %f degrees from plumb.\n"
         "  If you are performing spatial transformations on this dset, \n"
         "  or viewing/combining it with volumes of differing obliquity,\n"
         "  you should consider running: \n"
         "     3dWarp -deoblique \n"
         "  on this and  other oblique datasets in the same session.\n"
         ,pathname, ang_merit ) ;
     }
#endif

     if( nim->xyz_units == NIFTI_UNITS_METER ){
       dxtmp *= 1000.0 ; dytmp *= 1000.0 ; dztmp *= 1000.0 ;
     } else if(  nim->xyz_units == NIFTI_UNITS_MICRON ){
       dxtmp *= 0.001  ; dytmp *= 0.001  ; dztmp *= 0.001  ;
     }

     LOAD_FVEC3( dxyz , (ORIENT_sign[orixyz.ijk[0]]=='+') ? dxtmp : -dxtmp ,
                        (ORIENT_sign[orixyz.ijk[1]]=='+') ? dytmp : -dytmp ,
                        (ORIENT_sign[orixyz.ijk[2]]=='+') ? dztmp : -dztmp ) ;

     LOAD_MAT44(ijk_to_dicom44, -nim->sto_xyz.m[0][0] ,  /* negate x and y   */
                 -nim->sto_xyz.m[0][1] ,  /* coefficients,    */
                 -nim->sto_xyz.m[0][2] ,  /* since AFNI works */
                 -nim->sto_xyz.m[0][3] ,
                 -nim->sto_xyz.m[1][0] ,  /* with RAI coords, */
                 -nim->sto_xyz.m[1][1] ,  /* but NIFTI uses   */
                 -nim->sto_xyz.m[1][2] ,  /* LPI coordinates. */
                 -nim->sto_xyz.m[1][3] ,
                  nim->sto_xyz.m[2][0] ,  /* [Which is my own] */
                  nim->sto_xyz.m[2][1] ,  /* [damn fault!!!!!] */
                  nim->sto_xyz.m[2][2] ,  
                  nim->sto_xyz.m[2][3] ) ;

   } else { /* NO SPATIAL XFORM. BAD BAD BAD BAD BAD BAD. */

     float dxtmp, dytmp, dztmp ;

     /* if pixdim data are present, use them in order to set pixel
        dimensions.  otherwise, set the dimensions to 1 unit.         */

     dxtmp = ((nim->pixdim[1] > 0) ? nim->pixdim[1] : 1) ;
     dytmp = ((nim->pixdim[2] > 0) ? nim->pixdim[2] : 1) ;
     dztmp = ((nim->pixdim[3] > 0) ? nim->pixdim[3] : 1) ;

     if( nim->xyz_units == NIFTI_UNITS_METER ){
       dxtmp *= 1000.0 ; dytmp *= 1000.0 ; dztmp *= 1000.0 ;
     } else if(  nim->xyz_units == NIFTI_UNITS_MICRON ){
       dxtmp *= 0.001  ; dytmp *= 0.001  ; dztmp *= 0.001  ;
     }

     /* set orientation to LPI by default    */

     LOAD_IVEC3( orixyz , 1 ,
                          2 ,
                          4 ) ;

     LOAD_FVEC3( dxyz , (ORIENT_sign[orixyz.ijk[0]]=='+') ? dxtmp : -dxtmp ,
                        (ORIENT_sign[orixyz.ijk[1]]=='+') ? dytmp : -dytmp ,
                        (ORIENT_sign[orixyz.ijk[2]]=='+') ? dztmp : -dztmp ) ;

     /* no qform/sform should default to orig */
     /* use VIEW_ORIGINAL_TYPE, not NIFTI_default_view()  5 Feb 2016 [rickr] */
     iview = VIEW_ORIGINAL_TYPE;

     /* set origin to 0,0,0   */

     LOAD_FVEC3( orgxyz , 0 ,
                          0 ,
                          0 ) ;
     /* put scaled identity matrix by default */
     LOAD_MAT44(ijk_to_dicom44, dxtmp, 0.0, 0.0, 0.0,  
                                0.0, dytmp, 0.0, 0.0,
                                0.0, 0.0, dztmp, 0.0 );
   }



   /*-- make an AFNI dataset! --*/

   dset = EDIT_empty_copy(NULL) ;

   ppp  = THD_trailname(pathname,0) ;               /* strip directory */
   MCW_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;   /* to make prefix */
   
   /* You need to set the path too
      before, if you loaded ~/tmp/joe.nii the path appeared
      to be ./joe.nii, troubling in multiple instances.
                                                      ZSS Dec 2011 */
   THD_init_diskptr_names( dset->dblk->diskptr ,
                           THD_filepath(pathname) ,
                           NULL , prefix ,
                           dset->view_type , True );
                           
   nxyz.ijk[0] = nim->nx ;                          /* grid dimensions */
   nxyz.ijk[1] = nim->ny ;
   nxyz.ijk[2] = nim->nz ;

   dset->idcode.str[0] = 'N' ;  /* overwrite 1st 3 bytes with something special */
   dset->idcode.str[1] = 'I' ;
   dset->idcode.str[2] = 'I' ;

   MCW_hash_idcode( pathname , dset ) ;  /* 06 May 2005 */

   EDIT_dset_items( dset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_all   , datum ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_xyzorient   , orixyz ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_view_type   , iview ,
                      ADN_type        , (statcode != 0) ? HEAD_FUNC_TYPE
                                                        : HEAD_ANAT_TYPE ,
                    ADN_none ) ;

   /* copy transformation matrix to dataset structure */
   /* moved after setting grid     4 Apr 2014 [rickr,drg] */
   dset->daxes->ijk_to_dicom_real = ijk_to_dicom44;

   /* not a time dependent dataset */

   if( ntt < 2 ){
     EDIT_dset_items( dset ,
                        ADN_nvals     , nbuc ,
                        ADN_datum_all , datum ,
                        ADN_func_type , (statcode != 0) ? FUNC_BUCK_TYPE
                                                        : ANAT_BUCK_TYPE ,
                      ADN_none ) ;

   } else {  /* is a time dependent dataset */

     if( nim->time_units == NIFTI_UNITS_MSEC ){
            nim->dt *= 0.001 ;
            nim->toffset *= 0.001 ;
     } else if( nim->time_units == NIFTI_UNITS_USEC ){
            nim->dt *= 1.e-6 ;
            nim->toffset *= 1.e-6 ;
     }
     EDIT_dset_items( dset ,
                        ADN_nvals     , ntt ,
                        ADN_ntt       , ntt ,
                        ADN_datum_all , datum ,
                        ADN_ttorg     , nim->toffset , /* 12 Oct 2007 [rickr] */
                        ADN_ttdel     , nim->dt ,
                        ADN_ttdur     , 0.0 ,
                        ADN_tunits    , UNITS_SEC_TYPE ,
                        ADN_func_type , (statcode != 0) ? FUNC_FIM_TYPE
                                                        : ANAT_EPI_TYPE ,
                      ADN_none ) ;

     /* if present, add stuff about the slice-timing offsets */

     if( nim->slice_dim      == 3               && /* AFNI can only deal with */
         nim->slice_code     >  0               && /* slice timing offsets    */
         nim->slice_duration >  0.0             && /* along the k-axis of     */
         nim->slice_start    >= 0               && /* the dataset volume      */
         nim->slice_start    < nim->nz          &&
         nim->slice_end      > nim->slice_start &&
         nim->slice_end      < nim->nz             ){

       float *toff=(float *)calloc(sizeof(float),nim->nz) , tsl ;
       int kk ;

            if( nim->time_units == NIFTI_UNITS_MSEC ) nim->slice_duration *= 0.001;
       else if( nim->time_units == NIFTI_UNITS_USEC ) nim->slice_duration *= 1.e-6;

       /* set up slice time offsets in the divers orders */

       switch( nim->slice_code ){
         case NIFTI_SLICE_SEQ_INC:
           tsl = 0.0 ;
           for( kk=nim->slice_start ; kk <= nim->slice_end ; kk++ ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
         break ;
         case NIFTI_SLICE_SEQ_DEC:
           tsl = 0.0 ;
           for( kk=nim->slice_end ; kk >= nim->slice_end ; kk-- ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
         break ;
         case NIFTI_SLICE_ALT_INC:
           tsl = 0.0 ;
           for( kk=nim->slice_start ; kk <= nim->slice_end ; kk+=2 ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
           for( kk=nim->slice_start+1 ; kk <= nim->slice_end ; kk+=2 ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
         break ;
         case NIFTI_SLICE_ALT_INC2:
           tsl = 0.0 ;
           for( kk=nim->slice_start+1 ; kk <= nim->slice_end ; kk+=2 ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
           for( kk=nim->slice_start ; kk <= nim->slice_end ; kk+=2 ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
         break ;
         case NIFTI_SLICE_ALT_DEC:
           tsl = 0.0 ;
           for( kk=nim->slice_end ; kk >= nim->slice_start ; kk-=2 ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
           for( kk=nim->slice_end-1 ; kk >= nim->slice_start ; kk-=2 ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
         break ;
         case NIFTI_SLICE_ALT_DEC2:
           tsl = 0.0 ;
           for( kk=nim->slice_end-1 ; kk >= nim->slice_start ; kk-=2 ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
           for( kk=nim->slice_end ; kk >= nim->slice_start ; kk-=2 ){
             toff[kk] = tsl ; tsl += nim->slice_duration ;
           }
         break ;
       }

       EDIT_dset_items( dset ,
                          ADN_nsl     , nim->nz       ,
                          ADN_zorg_sl , orgxyz.xyz[2] ,
                          ADN_dz_sl   , dxyz.xyz[2]   ,
                          ADN_toff_sl , toff          ,
                        ADN_none ) ;

       free(toff) ;

     } /* end of slice timing stuff */

   } /* end of 3D+time dataset stuff */


   /* set atlas space based on NIFTI s/qform code */
   NIFTI_code_to_space(form_code,dset);

   /* add statistics, if present */

   if( statcode != 0 ){
     for( ibr=0 ; ibr < nvals ; ibr++ )
       EDIT_STATAUX4(dset,ibr,statcode,nim->intent_p1,nim->intent_p2,nim->intent_p3,0) ;
   }

   /*-- flag to read data from disk using NIFTI functions --*/

   dset->dblk->diskptr->storage_mode = STORAGE_BY_NIFTI ;
   strcpy( dset->dblk->diskptr->brick_name , pathname ) ;
   dset->dblk->diskptr->byte_order = nim->byteorder ;

#if 0
   for( ibr=0 ; ibr < nvals ; ibr++ ){     /* make sub-brick labels */
     sprintf(prefix,"%s[%d]",tname,ibr) ;
     EDIT_BRICK_LABEL( dset , ibr , prefix ) ;
   }
#endif

   /** 10 May 2005: see if there is an AFNI extension;
                    if so, load attributes from it and
                    then edit the dataset appropriately **/

   { int ee ;  /* extension index */

     /* scan extension list to find the first AFNI extension */

     for( ee=0 ; ee < nim->num_ext ; ee++ )
       if( nim->ext_list[ee].ecode == NIFTI_ECODE_AFNI &&
           nim->ext_list[ee].esize > 32                &&
           nim->ext_list[ee].edata != NULL               ) break ;

     /* if found an AFNI extension ... */

     if( ee < nim->num_ext ){
       char *buf = nim->ext_list[ee].edata , *rhs , *cpt ;
       int  nbuf = nim->ext_list[ee].esize - 8 ;
       NI_stream ns ;
       void     *nini ;
       NI_group *ngr , *nngr ;

       /* if have data, it's long enough, and starts properly, then ... */

       if( buf != NULL && nbuf > 32 && strncmp(buf,"<?xml",5)==0 ){
         if( buf[nbuf-1] != '\0' ) buf[nbuf-1] = '\0' ;         /* for safety */
         cpt = strstr(buf,"?>") ;                    /* find XML prolog close */
         if( cpt != NULL ){                          /* if found it, then ... */
           ns = NI_stream_open( "str:" , "r" ) ;
           NI_stream_setbuf( ns , cpt+2 ) ;        /* start just after prolog */
           nini = NI_read_element(ns,1) ;                 /* get root element */
           NI_stream_close(ns) ;
           if( NI_element_type(nini) == NI_GROUP_TYPE ){   /* must be a group */
             ngr = (NI_group *)nini ;
             if( strcmp(ngr->name,"AFNI_attributes") == 0 ){    /* root is OK */
               nngr = ngr ;
             } else {                   /* search in group for proper element */
               int nn ; void **nnini ;
               nn = NI_search_group_deep( ngr , "AFNI_attributes" , &nnini ) ;
               if( nn <= 0 ) nngr = NULL ;
               else        { nngr = (NI_group *)nnini[0]; NI_free(nnini); }
             }

             if( NI_element_type(nngr) == NI_GROUP_TYPE ){ /* have  good name */
               rhs = NI_get_attribute( nngr , "self_idcode" ) ;
               if( rhs == NULL )
                 rhs = NI_get_attribute( nngr , "AFNI_idcode" ) ;
               if( rhs != NULL )    /* set dataset ID code from XML attribute */
                 MCW_strncpy( dset->idcode.str , rhs , MCW_IDSIZE ) ;
               rhs = NI_get_attribute( nngr , "NIfTI_nums" ) ;    /* check if */
               if( rhs != NULL ){                       /* dataset dimensions */
                 char buf[128] ;                              /* were altered */
                 sprintf(buf,"%ld,%ld,%ld,%ld,%ld,%d" ,        /* 12 May 2005 */
                   nim->nx, nim->ny, nim->nz, nim->nt, nim->nu, nim->datatype );
                 if( strcmp(buf,rhs) != 0 ){
                   static int nnn=0 ;
                   if(nnn==0){fprintf(stderr,"\n"); nnn=1;}
                   fprintf(stderr,
                     "** WARNING: NIfTI file %s dimensions altered since "
                                 "AFNI extension was added\n",pathname ) ;
                 }
               }
               THD_dblkatr_from_niml( nngr , dset->dblk ); /* load attributes */
               THD_datablock_apply_atr( dset ) ;   /* apply to dataset struct */
             }
             NI_free_element( ngr ) ;          /* get rid of the root element */

           } /* end of if found a group element at the root */
         } /* end of if extension data array had an XML prolog close */
       } /* end of if had a good extension data array */
     } /* end of if had an AFNI extension */
   } /* end of processing extensions */

   /* return unpopulated dataset */

   nifti_image_free(nim) ; RETURN(dset) ;
}


/* n2   10 Jul, 2015 [rickr] */
int64_t * copy_ints_as_i64(int * ivals, int nvals)
{
   int64_t * i64;
   int       c;

   i64 = (int64_t *)malloc(nvals * sizeof(int64_t));
   if( ! i64 ) {
      fprintf(stderr,"** CIA64: failed to alloc %d int64_t's\n", nvals);
      return NULL;
   }
   for( c=0; c<nvals; c++ ) i64[c] = ivals[c];

   return i64;
}


/*-----------------------------------------------------------------
  Load a NIFTI dataset's data into memory
  (called from THD_load_datablock in thd_loaddblk.c)
    - RWC: modified 07 Apr 2005 to read data bricks via nifti_io.c
      'NBL' functions, rather than directly from disk
-------------------------------------------------------------------*/

void THD_load_nifti( THD_datablock *dblk )
{
   THD_diskptr *dkptr ;
   int nx,ny,nz,nxy,nxyz,nxyzv , nerr=0,ibr,nv, nslice ;
   int datum, need_copy=0 ;
   int scale_data=0 ;
   void *ptr ;
   nifti_image *nim ;
   nifti_brick_list NBL ;  /* holds the data read from disk */

ENTRY("THD_load_nifti") ;

   /*-- open and read input [these errors should never occur] --*/

   if( !ISVALID_DATABLOCK(dblk)                        ||
       dblk->diskptr->storage_mode != STORAGE_BY_NIFTI ||
       dblk->brick == NULL                               ) EXRETURN ;

   dkptr = dblk->diskptr ;

   /* purge any existing bricks [10 Mar 2014] */

   STATUS("purging existing data bricks (if any)") ;
   THD_purge_datablock(dblk,DATABLOCK_MEM_ANY) ;

   STATUS("calling nifti_image_read_bricks") ;
   NBL.nbricks = 0 ;

   if( ! DBLK_IS_MASTERED(dblk) )   /* allow mastering   14 Apr 2006 [rickr] */
       nim = nifti_image_read_bricks( dkptr->brick_name, 0,NULL , &NBL ) ;
   else {
       /* n2   10 Jul, 2015 [rickr] */
       /* convert master_ival to an array of int64_t */
       int64_t * i64_vals = copy_ints_as_i64(dblk->master_ival, dblk->nvals);
       nim = nifti_image_read_bricks( dkptr->brick_name, dblk->nvals,
                                      i64_vals, &NBL ) ;
   }

   if( nim == NULL || NBL.nbricks <= 0 ) EXRETURN ;

   datum = DBLK_BRICK_TYPE(dblk,0) ;  /* destination data type */

   /*-- determine if we need to copy the data from the
        bricks as loaded above because of a type conversion --*/

   switch( nim->datatype ){
     case DT_INT16:
     case DT_UINT8:      need_copy = (datum == MRI_float) ; break ;

     case DT_FLOAT32:
     case DT_COMPLEX64:
     case DT_RGB24:      need_copy = 0 ; break ;

     case DT_INT8:       /* these are the cases where AFNI can't */
     case DT_UINT16:     /* directly handle the NIFTI datatype,  */
     case DT_INT32:      /* so we'll convert them to floats.     */
     case DT_UINT32:
     case DT_FLOAT64:    need_copy = 1 ; break ;
#if 0
     case DT_COMPLEX128: need_copy = 1 ; break ;
#endif
   }

   /*-- various dimensions --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ;  nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ;  nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ;  if( nv > NBL.nbricks ) nv = NBL.nbricks ;
   nxyzv = nxyz * nv ; nslice = nz*nv ;

   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /*------ don't need to copy data ==> just copy pointers from NBL ------*/

   if( !need_copy ){

     STATUS("copying brick pointers directly") ;
     for( ibr=0 ; ibr < nv ; ibr++ ){
       mri_fix_data_pointer( NBL.bricks[ibr] ,DBLK_BRICK(dblk,ibr) ) ;
       NBL.bricks[ibr] = NULL ;  /* so it won't be deleted later */

       if( DBLK_BRICK_TYPE(dblk,ibr) == MRI_float ){
         STATUS("doing floatscan") ;
         nerr += thd_floatscan( DBLK_BRICK_NVOX(dblk,ibr) ,
                                DBLK_ARRAY(dblk,ibr)        ) ;
       } else if( DBLK_BRICK_TYPE(dblk,ibr) == MRI_complex ){
         STATUS("doing complexscan") ;
         nerr += thd_complexscan( DBLK_BRICK_NVOX(dblk,ibr) ,
                                  DBLK_ARRAY(dblk,ibr)        ) ;
       }
     }
     if( nerr > 0 ) WARNING_message("file %s: corrected %d float errors\n",
                                    dkptr->brick_name , nerr ) ;

   } else { /*---------- need to copy data ==> do some more work -----------*/

     register int ii ; void *nbuf ;

     STATUS("converting input bricks to floats") ;
     for( ibr=0 ; ibr < nv ; ibr++ ){

       if( DBLK_ARRAY(dblk,ibr) == NULL ){                     /* make space */
         ptr = AFMALL(void, DBLK_BRICK_BYTES(dblk,ibr) ) ;     /* for this   */
         if( ptr == NULL ) ERROR_message("malloc fails for NIfTI sub-brick #%d",ibr) ;
         mri_fix_data_pointer( ptr ,  DBLK_BRICK(dblk,ibr) ) ; /* sub-brick! */
       }
       ptr = DBLK_ARRAY(dblk,ibr) ; if( ptr == NULL ) break ;  /* bad news!! */

       nbuf = NBL.bricks[ibr] ;              /* data as read from NIfTI file */

       /* macro to convert data from type "ityp" in nbuf to float in dataset */

#undef  CPF
#define CPF(ityp) do{ ityp *sar = (ityp *)nbuf ; float *far = (float *)ptr ;   \
                      for( ii=0 ; ii < nxyz ; ii++ ) far[ii] = (float)sar[ii]; \
                  } while(0)

       /* load from nbuf into brick array (will be float or complex) */

       STATUS(" converting sub-brick") ;

       switch( nim->datatype ){
         case DT_UINT8:    CPF(unsigned char)  ; break ;
         case DT_INT8:     CPF(signed char)    ; break ;
         case DT_INT16:    CPF(signed short)   ; break ;
         case DT_UINT16:   CPF(unsigned short) ; break ;
         case DT_INT32:    CPF(signed int)     ; break ;
         case DT_UINT32:   CPF(unsigned int)   ; break ;
         case DT_FLOAT64:  /* added floatscan  2 Dec, 2014 [rickr] */
            { CPF(double) ; thd_floatscan(nxyz, (float *)ptr) ; break ; }
#if 0
         case DT_COMPLEX128: break ;
#endif
       }

       STATUS(" free-ing NIfTI volume") ;

       free(NBL.bricks[ibr]) ; NBL.bricks[ibr] = NULL ;
     }
   }

   STATUS("free-ing NBL") ;

   nifti_free_NBL( &NBL ) ;  /* done with this */

   /*-- scale results? ---*/

   /* errors for !isfinite() have been printed */
   scale_data = isfinite(nim->scl_slope) && isfinite(nim->scl_inter) 
                     && (nim->scl_slope != 0.0)
                     && (nim->scl_slope != 1.0 || nim->scl_inter != 0.0) ;

   if( scale_data ){
     STATUS("scaling sub-bricks") ;
     for( ibr=0 ; ibr < nv ; ibr++ ){
       if( DBLK_BRICK_TYPE(dblk,ibr) == MRI_float ){
         float *far = (float *) DBLK_ARRAY(dblk,ibr) ; int ii ;
         for( ii=0 ; ii < nxyz ; ii++ ){
           far[ii] = nim->scl_slope * far[ii] + nim->scl_inter ;
           if( !isfinite(far[ii]) ) far[ii] = 0.0f ;
         }
       } else if( DBLK_BRICK_TYPE(dblk,ibr) == MRI_complex ){
         complex *car = (complex *) DBLK_ARRAY(dblk,ibr) ; int ii ;
         for( ii=0 ; ii < nxyz ; ii++ ){
           car[ii].r = nim->scl_slope * car[ii].r + nim->scl_inter ;
           car[ii].i = nim->scl_slope * car[ii].i + nim->scl_inter ;
         }
       }
     }
   }

   /* rcr - replace this with DBLK_IS_RANGE_MASTERED to also check for
    *       csv list
    *     - then call a new parent function to THD_apply_master_subrange
    */
   if( DBLK_IS_MASTER_SUBRANGED(dblk) )
     THD_apply_master_subrange(dblk) ;

   /*-- throw away the trash and return --*/

   nifti_image_free(nim) ; EXRETURN ;
}


/* set atlas space based on NIFTI sform code */
static void NIFTI_code_to_space(int code,THD_3dim_dataset *dset)
{
    switch(code) {
        case NIFTI_XFORM_TALAIRACH:
            MCW_strncpy(dset->atlas_space, "TLRC", THD_MAX_NAME);
            break;
        case NIFTI_XFORM_MNI_152:
            MCW_strncpy(dset->atlas_space, "MNI", THD_MAX_NAME);
            break;
        default:
            THD_get_space(dset);
    }
}

/* return dataset view code based on NIFTI sform/qform code */
/* code for +tlrc, +orig */
static int NIFTI_code_to_view(int code)
{
   int iview;

   ENTRY("NIFTI_code_to_view");
   /* only two standard templates now defined */
   switch(code) {
       case NIFTI_XFORM_TALAIRACH:       /* TLRC space -> tlrc view */
           iview = VIEW_TALAIRACH_TYPE;
           break;
       case NIFTI_XFORM_MNI_152:         /* MNI space -> tlrc 'view' */
           iview = VIEW_TALAIRACH_TYPE;
           break;
       case NIFTI_XFORM_SCANNER_ANAT:    /* no code set or scanner -> orig 'view' */
       case NIFTI_XFORM_UNKNOWN:
           iview = VIEW_ORIGINAL_TYPE;
           break;
       case NIFTI_XFORM_ALIGNED_ANAT:    /* aligned to something... */
       default:                          /* or something else we don't know about yet (higher form codes)*/
           iview = NIFTI_default_view();
   }
   RETURN(iview);
}

/* get default view from environment variable */
static int NIFTI_default_view()
{
  char *ppp;
  int iview = VIEW_TALAIRACH_TYPE; /* default view if not otherwise set */

  ENTRY("NIFTI_default_view");
  ppp = my_getenv("AFNI_NIFTI_VIEW");
  if(ppp){
     if(strcasecmp(ppp, "TLRC")==0)
        iview = VIEW_TALAIRACH_TYPE;
     else if (strcasecmp(ppp,"ORIG")==0) {
           iview = VIEW_ORIGINAL_TYPE;
     }
     else if (strcasecmp(ppp,"ACPC")==0) {
           iview = VIEW_ACPCALIGNED_TYPE;
     }
  }
  RETURN(iview);
}
