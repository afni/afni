#include "mrilib.h"

#undef MANUAL_ORIENT

/*-------------------------------------------------------------------------*/
/*! Create an empty dataset with geometry given by a string. Examples:
     - "tlrc"
     - "RAI:nx,xorg,dx,ny,yorg,dy,nz,zorg,dz"
     - "MATRIX(a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34):nx,ny,nz"
*//*-----------------------------------------------------------------------*/

THD_3dim_dataset * EDIT_geometry_constructor( char *gstr , char *prefix )
{
   THD_3dim_dataset *dset = NULL ;
   THD_ivec3 orixyz , nxyz ;
   THD_fvec3 dxyz , orgxyz ;
   mat44 ijk_to_dicom44 ; THD_mat33 R ;
   int view=VIEW_ORIGINAL_TYPE ;
   float dx,dy,dz , xorg,yorg,zorg ;
   int   nx,ny,nz , ii ;
   char *lstr , *cpt ;
   float a11,a12,a13,a14 ;
   float a21,a22,a23,a24 ;
   float a31,a32,a33,a34 ;
   float orgx,orgy,orgz ;

ENTRY("EDIT_empty_constructor") ;

   if( gstr == NULL || strlen(gstr) < 4 ) RETURN(NULL) ;
   lstr = strdup(gstr) ;

   /*--- convert to MATRIX() type of string ---*/

   if( strncasecmp(lstr,"TLRC",4) == 0 ){

     free(lstr) ;
     lstr = strdup("MATRIX(1,0,0,-80 , 0,1,0,-80 , 0,0,1,-65):161,191,151") ;
     view = VIEW_TALAIRACH_TYPE ;

   } else if( lstr[3] == ':' ){

     THD_coorder cord ;

     THD_coorder_fill( lstr , &cord ) ;

     /* convert commas to blanks */

     for( cpt=lstr ; *cpt != '\0' ; cpt++ ) if( *cpt == ',' ) *cpt = ' ' ;
     nx = ny = nz = -1; dx = dy = dz = -1.0f; xorg = yorg = zorg = 0.0f ;
     ii = sscanf(lstr+4,"%d%f%f%d%f%f%d%f%f",
                 &nx,&xorg,&dx , &ny,&yorg,&dy , &nz,&zorg,&dz ) ;
     if( ii <  9 ||
			nx <= 0 || ny <= 0 || nz <= 0 ||
         dx <= 0 || dy <= 0 || dz <= 0   ){ free(lstr); RETURN(NULL); }

     a11 = dx ; a21 = 0.0f ; a31 = 0.0f ;
     THD_coorder_to_dicom( &cord , &a11,&a21,&a31 ) ;

     a12 = 0.0f ; a22 = dy ; a32 = 0.0f ;
     THD_coorder_to_dicom( &cord , &a12,&a22,&a32 ) ;

     a13 = 0.0f ; a23 = 0.0f ; a33 = dz ;
     THD_coorder_to_dicom( &cord , &a13,&a23,&a33 ) ;

     a14 = xorg ; a24 = yorg ; a34 = zorg ;
     THD_coorder_to_dicom( &cord , &a14,&a24,&a34 ) ;

     cpt = (char *)malloc(sizeof(char)*666) ;
     sprintf(cpt,
      "MATRIX(%.4f,%.4f,%.4f,%.4f , %.4f,%.4f,%.4f,%.4f , %.4f,%.4f,%.4f,%.4f):%d,%d,%d" ,
      a11,a12,a13,a14 , a21,a22,a23,a24, a31,a32,a33,a34 , nx,ny,nz ) ;

     free(lstr) ; lstr = cpt ;

   }

   if( strncasecmp(lstr,"matrix(",7) != 0 ){ free(lstr); RETURN(NULL); }

   /*--- decode MATRIX geometry string ---*/

   for( cpt=lstr ; *cpt != '\0' ; cpt++ ) if( *cpt == ',' ) *cpt = ' ' ;
   ii = sscanf(lstr+7,"%f%f%f%f%f%f%f%f%f%f%f%f):%d%d%d",
               &a11,&a12,&a13,&a14 ,
               &a21,&a22,&a23,&a24 , &a31,&a32,&a33,&a34 , &nx,&ny,&nz ) ;
   free(lstr) ;
   if( ii < 15 ) RETURN(NULL) ;

   /*--- create dataset and put stuff into it ---*/

   dset = EDIT_empty_copy(NULL) ;

   /*-- orientation stuff --*/

   LOAD_MAT44( ijk_to_dicom44 ,
               a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ) ;

   dset->daxes->ijk_to_dicom_real = ijk_to_dicom44 ;
   dset->daxes->ijk_to_dicom      = ijk_to_dicom44 ;

   nxyz.ijk[0] = nx ; nxyz.ijk[1] = ny ; nxyz.ijk[2] = nz ;
   EDIT_dset_items( dset , ADN_nxyz,nxyz , ADN_none ) ;

#ifdef MANUAL_ORIENT   /* This one does not work. 
                           Changed #ifndef to #ifdef
                           One below seems to work. 
                                    ZSS, RKR, Jan 22 10 */
   /* fprintf(stderr,"ZSS: Calling THD_daxes_from_mat44\n"); */
   THD_daxes_from_mat44( dset->daxes ) ;
#else
   /* fprintf(stderr,"ZSS: Orientation the hard way\n");  */
   LOAD_MAT( R , a11,a12,a13 , a21,a22,a23 , a31,a32,a33 ) ;

   orixyz = THD_matrix_to_orientation( R ) ;   /* compute orientation codes */

   orgx = ijk_to_dicom44.m[ORIENT_xyzint[orixyz.ijk[0]]-1][3] ;
   orgy = ijk_to_dicom44.m[ORIENT_xyzint[orixyz.ijk[1]]-1][3] ;
   orgz = ijk_to_dicom44.m[ORIENT_xyzint[orixyz.ijk[2]]-1][3] ;
   LOAD_FVEC3( orgxyz , orgx,orgy,orgz ) ;

   dx = sqrtf( a11*a11 + a21*a21 + a31*a31 ) ;
   dy = sqrtf( a12*a12 + a22*a22 + a32*a32 ) ;
   dz = sqrtf( a13*a13 + a23*a23 + a33*a33 ) ;
   if( ORIENT_sign[orixyz.ijk[0]] == '-' ) dx = -dx ;
   if( ORIENT_sign[orixyz.ijk[1]] == '-' ) dy = -dy ;
   if( ORIENT_sign[orixyz.ijk[2]] == '-' ) dz = -dz ;
   LOAD_FVEC3( dxyz , dx,dy,dz ) ;
   EDIT_dset_items( dset ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_xyzorient   , orixyz ,
                    ADN_none ) ;
#endif

   dset->idcode.str[0] = 'G' ;
   dset->idcode.str[1] = 'E' ;
   dset->idcode.str[2] = 'O' ;

   if( !THD_filename_ok(prefix) ) prefix = "gggeom" ;

   EDIT_dset_items( dset ,
                      ADN_prefix      , prefix ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_view_type   , view ,
                      ADN_type        , HEAD_FUNC_TYPE ,
                      ADN_func_type   , FUNC_BUCK_TYPE ,
                    ADN_none ) ;

   /* 
   fprintf(stderr,"ZSS: Orient %d %d %d\n", 
        dset->daxes->xxorient, dset->daxes->yyorient, dset->daxes->zzorient);
   */
   
   RETURN(dset) ;
}

/*-------------------------------------------------------------------------*/

char * EDIT_get_geometry_string( THD_3dim_dataset *dset )
{
   static char gstr[666] ;
   float a11,a12,a13,a14 ;
   float a21,a22,a23,a24 ;
   float a31,a32,a33,a34 ;
   int nx,ny,nz ;
   char b11[32],b12[32],b13[32],b14[32] ;
   char b21[32],b22[32],b23[32],b24[32] ;
   char b31[32],b32[32],b33[32],b34[32] ;

   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom_real) ){
     THD_daxes_to_mat44(dset->daxes) ;
     dset->daxes->ijk_to_dicom_real = dset->daxes->ijk_to_dicom ;
   }

   UNLOAD_MAT44( dset->daxes->ijk_to_dicom_real ,
                 a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ) ;

   MV_fval_to_char(a11,b11); MV_fval_to_char(a12,b12);
   MV_fval_to_char(a13,b13); MV_fval_to_char(a14,b14); nx = DSET_NX(dset);
   MV_fval_to_char(a21,b21); MV_fval_to_char(a22,b22);
   MV_fval_to_char(a23,b23); MV_fval_to_char(a24,b24); ny = DSET_NY(dset);
   MV_fval_to_char(a31,b31); MV_fval_to_char(a32,b32);
   MV_fval_to_char(a33,b33); MV_fval_to_char(a34,b34); nz = DSET_NZ(dset);

   sprintf( gstr ,
      "MATRIX(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s):%d,%d,%d" ,
      b11,b12,b13,b14,b21,b22,b23,b24,b31,b32,b33,b34 , nx,ny,nz ) ;

   return gstr ;
}
