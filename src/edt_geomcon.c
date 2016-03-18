#include "mrilib.h"

#undef MANUAL_ORIENT

/*-------------------------------------------------------------------------*/
/*! Create an empty dataset with geometry given by a string. Examples:
     - "tlrc"
     - "RAI:nx,xorg,dx,ny,yorg,dy,nz,zorg,dz"
     - "RAI:D:nx,xorg,dx,ny,yorg,dy,nz,zorg,dz" if xorg, yorg, zorg are DICOM
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
   int   nx,ny,nz , ii, dicomorigin = 0;
   char *lstr , *cpt ;
   float a11,a12,a13,a14 ;
   float a21,a22,a23,a24 ;
   float a31,a32,a33,a34 ;
   float orgx,orgy,orgz ;

ENTRY("EDIT_geometry_constructor") ;

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
     if (!strncmp(lstr+4,"D:",2)) {
       dicomorigin = 1;
       ii = sscanf(lstr+6,"%d%f%f%d%f%f%d%f%f",
                   &nx,&xorg,&dx , &ny,&yorg,&dy , &nz,&zorg,&dz ) ;
       if( ii <  9 ||
          nx <= 0 || ny <= 0 || nz <= 0 ||
          dx <= 0 || dy <= 0 || dz <= 0   ){
            ERROR_message("Negative or 0 voxel counts or voxel sizes");
            free(lstr); RETURN(NULL);
         }
     } else {
       dicomorigin = 0;
       ii = sscanf(lstr+4,"%d%f%f%d%f%f%d%f%f",
                   &nx,&xorg,&dx , &ny,&yorg,&dy , &nz,&zorg,&dz ) ;
       if( ii <  9 ||
          nx <= 0 || ny <= 0 || nz <= 0 ||
          dx <= 0 || dy <= 0 || dz <= 0   ){ free(lstr); RETURN(NULL); }
     }

     a11 = dx ; a21 = 0.0f ; a31 = 0.0f ;
     THD_coorder_to_dicom( &cord , &a11,&a21,&a31 ) ;

     a12 = 0.0f ; a22 = dy ; a32 = 0.0f ;
     THD_coorder_to_dicom( &cord , &a12,&a22,&a32 ) ;

     a13 = 0.0f ; a23 = 0.0f ; a33 = dz ;
     THD_coorder_to_dicom( &cord , &a13,&a23,&a33 ) ;

     a14 = xorg ; a24 = yorg ; a34 = zorg ;
     if (!dicomorigin) THD_coorder_to_dicom( &cord , &a14,&a24,&a34 ) ;

     cpt = (char *)malloc(sizeof(char)*666) ;
     sprintf(cpt,
      "MATRIX(%.4f,%.4f,%.4f,%.4f , %.4f,%.4f,%.4f,%.4f , %.4f,%.4f,%.4f,%.4f):%d,%d,%d" ,
      a11,a12,a13,a14 , a21,a22,a23,a24, a31,a32,a33,a34 , nx,ny,nz ) ;

     free(lstr) ; lstr = cpt ;

   }

   if( !ISVALID_GEOMETRY_STRING(lstr) ){ free(lstr); RETURN(NULL); }

   /*--- decode MATRIX geometry string ---*/

#if 0
INFO_message("EDIT_geometry_constructor: string = %s",lstr) ;
#endif

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

#if 0
DUMP_MAT44("EDIT_geometry_constructor ijk_to_dicom",ijk_to_dicom44) ;
#endif

   dset->daxes->ijk_to_dicom_real = ijk_to_dicom44 ;
   dset->daxes->ijk_to_dicom      = ijk_to_dicom44 ;

   nxyz.ijk[0] = nx ; nxyz.ijk[1] = ny ; nxyz.ijk[2] = nz ;
   EDIT_dset_items( dset , ADN_nxyz,nxyz , ADN_none ) ;

#ifdef MANUAL_ORIENT   /* This one does not work.
                           Changed #ifndef to #ifdef
                           One below seems to work.
                                    ZSS, RKR, Jan 22 10 */
   THD_daxes_from_mat44( dset->daxes ) ;
#else
   LOAD_MAT( R , a11,a12,a13 , a21,a22,a23 , a31,a32,a33 ) ;

   orixyz = THD_matrix_to_orientation( R ) ;   /* compute orientation codes */

#if 0
INFO_message("EDIT_geometry_constructor: orientation codes = %d %d %d",orixyz.ijk[0],orixyz.ijk[1],orixyz.ijk[2]) ;
#endif

   orgx = ijk_to_dicom44.m[ORIENT_xyzint[orixyz.ijk[0]]-1][3] ; /* somewhat  */
   orgy = ijk_to_dicom44.m[ORIENT_xyzint[orixyz.ijk[1]]-1][3] ; /* confusing */
   orgz = ijk_to_dicom44.m[ORIENT_xyzint[orixyz.ijk[2]]-1][3] ; /* I admit!  */
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

   dset->idcode.str[0] = 'G' ; /* this is just for fun */
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

   RETURN(dset) ;
}

/*-------------------------------------------------------------------------*/
/* create a random dataset [16 Mar 2016] */

THD_3dim_dataset * jRandomDataset( int nx, int ny, int nz, int nt )
{
   THD_3dim_dataset *dset ;
   char gstr[128] ;
   int iv,jj,nvox; float *far , zz=0.0f ;

   if( nx < 2 || ny < 2 || nz < 1 ) return NULL ;
   if( nt < 1 ) nt = 1 ;

   sprintf(gstr,"RAI:%d,0,1.0,%d,0,1.0,%d,0,1.0",nx,ny,nz) ;
   dset = EDIT_geometry_constructor(gstr,"jRandomDataset") ;

   EDIT_dset_items( dset ,
                      ADN_nvals  , nt ,
                    ADN_none ) ;
   if( nt > 1 ){
     EDIT_dset_items( dset ,
                        ADN_ntt    , nt ,
                        ADN_ttdel  , 1.0f ,
                       ADN_none ) ;
   }

   nvox = nx*ny*nz ;
   for( iv=0 ; iv < nt ; iv++ ){
     EDIT_substitute_brick( dset , iv , MRI_float , NULL ) ;
     far = DSET_ARRAY( dset , iv ) ;
     for( jj=0 ; jj < nvox ; jj++ ) far[jj] = 2.0f*(float)(drand48())-1.0f ;
     if( nvox%32 == 0 ){
       for( jj=0 ; jj < 17 ; jj++ ) zz += drand48() ;
     }
   }

   return dset ;
}

/*-------------------------------------------------------------------------*/
/* create a random 1D file [17 Mar 2016] */

MRI_IMAGE * jRandom1D( int nx , int ny )
{
   MRI_IMAGE *im ; int ii,jj,kk,nxy ; float *far , zz=0.0f ;

   if( nx < 1 ) return NULL ;
   if( ny < 1 ) ny = 1 ;

   im  = mri_new( nx , ny , MRI_float ) ;
   far = MRI_FLOAT_PTR(im) ;
   nxy = nx*ny ;
   for( kk=jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,kk++ )
       far[kk] = 2.0f*(float)(drand48())-1.0f ;
     if( nx%8 == 0 ){
       int qq ; for( qq=0 ; qq < 11 ; qq++ ) zz += drand48() ;
     }
   }

   return im ;
}

/*-------------------------------------------------------------------------*/

char * EDIT_get_geometry_string( THD_3dim_dataset *dset )
{
   char *ggg ;

   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom_real) ){
     THD_daxes_to_mat44(dset->daxes) ;
     dset->daxes->ijk_to_dicom_real = dset->daxes->ijk_to_dicom ;
   }

#if 0
DUMP_MAT44("EDIT_get_geometry_string: using ijk_to_dicom_real",dset->daxes->ijk_to_dicom_real) ;
#endif

   ggg = EDIT_imat_to_geometry_string( dset->daxes->ijk_to_dicom_real ,
                                       DSET_NX(dset),DSET_NY(dset),DSET_NZ(dset) ) ;
   return ggg ;
}

/*-------------------------------------------------------------------------*/

char * EDIT_imat_to_geometry_string( mat44 imat , int nx,int ny,int nz )
{
   static char gstr[666] ;
   float a11,a12,a13,a14 ;
   float a21,a22,a23,a24 ;
   float a31,a32,a33,a34 ;
   char b11[32],b12[32],b13[32],b14[32] ;
   char b21[32],b22[32],b23[32],b24[32] ;
   char b31[32],b32[32],b33[32],b34[32] ;

   UNLOAD_MAT44( imat ,
                 a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ) ;

   MV_fval_to_char(a11,b11) ; MV_fval_to_char(a12,b12) ;
   MV_fval_to_char(a13,b13) ; MV_fval_to_char(a14,b14) ;
   MV_fval_to_char(a21,b21) ; MV_fval_to_char(a22,b22) ;
   MV_fval_to_char(a23,b23) ; MV_fval_to_char(a24,b24) ;
   MV_fval_to_char(a31,b31) ; MV_fval_to_char(a32,b32) ;
   MV_fval_to_char(a33,b33) ; MV_fval_to_char(a34,b34) ;

   sprintf( gstr ,
      "MATRIX(%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s):%d,%d,%d" ,
      b11,b12,b13,b14,b21,b22,b23,b24,b31,b32,b33,b34 , nx,ny,nz ) ;

   return strdup(gstr) ;
}

/*-------------------------------------------------------------------------*/

mat44_nxyz EDIT_geometry_string_to_mat44( char *gstr )
{
   mat44_nxyz omat ;
   char *cpt, *lstr ;
   float a11,a12,a13,a14 ;
   float a21,a22,a23,a24 ;
   float a31,a32,a33,a34 ;
   int nx,ny,nz , ii ;

   LOAD_IDENT_MAT44(omat.mat) ; omat.nx = omat.ny = omat.nz = 0 ;
   if( !ISVALID_GEOMETRY_STRING(gstr) ) return omat ;

   lstr = strdup(gstr) ;

   for( cpt=lstr ; *cpt != '\0' ; cpt++ ) if( *cpt == ',' ) *cpt = ' ' ;
   ii = sscanf(lstr+7,"%f%f%f%f%f%f%f%f%f%f%f%f):%d%d%d",
               &a11,&a12,&a13,&a14 ,
               &a21,&a22,&a23,&a24 , &a31,&a32,&a33,&a34 , &nx,&ny,&nz ) ;
   free(lstr) ;
   if( ii < 15 ) return omat ;

   LOAD_MAT44( omat.mat ,
               a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ) ;
   omat.nx = nx ; omat.ny = ny ; omat.nz = nz ;
   return omat ;
}

/*-------------------------------------------------------------------------*/

float_triple EDIT_geometry_string_to_delxyz( char *gstr )
{
   mat44_nxyz omat ; float_triple dxyz={0.0f,0.0f,0.0f} ;

   omat = EDIT_geometry_string_to_mat44(gstr) ;
   if( omat.nx == 0 ) return dxyz ;

   dxyz.a = MAT44_COLNORM(omat.mat,0) ;
   dxyz.b = MAT44_COLNORM(omat.mat,1) ;
   dxyz.c = MAT44_COLNORM(omat.mat,2) ; return dxyz ;
}

/*-------------------------------------------------------------------------*/

float EDIT_geometry_string_diff( char *astr , char *bstr )
{
   mat44_nxyz amat , bmat ;
   float err=0.0f ;

   if( astr == NULL || bstr == NULL ) return 666.0f ;  /* bad inputs */

   if( strcasecmp(astr,bstr) == 0 ) return err ;  /* identical strings! */

   amat = EDIT_geometry_string_to_mat44(astr) ;
   bmat = EDIT_geometry_string_to_mat44(bstr) ;

   if( amat.nx != bmat.nx ) err += 1000.0f ;  /* grid size mismatch! */
   if( amat.ny != bmat.ny ) err += 1000.0f ;
   if( amat.nz != bmat.nz ) err += 1000.0f ;

   err += fabsf(amat.mat.m[0][0]-bmat.mat.m[0][0])  /* check for */
         +fabsf(amat.mat.m[0][1]-bmat.mat.m[0][1])  /* matrix mismatch */
         +fabsf(amat.mat.m[0][2]-bmat.mat.m[0][2])
         +fabsf(amat.mat.m[0][3]-bmat.mat.m[0][3])
         +fabsf(amat.mat.m[1][0]-bmat.mat.m[1][0])
         +fabsf(amat.mat.m[1][1]-bmat.mat.m[1][1])
         +fabsf(amat.mat.m[1][2]-bmat.mat.m[1][2])
         +fabsf(amat.mat.m[1][3]-bmat.mat.m[1][3])
         +fabsf(amat.mat.m[2][0]-bmat.mat.m[2][0])
         +fabsf(amat.mat.m[2][1]-bmat.mat.m[2][1])
         +fabsf(amat.mat.m[2][2]-bmat.mat.m[2][2])
         +fabsf(amat.mat.m[2][3]-bmat.mat.m[2][3]) ;

   return err ;
}

/*-------------------------------------------------------------------------*/
/* pad a geometry string (same in all directions) */

char * EDIT_geometry_string_pad( char *gsin , int npad )
{
   char *gsout ; mat44 cmat ; mat44_nxyz cmat_nxyz ;
   float dx,dy,dz , xorg,yorg,zorg ; int nx,ny,nz ;

   if( npad <= 0 ) return NULL ;

   cmat_nxyz = EDIT_geometry_string_to_mat44( gsin ) ;
   if( cmat_nxyz.nx <= 0 ) return NULL ;
   cmat = cmat_nxyz.mat; nx = cmat_nxyz.nx; ny = cmat_nxyz.ny; nz = cmat_nxyz.nz;
   dx = MAT44_CLEN(cmat,0); dy = MAT44_CLEN(cmat,1); dz = MAT44_CLEN(cmat,2);

   MAT44_VEC(cmat,-npad,-npad,-npad,xorg,yorg,zorg) ;
   LOAD_MAT44_VEC(cmat,xorg,yorg,zorg) ;

   gsout = EDIT_imat_to_geometry_string( cmat, nx+2*npad,ny+2*npad,nz+2*npad ) ;
   return gsout ;
}

/*-------------------------------------------------------------------------*/
/* Return a geomstring that includes the entire volume of all of them. */

char * EDIT_geomstring_from_collection( int nstr , char **gsin )
{
   char *gs , *hs ;
   int ii , ndif=0 ; float val ;
   float xxbot,xxtop, yybot,yytop, zzbot,zztop, dxyzbot ;
   float xxmin,xxmax, yymin,yymax, zzmin,zzmax ;
   THD_3dim_dataset *qset ; mat44 cmat ; int nxnew,nynew,nznew ;

ENTRY("EDIT_geomstring_from_collection") ;

   if( nstr <= 0 || gsin == NULL ) RETURN(NULL) ;  /* ztoopid caller */
   gs = gsin[0] ;
   if( nstr == 1 ) RETURN(strdup(gs)) ;            /* the easy case */

   for( ii=1 ; ii < nstr ; ii++ ){
     hs = gsin[ii] ; val = EDIT_geometry_string_diff(gs,hs) ;
     if( val > 0.01f ){
#if 0
       INFO_message("Warp geometries '%s' and '%s' differ by %f",gs,hs,val) ;
#endif
       ndif++ ;
     }
   }
   if( ndif == 0 ){                                /* the easy case */
#if 0
     INFO_message("No warp geometry differences") ;
#endif
     RETURN(strdup(gs)) ;
   }

   /* find a box big enough to hold everybody */

   xxbot = yybot = zzbot =  WAY_BIG ; dxyzbot = WAY_BIG ;
   xxtop = yytop = zztop = -WAY_BIG ;
   for( ii=0 ; ii < nstr ; ii++ ){
     gs = gsin[ii] ; qset = EDIT_geometry_constructor(gs,"Junk") ;
     THD_set_dicom_box(qset->daxes) ;
     xxmin = qset->daxes->dicom_xxmin ; yymin = qset->daxes->dicom_yymin ; zzmin = qset->daxes->dicom_zzmin ;
     xxmax = qset->daxes->dicom_xxmax ; yymax = qset->daxes->dicom_yymax ; zzmax = qset->daxes->dicom_zzmax ;
     if( xxmin < xxbot ) xxbot = xxmin; if( yymin < yybot ) yybot = yymin; if( zzmin < zzbot ) zzbot = zzmin;
     if( xxmax > xxtop ) xxtop = xxmax; if( yymax > yytop ) yytop = yymax; if( zzmax > zztop ) zztop = zzmax;
     xxmin = fabsf(DSET_DX(qset)); yymin = fabsf(DSET_DY(qset)); zzmin = fabsf(DSET_DY(qset));
     if( xxmin < dxyzbot ) dxyzbot = xxmin; if( yymin < dxyzbot ) dxyzbot = yymin; if( zzmin < dxyzbot ) dxyzbot = zzmin;
   }
   nxnew = 1 + (int)((xxtop-xxbot)/dxyzbot) ;
   nynew = 1 + (int)((yytop-yybot)/dxyzbot) ;
   nznew = 1 + (int)((zztop-zzbot)/dxyzbot) ;
   LOAD_MAT44(cmat , dxyzbot , 0.0f    , 0.0f    , xxbot ,
                     0.0f    , dxyzbot , 0.0f    , yybot ,
                     0.0f    , 0.0f    , dxyzbot , zzbot  ) ;
   gs = EDIT_imat_to_geometry_string( cmat , nxnew,nynew,nznew ) ;

   RETURN(gs) ;
}
