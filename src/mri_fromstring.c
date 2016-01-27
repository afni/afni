#include "mrilib.h"

#undef  IS_COLSEP
#define IS_COLSEP(s) ( strcmp((s),"\\")==0 || strcmp((s),"|")==0 )

/*----------------------------------------------------------------------------*/
/*! Produce a 1D (float) image from a string of the form "20@1,10@0,5@1".
    - 13 Apr 2006: make '\' be a column separator.
    - Later yet:   make '|' be a column separator, too.
    - Also see:    mri_1D_tostring().
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_1D_fromstring( char *str )
{
   int ii,nnn,count , ntot=0 ;
   float *far , value=0.0 , cstep=0.0 ;
   NI_str_array *sar ;
   char sep , sepx ;
   MRI_IMAGE *flim ;
   int col_num , *col_len ;

ENTRY("mri_1D_fromstring") ;

   sar = NI_decode_string_list( str , "," ) ;
   if( sar == NULL ){ ERROR_message("Can't decode 1D: string"); RETURN(NULL); }
   if( sar->num == 0 ){
     ERROR_message("1D: string is empty");
     NI_delete_str_array(sar); RETURN(NULL);
   }

   col_num = 1 ; col_len = (int *)calloc(1,sizeof(int)) ;
   far = (float *)malloc(sizeof(float)) ;
   for( ii=0 ; ii < sar->num ; ii++ ){

     cstep = 0.0f ;

     if( strstr(sar->str[ii],"@") != NULL ||    /* if has one of the    */
         strstr(sar->str[ii],"x") != NULL ||    /* allowed separator    */
         strstr(sar->str[ii],"X") != NULL ||    /* characters, then     */
         strstr(sar->str[ii],"*") != NULL   ){  /* scan for count@value */

        nnn = sscanf( sar->str[ii] , "%d%c%f" , &count , &sep , &value ) ;
        if( nnn != 3 || count < 1 ){
          ERROR_message("Illegal 1D: value '%s'",sar->str[ii]) ;
          free(col_len); free(far); NI_delete_str_array(sar); RETURN(NULL);
        }

     } else if( strstr(sar->str[ii],"%") != NULL ){  /* 26 Jan 2016 */
       nnn = sscanf( sar->str[ii] , "%d%c%f%c%f" , &count , &sep , &value , &sepx , &cstep ) ;
       if( nnn != 5 || count < 1 ){
          ERROR_message("Illegal 1D: value '%s'",sar->str[ii]) ;
          free(col_len); free(far); NI_delete_str_array(sar); RETURN(NULL);
       }

     } else if( IS_COLSEP(sar->str[ii]) ){ /* a col separator */
       col_num++ ;
       col_len = (int *)realloc( col_len , sizeof(int)*col_num ) ;
       col_len[col_num-1] = 0 ;
       count = 0 ;

     } else {                                 /* just scan for value */
        count = 1 ;
        nnn   = sscanf( sar->str[ii] , "%f" , &value ) ;
        if( nnn != 1 ){
          ERROR_message("Illegal 1D: value '%s'",sar->str[ii]) ;
          free(col_len); free(far); NI_delete_str_array(sar); RETURN(NULL);
        }
     }

     if( count > 0 ){
       far = (float *) realloc( far , sizeof(float)*(ntot+count) ) ;
       for( nnn=0 ; nnn < count ; nnn++ ) far[nnn+ntot] = value + nnn*cstep ;
       ntot += count ; col_len[col_num-1] += count ;
     }

   }

   NI_delete_str_array(sar) ;
   if( col_num == 1 ){
     flim = mri_new_vol_empty( ntot,1,1 , MRI_float ) ;
     mri_fix_data_pointer( far , flim ) ;
   } else {
#undef  FL
#define FL(i,j) flar[(i)+(j)*nnn]
     float *flar ; int jj,kk ;
     for( nnn=ii=0 ; ii < col_num ; ii++ ) nnn = MAX(nnn,col_len[ii]) ;
     flim = mri_new( nnn, col_num, MRI_float ) ; flar = MRI_FLOAT_PTR(flim) ;
     for( kk=jj=0 ; jj < col_num ; kk+=col_len[jj],jj++ ){
       for( ii=0 ; ii < col_len[jj] ; ii++ ) FL(ii,jj) = far[kk+ii] ;
     }
   }

   free(col_len) ; RETURN(flim) ;
}

/*---------------------------------------------------------------------------*/
/*! Similar to mri_1D_fromstring, but for 'ragged' 1D files. [05 Jan 2007] 
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * mri_read_ragged_fromstring( char *str , float filler )
{
   int ii,nnn,count , ntot=0 ;
   float *far , value=0.0 ;
   NI_str_array *sar ;
   char sep , sval[256] ;
   MRI_IMAGE *flim ;
   int col_num , *col_len ;

ENTRY("mri_read_ragged_fromstring") ;

   sar = NI_decode_string_list( str , "," ) ;
   if( sar == NULL ) RETURN(NULL) ;
   if( sar->num == 0 ){ NI_delete_str_array(sar); RETURN(NULL); }

   col_num = 1 ; col_len = (int *)calloc(1,sizeof(int)) ;
   far = (float *)malloc(sizeof(float)) ;
   for( ii=0 ; ii < sar->num ; ii++ ){

     if( (strstr(sar->str[ii],"@") != NULL ||
          strstr(sar->str[ii],"x") != NULL ||
          strstr(sar->str[ii],"X") != NULL ||
          strstr(sar->str[ii],"*") != NULL   ) && isdigit(*(sar->str[ii])) ){

        nnn = sscanf( sar->str[ii] , "%d%c%s" , &count , &sep , sval ) ;
        if( nnn != 3 || count < 1 ){
          free(col_len); free(far); NI_delete_str_array(sar); RETURN(NULL);
        }
        if( *sval == '*' ) value = filler ;
        else               value = (float)strtod(sval,NULL) ;

     } else if( IS_COLSEP(sar->str[ii]) ){

       col_num++ ;
       col_len = (int *)realloc( col_len , sizeof(int)*col_num ) ;
       col_len[col_num-1] = 0 ;
       count = 0 ;

     } else {                                 /* just scan for value */
        count = 1 ;
        nnn   = sscanf( sar->str[ii] , "%s" , sval ) ;
        if( nnn != 1 ){
          free(col_len); free(far); NI_delete_str_array(sar); RETURN(NULL);
        }
        if( *sval == '*' || isalpha(*sval) ) value = filler ;
        else                                 value = (float)strtod(sval,NULL) ;
     }

     if( count > 0 ){
       far = (float *) realloc( far , sizeof(float)*(ntot+count) ) ;
       for( nnn=0 ; nnn < count ; nnn++ ) far[nnn+ntot] = value ;
       ntot += count ; col_len[col_num-1] += count ;
     }

   }

   NI_delete_str_array(sar) ;

   if( col_num == 1 ){
     flim = mri_new_vol_empty( ntot,1,1 , MRI_float ) ;
     mri_fix_data_pointer( far , flim ) ;
   } else {
#undef  FL
#define FL(i,j) flar[(i)+(j)*nnn]
     float *flar ; int jj,kk ;
     for( nnn=ii=0 ; ii < col_num ; ii++ ) nnn = MAX(nnn,col_len[ii]) ;
     flim = mri_new( nnn, col_num, MRI_float ) ; flar = MRI_FLOAT_PTR(flim) ;
     for( jj=0 ; jj < flim->nvox ; jj++ ) flar[jj] = filler ;
     for( kk=jj=0 ; jj < col_num ; kk+=col_len[jj],jj++ ){
       for( ii=0 ; ii < col_len[jj] ; ii++ ) FL(ii,jj) = far[kk+ii] ;
     }
     free(far) ;
   }

   free(col_len) ; RETURN(flim) ;
}

/*--------------------------------------------------------------------------*/
/*! Kind of the inverse to mri_1D_fromstring().
    free() the output string when you are done with it!
*//*------------------------------------------------------------------------*/

char * mri_1D_tostring( MRI_IMAGE *im )  /* 17 Nov 2007 */
{
   char *outbuf = NULL ;
   int nx,ny , ii,jj ;
   MRI_IMAGE *tim ;
   float *far ;

ENTRY("mri_1D_tostring") ;

   if( im == NULL || im->nz > 1 ) RETURN(NULL) ;

   if( im->kind != MRI_float ) tim = mri_to_float(im) ;
   else                        tim = im ;

   nx = tim->nx ; ny = tim->ny ; far = MRI_FLOAT_PTR(tim) ;

   outbuf = THD_zzprintf( outbuf , "%s" , "1D:" ) ;
   for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       outbuf = THD_zzprintf( outbuf , " %g" , far[ii+jj*nx] ) ;
     }
     if( jj < ny-1 ) outbuf = THD_zzprintf( outbuf , "%s" , " |" ) ;
   }

   if( tim != im ) mri_free(tim) ;
   RETURN(outbuf) ;
}
