#include "mrilib.h"

/****************************************************************************
  Functions for dealing with data extracted/inserted from rows of a 3D
  dataset - parallel to x, y, or z axes.
  -- 08 Mar 2001 - RWCox
*****************************************************************************/

/*---------------------------------------------------------------------------
  Get the number of elements in a row of the dataset in a particular
  direction, dcode:
    +1 = x direction  -1 = reversed x direction
    +2 = y direction  -2 = reversed y direction
    +3 = z direction  -3 = reversed z direction
  This routine is mostly for the pitiful programmer who can't be bothered
  to do this calculation himself.
-----------------------------------------------------------------------------*/

int THD_get_dset_rowcount( THD_3dim_dataset * dset, int dcode )
{
   if( !ISVALID_DSET(dset) ) return 0 ;        /* bad */
   switch( dcode ){
      case  1: case -1: return DSET_NX(dset) ; /* good */
      case  2: case -2: return DSET_NY(dset) ; /* good */
      case  3: case -3: return DSET_NZ(dset) ; /* good */
   }
   return 0 ;                                  /* bad */
}

/*---------------------------------------------------------------------------
  Extract a row from a dataset sub-brick in the direction given by dcode.
  The 3-index of a voxel from the row is in (xx,yy,zz).  The return value
  is a pointer to a malloc()-ed array, whose type is given by
  DSET_BRICK_TYPE(dset,ival) and whose length is given by
  THD_get_dset_rowcount(dset,dcode).  If NULL is returned, something bad
  happened.
  N.B.: dcode < 0 ==> data is extracted in the reverse direction.
-----------------------------------------------------------------------------*/

void * THD_get_dset_row( THD_3dim_dataset * dset, int ival,
                         int dcode , int xx,int yy,int zz  )
{
   void * row , * brick ;
   int nrow , kind , nx,ny,nz,nxy , kbot,kdel,kk,ii ;

ENTRY("THD_get_dset_row") ;

   nrow = THD_get_dset_rowcount( dset , dcode ) ;
   if( nrow < 1 ) RETURN(NULL) ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy = nx*ny ;
   nz = DSET_NZ(dset) ;

   /*-- We will extract brick[kbot+i*kdel] for i=0..nrow-1 --*/

   switch( dcode ){
      case 1: case -1:
         if( yy < 0 || yy >= ny || zz < 0 || zz >= nz ) RETURN(NULL) ;
         kbot = yy*nx + zz*nxy ; kdel = 1 ;
      break ;

      case 2: case -2:
         if( xx < 0 || xx >= nx || zz < 0 || zz >= nz ) RETURN(NULL) ;
         kbot = xx + zz*nxy ; kdel = ny ;
      break ;

      case 3: case -3:
         if( xx < 0 || xx >= nx || yy < 0 || yy >= ny ) RETURN(NULL) ;
         kbot = xx + yy*nx ; kdel = nxy ;
      break ;
   }

   kind  = DSET_BRICK_TYPE(dset,ival) ;
   brick = DSET_ARRAY(dset,ival) ;
   row   = malloc( mri_datum_size((MRI_TYPE)kind) * nrow ) ;

   /*-- extract row, based on kind of data in sub-brick --*/

   switch( kind ){

      default: free(row) ; RETURN(NULL) ;  /* bad */

      case MRI_short:{
         short *rr = (short *)row , *bb = (short *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) rr[ii] = bb[kk+kbot] ;
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) rr[nrow-1-ii] = bb[kk+kbot] ;
      }
      break ;

      case MRI_byte:{
         byte *rr = (byte *)row , *bb = (byte *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) rr[ii] = bb[kk+kbot] ;
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) rr[nrow-1-ii] = bb[kk+kbot] ;
      }
      break ;

      case MRI_float:{
         float *rr = (float *)row , *bb = (float *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) rr[ii] = bb[kk+kbot] ;
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) rr[nrow-1-ii] = bb[kk+kbot] ;
      }
      break ;

      case MRI_complex:{
         complex *rr = (complex *)row , *bb = (complex *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) rr[ii] = bb[kk+kbot] ;
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) rr[nrow-1-ii] = bb[kk+kbot] ;
      }
      break ;

#if 0
      /*** the following data type is not allowed in AFNI (yet) ***/

      case MRI_rgb:{
         byte *rr = (byte *)row , *bb = (byte *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ){
               rr[3*ii  ] = bb[3*(kk+kbot)  ] ;
               rr[3*ii+1] = bb[3*(kk+kbot)+1] ;
               rr[3*ii+2] = bb[3*(kk+kbot)+2] ;
            }
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ){
               rr[3*(nrow-1-ii)  ] = bb[3*(kk+kbot)  ] ;
               rr[3*(nrow-1-ii)+1] = bb[3*(kk+kbot)+1] ;
               rr[3*(nrow-1-ii)+2] = bb[3*(kk+kbot)+2] ;
            }
      }
      break ;
#endif
   }

   RETURN(row) ;
}

/*---------------------------------------------------------------------------
   The inverse of THD_get_dset_row: put data back into a dataset.
-----------------------------------------------------------------------------*/

void THD_put_dset_row( THD_3dim_dataset * dset, int ival,
                       int dcode, int xx,int yy,int zz, void * row )
{
   void * brick ;
   int nrow , kind , nx,ny,nz,nxy , kbot,kdel,kk,ii ;

ENTRY("THD_put_dset_row") ;

   nrow = THD_get_dset_rowcount( dset , dcode ) ;
   if( nrow < 1 ) EXRETURN ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy = nx*ny ;
   nz = DSET_NZ(dset) ;

   /*-- We will insert brick[kbot+i*kdel] for i=0..nrow-1 --*/

   switch( dcode ){
      case 1: case -1:
         if( yy < 0 || yy >= ny || zz < 0 || zz >= nz ) EXRETURN ;
         kbot = yy*nx + zz*nxy ; kdel = 1 ;
      break ;

      case 2: case -2:
         if( xx < 0 || xx >= nx || zz < 0 || zz >= nz ) EXRETURN ;
         kbot = xx + zz*nxy ; kdel = ny ;
      break ;

      case 3: case -3:
         if( xx < 0 || xx >= nx || yy < 0 || yy >= ny ) EXRETURN ;
         kbot = xx + yy*nx ; kdel = nxy ;
      break ;
   }

   kind  = DSET_BRICK_TYPE(dset,ival) ;
   brick = DSET_ARRAY(dset,ival) ;

   /*-- insert row, based on kind of data in sub-brick --*/

   switch( kind ){

      default: EXRETURN ;  /* bad */

      case MRI_short:{
         short *rr = (short *)row , *bb = (short *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) bb[kk+kbot] = rr[ii] ;
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) bb[kk+kbot] = rr[nrow-1-ii] ;
      }
      break ;

      case MRI_byte:{
         byte *rr = (byte *)row , *bb = (byte *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) bb[kk+kbot] = rr[ii] ;
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) bb[kk+kbot] = rr[nrow-1-ii] ;
      }

      case MRI_complex:{
         complex *rr = (complex *)row , *bb = (complex *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) bb[kk+kbot] = rr[ii] ;
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) bb[kk+kbot] = rr[nrow-1-ii] ;
      }

      case MRI_float:{
         float *rr = (float *)row , *bb = (float *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) bb[kk+kbot] = rr[ii] ;
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ) bb[kk+kbot] = rr[nrow-1-ii] ;
      }

#if 0
      /*** the following data type is not allowed in AFNI (yet) ***/

      case MRI_rgb:{
         byte *rr = (byte *)row , *bb = (byte *)brick ;
         if( dcode > 0 )
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ){
               bb[3*(kk+kbot)  ] = rr[3*ii  ] ;
               bb[3*(kk+kbot)+1] = rr[3*ii+1] ;
               bb[3*(kk+kbot)+2] = rr[3*ii+2] ;
            }
         else
            for( ii=kk=0 ; ii < nrow ; ii++,kk+=kdel ){
               bb[3*(kk+kbot)  ] = rr[3*(nrow-1-ii)  ] ;
               bb[3*(kk+kbot)+1] = rr[3*(nrow-1-ii)+1] ;
               bb[3*(kk+kbot)+2] = rr[3*(nrow-1-ii)+2] ;
            }
      }
      break ;
#endif
   }

   EXRETURN ;
}
