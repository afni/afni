#include "mrilib.h"

/*-------------------------------------------------------------------
   Fill in the zeros in a row of bytes that are at most maxstep from
   a nonzero value.  Return value is number of filled-in zeros.
---------------------------------------------------------------------*/

static int THD_zfillin_byte( int nrow , byte * row , int maxstep )
{
   int ii , nfill=0 , jj ;
   byte vbot ;
   static byte *trow = NULL ; static int ntrow = 0 ;

   if( ntrow != nrow ){
     trow = AFREALL(trow , byte, sizeof(byte)*nrow ) ;
     ntrow = nrow ;
   }
   memcpy(trow,row,sizeof(byte)*nrow) ;

   for( ii=0 ; ii < nrow ; ii++ ){
      if( row[ii] != 0 ) continue ;  /* skip values already nonzero */

      /* scan up and down for nearest nonzero */

      for( jj=1 ; jj <= maxstep ; jj++ ){
         if( ii+jj< nrow && row[ii+jj]!=0 ){trow[ii]=row[ii+jj]; nfill++; break;}
         if( ii-jj>= 0   && row[ii-jj]!=0 ){trow[ii]=row[ii-jj]; nfill++; break;}
      }
   }

   if( nfill > 0 ) memcpy(row,trow,sizeof(byte)*nrow) ;
   return nfill;
}

/*---------------------------------------------------------------------------
   Do the fillin thing on each 1D row from a dataset sub-brick.
   Return value is number of voxels filled in (-1 if an error transpired).
-----------------------------------------------------------------------------*/

int THD_dataset_zfillin( THD_3dim_dataset * dset, int ival, int dcode, int maxstep )
{
   int kind , xx,yy,zz , nrow , nx,ny,nz ;
   int xtop,ytop,ztop , nff , nfftot=0 ;

ENTRY("THD_dataset_rowfillin") ;

   if( !ISVALID_DSET(dset)      ||
       ival < 0                 ||
       ival >= DSET_NVALS(dset) ||
       maxstep < 1                ) RETURN(-1) ;  /* bad things */

   kind = DSET_BRICK_TYPE(dset,ival) ;
   if( kind != MRI_byte ) RETURN(-1) ;  /* bad */

   nrow = THD_get_dset_rowcount( dset , dcode ) ;
   if( nrow < 1 ) RETURN(-1) ;  /* bad */

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ;
   nz = DSET_NZ(dset) ;

   xtop = ytop = ztop = 1 ;
   switch( dcode ){
      case 1: case -1: ytop=ny ; ztop=nz ; break ;
      case 2: case -2: xtop=nx ; ztop=nz ; break ;
      case 3: case -3: xtop=nx ; ytop=ny ; break ;
   }

   switch( kind ){

      case MRI_byte:{
         byte * row ;
         for( zz=0 ; zz < ztop ; zz++ )
          for( yy=0 ; yy < ytop ; yy++ )
            for( xx=0 ; xx < xtop ; xx++ ){
               row = THD_get_dset_row( dset,ival , dcode,xx,yy,zz ) ;
               nff = THD_zfillin_byte( nrow , row , maxstep ) ;
               if( nff > 0 ){
                  THD_put_dset_row( dset,ival , dcode,xx,yy,zz , row ) ;
                  nfftot += nff ;
               }
               free(row) ;
            }
      }
      break ;

   }

   RETURN(nfftot) ;
}
