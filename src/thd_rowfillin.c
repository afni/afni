#include "mrilib.h"

/*-------------------------------------------------------------------
   Fill in the blanks (zeros) in a row of shorts that are at most
   maxgap in length, and are bounded by the same value.
   Return value is number of filled-in blanks.
---------------------------------------------------------------------*/

static int THD_rowfillin_short( int nrow , short * row , int maxgap )
{
   int ii , nfill=0 , jj ;
   short vbot ;

   /*-- skip zeros at start --*/

   for( ii=0 ; ii < nrow && row[ii] == 0 ; ii++ ) ; /* nada */
   if( ii == nrow ) return nfill ;                    /*** was all zeros ***/

   /*-- row[ii] is not zero here; now find a gap at or above this --*/

#if 0
{ int kk ; fprintf(stderr,"maxgap=%d: row in:",maxgap);
  for( kk=0 ; kk < nrow ; kk++ ) fprintf(stderr," %d",row[kk]) ;
  fprintf(stderr,"\n") ; }
#endif

   while( 1 ){
      /*-- find next zero value (start of blank region) --*/

      for( ; ii < nrow && row[ii] != 0 ; ii++ ) ; /* nada */
      if( ii == nrow ) return nfill ;          /*** didn't find any zero ***/

#if 0
fprintf(stderr,"  gap start at %d\n",ii) ;
#endif

      vbot = row[ii-1] ;                          /* value just before gap */

      /*-- find next nonzero value above this zero --*/

      for( jj=ii+1 ; jj < nrow && row[jj] == 0 ; jj++ ) ; /* nada */
      if( jj == nrow ) return nfill ;      /*** was all zeros to the end ***/

#if 0
fprintf(stderr,"  gap end at %d: vbot=%d row[jj]=%d\n",jj,vbot,row[jj]) ;
#endif

      if( row[jj] == vbot && jj-ii <= maxgap ){ /*** fill in this gap!!! ***/
         nfill += (jj-ii) ;
         for( ; ii < jj ; ii++ ) row[ii] = vbot ;
#if 0
{ int kk ; fprintf(stderr,"filled:");
  for( kk=0 ; kk < nrow ; kk++ ) fprintf(stderr," %d",row[kk]) ;
  fprintf(stderr,"\n") ; }
#endif
      }

      ii = jj ;                      /* loop back and look for another gap */

   } /* endless loop */
}

/*------------------------------------------------------------------------*/

static int THD_rowfillin_byte( int nrow , byte * row , int maxgap )
{
   int ii , nfill=0 , jj ;
   byte vbot ;

   /*-- skip zeros --*/

   for( ii=0 ; ii < nrow && row[ii] == 0 ; ii++ ) ; /* nada */
   if( ii == nrow ) return nfill ;                    /*** was all zeros ***/

   /*-- row[ii] is not zero here; now find a gap at or above this --*/

   while( 1 ){
      /*-- find next zero value --*/

      for( ; ii < nrow && row[ii] != 0 ; ii++ ) ; /* nada */
      if( ii == nrow ) return nfill ;          /*** didn't find any zero ***/

      vbot = row[ii-1] ;                          /* value at start of gap */

      /*-- find next nonzero value above this zero --*/

      for( jj=ii+1 ; jj < nrow && row[jj] == 0 ; jj++ ) ; /* nada */
      if( jj == nrow ) return nfill ;      /*** was all zeros to the end ***/

      if( row[jj] == vbot && jj-ii <= maxgap ){ /*** fill in this gap!!! ***/
         nfill += (jj-ii) ;
         for( ; ii < jj ; ii++ ) row[ii] = vbot ;
      }

      ii = jj ;                      /* loop back and look for another gap */

   } /* endless loop */
}

/*------------------------------------------------------------------------*/

static int THD_rowfillin_float( int nrow , float * row , int maxgap )
{
   int ii , nfill=0 , jj ;
   float vbot ;

   /*-- skip zeros --*/

   for( ii=0 ; ii < nrow && row[ii] == 0 ; ii++ ) ; /* nada */
   if( ii == nrow ) return nfill ;                    /*** was all zeros ***/

   /*-- row[ii] is not zero here; now find a gap at or above this --*/

   while( 1 ){
      /*-- find next zero value --*/

      for( ; ii < nrow && row[ii] != 0 ; ii++ ) ; /* nada */
      if( ii == nrow ) return nfill ;          /*** didn't find any zero ***/

      vbot = row[ii-1] ;                          /* value at start of gap */

      /*-- find next nonzero value above this zero --*/

      for( jj=ii+1 ; jj < nrow && row[jj] == 0 ; jj++ ) ; /* nada */
      if( jj == nrow ) return nfill ;      /*** was all zeros to the end ***/

      if( row[jj] == vbot && jj-ii <= maxgap ){ /*** fill in this gap!!! ***/
         nfill += (jj-ii) ;
         for( ; ii < jj ; ii++ ) row[ii] = vbot ;
      }

      ii = jj ;                      /* loop back and look for another gap */

   } /* endless loop */
}

/*---------------------------------------------------------------------------
   Do the fillin thing on each 1D row from a dataset sub-brick.
   Return value is number of voxels filled in (-1 if an error transpired).
-----------------------------------------------------------------------------*/

int THD_dataset_rowfillin( THD_3dim_dataset * dset, int ival, int dcode, int maxgap )
{
   int kind , xx,yy,zz , nrow , nx,ny,nz ;
   int xtop,ytop,ztop , nff , nfftot=0 ;

ENTRY("THD_dataset_rowfillin") ;

   if( !ISVALID_DSET(dset)      ||
       ival < 0                 ||
       ival >= DSET_NVALS(dset) ||
       maxgap < 1                 ) RETURN(-1) ;  /* bad things */

   kind = DSET_BRICK_TYPE(dset,ival) ;
   if( kind != MRI_short && kind != MRI_byte && kind != MRI_float ) RETURN(-1) ;  /* bad */

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

      case MRI_short:{
         short * row ;
         for( zz=0 ; zz < ztop ; zz++ )
          for( yy=0 ; yy < ytop ; yy++ )
            for( xx=0 ; xx < xtop ; xx++ ){
               row = THD_get_dset_row( dset,ival , dcode,xx,yy,zz ) ;
               nff = THD_rowfillin_short( nrow , row , maxgap ) ;
               if( nff > 0 ){
                  THD_put_dset_row( dset,ival , dcode,xx,yy,zz , row ) ;
                  nfftot += nff ;
               }
               free(row) ;
            }
      }
      break ;

      case MRI_byte:{
         byte * row ;
         for( zz=0 ; zz < ztop ; zz++ )
          for( yy=0 ; yy < ytop ; yy++ )
            for( xx=0 ; xx < xtop ; xx++ ){
               row = THD_get_dset_row( dset,ival , dcode,xx,yy,zz ) ;
               nff = THD_rowfillin_byte( nrow , row , maxgap ) ;
               if( nff > 0 ){
                  THD_put_dset_row( dset,ival , dcode,xx,yy,zz , row ) ;
                  nfftot += nff ;
               }
               free(row) ;
            }
      }
      break ;

      case MRI_float:{
         float * row ;
         for( zz=0 ; zz < ztop ; zz++ )
          for( yy=0 ; yy < ytop ; yy++ )
            for( xx=0 ; xx < xtop ; xx++ ){
               row = THD_get_dset_row( dset,ival , dcode,xx,yy,zz ) ;
               nff = THD_rowfillin_float( nrow , row , maxgap ) ;
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
