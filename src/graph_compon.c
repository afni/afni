/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*----------------------------------------------------------------
   Find connected components in a graph.
   Inputs:
     int    npt  = # of points
     int ** gmat = gmat[i] is the connectivity list of pt # i
                   gmat[i][0] (=nc) is the number of connections
                   (if 0, can have gmat[i] = NULL to save space)
                   gmat[i][1..nc] is the index of connected points
                   (if gmat[i][j] < 0, this pt is skipped)
   Output
     int * ncom   = *ncom is the number of components found
     int *** cmat = *cmat[j] is the list of points in component # j
                    *cmat[j][0] (=nc) is the number of points
                    *cmat[j][1..nc] is the list of points

   If *ncom is returned as 0, something bad happened.  Otherwise,
   *cmat and *cmat[j] are malloc()-ed and should be free()-ed
   someday.
------------------------------------------------------------------*/

void GRAPH_find_components( int npt, int ** gmat, int * ncom, int *** cmat )
{
   int ** cm ;  /* will be *cmat */
   int ncm ;    /* will be *ncom */
   int cmall , cmuse ;
   byte * used ; /* used up point list */
   int ijk , ijk_last , ncon , ii,kk,pkk,nkk,pii , *tcm ;

   if( ncom == NULL ) return ;                             /* error */
   *ncom = 0 ;                                             /* default return */
   if( npt <= 0 || gmat == NULL || cmat == NULL ) return ; /* error */

   cm  = (int **) malloc( sizeof(int *) * 1 ) ;            /* initialize */
   ncm = 0 ;                                               /* # of compon */

   used = (byte *) calloc( npt , sizeof(byte) ) ;          /* used up? */

   /*--- scan through array,
         find nonzero point,
         build a component about it, start scan over again ---*/

   ijk_last = 0 ;
   do {
      for( ijk=ijk_last ; ijk < npt ; ijk++ )
         if( gmat[ijk] != NULL && gmat[ijk][0] > 0 && !used[ijk] ) break ;

      if( ijk == npt ) break ;  /* didn't find any! */

      ijk_last = ijk+1 ;        /* start here next time */

      /* make a new component */

      used[ijk] = 1 ;                           /* mark this point as used */
      ncon = gmat[ijk][0] ;

      ncm++ ;                                                /* # compon   */
      cm = (int **) realloc( cm , sizeof(int *) * ncm ) ;    /* add compon  */
      cm[ncm-1] = (int *) malloc( sizeof(int) * (ncon+8) ) ; /* compon array */
      cmuse = 1 ;                                            /* has 1 point   */
      cm[ncm-1][1] = ijk ;                                   /* add that point */
      cmall = (ncon+8) ;                                     /* space allocated */

      /*--
        for each point now in component:
           add all points connected to it to the component,
             that aren't already used up
           continue until end of component is reached
             (note that component is expanding as we progress)
      --*/

      for( kk=1 ; kk <= cmuse ; kk++ ){
         pkk = cm[ncm-1][kk] ;                 /* kk-th point in component */
         nkk = (gmat[pkk] == NULL) ? 0         /* # pts attached to it */
                                   : gmat[pkk][0] ;

         for( ii=1 ; ii <= nkk ; ii++ ){
            pii = gmat[pkk][ii] ;              /* ii-th point attached to pkk */
            if( pii >= 0 && !used[pii] ){      /* pii not used yet? */
                                               /* then add pii to component */

               if( ++cmuse >= cmall ){         /* expand component if needed */
                  cmall     = cmuse+32 ;
                  cm[ncm-1] = (int *) realloc( cm[ncm-1], sizeof(int)*cmall ) ;
               }

               cm[ncm-1][cmuse] = pii ;        /* add pii to component */
               used[pii] = 1 ;                 /* mark pii as used */
            }
         }
      }                                        /* this component is done */

      cm[ncm-1][0] = cmuse ;                   /* store size of component */

      if( cmall > cmuse+1 )                    /* trim component array */
         cm[ncm-1] = (int *) realloc( cm[ncm-1], sizeof(int)*(cmuse+1) ) ;

   } while( 1 ) ;  /* end of loop over components */

   /* prepare for output */

   free(used) ;                                /* toss the trash */

   if( ncm == 0 ){ free(cm) ; cm = NULL ; }    /* no components!? */

   /* sort components by size */

   for( kk=0 ; kk < ncm ; kk++ ){
      for( ijk=0,ii=1 ; ii < ncm ; ii++ ){
         if( cm[ii-1][0] < cm[ii][0] ){
            tcm = cm[ii-1] ; cm[ii-1] = cm[ii] ; cm[ii] = tcm ; ijk++ ;
         }
      }
      if( !ijk ) break ;
   }

   *ncom = ncm ; *cmat = cm ; return ;
}
