/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "coxplot.h"

/* type for a partition set */

typedef struct {
   int npart , nall ;
   int * typ ;
   float * x , * y ;
} kpart ;

#define INCKP 16

/* macro to create a new partition set */

#define NEWKP(kp)                             \
  do{ int nn = INCKP ;                        \
      (kp) = malloc(sizeof(kpart)) ;          \
      (kp)->typ = malloc(sizeof(int)  *nn ) ; \
      (kp)->x   = malloc(sizeof(float)*nn ) ; \
      (kp)->y   = malloc(sizeof(float)*nn ) ; \
      (kp)->nall= nn ; (kp)->npart = 1 ;      \
      (kp)->typ[0] = LONG_TYPE ;              \
      (kp)->x[0] = 0.0 ; (kp)->y[0] = 0.0 ; } while(0)

/* macro to make sure a partition set has at least n components */

#define ATLEAST(kp,n)                                           \
  do{ if( (kp)->nall < (n) ){                                   \
         int nn = (n)+INCKP ;                                   \
         (kp)->typ = realloc( (kp)->typ , sizeof(int)  *nn ) ;  \
         (kp)->x   = realloc( (kp)->x   , sizeof(float)*nn ) ;  \
         (kp)->y   = realloc( (kp)->y   , sizeof(float)*nn ) ;  \
         (kp)->nall= (nn) ;                                     \
      } } while(0)

#define LONG_TYPE       1
#define TRAN_TYPE       2
#define ILLEGAL_TYPE -666

#define DEL      0.001
#define FEQ(a,b) (fabs((a)-(b)) < DEL)

/* prototypes */

void collapse_illegals( kpart * kp ) ;
void collapse_xy( kpart * kp ) ;
void move( kpart * kp , float dx , float dy ) ;
void flip( kpart * kp ) ;
void flip90( kpart * kp ) ;
void flip180( kpart * kp ) ;
void crush( kpart * kp ) ;

/*----------------------------------------------------------------------*/

void collapse_illegals( kpart * kp )
{
   int ii , jj , jtop ;

   if( kp == NULL ) return ;

   /* find uppermost legal entry */

   for( jj=kp->npart-1 ; jj >= 0 ; jj-- )
      if( kp->typ[jj] >= 0 ) break ;

   if( jj <  0 ){ kp->npart = 0; return; }
   if( jj == 0 ){ kp->npart = 1; return; }

   jtop = jj ;

   /* for each entry below this, see if it is legal */

   for( jj=0 ; jj < jtop ; ){

      if( kp->typ[jj] < 0 ){                   /* illegal entry at jj   */
         for( ii=jj+1 ; ii <= jtop ; ii++ ){   /* => move all those     */
            kp->typ[ii-1] = kp->typ[ii] ;      /*    above jj down by 1 */
            kp->x[ii-1]   = kp->x[ii] ;
            kp->y[ii-1]   = kp->y[ii] ;
         }
         jtop-- ;                              /* top index is reduced */
      } else {
         jj++ ;                                /* go to next entry jj */
      }
   }

   return ;
}

/*----------------------------------------------------------------------*/

void collapse_xy( kpart * kp )
{
   int ii , jj ;

   if( kp == NULL || kp->npart < 2 ) return ;

   /* find all partitions that are at the same place */

   for( jj=1 ; jj < kp->npart ; jj++ ){
      if( kp->typ[jj] < 0 ) continue ;         /* skip illegals */

      for( ii=0 ; ii < jj ; ii++ ){            /* check all entries below jj */

         if( kp->typ[ii] >= 0         &&       /* if legal */
             FEQ(kp->x[ii],kp->x[jj]) &&       /* and at same place */
             FEQ(kp->y[ii],kp->y[jj])   ){

            kp->typ[jj] = ILLEGAL_TYPE ;       /* mark for demolition */
            break ;
         }
      }
   }

   collapse_illegals( kp ) ;                   /* demolition */
   return ;
}

/*----------------------------------------------------------------------*/

void move( kpart * kp , float dx , float dy )
{
   int ii ;

   if( kp == NULL || kp->npart == 0 ) return ;

   for( ii=0 ; ii < kp->npart ; kp++ ){
      if( kp->typ[ii] == TRAN_TYPE ){     /* only transverse components move */
         kp->x[ii] += dx ;
         kp->y[ii] += dy ;
      }
   }
   return ;
}

/*----------------------------------------------------------------------*/

void flip( kpart * kp )
{
   int ii , itop , jj ;

   if( kp == NULL || kp->npart == 0 ) return ;

   itop = kp->npart ;
   for( ii=0 ; ii < itop ; kp++ ){

      jj = kp->npart ;

      switch( kp->typ[ii] ){

         case TRAN_TYPE:{                   /* transverse magnetization   */
            ATLEAST(kp,jj+4) ;              /* component breaks into 4    */
                                            /* pieces: 1 at same place,   */
            kp->typ[jj] = TRAN_TYPE ;       /*         1 new transverse   */
            kp->x[jj]   = - kp->x[ii] ;     /*     and 2 new longitudinal */
            kp->y[jj]   = - kp->y[ii] ;

            kp->typ[jj+1] = LONG_TYPE ;
            kp->x[jj+1]   = kp->x[ii] ;
            kp->y[jj+1]   = kp->y[ii] ;

            kp->typ[jj+2] = LONG_TYPE ;
            kp->x[jj+2]   = - kp->x[ii] ;
            kp->y[jj+2]   = - kp->y[ii] ;
         }
         break ;

         case LONG_TYPE:{                   /* longitudinal magnetization */
            ATLEAST(kp,jj+1) ;              /* breaks into 2 pieces:      */
                                            /*   1 at same place          */
            kp->typ[jj] = LONG_TYPE ;       /*   1 new transverse         */
            kp->x[jj]   = kp->x[ii] ;
            kp->y[jj]   = kp->y[ii] ;
         }
         break ;

      }
   }

   collapse_xy(kp) ;
   return ;
}

/*----------------------------------------------------------------------*/

void flip90( kpart * kp )
{
   int ii , itop , jj ;

   if( kp == NULL || kp->npart == 0 ) return ;

   itop = kp->npart ;
   for( ii=0 ; ii < itop ; kp++ ){

      jj = kp->npart ;

      switch( kp->typ[ii] ){

         case TRAN_TYPE:{                  /* rules for transverse are */
            ATLEAST(kp,jj+4) ;             /* same as previous case    */

            kp->typ[jj] = LONG_TYPE ;
            kp->x[jj]   = - kp->x[ii] ;
            kp->y[jj]   = - kp->y[ii] ;

            kp->typ[jj+1] = TRAN_TYPE ;
            kp->x[jj+1]   = kp->x[ii] ;
            kp->y[jj+1]   = kp->y[ii] ;

            kp->typ[jj+2] = TRAN_TYPE ;
            kp->x[jj+2]   = - kp->x[ii] ;
            kp->y[jj+2]   = - kp->y[ii] ;
         }
         break ;

         case LONG_TYPE:{                  /* longitudinal is just         */
            kp->typ[ii] = LONG_TYPE ;      /* converted to pure transverse */
         }
         break ;

      }
   }

   collapse_xy(kp) ;
   return ;
}

/*----------------------------------------------------------------------*/

void flip180( kpart * kp )
{
   int ii , itop ;

   if( kp == NULL || kp->npart == 0 ) return ;

   itop = kp->npart ;
   for( ii=0 ; ii < itop ; kp++ ){
      if( kp->typ[ii] == TRAN_TYPE ){   /* transverse flips over */
         kp->x[ii] = - kp->x[ii] ;
         kp->y[ii] = - kp->y[ii] ;
      }
   }

   collapse_xy(kp) ;
   return ;
}

/*----------------------------------------------------------------------*/

void crush( kpart * kp )
{
   int ii , itop ;

   if( kp == NULL || kp->npart == 0 ) return ;

   itop = kp->npart ;
   for( ii=0 ; ii < itop ; kp++ ){
      if( kp->typ[ii] == TRAN_TYPE ){   /* transverse is killed */
         kp->typ[ii] = ILLEGAL_TYPE ;
      }
   }

   collapse_xy(kp) ;
   return ;
}

/*----------------------------------------------------------------------*/
