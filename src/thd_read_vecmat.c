#include "mrilib.h"

/*-------------------------------------------------------------------
  08 Feb 2001: read a matrix+vector from a file - RWCox
  14 Feb 2001: modified to add "-rotate ..." mode
---------------------------------------------------------------------*/

THD_dvecmat THD_read_dvecmat( char * fname , int invert )
{
   THD_dvecmat dvm ;
   THD_dmat33 tmat ;
   THD_dfvec3 tvec ;
   FILE * fp ;
   double dd[12] ;
   int  nn ;

ENTRY("THD_read_dvecmat") ;

   LOAD_DIAG_DMAT(tmat,0.0,0.0,0.0) ;  /* initialize to all 0s */
   LOAD_DFVEC3(tvec,0.0,0.0,0.0) ;

   if( fname == NULL || fname[0] == '\0' ){
      dvm.vv = tvec ; dvm.mm = tmat ; RETURN(dvm) ;
   }

   /*-- filename is "dataset::attribute" --*/

   if( strstr(fname,"::") != NULL ){  /*== read from dataset header ==*/
      char * dname = strdup(fname) ;
      char * cc = strstr(dname,"::") ;
      THD_3dim_dataset * dset ;
      ATR_float * atr ;

      *cc = '\0' ; cc += 2 ;  /* dname = dataset name ; cc = attribute name */

      dset = THD_open_one_dataset( dname ) ;
      if( !ISVALID_DSET(dset) ){
         fprintf(stderr,
                 "*** THD_read_dvecmat: can't open dataset %s\n",
                 dname) ;
         dvm.vv = tvec ; dvm.mm = tmat ; free(dname) ; RETURN(dvm) ;
      }

      atr = THD_find_float_atr( dset->dblk , cc ) ;
      if( atr == NULL ){
         fprintf(stderr,
                 "*** THD_read_dvecmat: can't find attribute %s in dataset %s\n",
                 cc,dname) ;
         dvm.vv = tvec ; dvm.mm = tmat ; free(dname) ; RETURN(dvm) ;
      }

      switch( atr->nfl ){

         default:
            fprintf(stderr,
                    "*** THD_read_dvecmat: can't read matrix+vector from dataset %s, attribute %s\n",
                    dname,cc) ;
            dvm.vv = tvec ; dvm.mm = tmat ; free(dname) ; RETURN(dvm) ;
         break ; /* unreachable */

         case 12: LOAD_DMAT(tmat,atr->fl[0],atr->fl[1],atr->fl[2],
                                 atr->fl[4],atr->fl[5],atr->fl[6],
                                 atr->fl[8],atr->fl[9],atr->fl[10] ) ;
                  LOAD_DFVEC3(tvec,atr->fl[3],atr->fl[7],atr->fl[11]) ;
         break ;

         case 9:  LOAD_DMAT(tmat,atr->fl[0],atr->fl[1],atr->fl[2],
                                 atr->fl[3],atr->fl[4],atr->fl[5],
                                 atr->fl[6],atr->fl[7],atr->fl[8] ) ;
                  LOAD_DFVEC3(tvec,0,0,0) ;
         break ;
      }

      free(dname) ; DSET_delete(dset) ;

   /*-- 14 Feb 2001: filename is "-rotate a b c -[ab]shift x y z" string --*/

   } else if( strstr(fname,"-rotate") != NULL ){  /*== compute directly ==*/

      dvm = THD_rotcom_to_matvec( NULL , fname ) ;  /* thd_rotangles.c */
      tvec = dvm.vv ; tmat = dvm.mm ;

   /*-- just a normal filename --*/

   } else {  /*== read numbers from file ==*/

      fp = fopen( fname , "r" ) ;
      if( fp == NULL ){
         fprintf(stderr,
                 "*** THD_read_dvecmat: can't open file %s\n",
                 fname) ;
         dvm.vv = tvec ; dvm.mm = tmat ; RETURN(dvm) ;
      }

      nn = fscanf(fp,"%lf %lf %lf %lf"      /* try to get 12 numbers */
                     "%lf %lf %lf %lf"
                     "%lf %lf %lf %lf" ,
                  dd+0,dd+1,dd+2 ,dd+3,
                  dd+4,dd+5,dd+6 ,dd+7,
                  dd+8,dd+9,dd+10,dd+11 ) ;

      switch( nn ){  /* how many did we actually read? */

         default:
            fprintf(stderr,
                    "*** THD_read_dvecmat: can't read matrix+vector from file %s\n",
                    fname) ;
            dvm.vv = tvec ; dvm.mm = tmat ; RETURN(dvm) ;
         break ; /* unreachable */

         case 12: LOAD_DMAT(tmat,dd[0],dd[1],dd[2],    /* 12 ==> matrix+vector */
                                 dd[4],dd[5],dd[6],
                                 dd[8],dd[9],dd[10] ) ;
                  LOAD_DFVEC3(tvec,dd[3],dd[7],dd[11]) ;
         break ;

         case 9:  LOAD_DMAT(tmat,dd[0],dd[1],dd[2],    /*  9 ==> matrix only */
                                 dd[3],dd[4],dd[5],
                                 dd[6],dd[7],dd[8] ) ;
                  LOAD_DFVEC3(tvec,0,0,0) ;
         break ;
      }
   }

   /*-- invert the transformation we just read?            --*/
   /*-- [y] = [R][x] + [v]       is transformation, so     --*/
   /*-- [x] = inv[R] - inv[R][v] is inverse transformation --*/

   if( invert ){
      THD_dmat33 imat ; THD_dfvec3 ivec ;
      imat = DMAT_INV(tmat) ;             /* matrix inverse */
      ivec = DMATVEC(imat,tvec) ;         /* multiply inverse into vector */
      tmat = imat ;
      tvec = ivec ; NEGATE_DFVEC3(tvec) ; /* negate vector */
   }

   /*-- store results and get outta here, dude! --*/

   dvm.vv = tvec ; dvm.mm = tmat ; RETURN(dvm) ;
}
