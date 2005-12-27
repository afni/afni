#include "mrilib.h"

/*-------------------------------------------------------------------
  08 Feb 2001: read a matrix+vector from a file - RWCox
  14 Feb 2001: modified to add "-rotate ..." mode
  10 Aug 2005: add MATRIX(...)
---------------------------------------------------------------------*/

THD_dvecmat THD_read_dvecmat( char *fname , int invert )
{
   THD_dvecmat dvm ;
   THD_dmat33 tmat ;
   THD_dfvec3 tvec ;
   FILE *fp ;
   double dd[12] ;
   int  nn , loaded=0 ;

ENTRY("THD_read_dvecmat") ;

   LOAD_DIAG_DMAT(tmat,0.0,0.0,0.0) ;  /* initialize to all 0s */
   LOAD_DFVEC3(tvec,0.0,0.0,0.0) ;

   if( fname == NULL || fname[0] == '\0' ){
     dvm.vv = tvec ; dvm.mm = tmat ; RETURN(dvm) ;
   }

   /*-- filename is "dataset::attribute" --*/

   if( strstr(fname,"::") != NULL ){  /*== read from dataset header ==*/
     char *dname = strdup(fname) ;
     char *cc = strstr(dname,"::") ;
     char *ss ; int iss=0 ;
     THD_3dim_dataset *dset ;
     ATR_float *atr, *atrc ;
     int incl;
     THD_dfvec3 tvec_co, tvec_cb;

     *cc = '\0' ; cc += 2 ;  /* dname = dataset name ; cc = attribute name */

     dset = THD_open_one_dataset( dname ) ;
     if( !ISVALID_DSET(dset) ){
       ERROR_message("THD_read_dvecmat: can't open dataset %s\n",dname) ;
       dvm.vv = tvec ; dvm.mm = tmat ; free(dname) ; RETURN(dvm) ;
     }

     ss = strstr(cc,"[") ;
     if( ss != NULL ){
       *ss = '\0'; ss++; iss = (int)strtol(ss,NULL,10); if( iss < 0 ) iss = 0;
     }
     atr = THD_find_float_atr( dset->dblk , cc ) ;
     if( atr == NULL ){
       ERROR_message("THD_read_dvecmat: can't find attribute %s in dataset %s\n",
                     cc,dname) ;
       dvm.vv = tvec ; dvm.mm = tmat ; free(dname) ; RETURN(dvm) ;
     }

     if( strcmp(cc,"WARP_DATA") == 0 ){  /* 10 Aug 2005 */

       LOAD_DMAT(tmat,atr->fl[0],atr->fl[1],atr->fl[2],
                      atr->fl[3],atr->fl[4],atr->fl[5],
                      atr->fl[6],atr->fl[7],atr->fl[8] ) ;
       LOAD_DFVEC3(tvec,-atr->fl[18],-atr->fl[19],-atr->fl[20]) ;
       loaded = 1 ;
    }

    if( !loaded ){
      switch( atr->nfl ){

        default:
          ERROR_message(
                  "THD_read_dvecmat: can't read matrix+vector from %s::%s\n",
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
    }

   {
      /* ZSS: Dec 27, the year of our Lord when the War on Christmas was raging */
      /* Need to include VOLREG_CENTER_OLD, VOLREG_CENTER_BASE, 
                         ROTATE_CENTER_OLD, ROTATE_CENTER_BASE, if applicable. */
      /* Do we have a ROTATE or VOLREG attribute ?*/
      incl = 0;
      if (strstr(cc,"VOLREG_MATVEC")) {
         atrc = THD_find_float_atr( dset->dblk , "VOLREG_CENTER_OLD");
         if ( atrc ) {
            LOAD_DFVEC3(tvec_co,atrc->fl[0],atrc->fl[1],atrc->fl[2]) ;
            incl = 1;
         } else {
            LOAD_DFVEC3(tvec_co,0,0,0) ;
         }
         
         atrc = THD_find_float_atr( dset->dblk , "VOLREG_CENTER_BASE");
         if ( atrc ) {
            LOAD_DFVEC3(tvec_cb,atrc->fl[0],atrc->fl[1],atrc->fl[2]) ;
            incl = 1;
         } else {
            LOAD_DFVEC3(tvec_cb,0,0,0) ;
         }
         
         if (incl == 1) INFO_message("THD_read_dvecmat:\n"
                                     "   Including VOLREG_CENTER_BASE and VOLREG_CENTER_OLD\n"
                                     "   attributes in final transform\n");
         else INFO_message("THD_read_dvecmat:\n"
                                     "   No VOLREG_CENTER_BASE or VOLREG_CENTER_OLD\n"
                                     "   attributes found with VOLREG_MATVEC\n");
      } else if (strstr(cc,"ROTATE_MATVEC")) {
         atrc = THD_find_float_atr( dset->dblk , "ROTATE_CENTER_OLD");
         if ( atrc ) {
            LOAD_DFVEC3(tvec_co,atrc->fl[0],atrc->fl[1],atrc->fl[2]) ;
            incl = 1;
         } else {
            LOAD_DFVEC3(tvec_co,0,0,0) ;
         }
         
         atrc = THD_find_float_atr( dset->dblk , "ROTATE_CENTER_BASE");
         if ( atrc ) {
            LOAD_DFVEC3(tvec_cb,atrc->fl[0],atrc->fl[1],atrc->fl[2]) ;
            incl = 1;
         } else {
            LOAD_DFVEC3(tvec_cb,0,0,0) ;
         }
                  
         if (incl == 1) INFO_message("THD_read_dvecmat:\n"
                                     "   Including ROTATE_CENTER_BASE and ROTATE_CENTER_OLD\n"
                                     "   attributes in final transform\n");
         else INFO_message("THD_read_dvecmat:\n"
                                     "   No ROTATE_CENTER_BASE or ROTATE_CENTER_OLD\n"
                                     "   attributes found with ROTATE_MATVEC\n");
      }
      if (incl == 1) {
         tvec_co = DMATVEC(tmat, tvec_co);
         NEGATE_DFVEC3(tvec_co);
         tvec.xyz[0] += tvec_cb.xyz[0] + tvec_co.xyz[0];
         tvec.xyz[1] += tvec_cb.xyz[1] + tvec_co.xyz[1];
         tvec.xyz[2] += tvec_cb.xyz[2] + tvec_co.xyz[2];
      }
   }
    free(dname) ; DSET_delete(dset) ;

   /*-- 14 Feb 2001: filename is "-rotate a b c -[ab]shift x y z" string --*/

   } else if( strstr(fname,"-rotate") != NULL ){  /*== compute directly ==*/

      dvm = THD_rotcom_to_matvec( NULL , fname ) ;  /* thd_rotangles.c */
      tvec = dvm.vv ; tmat = dvm.mm ;

   /*-- MATRIX(...) --*/

   } else if( strncmp(fname,"MATRIX(",7) == 0 ){
     float matar[12] ; int nn ;
     nn = sscanf(fname,"MATRIX(%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f",
                       matar+0 , matar+1 , matar+2 , matar+3 ,
                       matar+4 , matar+5 , matar+6 , matar+7 ,
                       matar+8 , matar+9 , matar+10, matar+11 ) ;
     if( nn < 12 ){
       ERROR_message("badly formatted: %s",fname) ;
     } else {
       LOAD_DMAT(tmat,matar[0],matar[1],matar[2],
                      matar[4],matar[5],matar[6],
                      matar[8],matar[9],matar[10] ) ;
       LOAD_DFVEC3(tvec,matar[3],matar[7],matar[11]) ;
     }

   /*-- just a normal filename --*/

   } else {  /*== read numbers from file ==*/

      fp = fopen( fname , "r" ) ;
      if( fp == NULL ){
         ERROR_message(
                 "THD_read_dvecmat: can't open file %s\n",
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
            ERROR_message(
                    "THD_read_dvecmat: can't read matrix+vector from file %s\n",
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
