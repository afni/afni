/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*--------------------------------------------------------------------*/
/*! Inline version of 3dinfo.
    - You must free() the output string when done with it.
    - verbose is -1 (shortest output), 0, or 1 (longest output)
----------------------------------------------------------------------*/

#include <stdarg.h>

#undef  ZMAX
#undef  SZMAX
#define ZMAX  8000
#define SZMAX "%.8000s"   /* same as ZMAX */

char * THD_zzprintf( char *sss , char *fmt , ... ) ;

char * THD_dataset_info( THD_3dim_dataset *dset , int verbose )
{
   THD_dataxes      *daxes ;
   THD_fvec3 fv1 , fv2 , fv3 ;
   THD_ivec3 iv ;
   int ival , ntimes , nval_per , n1,n2,n3 , kv,npar ;
   float tf ;

   static char *RR="[R]" , *LL="[L]" ,
               *PP="[P]" , *AA="[A]" ,
               *SS="[S]" , *II="[I]" , *ZZ="   " ;
   char *xlbot , *xltop , *ylbot , *yltop , *zlbot , *zltop , *cpt ;
   char str[256] ;
   int nstr ;

   char *outbuf = NULL ;

ENTRY("THD_dataset_info") ;

   if( ! ISVALID_3DIM_DATASET(dset) ) RETURN(NULL) ;

   daxes = dset->daxes ;

   if( DSET_IS_BRIK(dset) )
     outbuf = THD_zzprintf(outbuf,"Dataset File:    %s\n" , DSET_FILECODE(dset) ) ;
   else
     outbuf = THD_zzprintf(outbuf,"Dataset File:    %s\n" , DSET_BRIKNAME(dset) ) ;

   outbuf = THD_zzprintf(outbuf,"Identifier Code: %s  Creation Date: %s\n" ,
             dset->idcode.str , dset->idcode.date ) ;

   if( ISANAT(dset) ){
      outbuf = THD_zzprintf(outbuf,"Dataset Type:    %s (-%s)\n",
                ANAT_typestr[dset->func_type] , ANAT_prefixstr[dset->func_type] ) ;
   } else {
      outbuf = THD_zzprintf(outbuf,"Dataset Type:    %s (-%s)\n",
                FUNC_typestr[dset->func_type] , FUNC_prefixstr[dset->func_type] ) ;
   }

   /* 25 April 1998: do byte order stuff */

   switch( DSET_BYTEORDER(dset) ){
      case LSB_FIRST:
         outbuf = THD_zzprintf(outbuf,"Byte Order:      %s" , LSB_FIRST_STRING) ;
      break ;
      case MSB_FIRST:
         outbuf = THD_zzprintf(outbuf,"Byte Order:      %s" , MSB_FIRST_STRING) ;
      break ;
   }

   if( THD_find_string_atr(dset->dblk,ATRNAME_BYTEORDER) == NULL ) /* 19 Sep 1999 */
      outbuf = THD_zzprintf(outbuf," {assumed}") ;

   kv = mri_short_order() ;
   switch( kv ){
      case LSB_FIRST:
         outbuf = THD_zzprintf(outbuf," [this CPU native = %s]\n" , LSB_FIRST_STRING) ;
      break ;
      case MSB_FIRST:
         outbuf = THD_zzprintf(outbuf," [this CPU native = %s]\n" , MSB_FIRST_STRING) ;
      break ;
   }

   /*-- 21 Jun 2002: print storage mode --*/

   switch( dset->dblk->diskptr->storage_mode ){
     default: 
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    Undefined\n") ; break ;

     case STORAGE_BY_BRICK:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    BRIK file\n") ; break ;

     case STORAGE_BY_MINC:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    MINC file\n") ; break ;

     case STORAGE_BY_VOLUMES:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    Volume file(s)\n") ; break ;

     case STORAGE_BY_ANALYZE:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    ANALYZE files\n") ; break ;

     case STORAGE_BY_CTFMRI:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    CTF MRI file\n") ; break ;

     case STORAGE_BY_CTFSAM:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    CTF SAM file\n") ; break ;

     case STORAGE_BY_1D:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    AFNI .1D file\n") ; break ;

     case STORAGE_BY_3D:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    AFNI .3D file\n") ; break ;

     case STORAGE_BY_NIFTI:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    NIFTI file\n") ; break ;

     case STORAGE_BY_MPEG:
       outbuf = THD_zzprintf(outbuf,"Storage Mode:    MPEG file\n") ; break ;
   }

   /*-- keywords --*/

   if( verbose >= 0 ){
    cpt = DSET_KEYWORDS(dset) ;
    if( cpt != NULL && cpt[0] != '\0' )
       outbuf = THD_zzprintf(outbuf,"Keywords:        %s\n" , cpt ) ;
   }

   /*-- idcodes --*/

  if( verbose >= 0 ){
   if( ! ISZERO_IDCODE(dset->anat_parent_idcode) )
      outbuf = THD_zzprintf(outbuf,"Anatomy Parent:  %s [%s]\n" ,
                dset->anat_parent_name , dset->anat_parent_idcode.str ) ;
   else if( strlen(dset->anat_parent_name) > 0 )
      outbuf = THD_zzprintf(outbuf,"Anatomy Parent:  %s\n" , dset->anat_parent_name ) ;

   if( ! ISZERO_IDCODE(dset->warp_parent_idcode) )
      outbuf = THD_zzprintf(outbuf,"Warp Parent:     %s [%s]\n" ,
                 dset->warp_parent_name , dset->warp_parent_idcode.str) ;
   else if( strlen(dset->warp_parent_name) > 0 )
      outbuf = THD_zzprintf(outbuf,"Warp Parent:     %s\n" , dset->warp_parent_name ) ;
  }

   /*-- tagset --*/

   if( verbose > 0 && dset->tagset != NULL && dset->tagset->num > 0 ){
      int ii , ns=0 ;
      for( ii=0 ; ii < dset->tagset->num ; ii++ )
         if( dset->tagset->tag[ii].set ) ns++ ;

      outbuf = THD_zzprintf(outbuf,"Tagset:          %d set [out of %d total]\n",
                            ns , dset->tagset->num ) ;
   }

   outbuf = THD_zzprintf(outbuf,
      "Data Axes Orientation:\n"
      "  first  (x) = %s\n"
      "  second (y) = %s\n"
      "  third  (z) = %s   [-orient %c%c%c]\n" ,
    ORIENT_typestr[daxes->xxorient] ,
      ORIENT_typestr[daxes->yyorient] ,
      ORIENT_typestr[daxes->zzorient] ,
    ORIENT_typestr[daxes->xxorient][0] ,
      ORIENT_typestr[daxes->yyorient][0] ,
      ORIENT_typestr[daxes->zzorient][0]  ) ;

   LOAD_FVEC3(fv1 , daxes->xxorg , daxes->yyorg , daxes->zzorg) ;
   fv1 = THD_3dmm_to_dicomm( dset , fv1 ) ;

   LOAD_FVEC3(fv2 , daxes->xxorg + (daxes->nxx-1)*daxes->xxdel ,
                    daxes->yyorg + (daxes->nyy-1)*daxes->yydel ,
                    daxes->zzorg + (daxes->nzz-1)*daxes->zzdel  ) ;
   fv2 = THD_3dmm_to_dicomm( dset , fv2 ) ;

   if( fv1.xyz[0] > fv2.xyz[0] ) FSWAP( fv1.xyz[0] , fv2.xyz[0] ) ;
   if( fv1.xyz[1] > fv2.xyz[1] ) FSWAP( fv1.xyz[1] , fv2.xyz[1] ) ;
   if( fv1.xyz[2] > fv2.xyz[2] ) FSWAP( fv1.xyz[2] , fv2.xyz[2] ) ;

   LOAD_FVEC3(fv3 , daxes->xxdel , daxes->yydel , daxes->zzdel) ;
   fv3 = THD_3dmm_to_dicomm( dset , fv3 ) ;

   XLAB(xlbot,fv1.xyz[0]) ; YLAB(ylbot,fv1.xyz[1]) ; ZLAB(zlbot,fv1.xyz[2]) ;
   XLAB(xltop,fv2.xyz[0]) ; YLAB(yltop,fv2.xyz[1]) ; ZLAB(zltop,fv2.xyz[2]) ;

   n1 = DAXES_NUM(daxes,ORI_R2L_TYPE) ;
   n2 = DAXES_NUM(daxes,ORI_A2P_TYPE) ;
   n3 = DAXES_NUM(daxes,ORI_I2S_TYPE) ;

   outbuf = THD_zzprintf(outbuf,
      "R-to-L extent: %9.3f %s -to- %9.3f %s -step- %9.3f mm [%3d voxels]\n"
      "A-to-P extent: %9.3f %s -to- %9.3f %s -step- %9.3f mm [%3d voxels]\n"
      "I-to-S extent: %9.3f %s -to- %9.3f %s -step- %9.3f mm [%3d voxels]\n" ,
    fv1.xyz[0],xlbot , fv2.xyz[0],xltop , fabs(fv3.xyz[0]) , n1 ,
    fv1.xyz[1],ylbot , fv2.xyz[1],yltop , fabs(fv3.xyz[1]) , n2 ,
    fv1.xyz[2],zlbot , fv2.xyz[2],zltop , fabs(fv3.xyz[2]) , n3  ) ;

   /*-- 01 Feb 2001: print the center of the dataset as well --*/

   if( verbose > 0 ){
    fv1.xyz[0] = 0.5*(fv1.xyz[0]+fv2.xyz[0]) ; XLAB(xlbot,fv1.xyz[0]) ;
    fv1.xyz[1] = 0.5*(fv1.xyz[1]+fv2.xyz[1]) ; YLAB(ylbot,fv1.xyz[1]) ;
    fv1.xyz[2] = 0.5*(fv1.xyz[2]+fv2.xyz[2]) ; ZLAB(zlbot,fv1.xyz[2]) ;

    outbuf = THD_zzprintf(outbuf,
                            "R-to-L center: %9.3f %s\n"
                            "A-to-P center: %9.3f %s\n"
                            "I-to-S center: %9.3f %s\n" ,
                          fv1.xyz[0],xlbot ,
                          fv1.xyz[1],ylbot ,
                          fv1.xyz[2],zlbot  ) ;
   }

   ntimes   = DSET_NUM_TIMES(dset) ;
   nval_per = DSET_NVALS_PER_TIME(dset) ;
   if( ntimes > 1 ){
      outbuf = THD_zzprintf(outbuf,
         "Number of time steps = %d  Number of values at each pixel = %d\n",
         ntimes , nval_per ) ;

      outbuf = THD_zzprintf(outbuf, "Time step = %.3f%s  Origin = %.3f%s" ,
                 dset->taxis->ttdel ,
                 UNITS_TYPE_LABEL(dset->taxis->units_type) ,
                 dset->taxis->ttorg ,
                 UNITS_TYPE_LABEL(dset->taxis->units_type)  ) ;
      if( dset->taxis->nsl > 0 )
        outbuf = THD_zzprintf(outbuf,"  Number time-offset slices = %d  Thickness = %.3f",
                  dset->taxis->nsl , fabs(dset->taxis->dz_sl) ) ;
      outbuf = THD_zzprintf(outbuf,"\n") ;

      if( verbose > 0 && dset->taxis->nsl > 0 ){
         outbuf = THD_zzprintf(outbuf,"Time-offsets per slice:") ;
         for( ival=0 ; ival < dset->taxis->nsl ; ival++ )
           outbuf = THD_zzprintf(outbuf, " %.3f" , dset->taxis->toff_sl[ival] ) ;
         outbuf = THD_zzprintf(outbuf,"\n") ;
      }
   } else {
      outbuf = THD_zzprintf(outbuf,
           "Number of values stored at each pixel = %d\n" , nval_per ) ;
   }

#if 0
   if( verbose > 0 && ntimes > 1 ) nval_per = dset->dblk->nvals ;
   else                            nval_per = 1 ;                 /* 12 Feb 2002 */
#else
   nval_per = dset->dblk->nvals ;
   if( verbose < 0 ) nval_per = 1 ;                               /* 27 Mar 2002 */
#endif

   /* print out stuff for each sub-brick */

   for( ival=0 ; ival < nval_per ; ival++ ){

      sprintf( str ,
               "  -- At sub-brick #%d '%s' datum type is %s" ,
               ival , DSET_BRICK_LAB(dset,ival) ,
               MRI_TYPE_name[DSET_BRICK_TYPE(dset,ival)] ) ;
      nstr = strlen(str) ;

      tf = DSET_BRICK_FACTOR(dset,ival) ;

      if( ISVALID_STATISTIC(dset->stats) ){

         if( tf != 0.0 ){
            sprintf( str+nstr ,
                                ":%13.6g to %13.6g [internal]\n"
                    "%*s[*%13.6g] %13.6g to %13.6g [scaled]\n" ,
                    dset->stats->bstat[ival].min/tf ,
                    dset->stats->bstat[ival].max/tf ,
                    nstr-16," " , tf ,
                    dset->stats->bstat[ival].min , dset->stats->bstat[ival].max ) ;
          } else {
            sprintf( str+nstr , ":%13.6g to %13.6g\n" ,
                    dset->stats->bstat[ival].min , dset->stats->bstat[ival].max ) ;
          }
      } else if( tf != 0.0 ){
         sprintf( str+nstr , " [*%g]\n",tf) ;
      } else {
         sprintf( str+nstr , "\n") ;
      }
      outbuf = THD_zzprintf(outbuf,"%s",str) ;

      /** 30 Nov 1997: print sub-brick stat params **/

      kv = DSET_BRICK_STATCODE(dset,ival) ;
      if( FUNC_IS_STAT(kv) ){
         outbuf = THD_zzprintf(outbuf,"     statcode = %s",FUNC_prefixstr[kv] ) ;
         npar = FUNC_need_stat_aux[kv] ;
         if( npar > 0 ){
            outbuf = THD_zzprintf(outbuf,";  statpar =") ;
            for( kv=0 ; kv < npar ; kv++ )
               outbuf = THD_zzprintf(outbuf," %g",DSET_BRICK_STATPAR(dset,ival,kv)) ;
         }
         outbuf = THD_zzprintf(outbuf,"\n") ;
      }

      cpt = DSET_BRICK_KEYWORDS(dset,ival) ;
      if( cpt != NULL && cpt[0] != '\0' )
         outbuf = THD_zzprintf(outbuf,"     keywords = %s\n",cpt) ;
   }

   /** print out dataset global statistical parameters **/

   if( ISFUNC(dset) && FUNC_need_stat_aux[dset->func_type] > 0 ){
      outbuf = THD_zzprintf(outbuf,"Auxiliary functional statistical parameters:\n %s\n",
             FUNC_label_stat_aux[dset->func_type] ) ;
      for( ival=0 ; ival < FUNC_need_stat_aux[dset->func_type] ; ival++ )
         outbuf = THD_zzprintf(outbuf," %g",dset->stat_aux[ival]) ;
      outbuf = THD_zzprintf(outbuf,"\n") ;
   }

   /** If present, print out History **/

   { char *chn ; int j,k ;
     chn = tross_Get_History(dset) ;
     if( chn != NULL ){
       j = strlen(chn) ;
       outbuf = THD_zzprintf(outbuf,"\n----- HISTORY -----\n") ;
       for( k=0 ; k < j ; k += ZMAX )
         outbuf = THD_zzprintf(outbuf,SZMAX,chn+k) ;
       free(chn) ;
       outbuf = THD_zzprintf(outbuf,"\n") ;
     }
   }

   /** If present, print out Notes **/

   if( verbose >= 0 ){
     ATR_int *notecount;
     ATR_string *note;
     int num_notes, i, j, num_char , mmm ;
     char note_name[20], *chn , *chd ;

     notecount = THD_find_int_atr(dset->dblk, "NOTES_COUNT");
     if( notecount != NULL ){
        num_notes = notecount->in[0] ;
        if( verbose == 0 && num_notes > 5 ) num_notes = 5 ;
        mmm = (verbose > 0) ? ZMAX : 1200 ;         /* 400 it was! Come on Bob, have a heart! -ZSS */
        for (i=1; i<= num_notes; i++) {
           chn = tross_Get_Note( dset , i ) ;
           if( chn != NULL ){
              j = strlen(chn) ; if( j > mmm ) chn[mmm] = '\0' ;
              chd = tross_Get_Notedate(dset,i) ;
              if( chd == NULL ){ chd = AFMALL(char,16) ; strcpy(chd,"no date") ; }
              outbuf = THD_zzprintf(outbuf,"\n----- NOTE %d [%s] -----\n%s\n",i,chd,chn) ;
              free(chn) ; free(chd) ;
           }
        }
     }
   }

   RETURN(outbuf) ;
}

/*-----------------------------------------------------------*/

char * THD_zzprintf( char *sss , char *fmt , ... )
{
   static char *sbuf = NULL ;
   char *zz ;
   int   nzz , nsbuf ;
   va_list vararg_ptr ;

   va_start( vararg_ptr , fmt ) ;

   if( sbuf == NULL ) sbuf = AFMALL(char, ZMAX+90) ;

   sbuf[0] = '\0' ;
   vsprintf( sbuf , fmt , vararg_ptr ) ;
   nsbuf = strlen(sbuf) ;
   if( nsbuf == 0 ) return sss ;

   if( sss == NULL ){
      zz = (char *) malloc( sizeof(char)*(nsbuf+2) ) ;
      strcpy(zz,sbuf) ;
   } else {
      nzz = strlen(sss) + nsbuf + 2 ;
      zz  = (char *) malloc( sizeof(char) * nzz ) ;
      strcpy(zz,sss) ; strcat(zz,sbuf) ;
      free(sss) ;
   }
   return zz ;
}
