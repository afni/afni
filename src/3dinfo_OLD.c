/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1994-1996 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application.  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#include "mrilib.h"

void Syntax(void)
{
   printf(
    "Copyright 1994-6 Medical College of Wisconsin\n\n"
    "Prints out sort-of-useful information from a 3D dataset's header\n"
    "Usage: 3dinfo [-v] dataset [dataset ...]\n"
    "  The -v option means print out verbose information.  At present,\n"
    "  it just causes the printing of all the statistics for each time\n"
    "  in a time-dependent dataset.\n"
   ) ;
   exit(0) ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   THD_dataxes      * daxes ;
   THD_fvec3 fv1 , fv2 , fv3 ;
   THD_ivec3 iv ;
   int iarg , ival , ntimes , nval_per , n1,n2,n3 , verbose = 0 , kv,npar ;
   float tf ;

   static char * RR="[R]" , * LL="[L]" ,
               * PP="[P]" , * AA="[A]" ,
               * SS="[S]" , * II="[I]" , * ZZ="   " ;
   char * xlbot , * xltop , * ylbot , * yltop , * zlbot , * zltop , * cpt ;
   char str[256] ;
   int nstr ;

#define XLAB(xl,xv) ((xl) = ((xv)==0.0) ? (ZZ) : ( ((xv)<0.0) ? (RR) : (LL) ))
#define YLAB(yl,yv) ((yl) = ((yv)==0.0) ? (ZZ) : ( ((yv)<0.0) ? (AA) : (PP) ))
#define ZLAB(zl,zv) ((zl) = ((zv)==0.0) ? (ZZ) : ( ((zv)<0.0) ? (II) : (SS) ))

#define FSWAP(x,y) (tf=(x),(x)=(y),(y)=tf)

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   iarg = 1 ;

   if( strcmp(argv[iarg],"-v") == 0 ){ verbose = 1 ; iarg++ ; }

   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_one_dataset( argv[iarg] ) ;
      if( dset == NULL ) continue ;
      daxes = dset->daxes ;

      printf("\n"
         "Dataset File:    %s\n"
#if 0
         "Dataset Name:    %s\n"
         "Dataset Label:   %s\n"
#endif
       , argv[iarg]
#if 0
       , dset->self_name , dset->label1
#endif
      ) ;

#ifndef OMIT_DATASET_IDCODES
       printf("Identifier Code: %s  Creation Date: %s\n" ,
              dset->idcode.str , dset->idcode.date ) ;
#endif

      if( ISANAT(dset) ){
         printf("Dataset Type:    %s (-%s)\n",
                ANAT_typestr[dset->func_type] , ANAT_prefixstr[dset->func_type] ) ;
      } else {
         printf("Dataset Type:    %s (-%s)\n",
                FUNC_typestr[dset->func_type] , FUNC_prefixstr[dset->func_type] ) ;
      }

      cpt = DSET_KEYWORDS(dset) ;
      if( cpt != NULL && cpt[0] != '\0' )
         printf("Keywords:        %.99s\n" , cpt ) ;

#ifdef OMIT_DATASET_IDCODES
      if( strlen(dset->anat_parent_name) > 0 )
         printf("Anatomy Parent:  %s\n" , dset->anat_parent_name ) ;

      if( strlen(dset->warp_parent_name) > 0 )
         printf("Warp Parent:     %s\n" , dset->warp_parent_name ) ;
#else
      if( ! ISZERO_IDCODE(dset->anat_parent_idcode) )
         printf("Anatomy Parent:  %s [%s]\n" ,
                dset->anat_parent_name , dset->anat_parent_idcode.str ) ;
      else if( strlen(dset->anat_parent_name) > 0 )
         printf("Anatomy Parent:  %s\n" , dset->anat_parent_name ) ;

      if( ! ISZERO_IDCODE(dset->warp_parent_idcode) )
         printf("Warp Parent:     %s [%s]\n" ,
                 dset->warp_parent_name , dset->warp_parent_idcode.str) ;
      else if( strlen(dset->warp_parent_name) > 0 )
         printf("Warp Parent:     %s\n" , dset->warp_parent_name ) ;
#endif

      printf(
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

      n1 = DAXES_NUM(daxes,daxes->xxorient) ;
      n2 = DAXES_NUM(daxes,daxes->yyorient) ;
      n3 = DAXES_NUM(daxes,daxes->zzorient) ;

      printf(
         "R-to-L extent: %9.3f %s -to- %9.3f %s -step- %9.3f mm [%3d voxels]\n"
         "A-to-P extent: %9.3f %s -to- %9.3f %s -step- %9.3f mm [%3d voxels]\n"
         "I-to-S extent: %9.3f %s -to- %9.3f %s -step- %9.3f mm [%3d voxels]\n" ,
       fv1.xyz[0],xlbot , fv2.xyz[0],xltop , fabs(fv3.xyz[0]) , n1 ,
       fv1.xyz[1],ylbot , fv2.xyz[1],yltop , fabs(fv3.xyz[1]) , n2 ,
       fv1.xyz[2],zlbot , fv2.xyz[2],zltop , fabs(fv3.xyz[2]) , n3  ) ;

      ntimes   = DSET_NUM_TIMES(dset) ;
      nval_per = DSET_NVALS_PER_TIME(dset) ;
      if( ntimes > 1 ){
         printf(
            "Number of time steps = %d  Number of values at each pixel = %d\n",
            ntimes , nval_per ) ;

         printf( "Time step = %.3f (%s)" ,
                 dset->taxis->ttdel ,
                 UNITS_TYPE_LABEL(dset->taxis->units_type) ) ;
         if( dset->taxis->nsl > 0 )
           printf("  Number time-offset slices = %d  Thickness = %.3f",
                  dset->taxis->nsl , fabs(dset->taxis->dz_sl) ) ;
         printf("\n") ;

         if( verbose && dset->taxis->nsl > 0 ){
            printf("Time-offsets per slice:") ;
            for( ival=0 ; ival < dset->taxis->nsl ; ival++ )
              printf( " %.3f" , dset->taxis->toff_sl[ival] ) ;
            printf("\n") ;
         }
      } else {
         printf(
           "Number of values stored at each pixel = %d\n" , nval_per ) ;
      }

      if( verbose && ntimes > 1 ) nval_per = dset->dblk->nvals ;

      for( ival=0 ; ival < nval_per ; ival++ ){
         sprintf( str ,
                  "  -- At sub-brick #%d [%s] datum type is %s" ,
                  ival , dset->dblk->brick_lab[ival] ,
                  MRI_TYPE_name[DSET_BRICK_TYPE(dset,ival)] ) ;
         nstr = strlen(str) ;

         tf = DSET_BRICK_FACTOR(dset,ival) ;

         if( ISVALID_STATISTIC(dset->stats) ){
            if( tf != 0.0 )
               sprintf( str+nstr ,
                                   ":%13.6g to %13.6g [internal]\n"
                       "%*s[*%13.6g] %13.6g to %13.6g [scaled]\n" ,
                       dset->stats->bstat[ival].min/tf ,
                       dset->stats->bstat[ival].max/tf ,
                       nstr-16," " , tf ,
                       dset->stats->bstat[ival].min , dset->stats->bstat[ival].max ) ;
             else
               sprintf( str+nstr , ":%13.6g to %13.6g\n" ,
                       dset->stats->bstat[ival].min , dset->stats->bstat[ival].max ) ;
         } else if( tf != 0.0 ){
            sprintf( str+nstr , " [*%g]\n",tf) ;
         } else {
            sprintf( str+nstr , "\n") ;
         }
         printf(str) ;

         /** 30 Nov 1997: print sub-brick stat params **/

         kv = DSET_BRICK_STATCODE(dset,ival) ;
         if( FUNC_IS_STAT(kv) ){
            printf("     statcode = %s",FUNC_prefixstr[kv] ) ;
            npar = FUNC_need_stat_aux[kv] ;
            if( npar > 0 ){
               printf(";  statpar =") ;
               for( kv=0 ; kv < npar ; kv++ )
                  printf(" %g",DSET_BRICK_STATPAR(dset,ival,kv)) ;
            }
            printf("\n") ;
         }

         cpt = DSET_BRICK_KEYWORDS(dset,ival) ;
         if( cpt != NULL && cpt[0] != '\0' )
            printf("     keywords = %.66s\n",cpt) ;
      }

      /** print out dataset global statistical parameters **/

      if( ISFUNC(dset) && FUNC_need_stat_aux[dset->func_type] > 0 ){
         printf("Auxiliary functional statistical parameters:\n %s\n",
                FUNC_label_stat_aux[dset->func_type] ) ;
         for( ival=0 ; ival < FUNC_need_stat_aux[dset->func_type] ; ival++ )
            printf(" %g",dset->stat_aux[ival]) ;
         printf("\n") ;
      }

      THD_delete_3dim_dataset( dset , False ) ;
   }
   exit(0) ;
}
