#include "mrilib.h"
#include "thd.h"

#undef  IFILL_DIM
#undef  FFILL_DIM
#undef  IFILL
#undef  FFILL
#undef  ITFILL
#undef  FTFILL

#define IFILL_DIM 100
#define FFILL_DIM (MAPPING_LINEAR_FSIZE*20)

#define IFILL -999
#define FFILL -999999.0

#define ITFILL(p,q) for(ii=p;ii<q;ii++) itemp[ii] = IFILL
#define FTFILL(p,q) for(ii=p;ii<q;ii++) ftemp[ii] = FFILL

/*-------------------------------------------------------------------*/
/*! Set attributes from the dataset to the datablock,
   preparing for output to someplace or other.
   Adapted from elements of the earlier editions of
   THD_write_3dim_dataset() and THD_write_datablock() -- 09 Mar 2005.
---------------------------------------------------------------------*/

void THD_set_dataset_attributes( THD_3dim_dataset *dset )
{
   THD_datablock *blk ;
   THD_dataxes   *daxes ;
   THD_diskptr   *dkptr ;

   int itemp[IFILL_DIM] , ii ;
   float ftemp[FFILL_DIM] ;

   int id , nx , ny , nz , nv , nxy , nxyz , ibr , nb ;
   int atrank[ATRSIZE_DATASET_RANK] , atdims[ATRSIZE_DATASET_DIMENSIONS] ;
   MRI_IMAGE *im ;
   int save_order ;

ENTRY("THD_set_dataset_attributes") ;

   /*-- sanity checks --*/

   if( ! ISVALID_3DIM_DATASET(dset)    ||
       ! ISVALID_DATABLOCK(dset->dblk) ||
       ! ISVALID_DISKPTR(dset->dblk->diskptr) ) EXRETURN ;

   blk = dset->dblk ; daxes = dset->daxes ;  /* always used fixed daxes */
   dkptr = blk->diskptr ;

   /******/
   /****** These attributes used to be set in THD_write_3dim_dataset() *****/
   /******/

   /*----- write TYPESTRING attribute -----*/

   THD_set_string_atr( blk , ATRNAME_TYPESTRING ,
                       DATASET_typestr[dset->type] ) ;

   /*----- write IDCODE attributes -----*/

   THD_set_string_atr( blk , ATRNAME_IDSTRING , dset->idcode.str ) ;
   THD_set_string_atr( blk , ATRNAME_IDDATE   , dset->idcode.date ) ;

   if( ! ISZERO_IDCODE(dset->anat_parent_idcode) )
     THD_set_string_atr( blk, ATRNAME_IDANATPAR, dset->anat_parent_idcode.str );
   else
     THD_erase_one_atr ( blk, ATRNAME_IDANATPAR ) ;

   if( ! ISZERO_IDCODE(dset->warp_parent_idcode) )
     THD_set_string_atr( blk, ATRNAME_IDWARPPAR, dset->warp_parent_idcode.str );
   else
     THD_erase_one_atr ( blk, ATRNAME_IDWARPPAR ) ;

   /*----- write SCENE_TYPE attribute -----*/

   itemp[0] = dset->view_type ;
   itemp[1] = dset->func_type ;
   itemp[2] = dset->type ;

   ITFILL(3,ATRSIZE_SCENE_TYPE) ;
   THD_set_int_atr( blk , ATRNAME_SCENE_TYPE ,
                          ATRSIZE_SCENE_TYPE , itemp ) ;

   /*----- write data labels -----*/

   if( strlen(dset->self_name) == 0 ) DSET_FIX_NAMES(dset) ;

   THD_set_string_atr( blk , ATRNAME_LABEL1   , dset->label1 ) ;
   THD_set_string_atr( blk , ATRNAME_LABEL2   , dset->label2 ) ;
   THD_set_string_atr( blk , ATRNAME_DATANAME , dset->self_name ) ;

   if( dset->keywords != NULL )
      THD_set_string_atr( blk , ATRNAME_KEYWORDS , dset->keywords ) ;
   else
      THD_erase_one_atr ( blk , ATRNAME_KEYWORDS ) ;

   /*----- write parent names, if they exist -----*/

   if( strlen(dset->warp_parent_name) > 0 )
      THD_set_string_atr( blk , ATRNAME_WARP_PARENT ,
                          dset->warp_parent_name ) ;
   else
      THD_erase_one_atr ( blk , ATRNAME_WARP_PARENT ) ;

   if( strlen(dset->anat_parent_name) > 0 )
      THD_set_string_atr( blk , ATRNAME_ANATOMY_PARENT ,
                          dset->anat_parent_name ) ;
   else
      THD_erase_one_atr ( blk , ATRNAME_ANATOMY_PARENT ) ;

   /*----- write axes orientation -----*/

   itemp[0] = daxes->xxorient ;
   itemp[1] = daxes->yyorient ;
   itemp[2] = daxes->zzorient ;

   ITFILL(3,ATRSIZE_ORIENT_SPECIFIC) ;
   THD_set_int_atr( blk , ATRNAME_ORIENT_SPECIFIC ,
                          ATRSIZE_ORIENT_SPECIFIC , itemp ) ;

   /*----- write axes origin -----*/

   ftemp[0] = daxes->xxorg ;
   ftemp[1] = daxes->yyorg ;
   ftemp[2] = daxes->zzorg ;

   FTFILL(3,ATRSIZE_ORIGIN) ;
   THD_set_float_atr( blk , ATRNAME_ORIGIN ,
                            ATRSIZE_ORIGIN , ftemp ) ;

   /*----- write axes spacings -----*/

   ftemp[0] = daxes->xxdel ;
   ftemp[1] = daxes->yydel ;
   ftemp[2] = daxes->zzdel ;

   FTFILL(3,ATRSIZE_DELTA) ;
   THD_set_float_atr( blk , ATRNAME_DELTA ,
                            ATRSIZE_DELTA , ftemp ) ;

   /*----- write markers, if present -----*/

   if( dset->markers != NULL ){

      for( ii=0 ; ii < MARKS_MAXNUM ; ii++ ){   /* put bad data in */
         if( !dset->markers->valid[ii] )        /* invalid markers */
            dset->markers->xyz[ii][0] =
            dset->markers->xyz[ii][1] =
            dset->markers->xyz[ii][2] = FFILL ;
      }

      THD_set_float_atr( blk , ATRNAME_MARKSXYZ ,
                               ATRSIZE_MARKSXYZ ,
                               &(dset->markers->xyz[0][0]) ) ;

      THD_set_char_atr( blk , ATRNAME_MARKSLAB ,
                              ATRSIZE_MARKSLAB ,
                              &(dset->markers->label[0][0]) ) ;

      THD_set_char_atr( blk , ATRNAME_MARKSHELP ,
                              ATRSIZE_MARKSHELP ,
                              &(dset->markers->help[0][0]) ) ;

      THD_set_int_atr( blk , ATRNAME_MARKSFLAG ,
                             ATRSIZE_MARKSFLAG ,
                             &(dset->markers->aflags[0]) ) ;
   } else {
      THD_erase_one_atr( blk , ATRNAME_MARKSXYZ  ) ;
      THD_erase_one_atr( blk , ATRNAME_MARKSLAB  ) ;
      THD_erase_one_atr( blk , ATRNAME_MARKSHELP ) ;
      THD_erase_one_atr( blk , ATRNAME_MARKSFLAG ) ;
   }

   /*----- write warp, if present -----*/

   if( dset->warp != NULL ){
      int wdata_size = 0 ;

      switch( dset->warp->type ){

         case WARP_AFFINE_TYPE:{
            THD_affine_warp *ww = (THD_affine_warp *) dset->warp ;
            itemp[0] = WARP_AFFINE_TYPE ;
            itemp[1] = ww->resam_type ;

            wdata_size = MAPPING_LINEAR_FSIZE ;
            COPY_FROM_STRUCT( ww->warp ,
                              MAPPING_LINEAR_FSTART ,
                              float ,
                              ftemp ,
                              MAPPING_LINEAR_FSIZE ) ;
         }
         break ;

         case WARP_TALAIRACH_12_TYPE:{
            THD_talairach_12_warp *ww =
               (THD_talairach_12_warp *) dset->warp ;
            int iw , ioff ;

            itemp[0] = WARP_TALAIRACH_12_TYPE ;
            itemp[1] = ww->resam_type ;

            wdata_size = WARP_TALAIRACH_12_SIZE ;

            for( iw=0 ; iw < 12 ; iw++ ){
               ioff = iw * MAPPING_LINEAR_FSIZE ;

               COPY_FROM_STRUCT( ww->warp[iw] ,
                                 MAPPING_LINEAR_FSTART ,
                                 float ,
                                 &(ftemp[ioff]) ,
                                 MAPPING_LINEAR_FSIZE ) ;
            }
         }
         break ;
      } /* end of switch on warp type */

      ITFILL(2,ATRSIZE_WARP_TYPE) ;
      THD_set_int_atr( blk , ATRNAME_WARP_TYPE ,
                             ATRSIZE_WARP_TYPE , itemp ) ;

      THD_set_float_atr( blk , ATRNAME_WARP_DATA ,
                               wdata_size , ftemp ) ;

   } else {  /* no warp exists */

      THD_erase_one_atr( blk , ATRNAME_WARP_TYPE ) ;
      THD_erase_one_atr( blk , ATRNAME_WARP_DATA ) ;

   } /* end of if warp exists */

   /*----- if statistics exist, write them out (modern style of floats) -----*/

   if( ISVALID_STATISTIC(dset->stats) ){
      float *tfil ;
      int qq ;

      tfil = (float *) malloc( sizeof(float) * 2*blk->nvals ) ;
      if( tfil != NULL ){
         for( qq=0 ; qq < dset->stats->nbstat ; qq++ ){
            tfil[2*qq]   = dset->stats->bstat[qq].min ;
            tfil[2*qq+1] = dset->stats->bstat[qq].max ;
         }
         THD_set_float_atr( blk, ATRNAME_BRICK_STATS,
                            2*dset->stats->nbstat, tfil ) ;
         free( tfil ) ;
      }
   } else {
      THD_erase_one_atr( blk , ATRNAME_BRICK_STATS ) ;
   }

   /*----- if auxiliary statistics data exists, write them out too -----*/

   for( ii=MAX_STAT_AUX-1; ii>=0 && dset->stat_aux[ii]==0.0; ii-- ) ; /* nada */

   if( ii >= 0 ){  /* ii = largest index where stat_aux != 0 */
      ii++ ;       /* number of stat_aux values to save      */

      THD_set_float_atr( blk , ATRNAME_STAT_AUX , ii , dset->stat_aux ) ;
   } else {
      THD_erase_one_atr( blk , ATRNAME_STAT_AUX ) ;
   }

   /*----- if time-dependent data, write that stuff out too -----*/

   if( dset->taxis != NULL ){
      itemp[0] = dset->taxis->ntt ;
      itemp[1] = dset->taxis->nsl ;
      itemp[2] = dset->taxis->units_type ;  /* 21 Oct 1996 */

      ITFILL(3,ATRSIZE_TAXIS_NUMS) ;
      THD_set_int_atr( blk , ATRNAME_TAXIS_NUMS ,
                             ATRSIZE_TAXIS_NUMS , itemp ) ;

      ftemp[0] = dset->taxis->ttorg ;
      ftemp[1] = dset->taxis->ttdel ;
      ftemp[2] = dset->taxis->ttdur ;
      ftemp[3] = dset->taxis->zorg_sl ;
      ftemp[4] = dset->taxis->dz_sl ;

      FTFILL(5,ATRSIZE_TAXIS_FLOATS) ;
      THD_set_float_atr( blk , ATRNAME_TAXIS_FLOATS ,
                               ATRSIZE_TAXIS_FLOATS , ftemp ) ;

      if( dset->taxis->toff_sl != NULL )
         THD_set_float_atr( blk , ATRNAME_TAXIS_OFFSETS ,
                                  dset->taxis->nsl , dset->taxis->toff_sl ) ;
   } else {
      THD_erase_one_atr( blk , ATRNAME_TAXIS_NUMS    ) ;
      THD_erase_one_atr( blk , ATRNAME_TAXIS_FLOATS  ) ;
      THD_erase_one_atr( blk , ATRNAME_TAXIS_OFFSETS ) ;
   }

   /*----- 23 Oct 1998: write out tagset, if present -----*/

#undef  NFPER
#define NFPER 5
#define TF(i,j) vtag[(i)*NFPER+(j)]
   if( dset->tagset != NULL && dset->tagset->num > 0 ){
      int ii , ntag=dset->tagset->num , tlen,ilen,jj ;
      float *vtag = (float *) malloc( sizeof(float) * (NFPER*ntag) ) ;
      char *ctag ;

      /* set the counts */

      itemp[0] = ntag ;
      itemp[1] = NFPER ;
      THD_set_int_atr( blk , ATRNAME_TAGSET_NUM , 2 , itemp ) ;

      /* set the values */

      for( ii=0 ; ii < ntag ; ii++ ){
         TF(ii,0) = dset->tagset->tag[ii].x ;
         TF(ii,1) = dset->tagset->tag[ii].y ;
         TF(ii,2) = dset->tagset->tag[ii].z ;
         TF(ii,3) = dset->tagset->tag[ii].val ;
         if( dset->tagset->tag[ii].set ) TF(ii,4) = dset->tagset->tag[ii].ti ;
         else                            TF(ii,4) = -1.0 ;
      }
      THD_set_float_atr( blk , ATRNAME_TAGSET_FLOATS , NFPER*ntag , vtag ) ;
      free(vtag) ;

      /* set the labels */

      tlen = 4 ;                                       /* a little slop space */
      for( ii=0 ; ii < ntag ; ii++ )
        tlen += strlen( dset->tagset->tag[ii].label ) + 1; /* +1 for the '\0' */

      ctag = (char *) malloc( sizeof(char) * tlen ) ;   /* to hold all labels */
      jj   = 0 ;
      for( ii=0 ; ii < ntag ; ii++ ){
         ilen = strlen( dset->tagset->tag[ii].label ) + 1 ;
         memcpy( ctag+jj , dset->tagset->tag[ii].label , ilen ) ;
         jj += ilen ;
      }
      THD_set_char_atr( blk , ATRNAME_TAGSET_LABELS , tlen , ctag ) ;
      free(ctag) ;
   } else {
      THD_erase_one_atr( blk , ATRNAME_TAGSET_NUM    ) ;
      THD_erase_one_atr( blk , ATRNAME_TAGSET_LABELS ) ;
      THD_erase_one_atr( blk , ATRNAME_TAGSET_FLOATS ) ;
   }
#undef NFPER
#undef TF

   /******/
   /****** These attributes used to be set in THD_write_datablock() *****/
   /******/

   /* dataset dimensions */

   atrank[0] = dkptr->rank ;  /* should always be 3 */
   atrank[1] = dkptr->nvals ;
   for( id=2 ; id < ATRSIZE_DATASET_RANK ; id++ ) atrank[id] = 0 ;

   THD_set_int_atr( blk , ATRNAME_DATASET_RANK ,
                          ATRSIZE_DATASET_RANK , atrank ) ;

   for( id=0 ; id < ATRSIZE_DATASET_DIMENSIONS ; id++ )
     atdims[id] = (id < dkptr->rank) ? dkptr->dimsizes[id] : 0 ;

   THD_set_int_atr( blk , ATRNAME_DATASET_DIMENSIONS ,
                          ATRSIZE_DATASET_DIMENSIONS , atdims ) ;

   /* sub-brick types */

   { int *datum_type ;
     datum_type = AFMALL(int, sizeof(int) * blk->nvals ) ;
     for( id=0 ; id < blk->nvals ; id++ )
        datum_type[id] = DBLK_BRICK_TYPE(blk,id) ;
     THD_set_int_atr(   blk , ATRNAME_BRICK_TYPES  , blk->nvals , datum_type ) ;
     free( datum_type ) ;
   }
   THD_set_float_atr( blk, ATRNAME_BRICK_FLTFAC, blk->nvals, blk->brick_fac ) ;

   /** 30 Nov 1997: write out brick labels **/

   if( blk->brick_lab != NULL ){

      int ibr , nch , ipos , ll ;
      char *car ;

      for( ibr=0,nch=0 ; ibr < blk->nvals ; ibr++ ) /* total length  */
         nch += strlen(blk->brick_lab[ibr]) + 1 ;   /* of all labels */

      car = (char *) malloc( sizeof(char) * nch ) ; /* space for all labels */

      for( ibr=0,ipos=0 ; ibr < blk->nvals ; ibr++ ){ /* put all labels */
         ll = strlen(blk->brick_lab[ibr]) + 1 ;       /* together       */
         memcpy( car+ipos , blk->brick_lab[ibr] , ll ) ;
         ipos += ll ;
      }

      THD_set_char_atr( blk , ATRNAME_BRICK_LABS , nch , car ) ;
      free(car) ;
   } else {
      THD_erase_one_atr( blk , ATRNAME_BRICK_LABS ) ;
   }

   /** and write out brick keywords **/

   if( blk->brick_keywords != NULL ){

      int ibr , nch , ipos , ll ;
      char *car ;

      for( ibr=0,nch=0 ; ibr < blk->nvals ; ibr++ ){
        if( blk->brick_keywords[ibr] != NULL )
          nch += strlen(blk->brick_keywords[ibr]) + 1 ;
        else
          nch += 1 ;
      }

      car = (char *) malloc( sizeof(char) * nch ) ;

      for( ibr=0,ipos=0 ; ibr < blk->nvals ; ibr++ ){
        if( blk->brick_keywords[ibr] != NULL ){
          ll = strlen(blk->brick_keywords[ibr]) + 1 ;
          memcpy( car+ipos , blk->brick_keywords[ibr] , ll ) ;
          ipos += ll ;
        } else {
          car[ipos++] = '\0' ;
        }
      }

      THD_set_char_atr( blk , ATRNAME_BRICK_KEYWORDS , nch , car ) ;
      free(car) ;
   } else {
      THD_erase_one_atr( blk , ATRNAME_BRICK_KEYWORDS ) ;
   }

   /* and write out brick stataux parameters */

   if( blk->brick_statcode != NULL &&    /* write out brick stataux */
       blk->brick_stataux  != NULL   ){  /* stuff, if it exists.    */

      int ibr , nfl , jv , ipos , iv ;
      float *far ;

      for( ibr=0,nfl=0 ; ibr < blk->nvals ; ibr++ ){    /* compute total */
         jv = blk->brick_statcode[ibr] ;                /* space needed  */
         if( FUNC_IS_STAT(jv) ) nfl += FUNC_need_stat_aux[jv] + 3 ;
      }

      if( nfl > 0 ){
         far = (float *) malloc( sizeof(float) * nfl ) ;

         for( ibr=0,ipos=0 ; ibr < blk->nvals ; ibr++ ){
            jv = blk->brick_statcode[ibr] ;
            if( FUNC_IS_STAT(jv) ){
              far[ipos++] = ibr ;                     /* save index */
              far[ipos++] = jv ;                      /* save statcode */
              far[ipos++] = FUNC_need_stat_aux[jv] ;  /* save # of params */

              if( blk->brick_stataux[ibr] != NULL ){  /* if have params, save */
                for( iv=0 ; iv < FUNC_need_stat_aux[jv] ; iv++ )
                  far[ipos++] = blk->brick_stataux[ibr][iv] ;
              } else {                                /* should never be used */
                for( iv=0 ; iv < FUNC_need_stat_aux[jv] ; iv++ )
                  far[ipos++] = 0.0 ;
              }
            }
         }

         THD_set_float_atr( blk , ATRNAME_BRICK_STATAUX , nfl , far ) ;
         free(far) ;
      } else {
         THD_erase_one_atr( blk , ATRNAME_BRICK_STATAUX ) ;
      }
   } else {
      THD_erase_one_atr( blk , ATRNAME_BRICK_STATAUX ) ;
   }

   /******/
   /****** N.B.: we do NOT set the byte order attribute here *****/
   /******/ 

   EXRETURN ;
}
