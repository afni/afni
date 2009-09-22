#include "mrilib.h"
#include "thd.h"

/*******************************************************************/
/********** 21 Mar 2003: Read a 3D file as an AFNI dataset *********/
/*******************************************************************/

/*-----------------------------------------------------------------*/
/*! Open a NIML 3D file as an AFNI dataset.  Each column is a
    separate sub-brick.  Dataset returned is empty (no data).

    Broken into modules.      31 May 2006 [rickr]
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_3D( char *pathname )
{
   THD_3dim_dataset *dset=NULL ;  /* use locals only for tracing */
   NI_element *nel ;

ENTRY("THD_open_3D") ;

   nel = read_niml_file(pathname, 0) ;  /* do not get data */
   if( nel ) dset = THD_niml_3D_to_dataset(nel, pathname) ;

   RETURN(dset) ;
}

/*-----------------------------------------------------------------*/
/*! Convert a NIML 3D element (no data) to an AFNI dataset.
    This was moved from THD_open_3D().  31 May 2006 [rickr]
-------------------------------------------------------------------*/
THD_3dim_dataset * THD_niml_3D_to_dataset( NI_element * nel, char * pathname )
{
   THD_3dim_dataset *dset=NULL ;
   char prefix[1024] , *ppp ;
   THD_ivec3 nxyz , orixyz ;
   THD_fvec3 dxyz , orgxyz ;
   int nvals , ii ;

ENTRY("THD_3D_niml_to_dataset") ;

   /*-- check data element for reasonability --*/

   if( nel == NULL                               ||   /* bad read */
       NI_element_type(nel) != NI_ELEMENT_TYPE   ||   /* bad element */
       nel->vec_num <= 0                         ||   /* no data */
       nel->vec_len <= 0                         ||   /* no data */
       strcmp(nel->name,"AFNI_3D_dataset") != 0  ||   /* incorrect data */
       nel->vec_rank > 3                           ){ /* weird header */

     fprintf(stderr,"** Can't read 3D head from %s\n",pathname) ;
     NI_free_element(nel) ; RETURN(NULL) ;
   }

STATUS("checking header") ;

   /*-- check column types to make sure they are all numeric --*/
   /*   [AFNI doesn't like String or compound type datasets]   */

   for( ii=0 ; ii < nel->vec_num ; ii++ ){

     if( !NI_IS_NUMERIC_TYPE(nel->vec_typ[ii]) ){
       fprintf(stderr,"** 3D file %s isn't numeric!\n",pathname) ;
       NI_free_element(nel) ; RETURN(NULL) ;
     }
   }

   /*-- now have good data element ==> make a dataset --*/

STATUS("making dataset") ;

   dset = EDIT_empty_copy(NULL) ;  /* default dataset */

   /* set prefix from input filename */

STATUS("setting prefix") ;

   ppp = THD_trailname(pathname,0) ;              /* strip directory */
   NI_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;  /* to make prefix */

   /* set grid sizes from element header */

STATUS("setting grid sizes") ;

   nxyz.ijk[0] = nel->vec_len ; nxyz.ijk[1] = nxyz.ijk[2] = 1 ;
   if( nel->vec_axis_len != NULL ){
     if( nel->vec_rank >= 1 ) nxyz.ijk[0] = nel->vec_axis_len[0] ;
     if( nel->vec_rank >= 2 ) nxyz.ijk[1] = nel->vec_axis_len[1] ;
     if( nel->vec_rank >= 3 ) nxyz.ijk[2] = nel->vec_axis_len[2] ;
   }

   /* set grid spacings */

STATUS("setting grid spacings") ;

   dxyz.xyz[0] = dxyz.xyz[1] = dxyz.xyz[2] = 1.0 ;
   if( nel->vec_axis_delta != NULL ){
     if( nel->vec_rank >= 1) dxyz.xyz[0] = nel->vec_axis_delta[0] ;
     if( nel->vec_rank >= 2) dxyz.xyz[1] = nel->vec_axis_delta[1] ;
     if( nel->vec_rank >= 3) dxyz.xyz[2] = nel->vec_axis_delta[2] ;
   }

   /* set grid origins */

STATUS("setting grid origins") ;

   orgxyz.xyz[0] = orgxyz.xyz[1] = orgxyz.xyz[2] = 0.0 ;
   if( nel->vec_axis_origin != NULL ){
     if( nel->vec_rank >= 1) orgxyz.xyz[0] = nel->vec_axis_origin[0] ;
     if( nel->vec_rank >= 2) orgxyz.xyz[1] = nel->vec_axis_origin[1] ;
     if( nel->vec_rank >= 3) orgxyz.xyz[2] = nel->vec_axis_origin[2] ;
   }

   /* set grid orientations (default is RAI) */

STATUS("setting grid orientation") ;

   { char orcx='R', orcy='A', orcz='I' ;
     int oxx,oyy,ozz ;
     if( nel->vec_rank == 3 && nel->vec_axis_label != NULL ){
       orcx = toupper(nel->vec_axis_label[0][0]) ;
       orcy = toupper(nel->vec_axis_label[1][0]) ;
       orcz = toupper(nel->vec_axis_label[2][0]) ;
     }
     oxx = ORCODE(orcx); oyy = ORCODE(orcy); ozz = ORCODE(orcz);
     if( !OR3OK(oxx,oyy,ozz) ){
       oxx = ORI_R2L_TYPE; oyy = ORI_A2P_TYPE; ozz = ORI_I2S_TYPE;
     }

     orixyz.ijk[0] = oxx ; orixyz.ijk[1] = oyy ; orixyz.ijk[2] = ozz ;
   }

   /* number of sub-bricks (one per vector/column) */

   nvals = nel->vec_num ;

   /* set idcode from element, or take random default one */

STATUS("setting idcode") ;

   ppp = NI_get_attribute( nel , "self_idcode" ) ;
   if( ppp == NULL )
     ppp = NI_get_attribute( nel , "ni_idcode" ) ;
   if( ppp != NULL && *ppp != '\0' ){
     NI_strncpy( dset->idcode.str , ppp , MCW_IDSIZE ) ;
   } else {
     MCW_hash_idcode( pathname , dset ) ; /* 06 May 2005 */
     dset->idcode.str[0] = 'A' ;          /* overwrite 1st 3 bytes of idcode */
     dset->idcode.str[1] = '3' ;
     dset->idcode.str[2] = 'D' ;
   }

   /*-- now modify the default dataset --*/

STATUS("Editing dataset") ;

   EDIT_dset_items( dset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_array , nel->vec_typ ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_xyzorient   , orixyz ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_nvals       , nvals ,
                      ADN_type        , HEAD_ANAT_TYPE ,
                      ADN_func_type   , ANAT_BUCK_TYPE ,
                    ADN_none ) ;

   dset->dblk->diskptr->storage_mode = STORAGE_BY_3D ;
   NI_strncpy( dset->dblk->diskptr->brick_name , pathname , THD_MAX_NAME ) ;

   /*-- time axis? [03 Jun 2005] --*/

   ppp = NI_get_attribute( nel , "ni_timestep" ) ;
   if( ppp != NULL && nvals > 1 ){
     float dt = strtod(ppp,NULL) ; if( dt <= 0.0 ) dt = 1.0 ;
     if (dt > 360) {
      dt *= 0.001;
      fprintf(stderr,"Warning: ni_timestep appears incorrecly set in msec.\n"
                     "Reducing dt by a factor of 1000 to %fsec.\n", dt);
     }
     EDIT_dset_items( dset ,
                        ADN_func_type , ANAT_EPI_TYPE ,
                        ADN_ntt       , nvals ,
                        ADN_ttdel     , dt ,
                        ADN_tunits    , UNITS_SEC_TYPE ,
                      ADN_none ) ;
   }

STATUS("checking for statistics") ;

   /*-- see if we have any statistics bricks --*/

   ppp = NI_get_attribute( nel , "ni_stat" ) ;
   if( ppp != NULL ){
     NI_str_array *sar = NI_decode_string_list( ppp , ";" ) ;
     if( sar != NULL ){
       int itop=MIN(sar->num,nvals) , jj,ll ;
       char *dnam , qnam[64] ;
       for( ii=0 ; ii < itop ; ii++ ){
         if( strcmp(sar->str[ii],"none") == 0 ) continue ;
         for( jj=NI_STAT_FIRSTCODE ; jj <= NI_STAT_LASTCODE ; jj++ ){
           dnam = NI_stat_distname(jj) ;
           strcpy(qnam,dnam); strcat(qnam,"("); ll = strlen(qnam);
           if( strncmp(sar->str[ii],qnam,ll) == 0 ) break ;
         }
         if( jj >= AFNI_FIRST_STATCODE && jj <= AFNI_LAST_STATCODE ){
           float parm[4]={1,1,1,1} ; int np,kk,mm , sp ;
           np = NI_stat_numparam(jj) ; sp = ll ;
           for( kk=0 ; kk < np ; kk++ ){
             mm = 0 ; sscanf(sar->str[ii]+sp,"%f%n",parm+kk,&mm) ; sp += mm+1 ;
           }
           EDIT_STATAUX4( dset , ii , jj , parm[0],parm[1],parm[2],parm[3] ) ;
         }
       }
       NI_delete_str_array(sar) ;
     }
   }

   /*-- purge the NIML data element and return the new dataset --*/

STATUS("freeing element") ;

   NI_free_element( nel ) ;
   RETURN(dset) ;
}

/*-----------------------------------------------------------------*/
/*! Mark a dataset's storage mode.
-------------------------------------------------------------------*/

void THD_set_storage_mode( THD_3dim_dataset *dset , int mm )
{
   if( !ISVALID_DSET(dset)         ||
       mm < 0                      ||
       mm > LAST_STORAGE_MODE      ||
       dset->dblk == NULL          ||
       dset->dblk->diskptr == NULL   ) return ;

   dset->dblk->diskptr->storage_mode = mm ;
}

/*-----------------------------------------------------------------*/
/*!  Load a 3D dataset's data into memory.
     Called from THD_load_datablock() in thd_loaddblk.c.
-------------------------------------------------------------------*/

void THD_load_3D( THD_datablock *dblk )
{
   THD_diskptr *dkptr ;
   NI_element *nel ;
   NI_stream ns ;
   int nxyz , iv,nv ;
   char *ppp ;

ENTRY("THD_load_3D") ;

   /*-- open input [these errors should never occur] --*/

   if( !ISVALID_DATABLOCK(dblk)                     ||
       dblk->diskptr->storage_mode != STORAGE_BY_3D ||
       dblk->brick == NULL                            ) EXRETURN ;

   dkptr = dblk->diskptr ;
   nxyz  = dkptr->dimsizes[0] * dkptr->dimsizes[1] * dkptr->dimsizes[2] ;
   nv    = dkptr->nvals ;

   if( nxyz*nv > 1000000 ) fprintf(stderr,"++ Reading %s\n",dkptr->brick_name) ;

   ppp = (char*)calloc( sizeof(char) , strlen(dkptr->brick_name)+16 ) ;

   strcpy(ppp,"file:") ; strcat(ppp,dkptr->brick_name) ;
   ns = NI_stream_open( ppp , "r" ) ; free(ppp) ;
   if( ns == NULL ) EXRETURN ;

   NI_skip_procins(1) ;
   nel = NI_read_element(ns,333); NI_stream_close(ns);
   NI_skip_procins(0) ;
   if( nel == NULL ) EXRETURN ;

   /*-- allocate space for data --*/

   if( nxyz != nel->vec_len || nv != nel->vec_num ){
     fprintf(stderr,"THD_load_3D(%s): nxyz or nv mismatch!\n",dkptr->brick_name) ;
     fprintf(stderr,"                 expect nxyz=%d; got %d\n",nxyz, nel->vec_len);
     fprintf(stderr,"                 expect nv  =%d; got %d\n",nv  , nel->vec_num);
     NI_free_element(nel) ; EXRETURN ;
   }

   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /** malloc space for each brick separately,
       and copy data from element into place  **/

   for( iv=0 ; iv < nv ; iv++ ){
     if( DBLK_ARRAY(dblk,iv) == NULL ){                    /* needs data */
       ppp = AFMALL(char, DBLK_BRICK_BYTES(dblk,iv) );     /* make space */
       if( ppp == NULL ) break ;                           /* bad bad bad */
       mri_fix_data_pointer( ppp, DBLK_BRICK(dblk,iv) ) ;
       memcpy( ppp, nel->vec[iv], DBLK_BRICK_BYTES(dblk,iv) ) ;
       NI_free(nel->vec[iv]) ; nel->vec[iv] = NULL ;
     }
   }

   NI_free_element(nel) ;

   /* if malloc failed, then delete all bricks */

   if( iv < nv ){
     fprintf(stderr,"\n** malloc failed for 3D dataset input!\n");
     for( iv=0 ; iv < nv ; iv++ ){
       if( DBLK_ARRAY(dblk,iv) != NULL ){
         free( DBLK_ARRAY(dblk,iv) ) ;
         mri_fix_data_pointer( NULL, DBLK_BRICK(dblk,iv) ) ;
       }
     }
   }

   EXRETURN ;
}

/*------------------------------------------------------------------*/
/*! Write a dataset to disk as a 3D file.
    Called from THD_write_3dim_dataset().
    This is kind of cheating, since we just call THD_write_1D()
    instead.
--------------------------------------------------------------------*/

void THD_write_3D( char *sname, char *pname , THD_3dim_dataset *dset )
{
   THD_write_1D( sname,pname,dset ) ;
}
