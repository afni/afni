#include "mrilib.h"
#include "thd.h"
#include "niml.h"

/*---------------------------------------------------------------------*/
/*! Write all the attributes for a datablock into a set of NIML data
    elements, stored in a NIML group element.
-----------------------------------------------------------------------*/

NI_group * THD_nimlize_dsetatr( THD_3dim_dataset *dset )
{
   THD_datablock *blk ;
   ATR_any *atr_any ;
   NI_element *nel ;
   int ia , ii ;
   NI_group *ngr = NULL ;   /* will be output */

ENTRY("THD_nimlize_dsetatr") ;

   /*--- sanity checks ---*/

   if( !ISVALID_DSET(dset) ) RETURN(ngr) ;
   blk = dset->dblk ;
   if( blk == NULL || blk->natr == 0 || blk->atr == NULL ) RETURN(ngr) ;

   /* create empty output group */

   ngr = NI_new_group_element() ;

   NI_set_attribute( ngr , "AFNI_idcode" , dset->idcode.str ) ;

   /* make a data element for each attribute ... */

   for( ia=0 ; ia < blk->natr ; ia++ ){

     atr_any = &(blk->atr[ia]) ;
     if( atr_any == NULL ) continue ;   /* bad attribute */

     switch( atr_any->type ){

       case ATR_FLOAT_TYPE:{
         ATR_float *atr_flo = (ATR_float *) atr_any ;

         nel = NI_new_data_element( "AFNI_atr" , atr_flo->nfl ) ;
         nel->outmode = NI_TEXT_MODE ;
         NI_set_attribute( nel , "AFNI_name" , atr_flo->name ) ;
         NI_add_column( nel , NI_FLOAT , atr_flo->fl ) ;
         NI_add_to_group( ngr , nel ) ;
       }
       break ;

       case ATR_INT_TYPE:{
         ATR_int *atr_int = (ATR_int *) atr_any ;

         nel = NI_new_data_element( "AFNI_atr" , atr_int->nin ) ;
         nel->outmode = NI_TEXT_MODE ;
         NI_set_attribute( nel , "AFNI_name" , atr_int->name ) ;
         NI_add_column( nel , NI_INT , atr_int->in ) ;
         NI_add_to_group( ngr , nel ) ;
       }
       break ;

       case ATR_STRING_TYPE:{
         ATR_string *atr_str = (ATR_string *) atr_any ;
         char *str ;  /* create string to hold all data to send */

         nel = NI_new_data_element( "AFNI_atr" , 1 ) ;
         nel->outmode = NI_TEXT_MODE ;
         NI_set_attribute( nel , "AFNI_name" , atr_str->name ) ;

         str = malloc( atr_str->nch + 4 ) ;           /* convert from */
         memcpy( str , atr_str->ch , atr_str->nch ) ; /* char array   */
         THD_zblock( atr_str->nch , str ) ;           /* to C string  */
         str[ atr_str->nch ] = '\0' ;

         NI_add_column( nel , NI_STRING , &str ) ;
         NI_add_to_group( ngr , nel ) ;

         free((void *)str) ;
       }
       break ;

     } /* end of switch on atr type */

   } /* end of loop over all atr's */

   /*--- done ---*/

   RETURN(ngr) ;
}

/*---------------------------------------------------------------------*/
/*! Given a NIML group element, read AFNI attribute elements from it
    and load these into a datablock.
-----------------------------------------------------------------------*/

void THD_dblkatr_from_niml( NI_group *ngr , THD_datablock *blk )
{
   ATR_any       *atr ;
   NI_element    *nel ;
   int            ip  ;

ENTRY("THD_dblkatr_from_niml") ;

   if( ngr                  == NULL          ||
       NI_element_type(ngr) != NI_GROUP_TYPE ||
       blk                  == NULL            ) EXRETURN ;

   /*-- loop over parts and extract data from any '<AFNI_atr ...>' elements --*/

   for( ip=0 ; ip < ngr->part_num ; ip++ ){

     switch( ngr->part_typ[ip] ){

       /*-- a sub-group ==> recursion! --*/

       case NI_GROUP_TYPE:
         THD_dblkatr_from_niml( (NI_group *)ngr->part[ip] , blk ) ;
       break ;

       /*- data ==> see if is marked as an AFNI_atr and has exactly 1 column
                    if so, then extract that column and load into datablock  -*/

       case NI_ELEMENT_TYPE:{ /* data ==> see if is an AFNI attribute */
         NI_element *nel = (NI_element *)ngr->part[ip] ;
         char       *rhs = NI_get_attribute( nel , "AFNI_name" ) ;

         if( strcasecmp(nel->name,"AFNI_atr") == 0 &&    /* AFNI attribute?   */
             nel->vec_num == 1                     &&    /* with 1 column?    */
             nel->vec_len >  0                     &&    /* that is nonempty? */
             rhs != NULL                           &&    /* and has a name?   */
            *rhs != '\0'                              ){ /* a nonempty name?  */

           switch( nel->vec_typ[0] ){ /* 3 different data types of attributes */
             case NI_FLOAT:
               THD_set_float_atr( blk , rhs ,
                                  nel->vec_len , (float *)nel->vec[0] ) ;
             break ;

             case NI_INT:
               THD_set_int_atr( blk , rhs ,
                                nel->vec_len , (int *)nel->vec[0] ) ;
             break ;

             case NI_STRING:{
               char **sar = (char **)nel->vec[0] , *str ;
               int nch ;
               str = strdup(sar[0]) ; nch = strlen(str) ;
               THD_unzblock( nch+1 , str ) ;  /* re-insert NULs */
               THD_set_char_atr( blk , rhs , nch+1 , str ) ;
               free(str) ;
             }
             break ;
           }
         }
       }
       break ;
     }
   } /* end of loop over  parts */

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! Make an AFNI dataset from a NIML group element.
    - The element should contain enough '<AFNI_atr ...>' elements
      to define the dataset header.
    - It may also contain '<VOLUME_DATA ...>' elements that contain
      data for the sub-bricks.  This, however, is optional.
    - If the element contains a 'AFNI_prefix' attribute, then the RHS
      of that will become the dataset's prefix name.
    - If the element contains a 'AFNI_idcode' attribute, then the RHS
      of that will become the dataset's idcode, overriding the value
      that may be stored in the similar '<AFNI_atr ...>' element.
    - If this element can't easily be re-loaded (e.g., came from a
      socket), then the dataset should be super-locked into memory,
      so it won't be purged!
    - If nodata!=0 on input, then any actual sub-brick data elements in
      the group element will not be loaded.  You'll have to call
      THD_add_bricks() to do that later, if desired.
-------------------------------------------------------------------------*/

THD_3dim_dataset * THD_niml_to_dataset( NI_group *ngr , int nodata )
{
   THD_3dim_dataset *dset ;
   THD_datablock *blk ;
   char *rhs ;
   int ii ;

ENTRY("THD_niml_to_dataset") ;

   if( ngr                  == NULL          ||
       NI_element_type(ngr) != NI_GROUP_TYPE   ) RETURN(NULL) ;

   /* create the shell of a dataset and populate it's attributes */

   blk  = EDIT_empty_datablock() ;
   dset = EDIT_empty_copy(NULL) ;

   THD_dblkatr_from_niml( ngr , blk ) ;  /* load attributes from NIML */

   /* build the datablock from the loaded attributes */

   ii = THD_datablock_from_atr( blk , NULL , NULL ) ;

   if( ii == 0 ){                               /* bad attributes */
     THD_delete_datablock( blk ) ; RETURN(NULL) ;
   }

   /* build the dataset from the datablock */

   THD_allow_empty_dataset(1) ;
   dset = THD_3dim_from_block( blk ) ;
   THD_allow_empty_dataset(0) ;
   if( dset == NULL ){
     THD_delete_datablock( blk ) ; RETURN(NULL) ;
   }

   DSET_mallocize(dset) ;   /* just to be sure */

   /* change the name of the dataset? */

   rhs = NI_get_attribute( ngr , "AFNI_prefix" ) ;
   if( rhs != NULL )
     EDIT_dset_items( dset , ADN_prefix,rhs , ADN_none ) ;

   /* change the idcode of the dataset? */

   rhs = NI_get_attribute( ngr , "AFNI_idcode" ) ;
   if( rhs != NULL )
     NI_strncpy( dset->idcode.str , rhs , MCW_IDSIZE ) ;

   /* now scan the group element for data elements that fill sub-bricks */

   if( !nodata ){
     (void)THD_add_bricks( dset , ngr ) ;
     THD_update_statistics( dset ) ;
   }

   RETURN(dset) ;
}

/*---------------------------------------------------------------------------*/
/*! Scan the NIML data or group element for sub-bricks to add to the given
    dataset.
     - Return value is number sub-bricks found (0 == something bad).
     - Note that the '<VOLUME_DATA ...>' elements don't have to contain
       the right amount of data for a sub-brick.  If there is too little,
       the remaining voxels will be 0; if too much, the excess data is
       ignored.
     - Each column of data creates or replaces one sub-brick.
     - At the present time, the only datatypes allowed are:
         byte, short, float, complex (float pairs), and rgb (byte triples).
       Columns of other types will be ignored.
     - If a '<VOLUME_DATA ...>' element contains an 'index' attribute, then:
       - index >= 0 indicates which sub-brick the 1st column of the element
         goes into; subsequent columns go into successive sub-bricks, and
         any data already present in these sub-bricks will be over-written.
       - To indicate that the data is to be appended to the dataset, making
         new sub-bricks, just set index to a very large positive value
         (e.g., 9999999).
       - If 'index' is missing or negative, then the data columns will go
         into the first empty sub-bricks that are found; if none are found,
         new sub-bricks will be appended to the dataset.
     - If a '<VOLUME_DATA ...>' element contains a 'scale_factor' attribute,
       and the numerical value of factor is positive, this value is used to
       set the scale factor for all sub-bricks loaded from the element.
       Otherwise, any scale factor previously set will remain in effect.
     - Elements that are not named 'VOLUME_DATA' will be ignored here.
-----------------------------------------------------------------------------*/

int THD_add_bricks( THD_3dim_dataset *dset , void *nini )
{
   int nbr=0 , tt=NI_element_type(nini) ;
   NI_element *nel ;
   int nxyz , ii , jj , nbar , vlen , kk , bb ;
   void *bar ;
   char *str ;
   float fac ;

ENTRY("THD_add_bricks") ;

   if( !ISVALID_DSET(dset) || tt < 0 ) RETURN(0) ;

   /*-- if have a group element, do the parts by recursion --*/

   if( tt == NI_GROUP_TYPE ){
     NI_group *ngr = (NI_group *)nini ;
     int ip ;
     for( ip=0 ; ip < ngr->part_num ; ip++ )  /* loop over parts */
       nbr += THD_add_bricks( dset , ngr->part[ip] ) ;
     RETURN(nbr) ;
   }

   /*-- if here, have a single data element --*/

   nel = (NI_element *)nini ;

   /*- check element name to see if it's what we want -*/

   if( strcasecmp(nel->name,"VOLUME_DATA") != 0 ) RETURN(0) ;

   nxyz = DSET_NVOX(dset) ;   /* number of voxels in a sub-brick */
   vlen = nel->vec_len ;      /* number of values in a column of data */
   if( vlen > nxyz ) vlen = nxyz ;

   if( nel->vec_num < 1 || vlen < 1 ) RETURN(0) ;  /* no data at all? */

   /*- find index of sub-brick, if present -*/

   kk  = -1 ;                                 /* flag for overwrite */
                     str = NI_get_attribute( nel , "AFNI_index" ) ;
   if( str == NULL ) str = NI_get_attribute( nel , "index"      ) ;
   if( str != NULL && isdigit(*str) )
     kk = (int)strtol( str , NULL , 10 ) ;

   /*- and scale factor, if present -*/

   fac = 0.0 ;
                     str = NI_get_attribute( nel , "AFNI_factor"  ) ;
   if( str == NULL ) str = NI_get_attribute( nel , "scale_factor" ) ;
   if( str != NULL && ( *str== '-' || isdigit(*str) ) )
     fac = (float)strtod( str , NULL ) ;

   if(PRINT_TRACING){
     char str[256] ;
     sprintf(str,"kk=%d vlen=%d nxyz=%d fac=%f\n",kk,vlen,nxyz,fac);
     STATUS(str);
   }

   /*- loop over columns and enter them into the dataset -*/

   for( jj=0 ; jj < nel->vec_num ; jj++ ){

     if( !AFNI_GOOD_DTYPE(nel->vec_typ[jj]) ) continue ; /* skip this */

     /* create a volume array to hold this data */

     nbar = mri_datum_size(nel->vec_typ[jj]) ;   /* size of one value */
     bar = calloc( nbar , nxyz ) ;             /* will be zero filled */
     if( bar == NULL ) RETURN(nbr) ;               /* malloc failure! */

     /* copy data from element into this volume */

     memcpy( bar , nel->vec[jj] , vlen*nbar ) ;

     /* find a place (bb) to put this volume in the dataset */

     if( kk < 0 ){  /* scan for an empty sub-brick */
       for( ii=0 ; ii < DSET_NVALS(dset) ; ii++ )
         if( DSET_ARRAY(dset,ii) == NULL ) break ;
       if( ii == DSET_NVALS(dset) ) kk = ii ;  /* all full */
       bb = ii ;                               /* put here */
     } else if( kk > DSET_NVALS(dset) ){
       bb = DSET_NVALS(dset) ;                 /* at end */
     } else {
       bb = kk ;                               /* exactly here */
     }

     if( bb < DSET_NVALS(dset) ){   /* replace existing data */
       EDIT_substitute_brick( dset , bb , nel->vec_typ[jj] , bar ) ;

     } else {                       /* append new sub-brick */
       bb = DSET_NVALS(dset) ;
       EDIT_add_brick( dset , nel->vec_typ[jj] , 0.0 , bar ) ;
     }
     nbr++ ;   /* 1 more sub-brick! */

          if( fac > 0.0 ) EDIT_BRICK_FACTOR(dset,bb,fac) ;
     else if( fac < 0.0 ) EDIT_BRICK_FACTOR(dset,bb,0.0) ;

     DSET_CRUSH_BSTAT(dset,bb) ;

     if( kk >= 0 ) kk++ ;  /* move to next sub-brick */
   }

   RETURN(nbr) ;
}

/*---------------------------------------------------------------------------*/
/*! Put a dataset sub-brick into a '<VOLUME_DATA ...>' element.
    - Returns NULL if the input is stupid.
-----------------------------------------------------------------------------*/

NI_element * THD_subbrick_to_niml( THD_3dim_dataset *dset, int ival, int flags )
{
   NI_element *nel ;
   char rhs[64] ;
   void *bar ;
   int  ityp , nxyz , nbar ;

ENTRY("THD_subbrick_to_niml") ;

   if( !ISVALID_DSET(dset) ||
       ival < 0            || ival >= DSET_NVALS(dset) ) RETURN(NULL) ;

   bar  = DSET_ARRAY(dset,ival) ; if( bar == NULL ) RETURN(NULL) ;

   ityp = DSET_BRICK_TYPE(dset,ival) ;  /* type of data in bar */
   nbar = mri_datum_size(ityp) ;        /* size of one value */
   nxyz = DSET_NVOX(dset) ;             /* number of voxels */

   nel = NI_new_data_element( "VOLUME_DATA" , nxyz ) ;
   NI_set_attribute( nel , "AFNI_idcode" , dset->idcode.str ) ;
   NI_add_column( nel , ityp , bar ) ;
   nel->outmode = NI_BINARY_MODE ;  /* write this in binary mode */

   /*-- add any special attributes desired by the caller --*/

   if( (flags & SBFLAG_INDEX) ){
     sprintf(rhs,"%d",ival) ;
     NI_set_attribute( nel , "AFNI_index" , rhs ) ;
   }

   if( (flags & SBFLAG_FACTOR) ){
     float fac = DSET_BRICK_FACTOR(dset,ival) ;
     if( fac > 0.0 ){
       sprintf(rhs,"%f",fac) ;
       NI_set_attribute( nel , "AFNI_factor" , rhs ) ;
     }
   }

   RETURN(nel) ;
}

/*---------------------------------------------------------------------------*/
/*! Put an entire dataset into a single NI group element.
-----------------------------------------------------------------------------*/

NI_group * THD_dataset_to_niml( THD_3dim_dataset *dset )
{
   NI_element *nel ;
   NI_group *ngr ;
   int iv ;

ENTRY("THD_dataset_to_niml") ;

   /* put AFNI dataset attributes into a group */

   ngr = THD_nimlize_dsetatr( dset ) ;
   if( ngr == NULL ) RETURN(NULL) ;

STATUS("rename group to 'AFNI_dataset'") ;
   NI_rename_group( ngr , "AFNI_dataset" ) ;

   /* now add a data element for each sub-brick */

STATUS("adding sub-bricks") ;
   for( iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
     nel = THD_subbrick_to_niml( dset , iv , 0 ) ;
     if( nel != NULL ) NI_add_to_group( ngr , nel ) ;
   }

   RETURN(ngr) ;
}

/*---------------------------------------------------------------------------*/

int AFNI_obj_to_dataset( NI_objcontainer *dc )
{
   THD_3dim_dataset *dset ;

   if( dc == NULL || strcmp(dc->self_name,"AFNI_dataset") != 0 ) return 0 ;

   dset = THD_niml_to_dataset( (NI_group *)dc->self_data , 0 ) ;
   if( dset == NULL ) return 0 ;

   NI_free_element( dc->self_data ) ;
   dc->self_data = (void *)dset ;
   return 1 ;
}

/*---------------------------------------------------------------------------*/

int AFNI_dataset_to_obj( NI_objcontainer *dc )
{
   NI_group *ngr ;
   THD_3dim_dataset *dset ;

   if( dc == NULL || strcmp(dc->typename,"AFNI_dataset") != 0 ) return 0 ;

   dset = (THD_3dim_dataset *)dc->self_data ;
   if( !ISVALID_DSET(dset) ) return 0 ;

   ngr = THD_dataset_to_niml( dset ) ;
   if( ngr == NULL ) return 0 ;

   dc->self_data = (void *)ngr ;
   return 1 ;
}
