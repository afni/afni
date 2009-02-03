#include "niml_private.h"

/*-----------------------------------------------------------------------*/
/*! Construct an empty data element from a header.
    - The data vectors will have space allocated, but they will be
      filled with all zero bytes.
    - If the header was "empty" (ended in "/>"), then no vectors will
      be allocated, and nel->vec_num=0.
    - This function is used by NI_read_element() to create the
      data element after the header has been parsed.
    - 27 Mar 2003: modified to allow vec_len=0, indicating vector
      length to be inferred from amount of data
-------------------------------------------------------------------------*/

NI_element * make_empty_data_element( header_stuff *hs )
{
   NI_element *nel ;
   int ii ;

   if( hs == NULL || hs->name == NULL ) return NULL ;

#ifdef NIML_DEBUG
NI_dpr("ENTER make_empty_data_element\n") ;
#endif

   nel = NI_malloc(NI_element, sizeof(NI_element) ) ;

   nel->type = NI_ELEMENT_TYPE ;

   nel->outmode = -1 ;   /* 29 Mar 2005 */

   /* move name and attributes from hs to new element */

   nel->name = hs->name ; hs->name = NULL ;

   nel->attr_num = hs->nattr ;

   if( nel->attr_num > 0 ){
      nel->attr_lhs = hs->lhs ; hs->lhs = NULL ;
      nel->attr_rhs = hs->rhs ; hs->rhs = NULL ;
   } else {
      nel->attr_lhs = nel->attr_rhs = NULL ;
   }

   /* set default vector parameters [indicating no data] */

   nel->vec_num = 0 ;
   nel->vec_len = 0 ;
   nel->vec_typ = NULL ;
   nel->vec     = NULL ;

   nel->vec_filled = 0 ;  /* no data has been filled into vectors */

   nel->vec_rank        = 0 ;
   nel->vec_axis_len    = NULL ;
   nel->vec_axis_delta  = NULL ;
   nel->vec_axis_origin = NULL ;
   nel->vec_axis_unit   = NULL ;
   nel->vec_axis_label  = NULL ;

   if( !hs->empty ){  /* find and process ni_* attributes about vectors */

     /* ni_type attribute: set types of vectors */

     ii = string_index( "ni_type" , nel->attr_num , nel->attr_lhs ) ;

     if( ii >= 0 && nel->attr_rhs[ii] != NULL ){
       int_array *iar = decode_type_string( nel->attr_rhs[ii] ) ;
       if( iar != NULL ){
         nel->vec_num = iar->num ;  /* number of vectors */
         nel->vec_typ = iar->ar ;   /* vector types */
         NI_free(iar) ;             /* just the shell of the struct */
       }
     }

     /* ni_dimen attribute: set vector length and rank */

     ii = string_index( "ni_dimen" , nel->attr_num , nel->attr_lhs ) ;

     if( ii >= 0 && nel->attr_rhs[ii] != NULL ){
        int_array *dar = decode_dimen_string( nel->attr_rhs[ii] ) ;
        if( dar != NULL && dar->num > 0 ){
           int nd=dar->num , qq,pp ;
           /* compute product of all dimensions */
           for( qq=1,pp=0 ; pp < nd ; pp++ ) qq *= dar->ar[pp] ;
           nel->vec_len      = qq ;      /* length of vectors */
           nel->vec_rank     = nd ;      /* number of dimensions */
           nel->vec_axis_len = dar->ar ; /* array of dimension lengths */
           NI_free(dar) ;                /* just the struct shell */
           if( nel->vec_len == 0 )       /* 27 Mar 2003 */
             nel->vec_rank = 1 ;
        }
     }

     /* if we had ni_dimen, also try ni_delta */

     ii = string_index( "ni_delta" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_rank > 0 ){
        NI_str_array *sar = NI_decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_delta = NI_malloc(float,sizeof(float)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             sscanf( sar->str[pp] , "%f" , nel->vec_axis_delta+pp ) ;
           NI_delete_str_array(sar) ;
        }
     }

     /* if we had ni_dimen, also try ni_origin */

     ii = string_index( "ni_origin" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_rank > 0 ){
        NI_str_array *sar = NI_decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_origin = NI_malloc(float,sizeof(float)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             sscanf( sar->str[pp] , "%f" , nel->vec_axis_origin+pp ) ;
           NI_delete_str_array(sar) ;
        }
     }

     /* if we had ni_dimen, also try ni_units */

     ii = string_index( "ni_units" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_rank > 0 ){
        NI_str_array *sar = NI_decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_unit = NI_malloc(char*,sizeof(char *)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             nel->vec_axis_unit[pp] = NI_strdup(sar->str[pp]) ;
           NI_delete_str_array(sar) ;
        }
     }

     /* if we had ni_dimen, also try ni_axes */

     ii = string_index( "ni_axes" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_rank > 0 ){
        NI_str_array *sar = NI_decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_label = NI_malloc(char*,sizeof(char *)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             nel->vec_axis_label[pp] = NI_strdup(sar->str[pp]) ;
           NI_delete_str_array(sar) ;
        }
     }

     /* supply vector parameters if none was given */
     /* (remember, we DON'T have an empty element) */

     if( nel->vec_num == 0 ){                    /* default type */
        nel->vec_num    = 1 ;
        nel->vec_typ    = NI_malloc(int,sizeof(int)) ;
        nel->vec_typ[0] = NI_BYTE ;
     }

     if( nel->vec_rank == 0 ){                  /* default dimensions */
        nel->vec_len         = 0 ;
        nel->vec_rank        = 1 ;
        nel->vec_axis_len    = NI_malloc(int, sizeof(int)) ;
        nel->vec_axis_len[0] = 1 ;
     }

     /* now allocate space for vectors defined above */

     nel->vec = NI_malloc(void*, sizeof(void *)*nel->vec_num ) ;

     /* 27 Mar 2003: only allocate space if we know how long they are */

     if( nel->vec_len > 0 ){
       for( ii=0 ; ii < nel->vec_num ; ii++ )
         nel->vec[ii] = NI_malloc(void,
                                  NI_type_size(nel->vec_typ[ii])*nel->vec_len) ;
     } else {
       for( ii=0 ; ii < nel->vec_num ; ii++ )
         nel->vec[ii] = NULL ;
     }

   } /* end of processing non-empty header stuff */

   return nel ;
}

/*-------------------------------------------------------------------------*/
/*! Make an empty group element from parsed header info.
    The attributes in the header are assigned to the group, and the group
    parts are initialized to nothing.
---------------------------------------------------------------------------*/

NI_group * make_empty_group_element( header_stuff *hs )
{
   NI_group *ngr ;

   if( hs == NULL || hs->name == NULL ) return NULL ;

   ngr = NI_malloc(NI_group, sizeof(NI_group) ) ;

   ngr->type = NI_GROUP_TYPE ;

   ngr->name = hs->name ; hs->name = NULL ;  /* 24 Feb 2005 */

   ngr->outmode = -1 ;   /* 29 Mar 2005 */

   /* move attributes from hs to new element */

   ngr->attr_num = hs->nattr ;

   if( ngr->attr_num > 0 ){
     ngr->attr_lhs = hs->lhs ; hs->lhs = NULL ;
     ngr->attr_rhs = hs->rhs ; hs->rhs = NULL ;
   } else {
     ngr->attr_lhs = ngr->attr_rhs = NULL ;
   }

   /* have no pieces-parts yet */

   ngr->part_num = 0 ;
   ngr->part_typ = NULL ;
   ngr->part     = NULL ;

   return ngr ;
}

/*-------------------------------------------------------------------------*/
/*! Byte size of a given integer type code.
    Modified 13 Feb 2003 to use the new rowtype stuff.
---------------------------------------------------------------------------*/

int NI_type_size( int tval )
{
   int ii = NI_rowtype_code_to_size( tval ) ;
   return (ii > 0) ? ii : 0 ;
}

/*************************************************************************/
/********** Functions to create NIML data and group elements *************/
/*************************************************************************/

/*-----------------------------------------------------------------------*/
/*! Return the type of something that points to a NI element.
    - The input should be point to a NI_element, NI_group, or NI_procins.
    - The return value is NI_ELEMENT_TYPE, NI_GROUP_TYPE, NI_PROCINS_TYPE,
      or -1 if the type is anything else or unknowable.
-------------------------------------------------------------------------*/

int NI_element_type( void *nini )
{
   NI_element *nel = (NI_element *) nini ;
   NI_group   *ngr = (NI_group *)   nini ;
   NI_procins *npi = (NI_procins *) nini ;  /* 16 Mar 2005 */

   if( nini == NULL ) return -1 ;

   if( nel->type == NI_ELEMENT_TYPE ) return NI_ELEMENT_TYPE ;
   if( ngr->type == NI_GROUP_TYPE   ) return NI_GROUP_TYPE   ;
   if( npi->type == NI_PROCINS_TYPE ) return NI_PROCINS_TYPE ;

   return -1 ;
}

/*-----------------------------------------------------------------------*/
/*! Return the name of a NI element.  If the input is bad, returns
    a NULL pointer.  Do not free this pointer!  It points to the
    name string inside the element struct.
-------------------------------------------------------------------------*/

char * NI_element_name( void *nini )
{
   NI_element *nel = (NI_element *) nini ;
   NI_group   *ngr = (NI_group *)   nini ;
   NI_procins *npi = (NI_procins *) nini ;

   if( nini == NULL ) return NULL ;

   if( nel->type == NI_ELEMENT_TYPE ) return nel->name ;
   if( ngr->type == NI_GROUP_TYPE   ) return ngr->name ;
   if( npi->type == NI_PROCINS_TYPE ) return npi->name ;

   return NULL ;
}

/*-----------------------------------------------------------------------*/
/*! Expunge a data or group element and its contents from the universe.
-------------------------------------------------------------------------*/

void NI_free_element( void *nini )
{
   int ii , tt=NI_element_type(nini) ;

   if( tt < 0 ) return ; /* bad input */

   /*-- erase contents of data element --*/

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *)nini ;

      NI_free(nel->name) ;
      for( ii=0 ; ii < nel->attr_num ; ii++ ){
         NI_free( nel->attr_lhs[ii] ) ;
         NI_free( nel->attr_rhs[ii] ) ;
      }
      NI_free( nel->attr_lhs ) ;
      NI_free( nel->attr_rhs ) ;

      /* 14 Feb 2003: NI_free_column() will also free var dim arrays */

      if( nel->vec != NULL )
        for( ii=0 ; ii < nel->vec_num ; ii++ )
           NI_free_column( NI_rowtype_find_code(nel->vec_typ[ii]) ,
                           nel->vec_len , nel->vec[ii]             ) ;

      NI_free( nel->vec_typ  ) ;
      NI_free( nel->vec ) ;

      NI_free(nel->vec_axis_len) ;
      NI_free(nel->vec_axis_delta) ;
      NI_free(nel->vec_axis_origin) ;
      NI_free(nel->vec_axis_unit) ;
      NI_free(nel->vec_axis_label) ;

      NI_free( nel ) ;

   /*-- erase contents of group element --*/

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *)nini ;

      for( ii=0 ; ii < ngr->attr_num ; ii++ ){
        NI_free( ngr->attr_lhs[ii] ) ;
        NI_free( ngr->attr_rhs[ii] ) ;
      }
      NI_free( ngr->attr_lhs ) ;
      NI_free( ngr->attr_rhs ) ;

      if( ngr->part != NULL ){
        for( ii=0 ; ii < ngr->part_num ; ii++ )
          NI_free_element( ngr->part[ii] ) ;     /* recursion */
      }

      NI_free( ngr->part_typ ) ;
      NI_free( ngr->part ) ;
      NI_free( ngr->name ) ;    /* 03 Jun 2002 */
      NI_free( ngr ) ;

   /*-- erase contents of processing instruction --*/

   } else if( tt == NI_PROCINS_TYPE ){
      NI_procins *npi = (NI_procins *)nini ;

      for( ii=0 ; ii < npi->attr_num ; ii++ ){
        NI_free( npi->attr_lhs[ii] ) ;
        NI_free( npi->attr_rhs[ii] ) ;
      }
      NI_free( npi->attr_lhs ) ;
      NI_free( npi->attr_rhs ) ;

      NI_free( npi->name ) ;    /* 03 Jun 2002 */
      NI_free( npi ) ;
   }

   return ;
}

/*-----------------------------------------------------------------------*/
/*! Expunge all data from element or group.           17 Jul 2006 [rickr]
-------------------------------------------------------------------------*/

void NI_free_element_data( void *nini )
{
   int ii , tt=NI_element_type(nini) ;

   if( tt < 0 ) return ; /* bad input */

   /*-- if element, nuke data --*/

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *)nini ;

      if( nel->vec != NULL ){
        for( ii=0 ; ii < nel->vec_num ; ii++ )
           NI_free_column( NI_rowtype_find_code(nel->vec_typ[ii]) ,
                           nel->vec_len , nel->vec[ii]             ) ;
         NI_free( nel->vec ) ;
         nel->vec = NULL ;
      }

   /*-- if group, recur --*/

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *)nini ;

      if( ngr->part != NULL ){
        for( ii=0 ; ii < ngr->part_num ; ii++ )
          NI_free_element_data( ngr->part[ii] ) ;     /* recursion */
      }
   }

   /*-- no other cases for data --*/

   return ;
}

/*-----------------------------------------------------------------------*/

static void NI_init_veclen( NI_element *nel , int veclen )
{
   if( veclen == 0 ){                      /* empty element */
     nel->vec_len      = 0 ;
     nel->vec_filled   = 0 ;
     nel->vec_rank     = 0 ;
     nel->vec_axis_len = NULL ;
   } else {                                /* element with data to */
     nel->vec_len         = veclen ;       /* come via NI_add_column */
     nel->vec_filled      = veclen ;
     nel->vec_rank        = 1 ;
     nel->vec_axis_len    = NI_malloc(int, sizeof(int)) ;
     nel->vec_axis_len[0] = veclen ;
   }
}

/*-----------------------------------------------------------------------*/
/*! Create a new data element.

    - name   = string name for header.
    - veclen = size (length) of vectors (ni_dimen attribute).
               - Vectors are added with NI_add_column().
               - Set this to zero for "empty" elements (those with only
                 headers, no data).

    Return is NULL if inputs are stupid or criminal or insane.
-------------------------------------------------------------------------*/

NI_element * NI_new_data_element( char *name , int veclen )
{
   NI_element *nel ;

   if( name == NULL || name[0] == '\0' || veclen < 0 ) return NULL ;

   nel = NI_malloc(NI_element, sizeof(NI_element) ) ;

   nel->type = NI_ELEMENT_TYPE ;  /* mark as being a data element */

   nel->outmode = -1 ;   /* 29 Mar 2005 */

   nel->name = NI_strdup(name) ;
   nel->attr_num = 0 ;
   nel->attr_lhs = nel->attr_rhs = NULL ;  /* no attributes yes */

   nel->vec_num = 0 ;                      /* no vectors yet */
   nel->vec_typ = NULL ;
   nel->vec     = NULL ;

   NI_init_veclen( nel , veclen ) ;  /* 19 Sep 2008 */

   nel->vec_axis_delta  = NULL ;
   nel->vec_axis_origin = NULL ;
   nel->vec_axis_unit   = NULL ;
   nel->vec_axis_label  = NULL ;

   return nel ;
}

/*-----------------------------------------------------------------------*/
/*! Add a vector (column) of data to a data element.

    - nel = data element to modify
    - typ = integer type code of data (e.g., NI_FLOAT)
    - arr = pointer to data values - must be an array of length veclen
            (same value as used in NI_new_data_element() call)
    - if arr is NULL, then will add a zero-filled column of the given
      type to the data element

    The data array is copied into the element.  If the element was
    specified with veclen=0, then this function will do nothing.
    Since this function has no return value, the only way to check for
    such an error is to see if nel->vec_num was incremented.  Or don't
    be so stupid as to make this error.
-------------------------------------------------------------------------*/

void NI_add_column( NI_element *nel , int typ , void *arr )
{
   int nn ;
   NI_rowtype *rt ;

   /* check for reasonable inputs */

   if( nel == NULL || nel->vec_len <= 0 )            return ;
   if( nel->type != NI_ELEMENT_TYPE )                return ;
   rt = NI_rowtype_find_code(typ) ; if( rt == NULL ) return ;

   /* get number of vectors currently in element */

   nn = nel->vec_num ;

   /* add 1 to the vec_typ array */

   nel->vec_typ     = NI_realloc( nel->vec_typ, int, sizeof(int)*(nn+1) ) ;
   nel->vec_typ[nn] = typ ;

   /* add 1 element to the vec array, and copy data into it */

   nel->vec = NI_realloc( nel->vec , void*, sizeof(void *)*(nn+1) ) ;
   if( arr != NULL )
     nel->vec[nn] = NI_copy_column( rt , nel->vec_len , arr ) ;
   else
     nel->vec[nn] = NI_malloc(void, rt->size * nel->vec_len ) ;

   /* add 1 to the count of vectors */

   nel->vec_num = nn+1 ;

   /* if element has "ni_type" attribute, adjust it   14 Jul 2006 [rickr] */
   if( NI_get_attribute(nel, "ni_type") )
      NI_set_ni_type_atr(nel) ;

   return ;
}

/*-------------------------------------------------------------------------*/
/*!
   Like add_column, but inserts the column at nel->vec[icol] rather than
   at the end.
   if icol < 0 || icol > nel->vec_num then icol = nel->vec_num
*/
void NI_insert_column( NI_element *nel , int typ , void *arr, int icol )
{
   int nn, ii ;
   NI_rowtype *rt ;

   /* check for reasonable inputs */

   if( nel == NULL || nel->vec_len <= 0 )            return ;
   if( nel->type != NI_ELEMENT_TYPE )                return ;
   rt = NI_rowtype_find_code(typ) ; if( rt == NULL ) return ;
   
   /* get number of vectors currently in element */
   nn = nel->vec_num ;
   
   if (icol > nn || icol < 0) icol = nn;
   
   /* call add column */
   NI_add_column(nel, typ, arr);
   
   /* check on success */
   if (nel->vec_num != nn+1) return ;  /* misere */
   nn = nel->vec_num ;  /* the new number of vectors */
   
   NI_move_column(nel, nn-1, icol);

   return ;
}

/*-------------------------------------------------------------------------*/
/*!
   move a column from index ibefore to iafter
   if ibefore (or iafter) is (< 0 || > nel->vec_num) then  
      ibefore = nel->vec_num-1
*/
void NI_move_column(NI_element *nel, int ibefore, int iafter)
{
   int nn, ii ;
   int typ_buf;
   void *col_buf;
   
   if (nel == NULL || nel->vec_len <= 0 )            return ;
   
   nn = nel->vec_num ;
   if (ibefore < 0 || ibefore >= nn) ibefore = nn-1;
   if (iafter < 0 || iafter >= nn) iafter = nn-1;
   
   /* nothing to see here? */
   if (ibefore == iafter) return;
   
   /* do the deed */
   /* store the initial values */
   typ_buf = nel->vec_typ[ibefore];
   col_buf = nel->vec[ibefore];
   /* shift */
   if (ibefore > iafter) {
      /* shift columns to left*/
      for (ii=ibefore; ii > iafter; --ii) {
         nel->vec[ii] = nel->vec[ii-1];
         nel->vec_typ[ii] = nel->vec_typ[ii-1];
      }
   } else {
      /* shift columns to right*/
      for (ii=ibefore; ii < iafter; ++ii) {
         nel->vec[ii] = nel->vec[ii+1];
         nel->vec_typ[ii] = nel->vec_typ[ii+1];
      }
   }
   
   /* insert the trouble maker back*/
   nel->vec[iafter] = col_buf;
   nel->vec_typ[iafter] = typ_buf;
   
   /* house keeping */
   /* if element has "ni_type" attribute, adjust it   14 Jul 2006 [rickr] */
   if( NI_get_attribute(nel, "ni_type") )
      NI_set_ni_type_atr(nel) ;

   return ;
}

/*-------------------------------------------------------------------------*/
/*!
   Do we really need to document this too?
   Removes column irm from nel. If irm < 0 or
   irm >= nel->vec_num irm = nel->vec_num -1 
*/
void NI_remove_column(NI_element *nel, int irm)
{
   int nn;
   
   if (nel == NULL || nel->vec_len <= 0 )            return ;
   
   if (!(nn = nel->vec_num)) return;

   if (irm < 0 || irm >= nn) irm = nn-1;
   
   /* move irm to last column */
   NI_move_column(nel, irm, -1);
   
   /* free the last column */
   NI_free_column( NI_rowtype_find_code(nel->vec_typ[nn-1]) ,
                           nel->vec_len , nel->vec[nn-1]             ) ;
   nel->vec[nn-1] = NULL; /* to be sure */
   
   /* decrease the number of columns */
   --nn; 
   nel->vec_num = nn; 
   
   /* get rid of extra space */
   nel->vec_typ = NI_realloc( nel->vec_typ, int, sizeof(int)*(nn) ) ;
   nel->vec = NI_realloc( nel->vec , void*, sizeof(void *)*(nn) ) ;

   /* if element has "ni_type" attribute, adjust it   14 Jul 2006 [rickr] */
   if( NI_get_attribute(nel, "ni_type") )
      NI_set_ni_type_atr(nel) ;

   
   return;
}
/*------------------------------------------------------------------------*/
/*! Change the length of all the columns in a data element.
     - If the columns are longer, they will be zero filled.
     - New values can be inserted later with NI_insert_value().
     - If the columns are shorter, data will be lost.
     - You can use this to convert an element from empty to non-empty
       by entering newlen > 0 when the original vec_len is 0.
     - But, you cannot use this to convert an element to empty from
       non-empty, by entering newlen == 0 when vec_len > 0!
--------------------------------------------------------------------------*/

void NI_alter_veclen( NI_element *nel , int newlen )
{
   int oldlen , ii ;
   NI_rowtype *rt ;
   char *pt ;

   if( nel    == NULL || nel->type != NI_ELEMENT_TYPE ) return ;
   if( newlen <= 0                                    ) return ;

   if( nel->vec_num == 0 ){                       /* if have no data yet */
     nel->vec_len = nel->vec_filled = newlen; return;
   }

   if( nel->vec_len == 0 ) NI_init_veclen( nel , newlen ) ;  /* 19 Sep 2008 */

   oldlen = nel->vec_len ; if( oldlen == newlen ) return ;

   for( ii=0 ; ii < nel->vec_num ; ii++ ){
     rt = NI_rowtype_find_code( nel->vec_typ[ii] ) ;
     nel->vec[ii] = NI_realloc( nel->vec[ii] , void , rt->size * newlen ) ;
     if( oldlen < newlen ){
       pt = ((char *)nel->vec[ii]) + (rt->size * oldlen) ; /* zero fill */
       memset( pt , 0 , (newlen-oldlen)*rt->size ) ;       /* new data! */
     }
   }

   nel->vec_len = nel->vec_filled = newlen ; return ;
}

/*------------------------------------------------------------------------*/
/*! As in NI_add_column(), but adding every stride-th element from arr.
    Thus, arr should be at least nel->vec_len * stride elements long.
--------------------------------------------------------------------------*/

void NI_add_column_stride( NI_element *nel, int typ, void *arr, int stride )
{
   int nn , ii ;
   NI_rowtype *rt ;
   char *idat ;

   /* check for reasonable inputs */

   if( nel == NULL || nel->vec_len <= 0 )            return ;
   if( nel->type != NI_ELEMENT_TYPE )                return ;
   rt = NI_rowtype_find_code(typ) ; if( rt == NULL ) return ;

   /* add an empty column */

   NI_add_column( nel , typ , NULL ) ;
   if( arr == NULL ) return ;          /* no input data ==> we're done */

   /* loop over inputs and put them in one at a time */

   nn   = nel->vec_num-1 ;
   idat = (char *) arr ;

   for( ii=0 ; ii < nel->vec_len ; ii++ )
     NI_insert_value( nel , ii , nn , idat + (ii*stride*rt->size) ) ;

   return ;
}

/*-------------------------------------------------------------------------*/
/*!
   See NI_insert_column for inspiration
*/
void NI_insert_column_stride( NI_element *nel, int typ, void *arr, int stride, int icol )
{
   int nn , ii ;
   NI_rowtype *rt ;
   char *idat ;

   /* check for reasonable inputs */

   if( nel == NULL || nel->vec_len <= 0 )            return ;
   if( nel->type != NI_ELEMENT_TYPE )                return ;
   rt = NI_rowtype_find_code(typ) ; if( rt == NULL ) return ;

   /* get number of vectors currently in element */
   nn = nel->vec_num ;

   if (icol > nn || icol < 0) icol = nn;
   
   /* call add column_stride */
   NI_add_column_stride(nel, typ, arr, stride);
   
   /* check on success */
   if (nel->vec_num != nn+1) return ;  /* misere */
   nn = nel->vec_num ;  /* the new number of vectors */
   
   NI_move_column(nel, nn-1, icol);

   return ;
}


/*------------------------------------------------------------------------*/
/*! ZSS; Fills an already created column with values up to vec_filled
         the values in arr are inserted into nel->vec[nn]
--------------------------------------------------------------------------*/

void NI_fill_column_stride( NI_element *nel, int typ,
                            void *arr, int nn, int stride )
{
   int  ii , nf;
   NI_rowtype *rt ;
   char *idat ;

   /* check for reasonable inputs */

   if( nel == NULL || nel->vec_len <= 0 )            return ;
   if( nel->type != NI_ELEMENT_TYPE )                return ;
   rt = NI_rowtype_find_code(typ) ; if( rt == NULL ) return ;

   /* check for NULL column or other similar errors*/

   if( arr == NULL )                                 return ;
   if( nel->vec[nn] == NULL )                        return ;
   if( nn < 0 || nn >= nel->vec_num )                return ;
   if( typ != nel->vec_typ[nn] )                     return ;

   /* loop over inputs and put them in */

   if( nel->vec_filled > 0 && nel->vec_filled <= nel->vec_len )
     nf = nel->vec_filled ;
   else
     nf = nel->vec_len ;

   idat = (char *) arr ;

   for( ii=0 ; ii < nf ; ii++ )
     NI_insert_value( nel , ii , nn , idat + (ii*stride*rt->size) ) ;

   return ;
}

/*------------------------------------------------------------------------*/
/*! Replace the row-th value in the col-th column of the data element.
     - dat is the pointer to the data values to copy into the element.
     - The column must have been created with NI_add_column() before
       calling this function!
     - NOTE WELL: When the column type is NI_STRING, it is a mistake
       to call this function with dat being a pointer to the C string
       to insert.  Instead, dat should be a pointer to the pointer to
       the C string.  For example:
        - char *me = "RWCox" ;
        - WRONG:  NI_insert_value ( nel, 3,5,  me ) ; [Seg Fault ensues]
        - RIGHT:  NI_insert_value ( nel, 3,5, &me ) ;
        - RIGHT:  NI_insert_string( nel, 3,5,  me ) ;
        - The last case illustrates the NI_insert_string() function,
          which can be used to simplify insertion into a column
          of Strings; that function is just a simple wrapper to call
          NI_insert_value() properly.
        - The reason the first example is WRONG is that dat is supposed
          to point to the data to be stored.  In the case of a String,
          the data is the pointer to the C string.
--------------------------------------------------------------------------*/

void NI_insert_value( NI_element *nel, int row, int col, void *dat )
{
   NI_rowtype *rt ;
   char *cdat , *idat=(char *)dat , *qpt ;
   int jj , kk ;

   /* check for reasonable inputs */

   if( nel == NULL || idat == NULL        ) return ;
   if( nel->type    != NI_ELEMENT_TYPE    ) return ;
   if( nel->vec_len <= 0                  ) return ;
   if( row < 0     || row >= nel->vec_len ) return ;
   if( col < 0     || col >= nel->vec_num ) return ;

   rt = NI_rowtype_find_code( nel->vec_typ[col] ) ;
   if( rt == NULL )                         return ;

   cdat = (char *) nel->vec[col] ;   /* points to column data */
   cdat = cdat + rt->size * row ;    /* points to data to alter */

   /* shallow copy of input data over data now present */

   memcpy( cdat , idat , rt->size ) ;

   /* copy any var dim arrays inside */

   if( ROWTYPE_is_varsize(rt) ){
     for( jj=0 ; jj < rt->part_num ; jj++ ){            /* loop over parts */

       if( rt->part_typ[jj] == NI_STRING ){               /* a string part */
         char **apt = (char **)(cdat+rt->part_off[jj]) ;   /* *apt => data */
         qpt = NI_strdup(*apt) ; *apt = qpt ;

       } else if( rt->part_dim[jj] >= 0 ){                /* var dim array */
         char **apt = (char **)(cdat+rt->part_off[jj]) ;   /* *apt => data */
         if( *apt != NULL ){
           kk  = ROWTYPE_part_dimen(rt,cdat,jj) * rt->part_rtp[jj]->size ;
           qpt = NI_malloc(char, kk) ; memcpy(qpt,*apt,kk) ; *apt = qpt ;
         }
       }
     }
   }

   return ;
}

/*------------------------------------------------------------------------*/

void NI_insert_string( NI_element *nel, int row, int col, char *str )
{
   if( nel == NULL || str == NULL         ) return ;
   if( nel->type   != NI_ELEMENT_TYPE     ) return ;
   if( row < 0     || row >= nel->vec_len ) return ;
   if( col < 0     || col >= nel->vec_num ) return ;
   if( nel->vec_typ[col] != NI_STRING     ) return ;

   NI_insert_value( nel , row,col , &str ); return ;
}

/*------------------------------------------------------------------------*/
/*! Remove an attribute, if it exists from a data or group element.
                                    ZSS Feb 09 
--------------------------------------------------------------------------*/
void NI_kill_attribute( void *nini , char *attname  )
{
   int nn , tt=NI_element_type(nini) ;

   if( tt < 0 || attname == NULL || attname[0] == '\0' ) return ;

   /* input is a data element */

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      /* see if name is already in element header */

      for( nn=0 ; nn < nel->attr_num ; nn++ )
         if( strcmp(nel->attr_lhs[nn],attname) == 0 ) break ;

      

      if( nn == nel->attr_num ){ /* not found, return */
        return;
      } else {
        NI_free(nel->attr_lhs[nn]) ;  /* free old attribute */
        NI_free(nel->attr_rhs[nn]) ;
        if ( nn < nel->attr_num-1 ) { /* move last attr to nn */
         nel->attr_lhs[nn] = nel->attr_lhs[nel->attr_num-1];
         nel->attr_lhs[nel->attr_num-1] = NULL; 
         nel->attr_rhs[nn] = nel->attr_rhs[nel->attr_num-1];
         nel->attr_rhs[nel->attr_num-1] = NULL;      
        }
        --nel->attr_num;
        /* reallocate */
        nel->attr_lhs = NI_realloc( nel->attr_lhs, 
                                    char*, sizeof(char *)*(nel->attr_num) );
        nel->attr_rhs = NI_realloc( nel->attr_rhs, 
                                    char*, sizeof(char *)*(nel->attr_num) ); 
      }

   /* input is a group element */

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;

      for( nn=0 ; nn < ngr->attr_num ; nn++ )
         if( strcmp(ngr->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == ngr->attr_num ){
        return;
      } else {
        NI_free(ngr->attr_lhs[nn]) ;
        NI_free(ngr->attr_rhs[nn]) ;
        if ( nn < ngr->attr_num-1 ) { /* move last attr to nn */
         ngr->attr_lhs[nn] = ngr->attr_lhs[ngr->attr_num-1];
         ngr->attr_lhs[ngr->attr_num-1] = NULL; 
         ngr->attr_rhs[nn] = ngr->attr_rhs[ngr->attr_num-1];
         ngr->attr_rhs[ngr->attr_num-1] = NULL;      
        }
        --ngr->attr_num;
        /* reallocate */
        ngr->attr_lhs = NI_realloc( ngr->attr_lhs, 
                                    char*, sizeof(char *)*(ngr->attr_num) );
        ngr->attr_rhs = NI_realloc( ngr->attr_rhs, 
                                    char*, sizeof(char *)*(ngr->attr_num) ); 
      }

   /* input is a processing instruction */

   } else if( tt == NI_PROCINS_TYPE ){
      NI_procins *npi = (NI_procins *) nini ;

      for( nn=0 ; nn < npi->attr_num ; nn++ )
        if( strcmp(npi->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == npi->attr_num ){
        return;
      } else {
        NI_free(npi->attr_lhs[nn]) ;
        NI_free(npi->attr_rhs[nn]) ;
        if ( nn < npi->attr_num-1 ) { /* move last attr to nn */
         npi->attr_lhs[nn] = npi->attr_lhs[npi->attr_num-1];
         npi->attr_lhs[npi->attr_num-1] = NULL; 
         npi->attr_rhs[nn] = npi->attr_rhs[npi->attr_num-1];
         npi->attr_rhs[npi->attr_num-1] = NULL;      
        }
        --npi->attr_num;
        /* reallocate */
        npi->attr_lhs = NI_realloc( npi->attr_lhs, 
                                    char*, sizeof(char *)*(npi->attr_num) );
        npi->attr_rhs = NI_realloc( npi->attr_rhs, 
                                    char*, sizeof(char *)*(npi->attr_num) ); 
      }

   }

   return ;
}

/*------------------------------------------------------------------------*/
/*! Add an attribute to a data or group element.
    If an attribute with the same attname already exists, then
    it will be replaced with this one.
--------------------------------------------------------------------------*/

void NI_set_attribute( void *nini , char *attname , char *attvalue )
{
   int nn , tt=NI_element_type(nini) ;

   if( tt < 0 || attname == NULL || attname[0] == '\0' ) return ;

   /* input is a data element */

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      /* see if name is already in element header */

      for( nn=0 ; nn < nel->attr_num ; nn++ )
         if( strcmp(nel->attr_lhs[nn],attname) == 0 ) break ;

      /* if not, then add a header attribute */

      if( nn == nel->attr_num ){
        nel->attr_lhs = NI_realloc( nel->attr_lhs, char*, sizeof(char *)*(nn+1) );
        nel->attr_rhs = NI_realloc( nel->attr_rhs, char*, sizeof(char *)*(nn+1) );
        nel->attr_num = nn+1 ;
      } else {
        NI_free(nel->attr_lhs[nn]) ;  /* free old attribute */
        NI_free(nel->attr_rhs[nn]) ;
      }

      nel->attr_lhs[nn] = NI_strdup(attname) ;
      nel->attr_rhs[nn] = NI_strdup(attvalue);

   /* input is a group element */

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;

      for( nn=0 ; nn < ngr->attr_num ; nn++ )
         if( strcmp(ngr->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == ngr->attr_num ){
        ngr->attr_lhs = NI_realloc( ngr->attr_lhs, char*, sizeof(char *)*(nn+1) );
        ngr->attr_rhs = NI_realloc( ngr->attr_rhs, char*, sizeof(char *)*(nn+1) );
        ngr->attr_num = nn+1 ;
      } else {
        NI_free(ngr->attr_lhs[nn]) ;
        NI_free(ngr->attr_rhs[nn]) ;
      }

      ngr->attr_lhs[nn] = NI_strdup(attname) ;
      ngr->attr_rhs[nn] = NI_strdup(attvalue);

   /* input is a processing instruction */

   } else if( tt == NI_PROCINS_TYPE ){
      NI_procins *npi = (NI_procins *) nini ;

      for( nn=0 ; nn < npi->attr_num ; nn++ )
        if( strcmp(npi->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == npi->attr_num ){
        npi->attr_lhs = NI_realloc( npi->attr_lhs, char*, sizeof(char *)*(nn+1) );
        npi->attr_rhs = NI_realloc( npi->attr_rhs, char*, sizeof(char *)*(nn+1) );
        npi->attr_num = nn+1 ;
      } else {
        NI_free(npi->attr_lhs[nn]) ;
        NI_free(npi->attr_rhs[nn]) ;
      }

      npi->attr_lhs[nn] = NI_strdup(attname) ;
      npi->attr_rhs[nn] = NI_strdup(attvalue);
   }

   return ;
}

/*-----------------------------------------------------------------------*/
/*! Get an attribute with the given LHS name.  Returns a pointer to the
    RHS field in the element if the attribute name is found; otherwise
    returns NULL.  If the LHS is found, but the RHS is NULL, returns
    a pointer to an empty C string ("\0").  Do not free() the result
    from this function, since it points to the internal field
    of the element!
-------------------------------------------------------------------------*/

char * NI_get_attribute( void *nini , char *attname )
{
   int nn , tt=NI_element_type(nini) ;
   static char *zorkon = "\0" ;

   if( tt < 0 || attname == NULL || attname[0] == '\0' ) return NULL ;

   /* input is a data element */

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      for( nn=0 ; nn < nel->attr_num ; nn++ )
         if( strcmp(nel->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == nel->attr_num ) return NULL ;

      if( nel->attr_rhs[nn] == NULL ) return zorkon ;

      return nel->attr_rhs[nn] ;

   /* input is a group element */

   } else if( tt == NI_GROUP_TYPE ){
      NI_group *ngr = (NI_group *) nini ;

      for( nn=0 ; nn < ngr->attr_num ; nn++ )
         if( strcmp(ngr->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == ngr->attr_num ) return NULL ;

      if( ngr->attr_rhs[nn] == NULL ) return zorkon ;

      return ngr->attr_rhs[nn] ;

   /* input is a processing instruction */

   } else if( tt == NI_PROCINS_TYPE ){
      NI_procins *npi = (NI_procins *) nini ;

      for( nn=0 ; nn < npi->attr_num ; nn++ )
        if( strcmp(npi->attr_lhs[nn],attname) == 0 ) break ;

      if( nn == npi->attr_num ) return NULL ;

      if( npi->attr_rhs[nn] == NULL ) return zorkon ;

      return npi->attr_rhs[nn] ;
   }

   return NULL ; /* should never be reached */
}

/*-----------------------------------------------------------------------*/
/*! Set the dimen attribute for a data element.
-------------------------------------------------------------------------*/

void NI_set_dimen( NI_element *nel , int rank , int *nd )
{
   int ii , ntot ;

   if( nel == NULL || nel->type != NI_ELEMENT_TYPE ||
       rank < 1    || nd == NULL                     ) return ; /* bad */

   for( ntot=1,ii=0 ; ii < rank ; ii++ ){
      if( nd[ii] <= 0 ) return ;                                /* bad */
      ntot *= nd[ii] ;
   }
   if( ntot != nel->vec_len ) return ;                          /* bad */

   nel->vec_rank = rank ;
   nel->vec_axis_len = NI_realloc( nel->vec_axis_len, int, sizeof(int)*rank ) ;
   memcpy( nel->vec_axis_len , nd , sizeof(int)*rank ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Set the delta attribute for a data element.
    Do not call this function until NI_set_dimen() has been called,
    unless there is only 1 dimension (which is the default).
-------------------------------------------------------------------------*/

void NI_set_delta( NI_element *nel , float *del )
{
   if( nel == NULL       || nel->type != NI_ELEMENT_TYPE ||
       nel->vec_rank < 1 || del == NULL                    ) return ;

   nel->vec_axis_delta = NI_realloc( nel->vec_axis_delta , float,
                                     nel->vec_rank * sizeof(float) ) ;
   memcpy( nel->vec_axis_delta , del , nel->vec_rank * sizeof(float) ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Set the origin attribute for a data element.
    Do not call this function until NI_set_dimen() has been called,
    unless there is only 1 dimension (which is the default).
-------------------------------------------------------------------------*/

void NI_set_origin( NI_element *nel , float *org )
{
   if( nel == NULL       || nel->type != NI_ELEMENT_TYPE ||
       nel->vec_rank < 1 || org == NULL                    ) return ;

   nel->vec_axis_origin = NI_realloc( nel->vec_axis_origin , float,
                                      nel->vec_rank * sizeof(float) ) ;
   memcpy( nel->vec_axis_origin , org , nel->vec_rank * sizeof(float) ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Set the units attribute for a data element.
    Do not call this function until NI_set_dimen() has been called,
    unless there is only 1 dimension (which is the default).
-------------------------------------------------------------------------*/

void NI_set_units( NI_element *nel , char **units )
{
   int ii ;

   if( nel == NULL       || nel->type != NI_ELEMENT_TYPE ||
       nel->vec_rank < 1 || units == NULL                  ) return ;

   nel->vec_axis_unit = NI_realloc( nel->vec_axis_unit , char*,
                                    nel->vec_rank * sizeof(char *) ) ;
   for( ii=0 ; ii < nel->vec_rank ; ii++ )
      nel->vec_axis_unit[ii] = NI_strdup( units[ii] ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Set the axes attribute for a data element.
    Do not call this function until NI_set_dimen() has been called,
    unless there is only 1 dimension (which is the default).
-------------------------------------------------------------------------*/

void NI_set_axes( NI_element *nel , char **ax )
{
   int ii ;

   if( nel == NULL       || nel->type != NI_ELEMENT_TYPE ||
       nel->vec_rank < 1 || ax == NULL                     ) return ;

   nel->vec_axis_label = NI_realloc( nel->vec_axis_label , char*,
                                     nel->vec_rank * sizeof(char *) ) ;
   for( ii=0 ; ii < nel->vec_rank ; ii++ )
      nel->vec_axis_label[ii] = NI_strdup( ax[ii] ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Create a new processing instruction with a given 'target' name.
-------------------------------------------------------------------------*/

NI_procins * NI_new_processing_instruction( char *name )
{
   NI_procins *npi ;

   if( name == NULL || name[0] == '\0' ) return NULL ;

   npi = NI_malloc(NI_procins,sizeof(NI_procins)) ;

   npi->type = NI_PROCINS_TYPE ;
   npi->name = NI_strdup(name) ;

   npi->attr_num = 0 ;
   npi->attr_lhs = npi->attr_rhs = NULL ;

   return npi ;
}

/*-----------------------------------------------------------------------*/
/*! Create a new group element.
-------------------------------------------------------------------------*/

NI_group * NI_new_group_element(void)
{
   NI_group *ngr ;

   ngr = NI_malloc(NI_group, sizeof(NI_group) ) ;

   ngr->type = NI_GROUP_TYPE ;

   ngr->outmode = -1 ;   /* 29 Mar 2005 */

   ngr->attr_num = 0 ;
   ngr->attr_lhs = ngr->attr_rhs = NULL ;

   ngr->part_num = 0 ;
   ngr->part_typ = NULL ;
   ngr->part     = NULL ;
   ngr->name     = NULL ;  /* 03 Jun 2002 */

   return ngr ;
}

/*-----------------------------------------------------------------------*/
/*! Add an element to a group element.
-------------------------------------------------------------------------*/

void NI_add_to_group( NI_group *ngr , void *nini )
{
   int nn , tt=NI_element_type(nini) ;

   if( ngr == NULL || ngr->type != NI_GROUP_TYPE || tt < 0 ) return ;

   nn = ngr->part_num ;

   ngr->part_typ     = NI_realloc( ngr->part_typ , int, sizeof(int)*(nn+1) ) ;
   ngr->part_typ[nn] = tt ;
   ngr->part         = NI_realloc( ngr->part , void*, sizeof(void *)*(nn+1) );
   ngr->part[nn]     = nini ;
   ngr->part_num     = nn+1 ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Remove an element from a group.  Does NOT delete the element;
    that is the caller's responsibility, if desired.
-------------------------------------------------------------------------*/

void NI_remove_from_group( NI_group *ngr , void *nini )  /* 16 Apr 2005 */
{
   int ii , nn , jj ;

   if( ngr == NULL || ngr->type != NI_GROUP_TYPE || nini == NULL ) return ;

   nn = ngr->part_num ;
   for( ii=0 ; ii < nn ; ii++ )       /* search for part */
     if( nini == ngr->part[ii] ) break ;
   if( ii == nn ) return ;            /* not found */

   for( jj=ii+1 ; jj < nn ; jj++ ){   /* move parts above down */
     ngr->part_typ[jj-1] = ngr->part_typ[jj] ;
     ngr->part    [jj-1] = ngr->part    [jj] ;
   }
   ngr->part[nn-1] = NULL ;    /* NULL-ify last part to be safe */
   ngr->part_num -- ;          /* reduce part count */
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Rename a group element from the default - 03 Jun 2002.
-------------------------------------------------------------------------*/

void NI_rename_group( NI_group *ngr , char *nam )
{
   if( ngr == NULL || ngr->type != NI_GROUP_TYPE ) return ;
   NI_free( ngr->name ) ;
   ngr->name = NI_strdup(nam) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Return a list of all elements in a group that have a given name.
      - This is a 'shallow' search: if the group itself contains
        groups, these sub-groups are not searched.
      - Return value of function is number of elements found (might be 0).
      - If something is found, then *nipt is an array of 'void *', each
        of which points to a matching element.
      - The returned elements might be group or data elements.
      - Sample usage:
          - int n,i ; void **nelar ;
          - n = NI_search_group_shallow( ngr , "fred" , &nelar ) ;
          - for( i=0 ; i < n ; i++ ) do_something( nelar[ii] ) ;
          - if( n > 0 ) NI_free(nelar) ;
-------------------------------------------------------------------------*/

int NI_search_group_shallow( NI_group *ngr , char *enam , void ***nipt )
{
   void **nelar=NULL , *nini ;
   int ii , nn=0 ;
   char *nm ;

   if( ngr  == NULL || ngr->type != NI_GROUP_TYPE    ) return 0 ;
   if( enam == NULL || *enam == '\0' || nipt == NULL ) return 0 ;
   if( ngr->part_num == 0                            ) return 0 ;

   for( ii=0 ; ii < ngr->part_num ; ii++ ){
     nini = ngr->part[ii] ;
     nm   = NI_element_name( nini ) ;
     if( nm != NULL && strcmp(nm,enam) == 0 ){
                                  /* added *size  11 Jul 2006 [rickr] */
       nelar = (void **) NI_realloc(nelar,void*,(nn+1)*sizeof(void *)) ;
       nelar[nn++] = nini ;
     }
   }

   if( nn > 0 ) *nipt = nelar ;
   return nn ;
}

/*-----------------------------------------------------------------------*/
/*! Return a list of all elements in a group that have a given name.
      - This is a 'deep' search: if the group itself contains
        groups, these sub-groups are searched, etc.
      - If a group element has the name 'enam' AND a data element within
        that group has the name 'enam' as well, they will BOTH be returned
        in this list.
      - Return value of function is number of elements found (might be 0).
      - If something is found, then *nipt is an array of 'void *', each
        of which points to a matching element.
      - The returned elements might be group or data elements.
      - Sample usage:
          - int n,i ; void **nelar ;
          - n = NI_search_group_shallow( ngr , "fred" , &nelar ) ;
          - for( i=0 ; i < n ; i++ ) do_something( nelar[ii] ) ;
          - if( n > 0 ) NI_free(nelar) ;
-------------------------------------------------------------------------*/

int NI_search_group_deep( NI_group *ngr , char *enam , void ***nipt )
{
   void **nelar=NULL , *nini ;
   int ii , nn=0 ;
   char *nm ;

   if( ngr  == NULL || ngr->type != NI_GROUP_TYPE    ) return 0 ;
   if( enam == NULL || *enam == '\0' || nipt == NULL ) return 0 ;
   if( ngr->part_num == 0                            ) return 0 ;

   for( ii=0 ; ii < ngr->part_num ; ii++ ){
     nini = ngr->part[ii] ;
     nm   = NI_element_name( nini ) ;
     if( nm != NULL && strcmp(nm,enam) == 0 ){
       nelar = (void **) NI_realloc(nelar,void*,(nn+1)*sizeof(void *)) ;
       nelar[nn++] = nini ;
     }
     if( NI_element_type(nini) == NI_GROUP_TYPE ){  /* recursion */
       int nsub , jj ; void **esub ;
       nsub = NI_search_group_deep( nini , enam , &esub ) ;
       if( nsub > 0 ){
         nelar = (void **) NI_realloc(nelar,void*,(nn+nsub)*sizeof(void *)) ;
         for( jj=0 ; jj < nsub ; jj++ ) nelar[nn++] = esub[jj] ;
         NI_free(esub) ;
       }
     }
   }

   if( nn > 0 ) *nipt = nelar ;
   return nn ;
}

/*-----------------------------------------------------------------------*/
/*! add a ni_type attribute to the element            14 Jul 2006 [rickr]
    (based on NI_write_element())
-------------------------------------------------------------------------*/
void NI_set_ni_type_atr( NI_element * nel )
{
   char * buf ;  /* alloc in blocks of ~1K (bob noted names up to 255 chars) */
   char * posn ;
   int    ii, prev, count=0 ;
   int    req_len, total_len=1024 ;

   if( ! nel || nel->vec_num <= 0 ) return ;

   /* make enough space for a list of buffers, and init. to empty */
   /* -- NI_rowtype_define uses 255 as a max rowtype name length  */
   buf = (char *)NI_malloc(char, total_len * sizeof(char)) ;
   buf[0] = '\0' ;

   for( prev=-1, ii=0; ii < nel->vec_num; ii++ ){
      if( nel->vec_typ[ii] != prev ){   /* not the previous type */
         if( prev >= 0 ){               /* apply previous type now */
            posn = buf + strlen(buf);
            if( count > 1 ) sprintf(posn, "%d*%s,", count, NI_type_name(prev));
            else            sprintf(posn, "%s,",           NI_type_name(prev));
         }
         prev = nel->vec_typ[ii] ;   /* save new type code */
         count = 1 ;                 /* have 1 such type   */

         /* make sure there is enough space for the new code        */
         /* (old, new, and space for "5280*")   17 Jul 2006 [rickr] */
         req_len = strlen(buf) + strlen(NI_type_name(prev)) + 10 ;
         if( total_len < req_len )
            buf = (char *)NI_realloc(buf, char, (req_len+1024) * sizeof(char)) ;

      } else {                       /* same as previous type  */
         count++ ;                   /* so increment its count */
      }
   }

   /* now write the last type found */
   posn = buf + strlen(buf) ;
   if( count > 1 ) sprintf(posn, "%d*%s", count, NI_type_name(prev));
   else            sprintf(posn, "%s",           NI_type_name(prev));

   /* now add the string as an attribute, and nuke the old string */
   NI_set_attribute(nel, "ni_type", buf) ;

   NI_free(buf) ;

   return ;
}

