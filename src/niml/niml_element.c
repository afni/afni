#include "niml_private.h"

/*-----------------------------------------------------------------------*/
/*! Construct an empty data element from a header.

    The data vectors will have space allocated, but they will be
    filled with all zero bytes.  If the header was "empty" (ended in
    "/>"), then no vectors will be allocated, and nel->vec_num=0.
    This function is used by NI_read_element() to create the
    data element after the header has been parsed.
-------------------------------------------------------------------------*/

NI_element * make_empty_data_element( header_stuff *hs )
{
   NI_element *nel ;
   int ii , qq ;

   if( hs == NULL || hs->name == NULL ) return NULL ;

#ifdef NIML_DEBUG
NI_dpr("ENTER make_empty_data_element\n") ;
#endif

   nel = NI_malloc( sizeof(NI_element) ) ;

   nel->type = NI_ELEMENT_TYPE ;

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

     /* ni_type attribute */

     ii = string_index( "ni_type" , nel->attr_num , nel->attr_lhs ) ;

     if( ii >= 0 && nel->attr_rhs[ii] != NULL ){
       int_array *iar = decode_type_string( nel->attr_rhs[ii] ) ;
       if( iar != NULL ){
         nel->vec_num = iar->num ;  /* number of vectors */
         nel->vec_typ = iar->ar ;   /* vector types */
         NI_free(iar) ;             /* just the shell of the struct */
       }
     }

     /* ni_dimen attribute */

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
        }
     }

     /* if we had ni_dimen, also use ni_delta */

     ii = string_index( "ni_delta" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_len > 0 ){
        str_array *sar = decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_delta = NI_malloc(sizeof(float)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             sscanf( sar->str[pp] , "%f" , nel->vec_axis_delta+pp ) ;
           for( pp=0 ; pp < ns ; pp++ )
             NI_free( sar->str[pp] );
           NI_free(sar->str) ; NI_free(sar) ;
        }
     }

     /* if we had ni_dimen, also use ni_origin */

     ii = string_index( "ni_origin" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_len > 0 ){
        str_array *sar = decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_origin = NI_malloc(sizeof(float)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             sscanf( sar->str[pp] , "%f" , nel->vec_axis_origin+pp ) ;
           for( pp=0 ; pp < ns ; pp++ )
             NI_free( sar->str[pp] );
           NI_free(sar->str) ; NI_free(sar) ;
        }
     }

     /* if we had ni_dimen, also use ni_units */

     ii = string_index( "ni_units" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_len > 0 ){
        str_array *sar = decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_unit = NI_malloc(sizeof(char *)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             nel->vec_axis_unit[pp] = NI_strdup(sar->str[pp]) ;
           for( pp=0 ; pp < ns ; pp++ )
             NI_free( sar->str[pp] );
           NI_free(sar->str) ; NI_free(sar) ;
        }
     }

     /* if we had ni_dimen, also use ni_axes */

     ii = string_index( "ni_axes" , nel->attr_num , nel->attr_lhs ) ;
     if( ii >= 0 && nel->vec_len > 0 ){
        str_array *sar = decode_string_list( nel->attr_rhs[ii] , NULL ) ;
        if( sar != NULL && sar->num > 0 ){
           int ns=sar->num , nd=nel->vec_rank , pp ;
           nel->vec_axis_label = NI_malloc(sizeof(char *)*nd) ;
           if( nd > ns ) nd = ns ;
           for( pp=0 ; pp < nd ; pp++ )
             nel->vec_axis_label[pp] = NI_strdup(sar->str[pp]) ;
           for( pp=0 ; pp < ns ; pp++ )
             NI_free( sar->str[pp] );
           NI_free(sar->str) ; NI_free(sar) ;
        }
     }

     /* supply vector parameters if none was given */

     if( nel->vec_len == 0 ){                    /* default dimensions */
        nel->vec_len         = 1 ;
        nel->vec_rank        = 1 ;
        nel->vec_axis_len    = NI_malloc(sizeof(int)) ;
        nel->vec_axis_len[0] = 1 ;
     }

     if( nel->vec_num == 0 ){                    /* default type */
        nel->vec_num    = 1 ;
        nel->vec_typ    = NI_malloc(sizeof(int)) ;
        nel->vec_typ[0] = NI_BYTE ;
     }

     /* now allocate space for vectors defined above */

     nel->vec = NI_malloc( sizeof(void *)*nel->vec_num ) ;

     for( ii=0 ; ii < nel->vec_num ; ii++ )
       nel->vec[ii] = NI_malloc(NI_type_size(nel->vec_typ[ii])*nel->vec_len) ;

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
   int ii , qq ;

   if( hs == NULL || hs->name == NULL ) return NULL ;

   ngr = NI_malloc( sizeof(NI_group) ) ;

   ngr->type = NI_GROUP_TYPE ;

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
   ngr->name     = NULL ;  /* 03 Jun 2002 */

   return ngr ;
}

/*-------------------------------------------------------------------------*/
/*! Byte size of a given integer type code.
    Modified 13 Feb 2003 to use the new rowtype stuff.
---------------------------------------------------------------------------*/

int NI_type_size( int tval )
{
#if 0                   /* OLD code, removed 13 Feb 2003 */
   switch( tval ){
      case NI_BYTE:     return sizeof(byte)    ;
      case NI_SHORT:    return sizeof(short)   ;
      case NI_INT:      return sizeof(int)     ;
      case NI_FLOAT:    return sizeof(float)   ;
      case NI_DOUBLE:   return sizeof(double)  ;
      case NI_COMPLEX:  return sizeof(complex) ;
      case NI_RGB:      return sizeof(rgb)     ;
      case NI_RGBA:     return sizeof(rgba)    ;
      case NI_STRING:   return sizeof(char *)  ;
   }
   return 0 ;
#else
   int ii = NI_rowtype_code_to_size( tval ) ;
   return (ii > 0) ? ii : 0 ;
#endif
}

#ifndef USE_NEW_IOFUN
/*----------------------------------------------------------------------*/
/*! Return the size in bytes of one row in a data element.
    Note that this will only included the "fixed" size of the
    data, not any space for var dim arrays.
------------------------------------------------------------------------*/

int NI_element_rowsize( NI_element *nel )
{
   int ii , nb ;

   if( nel == NULL                  ||
       nel->type != NI_ELEMENT_TYPE ||
       nel->vec_num < 1             ||
       nel->vec_typ == NULL           ) return 0 ;  /* bad input */

   for( ii=nb=0 ; ii < nel->vec_num ; ii++ )
      nb += NI_type_size( nel->vec_typ[ii] ) ;

   return nb ;
}

/*----------------------------------------------------------------------*/
/*! Return the size of all the rows in a data element.
    Note that this will only included the "fixed" size of the
    data, not any space for var dim arrays.
------------------------------------------------------------------------*/

int NI_element_allsize( NI_element *nel )
{
   if( nel == NULL                  ||
       nel->type != NI_ELEMENT_TYPE ||
       nel->vec_num < 1             ||
       nel->vec_len < 1             ||
       nel->vec_typ == NULL           ) return 0 ;  /* bad input */

   return (nel->vec_len * NI_element_rowsize(nel)) ;
}
#endif  /* USE_NEW_IOFUN */

/*************************************************************************/
/********** Functions to create NIML data and group elements *************/
/*************************************************************************/

/*-----------------------------------------------------------------------*/
/*! Return the type of something that points to a NI element.

    - The input should be a pointer to a NI_element or a NI_group.
    - The return value is NI_ELEMENT_TYPE, NI_GROUP_TYPE, or -1.
-------------------------------------------------------------------------*/

int NI_element_type( void *nini )
{
   NI_element *nel = (NI_element *) nini ;
   NI_group   *ngr = (NI_group *)   nini ;

   if( nini == NULL ) return -1 ;

   if( nel->type == NI_ELEMENT_TYPE ) return NI_ELEMENT_TYPE ;
   if( ngr->type == NI_GROUP_TYPE   ) return NI_GROUP_TYPE   ;

   return -1 ;
}

/*-----------------------------------------------------------------------*/
/*! Expunge a data or group element and its contents from the universe.
-------------------------------------------------------------------------*/

void NI_free_element( void *nini )
{
   int ii , tt=NI_element_type(nini) , jj ;

   if( tt < 0 ) return ; /* bad input */

   /*-- erase contents of data element --*/

   if( tt == NI_ELEMENT_TYPE ){
      NI_element *nel = (NI_element *) nini ;

      NI_free(nel->name) ;
      for( ii=0 ; ii < nel->attr_num ; ii++ ){
         NI_free( nel->attr_lhs[ii] ) ;
         NI_free( nel->attr_rhs[ii] ) ;
      }
      NI_free( nel->attr_lhs ) ;
      NI_free( nel->attr_rhs ) ;

      /* 14 Feb 2003: NI_free_column() will also free var dim arrays */

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
      NI_group *ngr = (NI_group *) nini ;

      for( ii=0 ; ii < ngr->attr_num ; ii++ ){
         NI_free( ngr->attr_lhs[ii] ) ;
         NI_free( ngr->attr_rhs[ii] ) ;
      }
      NI_free( ngr->attr_lhs ) ;
      NI_free( ngr->attr_rhs ) ;

      for( ii=0 ; ii < ngr->part_num ; ii++ )
         NI_free_element( ngr->part[ii] ) ;     /* recursion */

      NI_free( ngr->part_typ ) ;
      NI_free( ngr->part ) ;
      NI_free( ngr->name ) ;    /* 03 Jun 2002 */
      NI_free( ngr ) ;
   }

   return ;
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

   nel = NI_malloc( sizeof(NI_element) ) ;

   nel->type = NI_ELEMENT_TYPE ;  /* mark as being a data element */

   nel->name = NI_strdup(name) ;
   nel->attr_num = 0 ;
   nel->attr_lhs = nel->attr_rhs = NULL ;  /* no attributes yes */

   nel->vec_num = 0 ;                      /* no vectors yet */
   nel->vec_typ = NULL ;
   nel->vec     = NULL ;

   if( veclen == 0 ){                      /* empty element */
     nel->vec_len      = 0 ;
     nel->vec_filled   = 0 ;
     nel->vec_rank     = 0 ;
     nel->vec_axis_len = NULL ;
   } else {                                /* element with data to */
     nel->vec_len         = veclen ;       /* come via NI_add_column */
     nel->vec_filled      = veclen ;
     nel->vec_rank        = 1 ;
     nel->vec_axis_len    = NI_malloc(sizeof(int)) ;
     nel->vec_axis_len[0] = veclen ;
   }

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
   int nn , ii ;
   NI_rowtype *rt ;

   /* check for reasonable inputs */

   if( nel == NULL || nel->vec_len <= 0 ) return ;

   if( nel->type != NI_ELEMENT_TYPE ) return ;

   rt = NI_rowtype_find_code(typ) ; if( rt == NULL ) return ;

   /* get number of vectors currently in element */

   nn = nel->vec_num ;

   /* add 1 to the vec_typ array */

   nel->vec_typ     = NI_realloc( nel->vec_typ , sizeof(int)*(nn+1) ) ;
   nel->vec_typ[nn] = typ ;

   /* add 1 element to the vec array, and copy data into it */

   nel->vec = NI_realloc( nel->vec , sizeof(void *)*(nn+1) ) ;
   if( arr != NULL )
     nel->vec[nn] = NI_copy_column( rt , nel->vec_len , arr ) ;
   else
     nel->vec[nn] = NI_malloc( rt->size * nel->vec_len ) ;

   /* add 1 to the count of vectors */

   nel->vec_num = nn+1 ;
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
        nel->attr_lhs = NI_realloc( nel->attr_lhs , sizeof(char *)*(nn+1) ) ;
        nel->attr_rhs = NI_realloc( nel->attr_rhs , sizeof(char *)*(nn+1) ) ;
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
        ngr->attr_lhs = NI_realloc( ngr->attr_lhs , sizeof(char *)*(nn+1) ) ;
        ngr->attr_rhs = NI_realloc( ngr->attr_rhs , sizeof(char *)*(nn+1) ) ;
        ngr->attr_num = nn+1 ;
      } else {
        NI_free(ngr->attr_lhs[nn]) ;
        NI_free(ngr->attr_rhs[nn]) ;
      }

      ngr->attr_lhs[nn] = NI_strdup(attname) ;
      ngr->attr_rhs[nn] = NI_strdup(attvalue);
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
   nel->vec_axis_len = NI_realloc( nel->vec_axis_len, sizeof(int)*rank ) ;
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

   nel->vec_axis_delta = NI_realloc( nel->vec_axis_delta ,
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

   nel->vec_axis_origin = NI_realloc( nel->vec_axis_origin ,
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

   nel->vec_axis_unit = NI_realloc( nel->vec_axis_unit ,
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

   nel->vec_axis_label = NI_realloc( nel->vec_axis_label ,
                                     nel->vec_rank * sizeof(char *) ) ;
   for( ii=0 ; ii < nel->vec_rank ; ii++ )
      nel->vec_axis_label[ii] = NI_strdup( ax[ii] ) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Create a new group element.
-------------------------------------------------------------------------*/

NI_group * NI_new_group_element(void)
{
   NI_group *ngr ;

   ngr = NI_malloc( sizeof(NI_group) ) ;

   ngr->type = NI_GROUP_TYPE ;

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

   ngr->part_typ     = NI_realloc( ngr->part_typ , sizeof(int)*(nn+1) ) ;
   ngr->part_typ[nn] = tt ;
   ngr->part         = NI_realloc( ngr->part , sizeof(void *)*(nn+1) ) ;
   ngr->part[nn]     = nini ;
   ngr->part_num     = nn+1 ;
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

#ifndef USE_NEW_IOFUN
/*-----------------------------------------------------------------------*/
/*! Fill one row of an element with some data bytes (numeric only).
-------------------------------------------------------------------------*/

void NI_fill_vector_row( NI_element *nel , int row , char *buf )
{
   int bpos=0 , col ;
   char tmp[16] ;  /* We copy into here from buf, then into the vector. */
                   /* The reason for this is to ensure proper byte     */
                   /* alignment for the assignment into the vector.   */

   /* check inputs for stupidity */

   if( nel->type != NI_ELEMENT_TYPE ||
       row       <  0               ||
       row       >= nel->vec_len    || buf == NULL ) return ;

   /* loop over columns, taking the requisite number of
      bytes from buf and stuffing them into the vectors */

   for( col=0 ; col < nel->vec_num ; col++ ){
     switch( nel->vec_typ[col] ){
       default:                     /* unimplemented types */
       break ;                      /* (STRING and LINE)  */

       case NI_BYTE:{
         byte *vpt = (byte *) nel->vec[col] ;
         byte *bpt = (byte *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(byte)) ;
         vpt[row]  = *bpt ; bpos += sizeof(byte) ;
       }
       break ;

       case NI_SHORT:{
         short *vpt = (short *) nel->vec[col] ;
         short *bpt = (short *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(short)) ;
         vpt[row]  = *bpt ; bpos += sizeof(short) ;
       }
       break ;

       case NI_INT:{
         int *vpt = (int *) nel->vec[col] ;
         int *bpt = (int *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(int)) ;
         vpt[row]  = *bpt ; bpos += sizeof(int) ;
       }
       break ;

       case NI_FLOAT:{
         float *vpt = (float *) nel->vec[col] ;
         float *bpt = (float *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(float)) ;
         vpt[row]  = *bpt ; bpos += sizeof(float) ;
       }
       break ;

       case NI_DOUBLE:{
         double *vpt = (double *) nel->vec[col] ;
         double *bpt = (double *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(double)) ;
         vpt[row]  = *bpt ; bpos += sizeof(double) ;
       }
       break ;

       case NI_COMPLEX:{
         complex *vpt = (complex *) nel->vec[col] ;
         complex *bpt = (complex *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(complex)) ;
         vpt[row]  = *bpt ; bpos += sizeof(complex) ;
       }
       break ;

       case NI_RGB:{
         rgb *vpt = (rgb *) nel->vec[col] ;
         rgb *bpt = (rgb *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(rgb)) ;
         vpt[row]  = *bpt ; bpos += sizeof(rgb) ;
       }
       break ;

       case NI_RGBA:{
         rgba *vpt = (rgba *) nel->vec[col] ;
         rgba *bpt = (rgba *) tmp ;
         memcpy(tmp,buf+bpos,sizeof(rgba)) ;
         vpt[row]  = *bpt ; bpos += sizeof(rgba) ;
       }
       break ;
     }
   }
   return ;
}
#endif  /* USE_NEW_IOFUN */

#ifndef USE_NEW_IOFUN
/*------------------------------------------------------------------*/
/*! Swap bytes for an array of type code tval.
--------------------------------------------------------------------*/

void NI_swap_vector( int tval , int nvec , void *vec )
{
   /* check inputs for stupidity */

   if( nvec <= 0 || vec == NULL ) return ;

   switch( tval ){

      default:  break ;   /* nothing to do */

      case NI_SHORT:    NI_swap2( nvec , vec ) ; break ;

      case NI_INT:
      case NI_FLOAT:    NI_swap4( nvec , vec ) ; break ;

      case NI_DOUBLE:   NI_swap8( nvec , vec ) ; break ;

      case NI_COMPLEX:  NI_swap4( 2*nvec, vec) ; break ;
   }
   return ;
}
#endif  /* USE_NEW_IOFUN */
