#include "niml_private.h"

/***************************************************************************/
/***** Stuff for defining rowtypes from strings.  RWCox - 09 Dec 2002. *****/
/***************************************************************************/

/*-------------------------------------------------------------------------*/

/* These typedefs are used to find the byte alignment
   the compiler imposes on the 'basic' types supported by NIML. */

typedef struct { char a; byte    b; } qvzw_byte    ;
typedef struct { char a; short   b; } qvzw_short   ;
typedef struct { char a; int     b; } qvzw_int     ;
typedef struct { char a; float   b; } qvzw_float   ;
typedef struct { char a; double  b; } qvzw_double  ;
typedef struct { char a; complex b; } qvzw_complex ;
typedef struct { char a; rgb     b; } qvzw_rgb     ;
typedef struct { char a; rgba    b; } qvzw_rgba    ;

/* Arrays to hold the alignments, sizes, and names of the basic types. */

static int   type_alignment[NI_NUM_BASIC_TYPES] ;
static int   type_size     [NI_NUM_BASIC_TYPES] ;
static char *type_name     [NI_NUM_BASIC_TYPES] = {
  "byte" , "short" , "int" , "float" , "double" , "complex" , "rgb" , "rgba"
} ;

/* these are used to find/save the alignment and size of pointers */

typedef struct { char a; void   *b; } qvzw_pointer ;
static int pointer_alignment ;
static int pointer_size ;

/*! The Htable of user-defined rowtypes, indexed by type name. */

static Htable *rowtype_table = NULL ;

/*! The array of user-defined rowtypes, indexed by type code. */

static NI_rowtype **rowtype_array = NULL ;
static int         rowtype_num    = 0    ;

/*! Special rowtype code for pointer type. */

#define ROWTYPE_POINTER     666

static NI_rowtype *pointer_rowtype = NULL ;

/*! Rowtype code for first user-defined type. */

#define ROWTYPE_OFFSET     1001

/*! Used to set the code for each new user-defined type. */

#define ROWTYPE_BASE_CODE (ROWTYPE_OFFSET-NI_NUM_BASIC_TYPES-1)

/*! Register a rowtype into the table and array. */

#define ROWTYPE_register(rr)                               \
 do{ int nn ;                                              \
     if( rowtype_table == NULL ) setup_basic_types() ;     \
     addto_Htable( (rr)->name , (rr) , rowtype_table ) ;   \
     nn = rowtype_num + 1 ;                                \
     rowtype_array = NI_realloc( rowtype_array ,           \
                                sizeof(NI_rowtype *)*nn ); \
     rowtype_array[nn-1] = rr ;  rowtype_num = nn ;        \
 } while(0)

/*! Debug flag for rowtype stuff. */

static int ROWTYPE_debug = 0 ;

void NI_rowtype_debug( int n ){ ROWTYPE_debug = n ; }

/*--------------------------------------------------------------------------*/
/*! Setup the alignment of basic NI types inside a struct (depends on CPU). */

static void setup_basic_types(void)
{
   NI_rowtype *rt ;
   int ii ;

   if( rowtype_table != NULL ) return ;  /* don't run this twice */

   /* get alignments and sizes of the basic types */

   type_alignment[NI_BYTE   ] = offsetof(qvzw_byte   ,b) ;
   type_alignment[NI_SHORT  ] = offsetof(qvzw_short  ,b) ;
   type_alignment[NI_INT    ] = offsetof(qvzw_int    ,b) ;
   type_alignment[NI_FLOAT  ] = offsetof(qvzw_float  ,b) ;
   type_alignment[NI_DOUBLE ] = offsetof(qvzw_double ,b) ;
   type_alignment[NI_COMPLEX] = offsetof(qvzw_complex,b) ;
   type_alignment[NI_RGB    ] = offsetof(qvzw_rgb    ,b) ;
   type_alignment[NI_RGBA   ] = offsetof(qvzw_rgba   ,b) ;

   type_size[NI_BYTE   ] = sizeof(byte   ) ;
   type_size[NI_SHORT  ] = sizeof(short  ) ;
   type_size[NI_INT    ] = sizeof(int    ) ;
   type_size[NI_FLOAT  ] = sizeof(float  ) ;
   type_size[NI_DOUBLE ] = sizeof(double ) ;
   type_size[NI_COMPLEX] = sizeof(complex) ;
   type_size[NI_RGB    ] = sizeof(rgb    ) ;
   type_size[NI_RGBA   ] = sizeof(rgba   ) ;

   /* initialize the rowtype table with the basic types */

   rowtype_table = new_Htable(17) ;

   for( ii=0 ; ii < NI_NUM_BASIC_TYPES ; ii++ ){

     rt              = NI_new( NI_rowtype ) ;
     rt->code        = ii ;
     rt->size        = type_size[ii] ;
     rt->algn        = type_alignment[ii] ;
     rt->name        = NI_strdup(type_name[ii]) ;
     rt->userdef     = NI_strdup(type_name[ii]) ;
     rt->flag        = 0 ;

     rt->comp_num    = 1 ;                      /* basic types have */
     rt->comp_typ    = NI_malloc(sizeof(int)) ; /* only one component */
     rt->comp_typ[0] = ii ;
     rt->comp_dim    = NI_malloc(sizeof(int)) ;
     rt->comp_dim[0] = -1 ;                     /* fixed size component */

     rt->part_num    = 1 ;                      /* basic types have */
     rt->part_typ    = NI_malloc(sizeof(int)) ; /* only one part */
     rt->part_typ[0] = ii ;
     rt->part_off    = NI_malloc(sizeof(int)) ;
     rt->part_off[0] = 0 ;
     rt->part_dim    = NI_malloc(sizeof(int)) ;
     rt->part_dim[0] = -1 ;                     /* fixed size part */

     ROWTYPE_register( rt ) ;
   }

   /* alignment and size of pointers */

   pointer_alignment = offsetof(qvzw_pointer,b) ;
   pointer_size      = sizeof(void *) ;

   /* insert a special rowtype for pointers */

   rt              = NI_new( NI_rowtype ) ;
   rt->code        = ROWTYPE_POINTER ;
   rt->size        = pointer_size ;
   rt->algn        = pointer_alignment ;
   rt->name        = NI_strdup("NI_pointer") ;
   rt->userdef     = NI_strdup("NI_pointer") ;
   rt->flag        = 0 ;

   rt->comp_num    = 1 ;
   rt->comp_typ    = NI_malloc(sizeof(int)) ;
   rt->comp_typ[0] = ROWTYPE_POINTER ;
   rt->comp_dim    = NI_malloc(sizeof(int)) ;
   rt->comp_dim[0] = -1 ;

   rt->part_num    = 1 ;
   rt->part_typ    = NI_malloc(sizeof(int)) ;
   rt->part_typ[0] = ROWTYPE_POINTER ;
   rt->part_off    = NI_malloc(sizeof(int)) ;
   rt->part_off[0] = 0 ;
   rt->part_dim    = NI_malloc(sizeof(int)) ;
   rt->part_dim[0] = -1 ;

   ROWTYPE_register( rt ) ; pointer_rowtype = rt ;

   if( ROWTYPE_debug )
     profile_Htable( "rowtype_table" , rowtype_table ) ;
}

/*-------------------------------------*/
/*! Error exit macro for next function */

#undef  ERREX
#define ERREX(str)                                                            \
 do { fprintf(stderr,"** NI_rowtype_define('%s','%s'): %s\n",tname,tdef,str); \
      return -1 ; } while(0)

/*--------------------------------------------------------------------------*/
/* Define a NI_rowtype, which is an expression of a C struct.
    - tname = name to call this thing
    - tdef = definition of the type
       - a list of type names separated by commas
       - a type name is one of the NIML basic types
         ("byte", "short", "int", "float", "double", "complex", "rgb", "rgba"),
         or is a previously defined tname from a another NI_rowtype
       - note that only fixed length types are definable here
       - a type name may be preceded by an integer count, as in "int,4*float"

   The intention is that a C struct type is mapped to a NI_rowtype.  Some
   examples:
       - typedef struct { int i; float x,y,z; } ifvec ;
       - NI_rowtype_define( "ifvec" , "int,3*float" ) ;
       - typedef struct { byte a,b; ifvec c; } abvec ;
       - NI_rowtype_define( "abvec" , "2*byte,ifvec" ) ;

   The return value is a positive integer code which can be used to
   identify this NI_rowtype (the tname string can also be used for
   this purpose).  If -1 is returned, something bad transpired.

   The NI_rowtype keeps track of the offsets of each field in these
   putative structs.  For example, you can define a rowmap for
   packing/unpacking structs to/from NI_elements using a function like
     - NI_define_rowmap_from_rowtype_code( nel , code ) ;
     - NI_define_rowmap_from_rowtype_name( nel , tname) ;
----------------------------------------------------------------------------*/

int NI_rowtype_define( char *tname , char *tdef )
{
   NI_rowtype *rt , *qt ;
   int ii,jj , id,jd,kd,isdim,nn , almax,cbase,np,pb , last_size ;
   str_array *sar ;
   char *tp,*sp,*bp , str[256] ;

   /*-- check inputs --*/

   if( !NI_is_name(tname) )              ERREX("bad typename") ;
   if( strlen(tname) > 255 )             ERREX("overlong typename") ;
   if( tdef  == NULL || *tdef  == '\0' ) ERREX("empty type definition") ;

   /*-- create Htable of basic types, if not already defined --*/

   if( rowtype_table == NULL ) setup_basic_types() ;

   /*-- see if type name already defined --*/

   rt = NI_rowtype_find_name( tname ) ;
   if( rt != NULL ) ERREX("type name already defined") ;

   /*-- break defining string into components --*/

   sar = decode_string_list( tdef , ",;" ) ;

   if( sar == NULL || sar->num < 1 ){ NI_free(sar); ERREX("illegal definition"); }

   /*-- make the new rowtype --*/

   rt          = NI_new( NI_rowtype ) ;
   rt->code    = ROWTYPE_BASE_CODE + sizeof_Htable(rowtype_table) ;
   rt->name    = NI_strdup( tname ) ;
   rt->userdef = NI_strdup( tdef ) ;
   rt->flag    = 0 ;

   /*-- loop over components in tdef, loading the new rt with their info --*/

   rt->part_num = rt->comp_num = 0 ;

   for( ii=0 ; ii < sar->num ; ii++ ){

     tp = sar->str[ii] ;
     id = 0 ; kd = strlen(tp) ;  /* type name is from tp[id..kd-1] */
     if( kd == 0 ){
       delete_rowtype(rt); delete_str_array(sar); ERREX("empty component name?");
     }

     /* get count, if present, into jd */

     sp = strchr(tp,'*') ;   /* count*type  */
     bp = strchr(tp,'[') ;   /* type[count] */

     if( sp != NULL || bp != NULL ){            /*** a count is present ***/

       if( sp != NULL && bp != NULL ){          /* can't have both forms! */
         delete_rowtype(rt); delete_str_array(sar); ERREX("two repeat counts?");
       }

       if( sp != NULL ){                        /* count*type */
         nn = 0 ;                               /*  - count starts at nn */
         id = (sp-tp)+1 ;                       /*  - type name starts at id */
       } else {                                 /* type[count] */
         kd = (bp-tp) ;                         /*  - type name ends at kd-1 */
         nn = kd+1 ;                            /*  - count starts at nn */
       }

       jd = -1 ;
       if( tp[nn] != '#' ){                     /* count is a number */
         isdim = 0 ;
         sscanf( tp+nn , "%d" , &jd ) ;
         if( jd <= 0 ){
           delete_rowtype(rt); delete_str_array(sar); ERREX("bad repeat number");
         }
       } else {                                 /* count is a reference */
         isdim = 1 ;
         sscanf( tp+nn+1 , "%d" , &jd ) ;       /* ref must be to index */
         if( jd <= 0 || jd > ii ){              /* before this component */
           delete_rowtype(rt); delete_str_array(sar); ERREX("bad #index");
         }
         if( rt->comp_typ[jd-1] != NI_INT ){    /* ref must be to an int */
           delete_rowtype(rt); delete_str_array(sar); ERREX("non-int #index");
         }
       }
     } else {
       isdim = 0 ; jd = 1 ;                     /* default count of 1 */
     }

     /* get the type of this component from its name */

     if( kd-id < 1 || kd-id > 255 ){
       delete_rowtype(rt); delete_str_array(sar); ERREX("overlong component name");
     }

     NI_strncpy( str , tp+id , kd-id+1 ) ;
     qt = NI_rowtype_find_name( str ) ;
     if( qt == NULL ){
       delete_rowtype(rt); delete_str_array(sar); ERREX("bad component type");
     }

     if( !isdim ){  /*** fixed count: add jd copies of this component type ***/

       rt->comp_typ = NI_realloc( rt->comp_typ , sizeof(int)*(rt->comp_num+jd) ) ;
       rt->comp_dim = NI_realloc( rt->comp_dim , sizeof(int)*(rt->comp_num+jd) ) ;

       for( jj=0 ; jj < jd ; jj++ ){
         rt->comp_typ[rt->comp_num + jj] = qt->code ;
         rt->comp_dim[rt->comp_num + jj] = -1 ;        /* fixed size part */
       }

       rt->comp_num += jd ;                 /* have more components now */
       rt->part_num += jd * qt->part_num ;  /* have more parts now */

     } else {       /*** variable count: add 1 component that is a pointer  */
                    /***                 to an array of fixed size elements */

       if( ROWTYPE_is_varsize(qt) ){
         delete_rowtype(rt); delete_str_array(sar);
         ERREX("variable size array must have fixed size type");
       }

       rt->comp_typ = NI_realloc( rt->comp_typ , sizeof(int)*(rt->comp_num+1) ) ;
       rt->comp_dim = NI_realloc( rt->comp_dim , sizeof(int)*(rt->comp_num+1) ) ;

       rt->comp_typ[rt->comp_num] = qt->code ;  /* type this points to */
       rt->comp_dim[rt->comp_num] = jd-1 ;      /* which component has */
                                                /* array dimension count */
       rt->comp_num ++ ;  /* 1 more component */
       rt->part_num ++ ;  /* and 1 more part */

       rt->flag |= ROWTYPE_VARSIZE_MASK ;   /* mark rowtype as variable sized */

     }

   } /* end of loop over components */

   delete_str_array(sar) ;                  /* done with this string array */

   if( rt->part_num == 0 ){ delete_rowtype(rt); ERREX("no components?"); }  /* no parts? */

   /*** now loop over components, breaking them down into their parts,
        storing the part types and their offsets into the C struct    ***/

   rt->part_off = NI_malloc( sizeof(int) * rt->part_num ) ;
   rt->part_typ = NI_malloc( sizeof(int) * rt->part_num ) ;
   rt->part_dim = NI_malloc( sizeof(int) * rt->part_num ) ;

   almax = 1 ;  /* will be largest type_alignment of any part */
   cbase = 0 ;  /* base offset into struct for next component */
   id    = 0 ;  /* part number we are about to work on */

   for( ii=0 ; ii < rt->comp_num ; ii++ ){

                                    /*** component is a      ***/
     if( rt->comp_dim[ii] >= 0 ){   /*** variable size array ***/
                                    /*** ==> store 1 pointer ***/
       if( pointer_alignment > 1 ){
         jd = cbase % pointer_alignment ;
         if( jd > 0 ) cbase += (pointer_alignment-jd) ;
       }

       rt->part_typ[id] = rt->comp_typ[ii] ;      /* what type this points to */
       rt->part_off[id] = cbase ;

       /* count the number of parts before the dimension component into kd */

       for( jd=kd=0 ; jd < rt->comp_dim[ii] ; jd++ ){
         if( rt->comp_dim[jd] >= 0 ){   /* this component is a pointer itself */
           kd++ ;
         } else {                      /* this component has fixed size parts */
           qt = NI_rowtype_find_code( rt->comp_typ[jd] ) ;
           kd += qt->part_num ;
         }
       }
       rt->part_dim[id] = kd ;    /* which part is the dimension of this part */

       kd = pointer_alignment ;
       if( kd > almax ) almax = kd ;

       id++ ; cbase += pointer_size ;

     } else {      /*** fixed size type, possibly with multiple parts ***/

       qt = NI_rowtype_find_code( rt->comp_typ[ii] ) ;  /* component type */

       /* adjust cbase upward if this component isn't properly aligned */

       if( qt->algn > 1 ){
         jd = cbase % qt->algn ;
         if( jd > 0 ) cbase += (qt->algn-jd) ;
       }

       pb = id ;                    /* part base index for this component */
       np = qt->part_num ;                 /* number of parts to add here */

       rt->part_typ[id] = qt->part_typ[0] ;  /* first part from component */
       rt->part_off[id] = cbase ;             /* goes at the current base */
       rt->part_dim[id] = -1 ;   /* 1st part cannot be variable dim array */

       kd = type_alignment[ rt->part_typ[id] ] ;     /* alignment of part */
       if( kd > almax ) almax = kd ;   /* keep track of largest alignment */

       last_size = type_size[ rt->part_typ[id] ] ;    /* size of 1st part */

       id++ ;  /* prepare to add next part */

       /* loop over rest of parts from this component */

       for( jj=1 ; jj < np ; jj++,id++ ){

         rt->part_typ[id] = qt->part_typ[jj] ;     /* type of new part      */

         if( qt->part_dim[jj] < 0 ){        /******* fixed size part       **/

           nn = last_size                        ; /* # bytes in last part  */
           jd = rt->part_off  [id-1            ] ; /* offset of last part   */
           kd = type_alignment[rt->part_typ[id]] ; /* how to align new part */
           if( kd > almax ) almax = kd ; /* keep track of largest alignment */

           nn += jd ;  /* next available byte = sum of last offset and size */
           if( kd > 1 ){                           /* must move nn up if    */
             jd = nn % kd ;                        /* not on exact multiple */
             if( jd > 0 ) nn += (kd-jd) ;          /* of jj bytes alignment */
           }
           rt->part_off[id] = nn ;
           rt->part_dim[id] = -1 ;               /* mark as fixed size part */

           last_size = type_size[rt->part_typ[id]] ;   /* size of this part */

         } else {                           /***** variable dim array part **/

           nn = last_size ;
           jd = rt->part_off[id-1] ;
           kd = pointer_alignment ;
           if( kd > almax ) almax = kd ;
           nn += jd ;
           if( kd > 1 ){
             jd = nn % kd ;
             if( jd > 0 ) nn += (kd-jd) ;
           }
           rt->part_off[id] = nn ;
           last_size = pointer_size ;

           /* qt->part_dim[jj] is the part index in qt
              of the dimension for this variable dim array part;
              we must convert that to a part index in the new rowtype */

           rt->part_dim[id] = pb + qt->part_dim[jj] ;

         }

       } /* end of loop over parts within this component */

       /* now move the base offset up by the size of the current
          component (which may be bigger than the sum of its parts) */

       cbase += qt->size ;

     } /* end of fixed size component part-izing */

   } /* end of loop over components */

   /* now compute the overall size of this new rowtype;
      at this point,
      cbase = next byte offset available after last part;
      this would be the size, but may have to be pushed
      up to allow for byte alignment of this rowtype     */

   rt->algn = almax ;
   if( rt->algn > 1 ){
     jd = cbase % rt->algn ;
     if( jd > 0 ) cbase += (rt->algn-jd) ;
   }
   rt->size = cbase ;

   /** debugging printouts **/

   if( ROWTYPE_debug ){
     printf("\n") ;
     printf("NI_rowtype_define: %s %s\n",tname,tdef) ;
     printf("  code     = %d\n",rt->code) ;
     printf("  size     = %d\n",rt->size) ;
     printf("  algn     = %d\n",rt->algn) ;
     printf("  flag     = %d\n",rt->flag) ;

     printf("  comp_num = %d\n",rt->part_num) ;

     printf("  comp_typ = " ) ;
     for( ii=0 ; ii < rt->comp_num ; ii++ ) printf("%4d ",rt->comp_typ[ii]) ;
     printf("\n") ;

     printf("  comp_dim = " ) ;
     for( ii=0 ; ii < rt->comp_num ; ii++ ) printf("%4d ",rt->comp_dim[ii]) ;
     printf("\n") ;

     printf("  part_num = %d\n",rt->part_num) ;

     printf("  part_typ = " ) ;
     for( ii=0 ; ii < rt->part_num ; ii++ ) printf("%4d ",rt->part_typ[ii]) ;
     printf("\n") ;

     printf("  part_off = " ) ;
     for( ii=0 ; ii < rt->part_num ; ii++ ) printf("%4d ",rt->part_off[ii]) ;
     printf("\n") ;

     printf("  part_dim = " ) ;
     for( ii=0 ; ii < rt->part_num ; ii++ ) printf("%4d ",rt->part_dim[ii]) ;
     printf("\n") ;
   }

   /* save this in the table of rowtypes,
      and return the numerical code for this new type */

   ROWTYPE_register(rt) ;
   return rt->code ;
}

/*--------------------------------------------------------------------*/
/*! Find a rowtype by its name. */

NI_rowtype * NI_rowtype_find_name( char *nn )
{
   if( nn == NULL || *nn == '\0' ) return NULL ;
   if( rowtype_table == NULL ) setup_basic_types() ;
   return (NI_rowtype *) findin_Htable(nn,rowtype_table) ;
}

/*--------------------------------------------------------------------*/
/*! Find a rowtype by its integer code. */

NI_rowtype * NI_rowtype_find_code( int nn )
{
   if( nn < 0 ) return NULL ;
   if( rowtype_table == NULL ) setup_basic_types() ;
   if( nn == ROWTYPE_POINTER ) return pointer_rowtype ;
   if( nn >= ROWTYPE_OFFSET ) nn = nn - ROWTYPE_BASE_CODE ;
   if( nn < 0 || nn > rowtype_num ) return NULL ;
   return rowtype_array[nn] ;
}

/*--------------------------------------------------------------------*/
/*! Given a rowtype name, find its integer code.
    Returns -1 if the name isn't found in the rowtype table.
----------------------------------------------------------------------*/

int NI_rowtype_name_to_code( char *nn )
{
   NI_rowtype *rt = NI_rowtype_find_name( nn ) ;
   if( rt != NULL ) return rt->code ;
   return -1 ;
}

/*--------------------------------------------------------------------*/
/*! Given a rowtype code, find its string name.
    Returns NULL if the code isn't found in the rowtype table,
    otherwise returns the pointer to the name inside the table
    (i.e., don't free this string!).
----------------------------------------------------------------------*/

char * NI_rowtype_code_to_name( int nn )
{
   NI_rowtype *rt = NI_rowtype_find_code( nn ) ;
   if( rt != NULL ) return rt->name ;
   return NULL ;
}

/*--------------------------------------------------------------------*/
/*! Given a rowtype name, find its size in bytes.
    Returns -1 if rowtype not found.
    Returns 0 if size is indeterminate.
----------------------------------------------------------------------*/

int NI_rowtype_name_to_size( char *nn )
{
   NI_rowtype *rt = NI_rowtype_find_name( nn ) ;
   if( rt != NULL ) return rt->size ;
   return -1 ;
}

/*-----------------------------------------------------------*/
/*! Return the size in bytes of an atomic datatype.
    If an unknown or variable length type (i.e., string)
    is given, then the return value is zero.
-------------------------------------------------------------*/

int NI_rowtype_code_to_size( int dtyp )
{
   static int last_dtyp=-1 , last_size=0 ;         /* 12 Dec 2002 */

   if( dtyp == last_dtyp ) return last_size ;

   switch( dtyp ){
     case NI_BYTE:         return sizeof(byte);
     case NI_SHORT:        return sizeof(short);
     case NI_INT:          return sizeof(int);
     case NI_FLOAT:        return sizeof(float);
     case NI_DOUBLE:       return sizeof(double);
     case NI_COMPLEX:      return sizeof(complex);
     case NI_RGB:          return sizeof(rgb);
     case NI_RGBA:         return sizeof(rgba);
     case NI_STRING:       return 0 ;          /* not fixed size */
     case ROWTYPE_POINTER: return pointer_size ;

     default:{
       NI_rowtype *rt = NI_rowtype_find_code(dtyp) ;
       if( rt != NULL ){
         last_dtyp = dtyp ; last_size = rt->size ; /* 12 Dec 2002 */
         return last_size ;
       }
     }
     return 0 ;
  }
  return 0 ;  /* unreachable */
}

/*-----------------------------------------------------------------------*/
/*! Define a rowmap in a NI_element from a NI_rowtype expression
    of a C struct.
    - code = integer code for the NI_rowtype
    - RWCox - 10 Dec 2002
-------------------------------------------------------------------------*/

void NI_define_rowmap_from_rowtype_code( NI_element *nel, int code )
{
   int nrow=0 , typ,off ;
   NI_rowtype *qt ;

   if( nel == NULL || nel->type != NI_ELEMENT_TYPE ) return ;
   if( code < 0 || code == ROWTYPE_POINTER ) return ;

   qt = NI_rowtype_find_code( code ) ;
   if( qt == NULL || ROWTYPE_is_varsize(qt) ) return ;

   NI_define_rowmap_AR( nel, qt->part_num, qt->part_typ, qt->part_off ) ;
}
