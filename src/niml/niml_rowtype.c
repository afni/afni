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

static int   type_alignment[NI_NUM_BASIC_TYPES+1] ;
static int   type_size     [NI_NUM_BASIC_TYPES+1] ;
static char *type_name     [NI_NUM_BASIC_TYPES+1] = {
  "byte"  , "short"  , "int"     ,
  "float" , "double" , "complex" ,
  "rgb"   , "rgba"   , "String"
} ;

/* these are used to find/save the alignment and size of pointers */

typedef struct { char a; void *b; } qvzw_pointer ;
static int pointer_alignment ;
static int pointer_size ;

/*! The Htable of user-defined rowtypes, indexed by type name. */

static Htable *rowtype_table = NULL ;

/*! The array of user-defined rowtypes, indexed by type code. */

static NI_rowtype **rowtype_array = NULL ;
static int         rowtype_num    = 0    ;

/*! Rowtype code for first user-defined type. */

#define ROWTYPE_OFFSET     1001

/*! Used to set the code for each new user-defined type.
    (The '-1' is to allow for the String type, which isn't
     a basic type but is a builtin type.) */

#define ROWTYPE_BASE_CODE (ROWTYPE_OFFSET-NI_NUM_BASIC_TYPES-1)

/*! Check if a rowtype code is a derived type or a builtin type */

#define ROWTYPE_is_builtin_code(cc) ((cc) >= 0 && (cc) < ROWTYPE_OFFSET)

/*! Get the dimension of the ii-th part of
    the struct stored at pointer pt, of type rt;
    pt is used only if the part has a variable dimension. */

#define ROWTYPE_part_dimen(rt,pt,ii)                           \
 ( ( (rt)->part_dim[ii] < 0 )                                  \
   ? 1                        /* fixed dimen is always 1 */    \
   : *((int *)( (pt) + (rt)->part_off[ (rt)->part_dim[ii] ] )) ) /* var dim */

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

/*--------------------------------*/
/*! Debug flag for rowtype stuff. */

static int ROWTYPE_debug = 0 ;

/*! Set debug flag for rowtype stuff. */

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

   rowtype_table = new_Htable(19) ;

   for( ii=0 ; ii < NI_NUM_BASIC_TYPES ; ii++ ){

     rt              = NI_new( NI_rowtype ) ;
     rt->code        = ii ;
     rt->size        = type_size[ii] ;
     rt->psiz        = rt->size ;
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
     rt->part_siz    = NI_malloc(sizeof(int)) ;
     rt->part_siz[0] = type_size[ii] ;
     rt->part_dim    = NI_malloc(sizeof(int)) ;
     rt->part_dim[0] = -1 ;                     /* fixed size part */

     ROWTYPE_register( rt ) ;
   }

   /* alignment and size of pointers */

   pointer_alignment = offsetof(qvzw_pointer,b) ;
   pointer_size      = sizeof(void *) ;

   type_alignment[NI_STRING] = pointer_alignment ;
   type_size     [NI_STRING] = pointer_size ;

   /* insert a special rowtype for String (really a pointer) */

   rt              = NI_new( NI_rowtype ) ;
   rt->code        = NI_STRING ;
   rt->size        = pointer_size ;
   rt->psiz        = 0 ;                       /* variable size */
   rt->algn        = pointer_alignment ;
   rt->name        = NI_strdup("String") ;
   rt->userdef     = NI_strdup("String") ;
   rt->flag        = ROWTYPE_VARSIZE_MASK ;    /* variable size */

   rt->comp_num    = 1 ;
   rt->comp_typ    = NI_malloc(sizeof(int)) ;
   rt->comp_typ[0] = NI_STRING ;
   rt->comp_dim    = NI_malloc(sizeof(int)) ;
   rt->comp_dim[0] = -1 ;

   rt->part_num    = 1 ;
   rt->part_typ    = NI_malloc(sizeof(int)) ;
   rt->part_typ[0] = NI_STRING ;
   rt->part_off    = NI_malloc(sizeof(int)) ;
   rt->part_off[0] = 0 ;
   rt->part_siz    = NI_malloc(sizeof(int)) ;
   rt->part_siz[0] = pointer_size ;
   rt->part_dim    = NI_malloc(sizeof(int)) ;
   rt->part_dim[0] = -1 ;

   ROWTYPE_register( rt ) ;

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
/* Define a NI_rowtype, which is an expression of a C struct type (with
   some restrictions).
    - tname = name to call this thing
    - tdef = definition of the type
       - A list of type names ('components') separated by commas.
       - A type name is one of the NIML basic types
         ("byte", "short", "int", "float", "double", "complex", "rgb", "rgba"),
         or is "String",
         or is a previously defined tname from a another NI_rowtype.
       - Recursive types cannot be defined; that is, no component of
         tdef can refer to the same tname that is being defined now.
       - A type name may be preceded/appended by an integer count,
         as in "int,4*float" or "int,float[4]".
       - Variable dimension arrays may be defined, IF the size of the
         array is an int component of this new rowtype that occurs before
         the array definition:
           - "int,float[#1]" defines the second component to be
             a float array whose length is given by the value of
             the first component
           - This example is analogous to this C fragment:
              - typedef struct { int n; float *x; } far ;
              - far xfar ;
              - xfar.n = 373 ;
              - xfar.x = malloc(sizeof(float)*xfar.n) ;
           - You can't do something like "int,3*float[#1]" - only one
             repeat count per component is allowed!  The right way to
             do this would be "int,float[#1],float[#1],float[#1]".
           - All variable components in instances of these arrays
             will be allocated with NI_malloc() and should be released
             with NI_free().
           - The type of a variable dim array must be fixed in size;
             that is, that type itself cannot contain any var dim
             array or String components.
       - String components are also variable dimension arrays,
         but whose size is given by the usual strlen().

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
   if( strlen(tname) > 255 )             ERREX("toolong typename") ;
   if( tdef  == NULL || *tdef  == '\0' ) ERREX("empty type definition") ;

   /*-- create Htable of basic types, if not already defined --*/

   if( rowtype_table == NULL ) setup_basic_types() ;

   /*-- see if type name already defined --*/

   rt = NI_rowtype_find_name( tname ) ;
   if( rt != NULL ) ERREX("type name already defined") ;

   /*-- break defining string into components --*/

   sar = decode_string_list( tdef , ",;" ) ;

   if( sar == NULL || sar->num < 1 ){
     NI_free(sar) ; ERREX("illegal definition") ;
   }

   /*-- initialize the new rowtype --*/

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

     sp = strchr(tp,'*') ;   /* format of component string: count*type  */
     bp = strchr(tp,'[') ;   /* format of component string: type[count] */

     if( sp != NULL || bp != NULL ){            /*** a count is present ***/

       if( sp != NULL && bp != NULL ){          /* can't have both forms! */
         delete_rowtype(rt); delete_str_array(sar); ERREX("two repeat counts?");
       }

       if( sp != NULL ){                        /* format: count*type */
         nn = 0 ;                               /*  - count starts at nn */
         id = (sp-tp)+1 ;                       /*  - type name starts at id */
       } else {                                 /* format: type[count] */
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
       } else {                                 /* count is a #reference */
         isdim = 1 ;
         sscanf( tp+nn+1 , "%d" , &jd ) ;       /* ref must be to index */
         if( jd <= 0 || jd > ii ){              /* before this component */
           delete_rowtype(rt); delete_str_array(sar); ERREX("bad #index");
         }
         if( rt->comp_typ[jd-1] != NI_INT ||    /* ref must be to an int */
             rt->comp_dim[jd-1] >= 0        ){  /* of fixed size (1 int) */
           delete_rowtype(rt); delete_str_array(sar); ERREX("non-int #index");
         }
       }
     } else {
       isdim = 0 ; jd = 1 ;                     /* default count of 1 */
     }

     /* get the type of this component from its name */

     if( kd-id < 1 || kd-id > 255 ){
      delete_rowtype(rt); delete_str_array(sar); ERREX("toolong component name");
     }

     NI_strncpy( str , tp+id , kd-id+1 ) ;
     qt = NI_rowtype_find_name( str ) ;
     if( qt == NULL ){
       delete_rowtype(rt); delete_str_array(sar); ERREX("bad component type");
     }

     if( !isdim ){  /*** fixed count: add jd copies of this component type ***/

       rt->comp_typ = NI_realloc( rt->comp_typ, sizeof(int)*(rt->comp_num+jd) );
       rt->comp_dim = NI_realloc( rt->comp_dim, sizeof(int)*(rt->comp_num+jd) );

       for( jj=0 ; jj < jd ; jj++ ){
         rt->comp_typ[rt->comp_num + jj] = qt->code ;
         rt->comp_dim[rt->comp_num + jj] = -1 ;        /* fixed size part */
       }

       rt->comp_num += jd ;                 /* have more components now */
       rt->part_num += jd * qt->part_num ;  /* have more parts now */

       if( ROWTYPE_is_varsize(qt) )         /* if component is variable sized, */
         rt->flag |= ROWTYPE_VARSIZE_MASK ; /* mark rowtype as variable sized */

     } else {       /*** variable count: add 1 component that is a pointer  */
                    /***                 to an array of fixed size elements */

       if( ROWTYPE_is_varsize(qt) ){
         delete_rowtype(rt); delete_str_array(sar);
         ERREX("variable size array must have fixed size type");
       }

       rt->comp_typ = NI_realloc( rt->comp_typ, sizeof(int)*(rt->comp_num+1) );
       rt->comp_dim = NI_realloc( rt->comp_dim, sizeof(int)*(rt->comp_num+1) );

       rt->comp_typ[rt->comp_num] = qt->code ;  /* type this points to */
       rt->comp_dim[rt->comp_num] = jd-1 ;      /* which component has */
                                                /* array dimension count */
       rt->comp_num ++ ;  /* 1 more component */
       rt->part_num ++ ;  /* and 1 more part */

       rt->flag |= ROWTYPE_VARSIZE_MASK ;   /* mark rowtype as variable sized */

     }

   } /* end of loop over components */

   delete_str_array(sar) ;                  /* done with this string array */

   if( rt->part_num == 0 ){ delete_rowtype(rt); ERREX("no components?"); }

   /*** now loop over components, breaking them down into their parts,
        storing the part types and their offsets into the C struct    ***/

   rt->part_off = NI_malloc( sizeof(int) * rt->part_num ) ;
   rt->part_typ = NI_malloc( sizeof(int) * rt->part_num ) ;
   rt->part_dim = NI_malloc( sizeof(int) * rt->part_num ) ;
   rt->part_siz = NI_malloc( sizeof(int) * rt->part_num ) ;

   almax = 1 ;  /* will be largest type_alignment of any part */
   cbase = 0 ;  /* base offset into struct for next component */
   id    = 0 ;  /* part number we are about to work on */

   for( ii=0 ; ii < rt->comp_num ; ii++ ){

                                    /*** component is a      ***/
     if( rt->comp_dim[ii] >= 0 ){   /*** variable size array ***/
                                    /*** ==> store 1 pointer ***/

       if( pointer_alignment > 1 ){            /* make sure cbase */
         jd = cbase % pointer_alignment ;      /* is aligned OK  */
         if( jd > 0 ) cbase += (pointer_alignment-jd) ;
       }

       /* note that this is the only case where a part_typ
          might end up as a derived type - normally, part_typ
          will be a builtin type code (NI_BYTE .. NI_STRING)  */

       rt->part_typ[id] = rt->comp_typ[ii] ;
       rt->part_off[id] = cbase ;
       rt->part_siz[id] = pointer_size ;

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
       rt->part_siz[id] = last_size ;

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
           rt->part_siz[id] = last_size ;

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
           rt->part_siz[id] = last_size ;

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

   /* 26 Dec 2002: Compute the sum of the part sizes
                   (zero if this has variable size arrays).
                   If rt->psiz == rt->size, then
                   struct is stored without padding bytes. */

   rt->psiz = 0 ;
   if( (rt->flag & ROWTYPE_VARSIZE_MASK) == 0 ){
     for( ii=0 ; ii < rt->part_num ; ii++ )
       rt->psiz += rt->part_siz[ii] ;
   }

   /** debugging printouts **/

   if( ROWTYPE_debug ){
     fprintf(stderr,"\n") ;
     fprintf(stderr,"NI_rowtype_define: '%s' = '%s'\n",tname,tdef) ;
     fprintf(stderr,"  code     = %d\n",rt->code) ;
     fprintf(stderr,"  size     = %d\n",rt->size) ;
     fprintf(stderr,"  psiz     = %d\n",rt->psiz) ;
     fprintf(stderr,"  algn     = %d\n",rt->algn) ;
     fprintf(stderr,"  flag     = %d\n",rt->flag) ;

     fprintf(stderr,"  comp_num = %d\n",rt->part_num) ;

     fprintf(stderr,"  comp_typ = " ) ;
     for( ii=0 ; ii < rt->comp_num ; ii++ ) fprintf(stderr,"%4d ",rt->comp_typ[ii]) ;
     fprintf(stderr,"\n") ;

     fprintf(stderr,"  comp_dim = " ) ;
     for( ii=0 ; ii < rt->comp_num ; ii++ ) fprintf(stderr,"%4d ",rt->comp_dim[ii]) ;
     fprintf(stderr,"\n") ;

     fprintf(stderr,"  part_num = %d\n",rt->part_num) ;

     fprintf(stderr,"  part_typ = " ) ;
     for( ii=0 ; ii < rt->part_num ; ii++ ) fprintf(stderr,"%4d ",rt->part_typ[ii]) ;
     fprintf(stderr,"\n") ;

     fprintf(stderr,"  part_off = " ) ;
     for( ii=0 ; ii < rt->part_num ; ii++ ) fprintf(stderr,"%4d ",rt->part_off[ii]) ;
     fprintf(stderr,"\n") ;

     fprintf(stderr,"  part_siz = " ) ;
     for( ii=0 ; ii < rt->part_num ; ii++ ) fprintf(stderr,"%4d ",rt->part_siz[ii]) ;
     fprintf(stderr,"\n") ;

     fprintf(stderr,"  part_dim = " ) ;
     for( ii=0 ; ii < rt->part_num ; ii++ ) fprintf(stderr,"%4d ",rt->part_dim[ii]) ;
     fprintf(stderr,"\n") ;
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
   if( nn >= ROWTYPE_OFFSET ) nn = nn - ROWTYPE_BASE_CODE ;
   if( nn < 0 || nn >= rowtype_num ) return NULL ;
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
/*! Given a rowtype name, find its struct size in bytes.
    - Returns -1 if rowtype not found.
    - Note that if the rowtype contains variable size arrays, this
      size returned here is just the size of the storage for
      the basic struct with pointers, not including the variable
      arrays.
    - See NI_rowtype_vsize() to get the size of a rowtype instance
      including its variable size arrays.
----------------------------------------------------------------------*/

int NI_rowtype_name_to_size( char *nn )
{
   NI_rowtype *rt = NI_rowtype_find_name( nn ) ;
   if( rt != NULL ) return rt->size ;
   return -1 ;
}

/*-----------------------------------------------------------*/
/*! Given a rowtype code, find its struct size in bytes.
    See also NI_rowtype_name_to_size().
-------------------------------------------------------------*/

int NI_rowtype_code_to_size( int dtyp )
{
   static int last_dtyp=-1 , last_size=-1 ;         /* 12 Dec 2002 */
   NI_rowtype *rt ;

   if( dtyp <  0              ) return -1 ;
   if( dtyp <  ROWTYPE_OFFSET ) return type_size[dtyp] ;
   if( dtyp == last_dtyp      ) return last_size ;

   rt = NI_rowtype_find_code(dtyp) ;
   if( rt != NULL ){
     last_dtyp = dtyp; last_size = rt->size; return last_size;
   }
   return -1 ;  /* bad */
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
   if( code < 0 ) return ;

   qt = NI_rowtype_find_code( code ) ;
   if( qt == NULL || ROWTYPE_is_varsize(qt) ) return ;

   NI_define_rowmap_AR( nel, qt->part_num, qt->part_typ, qt->part_off ) ;
}

/*-----------------------------------------------------------------------*/
/*! Compute the size of all the data in a struct defined in a NI_rowtype
    (not including padding), for this instance of the struct,
    allowing for variable array parts.  Zero is returned if something
    bad happens.
-------------------------------------------------------------------------*/

int NI_rowtype_vsize( NI_rowtype *rt , void *dpt )
{
   int ii,jj , ss , *dd ;
   char *dat = (char *)dpt ;

   if( rt == NULL   ) return 0 ;        /* nonsense input */
   if( rt->psiz > 0 ) return rt->psiz ; /* fixed size struct */
   if( dat == NULL  ) return 0 ;        /* var size struct with no data? */

#if 0
fprintf(stderr,"NI_rowtype_vsize:\n") ;
#endif

   /* loop over parts, adding up sizes */

   for( ii=ss=0 ; ii < rt->part_num ; ii++ ){
     if( rt->part_typ[ii] == NI_STRING ){      /* string is special */
       char *str = *((char **)((dat) + (rt)->part_off[ii])) ;
       ss += NI_strlen(str) ;
     } else if( rt->part_dim[ii] < 0 ){        /* 1 fixed size type */
#if 0
fprintf(stderr,"  part %d is fixed: siz=%d\n",ii,rt->part_siz[ii]) ;
#endif
       ss += rt->part_siz[ii] ;
     } else {                                  /* var dim array */
       jj = ROWTYPE_part_dimen(rt,dat,ii) ;    /* array size */
#if 0
fprintf(stderr,"  part %d is vardim[%d]: one=%d\n",ii,jj,NI_rowtype_code_to_size(rt->part_typ[ii])) ;
#endif
       ss += jj * NI_rowtype_code_to_size(rt->part_typ[ii]) ;
     }
   }

   return ss ;
}

/*-------------------------------------------------------------------------*/
/*! Encode 1 type value at the end of the text string wbuf (which is
    assumed to be plenty long).  typ must be a fixed size type code,
    or NI_STRING.
---------------------------------------------------------------------------*/

void NI_val_to_text( int typ , char *dpt , char *wbuf )
{
   int jj ;

   if( dpt == NULL || wbuf == NULL ) return ;
   jj = strlen(wbuf) ;

#if 0
fprintf(stderr," NI_val_to_text: typ=%d dpt=%p\n",typ,dpt) ;
#endif

   switch( typ ){

     /*-- a derived type (will not contain var dim arrays) --*/

     default:{
       char *vpt = dpt ;
       NI_rowtype *rt = NI_rowtype_find_code(typ) ;
       if( rt != NULL ){
         int ii ;
         for( ii=0 ; ii < rt->part_num ; ii++ )
           NI_val_to_text( rt->part_typ[ii] , vpt + rt->part_off[ii] , wbuf ) ;
       }
     }
     break ;

     /*-- integer types --*/

     case NI_BYTE:{
       byte *vpt = (byte *)dpt ;
       sprintf(wbuf+jj," %u",(unsigned int)vpt[0]) ;
     }
     break ;

     case NI_SHORT:{
       short *vpt = (short *)dpt ;
       sprintf(wbuf+jj," %d",(int)vpt[0]) ;
     }
     break ;

     case NI_INT:{
       int *vpt = (int *)dpt ;
       sprintf(wbuf+jj," %d",vpt[0]) ;
     }
     break ;

     /* multiple byte structs */

     case NI_RGB:{
       rgb *vpt = (rgb *)dpt ;
       sprintf(wbuf+jj,"  %u %u %u",vpt[0].r,vpt[0].g,vpt[0].b) ;
     }
     break ;

     case NI_RGBA:{
       rgba *vpt = (rgba *)dpt ;
       sprintf(wbuf+jj,"  %u %u %u %u",
               vpt[0].r,vpt[0].g,vpt[0].b,vpt[0].a) ;
     }
     break ;

     /* for floating point outputs,
        first print to a temp string, then clip trailing and leading blanks */

     case NI_FLOAT:{
       float *vpt = (float *)dpt ;
       char fbuf[32] ; int ff ;
       sprintf(fbuf,"%12.6g",vpt[0]) ;
       for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
       for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
       sprintf(wbuf+jj," %s",fbuf+ff) ;
     }
     break ;

     case NI_DOUBLE:{
       double *vpt = (double *)dpt ;
       char fbuf[32] ; int ff ;
       sprintf(fbuf,"%18.12g",vpt[0]) ;
       for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
       for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
       sprintf(wbuf+jj," %s",fbuf+ff) ;
     }
     break ;

     case NI_COMPLEX:{
       complex *vpt = (complex *)dpt ;
       char fbuf[32],gbuf[32] ; int ff,gg ;
       sprintf(fbuf,"%12.6g",vpt[0].r) ;
       for( ff=strlen(fbuf) ; fbuf[ff]==' ' ; ff-- ) fbuf[ff] = '\0' ;
       for( ff=0 ; fbuf[ff] == ' ' ; ff++ ) ;
       sprintf(gbuf,"%12.6g",vpt[0].i) ;
       for( gg=strlen(gbuf) ; gbuf[gg]==' ' ; gg-- ) gbuf[gg] = '\0' ;
       for( gg=0 ; gbuf[gg] == ' ' ; gg++ ) ;
       sprintf(wbuf+jj,"  %s %s",fbuf+ff,gbuf+gg) ;
     }
     break ;

     case NI_STRING:{                         /* 30 Dec 2002 */
       char **vpt = (char **)dpt , *str ;
       str = quotize_string( *vpt ) ;
       sprintf(wbuf+jj," %s",str) ;
       NI_free(str) ;
     }
     break ;

   } /* end of switch on part type */
}

/*-------------------------------------------------------------------------*/
/*! Encode nv type values at the end of the text string wbuf.
    typ must be a fixed size type code, or NI_STRING.
---------------------------------------------------------------------------*/

void NI_multival_to_text( int typ , int nv , char *dpt , char *wbuf )
{
   int ii , jj=NI_rowtype_code_to_size(typ) ;

#if 0
fprintf(stderr,"NI_multival_to_text: typ=%d nv=%d jj=%d dpt=%p\n",typ,nv,jj,dpt) ;
#endif

   if( jj <= 0 ) return ;  /* bad */

   for( ii=0 ; ii < nv ; ii++ )
     NI_val_to_text( typ , dpt+ii*jj , wbuf ) ;
}

/*-------------------------------------------------------------------------*/
/*! Copy 1 fixed size type value in binary format to the wbuf.
    Return value is number of bytes written.  Note that only data
    bytes are written, not any padding between elements.
---------------------------------------------------------------------------*/

int NI_val_to_binary( int typ , char *dpt , char *wbuf )
{
   int jj=0 ;

   if( ROWTYPE_is_builtin_code(typ) ){    /* builtin type */
     jj = type_size[typ] ;
     memcpy(wbuf,dpt,jj) ;
   } else {                               /* derived type */
     NI_rowtype *rt = NI_rowtype_find_code(typ) ;
     int ii ;
     if( rt != NULL && rt->psiz > 0 ){
       for( ii=0 ; ii < rt->part_num ; ii++ ){
         memcpy(wbuf+jj,dpt+rt->part_off[ii],rt->part_siz[ii]) ;
         jj += rt->part_siz[ii] ;
       }
     }
   }
   return jj ;
}

/*-------------------------------------------------------------------------*/
/*! Copy nv fixed size type values in binary format to the wbuf.
    Return value is number of bytes written.
---------------------------------------------------------------------------*/

int NI_multival_to_binary( int typ , int nv , char *dpt , char *wbuf )
{
   int jj=0 ;

   if( ROWTYPE_is_builtin_code(typ) ){   /* builtin type is easy */
     jj = nv * type_size[typ] ;
     memcpy(wbuf,dpt,jj);
   } else {                              /* derived type is harder */
     NI_rowtype *rt = NI_rowtype_find_code(typ) ;
     int ii ;
     if( rt != NULL && rt->psiz > 0 ){
       for( ii=0 ; ii < nv ; ii++ )
         jj += NI_val_to_binary( typ , dpt+(ii*rt->size) , wbuf+jj ) ;
     }
   }
   return jj ;
}

/*------------------------------------------------------------------------*/
/*! Write instances of a NI_rowtype struct to a NI_stream.
      - ns    = stream to write to
      - rt    = rowtype of the data
      - ndat  = number of structs in dat
      - dpt   = pointer to structs with data corresponding to rt
      - tmode = output mode flag
      - return value is number of bytes written to stream
        (-1 if something bad happened, 0 if can't write to stream yet)

   This function is adapted from NI_write_element().
--------------------------------------------------------------------------*/

int NI_write_rowtype( NI_stream_type *ns, NI_rowtype *rt,
                                          int ndat, void *dpt, int tmode )
{
   int ii,jj , row , vsiz,fsiz,dim , ntot,nout ;
   char *ptr , *dat=(char *)dpt ;
   int  nwbuf,bb=0,cc=0;
   char *wbuf=NULL ; /* write buffer */
   char *bbuf=NULL ; /* copy of write buffer */
   char *cbuf=NULL ; /* Base64 buffer */

   /*-- check inputs --*/

   if( ndat == 0 )                                           return  0 ;
   if( ns == NULL || rt == NULL || dat == NULL || ndat < 0 ) return -1 ;

   vsiz = (rt->psiz == 0) ;  /* is this a variable size type */
   fsiz = rt->size ;         /* fixed size of struct (w/padding) */

   /*-- check stream --*/

   if( ns->bad ){                       /* not connected yet? */
     jj = NI_stream_goodcheck(ns,1) ;   /* try to connect it */
     if( jj < 1 ) return jj ;           /* 0 is nothing yet, -1 is death */
   }
   jj = NI_stream_writecheck(ns,1) ;
   if( jj < 1 ) return jj ;

   if( ns->type == NI_STRING_TYPE )  /* output to string buffer ==> text mode */
     tmode = NI_TEXT_MODE ;

   if( !tmode ){                     /* any String parts ==> text mode */
     for( ii=0 ; ii < rt->part_num ; ii++ )
       if( rt->part_typ[ii] == NI_STRING ){ tmode = NI_TEXT_MODE; break; }
   }

   /*-- special case: vector of unpadded fixed-size data
                      (which includes all basic types)
                      can be written directly to the output in binary --*/

   if( fsiz == rt->psiz && tmode == NI_BINARY_MODE )
     return NI_stream_write( ns , dat , ndat*fsiz ) ;

   /*-- allocate space for the write buffer (1 struct at a time) --*/

   switch( tmode ){
     default:             tmode = NI_TEXT_MODE ; /* fall through */
     case NI_TEXT_MODE:   nwbuf = 6*fsiz ; break ;

     case NI_BASE64_MODE:
     case NI_BINARY_MODE: nwbuf =   fsiz ; break ;
   }
   wbuf = NI_malloc(nwbuf+128) ;  /* 128 for the hell of it */

   /* create buffers for Base64 output, if needed */

   if( tmode == NI_BASE64_MODE ){
     bbuf = NI_malloc(  nwbuf+128) ; bb = 0 ;  /* binary buffer */
     cbuf = NI_malloc(2*nwbuf+128) ; cc = 0 ;  /* base64 buffer */
     load_encode_table() ;
   }

  /* this macro take the 'nout' number of output bytes
     and adds into the running total ntot if all was well;
     if all was not well with the write, then it aborts the output */

# define ADDOUT                                              \
  if( nout < 0 ){                                            \
    fprintf(stderr,"NIML: write abort!\n");                  \
    NI_free(wbuf); NI_free(bbuf); NI_free(cbuf); return -1;  \
  } else ntot+=nout

   /*-- loop over output structs,
        format for output into wbuf, and then send to output stream --*/

   ntot = 0 ;  /* total number of bytes output to stream */

   for( row=0 ; row < ndat ; row++ ){

     ptr = dat + fsiz*row ;   /* pointer to start of this struct */

     /* expand write buffer if this type contains variable sized array(s) */

     if( vsiz ){
       jj = NI_rowtype_vsize( rt , ptr ) ;   /* size of struct, w/ var arrays */
       if( tmode == NI_TEXT_MODE ) jj *= 6 ;
       if( jj > nwbuf ){                     /* did it get bigger? */
         nwbuf = jj ;
         wbuf  = NI_realloc(wbuf,nwbuf+128) ;
         if( tmode == NI_BASE64_MODE ){          /* expand Base64 stuff, too */
           bbuf = NI_realloc(bbuf,  nwbuf+128) ;
           cbuf = NI_realloc(cbuf,2*nwbuf+128) ;
         }
       }
     }

     /* initialize write buffer for this struct */

     switch( tmode ){
       case NI_TEXT_MODE:    wbuf[0] = '\0'; break; /* clear buffer */
       case NI_BASE64_MODE:
       case NI_BINARY_MODE:  jj = 0 ;        break; /* clear byte count */
     }

     /* write each part into the buffer */
     /* in text mode, strlen(wbuf) keeps track of number of bytes;
        in binary mode, jj keeps track of number of bytes written */

     for( ii=0 ; ii < rt->part_num ; ii++ ){  /*-- loop over parts --*/

       if( rt->part_dim[ii] < 0 ){             /*-- a single value --*/
         switch( tmode ){      /*-- output method (text or binary) --*/

           case NI_TEXT_MODE:         /*-- sprintf value to output --*/
             NI_val_to_text( rt->part_typ[ii],
                             ptr+rt->part_off[ii], wbuf ) ;
           break ;

           case NI_BASE64_MODE:       /*-- memcpy values to output --*/
           case NI_BINARY_MODE:
             jj += NI_val_to_binary( rt->part_typ[ii],
                                     ptr+rt->part_off[ii], wbuf+jj ) ;
           break ;
         }

       } else {                      /*-- variable dimension array --*/

         char **apt = (char **)(ptr+rt->part_off[ii]); /* data in struct */
                                                      /* is ptr to array */

         dim = ROWTYPE_part_dimen(rt,ptr,ii) ;  /* dimension of part */
         if( dim > 0 && *apt != NULL ){
           switch( tmode ){
             case NI_TEXT_MODE:
               NI_multival_to_text( rt->part_typ[ii] , dim ,
                                    *apt , wbuf ) ;
             break ;
             case NI_BASE64_MODE:
             case NI_BINARY_MODE:
               jj += NI_multival_to_binary( rt->part_typ[ii] , dim ,
                                            *apt , wbuf+jj ) ;
             break ;
           }
         }
       }

     } /* end of loop over parts in this struct */

     /*- actually write the data in wbuf out -*/

     switch( tmode ){

       case NI_TEXT_MODE:     /* each struct is on a separate line */
         strcat(wbuf,"\n") ;
         nout = NI_stream_writestring( ns , wbuf ) ;
         ADDOUT ;
       break ;

       case NI_BINARY_MODE:   /* jj bytes of binary in wbuf */
         nout = NI_stream_write( ns , wbuf , jj ) ;
         ADDOUT ;
       break ;

       case NI_BASE64_MODE:{  /* convert binary triples into base64 quads */
         int nb , nb3 , nb64 , pp,qq ;
         byte a,b,c,w,x,y,z ;

         /* bbuf = bb bytes of unprocessed data from last struct
                   plus jj bytes of data from new struct
                   (bb will be 0 or 1 or 2)                     */

         memcpy(bbuf+bb,wbuf,jj) ;       /* add wbuf to tail of bbuf */
         nb = jj+bb ;                    /* number of bytes in bb */
         if( nb < 3 ){ bb = nb; break; } /* need at least 3 bytes */
         nb3 = 3*(nb/3) ;                /* will encode nb3 bytes */

         /* cbuf = base64 output buffer */
         /* cc   = # bytes written since last EOL */

         for( qq=pp=0 ; pp < nb3 ; ){
           a = bbuf[pp++] ; b = bbuf[pp++] ; c = bbuf[pp++] ;
           B64_encode3(a,b,c,w,x,y,z) ;
           cbuf[qq++] = w ; cbuf[qq++] = x ;
           cbuf[qq++] = y ; cbuf[qq++] = z ;
           cc += 4; if( cc > 64 ){ cbuf[qq++]=B64_EOL2; cc=0; }
         }

         /* write base64 bytes to output */

         nout = NI_stream_write( ns , cbuf , qq ) ;
         ADDOUT ;

         /* deal with leftover bytes in bbuf */

         bb = nb - nb3 ;  /* num leftover bytes = 0, 1, or 2 */
         if( bb > 0 ){
           bbuf[0] = bbuf[nb3] ;                /* copy leftovers   */
           if( bb > 1 ) bbuf[1] = bbuf[nb3+1] ; /* to front of bbuf */
         }
       }
       break ;
     }

   } /* end of loop over output structs (row) */

   /* in Base64 mode, we might have to clean
      up if there are any leftover bytes in bbuf */

   if( tmode == NI_BASE64_MODE && bb > 0 ){
     byte w,x,y,z ;
     if( bb == 2 ) B64_encode2(bbuf[0],bbuf[1],w,x,y,z) ;
     else          B64_encode1(bbuf[0],w,x,y,z) ;
     cbuf[0] = w ; cbuf[1] = x ;
     cbuf[2] = y ; cbuf[3] = z ; cbuf[4] = B64_EOL2 ;
     nout = NI_stream_write( ns , cbuf , 5 ) ;
     ADDOUT ;
   }

   /*-- cleanup and return --*/

   NI_free(cbuf) ; NI_free(bbuf) ; NI_free(wbuf) ;

   return ntot ;
}
