#include "niml_private.h"

/***************************************************************************/
/***** Stuff for defining rowtypes from strings.  RWCox - 09 Dec 2002. *****/
/***************************************************************************/

/*-------------------------------------------------------------------------*/

/* These typedefs are used to find the byte alignment
   the compiler imposes on the 'basic' types supported by NIML. */

typedef struct { char a; byte    b; } Qadgop_byte    ;  /* not a Mercotan */
typedef struct { char a; short   b; } Qadgop_short   ;
typedef struct { char a; int     b; } Qadgop_int     ;
typedef struct { char a; float   b; } Qadgop_float   ;
typedef struct { char a; double  b; } Qadgop_double  ;
typedef struct { char a; complex b; } Qadgop_complex ;
typedef struct { char a; rgb     b; } Qadgop_rgb     ;
typedef struct { char a; rgba    b; } Qadgop_rgba    ;

/* Arrays to hold the alignments, sizes, and names of the basic types. */

static int   type_alignment[NI_NUM_BASIC_TYPES+1] ;
static int   type_size     [NI_NUM_BASIC_TYPES+1] ;
static char *type_name     [NI_NUM_BASIC_TYPES+1] = {
  "byte"  , "short"  , "int"     ,
  "float" , "double" , "complex" ,
  "rgb"   , "rgba"   , "String"
} ;

static char *type_alias[NI_NUM_BASIC_TYPES+1] = {   /* aliases */
  "uint8"   , "int16"   , "int32"     ,
  "float32" , "float64" , "complex64" ,
  "rgb8"    , "rgba8"   , "CString"
} ;

/* these are used to find/save the alignment and size of pointers */

typedef struct { char a; void *b; } Qadgop_pointer ;
static int pointer_alignment ;
static int pointer_size ;

/*---------------------------------------------------------------*/
/*! The Htable of user-defined rowtypes, indexed by type name.   */

static Htable *rowtype_table = NULL ;

/*! The array of user-defined rowtypes, indexed by type code.
    Note that rowtypes are never deleted - they just accumulate. */

static NI_rowtype **rowtype_array = NULL ;
static int         rowtype_num    = 0    ;
/*---------------------------------------------------------------*/

/*! Rowtype code for first user-defined type. */

#define ROWTYPE_OFFSET     1001

/*! Used to set the code for each new user-defined type.
    (The '-1' is to allow for the String type, which isn't
     a basic type but is a builtin type.) */

#define ROWTYPE_BASE_CODE (ROWTYPE_OFFSET-NI_NUM_BASIC_TYPES-1)

/*! Check if a rowtype code is a derived type or a builtin type */

#define ROWTYPE_is_builtin_code(cc) ((cc) >= 0 && (cc) < ROWTYPE_OFFSET)

/*! Register a rowtype into the table and array. */

#define ROWTYPE_register(rr)                               \
 do{ int nn ;                                              \
     if( rowtype_table == NULL ) setup_basic_types() ;     \
     addto_Htable( (rr)->name , (rr) , rowtype_table ) ;   \
     nn = rowtype_num + 1 ;                                \
     rowtype_array = NI_realloc( rowtype_array ,           \
                                 NI_rowtype*,              \
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

   type_alignment[NI_BYTE   ] = offsetof(Qadgop_byte   ,b) ;
   type_alignment[NI_SHORT  ] = offsetof(Qadgop_short  ,b) ;
   type_alignment[NI_INT    ] = offsetof(Qadgop_int    ,b) ;
   type_alignment[NI_FLOAT  ] = offsetof(Qadgop_float  ,b) ;
   type_alignment[NI_DOUBLE ] = offsetof(Qadgop_double ,b) ;
   type_alignment[NI_COMPLEX] = offsetof(Qadgop_complex,b) ;
   type_alignment[NI_RGB    ] = offsetof(Qadgop_rgb    ,b) ;
   type_alignment[NI_RGBA   ] = offsetof(Qadgop_rgba   ,b) ;

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
     rt->size        = type_size[ii] ;          /* size of "struct" */
     rt->psiz        = rt->size ;               /* size of all parts */
     rt->algn        = type_alignment[ii] ;     /* byte alignment */
     rt->name        = NI_strdup(type_name[ii]);
     rt->userdef     = NI_strdup(type_name[ii]);
     rt->flag        = 0 ;

     rt->comp_num    = 1 ;                           /* basic types have */
     rt->comp_typ    = NI_malloc(int, sizeof(int)) ; /* only one component */
     rt->comp_typ[0] = ii ;
     rt->comp_dim    = NI_malloc(int, sizeof(int)) ;
     rt->comp_dim[0] = -1 ;                          /* fixed dim component */

     rt->part_num    = 1 ;                           /* basic types have */
     rt->part_typ    = NI_malloc(int, sizeof(int)) ; /* only one part */
     rt->part_typ[0] = ii ;
     rt->part_off    = NI_malloc(int, sizeof(int)) ;
     rt->part_off[0] = 0 ;
     rt->part_siz    = NI_malloc(int, sizeof(int)) ;
     rt->part_siz[0] = type_size[ii] ;
     rt->part_dim    = NI_malloc(int, sizeof(int)) ;
     rt->part_dim[0] = -1 ;                          /* fixed dim part */
     rt->part_rtp    = NI_malloc(NI_rowtype*, sizeof(NI_rowtype *)) ;
     rt->part_rtp[0] = rt ;

     ROWTYPE_register( rt ) ;                        /* put in the Htable */
   }

   /* alignment and size of pointers */

   pointer_alignment = offsetof(Qadgop_pointer,b) ;
   pointer_size      = sizeof(void *) ;

   /* insert a special rowtype for String (really a pointer) */

   type_alignment[NI_STRING] = pointer_alignment ;
   type_size     [NI_STRING] = pointer_size ;

   rt              = NI_new( NI_rowtype ) ;
   rt->code        = NI_STRING ;
   rt->size        = pointer_size ;
   rt->psiz        = 0 ;                       /* variable dim */
   rt->algn        = pointer_alignment ;
   rt->name        = NI_strdup("String") ;
   rt->userdef     = NI_strdup("String") ;
   rt->flag        = ROWTYPE_VARSIZE_MASK ;    /* variable dim */

   rt->comp_num    = 1 ;
   rt->comp_typ    = NI_malloc(int, sizeof(int)) ;
   rt->comp_typ[0] = NI_STRING ;
   rt->comp_dim    = NI_malloc(int, sizeof(int)) ;
   rt->comp_dim[0] = -1 ;

   rt->part_num    = 1 ;
   rt->part_typ    = NI_malloc(int, sizeof(int)) ;
   rt->part_typ[0] = NI_STRING ;
   rt->part_off    = NI_malloc(int, sizeof(int)) ;
   rt->part_off[0] = 0 ;
   rt->part_siz    = NI_malloc(int, sizeof(int)) ;
   rt->part_siz[0] = pointer_size ;
   rt->part_dim    = NI_malloc(int, sizeof(int)) ;
   rt->part_dim[0] = -1 ;

   rt->part_rtp    = NI_malloc(NI_rowtype*, sizeof(NI_rowtype *)) ;
   rt->part_rtp[0] = rt ;

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
    - tname = Name to call this thing (must be unique and can't be one
              of the builtin types listed below).
    - tdef = Definition of the type:
       - A list of type names ('components') separated by commas.
       - A type name is one of the NIML basic types
         ("byte", "short", "int", "float", "double", "complex", "rgb", "rgba"),
         or is "String",
         or is a previously defined tname from a another NI_rowtype.
       - Recursive types cannot be defined; that is, no component of
         tdef can refer to the same tname that is being defined now.
       - A type name may be preceded/appended by an integer count,
         as in "int,4*float" or "int,float[4]".  These are equivalent
         to "int,float,float,float,float" (just shorter).
       - Variable dimension arrays may be defined, IF the size of the
         array is an int component of this new rowtype which occurs before
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

   Also see
    - NI_rowtype_find_name()
    - NI_rowtype_find_code()
    - NI_rowtype_name_to_code()
    - NI_rowtype_code_to_name()
----------------------------------------------------------------------------*/

int NI_rowtype_define( char *tname , char *tdef )
{
   NI_rowtype *rt , *qt ;
   int ii,jj , id,jd,kd,isdim,nn , almax,cbase,np,pb , last_size ;
   NI_str_array *sar ;
   char *tp,*sp,*bp , str[256] ;

   /*-- check inputs --*/

   if( !NI_is_name(tname) )              ERREX("bad typename") ;
   if( strlen(tname) > 255 )             ERREX("overlong typename") ;
   if( tdef  == NULL || *tdef  == '\0' ) ERREX("empty type definition") ;

   /*-- create Htable of basic types, if not already defined --*/

   if( rowtype_table == NULL ) setup_basic_types() ;

   /*-- see if type name already defined --*/
   /*-- 25 Mar 2003: if it is, return the old code --*/

   rt = NI_rowtype_find_name( tname ) ;
   if( rt != NULL ){
     if( strcmp(rt->userdef,tdef) != 0 ){
       fprintf(stderr,
                "++ NI_rowtype_define: illegal attempt to redefine type '%s'\n"
                "++          old definition: %s\n"
                "++ (failed) new definition: %s\n" ,
               tname , rt->userdef , tdef ) ;
     }
     return rt->code ;
   }

   /*-- break defining string into components --*/

   sar = NI_decode_string_list( tdef , ",;" ) ;

   if( sar == NULL || sar->num < 1 ){
     NI_free(sar) ; ERREX("illegal definition") ;
   }

   /*-- initialize the new rowtype --*/

   rt          = NI_new( NI_rowtype ) ;
   rt->name    = NI_strdup( tname ) ;
   rt->userdef = NI_strdup( tdef ) ;
   rt->flag    = 0 ;

   /*-- loop over components in tdef, loading the new rt with their info --*/

   rt->part_num = rt->comp_num = 0 ;

   for( ii=0 ; ii < sar->num ; ii++ ){

     tp = sar->str[ii] ;
     id = 0 ; kd = strlen(tp) ; /* type name of part will be in tp[id..kd-1] */
     if( kd == 0 ){
      delete_rowtype(rt); NI_delete_str_array(sar); ERREX("empty component name?");
     }

     /* get count, if present, into jd */

     sp = strchr(tp,'*') ;   /* format of component string: count*type  */
     bp = strchr(tp,'[') ;   /* format of component string: type[count] */

     if( sp != NULL || bp != NULL ){            /*** a count is present ***/

       if( sp != NULL && bp != NULL ){          /* can't have both forms! */
        delete_rowtype(rt); NI_delete_str_array(sar); ERREX("two repeat counts?");
       }

       if( sp != NULL ){                        /* format: count*type */
         nn = 0 ;                               /*  - count starts at nn */
         id = (sp-tp)+1 ;                       /*  - type name starts at id */
       } else {                                 /* format: type[count] */
         kd = (bp-tp) ;                         /*  - type name ends at kd-1 */
         nn = kd+1 ;                            /*  - count starts at nn */
       }

       jd = -1 ;
       if( tp[nn] != '#' ){                     /* count is a plain number */
         isdim = 0 ;
         sscanf( tp+nn , "%d" , &jd ) ;
         if( jd <= 0 ){
          delete_rowtype(rt); NI_delete_str_array(sar); ERREX("bad repeat number");
         }
       } else {                                 /* count is a #reference */
         isdim = 1 ;
         sscanf( tp+nn+1 , "%d" , &jd ) ;       /* ref must be to index */
         if( jd <= 0 || jd > ii ){              /* before this component */
           delete_rowtype(rt); NI_delete_str_array(sar); ERREX("bad #index");
         }
         if( rt->comp_typ[jd-1] != NI_INT ||    /* ref must be to an int */
             rt->comp_dim[jd-1] >= 0        ){  /* of fixed dim (1 int) */
           delete_rowtype(rt); NI_delete_str_array(sar); ERREX("non-int #index");
         }
       }
     } else {
       isdim = 0 ; jd = 1 ;                     /* default count of 1 */
     }

     /* get the type of this component from its name */

     if( kd-id < 1 || kd-id > 255 ){
      delete_rowtype(rt); NI_delete_str_array(sar); ERREX("toolong component name");
     }

     NI_strncpy( str , tp+id , kd-id+1 ) ;  /* copy component name into str */
     qt = NI_rowtype_find_name( str ) ;     /* look it up in the table */
     if( qt == NULL ){
       delete_rowtype(rt); NI_delete_str_array(sar); ERREX("bad component type");
     }

     if( !isdim ){  /*** fixed count: add jd copies of this component type ***/

       rt->comp_typ = NI_realloc( rt->comp_typ, int, sizeof(int)*(rt->comp_num+jd) );
       rt->comp_dim = NI_realloc( rt->comp_dim, int, sizeof(int)*(rt->comp_num+jd) );

       for( jj=0 ; jj < jd ; jj++ ){
         rt->comp_typ[rt->comp_num + jj] = qt->code ;
         rt->comp_dim[rt->comp_num + jj] = -1 ;        /* fixed dim part */
       }

       rt->comp_num += jd ;                 /* have more components now */
       rt->part_num += jd * qt->part_num ;  /* have more parts now */

       if( ROWTYPE_is_varsize(qt) )         /* if component is variable dim, */
         rt->flag |= ROWTYPE_VARSIZE_MASK ; /* mark rowtype as variable dim  */

     } else {       /*** variable count: add 1 component that is a pointer   */
                    /***                 to an array of fixed dim elements,  */
                    /***                 dimension given in component #jd    */

       /* but can't have a var dim array of var dim arrays! */

       if( ROWTYPE_is_varsize(qt) ){
         delete_rowtype(rt); NI_delete_str_array(sar);
         ERREX("variable dim array must have fixed dim type");
       }

       rt->comp_typ = NI_realloc( rt->comp_typ, int, sizeof(int)*(rt->comp_num+1) );
       rt->comp_dim = NI_realloc( rt->comp_dim, int, sizeof(int)*(rt->comp_num+1) );

       rt->comp_typ[rt->comp_num] = qt->code ;  /* type this points to */
       rt->comp_dim[rt->comp_num] = jd-1 ;      /* which component has */
                                                /* array dimension count */
       rt->comp_num ++ ;  /* 1 more component */
       rt->part_num ++ ;  /* and 1 more part */

       rt->flag |= ROWTYPE_VARSIZE_MASK ;   /* mark rowtype as variable dim */

     }

   } /* end of loop over components */

   NI_delete_str_array(sar) ;                  /* done with this string array */

   if( rt->part_num == 0 ){ delete_rowtype(rt); ERREX("no components?"); }

   /*** now loop over components, breaking them down into their parts,
        storing the part types and their offsets into the C struct    ***/

   rt->part_off = NI_malloc(int, sizeof(int)          * rt->part_num ) ;
   rt->part_typ = NI_malloc(int, sizeof(int)          * rt->part_num ) ;
   rt->part_dim = NI_malloc(int, sizeof(int)          * rt->part_num ) ;
   rt->part_siz = NI_malloc(int, sizeof(int)          * rt->part_num ) ;
   rt->part_rtp = NI_malloc(NI_rowtype*, sizeof(NI_rowtype *) * rt->part_num ) ;

   almax = 1 ;  /* will be largest type_alignment of any part */
   cbase = 0 ;  /* base offset into struct for next component */
   id    = 0 ;  /* part number we are about to work on */

   for( ii=0 ; ii < rt->comp_num ; ii++ ){

                                    /*** component is a      ***/
     if( rt->comp_dim[ii] >= 0 ){   /*** variable dim array  ***/
                                    /*** ==> store 1 pointer ***/

       if( pointer_alignment > 1 ){            /* make sure cbase */
         jd = cbase % pointer_alignment ;      /* is aligned OK  */
         if( jd > 0 ) cbase += (pointer_alignment-jd) ;
       }

       /* Note that this is the only case where a part_typ
          might end up as a derived type - normally, part_typ
          will be a builtin type code (NI_BYTE .. NI_STRING).
          Note the limitation that the type of variable dim
          arrays be a fixed dim type.                        */

       rt->part_typ[id] = rt->comp_typ[ii] ;
       rt->part_off[id] = cbase ;
       rt->part_siz[id] = pointer_size ;
       rt->part_rtp[id] = NI_rowtype_find_code( rt->part_typ[id] ) ;

       /* count number of parts before the dimension component into kd */
       /* so we can store the part index of this dimension component   */

       for( jd=kd=0 ; jd < rt->comp_dim[ii] ; jd++ ){
         if( rt->comp_dim[jd] >= 0 ){   /* this component is a pointer itself */
           kd++ ;
         } else {                      /* this component has fixed dim parts */
           qt = NI_rowtype_find_code( rt->comp_typ[jd] ) ;
           kd += qt->part_num ;
         }
       }
       rt->part_dim[id] = kd ;    /* which part is the dimension of this part */

       kd = pointer_alignment ;
       if( kd > almax ) almax = kd ;

       id++ ; cbase += pointer_size ;

     } else {      /*** fixed dim type, possibly with multiple parts ***/

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
       rt->part_rtp[id] = NI_rowtype_find_code( rt->part_typ[id] ) ;

       kd = rt->part_rtp[id]->algn ;                 /* alignment of part */
       if( kd > almax ) almax = kd ;   /* keep track of largest alignment */

       last_size = rt->part_rtp[id]->size ;           /* size of 1st part */
       rt->part_siz[id] = last_size ;

       id++ ;  /* prepare to add next part */

       /* loop over rest of parts from this component */

       for( jj=1 ; jj < np ; jj++,id++ ){

         rt->part_typ[id] = qt->part_typ[jj] ;     /* type of new part      */
         rt->part_rtp[id] = NI_rowtype_find_code( rt->part_typ[id] ) ;

         if( qt->part_dim[jj] < 0 ){        /******* fixed dim part        **/

           nn = last_size ;                        /* # bytes in last part  */
           jd = rt->part_off[id-1] ;               /* offset of last part   */
           kd = rt->part_rtp[id]->algn ;           /* how to align new part */
           if( kd > almax ) almax = kd ; /* keep track of largest alignment */

           nn += jd ;  /* next available byte = sum of last offset and size */
           if( kd > 1 ){                           /* must move nn up if    */
             jd = nn % kd ;                        /* not on exact multiple */
             if( jd > 0 ) nn += (kd-jd) ;          /* of jj bytes alignment */
           }
           rt->part_off[id] = nn ;
           rt->part_dim[id] = -1 ;                /* mark as fixed dim part */

           last_size = rt->part_rtp[id]->size ;        /* size of this part */
           rt->part_siz[id] = last_size ;

         } else {                           /***** variable dim array part **/

           nn = last_size ;
           jd = rt->part_off[id-1] ;
           kd = pointer_alignment ;        /* we are storing a pointer here */
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

     } /* end of fixed dim component part-izing */

   } /* end of loop over components */

   /* now compute the overall size of this new rowtype:
      at this point,
      cbase = next byte offset available after last part;
      this would be the size, but may have to be pushed
      up to allow for byte alignment of this rowtype     */

   rt->algn = almax ;
   if( rt->algn > 1 ){
     jd = cbase % rt->algn ;
     if( jd > 0 ) cbase += (rt->algn-jd) ;
   }
   rt->size = cbase ;  /* this size is the sizeof(struct),
                          and doesn't include var dim arrays or
                          Strings, just the pointers to those things */

   /* 26 Dec 2002: Compute the sum of the part sizes
                   (zero if this has variable dim arrays).
                   If rt->psiz == rt->size, then
                   struct is stored without padding bytes. */

   rt->psiz = 0 ;
   if( !ROWTYPE_is_varsize(rt) ){
     for( ii=0 ; ii < rt->part_num ; ii++ )
       rt->psiz += rt->part_siz[ii] ;
   }

   /* 28 Oct 2004: Move assignment of the new rowtype code to the end,
                   since a recursive call via NI_rowtype_find_name()
                   might have created a new rowtype before this one.
                   An example definition: "int,VECTOR_float_32,int".  */

   rt->code = ROWTYPE_BASE_CODE + rowtype_num ;

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
/*! Make an 'ni_do' element that defines a given rowtype. */

NI_procins * NI_rowtype_procins( NI_rowtype *rt )  /* 19 Apr 2005 */
{
   NI_procins *npi ;
   char *rhs ;

   if( rt == NULL ) return NULL ;

   npi = NI_new_processing_instruction( "ni_do" ) ;
   NI_set_attribute( npi , "ni_verb" , "typedef" ) ;

   rhs = NI_malloc(char,strlen(rt->name)+strlen(rt->userdef)+4) ;
   sprintf( rhs , "%s %s" , rt->name , rt->userdef ) ;
   NI_set_attribute( npi , "ni_object" , rhs ) ;
   NI_free( rhs ) ;
   return npi ;
}

/*--------------------------------------------------------------------*/
/*! Find a rowtype by its name.
    19 Feb 2003: or its alias.
    28 Oct 2004: If name is of form VECTOR_basictype_length,
                 then a new rowtype is created on the spot;
                 e.g., "VECTOR_float_32" is like "float[32]". */

NI_rowtype * NI_rowtype_find_name( char *nn )
{
   NI_rowtype *rt ; int ii ;
   static int busy=0 ;       /* 28 Oct 2004: prevent recursion */

   if( nn == NULL || *nn == '\0' ) return NULL ;
   if( rowtype_table == NULL ) setup_basic_types() ;
   rt = (NI_rowtype *) findin_Htable(nn,rowtype_table) ;
   if( rt != NULL ) return rt ;

   /* 19 Feb 2003: linear search for basic type alias */

   for( ii=0 ; ii <= NI_NUM_BASIC_TYPES ; ii++ )
     if( strcmp(type_alias[ii],nn) == 0 ) return rowtype_array[ii] ;

   /*-- 28 Oct 2004: Define fixed size vector types here and now:
                     format of nn must be VECTOR_basictype_length --*/

   if( busy ) return NULL ;   /* cannot allow re-entry below here! */

   ii = strlen(nn) ;
   if( ii < 12 || strncmp(nn,"VECTOR_",7) != 0 || strchr(nn+7,'_') == NULL )
     return NULL ;

   { char bt[32] , rt[64] ; int tt , dd ;

     /* extract basic type name (after "VECTOR_") into bt array */

     for( ii=7 ; ii < 32 && nn[ii] != '\0' && nn[ii] != '_' ; ii++ )
       bt[ii-7] = nn[ii] ;
     if( nn[ii] != '_' ) return NULL ;   /* bad end of basic type name */
     bt[ii-7] = '\0' ;                   /* terminate with NUL byte */

     /* find bt name in basic type name list (or alias list) */

     for( tt=0 ; tt <= NI_NUM_BASIC_TYPES ; tt++ )
       if( strcmp(type_name[tt],bt) == 0 ) break ;

     if( tt > NI_NUM_BASIC_TYPES ){
       for( tt=0 ; tt <= NI_NUM_BASIC_TYPES ; tt++ )
         if( strcmp(type_alias[tt],bt) == 0 ) break ;
       if( tt > NI_NUM_BASIC_TYPES ) return NULL ;
     }

     /* find dimension after the nn[ii] character, which is '_' */

     dd = 0 ; sscanf( nn+ii+1 , "%d" , &dd ) ;
     if( dd <= 0 ) return NULL ;

     /* ready to create a new rowtype now */

     sprintf(rt,"%s[%d]",type_name[tt],dd) ;

     busy = 1 ;                        /* prevent recursion!!! */
     tt = NI_rowtype_define( nn , rt ) ;
     busy = 0 ;
     if( tt >= ROWTYPE_OFFSET ) return rowtype_array[tt-ROWTYPE_BASE_CODE] ;
   }

   return NULL ;
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
/*! Given a rowtype code, find its alias name.  This is only valid
    for builtin types; for user-defined types, the return value is
    the user-supplied type name string.  Don't free this string!
----------------------------------------------------------------------*/

char * NI_rowtype_code_to_alias( int nn )   /* 19 Feb 2003 */
{
   if( rowtype_table == NULL ) setup_basic_types() ;
   if( nn <= NI_NUM_BASIC_TYPES ) return type_alias[nn] ;
   return NI_rowtype_code_to_name( nn ) ;
}

/*--------------------------------------------------------------------*/
/*! Given a rowtype name, find its struct size in bytes.
    - Returns -1 if rowtype not found.
    - Note that if the rowtype contains variable dim arrays, this
      size returned here is just the size of the storage for
      the basic struct with pointers, not including the variable
      arrays.
    - See NI_rowtype_vsize() to get the size of a rowtype instance
      including its variable dim arrays.
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

   if( rowtype_table == NULL )  setup_basic_types() ;
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
/*! Compute the size of all the data in a struct defined in a NI_rowtype
    (not including padding), for this instance of the struct,
    allowing for variable array parts.  Zero is returned if something
    bad happens.
-------------------------------------------------------------------------*/

int NI_rowtype_vsize( NI_rowtype *rt , void *dpt )
{
   int ii,jj , ss ;
   char *dat = (char *)dpt ;

   if( rt == NULL              ) return 0;        /* nonsense input */
   if( !ROWTYPE_is_varsize(rt) ) return rt->psiz; /* fixed dim struct */
   if( dat == NULL             ) return 0;        /* var size with no data? */

   /* loop over parts, adding up part sizes,
      including var dim arrays and String parts */

   for( ii=ss=0 ; ii < rt->part_num ; ii++ ){
     if( rt->part_typ[ii] == NI_STRING ){      /* String is special */
       char *str = *((char **)((dat) + (rt)->part_off[ii])) ;
       ss += NI_strlen(str) ;
     } else if( rt->part_dim[ii] < 0 ){        /* 1 fixed dim type */
       ss += rt->part_siz[ii] ;
     } else {                                  /* var dim array */
       jj = ROWTYPE_part_dimen(rt,dat,ii) ;    /* array size */
       ss += jj * rt->part_rtp[ii]->psiz ;     /* size of all parts */
     }                                         /* in var dim array */
   }

   return ss ;
}

/*-------------------------------------------------------------------------*/
/*! Encode 1 type value at the end of the text string wbuf (which is
    assumed to be plenty long).  typ must be a fixed dim type code,
    or NI_STRING.  Structs with var dim arrays must be handled separately.
---------------------------------------------------------------------------*/

void NI_val_to_text( NI_rowtype *rt , char *dpt , char *wbuf )
{
   int jj = strlen(wbuf) ;

   switch( rt->code ){

     /*-- a derived type (will not contain var dim arrays) --*/

     default:{
       if( rt != NULL ){
         int ii ;
         for( ii=0 ; ii < rt->part_num ; ii++ )   /* recursion */
           NI_val_to_text( rt->part_rtp[ii] , dpt + rt->part_off[ii] , wbuf ) ;
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
    typ must be a fixed dim type code, or NI_STRING.
---------------------------------------------------------------------------*/

void NI_multival_to_text( NI_rowtype *rt , int nv , char *dpt , char *wbuf )
{
   int ii , jj=rt->size ;

   for( ii=0 ; ii < nv ; ii++ )
     NI_val_to_text( rt , dpt+ii*jj , wbuf ) ;
}

/*-------------------------------------------------------------------------*/
/*! Copy 1 fixed dim type (no String or var dim array parts here) value
    in binary format to the wbuf.
    - Return value is number of bytes written.
    - Note that only data bytes are written, not any padding between parts.
---------------------------------------------------------------------------*/

int NI_val_to_binary( NI_rowtype *rt , char *dpt , char *wbuf )
{
   int jj=0 ;  /* will be return value */

   if( rt->size == rt->psiz ){        /* fixed dim, unpadded struct */

     jj = rt->size ;
     memcpy(wbuf,dpt,jj) ;

   } else if( !ROWTYPE_is_varsize(rt) ){  /* derived fixed dim type */
                                  /* ==> write each part separately */
     int ii ;
     for( ii=0 ; ii < rt->part_num ; ii++ ){
       memcpy(wbuf+jj,dpt+rt->part_off[ii],rt->part_siz[ii]) ;
       jj += rt->part_siz[ii] ;
     }

   }

   return jj ;
}

/*-------------------------------------------------------------------------*/
/*! Copy nv fixed dim type values in binary format to the wbuf.
     - Return value is number of bytes written.
     - wbuf is assumed big enough to take the load
---------------------------------------------------------------------------*/

int NI_multival_to_binary( NI_rowtype *rt , int nv , char *dpt , char *wbuf )
{
   int jj=0 ;

   if( rt->size == rt->psiz ){         /* fixed dim, unpadded structs  */
                                       /* ==> Write all data at once   */
     jj = nv * rt->size ;
     memcpy(wbuf,dpt,jj);

   } else if( rt->psiz > 0 ){          /* Derived type is harder:      */
                                       /* Write each struct separately */
     int ii ;
     for( ii=0 ; ii < nv ; ii++ )
      jj += NI_val_to_binary( rt , dpt+(ii*rt->size) , wbuf+jj ) ;

   }
   return jj ;
}

/*------------------------------------------------------------------------*/
/*! Return 1 if the type contains a String part, 0 if not.
--------------------------------------------------------------------------*/

int NI_has_String( NI_rowtype *rt )
{
   int ii , jj ;

   if( rt == NULL ) return 0 ;

   /* test #1: if a NIML builtin type, test if it is String */

   if( ROWTYPE_is_builtin_code(rt->code) ) return (rt->code == NI_STRING) ;

   /* test the parts */

   for( ii=0 ; ii < rt->part_num ; ii++ ){
     if( ROWTYPE_is_builtin_code(rt->part_rtp[ii]->code) ){ /* builtin part */
       if( rt->part_rtp[ii]->code == NI_STRING ) return 1;
     } else {                                              /* derived part */
       if( NI_has_String( rt->part_rtp[ii] )   ) return 1; /* recursion */
     }
   }
   return 0 ;
}

/*------------------------------------------------------------------------*/
/*! Write one column of structs to the output stream.  Now superseded
    by NI_write_columns().
--------------------------------------------------------------------------*/

int NI_write_rowtype( NI_stream_type *ns , NI_rowtype *rt ,
                      int ndat , void *dat , int tmode )
{
   void *dpt = dat ;
   if( rt == NULL ) return -1 ;
   return NI_write_columns( ns , 1 , &(rt->code) , ndat , &dpt , tmode ) ;
}

/*------------------------------------------------------------------------*/
/*! Write "columns" of data to a NI_stream.  Each column is an array of
    structs of some NI_rowtype (including the builtin types):
      - ns         = stream to write to
      - col_num    = number of columns to write (1,2,...)
      - col_typ[i] = type code for column #i (i=0..col_num-1)
      - col_len    = number of elements in each column
      - col_dpt[i] = pointer to data in column #i
      - tmode is one of
         - NI_TEXT_MODE   ==> ASCII output
           - text mode is required if any data component is a String
           - text mode is required to "str:" streams
         - NI_BINARY_MODE ==> binary output (endian-ness of this CPU)
         - NI_BASE64_MODE ==> binary/base64 output (ditto)
      - return value is number of bytes written to stream
        (-1 if something bad happened, 0 if can't write to stream yet)

   Only the data is written to the stream - no header or footer.
   This function is adapted from the 1st edition of NI_write_element().
--------------------------------------------------------------------------*/

int NI_write_columns( NI_stream_type *ns,
                      int col_num , int   *col_typ ,
                      int col_len , void **col_dpt , int tmode )
{
   int ii,jj , row , dim , ntot,nout , col ;
   char *ptr , **col_dat=(char **)col_dpt ;
   int  nwbuf,bb=0,cc=0;
   char *wbuf=NULL ; /* write buffer */
   char *bbuf=NULL ; /* copy of write buffer */
   char *cbuf=NULL ; /* Base64 buffer */

   NI_rowtype **rt=NULL ;  /* array of NI_rowtype, 1 per column */
   int *vsiz=NULL , vsiz_tot=0 ;
   int *fsiz=NULL , fsiz_tot=0 ;

# undef  FREEUP
# define FREEUP do{ NI_free(wbuf); NI_free(bbuf); NI_free(cbuf); \
                    NI_free(rt)  ; NI_free(vsiz); NI_free(fsiz); \
                } while(0)

   /*-- check inputs --*/

   if( col_num <= 0    || col_len <= 0    ) return  0 ;
   if( col_typ == NULL || col_dat == NULL ) return -1 ;
   if( !NI_stream_writeable(ns)           ) return -1 ;

#if 0
fprintf(stderr,"NI_write_columns: col_num=%d col_len=%d tmode=%d\n",col_num,col_len,tmode) ;
#endif

   /*-- check stream --*/

   if( ns->bad ){                        /* not connected yet? */
     jj = NI_stream_goodcheck(ns,666) ;  /* try to connect it */
     if( jj < 1 ) return jj ;            /* 0 is nothing yet, -1 is death */
   }
#if 1
   jj = NI_stream_writecheck(ns,666) ;
   if( jj < 0 ) return jj ;              /* only exit if stream is actually bad */
#endif

   if( ns->type == NI_STRING_TYPE )  /* output to string buffer ==> text mode */
     tmode = NI_TEXT_MODE ;

   /* create array of NI_rowtype for columns, etc. */

   rt   = NI_malloc(NI_rowtype*, sizeof(NI_rowtype *) * col_num ) ;
   vsiz = NI_malloc(int,  sizeof(int)          * col_num ) ;
   fsiz = NI_malloc(int,  sizeof(int)          * col_num ) ;
   for( col=0 ; col < col_num ; col++ ){

     /* convert column type code to rowtype pointer */

     rt[col] = NI_rowtype_find_code( col_typ[col] ) ;

     /* can't find type, or no data in column?  take this job and shove it */

     if( rt[col] == NULL || col_dat[col] == NULL ){ FREEUP; return -1; }

     vsiz[col] = ROWTYPE_is_varsize(rt[col]) ;         /* variable dim type? */
     fsiz[col] = rt[col]->size ;         /* fixed size of struct (w/padding) */
     vsiz_tot += vsiz[col] ;
     fsiz_tot += fsiz[col] ;

     /* can only write String parts in text mode */

     if( tmode != NI_TEXT_MODE && NI_has_String(rt[col]) ) tmode = NI_TEXT_MODE;
   }

   /*-- Special (and fast) case:
        one compact (no padding) fixed-size rowtype,
        and binary output ==> can write all data direct to stream at once --*/

   if( col_num == 1 && tmode == NI_BINARY_MODE && fsiz[0] == rt[0]->psiz ){
#if 0
int ct = NI_clock_time() ;
#endif
     nout = NI_stream_write( ns , col_dat[0] , fsiz[0]*col_len ) ;
#if 0
ct = NI_clock_time()-ct ;
fprintf(stderr,"NI_write_columns FAST case: %d bytes in %d ms\n",fsiz[0]*col_len,ct) ;
#endif
     FREEUP ; return nout ;
   }

   /*-- allocate space for the write buffer (1 row at a time) --*/

   switch( tmode ){
     default:             tmode = NI_TEXT_MODE ; /* fall through */
     case NI_TEXT_MODE:   nwbuf = 6*fsiz_tot ; break ;

     case NI_BASE64_MODE:
     case NI_BINARY_MODE: nwbuf =   fsiz_tot ; break ;
   }
   wbuf = NI_malloc(char, nwbuf+128) ;  /* 128 for the hell of it */

   /* create buffers for Base64 output, if needed */

   if( tmode == NI_BASE64_MODE ){
     bbuf = NI_malloc(char,   nwbuf+128) ; bb = 0 ;  /* binary buffer */
     cbuf = NI_malloc(char, 2*nwbuf+128) ; cc = 0 ;  /* base64 buffer */
     load_encode_table() ;
   }

   /* this macro take the 'nout' number of output bytes
      and adds into the running total ntot if all was well;
      if all was not well with the write, then it aborts the output */

# undef  ADDOUT
# define ADDOUT                              \
  if( nout < 0 ){                            \
    fprintf(stderr,"NIML:: write abort!\n"); \
    FREEUP ; return -1 ;                     \
  } else ntot+=nout

   /*-- loop over output rows,
        format for output into wbuf, and then send to output stream --*/

   ntot = 0 ;  /* total number of bytes output to stream */

   for( row=0 ; row < col_len ; row++ ){

     /* expand write buffer if any type contains variable dim array(s) */

     if( vsiz_tot ){
       for( jj=col=0 ; col < col_num ; col++ ){
        ptr = col_dat[col] + fsiz[col]*row ;     /* ptr to row-th element */
        jj += NI_rowtype_vsize( rt[col] , ptr ); /* size of data, w/var arrays */
       }
       if( tmode == NI_TEXT_MODE ) jj *= 6 ;
       if( jj > nwbuf ){                     /* did it get bigger? */
         nwbuf = jj ;
         wbuf  = NI_realloc(wbuf, char,nwbuf+128) ;
         if( tmode == NI_BASE64_MODE ){          /* expand Base64 stuff, too */
           bbuf = NI_realloc(bbuf, char,  nwbuf+128) ;
           cbuf = NI_realloc(cbuf, char,2*nwbuf+128) ;
         }
       }
     }

     /* initialize write buffer for this row */

     switch( tmode ){
       case NI_TEXT_MODE:    wbuf[0] = '\0'; break; /* clear buffer */
       case NI_BASE64_MODE:
       case NI_BINARY_MODE:  jj = 0 ;        break; /* clear byte count */
     }

     /* loop over columns, write each into the buffer */

     for( col=0 ; col < col_num ; col++ ){
      ptr = col_dat[col] + fsiz[col]*row ; /* ptr to row-th struct */
                                           /* in this columns      */

      /* write each part of this struct into the buffer */

      /* in text mode, strlen(wbuf) keeps track of number of bytes;
         in binary mode, jj keeps track of number of bytes written */

      for( ii=0 ; ii < rt[col]->part_num ; ii++ ){ /*-- loop over parts --*/

       if( rt[col]->part_dim[ii] < 0 ){             /*-- a single value --*/
         switch( tmode ){           /*-- output method (text or binary) --*/

           case NI_TEXT_MODE:              /*-- sprintf value to output --*/
             NI_val_to_text( rt[col]->part_rtp[ii],
                             ptr+rt[col]->part_off[ii], wbuf ) ;
           break ;

           case NI_BASE64_MODE:            /*-- memcpy values to output --*/
           case NI_BINARY_MODE:
             jj += NI_val_to_binary( rt[col]->part_rtp[ii],
                                     ptr+rt[col]->part_off[ii], wbuf+jj ) ;
           break ;
         }

       } else {                           /*-- variable dimension array --*/

         char **apt = (char **)(ptr+rt[col]->part_off[ii]); /* data in struct */
                                                           /* is ptr to array */

         dim = ROWTYPE_part_dimen(rt[col],ptr,ii) ;      /* dimension of part */
         if( dim > 0 && *apt != NULL ){
           switch( tmode ){
             case NI_TEXT_MODE:
               NI_multival_to_text( rt[col]->part_rtp[ii] , dim ,
                                    *apt , wbuf ) ;
             break ;
             case NI_BASE64_MODE:
             case NI_BINARY_MODE:
               jj += NI_multival_to_binary( rt[col]->part_rtp[ii] , dim ,
                                            *apt , wbuf+jj ) ;
             break ;
           }
         }
       }

      } /* end of loop over parts in this column struct */
     } /* end of loop over columns */

     /*- actually write the row data in wbuf out -*/

     switch( tmode ){

       case NI_TEXT_MODE:     /* each row is on a separate line */
         strcat(wbuf,"\n") ;
         nout = NI_stream_writestring( ns , wbuf ) ;
         ADDOUT ;
       break ;

       case NI_BINARY_MODE:   /* jj bytes of binary in wbuf */
         nout = NI_stream_write( ns , wbuf , jj ) ;
#ifdef NIML_DEBUG
if( nout != jj ) NI_dpr("NI_write_columns: col#%d sends %d bytes; nout=%d\n",col,jj,nout) ;
#endif
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
      up if there are any leftover bytes in bbuf,
      or at least write an end of line           */

   if( tmode == NI_BASE64_MODE ){
     if( bb > 0 ){                  /* num leftover bytes of data */
       byte w,x,y,z , a=bbuf[0],b=bbuf[1] ;
       if( bb == 2 ) B64_encode2(a,b,w,x,y,z) ;
       else          B64_encode1(a,w,x,y,z) ;
       cbuf[0] = w ; cbuf[1] = x ;
       cbuf[2] = y ; cbuf[3] = z ; cbuf[4] = B64_EOL2 ;
       nout = NI_stream_write( ns , cbuf , 5 ) ;
       ADDOUT ;
     } else if( cc > 0 ){           /* just write an end of line */
       cbuf[0] = B64_EOL2 ;
       nout = NI_stream_write( ns , cbuf , 1 ) ;
       ADDOUT ;
     }
   }

   /*-- cleanup and return --*/

   FREEUP ;
   return ntot ;
}

/*------------------------------------------------------------------------*/
/*! Read "columns" of data from a NI_stream.  Each column is an array of
    structs of some NI_rowtype (including the builtin types):
      - ns         = stream to read from
      - col_num    = number of columns to read (1,2,...)
      - col_typ[i] = type code for column #i (i=0..col_num-1)
      - col_len    = number of elements in each column
         - col_len can be 0, which means read data until the end-of-input
         - end-of-input is end-of-stream, or '<' in NI_TEXT_MODE
      - col_dpt[i] = pointer to data in column #i
         - col_dpt can't be NULL
         - but if col_dpt[i] is NULL, it will be NI_malloc()-ed
         - if col_dpt[i] isn't NULL, it must point to space big
           enough to hold the input (col_num * fixed size of rowtype #i)
         - if col_len==0 on input:
            - then col_dpt[i] should be NULL for i=0..col_num-1
            - this function will NI_malloc() to fit the amount of data scanned
            - if col_dpt[i]!=NULL with col_len==0, then function returns -1
      - tmode is one of
         - NI_TEXT_MODE   ==> ASCII input
            - text mode is required if any rowtype component is a String
            - text mode is required from "str:" streams
         - NI_BINARY_MODE ==> binary input
         - NI_BASE64_MODE ==> binary/base64 input
      - flags is the OR of various bit masks
         - NI_SWAP_MASK  ==> binary input must be byte-swapped after input
         - NI_LTEND_MASK ==> text input stops at a '<' in the input
      - return value is:
         - number of complete rows actually read (<= col_len)
         - zero if stream isn't ready to read
         - -1 if something bad happened

   Only the data is read from the stream - no header or footer.
   This function is adapted from the 1st edition of NI_read_element().
--------------------------------------------------------------------------*/

int NI_read_columns( NI_stream_type *ns,
                     int col_num, int   *col_typ,
                     int col_len, void **col_dpt, int tmode, int flags )
{
   int ii,jj , row , dim , nin , col , nn=0 ;
   char *ptr , **col_dat=(char **)col_dpt ;

   NI_rowtype **rt=NULL ;  /* array of NI_rowtype, 1 per column */
   int *vsiz=NULL , vsiz_tot=0 ;
   int *fsiz=NULL , fsiz_tot=0 ;

   int (*ReadFun)( NI_stream_type *, NI_rowtype *, void *, int ) ;
   int ltend = (flags & NI_LTEND_MASK) != 0 ;
   int swap  = (flags & NI_SWAP_MASK)  != 0 ;
   int ReadFlag ;
   int open_ended = (col_len==0) , row_top ;  /* 27 Mar 2003 */

# undef  FREEUP
# define FREEUP do{ NI_free(rt); NI_free(vsiz); NI_free(fsiz); } while(0)

   /*-- check inputs --*/

   if( col_num <= 0    || col_len <  0    ) return  0 ;
   if( col_typ == NULL || col_dat == NULL ) return -1 ;
   if( !NI_stream_readable(ns)            ) return -1 ;

#ifdef NIML_DEBUG
NI_dpr("ENTER NI_read_columns\n") ;
#endif

   /*-- check stream --*/

   if( ns->bad ){                        /* not connected yet? */
     jj = NI_stream_goodcheck(ns,666) ;  /* try to connect it */
     if( jj < 1 ) return jj ;            /* 0 is nothing yet, -1 is death */
   }
   jj = NI_stream_hasinput(ns,666) ;     /* any data to be had? */
   if( jj < 0 ) return jj ;              /* only exit if stream is actually bad */

   /* create array of NI_rowtype for columns, etc. */

   rt   = NI_malloc(NI_rowtype*,  sizeof(NI_rowtype *) * col_num ) ;
   vsiz = NI_malloc(int,  sizeof(int)          * col_num ) ;
   fsiz = NI_malloc(int,  sizeof(int)          * col_num ) ;
   if( open_ended ) col_len = 1 ;
   for( col=0 ; col < col_num ; col++ ){

     rt[col] = NI_rowtype_find_code( col_typ[col] ) ;
     if( rt[col] == NULL ){ FREEUP; return -1; }
     if( tmode != NI_TEXT_MODE && NI_has_String(rt[col]) ){ FREEUP; return -1; }

     vsiz[col] = ROWTYPE_is_varsize(rt[col]) ;     /*  variable dim type? */
     fsiz[col] = rt[col]->size ;      /* fixed size of struct (w/padding) */
     vsiz_tot += vsiz[col] ;
     fsiz_tot += fsiz[col] ;

     /* setup data array for this column */

     if( col_dat[col] == NULL ){
       col_dat[col] = NI_malloc(char,  fsiz[col]*col_len ) ; /* make space */
     } else {
       if( open_ended ){ FREEUP; return -1; }
       memset( col_dat[col], 0 , fsiz[col]*col_len ) ; /* set space to 0 */
     }
   }

   /*-- Special (and fast) case:
        one compact (no padding) fixed-size rowtype,
        and binary input ==> can read all data direct from stream at once --*/

   if( col_num == 1              &&
       fsiz[0] == rt[0]->psiz    &&    /* struct size == data size */
       tmode   == NI_BINARY_MODE &&
       !open_ended                 ){

     nin = NI_stream_readbuf( ns , col_dat[0] , fsiz[0]*col_len ) ;
     if( nin < fsiz[0] ){ FREEUP; return (nin >= 0) ? 0 : -1 ; }  /* bad */
     nin = nin / fsiz[0] ;  /* number of rows finished */
     goto ReadFinality ;    /* post-process input down below */
   }

   /*-- 21 Apr 2005: repeat above for Base64 input --*/

   if( col_num == 1              &&
       fsiz[0] == rt[0]->psiz    &&    /* struct size == data size */
       tmode   == NI_BASE64_MODE &&
       !open_ended                 ){

     nin = NI_stream_readbuf64( ns , col_dat[0] , fsiz[0]*col_len ) ;
     if( nin < fsiz[0] ){ FREEUP; return (nin >= 0) ? 0 : -1 ; }  /* bad */
     nin = nin / fsiz[0] ;  /* number of rows finished */
     goto ReadFinality ;    /* post-process input down below */
   }

   /*-- Choose function to read from stream and fill one struct --*/

   switch( tmode ){
     case NI_TEXT_MODE:   ReadFun = NI_text_to_val  ; ReadFlag = ltend; break;
     case NI_BINARY_MODE: ReadFun = NI_binary_to_val; ReadFlag = swap ; break;
     case NI_BASE64_MODE: ReadFun = NI_base64_to_val; ReadFlag = swap ; break;
     default:
       fprintf(stderr,"\n** NI_read_columns: unknown input tmode=%d\n",tmode);
       FREEUP ; return -1 ;
   }

   /*-- OK, have to read the hard ways --*/

   row_top = (open_ended) ? 1999999999 : col_len ; /* 28 Mar 2003 */

   for( row=0 ; row < row_top ; row++ ){                  /* loop over rows */
                                                          /* until all done */

#ifdef NIML_DEBUG
NI_dpr(" Starting row #%d\n",row) ;
#endif

     /* 27 Mar 2003: maybe need to extend length of columns */

     if( open_ended && row >= col_len ){
#ifdef NIML_DEBUG
NI_dpr("  Extending column lengths!\n") ;
#endif
       jj = (int)(1.2*col_len+32) ;
       for( col=0 ; col < col_num ; col++ ){
         col_dat[col] = NI_realloc( col_dat[col] , char, fsiz[col]*jj ) ;
         memset( col_dat[col]+fsiz[col]*col_len, 0 , fsiz[col]*(jj-col_len) ) ;
       }
       col_len = jj ;
     }

     /* loop over columns, read into struct */

     for( col=0 ; col < col_num ; col++ ){
       ptr = col_dat[col] + fsiz[col]*row ;         /* ptr to row-th struct */
       nn  = ReadFun( ns, rt[col], ptr, ReadFlag ) ; /* read data to struct */
       if( !nn ) break ;
     }
     if( !nn ) break ;                             /* some ReadFun() failed */
   }

   if( row == 0 ){                                /* didn't finish any rows */
     if( open_ended ){
       for( col=0 ; col < col_num ; col++ ) NI_free(col_dat[col]) ;
     }
     FREEUP; return -1;
   }

   nin = row ;                                   /* number of rows finished */

   if( open_ended && nin < col_len ){                   /* truncate columns */
     for( col=0 ; col < col_num ; col++ )
       col_dat[col] = NI_realloc( col_dat[col] , char, fsiz[col]*nin ) ;
   }

   /*-- Have read all data; byte swap if needed, then get outta here --*/

ReadFinality:

   if( tmode != NI_TEXT_MODE && swap ){
     for( col=0 ; col < col_num ; col++ )
       NI_swap_column( rt[col] , nin , col_dat[col] ) ;
   }

#ifdef NIML_DEBUG
NI_dpr("Leaving NI_read_columns\n") ;
#endif

   FREEUP ; return nin ;
}

/*-------------------------------------------------------------------------*/
/*! Decode binary data from the NI_stream ns into a rowtype struct *dpt.
    - Note that String (aka NI_STRING) parts are illegal here.
    - Return value is 1 if all was OK, 0 if something bad happened.
    - Parameter swap indicates that the data coming in needs to be
      byte-swapped.
      - This is ONLY used to byte-swap the dimension for var-dimen arrays.
      - Actual byte-swapping of the data is done in NI_swap_column().
---------------------------------------------------------------------------*/

int NI_binary_to_val( NI_stream_type *ns, NI_rowtype *rt, void *dpt, int swap )
{
   int nn , jj ;

   if( rt->code == NI_STRING ) return 0 ;            /* shouldn't happen */

   if( rt->size == rt->psiz ){        /* fixed-size type with no padding */
                               /* ==> can read directly into data struct */

     jj = NI_stream_readbuf( ns , (char *)dpt , rt->size ) ;
     return (jj == rt->size) ;

   } else {                                              /* derived type */

     char *dat = (char *)dpt , **aaa = NULL ;
     int ii                  ,  naaa = 0 , iaaa = 0 ;

     if( ROWTYPE_is_varsize(rt) ){         /* variable dim arrays inside */
       for( naaa=ii=0 ; ii < rt->part_num ; ii++ )
         if( rt->part_dim[ii] >= 0 ) naaa++ ;    /* count var dim arrays */
       if( naaa > 0 )
         aaa = NI_malloc(char*, sizeof(char *)*naaa) ;  /* save their addresses */
     }                                    /* for possible deletion later */

     /* loop over parts and load them;
        set nn=0 if read fails at any part (and break out of read loop) */

     for( nn=1,ii=0 ; ii < rt->part_num ; ii++ ){

       if( rt->part_dim[ii] < 0 ){            /* read one fixed dim part */

         nn = NI_binary_to_val( ns, rt->part_rtp[ii], dat+rt->part_off[ii], 0 );

       } else {                                    /* read var dim array */

         char **apt = (char **)(dat+rt->part_off[ii]); /* data in struct */
                                                 /* will be ptr to array */
         int dim = ROWTYPE_part_dimen(rt,dat,ii) ;  /* dimension of part */
         int siz = rt->part_rtp[ii]->size ;          /* size of one part */

         if( swap ) NI_swap4( 1 , &dim ) ;   /* byte-swap dim, which was */
                                            /* just read in a moment ago */

         if( dim > 0 ){                         /* need to get some data */
           *apt = NI_malloc(char,  siz * dim );            /* make array */

           if( siz != rt->part_rtp[ii]->psiz ){     /* padded values ==> */
            for( jj=0 ; jj < dim ; jj++ ){       /* read 1 val at a time */
              nn = NI_binary_to_val( ns, rt->part_rtp[ii],
                                     *apt + siz * jj , 0  ) ;
              if( !nn ) break ;                              /* bad read */
            }

           } else {              /* unpadded values ==> read all at once */
             jj = NI_stream_readbuf( ns , *apt , siz*dim ) ;
             nn = ( jj == siz*dim ) ;
           }

         } else {
           *apt = NULL ;                    /* dim=0 ==> no array needed */
         }
         aaa[iaaa++] = *apt ;              /* save for possible deletion */
                                          /* if read fails later in loop */
       }

       if( !nn ) break ;                            /* some read was bad */
     } /* end of loop over parts */

     /* bad news ==> delete any allocated var dim arrays */

     if( !nn ){
       for( ii=0 ; ii < iaaa ; ii++ ) NI_free( aaa[ii] ) ;
     }
     NI_free( aaa ) ;  /* don't need list of var dim arrays no more */
   }

   return nn ;
}

/*-------------------------------------------------------------------------*/
/*! Decode Base64 data from the NI_stream ns into a rowtype struct *dpt.
    - Note that String (aka NI_STRING) parts are illegal here.
    - Return value is 1 if all was OK, 0 if something bad happened.
    - Parameter swap indicates that the data coming in needs to be
      byte-swapped.
      - This is ONLY used to byte-swap the dimension for var-dimen arrays.
      - Actual byte-swapping of the data is done in NI_swap_column().
---------------------------------------------------------------------------*/

int NI_base64_to_val( NI_stream_type *ns, NI_rowtype *rt, void *dpt, int swap )
{
   int nn , jj ;

   if( rt->code == NI_STRING ) return 0 ;            /* shouldn't happen */

   if( rt->size == rt->psiz ){        /* fixed-size type with no padding */
                               /* ==> can read directly into data struct */

     jj = NI_stream_readbuf64( ns , (char *)dpt , rt->size ) ;
     return (jj == rt->size) ;

   } else {                                              /* derived type */

     char *dat = (char *)dpt , **aaa = NULL ;
     int ii                  ,  naaa = 0 , iaaa = 0 ;

     if( ROWTYPE_is_varsize(rt) ){         /* variable dim arrays inside */
       for( naaa=ii=0 ; ii < rt->part_num ; ii++ )
         if( rt->part_dim[ii] >= 0 ) naaa++ ;    /* count var dim arrays */
       if( naaa > 0 )
         aaa = NI_malloc(char*, sizeof(char *)*naaa) ;  /* save their addresses */
     }                                    /* for possible deletion later */

     /* loop over parts and load them;
        set nn=0 if read fails at any part (and break out of read loop) */

     for( nn=1,ii=0 ; ii < rt->part_num ; ii++ ){

       if( rt->part_dim[ii] < 0 ){            /* read one fixed dim part */

         nn = NI_base64_to_val( ns, rt->part_rtp[ii], dat+rt->part_off[ii], 0 );

       } else {                                    /* read var dim array */

         char **apt = (char **)(dat+rt->part_off[ii]); /* data in struct */
                                                 /* will be ptr to array */
         int dim = ROWTYPE_part_dimen(rt,dat,ii) ;  /* dimension of part */
         int siz = rt->part_rtp[ii]->size ;          /* size of one part */

         if( swap ) NI_swap4( 1 , &dim ) ;   /* byte-swap dim, which was */
                                            /* just read in a moment ago */

         if( dim > 0 ){                         /* need to get some data */
           *apt = NI_malloc(char,  siz * dim );            /* make array */

           if( siz != rt->part_rtp[ii]->psiz ){     /* padded values ==> */
            for( jj=0 ; jj < dim ; jj++ ){       /* read 1 val at a time */
              nn = NI_base64_to_val( ns, rt->part_rtp[ii],
                                     *apt + siz * jj , 0  ) ;
              if( !nn ) break ;                              /* bad read */
            }

           } else {              /* unpadded values ==> read all at once */
             jj = NI_stream_readbuf64( ns , *apt , siz*dim ) ;
             nn = ( jj == siz*dim ) ;
           }

         } else {
           *apt = NULL ;                    /* dim=0 ==> no array needed */
         }
         aaa[iaaa++] = *apt ;              /* save for possible deletion */
                                          /* if read fails later in loop */
       }

       if( !nn ) break ;                            /* some read was bad */
     } /* end of loop over parts */

     /* bad news ==> delete any allocated var dim arrays */

     if( !nn ){
       for( ii=0 ; ii < iaaa ; ii++ ) NI_free( aaa[ii] ) ;
     }
     NI_free( aaa ) ;  /* don't need list of var dim arrays no more */
   }

   return nn ;
}

/*-------------------------------------------------------------------------*/
/*! Decode text from the NI_stream into a rowtype struct.
    - Parameter ltend != 0 means stop at '<' character.
    - Return value is 1 if it works, 0 if it fails.
    - If it works, *dpt will be filled with values.
    - Note that dpt must be pre-allocated rt->size bytes long.
---------------------------------------------------------------------------*/

int NI_text_to_val( NI_stream_type *ns, NI_rowtype *rt, void *dpt, int ltend )
{
   int nn ;

   switch( rt->code ){

     /*-- a derived type: fill the parts by recursion --*/

     default:{
       char *dat = (char *)dpt , **aaa = NULL ;
       int ii , jj ,              naaa = 0 , iaaa = 0 ;

       if( ROWTYPE_is_varsize(rt) ){         /* variable dim arrays inside */
         for( naaa=ii=0 ; ii < rt->part_num ; ii++ )
           if( rt->part_dim[ii] >= 0 ) naaa++ ;    /* count var dim arrays */
         if( naaa > 0 )
           aaa = NI_malloc(char*, sizeof(char *)*naaa) ;  /* save their addresses */
       }                                    /* for possible deletion later */

       /* loop over parts and load them */

       for( nn=1,ii=0 ; ii < rt->part_num ; ii++ ){

         if( rt->part_dim[ii] < 0 ){                 /* one fixed dim part */

           nn = NI_text_to_val( ns, rt->part_rtp[ii],
                                dat+rt->part_off[ii], ltend );

         } else {                                         /* var dim array */

           char **apt = (char **)(dat+rt->part_off[ii]); /* data in struct */
                                                   /* will be ptr to array */
           int dim = ROWTYPE_part_dimen(rt,dat,ii) ;  /* dimension of part */
           int siz = rt->part_rtp[ii]->size ;   /* size of one part struct */
           if( dim > 0 ){
             *apt = NI_malloc(char, siz * dim );                  /* make array */
             for( jj=0 ; jj < dim ; jj++ ){        /* get values for array */
               nn = NI_text_to_val( ns, rt->part_rtp[ii],
                                    *apt + siz * jj , ltend ) ;
               if( !nn ) break ;
             }
           } else {
             *apt = NULL ;                    /* dim=0 ==> no array needed */
           }
           aaa[iaaa++] = *apt ;              /* save for possible deletion */

         }

         if( !nn ) break ;                            /* some read was bad */
       } /* end of loop over parts */

       /* bad news ==> delete any allocated var dim arrays */

       if( !nn ){
         for( ii=0 ; ii < iaaa ; ii++ ) NI_free( aaa[ii] ) ;
         NI_free( aaa ) ;
         return 0 ;
       }
       NI_free( aaa ) ;  /* in any case, dump this */
     }
     break ;

     /*-- the 9 builtin types below here; first up: String! --*/

     case NI_STRING:{
        char *val=NULL ;
        char **vpt = (char **) dpt ;
        nn = NI_decode_one_string( ns , &val , ltend ) ;
        if( !nn || val == NULL ) return 0 ;
        unescape_inplace(val) ;
        *vpt = val ;
     }
     break ;

     /*-- numeric types below here --*/

     case NI_BYTE:{
        double val ;
        byte *vpt = (byte *) dpt ;
        nn = NI_decode_one_double( ns , &val , ltend ) ;
        if( !nn ) return 0 ;
        *vpt = (byte) val ;
     }
     break ;

     case NI_SHORT:{
        double val ;
        short *vpt = (short *) dpt ;
        nn = NI_decode_one_double( ns , &val , ltend ) ;
        if( !nn ) return 0 ;
        *vpt = (short) val ;
     }
     break ;

     case NI_INT:{
        double val ;
        int *vpt = (int *) dpt ;
        nn = NI_decode_one_double( ns , &val , ltend ) ;
        if( !nn ) return 0 ;
        *vpt = (int) val ;
     }
     break ;

     case NI_FLOAT:{
        double val ;
        float *vpt = (float *) dpt ;
        nn = NI_decode_one_double( ns , &val , ltend ) ;
        if( !nn ) return 0 ;
        *vpt = (float) val ;
     }
     break ;

     case NI_DOUBLE:{
        double val ;
        double *vpt = (double *) dpt ;
        nn = NI_decode_one_double( ns , &val , ltend ) ;
        if( !nn ) return 0 ;
        *vpt = (double) val ;
     }
     break ;

     case NI_COMPLEX:{
        double v1,v2 ;
        complex *vpt = (complex *) dpt ;
        nn = NI_decode_one_double( ns , &v1 , ltend ) ;
        if( !nn ) return 0 ;
        nn = NI_decode_one_double( ns , &v2 , ltend ) ;
        if( !nn ) return 0 ;
        vpt->r = (float) v1 ;
        vpt->i = (float) v2 ;
     }
     break ;

     case NI_RGB:{
        double v1,v2,v3 ;
        rgb *vpt = (rgb *) dpt ;
        nn = NI_decode_one_double( ns , &v1 , ltend ) ;
        if( !nn ) return 0 ;
        nn = NI_decode_one_double( ns , &v2 , ltend ) ;
        if( !nn ) return 0 ;
        nn = NI_decode_one_double( ns , &v3 , ltend ) ;
        if( !nn ) return 0 ;
        vpt->r = (byte) v1 ;
        vpt->g = (byte) v2 ;
        vpt->b = (byte) v3 ;
     }
     break ;

     case NI_RGBA:{
        double v1,v2,v3,v4 ;
        rgba *vpt = (rgba *) dpt ;
        nn = NI_decode_one_double( ns , &v1 , ltend ) ;
        if( !nn ) return 0 ;
        nn = NI_decode_one_double( ns , &v2 , ltend ) ;
        if( !nn ) return 0 ;
        nn = NI_decode_one_double( ns , &v3 , ltend ) ;
        if( !nn ) return 0 ;
        nn = NI_decode_one_double( ns , &v4 , ltend ) ;
        if( !nn ) return 0 ;
        vpt->r = (byte) v1 ;
        vpt->g = (byte) v2 ;
        vpt->b = (byte) v3 ;
        vpt->a = (byte) v4 ;
     }
     break ;

   } /* end of switch on type */

   return 1 ;  /* good */
}

/*-------------------------------------------------------------------------*/
/*! Swap bytes in a bunch of rowtype structs.
---------------------------------------------------------------------------*/

void NI_swap_column( NI_rowtype *rt , int nrow , char *dat )
{
   if( rt == NULL || nrow <= 0 || dat == NULL ) return ;  /* stupid inputs */

   switch( rt->code ){

     case NI_RGB:
     case NI_RGBA:
     case NI_STRING:
     case NI_BYTE:    return ;   /* nothing to do */

     /*-- basic types --*/

     case NI_SHORT:
       NI_swap2( nrow , dat ) ;
     return ;

     case NI_INT:
     case NI_FLOAT:
       NI_swap4( nrow , dat ) ;
     return ;

     case NI_DOUBLE:
       NI_swap8( nrow , dat ) ;
     return ;

     case NI_COMPLEX:
       NI_swap4( 2*nrow , dat ) ;
     return ;

     /* a derived type (use recursion) */

     default:{
       int ii , row , fsiz = rt->size ;
       char *ptr ;

       for( row=0 ; row < nrow ; row++ ){
         ptr = dat + fsiz*row ;     /* ptr to row-th element */

         /* loop over parts and swap them, 1 at a time */

         for( ii=0 ; ii < rt->part_num ; ii++ ){

           if( rt->part_dim[ii] < 0 ){                     /* fixed dim part */

             NI_swap_column( rt->part_rtp[ii] , 1 , ptr+rt->part_off[ii] ) ;

           } else {                                         /* var dim array */

             char **apt = (char **)(ptr+rt->part_off[ii]); /* data in struct */
                                                          /* is ptr to array */
             int dim = ROWTYPE_part_dimen(rt,dat,ii) ;  /* dimension of part */
             NI_swap_column( rt->part_rtp[ii] , dim , *apt ) ;

           }
         } /* end of loop over parts */
       } /* end of loop over rows */
     }
     return ;
   }
}

/*--------------------------------------------------------------------------*/
/*! Delete a column of rowtype structs, including any var dim arrays.
    Assumes everything was allocated with NI_malloc().
    After this is called, the cpt argument should be set to NULL.
----------------------------------------------------------------------------*/

void NI_free_column( NI_rowtype *rt , int col_len , void *cpt )
{
   char *dat=(char *)cpt , *ptr ;
   int ii , jj ;

   if( rt == NULL || dat == NULL || col_len < 1 ) return ; /* nothing to do */

   /* if has variable dim arrays inside, free them */

   if( ROWTYPE_is_varsize(rt) ){
     for( ii=0 ; ii < col_len ; ii++ ){            /* loop over structs */
       ptr = dat + rt->size * ii ;            /* pointer to this struct */
       for( jj=0 ; jj < rt->part_num ; jj++ ){       /* loop over parts */
         if( rt->part_typ[jj] == NI_STRING ||
             rt->part_dim[jj] >= 0           ){
           char **apt = (char **)(ptr+rt->part_off[jj]) ;
           NI_free(*apt) ; *apt = NULL ;
         }
       }
     }
   }

   /* free the column array itself */

   NI_free(cpt) ; return ;
}

/*----------------------------------------------------------------------------*/
/*! Copy a column of rowtype structs, including var dim arrays.  Return
    is the pointer to the copy.
------------------------------------------------------------------------------*/

void * NI_copy_column( NI_rowtype *rt , int col_len , void *cpt )
{
   char *dat=(char *)cpt , *ndat , *nptr , *qpt ;
   int ii , jj , kk ;

   if( rt == NULL || dat == NULL || col_len < 1 ) return NULL ;

   /* make a quick (surface) copy */

   ndat = NI_malloc(char,  rt->size * col_len ) ;  /* new data column */
   memcpy( ndat , dat , rt->size * col_len ) ;     /* the quick copying */

   /* copy any var dim arrays inside, since the pointers
      in ndat right now still point to data in dat,
      but we want ndat to be entirely self-contained!  */

   if( ROWTYPE_is_varsize(rt) ){
     for( ii=0 ; ii < col_len ; ii++ ){                 /* loop over structs */
       nptr = ndat + rt->size * ii ;                   /* ptr to this struct */
       for( jj=0 ; jj < rt->part_num ; jj++ ){            /* loop over parts */

         if( rt->part_typ[jj] == NI_STRING ){               /* a string part */
           char **apt = (char **)(nptr+rt->part_off[jj]) ;   /* *apt => data */
           qpt = NI_strdup(*apt) ; *apt = qpt ;
         } else if( rt->part_dim[jj] >= 0 ){
           char **apt = (char **)(nptr+rt->part_off[jj]) ;   /* *apt => data */
           if( *apt != NULL ){
             kk  = ROWTYPE_part_dimen(rt,nptr,jj) * rt->part_rtp[jj]->size ;
             qpt = NI_malloc(char, kk) ; memcpy(qpt,*apt,kk) ; *apt = qpt ;
           }
         }
       }
     }
   }

   return ndat ;
}

/*--------------------------------------------------------------------------*/
/*! Return the length in bytes of a column of data (not counting padding).
    The pointer to the data is needed since it might contain variable
    size data (String or vardim arrays).
----------------------------------------------------------------------------*/

int NI_size_column( NI_rowtype *rt , int col_len , void *cpt )
{
   char *dat = (char *)cpt ;
   int ii , ndat ;

   if( rt == NULL || col_len <= 0 )
     return 0;                                    /* nonsense input */
   if( !ROWTYPE_is_varsize(rt) || dat == NULL )
     return (col_len*rt->psiz);                   /* fixed dim struct */

   /* must loop through elements and add up their variable sizes */

   ndat = 0 ;
   for( ii=0 ; ii < col_len ; ii++ )
     ndat += NI_rowtype_vsize( rt , dat + ii*rt->size ) ;

   return ndat ;
}
