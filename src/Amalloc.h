#ifndef _AFNI_AMALLOC_HEADER_
#define _AFNI_AMALLOC_HEADER_
/************************************************************************
   Casting macros to fixup AFNI stuff for compiling with g++. 
   Original dealt with only malloc() casting stuff, but more
   has been added since.
************************************************************************/

/*---------------------------------------------------------------------*/
/*! Replacement for malloc(). */

#define AFMALL(typ,siz)    (typ*) calloc(1,siz)

/*---------------------------------------------------------------------*/
/*! Replacement for realloc(). */

#define AFREALL(v,typ,siz) (typ*) realloc((void*)v,sizeof(typ)*(siz))

/*---------------------------------------------------------------------*/
/*! Replacement for free(). */

#define AFFREE(v)          free((void*)v)

/*---------------------------------------------------------------------*/
/*! Cast a function pointer to the right
    prototype and call it, for 0D transformations. */

#define AFNI_CALL_0D_function(func,nar,far)                     \
 do{ void (*fp)(int,float *) = (void (*)(int,float *))(func) ;  \
     if( fp != NULL )                                           \
      fp( (nar) , (far) ) ;                                     \
 } while(0)

/*---------------------------------------------------------------------*/
/*! Cast a function pointer to the right
    prototype and call it, for 1D transformations. */

#define AFNI_CALL_1D_function(func,nar,d1,d2,far)               \
 do{ void (*fp)(int,double,double,float *) =                    \
      (void (*)(int,double,double,float *))(func) ;             \
     if( fp != NULL )                                           \
      fp(nar,(double)d1,(double)d2,far) ;                       \
 } while(0)

/*---------------------------------------------------------------------*/
/*! Cast a function pointer to the right
    prototype and call it, for 1D transformations
    that also return a pointer to a string.      */

#define AFNI_CALL_1D_funcstr(func,nar,d1,d2,far,str)            \
 do{ void (*fp)(int,double,double,float *,char **) =            \
      (void (*)(int,double,double,float *,char**))(func) ;      \
     if( fp != NULL )                                           \
      fp(nar,(double)d1,(double)d2,far,(char **)&(str)) ;       \
 } while(0)

/*---------------------------------------------------------------------*/
/*! Cast a function pointer to the right prototype
    and call it, for 1D transformations of images. */

#define AFNI_CALL_1D_funcmrim(func,mage)                        \
 do{ void (*fp)(MRI_IMAGE *) = (void (*)(MRI_IMAGE *))(func) ;  \
     if( fp != NULL )                                           \
      fp(mage) ;                                                \
 } while(0)

/*---------------------------------------------------------------------*/
/*! Cast a function pointer to the right prototype
    and call it, for 1D transformations of images
    that also return a pointer to a string.       */

#define AFNI_CALL_1D_funcmrimstr(func,mage,str)                 \
 do{ void (*fp)(MRI_IMAGE *,char **) =                          \
      (void (*)(MRI_IMAGE *,char **))(func) ;                   \
     if( fp != NULL )                                           \
      fp(mage,(char **)&(str)) ;                                \
 } while(0)

/*---------------------------------------------------------------------*/
/*! Cast a function pointer to the right
    prototype and call it, for 2D transformations. */

#define AFNI_CALL_2D_function(func,n1,n2,d1,d2,far)             \
 do{ void (*fp)(int,int,double,double,float *) =                \
      (void (*)(int,int,double,double,float *))(func) ;         \
     if( fp != NULL )                                           \
      fp( (n1),(n2),(double)(d1),(double)(d2),far ) ;           \
 } while(0)

/*---------------------------------------------------------------------*/
/*! Cast a function pointer to the right
    prototype and call it, for projections.
    The output value is assigned to "val". */

#define AFNI_CALL_proj_function(func,n,far,val)                 \
 do{ float (*fp)(int,float *) = (float (*)(int,float *))(func); \
     if( fp != NULL ) (val) = fp(n,far) ;                       \
 } while(0)

/*---------------------------------------------------------------------*/
/*! Cast a function pointer to the right
    prototype and call it, for FIM functions. */

#define AFNI_CALL_fim_function(func,n,ts,ud,nb,vv)             \
 do{ void (*fp)(int,float *,void *,int,void *) =               \
      (void (*)(int,float *,void *,int,void *))(func) ;        \
     if( fp != NULL )                                          \
       fp(n,ts,(void *)(ud),nb,(void *)(vv)) ;                 \
 } while(0)

/************************************************************************
     Macros to call functions with arguments of general types
     specified in the macro call, with various numbers of arguments.
     Functions returninothing (_VOID_) or return a value (_VALU_).
************************************************************************/

/*---------------------------------------------------------------------*/
#define AFNI_CALL_VOID_1ARG(func,typ1,arg1)                   \
 do{ void (*fp)(typ1) = (void (*)(typ1))(func) ;              \
     if( fp != NULL )                                         \
      fp((typ1)(arg1)) ;                                      \
 } while(0)

/*---------------------------------------------------------------------*/
#define AFNI_CALL_VALU_1ARG(func,vtyp,vval,typ1,arg1)         \
 do{ vtyp (*fp)(typ1) = (vtyp (*)(typ1))(func) ;              \
     if( fp != NULL )                                         \
      (vval) = fp((typ1)(arg1)) ;                             \
 } while(0)

/*---------------------------------------------------------------------*/
#define AFNI_CALL_VOID_2ARG(func,typ1,arg1,typ2,arg2)        \
 do{ void (*fp)(typ1,typ2) = (void (*)(typ1,typ2))(func) ;   \
     if( fp != NULL )                                        \
      fp((typ1)(arg1),(typ2)(arg2)) ;                        \
 } while(0)

/*---------------------------------------------------------------------*/
#define AFNI_CALL_VALU_2ARG(func,vtyp,vval,typ1,arg1,typ2,arg2) \
 do{ vtyp (*fp)(typ1,typ2) = (vtyp (*)(typ1,typ2))(func) ;      \
     if( fp != NULL )                                           \
      (vval) = fp((typ1)(arg1),(typ2)(arg2)) ;                  \
 } while(0)

/*---------------------------------------------------------------------*/
#define AFNI_CALL_VOID_3ARG(func,typ1,arg1,typ2,arg2,typ3,arg3)      \
 do{ void (*fp)(typ1,typ2,typ3) = (void (*)(typ1,typ2,typ3))(func) ; \
     if( fp != NULL )                                                \
      fp((typ1)(arg1),(typ2)(arg2),(typ3)(arg3)) ;                   \
 } while(0)

/*-------------------------------------------------------------------------*/
#define AFNI_CALL_VALU_3ARG(func,vtyp,vval,typ1,arg1,typ2,arg2,typ3,arg3) \
 do{ vtyp (*fp)(typ1,typ2,typ3) = (vtyp (*)(typ1,typ2,typ3))(func) ;      \
     if( fp != NULL )                                                     \
      (vval) = fp((typ1)(arg1),(typ2)(arg2),(typ3)(arg3)) ;               \
 } while(0)

/*----------------------------------------------------------------------------*/
#define AFNI_CALL_VOID_4ARG(func,typ1,arg1,typ2,arg2,typ3,arg3,typ4,arg4)     \
 do{ void (*fp)(typ1,typ2,typ3,typ4) = (void (*)(typ1,typ2,typ3,typ4))(func); \
     if( fp != NULL )                                                         \
      fp((typ1)(arg1),(typ2)(arg2),(typ3)(arg3),(typ4)(arg4)) ;               \
 } while(0)

/*----------------------------------------------------------------------------------*/
#define AFNI_CALL_VALU_4ARG(func,vtyp,vval,typ1,arg1,typ2,arg2,typ3,arg3,typ4,arg4) \
 do{ vtyp (*fp)(typ1,typ2,typ3,typ4) = (vtyp (*)(typ1,typ2,typ3,typ4))(func) ;      \
     if( fp != NULL )                                                               \
      (vval) = fp((typ1)(arg1),(typ2)(arg2),(typ3)(arg3),(typ4)(arg4)) ;            \
 } while(0)

/******************************************************************************/
#endif /* _AFNI_AMALLOC_HEADER_ */
