#ifndef MCW_TS_HEADER
#define MCW_TS_HEADER

/** structure type to hold data from one time series file **/

typedef struct {
         char *fname ;    /* filename data was from (NULL=none) */
         int   len ;      /* number of points in time series */
         float *ts ;      /* pointer to actual data */
   } time_series ;

typedef struct {
      int num , nall ;
      time_series ** tsarr ;
} time_series_array ;

#define INC_TSARR 8

#define INIT_TSARR(name)                                                       \
   ( (name) = (time_series_array *) malloc( sizeof(time_series_array) ) ,      \
     (name)->num = 0 , (name)->nall = INC_TSARR ,                              \
     (name)->tsarr = (time_series **)malloc(sizeof(time_series *)*INC_TSARR) )

#define ADDTO_TSARR(name,tsname) \
 do{ int nn ;                                                                               \
     if( (name)->num == (name)->nall ){                                                     \
       (name)->nall += INC_TSARR ; nn = sizeof(time_series *) * (name)->nall ;              \
       (name)->tsarr = ((name)->tsarr == NULL) ? (time_series **)malloc(nn)                 \
                                               : (time_series **)realloc((name)->tsarr,nn); \
       if( name->tsarr == NULL ){                                                           \
          fprintf(stderr,"ADDTO_TSARR malloc fails at line %d in file %s\n",                \
                  __LINE__,__FILE__) ; exit(1) ;                                            \
       } }                                                                                  \
     nn = (name)->num ; (name)->tsarr[nn] = (tsname) ; (name)->num++ ; break ; } while(0)

#define DESTROY_TSARR(name)                                                                \
  do{ int ii ;                                                                             \
      if( (name) != NULL )                                                                 \
         for( ii=0 ; ii < (name)->num ; ii++ ) RWC_free_time_series((name)->tsarr[ii]) ;   \
      free((name)->tsarr) ; free((name)) ; (name) = NULL ; break ; } while(0)

extern time_series * RWC_read_time_series() ;   /* more declarations */
extern float         RWC_norm_ts() ;
extern float         RWC_max_ts() ;
extern float         RWC_min_ts() ;
extern void          RWC_free_time_series() ;
extern time_series * RWC_blank_time_series() ;
extern void          RWC_medfilt_time_series() ;

#define MALLOC_ERR(str) \
     { fprintf(stderr,"MALLOC error: %s\n",str); exit(-1); }

#ifndef MIN
#  define MIN(x,y) (((x)<(y)) ? (x) : (y))
#endif

#ifndef MAX
#  define MAX(x,y) (((x)>(y)) ? (x) : (y))
#endif

#endif
