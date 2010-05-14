#ifndef _AFNI_GICOR_
#define _AFNI_GICOR_

typedef struct {
  int       ready ;
  int       ndset_A , ndset_B , nvec ;
  int       ttest_opcode , vmul ;
  float     seedrad ;
  NI_stream ns ;
  int       busy ;   /* 18 Mar 2010 */
  THD_session *session ; THD_3dim_dataset *dset ;
  int nds,nvox,nivec,*ivec ;

  char sdset_ID[2][50]; /* ZSS ID codes for surface dsets */
  int  nnode_domain[2]; /* Number of node in domain for L, and R, surfs */
  int  nnode_mask[2]; /* number of values on each of L and R surfs.
                         This is only needed when masking is done */

  char *label_AAA , *label_BBB , *toplabel ;  /* 14 May 2010 */

  int    num_stat_available ;
  char **lab_stat_available ;
} GICOR_setup ;

#undef  FREEIFNN
#define FREEIFNN(x) do{ if((x)!=NULL) free((void *)(x)); } while(0)

#define DESTROY_GICOR_setup(gi)                               \
 do{ if( (gi) != NULL ){                                      \
       int zz ;                                               \
       FREEIFNN((gi)->ivec) ;                                 \
       FREEIFNN((gi)->label_AAA) ;                            \
       FREEIFNN((gi)->label_BBB) ;                            \
       FREEIFNN((gi)->toplabel) ;                             \
       if( (gi)->lab_stat_available != NULL ){                \
         for( zz=0 ; zz < (gi)->num_stat_available ; zz++ )   \
           free((gi)->lab_stat_available[zz]) ;               \
         free((gi)->lab_stat_available) ;                     \
       }                                                      \
       free(gi) ; (gi) = NULL ;                               \
     } } while(0)

#endif
