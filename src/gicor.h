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
  char *brick_labels;  /* SUMA's dsets need this outside of init
                          function                       Jan 2012 */
  int nvals;           /* Number of values in output dset Jan 2012 */

  char *label_AAA , *label_BBB , *toplabel ;  /* 14 May 2010 */
  int    num_stat_available ;
  char **lab_stat_available ;

  unsigned int apair ;  /* bit 0 set ==> apair is on */
                        /* bit 1 set ==> apair has been set properly */
                        /* bit 2 set ==> apair is to be LR mirrored */
} GICOR_setup ;

#undef  FREEIFNN
#define FREEIFNN(x) do{ if((x)!=NULL) free((void *)(x)); } while(0)

#define GICOR_set_apair_allow_bit(gi)    (gi)->apair |= 1u
#define GICOR_set_apair_ready_bit(gi)    (gi)->apair |= 2u
#define GICOR_set_apair_mirror_bit(gi)   (gi)->apair |= 4u

#define GICOR_unset_apair_allow_bit(gi)  (gi)->apair &= ~1u
#define GICOR_unset_apair_ready_bit(gi)  (gi)->apair &= ~2u
#define GICOR_unset_apair_mirror_bit(gi) (gi)->apair &= ~4u

#define GICOR_flip_apair_allow_bit(gi)   (gi)->apair ^= 1u
#define GICOR_flip_apair_ready_bit(gi)   (gi)->apair ^= 2u
#define GICOR_flip_apair_mirror_bit(gi)  (gi)->apair ^= 4u

#define GICOR_apair_allow_bit(gi)        (((gi)->apair & 1u) != 0)
#define GICOR_apair_ready_bit(gi)        (((gi)->apair & 2u) != 0)
#define GICOR_apair_mirror_bit(gi)       (((gi)->apair & 4u) != 0)

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
