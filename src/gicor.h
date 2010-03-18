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
} GICOR_setup ;

#define DESTROY_GICOR_setup(gi)                     \
 do{ if( (gi) != NULL ){                            \
       if( (gi)->ivec != NULL ) free((gi)->ivec) ;  \
       free(gi) ; (gi) = NULL ;                     \
     } } while(0)


#endif

