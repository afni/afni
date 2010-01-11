#ifndef _AFNI_GICOR_
#define _AFNI_GICOR_

typedef struct {
  int       ready ;
  int       ndset_A , ndset_B , nvec ;
  int       ttest_opcode , vmul ;
  float     seedrad ;
  NI_stream ns ;
  THD_session *session ; THD_3dim_dataset *dset ;
  int nds,nvox,nivec,*ivec ;
} GICOR_setup ;

#define DESTROY_GICOR_setup(gi)                     \
 do{ if( (gi) != NULL ){                            \
       if( (gi)->ivec != NULL ) free((gi)->ivec) ;  \
       free(gi) ; (gi) = NULL ;                     \
     } } while(0)


#endif

