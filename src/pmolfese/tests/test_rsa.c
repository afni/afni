#include "thd_rsa.h"

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

static int closef( float aa, float bb )
{
   return fabsf(aa-bb)<1.0e-5f ;
}

static int condition_rdm_test( void )
{
   float raw[6]={1,10, 3,14, 5,16} ;
   float centered[6],want[6]={-2,-10.0f/3.0f, 0,2.0f/3.0f, 2,8.0f/3.0f} ;
   float euclid[6] ; THD_simmat *sm=NULL ; char err[256] ; int ii,fail=0 ;

   memcpy(centered,raw,sizeof(raw)) ;
   if( !THD_rsa_center_condition_patterns(3,2,centered,err,sizeof(err)) ){
     fprintf(stderr,"FAIL condition centering: %s\n",err) ; return 1 ;
   }
   for( ii=0 ; ii<6 ; ii++ ) if( !closef(centered[ii],want[ii]) ){
     fprintf(stderr,"FAIL centered pattern %d: got %.9g, wanted %.9g\n",
             ii,centered[ii],want[ii]) ; fail=1 ;
   }

   memcpy(euclid,raw,sizeof(raw)) ;
   sm=THD_rsa_condition_rdm(3,2,euclid,SIM_EUCLID,1,err,sizeof(err)) ;
   if( sm==NULL ){
     fprintf(stderr,"FAIL Euclidean condition RDM: %s\n",err) ; fail=1 ;
   } else {
     if( memcmp(euclid,raw,sizeof(raw))!=0 ){
       fprintf(stderr,"FAIL Euclidean condition centering was not an exact no-op\n") ; fail=1 ;
     }
     if( !sm->is_dist || !closef(sm->mat[1],sqrtf(20.0f)) ||
         !closef(sm->mat[2],sqrtf(52.0f)) ){
       fprintf(stderr,"FAIL Euclidean condition RDM values\n") ; fail=1 ;
     }
     THD_simmat_free(sm) ;
   }
   return fail ;
}

static THD_simmat * triangle_matrix( const float tri[3], int is_dist )
{
   THD_simmat *sm=THD_simmat_new(3) ;
   if( sm==NULL ) return NULL ;
   THD_tri_to_simmat(3,(float *)tri,sm) ; sm->is_dist=is_dist ; return sm ;
}

static int second_order_test( void )
{
   const float s0[3]={1.0f,0.5f,0.0f} ;
   const float s1[3]={1.0f,0.0f,-1.0f} ;
   const float s2[3]={0.5f,0.5f,0.5f} ;
   const float *src[3]={s0,s1,s2} ;
   float all[9],tri[3],meanwant[3] ;
   THD_simmat *inner[3]={NULL,NULL,NULL},*outer=NULL,*outer_sim=NULL,*mean=NULL ;
   THD_rsa_rdm_workspace *ws=NULL ; char err[256]={0} ; int ss,aa,fail=0 ;

   ws=THD_rsa_rdm_workspace_new(4,5) ;
   if( ws==NULL || !THD_rsa_rdm_workspace_reset(ws,3,3,err,sizeof(err)) ){
     fprintf(stderr,"FAIL RSA RDM workspace: %s\n",err) ; fail=1 ; goto done ;
   }
   for( ss=0 ; ss<3 ; ss++ ){
     inner[ss]=triangle_matrix(src[ss],0) ;
     if( inner[ss]==NULL ||
         !THD_rsa_simmat_to_triangle(inner[ss],tri,1,err,sizeof(err)) ||
         !THD_rsa_rdm_workspace_store(ws,ss,inner[ss],1,err,sizeof(err)) ){
       fprintf(stderr,"FAIL storing subject RDM %d: %s\n",ss+1,err) ; fail=1 ; goto done ;
     }
     for( aa=0 ; aa<3 ; aa++ ){
       float want=1.0f-src[ss][aa] ;
       if( !closef(tri[aa],want) ){
         fprintf(stderr,"FAIL canonical distance subject %d entry %d\n",ss+1,aa+1) ;
         fail=1 ;
       }
       all[ss*3+aa]=src[ss][aa] ;
     }
   }
   outer=THD_rsa_second_order_rdm(ws,SIM_EUCLID,err,sizeof(err)) ;
   if( outer==NULL ){
     fprintf(stderr,"FAIL second-order RDM: %s\n",err) ; fail=1 ;
   } else if( !closef(outer->mat[1],sqrtf(1.25f)) ||
              !closef(outer->mat[2],sqrtf(0.5f)) ||
              !closef(outer->mat[5],sqrtf(2.75f)) ){
     fprintf(stderr,"FAIL second-order Euclidean geometry: %.9g %.9g %.9g\n",
             outer->mat[1],outer->mat[2],outer->mat[5]) ; fail=1 ;
   }

   if( !THD_rsa_rdm_workspace_reset(ws,3,3,err,sizeof(err)) ){
     fprintf(stderr,"FAIL resetting RSA RDM workspace: %s\n",err) ; fail=1 ;
   } else {
     for( ss=0 ; ss<3 ; ss++ )
       if( !THD_rsa_rdm_workspace_store(ws,ss,inner[ss],0,err,sizeof(err)) ){
         fprintf(stderr,"FAIL storing similarity RDM %d: %s\n",ss+1,err) ; fail=1 ;
       }
     outer_sim=THD_rsa_second_order_rdm(ws,SIM_EUCLID,err,sizeof(err)) ;
     if( outer_sim==NULL || !closef(outer_sim->mat[1],sqrtf(1.25f)) ||
         !closef(outer_sim->mat[2],sqrtf(0.5f)) ||
         !closef(outer_sim->mat[5],sqrtf(2.75f)) ){
       fprintf(stderr,"FAIL reusable similarity-to-distance scratch conversion\n") ; fail=1 ;
     }
   }

   for( aa=0 ; aa<3 ; aa++ )
     meanwant[aa]=(s0[aa]+s1[aa]+s2[aa])/3.0f ;
   mean=THD_rsa_mean_subject_rdms(3,3,all,0,err,sizeof(err)) ;
   if( mean==NULL ){
     fprintf(stderr,"FAIL mean subject RDM: %s\n",err) ; fail=1 ;
   } else {
     THD_simmat_to_tri(mean,tri) ;
     if( mean->is_dist ){
       fprintf(stderr,"FAIL mean subject RDM changed similarity sense\n") ; fail=1 ;
     }
     for( aa=0 ; aa<3 ; aa++ ) if( !closef(tri[aa],meanwant[aa]) ){
       fprintf(stderr,"FAIL mean subject RDM entry %d\n",aa+1) ; fail=1 ;
     }
   }

done:
   for( ss=0 ; ss<3 ; ss++ ) THD_simmat_free(inner[ss]) ;
   THD_simmat_free(outer) ; THD_simmat_free(outer_sim) ; THD_simmat_free(mean) ;
   THD_rsa_rdm_workspace_free(ws) ; return fail ;
}

static int masked_rdm_test( void )
{
   char in[]="/tmp/afni_masked_rdm_in_XXXXXX",out[]="/tmp/afni_masked_rdm_out_XXXXXX" ;
   int fd1=mkstemp(in),fd2=mkstemp(out),fail=0 ; FILE *fp ;
   THD_simmat *sm=NULL,*back=NULL ; THD_rdm_ws *ws=NULL ;
   float neural[6]={0.0f,1.0f,9.0f,1.0f,9.0f,9.0f},r ; int perm[4]={1,2,3,0} ;
   if( fd1<0 || fd2<0 ){ fprintf(stderr,"FAIL masked RDM temp files\n") ; return 1 ; }
   close(fd1) ; close(fd2) ;
   fp=fopen(in,"w") ;
   fprintf(fp,"0 0 1 .\n0 0 1 .\n1 1 0 .\n. . . 0\n") ; fclose(fp) ;
   sm=THD_simmat_read_1D(in,4) ;
   if( sm==NULL || !THD_simmat_has_missing(sm) || THD_simmat_nvalid(sm)!=3 ){
     fprintf(stderr,"FAIL reading dot-masked RDM\n") ; fail=1 ; goto done ;
   }
   ws=THD_rdm_ws_new(4,1) ; r=THD_simmat_corr_triangle(neural,sm,NULL,CMP_PEARSON,ws) ;
   if( !closef(r,1.0f) ){
     fprintf(stderr,"FAIL masked RDM finite-pair correlation: %.9g\n",r) ; fail=1 ;
   }
   r=THD_simmat_corr_triangle(neural,sm,perm,CMP_PEARSON,ws) ;
   if( !closef(r,-0.5f) ){
     fprintf(stderr,"FAIL masked RDM row/column relabeling: %.9g\n",r) ; fail=1 ;
   }
   if( !THD_simmat_write_1D(out,sm) ){
     fprintf(stderr,"FAIL writing dot-masked RDM\n") ; fail=1 ; goto done ;
   }
   back=THD_simmat_read_1D(out,4) ;
   if( back==NULL || THD_simmat_nvalid(back)!=3 || !isnan(back->mat[3]) ||
       !isnan(back->mat[12]) ){
     fprintf(stderr,"FAIL dot-masked RDM round trip\n") ; fail=1 ;
   }
done:
   THD_simmat_free(sm) ; THD_simmat_free(back) ; THD_rdm_ws_free(ws) ;
   unlink(in) ; unlink(out) ; return fail ;
}

static int noise_failure_test( void )
{
   float valid[4]={1,2,-1,-2}, zero[4]={0,0,0,0};
   float nanres[4]={1,NAN,-1,-2}, infres[4]={1,INFINITY,-1,-2};
   float tiny[4]={1.e-43f,2.e-43f,-1.e-43f,-2.e-43f};
   float w[2],W[4],alpha ; int rank,ii,fail=0 ;
   float *bad[4]={zero,nanres,infres,tiny};
   int want[4]={THD_NOISE_ZERO_VARIANCE,THD_NOISE_NONFINITE,
                THD_NOISE_NONFINITE,THD_NOISE_NONFINITE};

   /* A positive, rank-one covariance is valid. Zero covariance is not.
      Prime the same output buffers before each failure to exercise reuse. */
   for( ii=0 ; ii<4 ; ii++ ){
     if( THD_noise_wdiag(2,2,valid,w)!=THD_NOISE_OK ||
         THD_noise_whalf(2,2,valid,W,&alpha,&rank)!=THD_NOISE_OK ||
         !isfinite(W[0]) || !closef(w[0],0.5f) || !closef(w[1],0.5f) ){
       fprintf(stderr,"FAIL positive rank-deficient residual covariance\n"); fail=1;
     }
     if( THD_noise_wdiag(2,2,bad[ii],w)!=want[ii] ||
         THD_noise_whalf(2,2,bad[ii],W,&alpha,&rank)!=want[ii] ){
       fprintf(stderr,"FAIL noise normalization failure status, case %d\n",ii); fail=1;
     }
   }
   if( THD_noise_wdiag(2,0,valid,w)!=THD_NOISE_BAD_INPUT ||
       THD_noise_whalf(2,0,valid,W,NULL,NULL)!=THD_NOISE_BAD_INPUT ){
     fprintf(stderr,"FAIL empty residual matrix status\n"); fail=1;
   }
   return fail ;
}

/* With one nonzero spatial feature, every nonconstant pattern is collinear.
   Pearson entries must be exact +/-1 ties before the rank comparison. Vary
   feature count and location to exercise different accumulation orders. */
static int pearson_tie_precision_test( void )
{
   const float amp[15]={-1.01f,-.78f,-.57f,-.37f,-.21f,-.11f,-.03f,0,
                        .03f,.11f,.21f,.37f,.57f,.78f,1.01f} ;
   const int sizes[3]={37,63,72} ;
   float patterns[15*72],tri[105],model[105],sc1[105],sc2[105] ;
   float expected[105],r,want ; double dot=0.0 ;
   int nn,loc,nf,pos,i,j,k,neg=0,zero=0,positive=0,fail=0 ;
   char err[256] ; THD_simmat *sm ;

   for( k=0,i=0 ; i<15 ; i++ ) for( j=i+1 ; j<15 ; j++,k++ ){
     expected[k]=(float)(((amp[i]>0)-(amp[i]<0))*((amp[j]>0)-(amp[j]<0))) ;
     model[k]=(i/5==j/5) ? 1.0f : 0.0f ;
     if( expected[k]<0 ) neg++ ;
     else if( expected[k]>0 ) positive++ ; else zero++ ;
   }
   /* Analytic average ranks: 75 between-run and 30 within-run model pairs.
      Neural ranks follow the counts of negative, zero, and positive pairs. */
   for( k=0 ; k<105 ; k++ ){
     double nr=(expected[k]<0) ? (neg+1)/2.0
              : (expected[k]==0) ? neg+(zero+1)/2.0
              : neg+zero+(positive+1)/2.0 ;
     double mr=model[k] ? 90.5 : 38.0 ;
     dot+=(nr-53.0)*(mr-53.0) ;
   }
   want=(float)(12.0*dot/(105.0*(105.0*105.0-1.0))) ;
   for( nn=0 ; nn<3 ; nn++ ) for( loc=0 ; loc<3 ; loc++ ){
     nf=sizes[nn] ; pos=(loc==0) ? 0 : (loc==1) ? nf/2 : nf-1 ;
     memset(patterns,0,sizeof(patterns)) ;
     for( i=0 ; i<15 ; i++ ) patterns[i*nf+pos]=amp[i] ;
     sm=THD_rsa_condition_rdm(15,nf,patterns,SIM_PEARSON,1,err,sizeof(err)) ;
     if( sm==NULL ){
       fprintf(stderr,"FAIL sparse Pearson RDM: %s\n",err) ; return 1 ;
     }
     THD_simmat_to_tri(sm,tri) ;
     for( k=0 ; k<105 ; k++ ) if( tri[k]!=expected[k] ){
       fprintf(stderr,"FAIL exact Pearson tie nfeat=%d pos=%d pair=%d: %.9g != %.9g\n",
               nf,pos,k,tri[k],expected[k]) ; fail=1 ; break ;
     }
     r=THD_tri_corr(105,tri,model,CMP_RHOA,sc1,sc2) ;
     if( r!=want ){
       fprintf(stderr,"FAIL sparse Pearson rho-a nfeat=%d pos=%d: %.9g != %.9g\n",
               nf,pos,r,want) ; fail=1 ;
     }
     THD_simmat_free(sm) ;
   }
   return fail ;
}

/* Alternating series remain exactly collinear after every circular shift.
   Include nonzero means and a constant series; an unused lag stays untouched. */
static int pearson_lag_precision_test( void )
{
   const float amp[7]={-1.01f,-.78f,-.57f,.37f,.21f,.11f,0.0f} ;
   float F[7*64],prep[7*64],norm[7],table[21*64] ;
   unsigned char need[21*64] ;
   THD_simmat *sm ; int variant,i,j,t,k,fail=0 ;
   for( variant=0 ; variant<2 ; variant++ ){
     for( i=0 ; i<7 ; i++ ) for( t=0 ; t<64 ; t++ )
       F[i*64+t]=(variant ? (float)(i+1)*10.0f : 0.0f)
                 + ((t%2) ? -amp[i] : amp[i]) ;
     for( k=0 ; k<21*64 ; k++ ){ table[k]=99.0f ; need[k]=(k%64!=17) ; }
     sm=THD_simmat_from_features(7,64,F,SIM_PEARSON) ;
     if( sm==NULL || THD_simmat_lag_table(7,64,F,SIM_PEARSON,need,table,prep,norm) ){
       THD_simmat_free(sm) ; fprintf(stderr,"FAIL Pearson lag construction\n") ; return 1 ;
     }
     for( k=0,i=0 ; i<7 ; i++ ) for( j=i+1 ; j<7 ; j++,k++ ){
       float sign=(float)(((amp[i]>0)-(amp[i]<0))*((amp[j]>0)-(amp[j]<0))) ;
       for( t=0 ; t<64 ; t++ ){
         float want=(t==17) ? 99.0f : ((t%2) ? -sign : sign) ;
         if( table[k*64+t]!=want ){
           fprintf(stderr,"FAIL Pearson lag tie offset=%d pair=%d lag=%d: %.9g != %.9g\n",
                   variant,k,t,table[k*64+t],want) ; fail=1 ; break ;
         }
       }
       if( table[k*64]!=sm->mat[i*7+j] ){
         fprintf(stderr,"FAIL Pearson lag zero differs from observed matrix\n") ; fail=1 ;
       }
     }
     THD_simmat_free(sm) ;
   }
   return fail ;
}

static int participant_outcome_test( void )
{
   float tri[9]={-1,0,1, -1,1,0, 1,0,-1},sc1[3],sc2[3];
   float bad[3]={1,1,1},p[6]={1,1, 1,2, 2,3};
   float centered_bad[6]={1,2, 1,2, 1,2};
   float zero[6]={0,0, 1,2, 2,3};
   THD_simmat *sm; int fail=0;
   float endpoint=atanhf(0.999999f);
   /* Pair correlations are +.5, -1, -.5: only the clipped -1 remains
      after summing the Fisher transforms. */
   if(!closef(THD_rsa_fisher_corr(3,tri,tri,CMP_PEARSON,sc1,sc2),endpoint) ||
      !closef(THD_rsa_fisher_corr(3,tri,tri+6,CMP_PEARSON,sc1,sc2),-endpoint) ||
      !closef(THD_rsa_repeat_stability(3,3,tri,CMP_PEARSON,sc1,sc2),-endpoint/3)) fail=1;
   if(!closef(THD_rsa_repeat_stability(3,3,tri,CMP_SPEARMAN,sc1,sc2),-endpoint/3)) fail=1;
   if(!isnan(THD_rsa_fisher_corr(3,tri,bad,CMP_PEARSON,sc1,sc2)) ||
      !isnan(THD_rsa_repeat_stability(1,3,tri,CMP_PEARSON,sc1,sc2))) fail=1;
   tri[8]=NAN;
   if(!isnan(THD_rsa_repeat_stability(3,3,tri,CMP_PEARSON,sc1,sc2))) fail=1;
   /* Strictness is opt-in: ordinary construction still permits a constant
      Pearson pattern, whereas participant outcomes must be undefined. */
   sm=THD_rsa_condition_rdm(3,2,p,SIM_PEARSON,0,NULL,0);
   if(!sm) fail=1;
   THD_simmat_free(sm);
   sm=THD_rsa_condition_rdm_checked(3,2,p,SIM_PEARSON,0);
   if(sm) fail=1;
   THD_simmat_free(sm);
   sm=THD_rsa_condition_rdm_checked(3,2,p,SIM_EUCLID,1);
   if(!sm) fail=1;
   THD_simmat_free(sm);
   sm=THD_rsa_condition_rdm_checked(3,2,zero,SIM_COSINE,0);
   if(sm) fail=1;
   THD_simmat_free(sm);
   sm=THD_rsa_condition_rdm_checked(3,2,centered_bad,SIM_PEARSON,1);
   if(sm) fail=1;
   THD_simmat_free(sm);
   p[0]=INFINITY;
   sm=THD_rsa_condition_rdm_checked(3,2,p,SIM_EUCLID,0);
   if(sm) fail=1;
   THD_simmat_free(sm);
   if(fail) fprintf(stderr,"FAIL participant outcome geometry/validity\n");
   return fail;
}

int main( void )
{
   int fail=condition_rdm_test()+second_order_test()+masked_rdm_test()+noise_failure_test()
           +pearson_tie_precision_test()+pearson_lag_precision_test()
           +participant_outcome_test() ;
   if( fail ) return 1 ;
   puts("PASS thd_rsa condition, subject, mean, and second-order RDM construction") ;
   return 0 ;
}
