#include "mrilib.h"
#include "thd_permute.h"

/* Nonascending, repeated labels in BOTH blocks. The literal identity is not
   the sorted-key representative used internally to enumerate the group. */
static int check_set( int exchange, int blockmode, int equivalent,
                      int exact, int expected )
{
   int blocks[6]={0,0,0,1,1,1},labels[6]={2,0,2,1,2,1};
   PERM_scheme *scheme=THD_perm_scheme_new(6);
   PERM_set *set=NULL;
   int ip,jp,i,fail=1;
   if(!scheme) return 1;
   scheme->exchange=exchange;scheme->blockmode=blockmode;
   scheme->exact=exact;scheme->nperm=exact?1000:2;scheme->seed=42;
   if(THD_perm_scheme_set_blocks(scheme,blocks)!=2) goto done;
   if(equivalent && !THD_perm_scheme_set_eqclass(scheme,labels)) goto done;
   set=THD_perm_set_build(scheme);
   if(!set || set->nperm!=expected || set->is_exact!=exact) goto done;
   for(i=0;i<6;i++) if(set->perm[i]!=i || set->sign[i]!=1) goto done;
   for(ip=0;ip<set->nperm;ip++){
      int seen[6]={0};
      for(i=0;i<6;i++){
         int src=set->perm[ip*6+i],sign=set->sign[ip*6+i];
         if(src<0 || src>=6 || seen[src]++ || (sign!=1 && sign!=-1)) goto done;
         if(blockmode==PERM_WITHIN_BLOCK && blocks[src]!=blocks[i]) goto done;
         if(exchange==PERM_EE && sign!=1) goto done;
         if(exchange==PERM_ISE && src!=i) goto done;
      }
      if(!exact) continue; /* random draws may repeat */
      for(jp=0;jp<ip;jp++){
         for(i=0;i<6;i++){
            int a=set->perm[ip*6+i],b=set->perm[jp*6+i];
            if((equivalent?labels[a]!=labels[b]:a!=b) ||
               set->sign[ip*6+i]!=set->sign[jp*6+i]) break;
         }
         if(i==6) goto done;
      }
   }
   fail=0;
done:
   if(fail) fprintf(stderr,"FAIL exact identity/coverage: exchange=%d blocks=%d eq=%d exact=%d\n",
                    exchange,blockmode,equivalent,exact);
   THD_perm_set_free(set);THD_perm_scheme_free(scheme);
   return fail;
}

/* An identity-only Freedman-Lane test must give p=1, including at locations
   with large fitted nuisance terms. Reduced-fit reconstruction may round y;
   that must never discard the identity exceedance. */
static int check_freedman_lane_identity(void)
{
   matrix X,Z; PERM_glm *glm=NULL; PERM_scheme *scheme=NULL;
   PERM_set *set=NULL; PERM_result *result=NULL;
   float storage[7][257],*data[7]; int i,j,fail=1;
   matrix_initialize(&X); matrix_initialize(&Z);
   matrix_create(7,1,&X); matrix_create(7,2,&Z);
   for(i=0;i<7;i++) {
      X.elts[i][0]=sin(i+0.1); Z.elts[i][0]=1.0; Z.elts[i][1]=i;
      data[i]=storage[i];
      for(j=0;j<257;j++) storage[i][j]=
         (float)(1000.0*(j%5)+0.7*i+sin(i*(j+1.0))+cos(2*i+j));
   }
   glm=THD_perm_glm_setup(X,Z,NULL);
   scheme=THD_perm_scheme_new(7);
   if(!glm || !scheme) goto done;
   scheme->exchange=PERM_EE; scheme->nperm=1; scheme->exact=0; scheme->seed=723;
   set=THD_perm_set_build(scheme);
   if(!set || set->nperm!=1) goto done;
   result=THD_permute_freedman_lane(data,7,257,NULL,glm,set,PERM_TAIL_TWO,NULL,NULL);
   if(!result) goto done;
   for(j=0;j<257;j++) if(result->p_unc[j]!=1.0f || result->p_fwe[j]!=1.0f) goto done;
   fail=0;
done:
   if(fail) fprintf(stderr,"FAIL Freedman-Lane identity exceedance\n");
   THD_perm_result_free(result); THD_perm_set_free(set); THD_perm_scheme_free(scheme);
   THD_perm_glm_free(glm); matrix_destroy(&X); matrix_destroy(&Z);
   return fail;
}

/* Independent analytic OLS example: residuals [1,-2,2,-2,1] are orthogonal
   to both the intercept and x=[-2,-1,0,1,2]. Exact/constant fits remain valid
   here; inference callers, rather than this arithmetic helper, reject them. */
static int check_glm_fit(void)
{
   matrix X,Z; PERM_glm *glm;
   double y[5]={0,-1,5,3,8},b[2],sse,sy; int i,fail=0;
   matrix_initialize(&X); matrix_initialize(&Z);
   matrix_create(5,1,&X); matrix_create(5,1,&Z);
   for(i=0;i<5;i++) { X.elts[i][0]=i-2; Z.elts[i][0]=1.0; }
   glm=THD_perm_glm_setup(X,Z,NULL);
   if(!glm) { fail=1; goto done; }
   if(!THD_perm_glm_fit(glm,y,b,&sse,&sy) || fabs(b[0]-2)>1.e-10 ||
      fabs(b[1]-3)>1.e-10 || fabs(sse-14)>1.e-10 || fabs(sy-54)>1.e-10)
      fail=1;
   if(!THD_perm_glm_fit(glm,y,b,&sse,NULL) || fabs(sse-14)>1.e-10) fail=1;
   for(i=0;i<5;i++) y[i]=3+2*(i-2);
   if(!THD_perm_glm_fit(glm,y,b,&sse,&sy) || sse>1.e-20) fail=1;
   for(i=0;i<5;i++) y[i]=3;
   if(!THD_perm_glm_fit(glm,y,b,&sse,&sy) || sy!=0.0 || sse>1.e-20) fail=1;
   y[2]=NAN;
   if(THD_perm_glm_fit(glm,y,b,&sse,&sy)) fail=1;
   y[2]=INFINITY;
   if(THD_perm_glm_fit(glm,y,b,&sse,&sy)) fail=1;
done:
   if(fail) fprintf(stderr,"FAIL shared GLM fit coefficients/sums/validity\n");
   THD_perm_glm_free(glm); matrix_destroy(&X); matrix_destroy(&Z);
   return fail;
}

int main(void)
{
   int fail=0;
   fail+=check_set(PERM_EE,PERM_WITHIN_BLOCK,1,1,9);
   fail+=check_set(PERM_ISE,PERM_WITHIN_BLOCK,1,1,64);
   fail+=check_set(PERM_BOTH,PERM_WITHIN_BLOCK,1,1,576);
   fail+=check_set(PERM_EE,PERM_WITHIN_BLOCK,0,1,36);
   fail+=check_set(PERM_EE,PERM_WHOLE_BLOCK,1,1,2);
   fail+=check_set(PERM_BOTH,PERM_WHOLE_BLOCK,1,1,8);
   fail+=check_set(PERM_EE,PERM_BOTH_BLOCK,1,1,18);
   fail+=check_set(PERM_EE,PERM_WITHIN_BLOCK,1,0,2);
   fail+=check_freedman_lane_identity();
   fail+=check_glm_fit();
   if(fail) return 1;
   puts("PASS permutation identity, bijections, and exact distinct-label coverage");
   return 0;
}
