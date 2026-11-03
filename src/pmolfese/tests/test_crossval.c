#include "thd_crossval.h"

#define CHECK(COND,MSG) do{ if(!(COND)){ fprintf(stderr,"FAIL: %s\n",MSG); fail=1; goto done; } }while(0)

static THD_sampleset * make_samples( void )
{
   char *tok[]={
     "Subj","Run","Item","Fold","Split","Class","InputFile",
     "s1","r1","a1","f1","train","A","unused",
     "s1","r1","a2","f1","train","B","unused",
     "s1","r2","a3","f2","test", "A","unused",
     "s1","r2","a4","f2","test", "B","unused",
     "s2","q1","b1","f1","train","A","unused",
     "s2","q1","b2","f1","train","B","unused",
     "s2","q2","b3","f2","test", "A","unused",
     "s2","q2","b4","f2","test", "B","unused" } ;
   THD_sampleset *ss=(THD_sampleset *)calloc(1,sizeof(*ss)) ; int ii ;
   ss->metadata=THD_parse_datatable(tok,(int)(sizeof(tok)/sizeof(tok[0])),"fold samples") ;
   ss->nsample=ss->metadata->nrow ; ss->source=strdup("fold samples") ;
   ss->sample_label=(char **)calloc((size_t)ss->nsample,sizeof(char *)) ;
   ss->table_row=(int *)malloc(sizeof(int)*(size_t)ss->nsample) ;
   for( ii=0 ; ii<ss->nsample ; ii++ ){
     char b[24] ; snprintf(b,sizeof(b),"sample%d",ii+1) ; ss->sample_label[ii]=strdup(b) ; ss->table_row[ii]=ii ;
   }
   return ss ;
}

typedef struct { int setup,prepared,evaluated,seen[4],mode; } Search_fixture;
static int search_setup(void *ctx,int batch,char *err,int nerr)
{ Search_fixture *x=ctx;(void)batch;(void)err;(void)nerr;x->setup++;return 1; }
static int search_prepare(void *ctx,int batch,const int *tr,int nt,const int *te,int ne,char *err,int nerr)
{
   Search_fixture *x=ctx;int i;(void)batch;(void)err;(void)nerr;x->prepared++;
   if(nt!=2 || ne!=2)return 0;
   for(i=0;i<nt;i++)if(tr[i]!=0 && tr[i]!=1 && tr[i]!=4 && tr[i]!=5)return 0;
   for(i=0;i<ne;i++)if(te[i]!=0 && te[i]!=1 && te[i]!=4 && te[i]!=5)return 0;
   return 1;
}
static int search_evaluate(void *ctx,int batch,int candidate,const int *tr,int nt,const int *te,int ne,char *err,int nerr)
{
   Search_fixture *x=ctx;(void)tr;(void)nt;(void)te;(void)err;(void)nerr;
   x->evaluated++;x->seen[batch*2+candidate]+=ne;return x->mode!=3;
}
static int search_finish(void *ctx,int batch,int candidate,double *loss,int *valid,char *err,int nerr)
{
   Search_fixture *x=ctx;int id=batch*2+candidate;(void)err;(void)nerr;
   *valid=x->mode==1 || id==3?0:1;*loss=*valid?(id==0?2:1):NAN;
   if(x->mode==2)*valid=1;return 1;
}
static int search_prefer(void *ctx,int a,int b){(void)ctx;return a>b;}
static int candidate_test(THD_sampleset *ss)
{
   THD_foldplan *outer=NULL;THD_nested_foldplan *nested=NULL;int fail=0,i,winner,nvalid;
   int groups[]={0,0,9,9,1,1,8,8},tr[8],te[8];double losses[4];char err[512];
   Search_fixture x={0};
   THD_cv_candidate_ops ops={search_setup,search_prepare,search_evaluate,search_finish,search_prefer};
   outer=THD_foldplan_fixed(ss,"Split","train","test",err,sizeof(err));CHECK(outer,err);
   nested=THD_nested_foldplan_grouped(outer,groups,0,err,sizeof(err));CHECK(nested,err);
   CHECK(THD_nested_candidate_search(nested,0,2,2,&ops,&x,tr,te,losses,&winner,&nvalid,err,sizeof(err)),err);
   CHECK(winner==2 && nvalid==1 && x.setup==2 && x.prepared==4 && x.evaluated==8,"shared setup, pooled evaluation and deterministic candidate ties");
   for(i=0;i<4;i++)CHECK(x.seen[i]==4,"each candidate sees only all four outer training rows in inner tests");
   x.mode=1;
   CHECK(THD_nested_candidate_search(nested,0,2,2,&ops,&x,tr,te,losses,&winner,&nvalid,err,sizeof(err)),err);
   CHECK(winner==3 && nvalid==0,"all-undefined candidates use deterministic fallback");
   x.mode=2;
   CHECK(!THD_nested_candidate_search(nested,0,2,2,&ops,&x,tr,te,losses,&winner,&nvalid,err,sizeof(err)) && winner==-1,"reject undefined loss with valid components");
   x.mode=3;
   CHECK(!THD_nested_candidate_search(nested,0,2,2,&ops,&x,tr,te,losses,&winner,&nvalid,err,sizeof(err)) && strstr(err,"candidate 0"),"propagate callback failure with fold/candidate context");
   x.mode=0;i=nested->inner[0]->train[0][0];nested->inner[0]->train[0][0]=4;
   CHECK(!THD_nested_candidate_search(nested,0,2,2,&ops,&x,tr,te,losses,&winner,&nvalid,err,sizeof(err)),"reject inner index outside outer training set");
   nested->inner[0]->train[0][0]=i;
done:
   THD_nested_foldplan_free(nested);THD_foldplan_free(outer);return fail;
}

static int transfer_test(THD_sampleset *ss)
{
   THD_foldplan *p=NULL;THD_nested_foldplan *in=NULL;int fail=0,count,i;
   int groups[]={0,1,99,99,0,1,98,98};char err[512];
   p=THD_foldplan_fixed_within(ss,"Split","train","test","Subj",err,sizeof(err));CHECK(p,err);
   CHECK(p->nfold==2 && p->nstratum==2,"two independent fixed strata");
   CHECK(!strcmp(p->name[0],"s1/test") && !strcmp(p->name[1],"s2/test"),"transfer fold names");
   for(i=0;i<2;i++)CHECK(p->ntrain[i]==2 && p->ntest[i]==2 && p->train[i][0]==4*i && p->test[i][0]==4*i+2,"subject-local directional rows");
   CHECK(p->sample_test_fold[0]==-1 && p->sample_test_fold[4]==-1 && p->sample_test_fold[6]==1,"training rows never tested");
   CHECK(THD_foldplan_overlap_count(ss,p,"Subj",NULL,&count,err,sizeof(err)) && count==2,"shared subject identities");
   CHECK(THD_foldplan_overlap_count(ss,p,"Item",NULL,&count,err,sizeof(err)) && count==0,"disjoint global item identities");
   CHECK(THD_foldplan_overlap_count(ss,p,"Fold","Subj",&count,err,sizeof(err)) && count==0,"qualified session-style identities");
   in=THD_nested_foldplan_grouped(p,groups,0,err,sizeof(err));CHECK(in,err);
   CHECK(in->inner[0]->nsample==2 && in->inner[1]->nsample==2,"nested folds never include destination or other subjects");
   p->train[1][0]=0;CHECK(!THD_foldplan_validate(p,err,sizeof(err)),"reject cross-stratum repeated training rows");p->train[1][0]=4;
   THD_nested_foldplan_free(in);in=NULL;
   THD_foldplan_free(p);p=THD_foldplan_fixed_within(ss,"Split","train","test","Run",err,sizeof(err));
   CHECK(!p && strstr(err,"both domains"),"reject strata lacking either domain");
done:THD_nested_foldplan_free(in);THD_foldplan_free(p);return fail;
}

static int assignment_test(THD_sampleset *ss)
{
   THD_foldplan *p=NULL,*table=NULL; char err[512]; int fail=0,f,i;
   int assignment[]={1,0,1,2,0,2,2};
   char a[]="first",b[]="second",c[]="third";
   const char *names[]={a,b,c};
   const int nt[]={2,2,3},want_test[]={1,4,0,2,3,5,6};
   const int want_train[]={0,2,3,5,6,1,3,4,5,6,0,1,2,4};
   int tr=0,te=0;
   p=THD_foldplan_from_assignments(7,3,assignment,names,err,sizeof(err));CHECK(p,err);
   CHECK(p->kind==THD_CV_LEAVE_GROUP_OUT && p->nstratum==1,"unstratified assigned folds");
   for(f=0;f<3;f++){
      CHECK(p->ntest[f]==nt[f] && p->ntrain[f]==7-nt[f],"unequal fold sizes");
      CHECK(!strcmp(p->name[f],names[f]),"explicit name order precedes sample occurrence order");
      for(i=0;i<p->ntest[f];i++)CHECK(p->test[f][i]==want_test[te++],"ordered test indices");
      for(i=0;i<p->ntrain[f];i++)CHECK(p->train[f][i]==want_train[tr++],"ordered complement indices");
   }
   assignment[0]=0;a[0]='X';
   CHECK(p->sample_test_fold[0]==1 && !strcmp(p->name[0],"first"),"plan owns assignments and names");
   CHECK(THD_foldplan_validate(p,err,sizeof(err)),err);
   THD_foldplan_free(p);p=NULL;
   { int ids[]={0,0,1,1,0,0,1,1}; const char *labels[]={"f1","f2"};
     table=THD_foldplan_leave_group_out(ss,"Fold",NULL,err,sizeof(err));CHECK(table,err);
     p=THD_foldplan_from_assignments(8,2,ids,labels,err,sizeof(err));CHECK(p,err);
     for(f=0;f<2;f++){
        CHECK(!strcmp(p->name[f],table->name[f]) && p->ntrain[f]==table->ntrain[f] && p->ntest[f]==table->ntest[f],"same table and array fold metadata");
        CHECK(!memcmp(p->train[f],table->train[f],sizeof(int)*(size_t)p->ntrain[f]) &&
              !memcmp(p->test[f],table->test[f],sizeof(int)*(size_t)p->ntest[f]),"same table and array indices");
     }
     CHECK(!memcmp(p->sample_test_fold,table->sample_test_fold,8*sizeof(int)),"same sample mapping");
   }
   THD_foldplan_free(p);p=NULL;
   { int ids[]={0,0,0}; const char *labels[]={"a","b"};
     CHECK(!THD_foldplan_from_assignments(3,2,ids,labels,err,sizeof(err)),"empty test fold rejected");
     ids[1]=-1;CHECK(!THD_foldplan_from_assignments(3,2,ids,labels,err,sizeof(err)),"negative assignment rejected");
     ids[1]=2;CHECK(!THD_foldplan_from_assignments(3,2,ids,labels,err,sizeof(err)),"out of range assignment rejected");
     ids[1]=1;labels[1]="a";CHECK(!THD_foldplan_from_assignments(3,2,ids,labels,err,sizeof(err)),"duplicate names rejected");
     labels[1]="";CHECK(!THD_foldplan_from_assignments(3,2,ids,labels,err,sizeof(err)),"empty name rejected");
     labels[1]=NULL;CHECK(!THD_foldplan_from_assignments(3,2,ids,labels,err,sizeof(err)),"missing name rejected");
     labels[1]="b";
     CHECK(!THD_foldplan_from_assignments(3,1,ids,labels,err,sizeof(err)),"one fold rejected");
     CHECK(!THD_foldplan_from_assignments(0,2,ids,labels,err,sizeof(err)),"empty samples rejected");
     CHECK(!THD_foldplan_from_assignments(3,4,ids,labels,err,sizeof(err)),"excess folds rejected before reading labels");
     CHECK(!THD_foldplan_from_assignments(3,2,NULL,labels,err,sizeof(err)),"missing assignments rejected");
     CHECK(!THD_foldplan_from_assignments(3,2,ids,NULL,NULL,0),"optional error buffer");
   }
done: THD_foldplan_free(p);THD_foldplan_free(table);return fail;
}

static int block_contract_test(void)
{
   THD_sampleset *ss=make_samples();THD_foldplan *p=NULL;char err[512];int fail=0;
   int blocks[8]={0,0,1,1,2,2,3,3},strata[8]={0,0,0,0,1,1,1,1};
   THD_cv_block_conflict conflict;
   CHECK(ss!=NULL,"samples");
   p=THD_foldplan_fixed(ss,"Split","train","test",err,sizeof(err));CHECK(p!=NULL,err);
   CHECK(THD_foldplan_check_permutation_blocks(p,blocks,4,strata,&conflict,err,sizeof(err)) &&
         conflict.block==-1 && conflict.kind==0,"legal partition/stratum blocks");
   blocks[2]=0;
   CHECK(!THD_foldplan_check_permutation_blocks(p,blocks,4,strata,&conflict,err,sizeof(err)) &&
         conflict.block==0 && conflict.kind==THD_CV_BLOCK_PARTITION,
         "reserved train/test rows cannot share a permutation block");
   strata[1]=1;
   CHECK(!THD_foldplan_check_permutation_blocks(p,blocks,4,strata,&conflict,err,sizeof(err)) &&
         conflict.kind==THD_CV_BLOCK_STRATUM,"report first crossing in sample order");
   strata[1]=0;blocks[2]=1;blocks[4]=0;
   CHECK(!THD_foldplan_check_permutation_blocks(p,blocks,4,strata,&conflict,err,sizeof(err)) &&
         conflict.block==0 && conflict.kind==THD_CV_BLOCK_STRATUM,
         "same-side blocks still must respect strata");
   CHECK(THD_foldplan_check_permutation_blocks(p,blocks,4,NULL,NULL,err,sizeof(err)),
         "stratum restriction is optional");
   blocks[0]=4;
   CHECK(!THD_foldplan_check_permutation_blocks(p,blocks,4,NULL,&conflict,err,sizeof(err)) &&
         conflict.block==-1 && conflict.kind==0 && blocks[0]==4,
         "invalid IDs return no misleading conflict and never mutate input");
   CHECK(!THD_foldplan_check_permutation_blocks(NULL,blocks,4,NULL,NULL,err,sizeof(err)),
         "null plans fail safely");
done:THD_foldplan_free(p);THD_sampleset_free(ss);return fail;
}

int main( void )
{
   THD_sampleset *ss=make_samples() ; THD_foldplan *p=NULL,*p2=NULL ; char err[1024] ;
   int fail=0,*class_id=NULL,nclass=0,ii ; char **class_label=NULL ; FILE *fp=NULL ;

   CHECK(!assignment_test(ss),"array fold constructor");
   CHECK(!transfer_test(ss),"directional transfer folds");
   CHECK(!candidate_test(ss),"candidate-search driver");
   p=THD_foldplan_leave_group_out(ss,"Run","Subj",err,sizeof(err)) ; CHECK(p!=NULL,err) ;
   CHECK(p->kind==THD_CV_LEAVE_GROUP_OUT && p->nfold==4 && p->nstratum==2,"leave-run-out within subject dimensions") ;
   CHECK(!strcmp(p->name[0],"s1/r1") && !strcmp(p->name[2],"s2/q1"),"stratified fold names") ;
   for( ii=0 ; ii<4 ; ii++ ) CHECK(p->ntrain[ii]==2 && p->ntest[ii]==2,"each within-subject run fold is 2 train, 2 test") ;
   CHECK(p->sample_test_fold[0]==p->sample_test_fold[1] && p->sample_test_fold[0]!=p->sample_test_fold[2],
         "a run is held out together") ;
   CHECK(THD_sampleset_encode_column(ss,"Class",&class_id,&class_label,&nclass,err,sizeof(err)),err) ;
   CHECK(THD_foldplan_check_classes(p,class_id,nclass,1,err,sizeof(err)),err) ;
   fp=tmpfile() ; CHECK(fp!=NULL,"open fold audit") ; THD_foldplan_print(p,ss,fp) ; rewind(fp) ;
   { char out[4096] ; size_t n=fread(out,1,sizeof(out)-1,fp) ; out[n]='\0' ;
     CHECK(strstr(out,"Fold\tStratum\tSet")!=NULL && strstr(out,"s1/r1\ts1\ttest")!=NULL,"fold audit output") ; }
   fclose(fp) ; fp=NULL ;
   class_id[0]=2 ; CHECK(!THD_foldplan_check_classes(p,class_id,3,0,err,sizeof(err)) && strstr(err,"no class 3")!=NULL,
                              "a class absent from training is rejected") ; class_id[0]=0 ;
   THD_foldplan_free(p) ; p=NULL ;

   p=THD_foldplan_leave_group_out(ss,"Subj",NULL,err,sizeof(err)) ; CHECK(p!=NULL,err) ;
   CHECK(p->nfold==2 && p->ntrain[0]==4 && p->ntest[0]==4,"leave-one-subject-out") ;
   CHECK(!strcmp(p->name[0],"s1") && !strcmp(p->name[1],"s2"),
         "unstratified fold names retain held-out group labels") ;
   p->ntest[0]-- ;
   CHECK(!THD_foldplan_validate(p,err,sizeof(err)) && strstr(err,"occurs in 0 test folds")!=NULL,
         "invalid coverage reports the preserved test-fold count") ;
   p->ntest[0]++ ;
   THD_foldplan_free(p) ; p=NULL ;
   p=THD_foldplan_leave_group_out(ss,"Fold",NULL,err,sizeof(err)) ; CHECK(p!=NULL,err) ;
   CHECK(p->nfold==2 && p->sample_test_fold[0]==p->sample_test_fold[4],"explicit folds across subjects") ;
   THD_foldplan_free(p) ; p=NULL ;
   p=THD_foldplan_leave_group_out(ss,"Item",NULL,err,sizeof(err)) ; CHECK(p!=NULL,err) ;
   CHECK(p->nfold==8 && p->ntrain[3]==7 && p->ntest[3]==1,"leave-one-item-group-out") ;
   THD_foldplan_free(p) ; p=NULL ;

   p=THD_foldplan_grouped_kfold(ss,"Run","Subj",2,err,sizeof(err)) ; CHECK(p!=NULL,err) ;
   CHECK(p->nfold==4 && p->ntrain[0]==2 && p->ntest[0]==2,"grouped K-fold within subject") ;
   CHECK(p->sample_test_fold[4]==p->sample_test_fold[5] && p->sample_test_fold[4]!=p->sample_test_fold[6],
         "grouped K-fold never splits a run") ;
   p2=THD_foldplan_grouped_kfold(ss,"Run","Subj",2,err,sizeof(err)) ; CHECK(p2!=NULL,err) ;
   for( ii=0 ; ii<ss->nsample ; ii++ )
     CHECK(p->sample_test_fold[ii]==p2->sample_test_fold[ii],"grouped K-fold is deterministic") ;
   THD_foldplan_free(p2) ; p2=NULL ;
   THD_foldplan_free(p) ; p=NULL ;
   CHECK(THD_foldplan_grouped_kfold(ss,"Run","Subj",3,err,sizeof(err))==NULL && strstr(err,"at least 3")!=NULL,
         "too many grouped folds are rejected") ;

   p=THD_foldplan_fixed(ss,"Split","train","test",err,sizeof(err)) ; CHECK(p!=NULL,err) ;
   CHECK(p->nfold==1 && p->ntrain[0]==4 && p->ntest[0]==4 && p->sample_test_fold[0]==-1 && p->sample_test_fold[2]==0,
         "fixed train/test partition") ;
   THD_foldplan_free(p) ; p=NULL ;
   CHECK(THD_foldplan_leave_group_out(ss,"NoSuchColumn",NULL,err,sizeof(err))==NULL && strstr(err,"NoSuchColumn")!=NULL,
         "a missing group column is rejected") ;

done:
   if( fp!=NULL ) fclose(fp) ; THD_foldplan_free(p) ; THD_foldplan_free(p2) ; free(class_id) ;
   THD_sampleset_free_levels(class_label,nclass) ; THD_sampleset_free(ss) ;
   if( fail || block_contract_test() ) return 1 ;
   puts("PASS thd_crossval grouped, stratified, explicit, and fixed fold plans") ; return 0 ;
}
