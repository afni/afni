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

int main( void )
{
   THD_sampleset *ss=make_samples() ; THD_foldplan *p=NULL,*p2=NULL ; char err[1024] ;
   int fail=0,*class_id=NULL,nclass=0,ii ; char **class_label=NULL ; FILE *fp=NULL ;

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
   if( fail ) return 1 ;
   puts("PASS thd_crossval grouped, stratified, explicit, and fixed fold plans") ; return 0 ;
}
