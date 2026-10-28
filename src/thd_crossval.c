#include "thd_crossval.h"

#include <stdarg.h>

static int cv_error( char *err, int nerr, const char *fmt, ... )
{
   va_list ap ;
   if( err!=NULL && nerr>0 ){
     va_start(ap,fmt) ; vsnprintf(err,(size_t)nerr,fmt,ap) ; va_end(ap) ;
   }
   return 0 ;
}

void THD_foldplan_free( THD_foldplan *p )
{
   int ff ;
   if( p==NULL ) return ;
   if( p->name!=NULL ) for( ff=0 ; ff<p->nfold ; ff++ ) free(p->name[ff]) ;
   if( p->stratum_label!=NULL ) for( ff=0 ; ff<p->nstratum ; ff++ ) free(p->stratum_label[ff]) ;
   if( p->train!=NULL ) for( ff=0 ; ff<p->nfold ; ff++ ) free(p->train[ff]) ;
   if( p->test!=NULL ) for( ff=0 ; ff<p->nfold ; ff++ ) free(p->test[ff]) ;
   free(p->name) ; free(p->fold_stratum) ; free(p->stratum_label) ;
   free(p->ntrain) ; free(p->ntest) ; free(p->train) ; free(p->test) ;
   free(p->sample_test_fold) ; free(p->group_column) ; free(p->within_column) ; free(p) ;
}

static THD_foldplan * cv_fail( THD_foldplan *p, char *err, int nerr,
                               const char *fmt, ... )
{
   va_list ap ;
   if( err!=NULL && nerr>0 ){
     va_start(ap,fmt) ; vsnprintf(err,(size_t)nerr,fmt,ap) ; va_end(ap) ;
   }
   THD_foldplan_free(p) ; return NULL ;
}

static THD_foldplan * cv_alloc( int kind, int nsample, int nfold, int nstratum,
                                const char *group, const char *within,
                                char **stratum_label, char *err, int nerr )
{
   THD_foldplan *p=(THD_foldplan *)calloc(1,sizeof(*p)) ; int ii ;
   if( p==NULL ){ cv_error(err,nerr,"out of memory building folds") ; return NULL ; }
   p->kind=kind ; p->nsample=nsample ; p->nfold=nfold ; p->nstratum=nstratum ;
   p->group_column=strdup(group?group:"") ; p->within_column=within?strdup(within):NULL ;
   p->name=(char **)calloc((size_t)nfold,sizeof(char *)) ;
   p->fold_stratum=(int *)calloc((size_t)nfold,sizeof(int)) ;
   p->stratum_label=(char **)calloc((size_t)nstratum,sizeof(char *)) ;
   p->ntrain=(int *)calloc((size_t)nfold,sizeof(int)) ; p->ntest=(int *)calloc((size_t)nfold,sizeof(int)) ;
   p->train=(int **)calloc((size_t)nfold,sizeof(int *)) ; p->test=(int **)calloc((size_t)nfold,sizeof(int *)) ;
   p->sample_test_fold=(int *)malloc(sizeof(int)*(size_t)nsample) ;
   if( p->group_column==NULL || (within!=NULL && p->within_column==NULL) ||
       p->name==NULL || p->fold_stratum==NULL || p->stratum_label==NULL || p->ntrain==NULL ||
       p->ntest==NULL || p->train==NULL || p->test==NULL || p->sample_test_fold==NULL )
     return cv_fail(p,err,nerr,"out of memory building %d folds",nfold) ;
   for( ii=0 ; ii<nstratum ; ii++ ){
     p->stratum_label[ii]=strdup(stratum_label[ii]) ;
     if( p->stratum_label[ii]==NULL )
       return cv_fail(p,err,nerr,"out of memory copying CV stratum labels") ;
   }
   for( ii=0 ; ii<nsample ; ii++ ) p->sample_test_fold[ii]=-1 ;
   return p ;
}

static int cv_encode_strata( const THD_sampleset *ss, const char *within,
                             int **code, char ***label, int *nlevel,
                             char *err, int nerr )
{
   int ii ;
   if( within!=NULL ) return THD_sampleset_encode_column(ss,within,code,label,nlevel,err,nerr) ;
   *code=(int *)calloc((size_t)ss->nsample,sizeof(int)) ;
   *label=(char **)malloc(sizeof(char *)) ; *nlevel=1 ;
   if( *code==NULL || *label==NULL ){
     free(*code) ; free(*label) ; *code=NULL ; *label=NULL ; *nlevel=0 ;
     return cv_error(err,nerr,"out of memory building the CV stratum") ;
   }
   (*label)[0]=strdup("All") ;
   if( (*label)[0]==NULL ){
     free(*code) ; free(*label) ; *code=NULL ; *label=NULL ; *nlevel=0 ;
     return cv_error(err,nerr,"out of memory building the CV stratum") ;
   }
   for( ii=0 ; ii<ss->nsample ; ii++ ) (*code)[ii]=0 ;
   return 1 ;
}

static char * cv_fold_name( const char *stratum, const char *fold, int stratified )
{
   if( fold==NULL || (stratified && stratum==NULL) ) return NULL ;
   size_t n=strlen(fold)+(stratified?strlen(stratum)+2:1) ; char *out=(char *)malloc(n) ;
   if( out!=NULL ){
     if( stratified ) snprintf(out,n,"%s/%s",stratum,fold) ;
     else             snprintf(out,n,"%s",fold) ;
   }
   return out ;
}

int THD_foldplan_validate( const THD_foldplan *p, char *err, int nerr )
{
   int ff,ii,*seen,*ntest ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( p==NULL || p->nsample<1 || p->nfold<1 ) return cv_error(err,nerr,"empty fold plan") ;
   if( p->kind<THD_CV_LEAVE_GROUP_OUT || p->kind>THD_CV_FIXED || p->nstratum<1 )
     return cv_error(err,nerr,"fold plan has an invalid kind or stratum count") ;
   if( p->name==NULL || p->ntrain==NULL || p->ntest==NULL || p->train==NULL ||
       p->test==NULL || p->sample_test_fold==NULL || p->fold_stratum==NULL ||
       p->stratum_label==NULL )
     return cv_error(err,nerr,"incomplete fold plan") ;
   for( ii=0 ; ii<p->nstratum ; ii++ ) if( p->stratum_label[ii]==NULL )
     return cv_error(err,nerr,"fold plan has an unnamed stratum") ;
   seen=(int *)malloc(sizeof(int)*(size_t)p->nsample) ; ntest=(int *)calloc((size_t)p->nsample,sizeof(int)) ;
   if( seen==NULL || ntest==NULL ){ free(seen) ; free(ntest) ; return cv_error(err,nerr,"out of memory validating folds") ; }
   for( ff=0 ; ff<p->nfold ; ff++ ){
     if( p->fold_stratum[ff]<0 || p->fold_stratum[ff]>=p->nstratum ){
       free(seen) ; free(ntest) ; return cv_error(err,nerr,"fold %d has an invalid stratum",ff+1) ;
     }
     if( p->name[ff]==NULL || p->train[ff]==NULL || p->test[ff]==NULL ){
       free(seen) ; free(ntest) ; return cv_error(err,nerr,"fold %d is incomplete",ff+1) ;
     }
     if( p->ntrain[ff]<1 || p->ntest[ff]<1 ){
       free(seen) ; free(ntest) ; return cv_error(err,nerr,"fold %s has %d training and %d test samples",
                                                  p->name[ff],p->ntrain[ff],p->ntest[ff]) ;
     }
     memset(seen,0,sizeof(int)*(size_t)p->nsample) ;
     for( ii=0 ; ii<p->ntrain[ff] ; ii++ ){
       int s=p->train[ff][ii] ;
       if( s<0 || s>=p->nsample || seen[s] ){
         free(seen) ; free(ntest) ; return cv_error(err,nerr,"fold %s has an invalid or duplicate training sample",p->name[ff]) ;
       }
       seen[s]=1 ;
     }
     for( ii=0 ; ii<p->ntest[ff] ; ii++ ){
       int s=p->test[ff][ii] ;
       if( s<0 || s>=p->nsample || seen[s] ){
         free(seen) ; free(ntest) ; return cv_error(err,nerr,"fold %s has an invalid, duplicate, or training-overlap test sample",p->name[ff]) ;
       }
       seen[s]=2 ; ntest[s]++ ;
       if( p->sample_test_fold[s]!=ff ){
         free(seen) ; free(ntest) ; return cv_error(err,nerr,"fold %s disagrees with sample_test_fold for sample %d",p->name[ff],s+1) ;
       }
     }
     if( p->kind==THD_CV_FIXED ) for( ii=0 ; ii<p->nsample ; ii++ ) if( !seen[ii] ){
       free(seen) ; free(ntest) ; return cv_error(err,nerr,"fixed fold omits sample %d",ii+1) ;
     }
   }
   if( p->kind!=THD_CV_FIXED ) for( ii=0 ; ii<p->nsample ; ii++ ) if( ntest[ii]!=1 ){
     int count=ntest[ii] ;
     free(seen) ; free(ntest) ;
     return cv_error(err,nerr,"sample %d occurs in %d test folds; expected exactly one",ii+1,count) ;
   }
   free(seen) ; free(ntest) ; return 1 ;
}

THD_foldplan * THD_foldplan_leave_group_out( const THD_sampleset *ss,
                                              const char *group_column,
                                              const char *within_column,
                                              char *err, int nerr )
{
   THD_foldplan *p=NULL ; int *group=NULL,*stratum=NULL,ng=0,ns=0,st,gg,ff=0,ii,nfold=0 ;
   char **glabel=NULL,**slabel=NULL ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( ss==NULL || ss->nsample<1 || group_column==NULL ){
     cv_error(err,nerr,"leave-group-out CV needs samples and a group column") ; return NULL ;
   }
   if( !THD_sampleset_encode_column(ss,group_column,&group,&glabel,&ng,err,nerr) ) return NULL ;
   if( !cv_encode_strata(ss,within_column,&stratum,&slabel,&ns,err,nerr) ) goto bad ;
   for( st=0 ; st<ns ; st++ ){
     int here=0 ;
     for( gg=0 ; gg<ng ; gg++ ){
       int n=0 ; for( ii=0 ; ii<ss->nsample ; ii++ ) n+=(stratum[ii]==st && group[ii]==gg) ;
       if( n>0 ){ here++ ; nfold++ ; }
     }
     if( here<2 ){ cv_error(err,nerr,"%s '%s' has %d level%s of '%s'; leave-group-out CV needs at least 2",
                            within_column?within_column:"stratum",slabel[st],here,here==1?"":"s",group_column) ; goto bad ; }
   }
   p=cv_alloc(THD_CV_LEAVE_GROUP_OUT,ss->nsample,nfold,ns,group_column,within_column,slabel,err,nerr) ;
   if( p==NULL ) goto bad ;
   for( st=0 ; st<ns ; st++ ) for( gg=0 ; gg<ng ; gg++ ){
     int ntr=0,nte=0,qtr=0,qte=0 ;
     for( ii=0 ; ii<ss->nsample ; ii++ ) if( stratum[ii]==st ){
       if( group[ii]==gg ) nte++ ; else ntr++ ;
     }
     if( nte==0 ) continue ;
     p->name[ff]=cv_fold_name(slabel[st],glabel[gg],within_column!=NULL) ; p->fold_stratum[ff]=st ;
     p->ntrain[ff]=ntr ; p->ntest[ff]=nte ;
     p->train[ff]=(int *)malloc(sizeof(int)*(size_t)ntr) ; p->test[ff]=(int *)malloc(sizeof(int)*(size_t)nte) ;
     if( p->name[ff]==NULL || p->train[ff]==NULL || p->test[ff]==NULL ){
       p=cv_fail(p,err,nerr,"out of memory filling fold %d",ff+1) ; goto bad ;
     }
     for( ii=0 ; ii<ss->nsample ; ii++ ) if( stratum[ii]==st ){
       if( group[ii]==gg ){ p->test[ff][qte++]=ii ; p->sample_test_fold[ii]=ff ; }
       else p->train[ff][qtr++]=ii ;
     }
     ff++ ;
   }
   if( !THD_foldplan_validate(p,err,nerr) ){ THD_foldplan_free(p) ; p=NULL ; }
bad:
   free(group) ; free(stratum) ; THD_sampleset_free_levels(glabel,ng) ; THD_sampleset_free_levels(slabel,ns) ;
   return p ;
}

typedef struct { int group,size,first,bin ; } cv_group ;

static int cv_group_cmp( const void *a, const void *b )
{
   const cv_group *x=(const cv_group *)a,*y=(const cv_group *)b ;
   if( x->size!=y->size ) return (x->size>y->size)?-1:1 ;
   return (x->first<y->first)?-1:(x->first>y->first) ;
}

THD_foldplan * THD_foldplan_grouped_kfold( const THD_sampleset *ss,
                                            const char *group_column,
                                            const char *within_column, int k,
                                            char *err, int nerr )
{
   THD_foldplan *p=NULL ; int *group=NULL,*stratum=NULL,ng=0,ns=0,st,ii,ff=0 ;
   char **glabel=NULL,**slabel=NULL ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( ss==NULL || ss->nsample<1 || group_column==NULL || k<2 ){
     cv_error(err,nerr,"grouped K-fold needs samples, a group column, and K >= 2") ; return NULL ;
   }
   if( !THD_sampleset_encode_column(ss,group_column,&group,&glabel,&ng,err,nerr) ) return NULL ;
   if( !cv_encode_strata(ss,within_column,&stratum,&slabel,&ns,err,nerr) ) goto bad ;
   for( st=0 ; st<ns ; st++ ){
     int ngrp=0,gg ;
     for( gg=0 ; gg<ng ; gg++ ){ int n=0 ; for( ii=0 ; ii<ss->nsample ; ii++ ) n+=(stratum[ii]==st && group[ii]==gg) ; ngrp+=(n>0) ; }
     if( ngrp<k ){ cv_error(err,nerr,"%s '%s' has %d group%s in '%s'; grouped %d-fold needs at least %d",
                            within_column?within_column:"stratum",slabel[st],ngrp,ngrp==1?"":"s",group_column,k,k) ; goto bad ; }
   }
   p=cv_alloc(THD_CV_GROUPED_KFOLD,ss->nsample,ns*k,ns,group_column,within_column,slabel,err,nerr) ;
   if( p==NULL ) goto bad ;
   for( st=0 ; st<ns ; st++ ){
     cv_group *cg=(cv_group *)malloc(sizeof(cv_group)*(size_t)ng) ; int *load=(int *)calloc((size_t)k,sizeof(int)) ;
     int nc=0,gg,bb ;
     if( cg==NULL || load==NULL ){ free(cg) ; free(load) ; p=cv_fail(p,err,nerr,"out of memory balancing grouped folds") ; goto bad ; }
     for( gg=0 ; gg<ng ; gg++ ){
       int n=0,first=ss->nsample ;
       for( ii=0 ; ii<ss->nsample ; ii++ ) if( stratum[ii]==st && group[ii]==gg ){ n++ ; if( ii<first ) first=ii ; }
       if( n>0 ){ cg[nc].group=gg ; cg[nc].size=n ; cg[nc].first=first ; cg[nc].bin=-1 ; nc++ ; }
     }
     qsort(cg,(size_t)nc,sizeof(cv_group),cv_group_cmp) ;
     for( gg=0 ; gg<nc ; gg++ ){
       int best=0 ; for( bb=1 ; bb<k ; bb++ ) if( load[bb]<load[best] ) best=bb ;
       cg[gg].bin=best ; load[best]+=cg[gg].size ;
     }
     for( bb=0 ; bb<k ; bb++,ff++ ){
       char fn[32] ; int ntr=0,nte=0,qtr=0,qte=0 ;
       for( ii=0 ; ii<ss->nsample ; ii++ ) if( stratum[ii]==st ){
         int bin=-1,q ; for( q=0 ; q<nc ; q++ ) if( cg[q].group==group[ii] ){ bin=cg[q].bin ; break ; }
         if( bin==bb ) nte++ ; else ntr++ ;
       }
       snprintf(fn,sizeof(fn),"fold%d",bb+1) ; p->name[ff]=cv_fold_name(slabel[st],fn,within_column!=NULL) ; p->fold_stratum[ff]=st ;
       p->ntrain[ff]=ntr ; p->ntest[ff]=nte ; p->train[ff]=(int *)malloc(sizeof(int)*(size_t)ntr) ;
       p->test[ff]=(int *)malloc(sizeof(int)*(size_t)nte) ;
       if( p->name[ff]==NULL || p->train[ff]==NULL || p->test[ff]==NULL ){
         free(cg) ; free(load) ; p=cv_fail(p,err,nerr,"out of memory filling fold %d",ff+1) ; goto bad ;
       }
       for( ii=0 ; ii<ss->nsample ; ii++ ) if( stratum[ii]==st ){
         int bin=-1,q ; for( q=0 ; q<nc ; q++ ) if( cg[q].group==group[ii] ){ bin=cg[q].bin ; break ; }
         if( bin==bb ){ p->test[ff][qte++]=ii ; p->sample_test_fold[ii]=ff ; }
         else p->train[ff][qtr++]=ii ;
       }
     }
     free(cg) ; free(load) ;
   }
   if( !THD_foldplan_validate(p,err,nerr) ){ THD_foldplan_free(p) ; p=NULL ; }
bad:
   free(group) ; free(stratum) ; THD_sampleset_free_levels(glabel,ng) ; THD_sampleset_free_levels(slabel,ns) ;
   return p ;
}

void THD_nested_foldplan_free(THD_nested_foldplan *p)
{
   int f;
   if(!p)return;
   if(p->inner)for(f=0;f<p->outer->nfold;f++)THD_foldplan_free(p->inner[f]);
   free(p->inner);free(p);
}

THD_nested_foldplan *THD_nested_foldplan_grouped(
 const THD_foldplan *outer,const int *groups,int k,char *err,int nerr)
{
   THD_nested_foldplan *nested;int f;
   if(!THD_foldplan_validate(outer,err,nerr))return NULL;
   if(!groups || k<0 || k==1){cv_error(err,nerr,"inner grouped CV requires group IDs and K=0 or K>=2");return NULL;}
   nested=calloc(1,sizeof(*nested));
   if(!nested){cv_error(err,nerr,"out of memory allocating nested folds");return NULL;}
   nested->outer=outer;nested->inner=calloc((size_t)outer->nfold,sizeof(*nested->inner));
   if(!nested->inner)goto memory;
   for(f=0;f<outer->nfold;f++){
      int n=outer->ntrain[f],ng=0,i,j,b,nf,*load;
      cv_group *cg=calloc((size_t)n,sizeof(*cg));THD_foldplan *p;char *stratum[1];
      if(!cg)goto memory;
      for(i=0;i<n;i++){
         int g=groups[outer->train[f][i]];
         if(g<0){free(cg);cv_error(err,nerr,"outer fold %s: invalid inner group at sample %d",outer->name[f],outer->train[f][i]+1);goto bad;}
         for(j=0;j<ng;j++)if(cg[j].group==g)break;
         if(j==ng){cg[ng].group=g;cg[ng].first=i;ng++;}
         cg[j].size++;
      }
      nf=k?k:ng;
      if(ng<2 || nf>ng){
         free(cg);cv_error(err,nerr,"outer fold %s has %d inner groups; need at least %d",outer->name[f],ng,k?k:2);goto bad;
      }
      load=calloc((size_t)nf,sizeof(int));if(!load){free(cg);goto memory;}
      if(k)qsort(cg,(size_t)ng,sizeof(*cg),cv_group_cmp);
      for(j=0;j<ng;j++){
         int best=k?0:j;
         if(k)for(b=1;b<nf;b++)if(load[b]<load[best])best=b;
         cg[j].bin=best;load[best]+=cg[j].size;
      }
      stratum[0]=outer->stratum_label[outer->fold_stratum[f]];
      p=cv_alloc(k?THD_CV_GROUPED_KFOLD:THD_CV_LEAVE_GROUP_OUT,n,nf,1,
                 "inner_group",NULL,stratum,err,nerr);
      nested->inner[f]=p;
      if(!p){free(cg);free(load);goto bad;}
      for(b=0;b<nf;b++){
         char name[64];int tr=0,te=0;
         snprintf(name,sizeof(name),"inner%d",b+1);p->name[b]=strdup(name);
         p->ntest[b]=load[b];p->ntrain[b]=n-load[b];
         p->train[b]=malloc(sizeof(int)*(size_t)p->ntrain[b]);
         p->test[b]=malloc(sizeof(int)*(size_t)p->ntest[b]);
         if(!p->name[b] || !p->train[b] || !p->test[b]){free(cg);free(load);goto memory;}
         for(i=0;i<n;i++){
            for(j=0;j<ng;j++)if(cg[j].group==groups[outer->train[f][i]])break;
            if(cg[j].bin==b){p->test[b][te++]=i;p->sample_test_fold[i]=b;}
            else p->train[b][tr++]=i;
         }
      }
      free(cg);free(load);
      if(!THD_foldplan_validate(p,err,nerr))goto bad;
   }
   return nested;
memory:
   cv_error(err,nerr,"out of memory building nested grouped folds");
bad:
   THD_nested_foldplan_free(nested);return NULL;
}

void THD_nested_foldplan_print(const THD_nested_foldplan *p,const THD_sampleset *ss,FILE *fp)
{
   int f,g,set,i;
   if(!p || !fp)return;
   fprintf(fp,"# Inner folds contain only their outer training rows\nOuterFold\tInnerFold\tStratum\tSet\tRow\tSample\n");
   for(f=0;f<p->outer->nfold;f++)for(g=0;g<p->inner[f]->nfold;g++)for(set=0;set<2;set++){
      const THD_foldplan *in=p->inner[f];
      int n=set?in->ntest[g]:in->ntrain[g];int *idx=set?in->test[g]:in->train[g];
      for(i=0;i<n;i++){
         int s=p->outer->train[f][idx[i]];
         fprintf(fp,"%s\t%s\t%s\t%s\t%d\t%s\n",p->outer->name[f],in->name[g],
                 in->stratum_label[0],set?"test":"train",s+1,ss?ss->sample_label[s]:"-");
      }
   }
}

THD_foldplan * THD_foldplan_fixed( const THD_sampleset *ss,
                                    const char *partition_column,
                                    const char *train_value,
                                    const char *test_value,
                                    char *err, int nerr )
{
   THD_foldplan *p ; THD_datatable *dt ; int icol,ii,ntr=0,nte=0,qtr=0,qte=0 ; char *slab[1]={"All"} ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( ss==NULL || ss->metadata==NULL || ss->nsample<1 || partition_column==NULL ||
       train_value==NULL || test_value==NULL || *train_value=='\0' || *test_value=='\0' ||
       !strcmp(train_value,test_value) ){
     cv_error(err,nerr,"fixed CV needs a partition column and distinct train/test values") ; return NULL ;
   }
   dt=ss->metadata ; icol=THD_datatable_column(dt,(char *)partition_column) ;
   if( icol<0 ){ cv_error(err,nerr,"dataTable (%s): column '%s' is missing",dt->source?dt->source:"?",partition_column) ; return NULL ; }
   for( ii=0 ; ii<ss->nsample ; ii++ ){
     const char *v=DT_CELL(dt,ii,icol) ;
     if( !strcmp(v,train_value) ) ntr++ ; else if( !strcmp(v,test_value) ) nte++ ;
     else { cv_error(err,nerr,"dataTable (%s), row %d: '%s' is '%s'; fixed CV accepts only '%s' and '%s'",
                     dt->source?dt->source:"?",ii+1,partition_column,v,train_value,test_value) ; return NULL ; }
   }
   if( ntr<1 || nte<1 ){ cv_error(err,nerr,"fixed CV has %d training and %d test samples",ntr,nte) ; return NULL ; }
   p=cv_alloc(THD_CV_FIXED,ss->nsample,1,1,partition_column,NULL,slab,err,nerr) ; if( p==NULL ) return NULL ;
   p->name[0]=strdup(test_value) ; p->ntrain[0]=ntr ; p->ntest[0]=nte ;
   p->train[0]=(int *)malloc(sizeof(int)*(size_t)ntr) ; p->test[0]=(int *)malloc(sizeof(int)*(size_t)nte) ;
   if( p->name[0]==NULL || p->train[0]==NULL || p->test[0]==NULL ) return cv_fail(p,err,nerr,"out of memory filling fixed CV") ;
   for( ii=0 ; ii<ss->nsample ; ii++ ) if( !strcmp(DT_CELL(dt,ii,icol),train_value) ) p->train[0][qtr++]=ii ;
   else { p->test[0][qte++]=ii ; p->sample_test_fold[ii]=0 ; }
   if( !THD_foldplan_validate(p,err,nerr) ){ THD_foldplan_free(p) ; return NULL ; }
   return p ;
}

int THD_foldplan_check_classes( const THD_foldplan *p, const int *class_id,
                                int nclass, int require_test, char *err, int nerr )
{
   int ff,ii,*ntr,*nte ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( p==NULL || class_id==NULL || nclass<2 ) return cv_error(err,nerr,"class coverage needs a fold plan and at least 2 classes") ;
   for( ii=0 ; ii<p->nsample ; ii++ ) if( class_id[ii]<0 || class_id[ii]>=nclass )
     return cv_error(err,nerr,"sample %d has invalid class index %d",ii+1,class_id[ii]) ;
   ntr=(int *)malloc(sizeof(int)*(size_t)nclass) ; nte=(int *)malloc(sizeof(int)*(size_t)nclass) ;
   if( ntr==NULL || nte==NULL ){ free(ntr) ; free(nte) ; return cv_error(err,nerr,"out of memory checking class coverage") ; }
   for( ff=0 ; ff<p->nfold ; ff++ ){
     memset(ntr,0,sizeof(int)*(size_t)nclass) ; memset(nte,0,sizeof(int)*(size_t)nclass) ;
     for( ii=0 ; ii<p->ntrain[ff] ; ii++ ) ntr[class_id[p->train[ff][ii]]]++ ;
     for( ii=0 ; ii<p->ntest[ff] ; ii++ ) nte[class_id[p->test[ff][ii]]]++ ;
     for( ii=0 ; ii<nclass ; ii++ ) if( ntr[ii]==0 || (require_test && nte[ii]==0) ){
       int train_missing=(ntr[ii]==0) ;
       free(ntr) ; free(nte) ;
       return cv_error(err,nerr,"fold %s has no class %d in its %s set",p->name[ff],ii+1,train_missing?"training":"test") ;
     }
   }
   free(ntr) ; free(nte) ; return 1 ;
}

void THD_foldplan_print( const THD_foldplan *p, const THD_sampleset *ss, FILE *fp )
{
   int ff,ii ;
   if( p==NULL || fp==NULL ) return ;
   fprintf(fp,"# %d folds over %d samples; group=%s%s%s\n",p->nfold,p->nsample,p->group_column,
           p->within_column?", within=":"",p->within_column?p->within_column:"") ;
   fprintf(fp,"Fold\tStratum\tSet\tRow\tSample\n") ;
   for( ff=0 ; ff<p->nfold ; ff++ ){
     for( ii=0 ; ii<p->ntrain[ff] ; ii++ ){
       int s=p->train[ff][ii] ; const char *lab=(ss!=NULL && ss->sample_label!=NULL)?ss->sample_label[s]:"-" ;
       fprintf(fp,"%s\t%s\ttrain\t%d\t%s\n",p->name[ff],p->stratum_label[p->fold_stratum[ff]],s+1,lab) ;
     }
     for( ii=0 ; ii<p->ntest[ff] ; ii++ ){
       int s=p->test[ff][ii] ; const char *lab=(ss!=NULL && ss->sample_label!=NULL)?ss->sample_label[s]:"-" ;
       fprintf(fp,"%s\t%s\ttest\t%d\t%s\n",p->name[ff],p->stratum_label[p->fold_stratum[ff]],s+1,lab) ;
     }
   }
}
