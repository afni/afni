#include "mrilib.h"
#include "thd_sampleset.h"
#include "thd_xmat.h"

#include <unistd.h>

#define CHECK(COND,MSG) do{ if(!(COND)){ fprintf(stderr,"FAIL: %s\n",MSG); fail=1; goto done; } }while(0)

static int write_bucket( const char *fname, int nx, int nbrick )
{
   THD_3dim_dataset *ds=EDIT_empty_copy(NULL) ; THD_ivec3 nxyz ; THD_fvec3 org,del ; int bb,vv ;
   LOAD_IVEC3(nxyz,nx,2,1) ; LOAD_FVEC3(org,0.0f,0.0f,0.0f) ; LOAD_FVEC3(del,2.0f,2.0f,2.0f) ;
   EDIT_dset_items(ds,ADN_nxyz,nxyz,ADN_xyzorg,org,ADN_xyzdel,del,ADN_prefix,fname,
                   ADN_nvals,nbrick,ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_none) ;
   ds->daxes->xxorient=ORI_R2L_TYPE ; ds->daxes->yyorient=ORI_A2P_TYPE ; ds->daxes->zzorient=ORI_I2S_TYPE ;
   for( bb=0 ; bb<nbrick ; bb++ ){
     float *v=(float *)malloc(sizeof(float)*(size_t)(2*nx)) ; char lab[32] ;
     for( vv=0 ; vv<2*nx ; vv++ ) v[vv]=(float)(10*bb+vv+1) ;
     EDIT_substitute_brick(ds,bb,MRI_float,v) ;
     snprintf(lab,sizeof(lab),"%s#0_Coef",bb==0?"face":"house") ; EDIT_BRICK_LABEL(ds,bb,lab) ;
   }
   if( DSET_write(ds)==False ){ DSET_delete(ds) ; return 0 ; }
   DSET_delete(ds) ; return 1 ;
}

static int write_xmat( const char *fname )
{
   FILE *fp=fopen(fname,"w") ;
   if( fp==NULL ) return 0 ;
   fprintf(fp,"# <matrix\n#  ni_type = \"2*double\"\n#  ni_dimen = \"4\"\n"
              "#  ColumnLabels = \"face#0 ; house#0\"\n"
              "#  GoodList = \"0..3\"\n#  NRowFull = \"4\"\n#  RunStart = \"0,2\"\n"
              "#  StimBots = \"0,1\"\n#  StimTops = \"0,1\"\n#  StimLabels = \"face ; house\"\n# >\n"
              "1 0\n0 0\n0 1\n0 0\n# </matrix>\n") ;
   fclose(fp) ; return 1 ;
}

static void remove_bucket( const char *fname )
{
   char b[1024] ;
   remove(fname) ; snprintf(b,sizeof(b),"%s.HEAD",fname) ; remove(b) ;
   snprintf(b,sizeof(b),"%s.BRIK",fname) ; remove(b) ;
   snprintf(b,sizeof(b),"%s.BRIK.gz",fname) ; remove(b) ;
}

static THD_datatable * direct_table( const char *bucket, const char *single )
{
   char b0[1024],b1[1024],bl[1024] ;
   char *tok[]={ "Subj","Run","Trial","Fold","Sample","Condition","InputFile",
                 "s02","r2","tA","fB","a","face",b0,
                 "s01","r1","tB","fA","b","house",b1,
                 "s02","r1","tC","fA","c","face",(char *)single,
                 "s01","r2","tD","fB","d","face",bl } ;
   snprintf(b0,sizeof(b0),"%s[0]",bucket) ; snprintf(b1,sizeof(b1),"%s[1]",bucket) ;
   snprintf(bl,sizeof(bl),"%s[face#0_Coef]",bucket) ;
   return THD_parse_datatable(tok,(int)(sizeof(tok)/sizeof(tok[0])),"direct samples") ;
}

static int check_identity( const char *bucket, const char *single )
{
   char alias[1024],b0[1200],b1[1200],a0[1200],al[1200],dot[1200],err[512],tiny[2];
   char *canonical=NULL,*single_path=NULL;
   THD_datatable *dt=NULL;
   THD_sampleset *ss=NULL,invalid;
   THD_sampleset_identity *identity=NULL;
   int fail=0,ii,expected[]={0,1,2,0,0,1};
   snprintf(alias,sizeof(alias),"%s.alias.nii",bucket);
   canonical=realpath(bucket,NULL);single_path=realpath(single,NULL);
   CHECK(canonical && single_path,"resolve identity fixture paths");
   CHECK(symlink(canonical,alias)==0,"create identity fixture alias");
   snprintf(b0,sizeof(b0),"%s[0]",bucket);snprintf(b1,sizeof(b1),"%s[1]",bucket);
   snprintf(a0,sizeof(a0),"%s[0]",alias);snprintf(al,sizeof(al),"%s[face#0_Coef]",alias);
   { const char *base=strrchr(canonical,'/');
     snprintf(dot,sizeof(dot),"%.*s/./%s[house#0_Coef]",(int)(base-canonical),canonical,base+1); }
   { char *tok[]={"InputFile",b0,b1,(char *)single,a0,al,dot};
     dt=THD_parse_datatable(tok,7,"identity selections"); }
   ss=THD_sampleset_from_table(dt,NULL,err,sizeof(err));CHECK(ss,err);
   strcpy(err,"stale error");identity=THD_sampleset_build_identity(ss,err,sizeof(err));CHECK(identity,err);
   CHECK(!err[0] && identity->nsample==6 && identity->ndset==ss->ndset,"identity shape and cleared error");
   for(ii=0;ii<6;ii++){
     CHECK(identity->first_sample[ii]==expected[ii],"first representative for numeric, named and aliased selections");
     CHECK(!strcmp(identity->dataset_path[ss->sample_dset[ii]],ii==2?single_path:canonical),
           "canonical paths resolve symlinks and dot components");
   }
   CHECK(identity->first_sample[3]==identity->first_sample[4],
         "subsets detect overlap even when earliest representative is outside both subsets");
   CHECK(identity->first_sample[0]!=identity->first_sample[1],"different bricks remain distinct");
   CHECK(identity->first_sample[0]!=identity->first_sample[2],"equal voxel values in different files remain distinct");

   CHECK(!THD_sampleset_build_identity(NULL,err,sizeof(err)) && strstr(err,"nonempty"),"NULL sample set rejected");
   memset(&invalid,0,sizeof(invalid));
   CHECK(!THD_sampleset_build_identity(&invalid,err,sizeof(err)),"empty sample set rejected");
   invalid=*ss;invalid.sample_brick=NULL;
   CHECK(!THD_sampleset_build_identity(&invalid,err,sizeof(err)),"missing selection storage rejected");
   { int bad_dset[]={0,0,0,0,0,ss->ndset},bad_brick[]={0,0,0,0,0,-1};
     invalid=*ss;invalid.sample_dset=bad_dset;
     CHECK(!THD_sampleset_build_identity(&invalid,err,sizeof(err)) && strstr(err,"sample 6"),"out-of-range dataset rejected");
     bad_dset[5]=-1;
     CHECK(!THD_sampleset_build_identity(&invalid,err,sizeof(err)),"negative dataset rejected");
     invalid=*ss;invalid.sample_brick=bad_brick;
     CHECK(!THD_sampleset_build_identity(&invalid,err,sizeof(err)),"negative brick rejected");
     bad_brick[5]=DSET_NVALS(ss->dset[ss->sample_dset[5]]);
     CHECK(!THD_sampleset_build_identity(&invalid,tiny,sizeof(tiny)) && !tiny[1],"brick range and bounded error"); }
   { THD_3dim_dataset *missing[]={NULL};
     invalid=*ss;invalid.ndset=1;invalid.dset=missing;
     CHECK(!THD_sampleset_build_identity(&invalid,err,sizeof(err)) && strstr(err,"header path"),"invalid dataset header rejected"); }
   THD_sampleset_free(ss);ss=NULL;THD_free_datatable(dt);dt=NULL;
   CHECK(identity->first_sample[4]==0 && !strcmp(identity->dataset_path[0],canonical),
         "identity snapshot owns storage after source sample set is freed");
   THD_sampleset_free_identity(identity);identity=NULL;

   /* An in-memory header with no on-disk file uses its original header path. */
   { THD_3dim_dataset *offline=EDIT_empty_copy(NULL);
     int zero=0;THD_sampleset source={0};
     EDIT_dset_items(offline,ADN_prefix,alias,ADN_nvals,1,ADN_none);
     remove(alias);
     source.nsample=source.ndset=1;source.dset=&offline;
     source.sample_dset=source.sample_brick=&zero;
     identity=THD_sampleset_build_identity(&source,NULL,0);
     ii=identity && !strcmp(identity->dataset_path[0],DSET_HEADNAME(offline));
     DSET_delete(offline);
     CHECK(ii,"unresolvable header path is retained without loading data"); }
done:
   THD_sampleset_free_identity(identity);THD_sampleset_free_identity(NULL);
   THD_sampleset_free(ss);THD_free_datatable(dt);free(canonical);free(single_path);remove(alias);
   return fail;
}

static int check_numeric_columns(void)
{
   char *tokens[]={"A","B","InputFile","1","10","mock_a","2","20","mock_b","3","30","mock_c"};
   const char *columns[]={"b","A"};int rows[]={2,0,2,1},fail=0,i;
   const float expected[]={30,3,10,1,30,3,20,2};
   THD_datatable *dt=THD_parse_datatable(tokens,12,"numeric metadata");
   THD_sampleset ss={0},invalid;float *v=NULL;char err[256];
   /* A nonidentity map, with more samples than metadata rows, exposes any
      accidental use of metadata order instead of sample order. */
   ss.nsample=4;ss.table_row=rows;ss.metadata=dt;
   strcpy(err,"stale error");v=THD_sampleset_numeric_columns(&ss,columns,2,err,sizeof(err));
   CHECK(v && !err[0],"extract numeric samples without dataset access");
   for(i=0;i<8;i++)CHECK(v[i]==expected[i],"numeric output follows repeated/reordered sample map");
   free(v);v=NULL;
   CHECK(!THD_sampleset_numeric_columns(NULL,columns,2,err,sizeof(err)),"NULL numeric sample set rejected");
   invalid=ss;invalid.table_row=NULL;
   CHECK(!THD_sampleset_numeric_columns(&invalid,columns,2,err,sizeof(err)),"missing sample row map rejected");
   invalid=ss;invalid.nsample=0;
   CHECK(!THD_sampleset_numeric_columns(&invalid,columns,2,err,sizeof(err)),"empty numeric sample set rejected");
   invalid=ss;invalid.metadata=NULL;
   CHECK(!THD_sampleset_numeric_columns(&invalid,columns,2,err,sizeof(err)),"missing sample metadata rejected");
   rows[0]=3;
   CHECK(!THD_sampleset_numeric_columns(&ss,columns,2,err,sizeof(err)) && strstr(err,"output row 1"),"sample row bounds checked");
   rows[0]=2;dt->val[0][2]=INFINITY;
   CHECK(!THD_sampleset_numeric_columns(&ss,columns,2,err,sizeof(err)) &&
         strstr(err,"output row 1 (table row 3) is not finite"),"finite diagnostics identify sample and metadata positions");
done:
   free(v);THD_free_datatable(dt);return fail;
}

int main( void )
{
   char *tmp=getenv("TMPDIR"),bucket[900],single[900],other[900],xmat[900],err[1024] ;
   THD_datatable *dt=NULL,*bad=NULL,*xt=NULL,*xe=NULL,*xd=NULL ;
   THD_sampleset *ss=NULL,*sx=NULL,*sd=NULL ; THD_sampleset_columns col ;
   int fail=0,*code=NULL,nlev=0,have_run=0 ; char **lev=NULL ; FILE *fp=NULL ;
   if( tmp==NULL ) tmp="." ;
   snprintf(bucket,sizeof(bucket),"%s/thd_sampleset_%ld_bucket.nii",tmp,(long)getpid()) ;
   snprintf(single,sizeof(single),"%s/thd_sampleset_%ld_single.nii",tmp,(long)getpid()) ;
   snprintf(other,sizeof(other),"%s/thd_sampleset_%ld_other.nii",tmp,(long)getpid()) ;
   snprintf(xmat,sizeof(xmat),"%s/thd_sampleset_%ld.xmat.1D",tmp,(long)getpid()) ;
   CHECK(write_bucket(bucket,2,2),"write the two-brick bucket") ;
   CHECK(write_bucket(single,2,1),"write the one-brick dataset") ;
   CHECK(write_bucket(other,3,1),"write the mismatched-grid dataset") ;
   CHECK(write_xmat(xmat),"write the X-matrix") ;
   CHECK(!check_identity(bucket,single),"shared sample identity checks") ;
   CHECK(!check_numeric_columns(),"sample-order numeric metadata") ;

   dt=direct_table(bucket,single) ; CHECK(dt!=NULL,"build a direct sample table") ;
   THD_sampleset_detect_columns(dt,&col) ;
   ss=THD_sampleset_from_table(dt,&col,err,sizeof(err)) ; CHECK(ss!=NULL,err) ;
   THD_free_datatable(dt) ; dt=NULL ;                         /* the sample set owns its copy */
   CHECK(ss->nsample==4 && ss->ndset==2 && ss->nvox==4,"sample, dataset, and grid counts") ;
   CHECK(ss->nsubject==2 && !strcmp(ss->subject_label[0],"s02") && !strcmp(ss->subject_label[1],"s01"),
         "subjects encoded in first-appearance order") ;
   CHECK(ss->nrun==2 && !strcmp(ss->run_label[0],"r2") && ss->run[1]==1,"run encoding") ;
   CHECK(ss->nitem==4 && ss->nfold==2 && ss->fold[0]==0 && ss->fold[1]==1,"item and fold encoding") ;
   CHECK(ss->sample_dset[0]==0 && ss->sample_dset[1]==0 && ss->sample_dset[2]==1,
         "base datasets deduplicated") ;
   CHECK(ss->sample_brick[0]==0 && ss->sample_brick[1]==1 && ss->sample_brick[3]==0,
         "numeric and label selectors resolved") ;
   CHECK(!strcmp(ss->sample_label[2],"c") && ss->table_row[3]==3,"sample labels and original rows") ;
   CHECK(THD_sampleset_encode_column(ss,"Condition",&code,&lev,&nlev,err,sizeof(err)),err) ;
   CHECK(nlev==2 && !strcmp(lev[0],"face") && !strcmp(lev[1],"house") && code[3]==0,
         "an arbitrary categorical column") ;
   free(code) ; code=NULL ; THD_sampleset_free_levels(lev,nlev) ; lev=NULL ;
   fp=tmpfile() ; CHECK(fp!=NULL,"open the audit output") ; THD_sampleset_print(ss,fp) ; rewind(fp) ;
   { char out[4096] ; size_t n=fread(out,1,sizeof(out)-1,fp) ; out[n]='\0' ;
     CHECK(strstr(out,"Row\tSample\tSubj")!=NULL && strstr(out,"face#0_Coef")!=NULL,"print the resolved samples") ; }
   fclose(fp) ; fp=NULL ; THD_sampleset_free(ss) ; ss=NULL ;

   /* The Xmat adapter and a hand-written table converge on the same sample map. */
   { char *tok[]={ "Subj","Xmat","InputFile", "s01",xmat,bucket } ;
     xt=THD_parse_datatable(tok,6,"Xmat subjects") ; }
   xe=THD_datatable_expand_xmat_items(xt,"Xmat","Item","Run",NULL,&have_run) ;
   CHECK(xe!=NULL && have_run && xe->nrow==2,"expand Xmat items and runs") ;
   THD_sampleset_detect_columns(xe,&col) ; sx=THD_sampleset_from_table(xe,&col,err,sizeof(err)) ; CHECK(sx!=NULL,err) ;
   { char s0[1024],s1[1024] ; char *tok[]={ "Subj","Item","Condition","Run","InputFile",
       "s01","face","face","1",s0, "s01","house","house","2",s1 } ;
     snprintf(s0,sizeof(s0),"%s[0]",bucket) ; snprintf(s1,sizeof(s1),"%s[1]",bucket) ;
     xd=THD_parse_datatable(tok,15,"manual equivalents") ; }
   THD_sampleset_detect_columns(xd,&col) ; sd=THD_sampleset_from_table(xd,&col,err,sizeof(err)) ; CHECK(sd!=NULL,err) ;
   CHECK(sx->nsample==sd->nsample && sx->nrun==sd->nrun && sx->nitem==sd->nitem,"Xmat and manual dimensions agree") ;
   CHECK(!strcmp(sx->item_label[0],sd->item_label[0]) && !strcmp(sx->item_label[1],sd->item_label[1]) &&
         sx->sample_brick[0]==sd->sample_brick[0] && sx->sample_brick[1]==sd->sample_brick[1],
         "Xmat and manual samples resolve identically") ;

   /* Recoverable validation errors name the problem instead of exiting. */
   { char *tok[]={ "Subj","InputFile", "s01",bucket } ; bad=THD_parse_datatable(tok,4,"no selector") ; }
   THD_sampleset_detect_columns(bad,&col) ; CHECK(THD_sampleset_from_table(bad,&col,err,sizeof(err))==NULL && strstr(err,"select exactly one")!=NULL,
         "reject a multi-brick dataset without a selector") ; THD_free_datatable(bad) ; bad=NULL ;
   { char both[1024] ; char *tok[]={ "Subj","InputFile", "s01",both } ; snprintf(both,sizeof(both),"%s[0,1]",bucket) ;
     bad=THD_parse_datatable(tok,4,"two selectors") ; }
   THD_sampleset_detect_columns(bad,&col) ; CHECK(THD_sampleset_from_table(bad,&col,err,sizeof(err))==NULL && strstr(err,"selects 2")!=NULL,
         "reject a selector with two bricks") ; THD_free_datatable(bad) ; bad=NULL ;
   { char one[1024] ; char *tok[]={ "Subj","Sample","InputFile", "s01","same",one, "s02","same",one } ;
     snprintf(one,sizeof(one),"%s",single) ; bad=THD_parse_datatable(tok,9,"duplicate labels") ; }
   THD_sampleset_detect_columns(bad,&col) ; CHECK(THD_sampleset_from_table(bad,&col,err,sizeof(err))==NULL && strstr(err,"duplicate sample label")!=NULL,
         "reject duplicate explicit sample labels") ; THD_free_datatable(bad) ; bad=NULL ;
   { char *tok[]={ "Subj","InputFile", "s01",single } ; bad=THD_parse_datatable(tok,4,"missing role") ; }
   THD_sampleset_detect_columns(bad,&col) ; col.run="NoSuchRun" ;
   CHECK(THD_sampleset_from_table(bad,&col,err,sizeof(err))==NULL && strstr(err,"NoSuchRun")!=NULL,
         "reject a requested metadata role that is absent") ; THD_free_datatable(bad) ; bad=NULL ;
   { char a[1024],b[1024] ; char *tok[]={ "Subj","InputFile", "s01",a, "s02",b } ;
     snprintf(a,sizeof(a),"%s[0]",bucket) ; snprintf(b,sizeof(b),"%s",other) ; bad=THD_parse_datatable(tok,6,"grid mismatch") ; }
   THD_sampleset_detect_columns(bad,&col) ; CHECK(THD_sampleset_from_table(bad,&col,err,sizeof(err))==NULL && strstr(err,"not on the grid")!=NULL,
         "reject incompatible grids") ;

done:
   if( fp!=NULL ) fclose(fp) ; free(code) ; THD_sampleset_free_levels(lev,nlev) ;
   THD_sampleset_free(ss) ; THD_sampleset_free(sx) ; THD_sampleset_free(sd) ;
   THD_free_datatable(dt) ; THD_free_datatable(bad) ; THD_free_datatable(xt) ; THD_free_datatable(xe) ; THD_free_datatable(xd) ;
   remove(xmat) ; remove_bucket(bucket) ; remove_bucket(single) ; remove_bucket(other) ;
   if( fail ) return 1 ;
   puts("PASS thd_sampleset direct and Xmat-expanded sample organization") ; return 0 ;
}
