/* Command-line driver for thd_srm.c, for comparing against other
   implementations; tests/scripts/check_3dSRM.py uses it as the reference for
   3dSRM.  Raw float32 in, raw binary out; no AFNI libraries.

   srm_cli --algo prob|det --k K --niter N --center 0|1 --T T --out PREFIX
           [--init PRE] [--nstart N] [--tol X] [--seed S]
           --view V FILE [--view V FILE ...]

     --seglen a,b,c  run lengths (comma list summing to T); with --coupling L
                 fits a separate W per run tied to the subject anchor, worth L
                 time points of data.  Also writes PREFIX.Wr<i>_<r>.f32
     --coupling L    see --seglen
     --init PRE  start from PRE.W<i>.f32, the V_i x K float32 W for view i
                 (rho2 = 1 and sigma_s = I are used for prob)
     --init-rho2 F   with --init and prob: starting noise variances (float64, one
                 per view) instead of 1
     --init-sigma F  with --init and prob: starting sigma_s (float32 k x k)
                 instead of the identity
     --nstart N  keep the best of N random starts (default 1)
     --kernel K  auto, voxel or gram (SRM_params.kernel; default auto)
     --obs I F   view I (0-based) is observed where the T bytes of F are 1, missing
                 where 0 (SRM_params.obs); missing values are never read
     --tol X     relative convergence tolerance (default 0: run exactly N
                 iterations)
     --view V F  a view with V rows, stored as V x T float32 row-major in F

   Model files:
     --save-model FILE      after the fit, write the model (SRM_save_model) with
                            labels s00, s01, ...; one run unless --seglen is given
     --load-model FILE      do not fit: read the model and go straight to
                            --update-view (also needs --out, and --niter/--tol/
                            --coupling/--seglen as for the update)

   Letting S absorb new subjects, after everything else has been written:
     --update-view V FILE   a new subject's data (V x T); may be repeated.  They
                            are added in one SRM_update call (or one call each
                            with --update-each, in the order given).
     --update-each          one update per subject instead of one together
     --streams a,b,...      the random start of view i uses stream a_i
                            (SRM_params.stream; default: the view's index)
     --update-obs J F       --update-view J (0-based) is observed where the T
                            bytes of F are 1 (SRM_update's p->obs)
     writes PREFIX.u_S.f32, u_W<j>.f32 (the new subjects), u_rho2.f64 (all),
     u_sigma.f32, u_asum.f64, u_obj.f64 (last call), u_info.txt:
     niter converged drift nupdate nsubj_eff rho_inv_sum

   Held-out metrics (need --test-T and --test-view):
     --metrics WIN          for every view i, against the leave-one-out reference
                            from the other views: writes PREFIX.metrics.txt, one
                            row per view: seg_acc seg_chance comp_corr ident_acc
                            r2 (r2 uses the posterior-mean reference for a PROB
                            model, the plain mean for DET)
     --compare-S FILE       k x T float32 shared response to compare with the
                            fitted one: PREFIX.similarity.txt (mean cos^2, then
                            the k cosines)
     --split-rel V C FA FB  two V x C float32 measurements of the same samples:
                            PREFIX.splitrel.txt (r_pattern r_series)
     --zscore V FILE        z-score the V x T float32 FILE within the runs given
                            by --seglen: PREFIX.z.f32 and PREFIX.zbad.txt
                            (count, then the flags).  No model is fitted.

   Using the fitted model (all optional; files are raw float32, k x columns):
     --canon                canonicalize the gauge before anything is written
     --test-T T2            columns of the held-out views
     --test-view V FILE     held-out data for view i (V x T2), given once per
                            training view, in order.  Writes PREFIX.proj<i>.f32
                            (SRM_project), PREFIX.shw.f32 / PREFIX.shu.f32 (shared
                            response from all views: posterior mean / plain mean)
     --loo I                with --test-view: also PREFIX.looW.f32 / looU.f32, the
                            shared response from every view except I
     --new-train V FILE     a new subject's data on the training columns (V x T):
                            writes PREFIX.newW.f32 and PREFIX.newrho2.f64
     --new-test V FILE      that subject's held-out data: PREFIX.newproj.f32

   Writes PREFIX.S.f32, PREFIX.W<i>.f32, PREFIX.rho2.f64, PREFIX.sigma.f32,
   PREFIX.asum.f64, PREFIX.obj.f64 (the kept start's trace), PREFIX.starts.f64
   (every start's final objective), and PREFIX.info.txt:
   niter_done converged rho_inv_sum best_start. */

#include "../../thd_srm.c"

static float * read_f32( const char *name , size_t n )
{
   FILE *fp = fopen(name,"rb") ; float *a ;
   if( fp == NULL ){ fprintf(stderr,"cannot open %s\n",name) ; exit(2) ; }
   a = (float *)malloc(n*sizeof(float)) ;
   if( a == NULL || fread(a,sizeof(float),n,fp) != n ){
     fprintf(stderr,"%s: expected %zu floats\n",name,n) ; exit(2) ;
   }
   fclose(fp) ; return a ;
}

static void write_raw( const char *prefix , const char *suffix , const void *a ,
                       size_t sz , size_t n )
{
   char name[4096] ; FILE *fp ;
   snprintf(name,sizeof(name),"%s.%s",prefix,suffix) ;
   fp = fopen(name,"wb") ;
   if( fp == NULL ){ fprintf(stderr,"cannot write %s\n",name) ; exit(2) ; }
   fwrite(a,sz,n,fp) ; fclose(fp) ;
}

static const char *upd_obsfile[4096] ;              /* --update-obs */

/* SRM_update with the views in upd_file[], all at once or one call each, and the results written */
static int run_updates( SRM_model *m , SRM_params *pp , int nupd , int upd_each , const int *upd_rows ,
                        const char **upd_file , int T , const char *out )
{
   int k=m->nfeat , j , ncalls = upd_each ? nupd : 1 , done=0 , c , rc , anyo=0 ; SRM_fitinfo fu ;
   float **xu=(float **)malloc(sizeof(float *)*nupd) ; int old_nview=m->nview ; char name[64] ;
   unsigned char **uo=(unsigned char **)calloc(nupd,sizeof(unsigned char *)) ; const unsigned char * const *fit_obs=pp->obs ;
   memset(&fu,0,sizeof(fu)) ;
   for( j=0 ; j<nupd ; j++ ) xu[j]=read_f32(upd_file[j],(size_t)upd_rows[j]*T) ;
   for( j=0 ; j<nupd ; j++ ) if( upd_obsfile[j]!=NULL ){
     FILE *fo=fopen(upd_obsfile[j],"rb") ;
     uo[j]=(unsigned char *)malloc((size_t)T) ;
     if( fo==NULL || fread(uo[j],1,(size_t)T,fo)!=(size_t)T ){ fprintf(stderr,"cannot read %s\n",upd_obsfile[j]) ; return 2 ; }
     fclose(fo) ; anyo=1 ;
   }
   for( c=0 ; c<ncalls ; c++ ){
     int n1 = upd_each ? 1 : nupd ;
     memset(&fu,0,sizeof(fu)) ;
     pp->obs = anyo ? (const unsigned char * const *)(uo+done) : NULL ;
     rc = SRM_update(m,n1,upd_rows+done,xu+done,pp,&fu) ;
     if( rc != SRM_OK ){ fprintf(stderr,"SRM_update: %s\n",SRM_strerror(rc)) ; return 1 ; }
     done += n1 ;
     if( c < ncalls-1 ) SRM_free_fitinfo(&fu) ;
   }
   pp->obs=fit_obs ;
   write_raw(out,"u_S.f32",m->s,sizeof(float),(size_t)k*T) ;
   write_raw(out,"u_asum.f64",m->a_sum,sizeof(double),(size_t)k*T) ;
   if( m->nsubj_t!=NULL ) write_raw(out,"u_nsubj_t.f64",m->nsubj_t,sizeof(double),(size_t)T) ;
   for( j=0 ; j<nupd ; j++ ){
     snprintf(name,sizeof(name),"u_W%d.f32",j) ;
     write_raw(out,name,m->w[old_nview+j],sizeof(float),(size_t)upd_rows[j]*k) ;
     if( m->wrun != NULL ){
       int rr ;
       for( rr=0 ; rr<m->nseg ; rr++ ){
         snprintf(name,sizeof(name),"u_Wr%d_%d.f32",j,rr) ;
         write_raw(out,name,m->wrun[(size_t)(old_nview+j)*m->nseg+rr],sizeof(float),(size_t)upd_rows[j]*k) ;
       }
     }
   }
   if( m->rho2    ) write_raw(out,"u_rho2.f64",m->rho2,sizeof(double),m->nview) ;
   if( m->sigma_s ) write_raw(out,"u_sigma.f32",m->sigma_s,sizeof(float),(size_t)k*k) ;
   write_raw(out,"u_obj.f64",fu.objective,sizeof(double),(size_t)fu.niter_done+1) ;
   {
     char fn[4096] ; FILE *fp ;
     snprintf(fn,sizeof(fn),"%s.u_info.txt",out) ;
     fp=fopen(fn,"w") ;
     fprintf(fp,"%d %d %.17g %d %d %.17g\n",fu.niter_done,fu.converged,m->drift,m->nupdate,m->nsubj_eff,m->rho_inv_sum) ;
     fclose(fp) ;
   }
   SRM_free_fitinfo(&fu) ;
   return 0 ;
}

int main( int argc , char **argv )
{
   int nview=0 , T=0 , ii , rc , nstart=1 , best=0 , init_given=0 ;
   int *nvox=NULL ; float **x=NULL ; double *start_obj ;
   SRM_params p ; SRM_model *m=NULL , init ; SRM_fitinfo fi ;
   char name[64] ; const char *out=NULL , *init_prefix=NULL ;
   int canon=0 , T2=0 , ntest=0 , loo=-1 , new_nv=0 , new_tnv=0 ;
   int nupd=0 , upd_each=0 , upd_rows[4096] ; const char *upd_file[4096] ;
   const char *save_model=NULL , *load_model=NULL , *init_rho2=NULL , *init_sigma=NULL ;
   int metrics_win=0 , sr_v=0 , sr_c=0 , zs_v=0 ; const char *cmpS_file=NULL , *sr_fa=NULL , *sr_fb=NULL , *zs_file=NULL ;
   const char *tfile[4096] ; int trows[4096] ;
   const char *new_file=NULL , *new_tfile=NULL ;
   const char *vfile[4096] ;
   int segbuf[256] , nseg=0 ;
   int vrows[4096] ;
   const char *obsfile[4096]={0} ;

   SRM_params_default(&p) ; p.tol=0.0 ;
   for( ii=1 ; ii<argc ; ii++ ){
     const char *a=argv[ii] ;
#define NEEDS(n) do{ if( ii+(n)>=argc ){ fprintf(stderr,"%s needs %d value(s)\n",a,(n)) ; return 2 ; } }while(0)
     if( !strcmp(a,"--algo") ){ NEEDS(1) ; p.algo = strcmp(argv[++ii],"det")==0 ? SRM_ALGO_DET : SRM_ALGO_PROB ; }
     else if( !strcmp(a,"--k") ){ NEEDS(1) ; p.nfeat=atoi(argv[++ii]) ; }
     else if( !strcmp(a,"--niter") ){ NEEDS(1) ; p.niter=atoi(argv[++ii]) ; }
     else if( !strcmp(a,"--center") ){ NEEDS(1) ; p.center=atoi(argv[++ii]) ; }
     else if( !strcmp(a,"--T") ){ NEEDS(1) ; T=atoi(argv[++ii]) ; }
     else if( !strcmp(a,"--out") ){ NEEDS(1) ; out=argv[++ii] ; }
     else if( !strcmp(a,"--init") ){ NEEDS(1) ; init_prefix=argv[++ii] ; init_given=1 ; }
     else if( !strcmp(a,"--nstart") ){ NEEDS(1) ; nstart=atoi(argv[++ii]) ; }
     else if( !strcmp(a,"--tol") ){ NEEDS(1) ; p.tol=atof(argv[++ii]) ; }
     else if( !strcmp(a,"--obs") ){ NEEDS(2) ;
       { int iv=atoi(argv[ii+1]) ; if( iv<0 || iv>=4096 ){ fprintf(stderr,"--obs: bad view\n") ; return 2 ; }
         obsfile[iv]=argv[ii+2] ; ii+=2 ; } }
     else if( !strcmp(a,"--kernel") ){ NEEDS(1) ; ++ii ;
       p.kernel = !strcmp(argv[ii],"gram") ? SRM_KERNEL_GRAM : !strcmp(argv[ii],"voxel") ? SRM_KERNEL_VOXEL : SRM_KERNEL_AUTO ; }
     else if( !strcmp(a,"--seed") ){ NEEDS(1) ; p.seed=(unsigned int)strtoul(argv[++ii],NULL,10) ; }
     else if( !strcmp(a,"--streams") ){ NEEDS(1) ; { static int st[4096] ; int n=0 ; char *c=argv[++ii] , *e ;
       while( *c && n<4096 ){ st[n++]=(int)strtol(c,&e,10) ; c=(*e==',') ? e+1 : e ; if( e==c && *e!=',' ) break ; }
       p.stream=st ; } }
     else if( !strcmp(a,"--canon") ){ canon=1 ; }
     else if( !strcmp(a,"--update-each") ){ upd_each=1 ; }
     else if( !strcmp(a,"--update-obs") ){ NEEDS(2) ; { int jv=atoi(argv[ii+1]) ;
       if( jv<0 || jv>=4096 ){ fprintf(stderr,"--update-obs: bad index\n") ; return 2 ; }
       upd_obsfile[jv]=argv[ii+2] ; ii+=2 ; } }
     else if( !strcmp(a,"--init-rho2") ){ NEEDS(1) ; init_rho2=argv[++ii] ; }
     else if( !strcmp(a,"--init-sigma") ){ NEEDS(1) ; init_sigma=argv[++ii] ; }
     else if( !strcmp(a,"--save-model") ){ NEEDS(1) ; save_model=argv[++ii] ; }
     else if( !strcmp(a,"--load-model") ){ NEEDS(1) ; load_model=argv[++ii] ; }
     else if( !strcmp(a,"--metrics") ){ NEEDS(1) ; metrics_win=atoi(argv[++ii]) ; }
     else if( !strcmp(a,"--compare-S") ){ NEEDS(1) ; cmpS_file=argv[++ii] ; }
     else if( !strcmp(a,"--split-rel") ){ NEEDS(4) ; sr_v=atoi(argv[ii+1]) ; sr_c=atoi(argv[ii+2]) ; sr_fa=argv[ii+3] ; sr_fb=argv[ii+4] ; ii+=4 ; }
     else if( !strcmp(a,"--zscore") ){ NEEDS(2) ; zs_v=atoi(argv[ii+1]) ; zs_file=argv[ii+2] ; ii+=2 ; }
     else if( !strcmp(a,"--update-view") ){
       NEEDS(2) ; upd_rows[nupd]=atoi(argv[ii+1]) ; upd_file[nupd]=argv[ii+2] ; nupd++ ; ii+=2 ;
     }
     else if( !strcmp(a,"--coupling") ){ NEEDS(1) ; p.coupling=atof(argv[++ii]) ; }
     else if( !strcmp(a,"--seglen") ){
       char *buf , *tok ; NEEDS(1) ;
       buf=strdup(argv[++ii]) ;
       for( tok=strtok(buf,",") ; tok!=NULL && nseg<256 ; tok=strtok(NULL,",") ) segbuf[nseg++]=atoi(tok) ;
       free(buf) ; p.nseg=nseg ; p.seglen=segbuf ;
     }
     else if( !strcmp(a,"--test-T") ){ NEEDS(1) ; T2=atoi(argv[++ii]) ; }
     else if( !strcmp(a,"--loo") ){ NEEDS(1) ; loo=atoi(argv[++ii]) ; }
     else if( !strcmp(a,"--test-view") ){
       NEEDS(2) ; trows[ntest]=atoi(argv[ii+1]) ; tfile[ntest]=argv[ii+2] ; ntest++ ; ii+=2 ;
     }
     else if( !strcmp(a,"--new-train") ){ NEEDS(2) ; new_nv=atoi(argv[ii+1]) ; new_file=argv[ii+2] ; ii+=2 ; }
     else if( !strcmp(a,"--new-test") ){ NEEDS(2) ; new_tnv=atoi(argv[ii+1]) ; new_tfile=argv[ii+2] ; ii+=2 ; }
     else if( !strcmp(a,"--view") ){
       NEEDS(2) ;
       if( nview>=4096 ){ fprintf(stderr,"too many views\n") ; return 2 ; }
       vrows[nview]=atoi(argv[ii+1]) ; vfile[nview]=argv[ii+2] ; nview++ ; ii+=2 ;
     } else { fprintf(stderr,"unknown option %s\n",a) ; return 2 ; }
   }
   if( load_model != NULL ){
     SRM_model *lm=NULL ; SRM_provenance *lp=NULL ;
     if( out==NULL || nupd<1 ){ fprintf(stderr,"--load-model needs --out and --update-view\n") ; return 2 ; }
     rc = SRM_load_model(load_model,&lm,&lp) ;
     if( rc != SRM_OK ){ fprintf(stderr,"SRM_load_model: %s\n",SRM_strerror(rc)) ; return 1 ; }
     rc = run_updates(lm,&p,nupd,upd_each,upd_rows,upd_file,lm->ntime,out) ;
     return rc ;
   }
   if( sr_fa != NULL && out != NULL ){
     float *xa=read_f32(sr_fa,(size_t)sr_v*sr_c) , *xb=read_f32(sr_fb,(size_t)sr_v*sr_c) ;
     double rp=0.0 , rs=0.0 ; char fn[4096] ; FILE *fp ;
     rc=SRM_split_reliability(xa,xb,sr_v,sr_c,&rp,&rs) ;
     if( rc != SRM_OK ){ fprintf(stderr,"SRM_split_reliability: %s\n",SRM_strerror(rc)) ; return 1 ; }
     snprintf(fn,sizeof(fn),"%s.splitrel.txt",out) ; fp=fopen(fn,"w") ; fprintf(fp,"%.17g %.17g\n",rp,rs) ; fclose(fp) ;
     return 0 ;
   }
   if( zs_file != NULL && out != NULL && T > 0 ){
     float *xz=read_f32(zs_file,(size_t)zs_v*T) ; unsigned char *bad=(unsigned char *)calloc(zs_v,1) ;
     char fn[4096] ; FILE *fp ; int f ;
     rc=SRM_zscore_segments(xz,zs_v,T,nseg>0 ? nseg : 1,nseg>0 ? segbuf : &T,bad) ;
     if( rc < 0 ){ fprintf(stderr,"SRM_zscore_segments: %s\n",SRM_strerror(rc)) ; return 1 ; }
     write_raw(out,"z.f32",xz,sizeof(float),(size_t)zs_v*T) ;
     snprintf(fn,sizeof(fn),"%s.zbad.txt",out) ; fp=fopen(fn,"w") ; fprintf(fp,"%d\n",rc) ;
     for( f=0 ; f<zs_v ; f++ ) fprintf(fp,"%d\n",(int)bad[f]) ;
     fclose(fp) ;
     return 0 ;
   }
   if( nview<1 || T<1 || out==NULL || p.nfeat<1 ){
     fprintf(stderr,"usage: srm_cli --algo prob|det --k K --niter N --center 0|1 --T T --out PREFIX "
                    "[--init PRE] [--nstart N] [--tol X] [--seed S] --view V FILE ...\n") ;
     return 2 ;
   }
   nvox = (int *)malloc(sizeof(int)*nview) ;
   x    = (float **)malloc(sizeof(float *)*nview) ;
   for( ii=0 ; ii<nview ; ii++ ){
     nvox[ii] = vrows[ii] ;
     x[ii]    = read_f32(vfile[ii],(size_t)nvox[ii]*T) ;
   }
   { int any=0 ; unsigned char **ob=(unsigned char **)calloc(nview,sizeof(unsigned char *)) ;
     for( ii=0 ; ii<nview ; ii++ ) if( obsfile[ii]!=NULL ){
       FILE *fo=fopen(obsfile[ii],"rb") ;
       ob[ii]=(unsigned char *)malloc((size_t)T) ;
       if( fo==NULL || fread(ob[ii],1,(size_t)T,fo)!=(size_t)T ){ fprintf(stderr,"cannot read %s\n",obsfile[ii]) ; return 2 ; }
       fclose(fo) ; any=1 ;
     }
     if( any ) p.obs=(const unsigned char * const *)ob ; else free(ob) ; }

   if( init_given ){
     int k=p.nfeat ;
     memset(&init,0,sizeof(init)) ;
     init.algo=p.algo ; init.nview=nview ; init.ntime=T ; init.nfeat=k ;
     init.nvox = nvox ;
     init.w = (float **)malloc(sizeof(float *)*nview) ;
     for( ii=0 ; ii<nview ; ii++ ){
       char fn[4096] ;
       snprintf(fn,sizeof(fn),"%s.W%d.f32",init_prefix,ii) ;
       init.w[ii] = read_f32(fn,(size_t)nvox[ii]*k) ;
     }
     if( p.algo == SRM_ALGO_PROB ){
       int j ;
       init.rho2    = (double *)malloc(sizeof(double)*nview) ;
       init.sigma_s = (float *)calloc((size_t)k*k,sizeof(float)) ;
       for( ii=0 ; ii<nview ; ii++ ) init.rho2[ii] = 1.0 ;
       for( j=0 ; j<k ; j++ ) init.sigma_s[j*k+j] = 1.0f ;
       if( init_rho2!=NULL ){
         FILE *fr=fopen(init_rho2,"rb") ;
         if( fr==NULL || fread(init.rho2,sizeof(double),(size_t)nview,fr)!=(size_t)nview ){ fprintf(stderr,"cannot read %s\n",init_rho2) ; return 2 ; }
         fclose(fr) ;
       }
       if( init_sigma!=NULL ){
         float *sg=read_f32(init_sigma,(size_t)k*k) ; memcpy(init.sigma_s,sg,sizeof(float)*(size_t)k*k) ; free(sg) ;
       }
     }
     p.init = SRM_INIT_MODEL ; p.init_model = &init ;
   }

   start_obj = (double *)calloc(nstart>0 ? nstart : 1,sizeof(double)) ;
   rc = SRM_fit_multistart(nview,T,nvox,x,&p,nstart,&m,&fi,start_obj,&best) ;
   if( rc != SRM_OK ){ fprintf(stderr,"SRM_fit: %s\n",SRM_strerror(rc)) ; return 1 ; }

   if( save_model != NULL ){
     SRM_provenance *pv=SRM_provenance_new() ; int j ; char lab[32] ;
     pv->axis=SRM_AXIS_TIME ; pv->zscore=SRM_ZSCORE_RUN ; pv->ntime_in=T ;
     pv->nseg = nseg>0 ? nseg : 1 ; pv->seglen=(int *)malloc(sizeof(int)*pv->nseg) ;
     for( j=0 ; j<pv->nseg ; j++ ) pv->seglen[j] = nseg>0 ? segbuf[j] : T ;
     pv->nsubj=m->nview ; pv->label=(char **)malloc(sizeof(char *)*m->nview) ;
     for( j=0 ; j<m->nview ; j++ ){ snprintf(lab,sizeof(lab),"s%02d",j) ; pv->label[j]=strdup(lab) ; }
     rc = SRM_save_model(save_model,m,pv) ;
     if( rc != SRM_OK ){ fprintf(stderr,"SRM_save_model: %s\n",SRM_strerror(rc)) ; return 1 ; }
     SRM_provenance_free(pv) ;
   }
   if( canon ){
     rc = SRM_canonicalize(m) ;
     if( rc != SRM_OK ){ fprintf(stderr,"SRM_canonicalize: %s\n",SRM_strerror(rc)) ; return 1 ; }
   }
   {
     int k=p.nfeat ;
     write_raw(out,"S.f32",m->s,sizeof(float),(size_t)k*T) ;
     write_raw(out,"asum.f64",m->a_sum,sizeof(double),(size_t)k*T) ;
     for( ii=0 ; ii<nview ; ii++ ){
       snprintf(name,sizeof(name),"W%d.f32",ii) ;
       write_raw(out,name,m->w[ii],sizeof(float),(size_t)nvox[ii]*k) ;
       if( m->wrun != NULL ){
         int rr ;
         for( rr=0 ; rr<m->nseg ; rr++ ){
           snprintf(name,sizeof(name),"Wr%d_%d.f32",ii,rr) ;
           write_raw(out,name,m->wrun[(size_t)ii*m->nseg+rr],sizeof(float),(size_t)nvox[ii]*k) ;
         }
       }
     }
     if( m->rho2    ) write_raw(out,"rho2.f64",m->rho2,sizeof(double),nview) ;
     if( m->sigma_s ) write_raw(out,"sigma.f32",m->sigma_s,sizeof(float),(size_t)k*k) ;
   }
   if( ntest > 0 ){
     int k=p.nfeat , i , *all=(int *)malloc(sizeof(int)*nview) , *others=(int *)malloc(sizeof(int)*nview) ;
     float **xt=(float **)malloc(sizeof(float *)*nview) , **xo=(float **)malloc(sizeof(float *)*nview) ;
     float *o=(float *)malloc(sizeof(float)*(size_t)k*T2) ; int no=0 ;
     if( ntest != nview || T2 < 1 ){ fprintf(stderr,"need --test-T and one --test-view per view\n") ; return 2 ; }
     for( i=0 ; i<nview ; i++ ){
       xt[i]=read_f32(tfile[i],(size_t)trows[i]*T2) ; all[i]=i ;
       rc = SRM_project(m,i,xt[i],T2,o) ;
       if( rc != SRM_OK ){ fprintf(stderr,"SRM_project: %s\n",SRM_strerror(rc)) ; return 1 ; }
       snprintf(name,sizeof(name),"proj%d.f32",i) ;
       write_raw(out,name,o,sizeof(float),(size_t)k*T2) ;
       if( i != loo ){ others[no]=i ; xo[no]=xt[i] ; no++ ; }
     }
     rc = SRM_shared_response(m,nview,all,xt,T2,1,o) ;
     if( rc != SRM_OK ){ fprintf(stderr,"shared response: %s\n",SRM_strerror(rc)) ; return 1 ; }
     write_raw(out,"shw.f32",o,sizeof(float),(size_t)k*T2) ;
     SRM_shared_response(m,nview,all,xt,T2,0,o) ;
     write_raw(out,"shu.f32",o,sizeof(float),(size_t)k*T2) ;
     if( loo >= 0 ){
       SRM_shared_response(m,no,others,xo,T2,1,o) ; write_raw(out,"looW.f32",o,sizeof(float),(size_t)k*T2) ;
       SRM_shared_response(m,no,others,xo,T2,0,o) ; write_raw(out,"looU.f32",o,sizeof(float),(size_t)k*T2) ;
     }
   }
   if( metrics_win > 0 || cmpS_file != NULL ){
     int k=p.nfeat ;
     if( cmpS_file != NULL ){
       float *s2=read_f32(cmpS_file,(size_t)k*T) , *cs=(float *)malloc(sizeof(float)*k) ; double mc ;
       char fn[4096] ; FILE *fp ; int c ;
       rc=SRM_shared_similarity(m->s,s2,k,T,&mc,cs) ;
       if( rc != SRM_OK ){ fprintf(stderr,"SRM_shared_similarity: %s\n",SRM_strerror(rc)) ; return 1 ; }
       snprintf(fn,sizeof(fn),"%s.similarity.txt",out) ; fp=fopen(fn,"w") ; fprintf(fp,"%.17g",mc) ;
       for( c=0 ; c<k ; c++ ) fprintf(fp," %.9g",cs[c]) ;
       fprintf(fp,"\n") ; fclose(fp) ;
     }
     if( metrics_win > 0 ){
       int i , j ; char fn[4096] ; FILE *fp ;
       if( ntest != nview || T2 < 1 ){ fprintf(stderr,"--metrics needs --test-T and one --test-view per view\n") ; return 2 ; }
       snprintf(fn,sizeof(fn),"%s.metrics.txt",out) ; fp=fopen(fn,"w") ;
       for( i=0 ; i<nview ; i++ ){
         int *oth=(int *)malloc(sizeof(int)*nview) , no=0 ; float **xo2=(float **)malloc(sizeof(float *)*nview) ;
         float *pi=(float *)malloc(sizeof(float)*(size_t)k*T2) , *ru=(float *)malloc(sizeof(float)*(size_t)k*T2) ;
         float *rw=(float *)malloc(sizeof(float)*(size_t)k*T2) , *xt=read_f32(tfile[i],(size_t)trows[i]*T2) ;
         double sa=0 , sc=0 , cc=0 , ia=0 , ic=0 , r2=0 ;
         for( j=0 ; j<nview ; j++ ) if( j != i ){ oth[no]=j ; xo2[no]=read_f32(tfile[j],(size_t)trows[j]*T2) ; no++ ; }
         rc = SRM_project(m,i,xt,T2,pi) ;
         if( rc == SRM_OK ) rc = SRM_shared_response(m,no,oth,xo2,T2,0,ru) ;
         if( rc == SRM_OK ) rc = SRM_shared_response(m,no,oth,xo2,T2,1,rw) ;
         if( rc == SRM_OK ) rc = SRM_segment_match(pi,ru,k,T2,metrics_win,&sa,&sc) ;
         if( rc == SRM_OK ) rc = SRM_component_corr(pi,ru,k,T2,NULL,&cc) ;
         if( rc == SRM_OK ) rc = SRM_item_identify(pi,ru,k,T2,&ia,&ic) ;
         if( rc == SRM_OK ) rc = SRM_reconstruction_r2(m,i,xt,rw,T2,&r2,NULL) ;
         if( rc != SRM_OK ){ fprintf(stderr,"metrics for view %d: %s\n",i,SRM_strerror(rc)) ; return 1 ; }
         fprintf(fp,"%.17g %.17g %.17g %.17g %.17g\n",sa,sc,cc,ia,r2) ;
         for( j=0 ; j<no ; j++ ) free(xo2[j]) ;
         free(oth) ; free(xo2) ; free(pi) ; free(ru) ; free(rw) ; free(xt) ;
       }
       fclose(fp) ;
     }
   }
   if( new_file != NULL ){
     int k=p.nfeat ; float *xn=read_f32(new_file,(size_t)new_nv*T) ;
     float *wn=(float *)malloc(sizeof(float)*(size_t)new_nv*k) ; double r2=0.0 ;
     rc = SRM_fit_view(m->s,k,T,xn,new_nv,p.center,wn,&r2,NULL) ;
     if( rc != SRM_OK ){ fprintf(stderr,"SRM_fit_view: %s\n",SRM_strerror(rc)) ; return 1 ; }
     write_raw(out,"newW.f32",wn,sizeof(float),(size_t)new_nv*k) ;
     write_raw(out,"newrho2.f64",&r2,sizeof(double),1) ;
     if( new_tfile != NULL ){
       float *xt=read_f32(new_tfile,(size_t)new_tnv*T2) , *o=(float *)malloc(sizeof(float)*(size_t)k*T2) ;
       rc = SRM_model_add_view(m,xn,new_nv) ;
       if( rc == SRM_OK ) rc = SRM_project(m,m->nview-1,xt,T2,o) ;
       if( rc != SRM_OK ){ fprintf(stderr,"new view: %s\n",SRM_strerror(rc)) ; return 1 ; }
       write_raw(out,"newproj.f32",o,sizeof(float),(size_t)k*T2) ;
     }
   }
   write_raw(out,"obj.f64",fi.objective,sizeof(double),(size_t)fi.niter_done+1) ;
   write_raw(out,"starts.f64",start_obj,sizeof(double),(size_t)nstart) ;
   /* written before any update, which changes rho_inv_sum */
   {
     char fn[4096] ; FILE *fp ;
     snprintf(fn,sizeof(fn),"%s.info.txt",out) ;
     fp = fopen(fn,"w") ;
     fprintf(fp,"%d %d %.17g %d\n",fi.niter_done,fi.converged,m->rho_inv_sum,best) ;
     fclose(fp) ;
   }
   if( nupd > 0 ){
     rc = run_updates(m,&p,nupd,upd_each,upd_rows,upd_file,T,out) ;
     if( rc != 0 ) return rc ;
   }
   SRM_free_fitinfo(&fi) ; SRM_free_model(m) ;
   return 0 ;
}
