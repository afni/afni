#include "mrilib.h"

static int verb = 0 ;

/*----------------------------------------------------------------------------*/
/* Setup to solve a collection of equations of the form

   [z] = [X] [beta] + gamma [b] + delta [c]

   where [z] = data = N vector
         [X] = fixed N x (M-1) matrix } [X] and [b] together make
         [b] = fixed N vector         } up the N x M matrix [A]
         [c] = variable N vector
      [beta] = (M-1) vector = fit coefficients of columns of [X]
       gamma = scalar fit coefficient of [b]
       delta = scalar fit coefficient of [c]
   The LSS goal is to find the value of gamma+delta for a bunch of
   different [c] vectors.  To that end, this function returns an
   N vector [s] for each input [c], such that gamma+delta = [s] *dot* [z].
   All the other stuff that COULD be estimated, such as [beta], is ignored
   for the sake of efficiency.

   If NULL is returned, the inputs are illegal.  In particular, the nx element
   (column length) of the 2 input images must be the same, or you will be
   chastised and anathematised in public.
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * LSS_setup( MRI_IMAGE *ima , MRI_IMAGE *imc )
{
   int nn, mm, nc, ii, jj, ic ;
   float *cc, *pp, *qq, *qj, *vv, *ss, *pv , cj, cvdot, pc ;
   MRI_IMAGE *ims , *imp , *imq ;
   MRI_IMARR *imar ;

ENTRY("LSS_setup") ;

   if( ima == NULL || imc == NULL ){  /* bad user */
     ERROR_message("LSS_setup: NULL input image?!") ;
     RETURN(NULL) ;
   }

   /* [A] is nn X mm ; [C] is nn x nc */

   nn = ima->nx ; mm = ima->ny ; nc = imc->ny ; cc = MRI_FLOAT_PTR(imc) ;

   if( imc->nx != nn || nn <= mm+2 ){  /* stoopid user */
     ERROR_message("LSS_setup: ima->nx=%d does not equal imc->nx=%d :-(" ,
                   ima->nx,imc->nx) ;
     RETURN(NULL) ;
   }

   /* get imp = [P] = psinv of [A] = mm X nn matrix
          imq = [Q] = ortproj onto column null space of [A] = nn X nn matrix */

   mri_matrix_psinv_svd(1) ;
   imar = mri_matrix_psinv_ortproj( ima , 1 ) ;

   if( imar == NULL ){  /* should not happen */
     ERROR_message("LSS_setup: cannot compute pseudo-inverse :-(") ;
     RETURN(NULL) ;
   }

   imp = IMARR_SUBIM(imar,0) ; pp = MRI_FLOAT_PTR(imp) ;
   imq = IMARR_SUBIM(imar,1) ; qq = MRI_FLOAT_PTR(imq) ;

   /* create output image = [S] = nn X nc
      Each column of [S] is the vector that we
      dot into a data vector [z] to get the estimate of
      gamma+delta for the corresponding column from [C] */

   ims = mri_new(nn,nc,MRI_float) ; ss = MRI_FLOAT_PTR(ims) ;

   /* workspace vectors */

   vv = (float *)malloc(sizeof(float)*nn) ;  /* will be [Q] [c] */

   pv = (float *)malloc(sizeof(float)*nn) ;  /* last row of [P] */
   for( ii=0 ; ii < nn ; ii++ ) pv[ii] = pp[ mm-1 + ii*mm ] ;

   /* loop over columns of [C] (and [S]) */

   for( ic=0 ; ic < nc ; ic++,cc+=nn,ss+=nn ){

     /* compute [v] = [Q] [c] */

     for( ii=0 ; ii < nn ; ii++ ) vv[ii] = 0.0f ;     /* initialize [v] to 0 */
     for( jj=0 ; jj < nn ; jj++ ){                 /* loop over columns of Q */
       qj = qq + jj*nn ;                         /* ptr to jj-th column of Q */
       cj = cc[jj] ;                                   /* jj-th value of [c] */
       for( ii=0 ; ii < nn ; ii++ ) vv[ii] += qj[ii] * cj ;  /* sum into [v] */
     }

     /* compute cvdot = [c] *dot* [v]
                cj    = [c] *dot* [c]
                pc    = [c] *dot* {last row of [P] = pv} */

     for( pc=cj=cvdot=ii=0 ; ii < nn ; ii++ ){
       cvdot += cc[ii]*vv[ii] ; cj += cc[ii]*cc[ii] ; pc += pv[ii]*cc[ii] ;
     }

     /* initialize [s] = last row of [P] */

     for( ii=0 ; ii < nn ; ii++ ) ss[ii] = pv[ii] ;

     /* If cvdot is zero(ish), this means that the extra column [c]
        is collinear(ish) with the columns of [A], and we skip the next step.
        Note that since [Q] is an orthogonal matrix,
        we are guaranteed that L2norm([Q][c]) == L2norm([c]),
        which implies that abs(cvdot) <= abs(cj), by the triangle inequality. */

     if( fabsf(cvdot) >= 1.e-5*cj ){

       /* add the proper fraction of [v] into [s] */

       pc = (1.0f - pc) / cvdot ;
       for( ii=0 ; ii < nn ; ii++ ) ss[ii] += pc * vv[ii] ;
     }

   } /* end of loop over columns of [C] */

   /* toss the trash and return the output set of columns */

   free(pv) ; free(vv) ; DESTROY_IMARR(imar) ; RETURN(ims) ;
}

/*----------------------------------------------------------------------------*/
/* Create 2 new matrix images.
   (0) One where columns jbot..jtop are excised, and the last column is
       the sum of those columns.
   (1) The matrix of the excised columns.
*//*--------------------------------------------------------------------------*/

MRI_IMARR * LSS_mangle_matrix( MRI_IMAGE *ima, int jbot, int jtop )
{
   int ii , jj , jnew , jn , njj , nn,mm ;
   MRI_IMAGE *imb , *imc ; MRI_IMARR *imar ;
   float *aa , *bb , *cc , *acol , *bcol , *ccol ;

ENTRY("LSS_mangle_matrix") ;

   if( ima == NULL || ima->kind != MRI_float ) RETURN(NULL) ;
   nn = ima->nx ; mm = ima->ny ;
   njj = jtop-jbot+1 ; if( njj <= 1 )          RETURN(NULL) ;
   if( jbot < 0 || jtop >= mm )                RETURN(NULL) ;

   imb = mri_new( nn , mm-njj+1 , MRI_float ) ;  /* matrix without jbot..jtop */
   imc = mri_new( nn , njj      , MRI_float ) ;  /* matrix with jbot..jtop */
   aa  = MRI_FLOAT_PTR(ima) ;
   bb  = MRI_FLOAT_PTR(imb) ;
   cc  = MRI_FLOAT_PTR(imc) ;

   /* copy non-excised columns into the new imb */

   for( jn=jj=0 ; jj < mm ; jj++ ){
     if( jj >= jbot && jj <= jtop ) continue ;
     acol = aa + jj*nn ;        /* jj = old column index */
     bcol = bb + jn*nn ; jn++ ; /* jn = new column index */
     for( ii=0 ; ii < nn ; ii++ ) bcol[ii] = acol[ii] ;
   }

   /* copy excised columns into the new imc,
      and also add them up into the last column of imb */

   bcol = bb + (mm-njj)*nn ;  /* last col of imb */
   for( jn=0,jj=jbot ; jj <= jtop ; jj++ ){
     acol = aa + jj*nn ;
     ccol = cc + jn*nn ; jn++ ;
     for( ii=0 ; ii < nn ; ii++ ){
       ccol[ii] = acol[ii] ; bcol[ii] += acol[ii] ;
     }
   }

   INIT_IMARR(imar) ; ADDTO_IMARR(imar,imb) ; ADDTO_IMARR(imar,imc) ; RETURN(imar) ;
}

/*----------------------------------------------------------------------------*/

void LSS_help(void)
{
   printf(
     "Usage: 3dLSS [options]\n"
     "\n"
     "  ** Least-Squares-Sum (LSS) estimation from a -stim_times_IM matrix,       **\n"
     "  ** as described in the paper:                                             **\n"
     "  **   JA Mumford et al.  Deconvolving BOLD activation in event-related     **\n"
     "  **   designs for multivoxel pattern classification analyses.              **\n"
     "  **   NeuroImage (2011) http://dx.doi.org/10.1016/j.neuroimage.2011.08.076 **\n"
     "\n"
     "-------------------------------------\n"
     "Options (the first two are mandatory)\n"
     "-------------------------------------\n"
     " -input ddd  = Read time series dataset 'ddd'\n"
     "\n"
     " -matrix mmm = Read the matrix 'mmm', which should have been\n"
     "                output from 3dDeconvolve via the '-x1D' option, and\n"
     "                should have included exactly one '-stim_times_IM' option.\n"
     "\n"
     " -mask kkk   = Read dataset 'kkk' as a mask for the input; voxels outside\n"
     "                the mask will not be fit by the regression model.\n"
     " -automask   = If you don't know what this does by now, please don't use\n"
     "                this program.\n"
     "\n"
     " -prefix ppp = Prefix name for the output dataset;\n"
     "                this dataset will contain ONLY the LSS estimates of the\n"
     "                beta weights for the '-stim_times_IM' stimuli.\n"
     "------\n"
     "Method\n"
     "------\n"
     " 3dLSS is fast since it uses a rank-1 bordering technique to pre-compute\n"
     " the estimator for each separate stimulus regressor from the fixed part of\n"
     " the matrix, then applies these estimators to each time series in the input\n"
     " dataset by a simple dot product.\n"
     "          ** Compute fast, exit early, leave a pretty dataset **\n"
     "\n"
     "-- RWCox - Dec 2011\n"
   ) ;
   PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg , nerr=0 , nvals,nvox , nx,ny,nz , ii,jj,kk ;
   char *prefix = "LSSout" , nbuf[256] ;
   THD_3dim_dataset *inset=NULL , *outset ;
   MRI_vectim   *inset_mrv=NULL ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0, automask=0, nmask=0 ;
   NI_element *nelmat=NULL ; char *matname=NULL ;
   char *cgl ;
   int Ngoodlist,*goodlist=NULL , nfull , ncmat,ntime ;
   NI_int_array *giar ; NI_str_array *gsar ; NI_float_array *gfar ;
   MRI_IMAGE *imX, *imA, *imC, *imS ; float *Xar, *Sar ; MRI_IMARR *imar ;
   int nS ; float *ss , *oo , *fv , sum ; int nvec , iv ;
   int nbstim , nst=0 , jst_bot,jst_top ; char *stlab="LSS" ;

   /*--- help me if you can ---*/

   if( argc < 2 || strcasecmp(argv[1],"-HELP") == 0 ) LSS_help() ;

   /*--- bureaucratic startup ---*/

   PRINT_VERSION("3dLSS"); mainENTRY("3dLSS main"); machdep();
   AFNI_logger("3dLSS",argc,argv); AUTHOR("RWCox");
   (void)COX_clock_time() ;

   /**------- scan command line --------**/

   iarg = 1 ;
   while( iarg < argc ){

     if( strcmp(argv[iarg],"-verb") == 0 ){ verb++  ; iarg++ ; continue ; }
     if( strcmp(argv[iarg],"-VERB") == 0 ){ verb+=2 ; iarg++ ; continue ; }

     /**==========   -mask  ==========**/

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%.33s'",argv[iarg]) ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       if( verb || nmask < 1 ) INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < 1 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ; iarg++ ; continue ;
     }

     /**==========   -matrix  ==========**/

     if( strcasecmp(argv[iarg],"-matrix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       if( nelmat != NULL ) ERROR_exit("More than 1 -matrix option!?");
       nelmat = NI_read_element_fromfile( argv[iarg] ) ; /* read NIML file */
       matname = argv[iarg];
       if( nelmat == NULL || nelmat->type != NI_ELEMENT_TYPE )
         ERROR_exit("Can't process -matrix file '%s'!?",matname) ;
       iarg++ ; continue ;
     }

     /**==========   -input  ==========**/

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options!?") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /**==========   -prefix  =========**/

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       prefix = strdup(argv[iarg]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Illegal string after %s",argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     /***** Loser User *****/

      ERROR_message("Unknown option: %s",argv[iarg]) ;
      suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);

   }  /* end of loop over options */

   /*----- check for errors -----*/

   if( inset  == NULL ){ ERROR_message("No -input dataset?!") ; nerr++ ; }
   if( nelmat == NULL ){ ERROR_message("No -matrix option!?") ; nerr++ ; }
   if( nerr > 0 ) ERROR_exit("Can't continue without these inputs!") ;

   nvals = DSET_NVALS(inset) ; nvox = DSET_NVOX(inset) ;
   nx = DSET_NX(inset) ; ny = DSET_NY(inset) ; nz = DSET_NZ(inset) ;

   /*----- masque -----*/

   if( mask != NULL ){     /* check -mask option for compatibility */
     if( mask_nx != nx || mask_ny != ny || mask_nz != nz )
       ERROR_exit("-mask dataset grid doesn't match input dataset :-(") ;

   } else if( automask ){  /* create a mask from input dataset */
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset :-(") ;
     nmask = THD_countmask( nvox , mask ) ;
     if( verb || nmask < 1 )
       INFO_message("Number of voxels in automask = %d (out of %d = %.1f%%)",
                    nmask, nvox, (100.0f*nmask)/nvox ) ;
     if( nmask < 1 ) ERROR_exit("Automask is too small to process") ;

   } else {                /* create a 'mask' for all voxels */
     if( verb )
       INFO_message("No mask ==> computing for all %d voxels",nvox) ;
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset( mask , 1 , sizeof(byte)*nvox ) ;

   }

   /*----- get matrix info from the NIML element -----*/

   if( verb ) INFO_message("extracting matrix info") ;

   ncmat = nelmat->vec_num ;  /* number of columns */
   ntime = nelmat->vec_len ;  /* number of rows */
   if( ntime < ncmat+2 )
     ERROR_exit("Matrix has too many columns (%d) for number of rows (%d)",ncmat,ntime) ;

   /*--- number of rows in the full matrix (without censoring) ---*/

   cgl = NI_get_attribute( nelmat , "NRowFull" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'NRowFull' attribute!") ;
   nfull = (int)strtod(cgl,NULL) ;
   if( nvals != nfull )
     ERROR_exit("-input dataset has %d time points, but matrix indicates %d",
                nvals , nfull ) ;

   /*--- the goodlist = mapping from matrix row index to time index
                        (which allows for possible time point censoring) ---*/

   cgl = NI_get_attribute( nelmat , "GoodList" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'GoodList' attribute!") ;
   giar = NI_decode_int_list( cgl , ";," ) ;
   if( giar == NULL || giar->num < ntime )
     ERROR_exit("Matrix 'GoodList' badly formatted?!") ;
   Ngoodlist = giar->num ; goodlist = giar->ar ;
   if( Ngoodlist != ntime )
     ERROR_exit("Matrix 'GoodList' incorrect length?!") ;

   /*--- extract the matrix from the NIML element ---*/

   imX = mri_new( ntime , ncmat , MRI_float ) ;
   Xar = MRI_FLOAT_PTR(imX) ;

   if( nelmat->vec_typ[0] == NI_FLOAT ){  /* from 3dDeconvolve_f */
     float *cd ;
     for( jj=0 ; jj < ncmat ; jj++ ){
       cd = (float *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) Xar[ii+jj*ntime] = cd[ii] ;
     }
   } else if( nelmat->vec_typ[0] == NI_DOUBLE ){  /* from 3dDeconvolve */
     double *cd ;
     for( jj=0 ; jj < ncmat ; jj++ ){
       cd = (double *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) Xar[ii+jj*ntime] = cd[ii] ;
     }
   } else {
     ERROR_exit("Matrix file stored with illegal data type!?") ;
   }

   /*--- find the stim_times_IM option ---*/

   cgl = NI_get_attribute( nelmat , "BasisNstim") ;
   if( cgl == NULL ) ERROR_exit("Matrix doesn't have 'BasisNstim' attribute!") ;
   nbstim = (int)strtod(cgl,NULL) ;
   if( nbstim <= 0 ) ERROR_exit("Matrix 'BasisNstim' attribute is %d",nbstim) ;
   for( jj=1 ; jj <= nbstim ; jj++ ){
     sprintf(nbuf,"BasisOption_%06d",jj) ;
     cgl = NI_get_attribute( nelmat , nbuf ) ;
     if( cgl == NULL || strcmp(cgl,"-stim_times_IM") != 0 ) continue ;
     if( nst > 0 )
       ERROR_exit("More than one -stim_time_IM option was found in the matrix") ;
     nst = jj ;
     sprintf(nbuf,"BasisColumns_%06d",jj) ;
     cgl = NI_get_attribute( nelmat , nbuf ) ;
     if( cgl == NULL )
       ERROR_exit("Matrix doesn't have %s attribute!",nbuf) ;
     jst_bot = jst_top = -1 ;
     sscanf(cgl,"%d:%d",&jst_bot,&jst_top) ;
     if( jst_bot < 0 || jst_top < 0 )
       ERROR_exit("Can't decode matrix attribute %s",nbuf) ;
     if( jst_bot >= jst_top || jst_top >= ncmat )
       ERROR_exit("Matrix attribute %s has illegal value",nbuf) ;
     sprintf(nbuf,"BasisName_%06d",jj) ;
     cgl = NI_get_attribute( nelmat , nbuf ) ;
     if( cgl != NULL ) stlab = strdup(cgl) ;
     if( verb > 1 )
       ININFO_message("-stim_times_IM at stim #%d; cols %d..%d",jj,jst_bot,jst_top) ;
   }
   if( nst == 0 )
     ERROR_exit("Matrix doesn't have any -stim_times_IM options inside :-(") ;

   /*--- mangle matrix to segregate IM regressors from the rest ---*/

   if( verb ) INFO_message("setting up LSS vectors") ;

   imar = LSS_mangle_matrix( imX , jst_bot , jst_top ) ;
   if( imar == NULL )
     ERROR_exit("Can't compute LSS 'mangled' matrix :-(") ;

   /*--- setup for LSS computations ---*/

   imA = IMARR_SUBIM(imar,0) ;
   imC = IMARR_SUBIM(imar,1) ;
   imS = LSS_setup( imA , imC ) ; DESTROY_IMARR(imar) ;
   if( imS == NULL )
     ERROR_exit("Can't complete LSS setup :-((") ;
   nS = imS->ny ; Sar = MRI_FLOAT_PTR(imS) ;

   /*----- create output dataset -----*/

   if( verb ) INFO_message("creating output datset in memory") ;

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix    ,
                      ADN_datum_all , MRI_float ,
                      ADN_brick_fac , NULL      ,
                      ADN_nvals     , nS        ,
                      ADN_ntt       , nS        ,
                    ADN_none ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLSS" , argc,argv , outset ) ;
   for( kk=0 ; kk < nS ; kk++ ){
     EDIT_substitute_brick( outset , kk , MRI_float , NULL ) ;
     sprintf(nbuf,"%s#%03d",stlab,kk) ;
     EDIT_BRICK_LABEL( outset , kk , nbuf ) ;
   }

   /*----- convert input dataset to vectim -----*/

   if( verb ) INFO_message("loading input dataset into memory") ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   inset_mrv = THD_dset_to_vectim( inset , mask , 0 ) ;
   DSET_unload(inset) ;

   /*----- compute dot products, store results -----*/

   if( verb ) INFO_message("computing away, me buckos!") ;

   nvec = inset_mrv->nvec ;
   for( kk=0 ; kk < nS ; kk++ ){
     ss = Sar + kk*ntime ;
     oo = DSET_ARRAY(outset,kk) ;
     for( iv=0 ; iv < nvec ; iv++ ){
       fv = VECTIM_PTR(inset_mrv,iv) ;
       for( sum=0.0f,ii=0 ; ii < ntime ; ii++ )
         sum += ss[ii] * fv[goodlist[ii]] ;
       oo[inset_mrv->ivec[iv]] = sum ;
     }
   }

   DSET_write(outset) ; WROTE_DSET(outset) ;

   /*-------- Hasta la vista, baby --------*/

   if( verb )
     INFO_message("3dLSS finished: total CPU=%.2f Elapsed=%.2f",
                  COX_cpu_time() , COX_clock_time() ) ;
   exit(0) ;
}
