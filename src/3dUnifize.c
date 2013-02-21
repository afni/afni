#include "mrilib.h"

static int verb = 1 ;

/*---------------------------------------------------------------------------*/
#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)
#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

static INLINE float median7(float *p)
{
    register float temp ;
    SORT2(p[0],p[1]) ; SORT2(p[4],p[5]) ; SORT2(p[1],p[2]) ;
    SORT2(p[5],p[6]) ; SORT2(p[0],p[1]) ; SORT2(p[4],p[5]) ;
    SORT2(p[0],p[4]) ; SORT2(p[2],p[6]) ; SORT2(p[1],p[3]) ;
    SORT2(p[3],p[5]) ; SORT2(p[1],p[3]) ; SORT2(p[2],p[3]) ;
    SORT2(p[3],p[4]) ; SORT2(p[2],p[3]) ; return(p[3]) ;
}

/*---------------------------------------------------------------------------*/

#undef  FSUB
#define FSUB(far,i,j,k,ni,nij) far[(i)+(j)*(ni)+(k)*(nij)]

MRI_IMAGE * mri_double_down( MRI_IMAGE *fim )
{
   MRI_IMAGE *gim=NULL ;
   float *far , *gar , par[7] ;
   int nxf,nyf,nzf , nxg,nyg,nzg , nxyf,nxyg , ii,jj,kk ;
   int iuu,iup,ium , juu,jum,jup , kuu,kum,kup ;

ENTRY("mri_double_down") ;

   if( fim == NULL ) RETURN(NULL) ;

   if( fim->kind != MRI_float ){
     MRI_IMAGE *qim = mri_to_float(fim) ;
     gim = mri_double_down(qim) ; mri_free(qim) ; RETURN(gim) ;
   }

   nxf = fim->nx ; nyf = fim->ny ; nzf = fim->nz ;
   nxg = nxf / 2 ; if( nxg < 1 ) nxg = 1 ;
   nyg = nyf / 2 ; if( nyg < 1 ) nyg = 1 ;
   nzg = nzf / 2 ; if( nzg < 1 ) nzg = 1 ;

   nxyf = nxf*nyf ; nxyg = nxg*nyg ;

   gim = mri_new_vol(nxg,nyg,nzg,MRI_float) ;
   gar = MRI_FLOAT_PTR(gim) ;
   far = MRI_FLOAT_PTR(fim) ;

   for( kk=0 ; kk < nzg ; kk++ ){
    kuu = 2*kk ; kum = kuu-1 ; if( kum <  0   ) kum = 0 ;
                 kup = kuu+1 ; if( kup >= nzf ) kup = nzf-1 ;
    for( jj=0 ; jj < nyg ; jj++ ){
      juu = 2*jj ; jum = juu-1 ; if( jum <  0   ) jum = 0 ;
                   jup = juu+1 ; if( jup >= nyf ) jup = nyf-1 ;
      for( ii=0 ; ii < nxg ; ii++ ){
        iuu = 2*ii ; ium = iuu-1 ; if( ium <  0   ) ium = 0 ;
                     iup = iuu+1 ; if( iup >= nxf ) iup = nxf-1 ;
        par[0] = FSUB(far,iuu,juu,kuu,nxf,nxyf) ;
        par[1] = FSUB(far,ium,juu,kuu,nxf,nxyf) ;
        par[2] = FSUB(far,iup,juu,kuu,nxf,nxyf) ;
        par[3] = FSUB(far,iuu,jum,kuu,nxf,nxyf) ;
        par[4] = FSUB(far,iuu,jup,kuu,nxf,nxyf) ;
        par[5] = FSUB(far,iuu,juu,kum,nxf,nxyf) ;
        par[6] = FSUB(far,iuu,juu,kup,nxf,nxyf) ;
        FSUB(gar,ii,jj,kk,nxg,nxyg) = median7(par) ;
   }}}

   RETURN(gim) ;
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_double_up( MRI_IMAGE *fim , int xadd,int yadd,int zadd )
{
   MRI_IMAGE *gim ;
   int nxf,nyf,nzf , nxg,nyg,nzg , nxyf,nxyg , ii,jj,kk , im,jm,km,ip,jp,kp ;
   float *far , *gar ;

ENTRY("mri_double_up") ;

   if( fim == NULL ) RETURN(NULL) ;

   if( fim->kind != MRI_float ){
     MRI_IMAGE *qim = mri_to_float(fim) ;
     gim = mri_double_up(qim,xadd,yadd,zadd) ; mri_free(qim) ; RETURN(gim) ;
   }

   nxf = fim->nx ; nyf = fim->ny ; nzf = fim->nz  ;

   nxg = (nxf == 1) ? 1 : (2*nxf+(xadd != 0)) ;
   nyg = (nyf == 1) ? 1 : (2*nyf+(yadd != 0)) ;
   nzg = (nzf == 1) ? 1 : (2*nzf+(zadd != 0)) ;

   nxyf = nxf*nyf ; nxyg = nxg*nyg ;

   gim = mri_new_vol(nxg,nyg,nzg,MRI_float) ;
   gar = MRI_FLOAT_PTR(gim) ;
   far = MRI_FLOAT_PTR(fim) ;

   for( kk=0 ; kk < nzg ; kk++ ){
    kp = km = kk/2 ; if( kk%2 ){ kp++; if( kp >= nzf ) kp = nzf-1; }
    for( jj=0 ; jj < nyg ; jj++ ){
      jp = jm = jj/2 ; if( jj%2 ){ jp++; if( jp >= nyf ) jp = nyf-1; }
      for( ii=0 ; ii < nxg ; ii++ ){
        ip = im = ii/2 ; if( ii%2 ){ ip++; if( ip >= nxf ) ip = nxf-1; }
        FSUB(gar,ii,jj,kk,nxg,nxyg) =
          0.125f * ( FSUB(far,im,jm,km,nxf,nxyf) + FSUB(far,ip,jm,km,nxf,nxyf)
                    +FSUB(far,im,jp,km,nxf,nxyf) + FSUB(far,ip,jp,km,nxf,nxyf)
                    +FSUB(far,im,jm,kp,nxf,nxyf) + FSUB(far,ip,jm,kp,nxf,nxyf)
                    +FSUB(far,im,jp,kp,nxf,nxyf) + FSUB(far,ip,jp,kp,nxf,nxyf) ) ;
   }}}

   RETURN(gim) ;
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_local_percentile( MRI_IMAGE *fim , float vrad , float perc )
{
   MRI_IMAGE *aim , *bim , *cim , *dim ;
   float     *aar , *bar , *car , *dar , *nbar ;
   byte      *ams , *bms ;
   MCW_cluster *nbhd ;
   int ii,jj,kk , nbar_num , nx,ny,nz,nxy ;
   float fq,val ; int qq,qp,qm ;

ENTRY("mri_local_percentile") ;

   if( fim == NULL || vrad < 4.0f || perc < 0.0f || perc > 100.0f ) RETURN(NULL) ;

   aim = mri_to_float(fim) ; aar = MRI_FLOAT_PTR(aim) ;
   ams = mri_automask_image(aim) ;
   if( ams == NULL ){ mri_free(aim) ; RETURN(NULL) ; }

   for( ii=0 ; ii < aim->nvox ; ii++ ) if( ams[ii] == 0 ) aar[ii] = 0.0f ;
   free(ams) ;

   bim = mri_double_down(aim) ; bar = MRI_FLOAT_PTR(bim) ; mri_free(aim) ;
   bms = (byte *)malloc(sizeof(byte)*bim->nvox) ;
   for( ii=0 ; ii < bim->nvox ; ii++ ) bms[ii] = (bar[ii] != 0.0f) ;

   nbhd = MCW_spheremask( 1.0f,1.0f,1.0f , 0.5f*vrad+0.001f ) ;
   nbar = (float *)malloc(sizeof(float)*nbhd->num_pt) ;

   cim = mri_new_conforming(bim,MRI_float) ; car = MRI_FLOAT_PTR(cim) ;
   SetSearchAboutMaskedVoxel(1) ;

   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ; nxy = nx*ny ;

   for( kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++ ){
         nbar_num = mri_get_nbhd_array( bim,bms , ii,jj,kk , nbhd,nbar ) ;
         if( nbar_num < 1 ){
           val = 0.0f ;
         } else {
           qsort_float(nbar_num,nbar) ;
           if( nbar_num == 1 || perc <= 0.000001f ){
             val = nbar[0] ;
           } else if( perc >= 99.9999f ){
             val = nbar[nbar_num-1] ;
           } else {
             fq = (0.01f*perc)*nbar_num ;
             qq = (int)fq ; qp = qq+1 ; if( qp == nbar_num ) qp = qq ;
                            qm = qq-1 ; if( qm <  0        ) qm = 0  ;
             val = 0.3333333f * ( nbar[qm] + nbar[qq] + nbar[qp] ) ;
           }
         }
         FSUB(car,ii,jj,kk,nx,nxy) = val ;
   }}}

   mri_free(bim) ; free(bms) ; free(nbar) ; KILL_CLUSTER(nbhd) ;

   dim = mri_double_up( cim , fim->nx%2 , fim->ny%2 , fim->nz%2 ) ;

   mri_free(cim) ;

   RETURN(dim) ;
}

/*---------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_local_percmean( MRI_IMAGE *fim , float vrad , float p1, float p2 )
{
   MRI_IMAGE *aim , *bim , *cim , *dim ;
   float     *aar , *bar , *car , *dar , *nbar ;
   byte      *ams , *bms ;
   MCW_cluster *nbhd ;
   int ii,jj,kk , nbar_num , nx,ny,nz,nxy , vstep,vvv ;
   float val ;

ENTRY("mri_local_percmean") ;

   if( p1 > p2 ){ val = p1; p1 = p2; p2 = val; }

   if( fim == NULL || vrad < 4.0f || p1 < 0.0f || p2 > 100.0f ) RETURN(NULL) ;

   if( p1 == p2 ) RETURN( mri_local_percentile(fim,vrad,p1) ) ;

   if( verb ) fprintf(stderr,"A") ;

   aim = mri_to_float(fim) ; aar = MRI_FLOAT_PTR(aim) ;
   ams = mri_automask_image(aim) ;
   if( ams == NULL ){ mri_free(aim) ; RETURN(NULL) ; }

   if( verb ) fprintf(stderr,"B") ;

   for( ii=0 ; ii < aim->nvox ; ii++ ) if( ams[ii] == 0 ) aar[ii] = 0.0f ;
   free(ams) ;

   bim = mri_double_down(aim) ; bar = MRI_FLOAT_PTR(bim) ; mri_free(aim) ;
   bms = (byte *)malloc(sizeof(byte)*bim->nvox) ;
   for( ii=0 ; ii < bim->nvox ; ii++ ) bms[ii] = (bar[ii] != 0.0f) ;

   nbhd = MCW_spheremask( 1.0f,1.0f,1.0f , 0.5f*vrad+0.001f ) ;
   nbar = (float *)malloc(sizeof(float)*nbhd->num_pt) ;

   cim = mri_new_conforming(bim,MRI_float) ; car = MRI_FLOAT_PTR(cim) ;
   SetSearchAboutMaskedVoxel(1) ;

   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ; nxy = nx*ny ;

   vstep = (nxy*nz)/50 ; if( !verb || vstep < 10 ) vstep = 0 ;

   if( verb ) fprintf(stderr,"\n") ;

   if( vstep ) fprintf(stderr," + Voxel loop: ") ;

   for( vvv=kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++,vvv++ ){
         if( vstep && vvv%vstep == vstep-1 ) vstep_print() ;
         nbar_num = mri_get_nbhd_array( bim,bms , ii,jj,kk , nbhd,nbar ) ;
         if( nbar_num < 1 ){
           val = 0.0f ;
         } else {
           qsort_float(nbar_num,nbar) ;
           if( nbar_num == 1 ){
             val = nbar[0] ;
           } else {
             int q1,q2,qq ;
             q1 = (int)( 0.01f*p1*(nbar_num-1) ) ;
             q2 = (int)( 0.01f*p2*(nbar_num-1) ) ;
             for( qq=q1,val=0.0f ; qq <= q2 ; qq++ ) val += nbar[qq] ;
             val /= (q2-q1+1.0f) ;
           }
         }
         FSUB(car,ii,jj,kk,nx,nxy) = val ;
   }}}

   if( vstep ) fprintf(stderr,"*") ;

   mri_free(bim) ; free(bms) ; free(nbar) ; KILL_CLUSTER(nbhd) ;

   dim = mri_double_up( cim , fim->nx%2 , fim->ny%2 , fim->nz%2 ) ;

   if( verb ) fprintf(stderr,"C") ;

   mri_free(cim) ;

   RETURN(dim) ;
}

/*---------------------------------------------------------------------------*/

static float Upbot = 70.0f ;
static float Uptop = 80.0f ;
static float Uprad = 17.0f ;

MRI_IMAGE * mri_unifize( MRI_IMAGE *fim )
{
   MRI_IMAGE *pim, *gim ; float *par,*gar , pval ; int ii ;

ENTRY("mri_unifize") ;

   if( fim == NULL ) RETURN(NULL) ;

   pim = mri_local_percmean( fim , Uprad , Upbot,Uptop ) ;
   if( pim == NULL ) RETURN(NULL) ;
   gim = mri_to_float(fim) ;
   par = MRI_FLOAT_PTR(pim) ; gar = MRI_FLOAT_PTR(gim) ;

   for( ii=0 ; ii < gim->nvox ; ii++ ){
     pval = par[ii] ;
     gar[ii] = ( pval <= 0.0f ) ? 0.0f : 1000.0f * gar[ii] / pval ;
   }

   if( verb ) fprintf(stderr,"D\n") ;

   mri_free(pim) ;
   RETURN(gim) ;
}

/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg , ct ;
   char *prefix = "Unifized" ;
   THD_3dim_dataset *inset=NULL , *outset ;
   MRI_IMAGE *imin , *imout ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dUnifize [options] inputdataset\n"
            "* The input dataset is supposed to be a T1-weighted volume,\n"
            "  preferably already skull-stripped (via 3dSkullStrip).\n"
            "* The output dataset has the white matter intensity approximately\n"
            "  uniformized across space.\n"
            "* The output dataset is always stored in float format.\n"
            "* If the input dataset has more than 1 sub-brick, only sub-brick\n"
            "  #0 will be processed.\n"
            "* Method: my personal variant of Ziad's sneaky trick.\n"
            "  (If you want to know what his trick is, you'll have to ask him,\n"
            "  or read my source code, which of course is a world of fun and joy.)\n"
            "* The principal motive for this program is for use in an image\n"
            "  registration script, and it may or may not be useful otherwise.\n"
            "\n"
            "Options:\n"
            "  -prefix pp = Use 'pp' for prefix of output dataset\n"
            "  -input dd  = Alternative way to specify input dataset\n"
            "\n"
            "-- Feb 2013 - RWCox\n"
           ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dUnifize main"); machdep(); AFNI_logger("3dUnifize",argc,argv);
   PRINT_VERSION("3dUnifize") ;

   /*-- scan command line --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("Illegal value after -prefix!") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-input") == 0 || strcmp(argv[iarg],"-inset") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       if( inset  != NULL ) ERROR_exit("Can't use '%s' twice"    ,argv[iarg-1]) ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-param") == 0 ||      /* HIDDEN OPTION */
         strcmp(argv[iarg],"-rbt"  ) == 0    ){
       if( ++iarg >= argc-2 ) ERROR_exit("Need 3 arguments (R pb pt) after '%s'",argv[iarg-1]) ;
       Uprad = (float)strtod(argv[iarg++],NULL) ;
       Upbot = (float)strtod(argv[iarg++],NULL) ;
       Uptop = (float)strtod(argv[iarg++],NULL) ;
       if( Uprad <   5.0f || Uprad > 40.0f ||
           Upbot <  40.0f || Upbot > 80.0f ||
           Uptop <= Upbot || Uptop > 90.0f   )
         ERROR_exit("Illegal values (R pb pt) after '%s'",argv[iarg-4]) ;
       continue ;
     }

     ERROR_exit("Unknown option: %s\n",argv[iarg]);
   }

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No dataset name on command line?\n") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }

   ct = NI_clock_time() ;

   if( verb ) fprintf(stderr," + Pre-processing: ") ;

   DSET_load( inset ) ; CHECK_LOAD_ERROR(inset) ;
   if( DSET_NVALS(inset) > 1 )
     WARNING_message("Only processing sub-brick #0") ;

   imin = mri_to_float( DSET_BRICK(inset,0) ) ; DSET_unload(inset) ;
   if( imin == NULL ) ERROR_exit("Can't copy input dataset brick") ;

   imout = mri_unifize(imin) ; free(imin) ;

   if( imout == NULL )
     ERROR_exit("Can't compute Unifize-d dataset for some reason :-(") ;

   outset = EDIT_empty_copy( inset )  ;
   EDIT_dset_items( outset ,
                       ADN_prefix , prefix ,
                       ADN_nvals  , 1 ,
                       ADN_ntt    , 0 ,
                    ADN_none ) ;
   EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(imout) ) ;
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dUnifize" , argc,argv , outset ) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   INFO_message("===== clock time =%s",nice_time_string(NI_clock_time()-ct)) ;
   exit(0) ;
}
