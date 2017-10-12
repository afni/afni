/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

void find_base_value( int nxyz , short * sfim , int * base , int * peak ) ;
void remove_isolated_stuff( THD_3dim_dataset * qset ) ;
void xyz_to_ijk( THD_3dim_dataset * ds , float x , float y , float z ,
                                         int * i , int * j , int * k  ) ;
void DRAW_2dfiller( int nx , int ny , int ix , int jy , byte * ar ) ;

/*---------------------------------------------------------------------------*/

#define DD(i,j,k) dar[(i)+(j)*nx+(k)*nxy]
#define QQ(i,j,k) qar[(i)+(j)*nx+(k)*nxy]
#define BB(i,j)   bar[(i)+(j)*nx]

static int verbose = 0 ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset , * qset ;
   short * dar , * qar ;

   char prefix[128] = "strip" ;
   int base = 0 , hyval = 0 , hyper = 0 ;
   double metric_thresh = 1.05 , area_thresh = 9999.0 ;

   int iarg , nx,ny,nz,nxy,nxyz , dx,dy,dz , ii,jj,kk,  ic,jc,kc , ktop , cc,pp ;
   byte * bar ;
   double qsum , sum , rr , metric,area ;
   int   nsum , didit ;

   /* help? */

WARNING_message("Do not use this program (3dstrip)! It is old and unmaintained.") ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
             "Usage: 3dstrip [options] dataset\n"
             "Attempts to strip the scalp tissue from the input anatomical dataset.\n"
             "This dataset must be in AC-PC or Talairach coordinates, must be stored\n"
             "as shorts, and must have cubical voxels.\n"
             "\n"
             "Options:\n"
             "  -base   bb = Set all voxels at or below value 'bb' to zero.\n"
             "                 [default = computed from the data histogram]\n"
             "  -metric mm = Set the metric threshold to 'mm'; the metric is\n"
             "                 used to determine the annularity of the removed\n"
             "                 tissue in each axial slice.\n"
             "                 [default = 1.05 (unitless); must be > 1.0]\n"
             "  -area   aa = Set the area threshold to 'aa' square millimeters;\n"
             "                 if the amount of removed tissue would be larger \n"
             "                 than 'aa', or the metric is larger than 'mm',\n"
             "                 then nothing will be removed in or below the slice.\n"
             "                 [default = 9999]\n"
             "  -hyper     = Flags the elimination of hyperintensity voxels after\n"
             "                 the stripping procedure.\n"
             "                 [default = do not remove these voxels]\n"
             "  -hyclip    = Flags the setting of hyperintensity voxels back to\n"
             "                 the hyperintensity threshold (rather than to zero).\n"
             "  -hyval  hh = Set the hyperintensity threshold to 'hh'; all voxels\n"
             "                 with intensity 'hh' or above will be set to zero\n"
             "                 (for -hyper) or set to this value (for -hyclip).\n"
             "                 [default = computed from the data histogram]\n"
             "  -prefix pp = Use 'pp' for the prefix of the output dataset.\n"
             "                 [default = 'strip']\n"
             "  -verbose   = Print out progress reports, including the area\n"
             "                 and metric for each axial slice\n"
             "\n"
             "The metric is average(r**2)/average(r)**2 for all candidate removal\n"
             "voxels, where r is measured from the center of the slice.  This ratio\n"
             "is 1 for a circle, and will be nearly 1 for a thin annular region.\n"
             "The default threshold of 1.05 is chosen from experience.  The default\n"
             "area threshold is also chosen from experience.  The goal of the thresholds\n"
             "is to avoid removing brain tissue.\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* arguments */

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-verbose",5) == 0 ){
         verbose = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-metric") == 0 ){
         metric_thresh = strtod( argv[++iarg] , NULL ) ;
         if( metric_thresh <= 1.0 ){fprintf(stderr,"** Illegal value after -metric\n");exit(1);}
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-area") == 0 ){
         area_thresh = strtod( argv[++iarg] , NULL ) ;
         if( area_thresh <= 1.0 ){fprintf(stderr,"** Illegal value after -area\n");exit(1);}
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-base") == 0 ){
         base = strtol( argv[++iarg] , NULL , 10 ) ;
         if( base <= 0 ){fprintf(stderr,"** Illegal value after -base\n");exit(1);}
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         strcpy( prefix , argv[++iarg] ) ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-hyper") == 0 ){
         hyper = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-hyclip") == 0 ){
         hyper = 2 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-hyval") == 0 ){
         hyval = strtol( argv[++iarg] , NULL , 10 ) ;
         if( hyval <= 0 ){fprintf(stderr,"** Illegal value after -hyval\n");exit(1);}
         iarg++ ; continue ;
      }

      fprintf(stderr,"** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /* open dataset */

   if( iarg >= argc ){fprintf(stderr,"** No dataset name on command line?\n");exit(1);}

   dset = THD_open_dataset( argv[iarg] ) ;
   if( dset == NULL ){fprintf(stderr,"** Can't open dataset %s\n",argv[iarg]);exit(1);}

   dx = DSET_DX(dset) ; dy = DSET_DY(dset) ; dz = DSET_DZ(dset) ;
   nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nz = DSET_NZ(dset) ;

   nxy = nx*ny ; nxyz = nx*ny*nz ;

   if( dx <= 0.0 || dx != dy || dx != dz ){
      fprintf(stderr,"** Dataset voxel shape is illegal!\n") ; exit(1) ;
   }

   if( dset->view_type == VIEW_ORIGINAL_TYPE ){
      fprintf(stderr,"** Dataset is in the +orig view, which is illegal!\n") ; exit(1) ;
   }

   DSET_mallocize(dset) ; DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   dar = DSET_ARRAY(dset,0) ;

   /* find base value, if needed */

   if( base <= 0 || (hyval <= 0 && hyper) ){
      int bb , hh ;
      find_base_value( nxyz , dar , &bb , &hh ) ;
      if( base <= 0 ){
         base = bb ;
         if( verbose ) printf("-- Base value set to %d\n",base) ;
      }
      if( hyval <= 0 && hyper && hh > base+1 ){
         hyval = hh ;
         if( verbose ) printf("-- Hyperintensity value set to %d\n",hyval) ;
      }
   }

   /* make new dataset */

   qset = EDIT_empty_copy( dset ) ;
   EDIT_dset_items( qset,
                       ADN_prefix    , prefix ,
                       ADN_nvals     , 1 ,
                       ADN_ntt       , 0 ,
                    ADN_none ) ;

   /* copy dar to qar, omiting values <= base */

   qar = (short *) malloc( sizeof(short) * nxyz ) ; /* another array */
   if( qar == NULL ){fprintf(stderr,"** Can't malloc workspace!\n");exit(1);}

   EDIT_substitute_brick( qset , 0 , MRI_short , qar ) ;

   for( ii=pp=0 ; ii < nxyz ; ii++ ){
      if( dar[ii] > base ){
         qar[ii] = dar[ii] ;
      } else {
         qar[ii] = 0 ; pp++ ;
      }
   }
   if( verbose ) printf("-- Blasted %d voxels at or below base\n",pp) ;

   /* set edges to zero */

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ ){ QQ(ii,jj,0) = QQ(ii,jj,nz-1) = 0 ; }

   for( kk=0 ; kk < nz ; kk++ )
      for( jj=0 ; jj < ny ; jj++ ){ QQ(0,jj,kk) = QQ(nx-1,jj,kk) = 0 ; }

   for( kk=0 ; kk < nz ; kk++ )
      for( ii=0 ; ii < nx ; ii++ ){ QQ(ii,0,kk) = QQ(ii,ny-1,kk) = 0 ; }

   /* if in Talairach view, remove stuff above the brain */
   /* [coordinate order is x=R-L, y=A-P, z=I-S]          */

   if( dset->view_type == VIEW_TALAIRACH_TYPE ){
      xyz_to_ijk( dset , 0.0 , 0.0 , 75.0 , NULL,NULL , &kc ) ;
      for( kk=kc ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
            for( ii=0 ; ii < nx ; ii++ ) QQ(ii,jj,kk) = 0 ;
      ktop = kc-1 ;
   } else {
      ktop = nz-1 ;
   }

   /* now prune the dataset of the little isolated junk */

   remove_isolated_stuff( qset ) ;

   /* find center of brain */

   xyz_to_ijk( dset , 0.0,15.0,0.0 , &ic , &jc , NULL ) ;  /* a decent place */

   /* for each slice:
        build a mask of blocking points (zero voxels in the slice)
        find some outermost points
        flood fill from the outermost points
        compute a metric of "annularity" of the filled points
        if the slice passes the metric, blast out the filled points
   */

   bar = (byte *) malloc( sizeof(byte) * nxy ) ;
   didit = 0 ;

   for( kk=ktop ; kk >= 0 ; kk-- ){

      memset( bar , 0 , sizeof(byte) * nxy ) ;       /* build the mask */
      cc = 0 ;
      for( jj=0 ; jj < ny ; jj++ )
         for( ii=0 ; ii < nx ; ii++ )
            if( QQ(ii,jj,kk) == 0 ){ BB(ii,jj) = 1 ; cc++ ; }

      if( cc < 0.1*nxy ){
         if( verbose ) printf("-- Slice %3d: skipping because not enough zeros\n",kk) ;
         continue ;
      }

      /* scan in from right, fill from the first nonmasked point we hit */

      for( ii=0 ; ii < ic ; ii++ )
         if( BB(ii,jc) != 1 ) break ;
      if( ii < ic ) DRAW_2dfiller( nx,ny , ii,jc , bar ) ;

      /* scan in from left */

      for( ii=nx-1 ; ii > ic ; ii-- )
         if( BB(ii,jc) != 1 ) break ;
      if( ii > ic && BB(ii,jc) == 0 ) DRAW_2dfiller( nx,ny , ii,jc , bar ) ;

      /* scan in from diagonals */

      pp = MIN(ic,jc) ;
      for( ii=ic-pp,jj=jc-pp ; ii < ic && jj < jc ; ii++,jj++ )
         if( BB(ii,jj) != 1 ) break ;
      if( ii < ic && jj < jc && BB(ii,jj) == 0 ) DRAW_2dfiller( nx,ny , ii,jj , bar ) ;

      pp = MIN(ic,ny-1-jc) ;
      for( ii=ic-pp,jj=jc+pp ; ii < ic && jj > jc ; ii++,jj-- )
         if( BB(ii,jj) != 1 ) break ;
      if( ii < ic && jj > jc && BB(ii,jj) == 0 ) DRAW_2dfiller( nx,ny , ii,jj , bar ) ;

      pp = MIN(nx-1-ic,jc) ;
      for( ii=ic+pp,jj=jc-pp ; ii > ic && jj < jc ; ii--,jj++ )
         if( BB(ii,jj) != 1 ) break ;
      if( ii > ic && jj < jc && BB(ii,jj) == 0 ) DRAW_2dfiller( nx,ny , ii,jj , bar ) ;

      pp = MIN(nx-1-ic,ny-1-jc) ;
      for( ii=ic+pp,jj=jc+pp ; ii > ic && jj > jc ; ii--,jj-- )
         if( BB(ii,jj) != 1 ) break ;
      if( ii > ic && jj > jc && BB(ii,jj) == 0 ) DRAW_2dfiller( nx,ny , ii,jj , bar ) ;

      /* we don't scan in from top/bottom since those might be
         clipped off in Talairach coordinates, which would be disastrous! */

      /* compute metric for marked points:
           metric = average( r**2 ) / average(r)**2
           this should be nearly 1 for an annular region */

      qsum = sum = 0.0 ; nsum = 0 ;
      for( jj=0 ; jj < ny ; jj++ ){
         for( ii=0 ; ii < nx ; ii++ ){
            if( BB(ii,jj) == 2 ){
               rr    = (ii-ic)*(ii-ic) + (jj-jc)*(jj-jc) ;
               qsum += rr ;
               sum  += sqrt(rr) ;
               nsum++ ;
            }
         }
      }

      if( nsum < 10 ){
         if( verbose ) printf("-- Slice %3d: skipping because not enough boundary fillin\n",kk) ;
         continue ;
      }

      qsum   = qsum/nsum ;
      sum    = (sum*sum)/(nsum*nsum) ;
      metric = qsum / sum ;
      area   = nsum * dx*dx ;
      if( verbose ) printf("-- Slice %3d: metric =%8.4f  area =%8.0f\n",kk,metric,area) ;

      if( metric <= metric_thresh && area <= area_thresh ){
         for( jj=0 ; jj < ny ; jj++ ){
            for( ii=0 ; ii < nx ; ii++ ){
               if( BB(ii,jj) == 2 ) QQ(ii,jj,kk) = 0 ;
            }
         }
         didit++ ;
      } else if( didit > 0 ) {
         break ;  /* don't go below first slice that fails, after some slice worked */
      }
   }

   if( ! didit ){fprintf(stderr,"** No slices were trimmed -- end of run!\n");exit(1);}

   /* if in Talairach view, remove stuff to the sides of the brain   */
   /* (done after the slice stuff so the scalp annulus isn't broken) */

   if( dset->view_type == VIEW_TALAIRACH_TYPE ){

      xyz_to_ijk( dset ,  0.0, -71.0, 0.0 , NULL , &jc , NULL ) ;   /* anterior */
      for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj <= jc ; jj++ )
            for( ii=0 ; ii < nx ; ii++ ) QQ(ii,jj,kk) = 0 ;

      xyz_to_ijk( dset ,  0.0, 103.0, 0.0 , NULL , &jc , NULL ) ;   /* posterior */
      for( kk=0 ; kk < nz ; kk++ )
         for( jj=jc ; jj < ny ; jj++ )
            for( ii=0 ; ii < nx ; ii++ ) QQ(ii,jj,kk) = 0 ;

      xyz_to_ijk( dset ,  -69.0, 0.0, 0.0 , &ic , NULL , NULL ) ;   /* right */
      for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
            for( ii=0 ; ii <= ic ; ii++ ) QQ(ii,jj,kk) = 0 ;

      xyz_to_ijk( dset ,  69.0, 0.0, 0.0 , &ic , NULL , NULL ) ;    /* left */
      for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
            for( ii=ic ; ii < nx ; ii++ ) QQ(ii,jj,kk) = 0 ;

      xyz_to_ijk( dset , 0.0, 0.0, -43.0 , NULL , &jc , &kc ) ;     /* below */
      for( kk=0 ; kk <= kc ; kk++ )
         for( jj=0 ; jj <= jc ; jj++ )
            for( ii=0 ; ii < nx ; ii++ ) QQ(ii,jj,kk) = 0 ;
   }

   /* get rid of the hyperintensity stuff */

   if( hyper && hyval > base+1 ){
      short val ;

           if( hyper == 1 ) val = 0 ;
      else if( hyper == 2 ) val = hyval ;

      for( ii=pp=0 ; ii < nxyz ; ii++ ){
         if( qar[ii] >= hyval ){ qar[ii] = val ; pp++ ; }
      }
      if( verbose ) printf("-- Processed %d voxels at or above hyval\n",pp) ;
   }

   /* re-prune the dataset of the little isolated junk */

   remove_isolated_stuff( qset ) ;

   /* write output */

   DSET_write(qset) ;
   exit(0) ;
}

/*==================================================================================*/

#define NPMAX 128

void find_base_value( int nxyz , short * sfim , int * nbase , int * hyval )
{
   float bper = 60.0 , bmin = 1 ;

   int ii , kk , nbin , sval , sum , nbot , a,b,c , npeak,ntop , nvox ;
   int * fbin ;
   int   kmin[NPMAX] , kmax[NPMAX] ;
   int   nmin        , nmax        ;

   /*-- make histogram of shorts --*/

   fbin = (int *) malloc( sizeof(int) * 32768 ) ;
   for( kk=0 ; kk < 32768 ; kk++ ) fbin[kk] = 0 ;

   nvox = 0 ;

   for( ii=0 ; ii < nxyz ; ii++ ){
      kk = sfim[ii] ; if( kk >= 0 ){ fbin[kk]++ ; nvox++ ; }
   }

   /*-- find largest value --*/

   for( kk=32767 ; kk > 0 ; kk-- ) if( fbin[kk] > 0 ) break ;
   if( kk == 0 ){fprintf(stderr,"** find_base_value: All voxels are zero!\n");exit(1);}
   nbin = kk+1 ;

   /*-- find bper point in cumulative distribution --*/

   sval = 0.01 * bper * nvox ;
   sum  = 0 ;
   for( kk=0 ; kk < nbin ; kk++ ){
      sum += fbin[kk] ; if( sum >= sval ) break ;
   }
   nbot = kk ; if( nbot == 0 ) nbot = 1 ; if( bmin > nbot ) nbot = bmin ;
   if( nbot >= nbin-9 ){
      fprintf(stderr,"** find_base_value: Base point on histogram too high\n");
      exit(1);
   }

   /*-- smooth histogram --*/

   b = fbin[nbot-1] ; c = fbin[nbot] ;
   for( kk=nbot ; kk < nbin ; kk++ ){
      a = b ; b = c ; c = fbin[kk+1] ; fbin[kk] = 0.25*(a+c+2*b) ;
   }

   /*-- find minima and maxima above bper point --*/

   nmin = nmax = 0 ;
   for( kk=nbot+1 ; kk < nbin ; kk++ ){
      if( fbin[kk] < fbin[kk-1] && fbin[kk] < fbin[kk+1] && nmin < NPMAX ){
         kmin[nmin++] = kk ;
      } else if( fbin[kk] > fbin[kk-1] && fbin[kk] > fbin[kk+1] && nmax < NPMAX ){
         kmax[nmax++] = kk ;
      }
   }

   /*-- find the largest two maxima --*/

   if( nmax == 0 ){
      fprintf(stderr,"** find_base_value: No histogram maxima above base point\n");
      exit(1);
   }

   if( nmax == 1 ){
      npeak = kmax[0] ; ntop = 0 ;
   } else {
      int f1,f2 , k1,k2 , fk , klow,kup ;

      k1 = 0 ; f1 = fbin[kmax[0]] ;
      k2 = 1 ; f2 = fbin[kmax[1]] ;
      if( f1 < f2 ){
         k1 = 1 ; f1 = fbin[kmax[1]] ;
         k2 = 0 ; f2 = fbin[kmax[0]] ;
      }

      for( kk=2 ; kk < nmax ; kk++ ){
         fk = fbin[kmax[kk]] ;
         if( fk > f1 ){
            f2 = f1 ; k2 = k1 ;
            f1 = fk ; k1 = kk ;
         } else if( fk > f2 ){
            f2 = fk ; k2 = kk ;
         }
      }
      npeak = MIN( kmax[k1] , kmax[k2] ) ;  /* smaller bin of the 2 top peaks */

      /* find valley between 2 peaks */

      ntop  = MAX( kmax[k1] , kmax[k2] ) ;

      fk = fbin[ntop] ; klow = ntop ;
      for( kk=ntop-1 ; kk >= npeak ; kk-- ){
         if( fbin[kk] < fk ){ fk = fbin[kk] ; klow = kk ; }
      }
      fk  = MAX( 0.10*fk , 0.05*fbin[ntop] ) ;
      kup = MIN( nbin-1 , ntop+3*(ntop-klow+2) ) ;
      for( kk=ntop+1 ; kk <= kup ; kk++ ) if( fbin[kk] < fk ) break ;

      ntop = kk ;
   }

   for( kk=npeak-1 ; kk > 0 ; kk-- )
      if( fbin[kk] < fbin[kk-1] && fbin[kk] < fbin[kk+1] ) break ;

   if( ntop == 0 ) ntop = npeak + (npeak-kk) ;

   *nbase = kk ;
   *hyval = ntop ;

   free(fbin) ; return ;
}

/*==================================================================================*/

void remove_isolated_stuff( THD_3dim_dataset * qset )
{
   short * qar = DSET_ARRAY(qset,0) ;
   short nb[27] ;
   int   nblast , nx,ny,nz , nxy,nxyz , ii,jj,kk,ll,cc ;
   float dx = DSET_DX(qset) ;
   EDIT_options edopt ;

   nx = DSET_NX(qset) ; ny = DSET_NY(qset) ; nz = DSET_NZ(qset) ;
   nxy = nx*ny ; nxyz = nx*ny*nz ;

   do{ nblast = 0 ;

       /* edit 3x3x3 volumes */

       for( kk=1 ; kk < nz-1 ; kk++ ){
         for( jj=1 ; jj < ny-1 ; jj++ ){
            for( ii=1 ; ii < nx-1 ; ii++ ){
               if( QQ(ii,jj,kk) > 0 ){           /* must have at least 3 nonzero   */
                  nb[ 0] = QQ(ii-1,jj-1,kk-1) ;  /* voxels in a 3x3x3 neighborhood */
                  nb[ 1] = QQ(ii  ,jj-1,kk-1) ;
                  nb[ 2] = QQ(ii+1,jj-1,kk-1) ;
                  nb[ 3] = QQ(ii-1,jj  ,kk-1) ;
                  nb[ 4] = QQ(ii  ,jj  ,kk-1) ;
                  nb[ 5] = QQ(ii+1,jj  ,kk-1) ;
                  nb[ 6] = QQ(ii-1,jj+1,kk-1) ;
                  nb[ 7] = QQ(ii  ,jj+1,kk-1) ;
                  nb[ 8] = QQ(ii+1,jj+1,kk-1) ;
                  nb[ 9] = QQ(ii-1,jj-1,kk  ) ;
                  nb[10] = QQ(ii  ,jj-1,kk  ) ;
                  nb[11] = QQ(ii+1,jj-1,kk  ) ;
                  nb[12] = QQ(ii-1,jj  ,kk  ) ;
                  nb[13] = QQ(ii  ,jj  ,kk  ) ;
                  nb[14] = QQ(ii+1,jj  ,kk  ) ;
                  nb[15] = QQ(ii-1,jj-1,kk  ) ;
                  nb[16] = QQ(ii  ,jj-1,kk  ) ;
                  nb[17] = QQ(ii+1,jj-1,kk  ) ;
                  nb[18] = QQ(ii-1,jj+1,kk+1) ;
                  nb[19] = QQ(ii  ,jj+1,kk+1) ;
                  nb[20] = QQ(ii+1,jj+1,kk+1) ;
                  nb[21] = QQ(ii-1,jj  ,kk+1) ;
                  nb[22] = QQ(ii  ,jj  ,kk+1) ;
                  nb[23] = QQ(ii+1,jj  ,kk+1) ;
                  nb[24] = QQ(ii-1,jj+1,kk+1) ;
                  nb[25] = QQ(ii  ,jj+1,kk+1) ;
                  nb[26] = QQ(ii+1,jj+1,kk+1) ;

                  for( ll=cc=0 ; ll < 27 ; ll++ ) if( nb[ll] > 0 ) cc++ ;
                  if( cc < 4 ){ QQ(ii,jj,kk) = 0 ; nblast++ ; }
               }
            }
         }
       }

       /* edit 3x3 areas in each slice orientation */

       for( kk=1 ; kk < nz-1 ; kk++ ){
         for( jj=1 ; jj < ny-1 ; jj++ ){
            for( ii=1 ; ii < nx-1 ; ii++ ){
               if( QQ(ii,jj,kk) > 0 ){         /* must have at least 2 nonzero */
                  nb[ 0] = QQ(ii-1,jj-1,kk) ;  /* voxels in a 3x3 neighborhood */
                  nb[ 1] = QQ(ii  ,jj-1,kk) ;
                  nb[ 2] = QQ(ii+1,jj-1,kk) ;
                  nb[ 3] = QQ(ii-1,jj  ,kk) ;
                  nb[ 4] = QQ(ii  ,jj  ,kk) ;
                  nb[ 5] = QQ(ii+1,jj  ,kk) ;
                  nb[ 6] = QQ(ii-1,jj+1,kk) ;
                  nb[ 7] = QQ(ii  ,jj+1,kk) ;
                  nb[ 8] = QQ(ii+1,jj+1,kk) ;
                  for( ll=cc=0 ; ll < 9 ; ll++ ) if( nb[ll] > 0 ) cc++ ;
                  if( cc < 2 ){ QQ(ii,jj,kk) = 0 ; nblast++ ; }
               }
            }
         }
       }

       for( jj=1 ; jj < ny-1 ; jj++ ){
          for( kk=1 ; kk < nz-1 ; kk++ ){
            for( ii=1 ; ii < nx-1 ; ii++ ){
               if( QQ(ii,jj,kk) > 0 ){         /* must have at least 2 nonzero */
                  nb[ 0] = QQ(ii-1,jj,kk-1) ;  /* voxels in a 3x3 neighborhood */
                  nb[ 1] = QQ(ii  ,jj,kk-1) ;
                  nb[ 2] = QQ(ii+1,jj,kk-1) ;
                  nb[ 3] = QQ(ii-1,jj,kk  ) ;
                  nb[ 4] = QQ(ii  ,jj,kk  ) ;
                  nb[ 5] = QQ(ii+1,jj,kk  ) ;
                  nb[ 6] = QQ(ii-1,jj,kk+1) ;
                  nb[ 7] = QQ(ii  ,jj,kk+1) ;
                  nb[ 8] = QQ(ii+1,jj,kk+1) ;
                  for( ll=cc=0 ; ll < 9 ; ll++ ) if( nb[ll] > 0 ) cc++ ;
                  if( cc < 2 ){ QQ(ii,jj,kk) = 0 ; nblast++ ; }
               }
            }
         }
       }

       for( ii=1 ; ii < nx-1 ; ii++ ){
         for( jj=1 ; jj < ny-1 ; jj++ ){
            for( kk=1 ; kk < nz-1 ; kk++ ){
               if( QQ(ii,jj,kk) > 0 ){         /* must have at least 2 nonzero */
                  nb[ 0] = QQ(ii,jj-1,kk-1) ;  /* voxels in a 3x3 neighborhood */
                  nb[ 1] = QQ(ii,jj  ,kk-1) ;
                  nb[ 2] = QQ(ii,jj+1,kk-1) ;
                  nb[ 3] = QQ(ii,jj-1,kk  ) ;
                  nb[ 4] = QQ(ii,jj  ,kk  ) ;
                  nb[ 5] = QQ(ii,jj+1,kk  ) ;
                  nb[ 6] = QQ(ii,jj-1,kk+1) ;
                  nb[ 7] = QQ(ii,jj  ,kk+1) ;
                  nb[ 8] = QQ(ii,jj+1,kk+1) ;
                  for( ll=cc=0 ; ll < 9 ; ll++ ) if( nb[ll] > 0 ) cc++ ;
                  if( cc < 2 ){ QQ(ii,jj,kk) = 0 ; nblast++ ; }
               }
            }
         }
       }

      if( verbose ) printf("-- Blasted %d voxels for being too isolated\n",nblast) ;
   } while( nblast > 0 ) ;

   /* cluster data, remove small clusters */

   INIT_EDOPT( &edopt ) ;
   edopt.edit_clust = ECFLAG_SAME;
   edopt.clust_rmm  = 1.01*dx ;
   edopt.clust_vmul = 25000.0 ;
   EDIT_one_dataset( qset , &edopt ) ;

   return ;
}

/*---------------------------------------------------------------------------*/

void xyz_to_ijk( THD_3dim_dataset * ds , float x , float y , float z ,
                                         int * i , int * j , int * k  )
{
   THD_fvec3 fv ;
   THD_ivec3 iv ;

   LOAD_FVEC3( fv , x,y,z ) ;
   fv = THD_dicomm_to_3dmm( ds , fv ) ;
   iv = THD_3dmm_to_3dind ( ds , fv ) ;
   if( i != NULL ) *i = iv.ijk[0] ;
   if( j != NULL ) *j = iv.ijk[1] ;
   if( k != NULL ) *k = iv.ijk[2] ;
   return ;
}

/*---------------------------------------------------------------------------
   Flood filling a byte array:
     nx = 1st dimension
     ny = 2nd dimension
     ix = start point
     jy = end point
     ar = array, with 0's everwhere except 1's as barriers to flooding

   All filled points (starting with ix,jy) will get the value 2.
-----------------------------------------------------------------------------*/

void DRAW_2dfiller( int nx , int ny , int ix , int jy , byte * ar )
{
   int ii,jj , ip,jp , num ;

#define AR(i,j) ar[(i)+(j)*nx]

   /* fill out in cross from 1st point */

   ip = ix ; jp = jy ; AR(ip,jp) = 2 ;

   for( ii=ip+1; ii < nx && AR(ii,jp) == 0; ii++ ) AR(ii,jp) = 2;
   for( ii=ip-1; ii >= 0 && AR(ii,jp) == 0; ii-- ) AR(ii,jp) = 2;
   for( jj=jp+1; jj < ny && AR(ip,jj) == 0; jj++ ) AR(ip,jj) = 2;
   for( jj=jp-1; jj >= 0 && AR(ip,jj) == 0; jj-- ) AR(ip,jj) = 2;

   /* brute force repetition of the cross technique */

   do {
      num = 0 ;
      for( jp=0 ; jp < ny ; jp++ ){
         for( ip=0 ; ip < nx ; ip++ ){
            if( AR(ip,jp) == 2 ){
               for( ii=ip+1; ii < nx && AR(ii,jp) == 0; ii++ ){ AR(ii,jp) = 2; num++; }
               for( ii=ip-1; ii >= 0 && AR(ii,jp) == 0; ii-- ){ AR(ii,jp) = 2; num++; }
               for( jj=jp+1; jj < ny && AR(ip,jj) == 0; jj++ ){ AR(ip,jj) = 2; num++; }
               for( jj=jp-1; jj >= 0 && AR(ip,jj) == 0; jj-- ){ AR(ip,jj) = 2; num++; }
            }
         }
      }
   } while( num > 0 ) ;

   return ;
}
