/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*===========================================================================
   Function to extract a plane of shifted bytes from a 3D volume.
     nx, ny, nz = dimensions of vol
     vol        = input 3D volume of bytes

     fixdir = fixed direction (1=x, 2=y, 3=z)
     fixijk = fixed index
     da, db = shift in planar coordinaes (non-fixed directions)
     ma, mb = dimensions of im
     im     = output 2D image

   Goal is im[a,b] = vol[ P(a-da,b-db,c=fixijk) ] for a=0..ma-1, b=0..mb-1,
   where P(a,b,c) is the permutation of (a,b,c) that goes with fixdir:
     P(x,y,z) = (y,z,x) for fixdir == 1
     P(x,y,z) = (z,x,y) for fixdir == 2
     P(x,y,z) = (x,y,z) for fixdir == 3
   For values outside the range of vol[], im[] is set to 0.
=============================================================================*/

  /* macros for offsets in vol[] to corners of the interpolation square */

#undef LL
#undef LR
#undef UL
#undef UR

#define LL 0                /* voxel offset to lower left  */
#define LR astep            /* voxel offset to lower right */
#define UL bstep            /* voxel offset to upper left  */
#define UR (astep+bstep)    /* voxel offset to upper right */

#define ASSIGN_DIRECTIONS                                       \
 do{ switch( fixdir ){                                          \
      default:                                                  \
      case 1:            /* x-direction: (a,b,c) = (y,z,x) */   \
         astep = nx ; bstep = nxy ; cstep = 1  ;                \
         na    = ny ; nb    = nz  ; nc    = nx ;                \
      break ;                                                   \
                                                                \
      case 2:            /* y-direction: (a,b,c) = (z,x,y) */   \
         astep = nxy ; bstep = 1  ; cstep = nx ;                \
         na    = nz  ; nb    = nx ; nc    = ny ;                \
      break ;                                                   \
                                                                \
      case 3:            /* z-direction: (a,b,c) = (x,y,z) */   \
         astep = 1  ; bstep = nx ; cstep = nxy ;                \
         na    = nx ; nb    = ny ; nc    = nz  ;                \
      break ;                                                   \
    } } while(0)

/*-----------------------------------------------------------------------*/

void extract_assign_directions( int nx, int ny, int nz, int fixdir ,
                                int *Astep, int *Bstep, int *Cstep ,
                                int *Na   , int *Nb   , int *Nc     )
{
   int astep,bstep,cstep , na,nb,nc , nxy=nx*ny ;

   ASSIGN_DIRECTIONS ;

   *Astep = astep ; *Bstep = bstep ; *Cstep = cstep ;
   *Na    = na    ; *Nb    = nb    ; *Nc    = nc    ; return ;
}

/*-----------------------------------------------------------------------
   NN "interpolation"
-------------------------------------------------------------------------*/

void extract_byte_nn( int nx , int ny , int nz , byte * vol ,
                      Tmask * tm ,
                      int fixdir , int fixijk , float da , float db ,
                      int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc ;
   byte * mask ;

   memset( im , 0 , ma*mb ) ;  /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   da += 0.5 ; adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da+0.5) */
   db += 0.5 ; bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db+0.5) */

   abot = 0       ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel ; if( atop > ma   ) atop = ma ;

   bbot = 0       ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
      if( mask == NULL || mask[bb] )
         for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
            im[aa+boff] = vol[aoff+ijkoff] ;  /* im(aa,bb) = vol(aa-adel,bb-bdel,fixijk) */
                                              /*           = vol[ (aa-adel)*astep +
                                                                  (bb-bdel)*bstep +
                                                                  fixijk   *cstep   ]    */

   return ;
}

/*---------------------------------------------------------------------------
    Two-step interpolation
-----------------------------------------------------------------------------*/

#if 1
# define TSBOT 0.3
# define TSTOP 0.7
#else
# define TSBOT 0.25
# define TSTOP 0.75
#endif

void extract_byte_ts( int nx , int ny , int nz , byte * vol ,
                      Tmask * tm ,
                      int fixdir , int fixijk , float da , float db ,
                      int ma , int mb , byte * im )
{
   int adel,bdel , abot,atop , bb,bbot,btop , nxy=nx*ny ;
   register int aa , ijkoff , aoff,boff ;
   int astep,bstep,cstep , na,nb,nc , nts,dts1,dts2 ;
   float fa , fb ;
   byte * mask ;

   memset( im , 0 , ma*mb ) ;  /* initialize output to zero */

   if( fixijk < 0 ) return ;

   ASSIGN_DIRECTIONS ;

   if( fixijk >= nc ) return ;

   adel = (int) da ; if( da < 0.0 ) adel-- ;  /* floor(da) */
   bdel = (int) db ; if( db < 0.0 ) bdel-- ;  /* floor(db) */

   fa = da - adel ;               /* fractional part of dj */
   fb = db - bdel ;               /* fractional part of dk */

   fa = 1.0-fa ; fb = 1.0-fb ;

   if( fa < TSBOT ){                      /*- Left 30% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 1 ; dts1 = LL ;               /* [0,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 1 ; dts1 = UL ;               /* [0,1] */
      } else {                            /*- Middle 40% -*/
        nts = 2 ; dts1 = LL ; dts2 = UL ;   /* mid of [0,0] and [0,1] */
      }
   } else if( fa > TSTOP ){               /*- Right 30% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 1 ; dts1 = LR ;               /* [1,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 1 ; dts1 = UR ;               /* [1,1] */
      } else {                            /*- Middle 40% -*/
        nts = 2 ; dts1 = LR ; dts2 = UR ;   /* mid of [1,0] and [1,1] */
      }
   } else {                               /*- Middle 40% -*/
      if( fb < TSBOT ){                   /*- Lower 30% -*/
        nts = 2 ; dts1 = LL ; dts2 = LR ;   /* mid of [0,0] and [1,0] */
      } else if( fb > TSTOP ){            /*- Upper 30% -*/
        nts = 2 ; dts1 = UL ; dts2 = UR ;   /* mid of [0,1] and [1,1] */
      } else {                            /*- Middle 40% -*/
        nts = 4 ;                           /* mid of all 4 points */
      }
   }

   adel++ ; bdel++ ;

   abot = 0         ; if( abot < adel ) abot = adel ;       /* range in im[] */
   atop = na+adel-1 ; if( atop > ma   ) atop = ma ;

   bbot = 0         ; if( bbot < bdel ) bbot = bdel ;
   btop = nb+bdel-1 ; if( btop > mb   ) btop = mb ;

   ijkoff = fixijk*cstep + (abot-adel)*astep + (bbot-bdel)*bstep ;
   boff   = bbot * ma ;

   mask = (tm == NULL) ? NULL
                       : tm->mask[fixdir%3] + (fixijk*nb - bdel) ;

   switch( nts ){

      case 1:
         ijkoff += dts1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = vol[aoff+ijkoff] ;
      break ;

      case 2:
         ijkoff += dts1 ; dts2 -= dts1 ;
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = (vol[aoff+ijkoff] + vol[aoff+(ijkoff+dts2)]) >> 1;
      break ;

      case 4:
         for( bb=bbot ; bb < btop ; bb++,boff+=ma,ijkoff+=bstep )
           if( mask == NULL || mask[bb] || mask[bb+1] )
             for( aa=abot,aoff=0 ; aa < atop ; aa++,aoff+=astep )
               im[aa+boff] = ( vol[aoff+ijkoff]     +vol[aoff+(ijkoff+LR)]
                              +vol[aoff+(ijkoff+UL)]+vol[aoff+(ijkoff+UR)]) >> 2;
      break ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/

#undef U
#define U(i,j) uu.mat[i][j]

MRI_IMAGE * project_byte_mip( int nx, int ny, int nz, byte * vol, Tmask * tm,
                              THD_mat33 uu )
{
   int ii,jj,kk , ni,nj,nk,pk , ma,mb,mab , pij , nnn[3] ;
   float utop,uabs , a,b , aii,aij,aji,ajj , hnk , ba,bb ;
   byte *im , *sl ;
   MRI_IMAGE *bim , *qim ;

#if 1
DUMP_MAT33("rotation",uu) ;
#endif

   /*-- find element U(kk,2) that is largest --*/

   nnn[0] = nx ; nnn[1] = ny ; nnn[2] = nz ;

   kk = 0 ; utop = fabs(U(0,2)) ;
   uabs = fabs(U(1,2)) ; if( uabs > utop ){ utop = uabs; kk = 1; }
   uabs = fabs(U(2,2)) ; if( uabs > utop ){ utop = uabs; kk = 2; }

   if( utop == 0.0 ) return ;   /* bad matrix */

   ii = (kk+1) % 3 ;  /* image axes */
   jj = (kk+2) % 3 ;

   a = U(ii,2) / U(kk,2) ;  /* shearing parameters */
   b = U(jj,2) / U(kk,2) ;

#if 0
fprintf(stderr,"kk=%d a=%g b=%g\n",kk,a,b) ;
#endif

   aii = U(ii,0) - a * U(kk,0) ;  /* warping parameters */
   aij = U(ii,1) - a * U(kk,1) ;  /* [not used just yet] */
   aji = U(jj,0) - b * U(kk,0) ;
   ajj = U(jj,1) - b * U(kk,1) ;

#if 0
fprintf(stderr,"warp: aii=%g  aij=%g\n"
               "      aji=%g  ajj=%g\n" , aii,aij,aji,ajj ) ;
#endif

   ni  = nnn[ii] ; nj = nnn[jj] ; nk = nnn[kk] ; hnk = 0.5*nk ;
   ma  = MAX(ni,nj) ; ma = MAX(ma,nk) ; ma *= 1.2 ;
   mb  = ma ; mab = ma * mb ; ba = 0.5*(ma-ni) ; bb = 0.5*(mb-nj) ;
   sl  = (byte *) malloc(mab) ;
   bim = mri_new(ma,mb,MRI_byte) ; im = MRI_BYTE_PTR(bim) ; memset(im,0,mab) ;
   for( pk=0 ; pk < nk ; pk++ ){
      extract_byte_ts( nx,ny,nz , vol , tm ,
                       kk+1 , pk , ba-a*(pk-hnk) , bb-b*(pk-hnk) ,
                       ma,mb , sl ) ;
      for( pij=0 ; pij < mab ; pij++ )
         if( sl[pij] > im[pij] ) im[pij] = sl[pij] ;
   }

   free(sl) ;

#if 1
   qim = mri_aff2d_byte( bim , 1 , aii,aij,aji,ajj ) ;
   mri_free(bim) ; bim = qim ;
#endif

   return bim ;
}

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

static THD_mat33 rotmatrix( int ax1,float th1 ,
                            int ax2,float th2 , int ax3,float th3  )
{
   THD_mat33 q , p ;

   LOAD_ROT_MAT( q , th1 , ax1 ) ;
   LOAD_ROT_MAT( p , th2 , ax2 ) ; q = MAT_MUL( p , q ) ;
   LOAD_ROT_MAT( p , th3 , ax3 ) ; q = MAT_MUL( p , q ) ;

   return q ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset ;
   int iarg=1 ;
   char *cc1="x",*cc2="y",*cc3="z" ;
   float th1=0.0, th2=0.0, th3=0.0 ;
   float thx,thy,thz ;
   int   axx,ayy,azz ;
   char *fname="testcox.ppm" ;
   void * rhand ;
   int bot=1 , ii ;
   float omap[128] , bfac ;
   MRI_IMAGE * im , * brim ;
   int hbr[256] , nperc,ibot,itop,sum ;
   byte * bar ;
   THD_mat33 rmat ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: testcox [-rotate a b c] [-out f] [-bot b] dset\n") ;
      exit(0) ;
   }

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-bot") == 0 ){
         bot = strtod( argv[++iarg] , NULL ) ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-rotate") == 0 ){
         th1 = (PI/180.0) * strtod( argv[++iarg] , &cc1 ) ;
         th2 = (PI/180.0) * strtod( argv[++iarg] , &cc2 ) ;
         th3 = (PI/180.0) * strtod( argv[++iarg] , &cc3 ) ;

         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-out") == 0 ){
         fname = argv[++iarg] ;
         iarg++ ; continue ;
      }

      fprintf(stderr,"Illegal option: %s\n",argv[iarg]); exit(1);
   }

   if( iarg >= argc ){fprintf(stderr,"No dataset?\n"); exit(1); }

   dset = THD_open_dataset( argv[iarg] ) ;
   if( dset == NULL ){fprintf(stderr,"Can't open dataset!\n");exit(1);}
   if( DSET_BRICK_TYPE(dset,0) != MRI_byte ){
      fprintf(stderr,"Non-byte dataset input!\n");exit(1);
   }
   DSET_mallocize(dset) ; DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
      fprintf(stderr,"Can't load dataset!\n");exit(1);
   }

   /* correct angles to go with rendering plugin are
         <yaw>A <pitch>R <roll>I
      in that order                                 */

   THD_rotangle_user_to_dset( dset ,
                              th1,*cc1  , th2,*cc2  , th3,*cc3 ,
                              &thx,&axx , &thy,&ayy , &thz,&azz ) ;
   rmat = rotmatrix( axx,thx , ayy,thy , azz,thz ) ;

   im = project_byte_mip( DSET_NX(dset) , DSET_NY(dset) , DSET_NZ(dset) ,
                          DSET_ARRAY(dset,0) , NULL , rmat ) ;

   mri_write_pnm( fname , im ) ;
   fprintf(stderr,"+++ Output to file %s\n",fname);
   exit(0) ;
}
