#include "mrilib.h"

/*--------------------------------------------------------------------------*/

typedef struct {
   int npts ;
   int * nstart ;
   int * nwt ;
   float ** wt ;
} intermap ;

/*---------------------------------------------------------------------------
  Setup for linear interpolation from grid xin[0..nin-1] to xout[0..nout-1].
  Both grids are assumed to be in increasing order.
-----------------------------------------------------------------------------*/

intermap * INTERP_setup_linear( int nin, float * xin, int nout, float * xout )
{
   intermap * imap ;
   int ii,ib ;
   float xx ;

   if( nin < 2 || nout < 2 || xin == NULL || xout == NULL ) return NULL ;

   imap = (intermap *) malloc(sizeof(intermap)) ;

   imap->npts   = nout ;
   imap->nstart = (int *)    calloc(sizeof(int)    ,nout) ;
   imap->nwt    = (int *)    calloc(sizeof(int)    ,nout) ;
   imap->wt     = (float **) calloc(sizeof(float *),nout) ;

   for( ii=0 ; ii < nout ; ii++ ){
      xx = xout[ii] ;                            /* output point */

      if( xx < xin[0] ){                         /* before 1st point */
         if( xin[0]-xx <= 0.5*(xin[1]-xin[0]) ){ /* but not too much */
            imap->nstart[ii] = 0 ;
            imap->nwt[ii]    = 1 ;
            imap->wt[ii]     = (float *) malloc(sizeof(float)) ;
            imap->wt[ii][0]  = 1.0 ;
         }
         continue ;
      } else if ( xx > xin[nin-1] ){                         /* after last point */
         if( xx-xin[nin-1] <= 0.5*(xin[nin-1]-xin[nin-2]) ){ /* but not too much */
            imap->nstart[ii] = nin-1 ;
            imap->nwt[ii]    = 1 ;
            imap->wt[ii]     = (float *) malloc(sizeof(float)) ;
            imap->wt[ii][0]  = 1.0 ;
         }
         continue ;
      }

      /* OK, somewhere in the middle;
         search for the input interval that brackets this output point */

      for( ib=0 ; ib < nin-1 ; ib++ )
         if( xx >= xin[ib] && xx <= xin[ib+1] ) break ;
      if( ib == nin ) continue ;                        /* outside!? */

      /* make linear interpolation coefficients */

      imap->nstart[ii] = ib ;
      imap->nwt[ii]    = 2 ;
      imap->wt[ii]     = (float *) malloc(sizeof(float)*2) ;
      imap->wt[ii][1]  = (xx-xin[ib]) / (xin[ib+1]-xin[ib]) ;
      imap->wt[ii][0]  = 1.0 - imap->wt[ii][1] ;
   }

   return imap ;
}

/*--------------------------------------------------------------------------*/

void INTERP_destroy( intermap * imap )
{
   int ii ;

   if( imap == NULL ) return ;

   for( ii=0 ; ii < imap->npts ; ii++ )
      if( imap->wt[ii] != NULL ) free(imap->wt[ii]) ;

   free(imap->wt) ; free(imap->nwt) ; free(imap->nstart) ;
   free(imap) ; return ;
}

/*--------------------------------------------------------------------------
  Evaluate interpolation given by imap, from input data vin[] to
  output vout[]
----------------------------------------------------------------------------*/

void INTERP_evaluate( intermap * imap , float * vin , float * vout )
{
   int ii , jj,ib,nj , nout ;

   if( imap == NULL || vin == NULL || vout == NULL ) return ;

   nout = imap->npts ;
   for( ii=0 ; ii < nout ; ii++ ){
      vout[ii] = 0.0 ;
      ib = imap->nstart[ii] ;
      nj = imap->nwt[ii] ;
      for( jj=0 ; jj < nj ; jj++ )
         vout[ii] += imap->wt[ii][jj] * vin[ib+jj] ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset , * nset ;
   int iarg=1 ;
   float new_dz=0.0    , old_dz    ;
   int   new_nz=0      , old_nz    ;
   float new_zsize=0.0 , old_zsize ;
   char * prefix = "regrid" ;
   float old_zcen , new_zcen , zshift ;
   int bkind , nx,ny,nxy , iv,ii,kk , verb=0 ;
   byte    *bar , *nbar ;
   short   *sar , *nsar ;
   float   *far , *nfar , *ofz , *nfz ;
   complex *car , *ncar ;
   void * nvar ;
   float * zin , * zout ;
   intermap * imap ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dZregrid [option] dataset\n"
             "Alters the input dataset's slice thickness and/or number.\n"
             "\n"
             "***  For most purposes, this program has been superseded ***\n"
             "***  by program 3dresample, which can change the grid of ***\n"
             "***  a dataset in all 3 directions at once.              ***\n"
             "\n"
             "OPTIONS:\n"
             " -dz D     = sets slice thickness to D mm\n"
             " -nz N     = sets slice count to N\n"
             " -zsize Z  = sets thickness of dataset (center-to-center of\n"
             "              first and last slices) to Z mm\n"
             " -prefix P = write result in dataset with prefix P\n"
             " -verb     = write progress reports to stderr\n"
             "\n"
             "At least one of '-dz', '-nz', or '-zsize' must be given.\n"
             "On the other hand, using all 3 is over-specification.\n"
             "The following combinations make sense:\n"
             " -dz only                   ==> N stays fixed from input dataset\n"
             "                                 and then is like setting Z = N*D\n"
             " -dz and -nz together       ==> like setting Z = N*D\n"
             " -dz and -zsize together    ==> like setting N = Z/D\n"
             " -nz only                   ==> D stays fixed from input dataset\n"
             "                                 and then is like setting Z = N*D\n"
             " -zsize only                ==> D stays fixed from input dataset\n"
             "                                 and then is like setting N = Z/D\n"
             " -nsize and -zsize together ==> like setting D = Z/N\n"
             "\n"
             "NOTES:\n"
             " * If the input is a 3D+time dataset with slice-dependent time\n"
             "    offsets, the output will have its time offsets cleared.\n"
             "    It probably makes sense to do 3dTshift BEFORE using this\n"
             "    program in such a case.\n"
             " * The output of this program is centered around the same\n"
             "    location as the input dataset.  Slices outside the\n"
             "    original volume (e.g., when Z is increased) will be\n"
             "    zero.  This is NOT the same as using 3dZeropad, which\n"
             "    only adds zeros, and does not interpolate to a new grid.\n"
             " * Linear interpolation is used between slices.  However,\n"
             "    new slice positions outside the old volume but within\n"
             "    0.5 old slice thicknesses will get a copy of the last slice.\n"
             "    New slices outside this buffer zone will be all zeros.\n"
             "\n"
             "EXAMPLE:\n"
             " You have two 3D anatomical datasets from the same subject that\n"
             " need to be registered.  Unfortunately, the first one has slice\n"
             " thickness 1.2 mm and the second 1.3 mm.  Assuming they have\n"
             " the same number of slices, then do something like\n"
             "  3dZregrid -dz 1.2 -prefix ElvisZZ Elvis2+orig\n"
             "  3dvolreg -base Elvis1+orig -prefix Elvis2reg ElvisZZ+orig\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dZregrid main"); machdep(); PRINT_VERSION("3dZregrid") ;
   AFNI_logger("3dZregrid",argc,argv) ;

   /*-- scan options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-verb",5) == 0 ){
         verb++ ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-dz") == 0 ){
         new_dz = strtod( argv[++iarg] , NULL ) ;
         if( new_dz <= 0.0 ){ fprintf(stderr,"** ILLEGAL -dz value!\n"); exit(1); }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-nz") == 0 ){
         new_nz = strtod( argv[++iarg] , NULL ) ;
         if( new_nz <= 2 ){ fprintf(stderr,"** ILLEGAL -nz value!\n"); exit(1); }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-zsize") == 0 ){
         new_zsize = strtod( argv[++iarg] , NULL ) ;
         if( new_zsize <= 0.0 ){ fprintf(stderr,"** ILLEGAL -zsize value!\n"); exit(1); }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){ fprintf(stderr,"** ILLEGAL -prefix value!\n"); exit(1); }
         iarg++ ; continue ;
      }

      fprintf(stderr,"** ILLEGAL option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*-- read input dataset --*/

   if( verb ) fprintf(stderr,"++ Reading input dataset %s\n",argv[iarg]) ;
   dset = THD_open_dataset( argv[iarg] ) ;
   if( dset == NULL ){
      fprintf(stderr,"** CANNOT open dataset %s\n",argv[iarg]) ; exit(1) ;
   }
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   /*-- check for consistency --*/

   if( new_dz == 0.0 && new_nz == 0 && new_zsize == 0.0 ){
      fprintf(stderr,"** AT LEAST one of -dz, -nz, -zsize must be given!\n"); exit(1);
   }

   /*-- consider all the combinations of inputs we like --*/

   if( new_dz > 0.0 ){
      if( new_nz > 0 ){                       /* -dz and -nz */
         new_zsize = new_dz * new_nz ;
      } else if( new_zsize > 0.0 ){           /* -dz and -zsize */
         new_nz = rint( new_zsize / new_dz ) ;
      } else {                                /* -dz only */
         new_nz = DSET_NZ(dset) ;
         new_zsize = new_dz * new_nz ;
      }
   } else if( new_nz > 0 ){

      if( new_zsize > 0.0 ){                  /* -nz and -zsize */
         new_dz = new_zsize / new_nz ;
      } else {                                /* -nz only */
         new_dz = fabs(DSET_DZ(dset)) ;
         new_zsize = new_nz * new_dz ;
      }
   } else {                                   /* -zsize only */
      new_dz = fabs(DSET_DZ(dset)) ;
      new_nz = rint( new_zsize / new_dz ) ;
   }

   /*-- make sure we aren't trimming TOO much fat off --*/

   if( new_nz < 2 ){
      fprintf(stderr,"** ILLEGAL number of new slices computed = %d\n",new_nz) ;
      exit(1) ;
   }

   /*-- make empty shell of output dataset --*/

   old_nz    = DSET_NZ(dset) ;
   old_dz    = fabs(DSET_DZ(dset)) ;
   old_zsize = old_nz * old_dz ;

   nset = EDIT_empty_copy( dset ) ;
   EDIT_dset_items( nset , ADN_prefix , prefix , ADN_none ) ;
   if( THD_deathcon() && THD_is_file( DSET_HEADNAME(nset) ) ){
      fprintf(stderr,"** Output file %s exists -- cannot overwrite!\n",
              DSET_HEADNAME(nset) ) ;
      exit(1) ;
   }

   tross_Copy_History( dset , nset ) ;
   tross_Make_History( "3dZregrid" , argc,argv , nset ) ;

   /*-- adjust z-axis stuff --*/

   if( new_nz != old_nz ){
      THD_ivec3 iv_nxyz ;
      LOAD_IVEC3( iv_nxyz , DSET_NX(dset) , DSET_NY(dset) , new_nz ) ;
      EDIT_dset_items( nset , ADN_nxyz , iv_nxyz , ADN_none ) ;
      if( verb )
        fprintf(stderr,"++ Adjusting slice count from %d to %d\n",old_nz,new_nz) ;
   }

   if( new_dz != old_dz ){
      THD_fvec3 fv_dxyz ; float dz=new_dz ;
      if( DSET_DZ(dset) < 0.0 ) dz = -dz ;
      LOAD_FVEC3( fv_dxyz , DSET_DX(dset) , DSET_DY(dset) , dz ) ;
      EDIT_dset_items( nset , ADN_xyzdel , fv_dxyz , ADN_none ) ;
      if( verb )
        fprintf(stderr,"++ Adjusting slice thickness from %6.3f to %6.3f\n",old_dz,new_dz) ;
   }

   old_zcen = dset->daxes->zzorg + 0.5*(dset->daxes->nzz-1)*dset->daxes->zzdel ;
   new_zcen = nset->daxes->zzorg + 0.5*(nset->daxes->nzz-1)*nset->daxes->zzdel ;
   zshift   = new_zcen - old_zcen ;
   if( fabs(zshift) > 0.01*new_dz ){
      THD_fvec3 fv_xyzorg ; float zz ;
      zz = nset->daxes->zzorg - zshift ;
      LOAD_FVEC3( fv_xyzorg , nset->daxes->xxorg , nset->daxes->yyorg , zz ) ;
      EDIT_dset_items( nset , ADN_xyzorg , fv_xyzorg , ADN_none ) ;
   }

   if( nset->taxis != NULL && nset->taxis->nsl > 0 ){
      EDIT_dset_items( nset , ADN_nsl , 0 , ADN_none ) ;
      fprintf(stderr,
              "++ WARNING - slice-dependent time shifts have been removed!\n") ;
   }

   /*-- setup to interpolate --*/

   zin  = (float *) malloc(sizeof(float)*old_nz) ;
   zout = (float *) malloc(sizeof(float)*new_nz) ;
   for( kk=0 ; kk < old_nz ; kk++ ) zin[kk]  = (kk-0.5*old_nz) * old_dz ;
   for( kk=0 ; kk < new_nz ; kk++ ) zout[kk] = (kk-0.5*new_nz) * new_dz ;

   imap =INTERP_setup_linear( old_nz,zin , new_nz,zout ) ;

   free(zin) ; free(zout) ;

   /*-- loop over sub-bricks --*/

   nx = DSET_NX(nset) ; ny = DSET_NY(nset) ; nxy = nx*ny ;

   ofz = (float *) malloc(sizeof(float)*old_nz*2) ; /* old data */
   nfz = (float *) malloc(sizeof(float)*new_nz*2) ; /* new data */

   if( verb ) fprintf(stderr,"++ Sub-bricks:") ;

   for( iv=0 ; iv < DSET_NVALS(nset) ; iv++ ){

      if( verb ) fprintf(stderr,"%d",iv) ;

      bkind = DSET_BRICK_TYPE(dset,iv) ;
      switch( bkind ){
         default:
            fprintf(stderr,"** ILLEGAL brick type %s at sub-brick %d\n",
                    MRI_type_name[bkind] , iv ) ;
         exit(1) ;

         case MRI_byte:    bar = DSET_ARRAY(dset,iv) ;
                          nbar = (byte *)malloc(sizeof(byte)*nxy*new_nz) ;
                          nvar = (void *) nbar ;
         break ;

         case MRI_short:   sar = DSET_ARRAY(dset,iv) ;
                          nsar = (short *)malloc(sizeof(short)*nxy*new_nz) ;
                          nvar = (void *) nsar ;
         break ;

         case MRI_float:   far = DSET_ARRAY(dset,iv) ;
                          nfar = (float *)malloc(sizeof(float)*nxy*new_nz) ;
                          nvar = (void *) nfar ;
         break ;

         case MRI_complex: car = DSET_ARRAY(dset,iv) ;
                          ncar = (complex *)malloc(sizeof(complex)*nxy*new_nz) ;
                          nvar = (void *) ncar ;
         break ;
      }

      /*-- loop over rows --*/

      for( ii=0 ; ii < nxy ; ii++ ){

         /*-- extract old data into ofz array --*/

         switch( bkind ){
            case MRI_byte:
               for( kk=0 ; kk < old_nz ; kk++ ) ofz[kk] = bar[ii+kk*nxy] ;
            break ;

            case MRI_short:
               for( kk=0 ; kk < old_nz ; kk++ ) ofz[kk] = sar[ii+kk*nxy] ;
            break ;

            case MRI_float:
               for( kk=0 ; kk < old_nz ; kk++ ) ofz[kk] = far[ii+kk*nxy] ;
            break ;

            case MRI_complex:
               for( kk=0 ; kk < old_nz ; kk++ ){
                  ofz[kk]        = car[ii+kk*nxy].r ;
                  ofz[kk+old_nz] = car[ii+kk*nxy].i ;
               }
            break ;
         }

         /*-- interpolate into nfz array --*/

         INTERP_evaluate( imap , ofz , nfz ) ;
         if( bkind == MRI_complex )
            INTERP_evaluate( imap , ofz+old_nz , nfz+new_nz ) ;

         /*-- put into output array --*/

         switch( bkind ){
            case MRI_byte:
               for( kk=0 ; kk < new_nz ; kk++ ) nbar[ii+kk*nxy] = BYTEIZE(nfz[kk]) ;
            break ;

            case MRI_short:
               for( kk=0 ; kk < new_nz ; kk++ ) nsar[ii+kk*nxy] = SHORTIZE(nfz[kk]) ;
            break ;

            case MRI_float:
               for( kk=0 ; kk < new_nz ; kk++ ) nfar[ii+kk*nxy] = nfz[kk] ;
            break ;

            case MRI_complex:
               for( kk=0 ; kk < new_nz ; kk++ ){
                 ncar[ii+kk*nxy].r = nfz[kk] ;
                 ncar[ii+kk*nxy].i = nfz[kk+new_nz] ;
               }
            break ;
         }
      } /* end of loop over rows */

      if( verb ) fprintf(stderr,".") ;

      EDIT_substitute_brick( nset , iv , bkind , nvar ) ;
      DSET_unload_one( dset , iv ) ;

   } /* end of loop over sub-bricks */

   DSET_delete(dset) ; INTERP_destroy(imap) ; free(nfz) ; free(ofz) ;

   DSET_write(nset) ;
   if( verb ) fprintf(stderr,"\n++ Output dataset = %s\n",DSET_BRIKNAME(nset)) ;
   exit(0) ;
}
