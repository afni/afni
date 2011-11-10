/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "vecmat.h"
#include "mrilib.h"

/***************************************************************************/

#define ERREX(str) (fprintf(stderr,"** %s\n",str),exit(1))

THD_dmat33 DBLE_mat_to_dicomm ( THD_3dim_dataset * ) ;    /* at end of file */

#define ROTATION  1  /* matrix_type options */
#define AFFINE    2
#define ROTSCL    3

/*-------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_dfvec3 *xx , *yy , dv ;
   int nvec=0 , ii,jj, iarg ;
   THD_dvecmat rt , rtinv ;
   THD_dmat33  pp,ppt , rr ;
   THD_dfvec3  tt ;

   THD_3dim_dataset *mset=NULL , *dset=NULL ;
   double *ww=NULL ;
   int     nww=0 ;
   int keeptags=1 , wtval=0 , verb=0 , dummy=0 ;
   char * prefix = "tagalign" , *mfile=NULL ;

   float *fvol , cbot,ctop , dsum ;
   int nval , nvox , clipit , ival ;

   float matar[12] ;

   int use_3dWarp=1 , matrix_type=ROTATION ;

   mainENTRY("3dTagalign main");
   
   /*--- help? ---*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTagalign [options] dset\n"
             "Rotates/translates dataset 'dset' to be aligned with the master,\n"
             "using the tagsets embedded in their .HEAD files.\n"
             "\n"
             "Options:\n"
             " -master mset  = Use dataset 'mset' as the master dataset\n"
             "                   [this is a nonoptional option]\n"
#if 0
             "\n"
             " -wtval        = Use the numerical value attached to each tag\n"
             "                   in the master as weighting factor for that tag\n"
             " -wt1D ff      = Read the *.1D file to use as a vector of weights\n"
             "                   for each tag\n"
             "           N.B.: The default is to weight each tag the same.\n"
             "                   If you use weights, a larger weight means to\n"
             "                   count aligning that tag as more important.\n"
#endif
             "\n"
             " -nokeeptags   = Don't put transformed locations of dset's tags\n"
             "                   into the output dataset [default = keep tags]\n"
             "\n"
             " -matvec mfile = Write the matrix+vector of the transformation to\n"
             "                   file 'mfile'.  This can be used as input to the\n"
             "                   '-matvec_in2out' option of 3dWarp, if you want\n"
             "                   to align other datasets in the same way (e.g.,\n"
             "                   functional datasets).\n"
#if 0
             "           N.B.: The matrix+vector of the transformation is also\n"
             "                   saved in the .HEAD file of the output dataset\n"
             "                   (unless -dummy is used).  You can use the\n"
             "                   output dataset from 3dTagalign as the dataset\n"
             "                   for 3drotates's '-matvec_dset' option; this\n"
             "                   lets you avoid using an intermediate mfile.\n"
             "\n"
             " -3dWarp       = Use the '3dWarp' function to transform the dataset,\n"
             "                   rather than the '3drotate' function.  The output\n"
             "                   dataset will be computed on the same grid as the\n"
             "                   master dataset.\n"
             "           N.B.: This option is implied by '-affine' and '-rotscl'.\n"
#endif
             "\n"
             " -rotate       = Compute the best transformation as a rotation + shift.\n"
             "                   This is the default.\n"
             "\n"
             " -affine       = Compute the best transformation as a general affine\n"
             "                   map rather than just a rotation + shift.  In all\n"
             "                   cases, the transformation from input to output\n"
             "                   coordinates is of the form\n"
             "                      [out] = [R] [in] + [V]\n"
             "                   where [R] is a 3x3 matrix and [V] is a 3-vector.\n"
             "                   By default, [R] is computed as a proper (det=1)\n"
             "                   rotation matrix (3 parameters).  The '-affine'\n"
             "                   option says to fit [R] as a general matrix\n"
             "                   (9 parameters).\n"
             "           N.B.: An affine transformation can rotate, rescale, and\n"
             "                   shear the volume.  Be sure to look at the dataset\n"
             "                   before and after to make sure things are OK.\n"
             "\n"
             " -rotscl       = Compute transformation as a rotation times an isotropic\n"
             "                   scaling; that is, [R] is an orthogonal matrix times\n"
             "                   a scalar.\n"
             "           N.B.: '-affine' and '-rotscl' do unweighted least squares.\n"
             "\n"
             " -prefix pp    = Use 'pp' as the prefix for the output dataset.\n"
             "                   [default = 'tagalign']\n"
             " -verb         = Print progress reports\n"
             " -dummy        = Don't actually rotate the dataset, just compute\n"
             "                   the transformation matrix and vector.  If\n"
             "                   '-matvec' is used, the mfile will be written.\n"
             "\n"
             "Nota Bene:\n"
             "* Cubic interpolation is used.  The transformation is carried out\n"
             "  using the same methods as program 3dWarp.\n"
             "\n"
             "Author: RWCox - 16 Jul 2000, etc.\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*- scan args -*/
   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      /*-----*/

      if( strcmp(argv[iarg],"-rotate") == 0 ){   /* 22 Apr 2003 */
        matrix_type = ROTATION ; use_3dWarp = 1 ;
        iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-affine") == 0 ){   /* 21 Apr 2003 */
        matrix_type = AFFINE ; use_3dWarp = 1 ;
        iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-rotscl") == 0 ){   /* 22 Apr 2003 */
        matrix_type = ROTSCL ; use_3dWarp = 1 ;
        iarg++ ; continue ;
      }

#if 0
      /*-----*/

      if( strcmp(argv[iarg],"-3dWarp") == 0 ){   /* 21 Apr 2003 */
        use_3dWarp = 1 ;
        iarg++ ; continue ;
      }
#endif

      /*-----*/

      if( strcmp(argv[iarg],"-master") == 0 ){
         if( mset != NULL )                    ERREX("Can only have one -master option") ;
         if( ++iarg >= argc )                  ERREX("Need an argument after -master") ;

         mset = THD_open_dataset( argv[iarg] ) ;

         if( mset == NULL )                    ERREX("Can't open -master dataset") ;
         if( mset->tagset == NULL )            ERREX("No tags in -master dataset") ;
         if( TAGLIST_COUNT(mset->tagset) < 3 ) ERREX("Not enough tags in -master dataset") ;

         for( nvec=ii=0 ; ii < TAGLIST_COUNT(mset->tagset) ; ii++ )
            if( TAG_SET(TAGLIST_SUBTAG(mset->tagset,ii)) ) nvec++ ;

         if( nvec < 3 )                        ERREX("Not enough tags set in -master dataset") ;

         if( nvec < TAGLIST_COUNT(mset->tagset) )
            fprintf(stderr,"++ WARNING: not all tags are set in -master dataset\n") ;

         if( verb ) fprintf(stderr,"++ Found %d tags in -master dataset\n",nvec) ;

         iarg++ ; continue ;
      }

#if 0
      /*-----*/

      if( strcmp(argv[iarg],"-wtval") == 0 ){
         if( ww != NULL )                      ERREX("Can't have -wtval after -wt1D") ;
         wtval++ ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-wt1D") == 0 ){
         MRI_IMAGE * wtim ; float * wtar ;

         if( wtval )                           ERREX("Can't have -wt1D after -wtval") ;
         if( ww != NULL )                      ERREX("Can't have two -wt1D options!") ;
         if( ++iarg >= argc )                  ERREX("Need an argument after -wt1D") ;

         wtim = mri_read_1D( argv[iarg] ) ;

         if( wtim == NULL )                    ERREX("Can't read -wtim file") ;
         if( wtim->ny > 1 )                    ERREX("-wtim file has more than one columm") ;

         wtar = MRI_FLOAT_PTR(wtim) ;
         ww   = (double *) malloc(sizeof(double)*wtim->nx) ; nww = wtim->nx ;
         for( ii=0 ; ii < nww ; ii++ ){
            ww[ii] = (double) wtar[ii] ;
            if( ww[ii] < 0.0 )                 ERREX("Negative value found in -wt1D file") ;
         }

         mri_free(wtim) ;
         iarg++ ; continue ;
      }
#endif

      /*-----*/

      if( strcmp(argv[iarg],"-nokeeptags") == 0 ){
         keeptags = 0 ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[iarg],"-verb",5) == 0 ){
         verb++ ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-dummy") == 0 ){
         dummy++ ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         if( ++iarg >= argc )                  ERREX("Need an argument after -prefix") ;
         prefix = argv[iarg] ;
         if( !THD_filename_ok(prefix) )        ERREX("-prefix string is illegal") ;
         iarg++ ; continue ;
      }

      /*-----*/

      if( strcmp(argv[iarg],"-matvec") == 0 ){
         if( ++iarg >= argc )                  ERREX("Need an argument after -matvec") ;
         mfile = argv[iarg] ;
         if( !THD_filename_ok(mfile) )         ERREX("-matvec string is illegal") ;
         iarg++ ; continue ;
      }


      /*-----*/

      fprintf(stderr,"** Unknown option: %s\n",argv[iarg]) ; exit(1) ;

   } /* end of scanning command line for options */

   if( mset == NULL )                    ERREX("No -master option found on command line") ;

#if 0
   if( ww != NULL && nww < nvec )        ERREX("Not enough weights found in -wt1D file") ;

   /*-- if -wtval, setup weights from master tag values --*/

   if( wtval ){
      ww = (double *) malloc(sizeof(double)*nvec) ; nww = nvec ;
      for( ii=jj=0 ; ii < TAGLIST_COUNT(mset->tagset) ; ii++ ){
         if( TAG_SET(TAGLIST_SUBTAG(mset->tagset,ii)) ){
            ww[jj] = (double) TAG_VAL(TAGLIST_SUBTAG(mset->tagset,ii)) ;

            if( ww[jj] < 0.0 )           ERREX("Negative value found in -master tag values") ;
            jj++ ;
         }
      }
   }
#endif

   /*-- read input dataset (to match to master dataset) --*/

   if( iarg >= argc )                    ERREX("No input dataset?") ;

   dset = THD_open_dataset( argv[iarg] ) ;

   if( dset == NULL )                    ERREX("Can't open input dataset") ;
   if( dset->tagset == NULL )            ERREX("No tags in input dataset") ;
   if( TAGLIST_COUNT(dset->tagset) !=
       TAGLIST_COUNT(mset->tagset)   )   ERREX("Tag counts don't match in -master and input") ;

   /* check if set tags match exactly */

   for( ii=0 ; ii < TAGLIST_COUNT(mset->tagset) ; ii++ ){
      if( TAG_SET(TAGLIST_SUBTAG(mset->tagset,ii)) !=
          TAG_SET(TAGLIST_SUBTAG(dset->tagset,ii))    )
                                         ERREX("Set tags don't match in -master and input") ;
   }

   /*-- load vector lists: xx=master, yy=input --*/

   xx = (THD_dfvec3 *) malloc( sizeof(THD_dfvec3) * nvec ) ;
   yy = (THD_dfvec3 *) malloc( sizeof(THD_dfvec3) * nvec ) ;
   dsum = 0.0 ;
   for( ii=jj=0 ; ii < nvec ; ii++ ){
      if( TAG_SET(TAGLIST_SUBTAG(mset->tagset,ii)) ){

         LOAD_DFVEC3( xx[jj] ,                                      /* N.B.:     */
                     TAG_X( TAGLIST_SUBTAG(mset->tagset,ii) ) ,     /* these are */
                     TAG_Y( TAGLIST_SUBTAG(mset->tagset,ii) ) ,     /* in Dicom  */
                     TAG_Z( TAGLIST_SUBTAG(mset->tagset,ii) )  ) ;  /* order now */

         LOAD_DFVEC3( yy[jj] ,
                     TAG_X( TAGLIST_SUBTAG(dset->tagset,ii) ) ,
                     TAG_Y( TAGLIST_SUBTAG(dset->tagset,ii) ) ,
                     TAG_Z( TAGLIST_SUBTAG(dset->tagset,ii) )  ) ;

         dv    = SUB_DFVEC3( xx[jj] , yy[jj] ) ;
         dsum += dv.xyz[0]*dv.xyz[0] + dv.xyz[1]*dv.xyz[1] + dv.xyz[2]*dv.xyz[2] ;

         jj++ ;
      }
   }

   dsum = sqrt(dsum/nvec) ;
   fprintf(stderr,"++ RMS distance between tags before = %.2f mm\n" , dsum ) ;

   /*-- compute best transformation from mset to dset coords --*/

   switch( matrix_type ){
     default:
     case ROTATION:
       rt = DLSQ_rot_trans( nvec , yy , xx , ww ) ;  /* in thd_rot3d.c */
     break ;

     case AFFINE:
       rt = DLSQ_affine   ( nvec , yy , xx ) ;       /* 21 Apr 2003 */
     break ;

     case ROTSCL:
       rt = DLSQ_rotscl   ( nvec , yy , xx , (DSET_NZ(dset)==1) ? 2 : 3 ) ;
     break ;
   }
   rtinv = INV_DVECMAT(rt) ;

   /*-- check for floating point legality --*/

   nval = 0 ;
   for( ii=0 ; ii < 3 ; ii++ ){
      dsum = rt.vv.xyz[ii] ; nval += thd_floatscan(1,&dsum) ;
      for( jj=0 ; jj < 3 ; jj++ ){
         dsum = rt.mm.mat[ii][jj] ; nval += thd_floatscan(1,&dsum) ;
      }
   }
   if( nval > 0 ){
      fprintf(stderr,"** Floating point errors during calculation\n"
                     "** of transform matrix and translation vector\n" ) ;
      exit(1) ;
   }

   /*-- check for rotation matrix legality --*/

   dsum = DMAT_DET(rt.mm) ;

   if( dsum == 0.0 || (matrix_type == ROTATION && fabs(dsum-1.0) > 0.01) ){
     fprintf(stderr,"** Invalid transform matrix computed: tags dependent?\n"
                    "** computed [matrix] and [vector] follow:\n" ) ;

     for( ii=0 ; ii < 3 ; ii++ )
       fprintf(stderr,"  [ %10.5f %10.5f %10.5f ]   [ %10.5f ] \n",
               rt.mm.mat[ii][0],rt.mm.mat[ii][1],rt.mm.mat[ii][2],rt.vv.xyz[ii] );

     exit(1) ;
   }

   /*-- print summary --*/

   if( verb ){
     fprintf(stderr,"++ Matrix & Vector [Dicom: x=R-L; y=A-P; z=I-S]\n") ;
     for( ii=0 ; ii < 3 ; ii++ )
       fprintf(stderr,"    %10.5f %10.5f %10.5f   %10.5f\n",
               rt.mm.mat[ii][0],rt.mm.mat[ii][1],rt.mm.mat[ii][2],rt.vv.xyz[ii] );
   }

   if( matrix_type == ROTATION || matrix_type == ROTSCL ){
     double theta, costheta , dist , fac=1.0 ;

     if( matrix_type == ROTSCL ){
       fac = DMAT_DET(rt.mm); fac = fabs(fac);
       if( DSET_NZ(dset) == 1 ) fac = sqrt(fac) ;
       else                     fac = cbrt(fac) ;
     }

     costheta = 0.5 * sqrt(1.0 + DMAT_TRACE(rt.mm)/fac ) ;
     theta    = 2.0 * acos(costheta) * 180/3.14159265 ;
     dist     = SIZE_DFVEC3(rt.vv) ;

     fprintf(stderr,"++ Total rotation=%.2f degrees; translation=%.2f mm; scaling=%.2f\n",
             theta,dist,fac) ;
   }

   if( mfile ){
      FILE * mp ;

      if( THD_is_file(mfile) )
         fprintf(stderr,"++ Warning: -matvec will overwrite file %s\n",mfile) ;

      mp = fopen(mfile,"w") ;
      if( mp == NULL ){
         fprintf(stderr,"** Can't write to -matvec %s\n",mfile) ;
      } else {
        for( ii=0 ; ii < 3 ; ii++ )
          fprintf(mp,"    %10.5f %10.5f %10.5f   %10.5f\n",
                  rt.mm.mat[ii][0],rt.mm.mat[ii][1],rt.mm.mat[ii][2],rt.vv.xyz[ii] );
        fclose(mp) ;
        if( verb ) fprintf(stderr,"++ Wrote matrix+vector to %s\n",mfile) ;
      }
   }

   if( dummy ){
      fprintf(stderr,"++ This was a -dummy run: no output dataset\n") ; exit(0) ;
   }

   /*-- 21 Apr 2003: transformation can be done the old way (a la 3drotate),
                     or the new way (a la 3dWarp).                          --*/

#if 0
   if( !use_3dWarp ){          /**** the old way ****/

     /*-- now must scramble the rotation matrix and translation
          vector from Dicom coordinate order to dataset brick order --*/

     pp  = DBLE_mat_to_dicomm( dset ) ;
     ppt = TRANSPOSE_DMAT(pp) ;
     rr  = DMAT_MUL(ppt,rt.mm) ; rr = DMAT_MUL(rr,pp) ; tt = DMATVEC(ppt,rt.vv) ;

     /*-- now create the output dataset by screwing with the input dataset
          (this code is adapted from 3drotate.c)                           --*/

     DSET_mallocize(dset) ;
     DSET_load( dset ) ;  CHECK_LOAD_ERROR(dset) ;
     dset->idcode = MCW_new_idcode() ;
     dset->dblk->diskptr->storage_mode = STORAGE_BY_BRICK ; /* 14 Jan 2004 */
     EDIT_dset_items( dset ,
                         ADN_prefix , prefix ,
                         ADN_label1 , prefix ,
                      ADN_none ) ;

     if( !THD_ok_overwrite() && 
         (THD_deathcon() && THD_is_file(dset->dblk->diskptr->header_name) )){
        fprintf(stderr,
                "** Output file %s already exists -- cannot continue!\n",
                dset->dblk->diskptr->header_name ) ;
        exit(1) ;
     }

     tross_Make_History( "3dTagalign" , argc,argv , dset ) ;

     /*-- if desired, keep old tagset --*/

     if( keeptags ){
        THD_dfvec3 rv ;

        dsum = 0.0 ;
        for( jj=ii=0 ; ii < TAGLIST_COUNT(dset->tagset) ; ii++ ){
           if( TAG_SET(TAGLIST_SUBTAG(dset->tagset,ii)) ){
              rv = DMATVEC( rt.mm , yy[jj] ) ;                     /* operating on */
              rv = ADD_DFVEC3( rt.vv , rv ) ;                      /* Dicom order  */

              dv    = SUB_DFVEC3( xx[jj] , rv ) ;
              dsum += dv.xyz[0]*dv.xyz[0] + dv.xyz[1]*dv.xyz[1]
                                          + dv.xyz[2]*dv.xyz[2] ;

              UNLOAD_DFVEC3( rv , TAG_X( TAGLIST_SUBTAG(dset->tagset,ii) ) ,
                                  TAG_Y( TAGLIST_SUBTAG(dset->tagset,ii) ) ,
                                  TAG_Z( TAGLIST_SUBTAG(dset->tagset,ii) )  ) ;

              jj++ ;
           }
        }
        dsum = sqrt(dsum/nvec) ;
        fprintf(stderr,"++ RMS distance between tags after  = %.2f mm\n" , dsum ) ;

     } else {
        myXtFree(dset->tagset) ;  /* send it to the dustbin */
     }

     /*-- rotate sub-bricks --*/

     if( verb ) fprintf(stderr,"++ computing output BRIK") ;

     nvox = DSET_NVOX(dset) ;
     nval = DSET_NVALS(dset) ;
     fvol = (float *) malloc( sizeof(float) * nvox ) ;

     THD_rota_method( MRI_HEPTIC ) ;
     clipit = 1 ;

     for( ival=0 ; ival < nval ; ival++ ){

        /*- get sub-brick out of dataset -*/

        EDIT_coerce_type( nvox ,
                          DSET_BRICK_TYPE(dset,ival),DSET_ARRAY(dset,ival) ,
                          MRI_float,fvol ) ;

        if( clipit ){
           register int ii ; register float bb,tt ;
           bb = tt = fvol[0] ;
           for( ii=1 ; ii < nvox ; ii++ ){
                   if( fvol[ii] < bb ) bb = fvol[ii] ;
              else if( fvol[ii] > tt ) tt = fvol[ii] ;
           }
           cbot = bb ; ctop = tt ;
        }

        if( verb && nval < 5 ) fprintf(stderr,".") ;

        /*- rotate it -*/

        THD_rota_vol_matvec( DSET_NX(dset) , DSET_NY(dset) , DSET_NZ(dset) ,
                             fabs(DSET_DX(dset)) , fabs(DSET_DY(dset)) ,
                                                   fabs(DSET_DZ(dset)) ,
                             fvol , rr , tt ) ;

        if( verb ) fprintf(stderr,".") ;

        if( clipit ){
           register int ii ; register float bb,tt ;
           bb = cbot ; tt = ctop ;
           for( ii=0 ; ii < nvox ; ii++ ){
                   if( fvol[ii] < bb ) fvol[ii] = bb ;
              else if( fvol[ii] > tt ) fvol[ii] = tt ;
           }
        }

        if( verb && nval < 5 ) fprintf(stderr,".") ;

        /*- put it back into dataset -*/

        EDIT_coerce_type( nvox, MRI_float,fvol ,
                                DSET_BRICK_TYPE(dset,ival),DSET_ARRAY(dset,ival) );

     } /* end of loop over sub-brick index */

     if( verb ) fprintf(stderr,":") ;

     /* save matrix+vector into dataset, too */

     UNLOAD_DMAT(rt.mm,matar[0],matar[1],matar[2],
                       matar[4],matar[5],matar[6],
                       matar[8],matar[9],matar[10] ) ;
     UNLOAD_DFVEC3(rt.vv,matar[3],matar[7],matar[11]) ;
     THD_set_atr( dset->dblk, "TAGALIGN_MATVEC", ATR_FLOAT_TYPE, 12, matar ) ;

     /* write dataset to disk */

     dset->dblk->master_nvals = 0 ;  /* in case this was a mastered dataset */
     DSET_write(dset) ;

     if( verb ) fprintf(stderr,"\n") ;

   } else
#endif
   {   /**** the new way: use 3dWarp type transformation ****/

     THD_3dim_dataset *oset ;
     THD_vecmat tran ;

#if 0
     DFVEC3_TO_FVEC3( rt.vv , tran.vv ) ;
     DMAT_TO_MAT    ( rt.mm , tran.mm ) ;
#else
     DFVEC3_TO_FVEC3( rtinv.vv , tran.vv ) ;
     DMAT_TO_MAT    ( rtinv.mm , tran.mm ) ;
#endif

     mri_warp3D_method( MRI_CUBIC ) ;
     oset = THD_warp3D_affine( dset, tran, mset, prefix, 0, WARP3D_NEWDSET ) ;
     if( oset == NULL ){
       fprintf(stderr,"** ERROR: THD_warp3D() fails!\n"); exit(1);
     }

     tross_Copy_History( dset , oset ) ;
     tross_Make_History( "3dTagalign" , argc,argv , oset ) ;

     UNLOAD_DMAT(rt.mm,matar[0],matar[1],matar[2],
                       matar[4],matar[5],matar[6],
                       matar[8],matar[9],matar[10] ) ;
     UNLOAD_DFVEC3(rt.vv,matar[3],matar[7],matar[11]) ;
     THD_set_atr( oset->dblk, "TAGALIGN_MATVEC", ATR_FLOAT_TYPE, 12, matar ) ;

     /*-- if desired, keep old tagset --*/

     if( keeptags ){
        THD_dfvec3 rv ;

        oset->tagset = myXtNew(THD_usertaglist) ;
        *(oset->tagset) = *(dset->tagset) ;

        dsum = 0.0 ;
        for( jj=ii=0 ; ii < TAGLIST_COUNT(oset->tagset) ; ii++ ){
          if( TAG_SET(TAGLIST_SUBTAG(oset->tagset,ii)) ){
            rv = DMATVEC( rt.mm , yy[jj] ) ;
            rv = ADD_DFVEC3( rt.vv , rv ) ;

            dv    = SUB_DFVEC3( xx[jj] , rv ) ;
            dsum += dv.xyz[0]*dv.xyz[0] + dv.xyz[1]*dv.xyz[1]
                                        + dv.xyz[2]*dv.xyz[2] ;

            UNLOAD_DFVEC3( rv , TAG_X( TAGLIST_SUBTAG(oset->tagset,ii) ) ,
                                TAG_Y( TAGLIST_SUBTAG(oset->tagset,ii) ) ,
                                TAG_Z( TAGLIST_SUBTAG(oset->tagset,ii) )  ) ;

            jj++ ;
          }
        }
        dsum = sqrt(dsum/nvec) ;
        fprintf(stderr,"++ RMS distance between tags after  = %.2f mm\n" , dsum ) ;
     }

     DSET_write(oset) ;

   } /* end of 3dWarp-like work */

   exit(0) ;
}

/***********************************************************************/

#include "thd.h"

/*---------------------------------------------------------------------
  This produces a permutation-like matrix that transforms from
  brick axis coordinates to Dicom order coordinates.
-----------------------------------------------------------------------*/

THD_dmat33 DBLE_mat_to_dicomm( THD_3dim_dataset * dset )
{
   THD_dmat33 tod ;

   LOAD_ZERO_DMAT(tod) ;

   switch( dset->daxes->xxorient ){
      case ORI_R2L_TYPE: tod.mat[0][0] =  1.0 ; break ;
      case ORI_L2R_TYPE: tod.mat[0][0] = -1.0 ; break ;
      case ORI_P2A_TYPE: tod.mat[1][0] = -1.0 ; break ;
      case ORI_A2P_TYPE: tod.mat[1][0] =  1.0 ; break ;
      case ORI_I2S_TYPE: tod.mat[2][0] =  1.0 ; break ;
      case ORI_S2I_TYPE: tod.mat[2][0] = -1.0 ; break ;

      default: THD_FATAL_ERROR("illegal xxorient code") ;
   }

   switch( dset->daxes->yyorient ){
      case ORI_R2L_TYPE: tod.mat[0][1] =  1.0 ; break ;
      case ORI_L2R_TYPE: tod.mat[0][1] = -1.0 ; break ;
      case ORI_P2A_TYPE: tod.mat[1][1] = -1.0 ; break ;
      case ORI_A2P_TYPE: tod.mat[1][1] =  1.0 ; break ;
      case ORI_I2S_TYPE: tod.mat[2][1] =  1.0 ; break ;
      case ORI_S2I_TYPE: tod.mat[2][1] = -1.0 ; break ;

      default: THD_FATAL_ERROR("illegal yyorient code") ;
   }

   switch( dset->daxes->zzorient ){
      case ORI_R2L_TYPE: tod.mat[0][2] =  1.0 ; break ;
      case ORI_L2R_TYPE: tod.mat[0][2] = -1.0 ; break ;
      case ORI_P2A_TYPE: tod.mat[1][2] = -1.0 ; break ;
      case ORI_A2P_TYPE: tod.mat[1][2] =  1.0 ; break ;
      case ORI_I2S_TYPE: tod.mat[2][2] =  1.0 ; break ;
      case ORI_S2I_TYPE: tod.mat[2][2] = -1.0 ; break ;

      default: THD_FATAL_ERROR("illegal zxorient code") ;
   }

   return tod ;
}
