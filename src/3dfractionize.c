/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*-- 14 Oct 1999: modified to allow for inverse warping --*/

/*-- 18 Oct 1999: modified to implement -preserve option --*/

THD_fvec3 AFNI_backward_warp_vector( THD_warp * warp , THD_fvec3 old_fv ) ;

int main( int argc , char * argv[] )
{
   char *prefix = "fractionize" ;
   THD_3dim_dataset *tset=NULL , *iset=NULL , *dset=NULL , *wset=NULL ;
   int iarg=1 ;
   int nxin,nyin,nzin , nxyin ;
   float dxin,dyin,dzin , xorgin,yorgin,zorgin , clip=0.0 ;
   int nxout,nyout,nzout , nxyout , nvoxout ;
   float dxout,dyout,dzout , xorgout,yorgout,zorgout ;
   float f1,f2,f , g1,g2,g , h1,h2,h , sx,sy,sz , tx,ty,tz ;
   float x1,x2 , y1,y2 , z1,z2 , xx1,xx2 , yy1,yy2 , zz1,zz2 ;
   int i,ip , j,jp , k,kp , iv,jv,kv , vtype , ijk ;
   float *voxout ;
   byte  *bin   = NULL ;
   short *sin   = NULL , sclip=0   ;
   float *fin   = NULL , fclip=0.00001 ;
   void  *voxin = NULL ;
   THD_fvec3 vv ;

   THD_warp *warp=NULL ;  /* 14 Oct 1999 */

   int do_vote=0 ;          /* 18 Oct 1999 */
   int *vote_val = NULL ;
   int  nvote_val , ivote , voter , vote_print=0 ;
   byte  *vote_bout = NULL ;
   short *vote_sout = NULL ;
   float *vote_best = NULL ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dfractionize [options]\n"
             "\n"
             "* For each voxel in the output dataset, computes the fraction\n"
             "    of it that is occupied by nonzero voxels from the input.\n"
             "* The fraction is stored as a short in the range 0..10000,\n"
             "    indicating fractions running from 0..1.\n"
             "* The template dataset is used only to define the output grid;\n"
             "    its brick(s) will not be read into memory.  (The same is\n"
             "    true of the warp dataset, if it is used.)\n"
             "* The actual values stored in the input dataset are irrelevant,\n"
             "    except in that they are zero or nonzero (UNLESS the -preserve\n"
             "    option is used).\n"
             "\n"
             "The purpose of this program is to allow the resampling of a mask\n"
             "dataset (the input) from a fine grid to a coarse grid (defined by\n"
             "the template).  When you are using the output, you will probably\n"
             "want to threshold the mask so that voxels with a tiny occupancy\n"
             "fraction aren't used.  This can be done in 3dmaskave, by using\n"
             "3calc, or with the '-clip' option below.\n"
             "\n"
             "Options are [the first 2 are 'mandatory options']:\n"
             "  -template tset  = Use dataset 'tset' as a template for the output.\n"
             "                      The output dataset will be on the same grid as\n"
             "                      this dataset.\n"
             "\n"
             "  -input iset     = Use dataset 'iset' for the input.\n"
             "                      Only the sub-brick #0 of the input is used.\n"
             "                      You can use the sub-brick selection technique\n"
             "                      described in '3dcalc -help' to choose the\n"
             "                      desired sub-brick from a multi-brick dataset.\n"
             "\n"
             "  -prefix ppp     = Use 'ppp' for the prefix of the output.\n"
             "                      [default prefix = 'fractionize']\n"
             "\n"
             "  -clip fff       = Clip off voxels that are less than 'fff' occupied.\n"
             "                      'fff' can be a number between 0.0 and 1.0, meaning\n"
             "                      the fraction occupied, can be a number between 1.0\n"
             "                      and 100.0, meaning the percent occupied, or can be\n"
             "                      a number between 100.0 and 10000.0, meaning the\n"
             "                      direct output value to use as a clip level.\n"
             "                   ** Some sort of clipping is desirable; otherwise,\n"
             "                        an output voxel that is barely overlapped by a\n"
             "                        single nonzero input voxel will enter the mask.\n"
             "                      [default clip = 0.0]\n"
             "\n"
             "  -warp wset      = If this option is used, 'wset' is a dataset that\n"
             "                      provides a transformation (warp) from +orig\n"
             "                      coordinates to the coordinates of 'iset'.\n"
             "                      In this case, the output dataset will be in\n"
             "                      +orig coordinates rather than the coordinates\n"
             "                      of 'iset'.  With this option:\n"
             "                   ** 'tset' must be in +orig coordinates\n"
             "                   ** 'iset' must be in +acpc or +tlrc coordinates\n"
             "                   ** 'wset' must be in the same coordinates as 'iset'\n"
             "\n"
             "  -preserve       = When this option is used, the program will copy\n"
             "     or               the nonzero values of input voxels to the output\n"
             "  -vote               dataset, rather than create a fractional mask.\n"
             "                      Since each output voxel might be overlapped\n"
             "                      by more than one input voxel, the program 'votes'\n"
             "                      for which input value to preserve.  For example,\n"
             "                      if input voxels with value=1 occupy 10%% of an\n"
             "                      output voxel, and inputs with value=2 occupy 20%%\n"
             "                      of the same voxel, then the output value in that\n"
             "                      voxel will be set to 2 (provided that 20%% is >=\n"
             "                      to the clip fraction).\n"
             "                   ** Voting can only be done on short-valued datasets,\n"
             "                        or on byte-valued datasets.\n"
             "                   ** Voting is a relatively time-consuming option,\n"
             "                        since a separate loop is made through the\n"
             "                        input dataset for each distinct value found.\n"
             "                   ** Combining this with the -warp option does NOT\n"
             "                        make a general +tlrc to +orig transformer!\n"
             "                        This is because for any value to survive the\n"
             "                        vote, its fraction in the output voxel must be\n"
             "                        >= clip fraction, regardless of other values\n"
             "                        present in the output voxel.\n"
             "\n"
             "Sample usage:\n"
             "\n"
             "  1. Compute the fraction of each voxel occupied by the warped input.\n"
             "\n"
             "          3dfractionize -template grid+orig -input data+tlrc  \\\n"
             "                        -warp anat+tlrc -clip 0.2\n"
             "\n"
             "  2. Apply the (inverse) -warp transformation to transform the -input\n"
             "     from +tlrc space to +orig space, storing it according to the grid\n"
             "     of the -template.\n"
             "     A voxel in the output dataset gets the value that occupies most of\n"
             "     its volume, providing that value occupies 20%% of the voxel.\n"
             "\n"
             "     Note that the essential difference from above is '-preserve'.\n"
             "\n"
             "          3dfractionize -template grid+orig -input data+tlrc  \\\n"
             "                        -warp anat+tlrc -preserve -clip 0.2   \\\n"
             "                        -prefix new_data\n"
             "\n"
             "     Note that 3dAllineate can also be used to warp from +tlrc to +orig\n"
             "     space.  In this case, data is computed through interpolation, rather\n"
             "     than voting based on the fraction of a voxel occupied by each data\n"
             "     value.  The transformation comes from the WARP_DATA attribute directly.\n"
             "     Nearest neighbor interpolation is used in this 'mask' example.\n"
             "\n"
             "         cat_matvec -ONELINE anat+tlrc::WARP_DATA > tlrc.aff12.1D\n"
             "         3dAllineate -1Dmatrix_apply tlrc.aff12.1D -source group_mask+tlrc \\\n"
             "                     -master subj_epi+orig -prefix subj_mask -final NN\n"
             "\n"
             "This program will also work in going from a coarse grid to a fine grid,\n"
             "but it isn't clear that this capability has any purpose.\n"
             "-- RWCox - February 1999\n"
             "         - October 1999: added -warp and -preserve options\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dfractionize main"); machdep();
   AFNI_logger("3dfractionize",argc,argv);
   PRINT_VERSION("3dfractionize"); AUTHOR("RW Cox");

   /*-- read command line args --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-clip") == 0 ){
         clip = strtod( argv[++iarg] , NULL ) ;

              if( clip <= 1.0     ) sclip = (short)(10000.0 * clip + 0.49999) ;
         else if( clip <= 100.0   ) sclip = (short)(  100.0 * clip + 0.49999) ;
         else if( clip <= 10000.0 ) sclip = (short)(clip) ;
         else                       sclip = -1 ;

         if( sclip < 0 || sclip > 10000 ){
            fprintf(stderr,"** Illegal clip value!\n") ; exit(1) ;
         }

         fclip = 0.0001 * sclip ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-template") == 0 || strcmp(argv[iarg],"-tset") == 0 ){
         if( tset != NULL ){
            fprintf(stderr,"** Can't have more than one -template argument!\n") ;
            exit(1) ;
         }
         tset = THD_open_one_dataset( argv[++iarg] ) ;
         if( tset == NULL ){
            fprintf(stderr,"** Can't open template %s\n",argv[iarg]) ;
            exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-input") == 0 || strcmp(argv[iarg],"-iset") == 0 ){
         if( iset != NULL ){
            fprintf(stderr,"** Can't have more than one -input argument!\n") ;
            exit(1) ;
         }
         iset = THD_open_dataset( argv[++iarg] ) ;
         if( iset == NULL ){
            fprintf(stderr,"** Can't open input %s\n",argv[iarg]) ;
            exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-warp") == 0 || strcmp(argv[iarg],"-wset") == 0 ){
         if( wset != NULL ){
            fprintf(stderr,"** Can't have more than one -warp argument!\n") ;
            exit(1) ;
         }
         wset = THD_open_dataset( argv[++iarg] ) ;
         if( wset == NULL ){
            fprintf(stderr,"** Can't open warp %s\n",argv[iarg]) ;
            exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-preserve") == 0 || strcmp(argv[iarg],"-vote") == 0 ){
         do_vote = 1 ;
         iarg++ ; continue ;
      }

      fprintf(stderr,"** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*-- check inputs for sanity --*/

   if( tset == NULL ){
      fprintf(stderr,"** No template dataset?\n") ; exit(1) ;
   }
   if( iset == NULL ){
      fprintf(stderr,"** No input dataset?\n") ; exit(1) ;
   }
   if( !THD_filename_ok(prefix) ){
      fprintf(stderr,"** Illegal prefix?\n") ; exit(1) ;
   }

   /*- 14 Oct 1999: additional checks with the -warp option -*/

   if( wset != NULL ){

      if( tset->view_type != VIEW_ORIGINAL_TYPE ){
         fprintf(stderr,"** Template is not in +orig view - this is illegal!\n");
         exit(1);
      }

      if( iset->view_type == VIEW_ORIGINAL_TYPE ){
         fprintf(stderr,"** Input is in +orig view - this is illegal!\n"); exit(1);
      }

      if( wset != NULL && wset->view_type != iset->view_type ){
         fprintf(stderr,"** Warp and Input are not in same view - this is illegal!\n");
         exit(1);
      }

      warp = wset->warp ;
      if( warp == NULL ){
         fprintf(stderr,"** Warp dataset doesn't contain a warp transformation!\n");
         exit(1) ;
      }
   }

   /*- 18 Oct 1999: check input for legality if we are voting --*/

   if( do_vote ){
     vtype = DSET_BRICK_TYPE(iset,0) ;
     if( vtype != MRI_short && vtype != MRI_byte ){
       fprintf(stderr,"** -preserve option requires short- or byte-valued input dataset!\n");
       exit(1);
     }
   }

   /*-- start to create output dataset --*/

   dset = EDIT_empty_copy( tset ) ;

   tross_Copy_History( iset , dset ) ;                          /* 14 Oct 1999 */
   tross_Make_History( "3dfractionize" , argc,argv , dset ) ;

   EDIT_dset_items( dset ,
                       ADN_prefix    , prefix ,
                       ADN_nvals     , 1 ,
                       ADN_ntt       , 0 ,
                       ADN_brick_fac , NULL ,
                       ADN_datum_all , MRI_short ,
                    ADN_none ) ;

   if( ISFUNC(dset) )
      EDIT_dset_items( dset , ADN_func_type,FUNC_FIM_TYPE , ADN_none ) ;

   if( THD_deathcon() && THD_is_file(dset->dblk->diskptr->header_name) ){
      fprintf(stderr,
              "** Output file %s already exists -- cannot continue!\n",
              dset->dblk->diskptr->header_name ) ;
      exit(1) ;
   }

   DSET_delete( tset ) ;  /* don't need template any more */

   /* load the input dataset */

   DSET_load( iset ) ; CHECK_LOAD_ERROR(iset) ;
   voxin = DSET_ARRAY(iset,0) ; vtype = DSET_BRICK_TYPE(iset,0) ;
   switch( vtype ){
      default: fprintf(stderr,"** Illegal brick type in input dataset!\n"); exit(1);

      case MRI_byte:  bin = (byte * ) voxin ; break ;
      case MRI_short: sin = (short *) voxin ; break ;
      case MRI_float: fin = (float *) voxin ; break ;
   }

   /*-- setup voxel index (iv,jv,kv) to coordinate (x,y,z) calculations --*/

   nxin   =iset->daxes->nxx  ; nyin   =iset->daxes->nyy  ; nzin   =iset->daxes->nzz  ;
   dxin   =iset->daxes->xxdel; dyin   =iset->daxes->yydel; dzin   =iset->daxes->zzdel;
   xorgin =iset->daxes->xxorg; yorgin =iset->daxes->yyorg; zorgin =iset->daxes->zzorg;

   nxout  =dset->daxes->nxx  ; nyout  =dset->daxes->nyy  ; nzout  =dset->daxes->nzz  ;
   dxout  =dset->daxes->xxdel; dyout  =dset->daxes->yydel; dzout  =dset->daxes->zzdel;
   xorgout=dset->daxes->xxorg; yorgout=dset->daxes->yyorg; zorgout=dset->daxes->zzorg;

   /* voxel (i,j,k) center is at (xorg+i*dx,yorg+j*dy,zorg+k*dz) */

   nxyout = nxout * nyout ; nvoxout = nxyout * nzout ;

   voxout = (float *) malloc( sizeof(float) * nvoxout ) ;

   nxyin = nxin * nyin ;

   /*-- if voting, do some setup --*/

   if( do_vote ){
      int nvoxin = nxyin * nzin ;

      /* 1: find all distinct nonzero values in dataset */

      vote_val  = (int *) malloc( sizeof(int) ) ;
      nvote_val = 0 ;
      for( iv=0 ; iv < nvoxin ; iv++ ){
         kv = (vtype == MRI_short) ? sin[iv] : bin[iv] ;   /* voxel value */
         if( kv == 0 ) continue ;                          /* skip zeroes */
         for( jv=0 ; jv < nvote_val ; jv++ )              /* find in list */
            if( vote_val[jv] == kv ) break ;
         if( jv < nvote_val ) continue ;       /* skip if already in list */

         vote_val = (int *) realloc( vote_val , sizeof(int)*(nvote_val+1) ) ;
         vote_val[nvote_val++] = kv ;
      }
      if( nvote_val == 0 ){
         fprintf(stderr,"** Input dataset is all zero!\n") ; exit(1) ;
      }
      fprintf(stderr,"++ Found %d distinct nonzero values in input.\n",nvote_val) ;

      if( nvote_val > 1 )                    /* arrange into ascending value */
         qsort_int( nvote_val , vote_val ) ;

      /* 2: make a receptacle for the voting results */

      if( vtype == MRI_byte ){
          vote_bout = (byte *) malloc( sizeof(byte) * nvoxout ) ;
          memset( vote_bout , 0 , sizeof(byte) * nvoxout ) ;
      } else {
          vote_sout = (short *) malloc( sizeof(short) * nvoxout ) ;
          memset( vote_sout , 0 , sizeof(short) * nvoxout ) ;
      }

      /* 3: make a place to hold the best fraction yet */

      vote_best = (float *) malloc( sizeof(float) * nvoxout ) ;
      for( i=0 ; i < nvoxout ; i++ ) vote_best[i] = 0.0 ;

      /* 4: if needed, attach the correct brick factor to the output */

      f = DSET_BRICK_FACTOR(iset,0) ;
      if( f != 0.0 && f != 1.0 ) EDIT_BRICK_FACTOR(dset,0,f) ;

      /* 5: if input is bytes, make the output be that too */

      if( vtype == MRI_byte )
         EDIT_dset_items( dset , ADN_datum_all , MRI_byte , ADN_none ) ;
   }

   /*-- loop over values to vote on --*/

   ivote = 0 ;
   do{

      if( do_vote ){
         voter = vote_val[ivote] ;  /* who gets to vote this time */
         if( ivote%10 == 9 ){
            if( !vote_print ) fprintf(stderr,"++ Voting:") ;
            fprintf(stderr,"%d",(ivote/10)%10) ;
            vote_print++ ;
         }
      }

      for( i=0 ; i < nvoxout ; i++ ) voxout[i] = 0.0 ;  /* fractions */

      /*-- loop over input voxels --*/

      for( kv=0 ; kv < nzin ; kv++ ){

       z1 = zorgin + dzin * (kv-0.5) ; z2 = zorgin + dzin * (kv+0.49999) ;

       for( jv=0 ; jv < nyin ; jv++ ){

        y1 = yorgin + dyin * (jv-0.5) ; y2 = yorgin + dyin * (jv+0.49999) ;

        for( iv=0 ; iv < nxin ; iv++ ){

         ijk = iv + jv*nxin + kv*nxyin ;  /* 1D index of voxel (iv,jv,kv) */

         /* decide if we use this voxel, based on its value */

         if( do_vote ){
                 if( vtype == MRI_short && sin[ijk] != voter ) continue ;
            else if( vtype == MRI_byte  && bin[ijk] != voter ) continue ;
         } else {
                 if( vtype == MRI_short && sin[ijk] == 0 ) continue ;
            else if( vtype == MRI_byte  && bin[ijk] == 0 ) continue ;
            else if( vtype == MRI_float && fin[ijk] == 0 ) continue ;
         }

         x1 = xorgin + dxin * (iv-0.5) ; x2 = xorgin + dxin * (iv+0.49999) ;

         /* input voxel (iv,jv,kv) spans coordinates [x1,x2] X [y1,y2] X [z1,z2] */

         /* transform these corner coordinates to output dataset grid coordinates */

         LOAD_FVEC3(vv , x1,y1,z1) ;
         vv = THD_3dmm_to_dicomm( iset , vv );        /* transpose from iset to Dicom */
         vv = AFNI_backward_warp_vector( warp , vv ); /* transform to +orig ???*/
         vv = THD_dicomm_to_3dmm( dset , vv );        /* and back from Dicom to dset  */
         UNLOAD_FVEC3(vv , xx1,yy1,zz1) ;

         LOAD_FVEC3(vv , x2,y2,z2) ;
         vv = THD_3dmm_to_dicomm( iset , vv ) ;
         vv = AFNI_backward_warp_vector( warp , vv ) ;
         vv = THD_dicomm_to_3dmm( dset , vv ) ;
         UNLOAD_FVEC3(vv , xx2,yy2,zz2) ;

         /* [xx1,xx2] X [yy1,yy2] X [zz1,zz2] is now in coordinates of output dataset */

         /* compute indices into output dataset voxel (keeping fractions) */

         f1 = (xx1-xorgout)/dxout + 0.49999 ; f2 = (xx2-xorgout)/dxout + 0.49999 ;
         if( f1 > f2 ){ tx = f1 ; f1 = f2 ; f2 = tx ; }
         if( f1 >= nxout || f2 <= 0.0 ) continue ;
         if( f1 < 0.0 ) f1 = 0.0 ;  if( f2 >= nxout ) f2 = nxout - 0.001 ;

         g1 = (yy1-yorgout)/dyout + 0.49999 ; g2 = (yy2-yorgout)/dyout + 0.49999 ;
         if( g1 > g2 ){ ty = g1 ; g1 = g2 ; g2 = ty ; }
         if( g1 >= nyout || g2 <= 0.0 ) continue ;
         if( g1 < 0.0 ) g1 = 0.0 ;  if( g2 >= nyout ) g2 = nyout - 0.001 ;

         h1 = (zz1-zorgout)/dzout + 0.49999 ; h2 = (zz2-zorgout)/dzout + 0.49999 ;
         if( h1 > h2 ){ tz = h1 ; h1 = h2 ; h2 = tz ; }
         if( h1 >= nzout || h2 <= 0.0 ) continue ;
         if( h1 < 0.0 ) h1 = 0.0 ;  if( h2 >= nzout ) h2 = nzout - 0.001 ;

         /* input voxel covers voxels [f1,f2] X [g1,g2] X [h1,h2] in the output */

         /* For example, [6.3,7.2] X [9.3,9.6] X [11.7,13.4], which must be     */
         /* distributed into these voxels:                                      */
         /*  (6,9,11), (7,9,11), (6,9,12), (7,9,12), (6,9,13), and (7,9,13)     */

         for( f=f1 ; f < f2 ; f = ip ){
            i = (int) f ; ip = i+1 ; tx = MIN(ip,f2) ; sx = tx - f ;
            for( g=g1 ; g < g2 ; g = jp ){
               j = (int) g ; jp = j+1 ; ty = MIN(jp,g2) ; sy = ty - g ;
               for( h=h1 ; h < h2 ; h = kp ){
                  k = (int) h ; kp = k+1 ; tz = MIN(kp,h2) ; sz = tz - h ;
                  voxout[ i + j*nxout + k * nxyout ] += sx * sy * sz ;

               }
            }
         }

      }}} /* end of loops over voxels */

      /* at this point, voxout[i] contains the fraction that output voxel [i]
         is occupied by input voxels that were allowed to vote (or were nonzero) */

      /* if not voting, we are done; otherwise, we must tally the count so far */

      if( do_vote ){
         for( i=0 ; i < nvoxout ; i++ ){
            if( voxout[i] > vote_best[i] && voxout[i] >= fclip ){  /* wins */
               vote_best[i] = voxout[i] ;
                    if( vtype == MRI_byte  ) vote_bout[i] = (byte)  voter ;
               else if( vtype == MRI_short ) vote_sout[i] = (short) voter ;
            }
         }
      }

      /* if we are voting, loop back for the next ballot */

   } while( do_vote && ++ivote < nvote_val ) ; /* end of voting loop */

   /*- throw out some trash -*/

   DSET_delete(iset) ;
   if( wset != NULL ) DSET_delete(wset) ;

   if( do_vote ){
      free(voxout) ; free(vote_best) ; free(vote_val) ;
      if( vote_print ) fprintf(stderr,"\n") ;
   }

   /*-- if not voting, compute the output brick --*/

   if( ! do_vote ){
      sin = (short *) malloc( sizeof(short) * nvoxout ) ;
      if( sin == NULL ){
         fprintf(stderr,"** Can't malloc output brick!\n") ; exit(1) ;
      }

      for( i=ijk=0 ; i < nvoxout ; i++ ){
         sin[i] = (short) (10000.0 * voxout[i] + 0.49999) ;
              if( sin[i] < sclip ) sin[i] = 0 ;
         else if( sin[i] > 10000 ) sin[i] = 10000 ;

         if( sin[i] != 0 ) ijk++ ;
      }

      free(voxout) ;
      EDIT_substitute_brick( dset , 0 , MRI_short , sin ) ;

      fprintf(stderr,"-- Writing %d nonzero mask voxels to dataset %s\n",
             ijk , DSET_BRIKNAME(dset) ) ;

   /*-- if voting, output brick will be in vote_bout or vote_sout --*/

   } else {
      if( vtype == MRI_byte ){
         EDIT_substitute_brick( dset , 0 , MRI_byte , vote_bout ) ;
         for( i=ijk=0 ; i < nvoxout ; i++ )
            if( vote_bout[i] != 0 ) ijk++ ;
      } else {
         EDIT_substitute_brick( dset , 0 , MRI_short , vote_sout ) ;
         for( i=ijk=0 ; i < nvoxout ; i++ )
            if( vote_sout[i] != 0 ) ijk++ ;
      }

      fprintf(stderr,"-- Writing %d nonzero voted voxels to dataset %s\n",
             ijk , DSET_BRIKNAME(dset) ) ;
   }

   DSET_write(dset) ;
   exit(0) ;
}

#if 0 /* Now in libmri.a ZSS Feb 2006 */
/*------------------------------------------------------------------------
   Backward transform a vector following a warp
   - lifted from afni.c
   - note that a NULL warp is equivalent to the identity
--------------------------------------------------------------------------*/

THD_fvec3 AFNI_backward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )
{
   THD_fvec3 new_fv ;

   if( warp == NULL ) return old_fv ;

   switch( warp->type ){

      default: new_fv = old_fv ; break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* test if input is in bot..top of each defined map */

         for( iw=0 ; iw < 12 ; iw++ ){
            map = warp->tal_12.warp[iw] ;

            if( old_fv.xyz[0] >= map.bot.xyz[0] &&
                old_fv.xyz[1] >= map.bot.xyz[1] &&
                old_fv.xyz[2] >= map.bot.xyz[2] &&
                old_fv.xyz[0] <= map.top.xyz[0] &&
                old_fv.xyz[1] <= map.top.xyz[1] &&
                old_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
         }
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

   }
   return new_fv ;
}
#endif
