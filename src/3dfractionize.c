#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*-- 14 Oct 1999: modified to allow for inverse warping --*/

THD_fvec3 AFNI_backward_warp_vector( THD_warp * warp , THD_fvec3 old_fv ) ;

int main( int argc , char * argv[] )
{
   char * prefix = "fractionize" ;
   THD_3dim_dataset * tset=NULL , * iset=NULL , * dset=NULL , *wset=NULL ;
   THD_warp * warp=NULL ;
   int iarg=1 ;
   int nxin,nyin,nzin , nxyin ;
   float dxin,dyin,dzin , xorgin,yorgin,zorgin , clip=0.0 ;
   int nxout,nyout,nzout , nxyout , nvoxout ;
   float dxout,dyout,dzout , xorgout,yorgout,zorgout ;
   float f1,f2,f , g1,g2,g , h1,h2,h , sx,sy,sz , tx,ty,tz ;
   float x1,x2 , y1,y2 , z1,z2 , xx1,xx2 , yy1,yy2 , zz1,zz2 ;
   int i,ip , j,jp , k,kp , iv,jv,kv , vtype , ijk ;
   float * voxout ;
   byte  * bin   = NULL ;
   short * sin   = NULL , sclip=0 ;
   float * fin   = NULL ;
   void  * voxin = NULL ;
   THD_fvec3 vv ;

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
             "    except in that they are zero or nonzero.\n"
             "\n"
             "The purpose of this program is to allow the resampling of a mask\n"
             "dataset (the input) from a fine grid to a coarse grid (defined by\n"
             "the template).  When you are using the output, you will probably\n"
             "want to threshold the mask so that voxels with a tiny occupancy\n"
             "fraction aren't used.  This can be done in 3dmaskave, by using\n"
             "3calc, or with the '-clip' option below.\n"
             "\n"
             "Options are [the first 2 are 'required options']:\n"
             "  -template tset  = Use dataset 'tset' as a template for the output.\n"
             "  -input iset     = Use dataset 'iset' for the input.\n"
             "                      Only the sub-brick #0 of the input is used.\n"
             "                      You can use the sub-brick selection technique\n"
             "                      described in '3dcalc -help' to choose the\n"
             "                      desired sub-brick from a multi-brick dataset.\n"
             "  -prefix ppp     = Use 'ppp' for the prefix of the output.\n"
             "                      [default = 'fractionize']\n"
             "  -clip fff       = Clip off voxels that are less than 'fff' occupied.\n"
             "                      'fff' can be a number between 0.0 and 1.0, meaning\n"
             "                      the fraction occupied, can be a number between 1.0\n"
             "                      and 100.0, meaning the percent occupied, or can be\n"
             "                      a number between 100.0 and 10000.0, meaning the\n"
             "                      direct output value to use as a clip level.\n"
             "                      [default = 0.0]\n"
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
             "Example usage:\n"
             " 3dfractionize -template a+orig -input b+tlrc -warp anat+tlrc -clip 0.20\n"
             "\n"
             "This program will also work in going from a coarse grid to a fine grid,\n"
             "but it isn't clear that this capability has any purpose.\n"
             "-- RWCox - February 1999\n"
             "         - October 1999: added -warp option (for Rob Risinger)\n"
            ) ;
      exit(0) ;
   }

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
         fprintf(stderr,"** Template is not in +orig view - this is illegal!\n"); exit(1);
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
         fprintf(stderr,"** Warp dataset does not actually contain a warp transformation!\n");
         exit(1) ;
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

   if( ISFUNC(dset) ) EDIT_dset_items( dset , ADN_func_type,FUNC_FIM_TYPE , ADN_none ) ;

   if( THD_is_file(dset->dblk->diskptr->header_name) ){
      fprintf(stderr,
              "** Output file %s already exists -- cannot continue!\n",
              dset->dblk->diskptr->header_name ) ;
      exit(1) ;
   }

   DSET_delete( tset ) ;
   DSET_load( iset ) ;
   if( ! DSET_LOADED(iset) ){
      fprintf(stderr,"** Can't read input dataset brick!\n") ; exit(1) ;
   }
   voxin = DSET_ARRAY(iset,0) ; vtype = DSET_BRICK_TYPE(iset,0) ;
   switch( vtype ){
      default: fprintf(stderr,"** Illegal brick type in input dataset!\n"); exit(1);

      case MRI_byte:  bin = (byte * ) voxin ; break ;
      case MRI_short: sin = (short *) voxin ; break ;
      case MRI_float: fin = (float *) voxin ; break ;
   }

   /*-- setup --*/

   nxin    = iset->daxes->nxx  ; nyin    = iset->daxes->nyy  ; nzin    = iset->daxes->nzz  ;
   dxin    = iset->daxes->xxdel; dyin    = iset->daxes->yydel; dzin    = iset->daxes->zzdel;
   xorgin  = iset->daxes->xxorg; yorgin  = iset->daxes->yyorg; zorgin  = iset->daxes->zzorg;

   nxout   = dset->daxes->nxx  ; nyout   = dset->daxes->nyy  ; nzout   = dset->daxes->nzz  ;
   dxout   = dset->daxes->xxdel; dyout   = dset->daxes->yydel; dzout   = dset->daxes->zzdel;
   xorgout = dset->daxes->xxorg; yorgout = dset->daxes->yyorg; zorgout = dset->daxes->zzorg;

   /* voxel (i,j,k) center is at (xorg+i*dx,yorg+j*dy,zorg+k*dz) */

   nxyout = nxout * nyout ; nvoxout = nxyout * nzout ;

   voxout = (float *) malloc( sizeof(float) * nvoxout ) ;
   if( voxout == NULL ){
      fprintf(stderr,"** Can't malloc workspace!\n") ; exit(1) ;
   }

   for( i=0 ; i < nvoxout ; i++ ) voxout[i] = 0.0 ;

   nxyin = nxin * nyin ;

   /*-- loop over input voxels --*/

   for( kv=0 ; kv < nzin ; kv++ ){

    z1 = zorgin + dzin * (kv-0.5) ; z2 = zorgin + dzin * (kv+0.49999) ;

    for( jv=0 ; jv < nyin ; jv++ ){

     y1 = yorgin + dyin * (jv-0.5) ; y2 = yorgin + dyin * (jv+0.49999) ;

     for( iv=0 ; iv < nxin ; iv++ ){

      ijk = iv + jv*nxin + kv*nxyin ;
           if( vtype == MRI_short && sin[ijk] == 0 ) continue ;
      else if( vtype == MRI_byte  && bin[ijk] == 0 ) continue ;
      else if( vtype == MRI_float && fin[ijk] == 0 ) continue ;

      x1 = xorgin + dxin * (iv-0.5) ; x2 = xorgin + dxin * (iv+0.49999) ;

      /* input voxel (iv,jv,kv) spans coordinates [x1,x2] X [y1,y2] X [z1,z2] */

#undef DEBUG
#ifdef DEBUG
      printf("\niv=%d jv=%d kv=%d\n",iv,jv,kv) ;
      printf("x1=%f x2=%f  y1=%f y2=%f  z1=%f z2=%f  [iset]\n",
             x1,x2 , y1,y2 , z1,z2 ) ;
#endif

      /* transform these corner coordinates to output dataset grid coordinates */

      LOAD_FVEC3(vv , x1,y1,z1) ;
      vv = THD_3dmm_to_dicomm( iset , vv ) ;        /* transpose from iset to Dicom */
      vv = AFNI_backward_warp_vector( warp , vv ) ; /* transform to +orig ???*/
      vv = THD_dicomm_to_3dmm( dset , vv ) ;        /* and back from Dicom to dset  */
      UNLOAD_FVEC3(vv , xx1,yy1,zz1) ;

      LOAD_FVEC3(vv , x2,y2,z2) ;
      vv = THD_3dmm_to_dicomm( iset , vv ) ;
      vv = AFNI_backward_warp_vector( warp , vv ) ;
      vv = THD_dicomm_to_3dmm( dset , vv ) ;
      UNLOAD_FVEC3(vv , xx2,yy2,zz2) ;

      /* [xx1,xx2] X [yy1,yy2] X [zz1,zz2] is now in coordinates of output dataset */

#ifdef DEBUG
      printf("xx1=%f xx2=%f  yy1=%f yy2=%f  zz1=%f zz2=%f  [dset]\n",
             xx1,xx2 , yy1,yy2 , zz1,zz2 ) ;
#endif

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

#ifdef DEBUG
      printf("f1=%f f2=%f  g1=%f g2=%f  h1=%f h2=%f\n",
             f1,f2 , g1,g2 , h1,h2 ) ;
#endif

      for( f=f1 ; f < f2 ; f = ip ){
         i = (int) f ; ip = i+1 ; tx = MIN(ip,f2) ; sx = tx - f ;
         for( g=g1 ; g < g2 ; g = jp ){
            j = (int) g ; jp = j+1 ; ty = MIN(jp,g2) ; sy = ty - g ;
            for( h=h1 ; h < h2 ; h = kp ){
               k = (int) h ; kp = k+1 ; tz = MIN(kp,h2) ; sz = tz - h ;
               voxout[ i + j*nxout + k * nxyout ] += sx * sy * sz ;

#ifdef DEBUG
               printf("  f=%f g=%f h=%f  sx=%f sy=%f sz=%f  frac=%f\n" ,
                      f,g,h , sx,sy,sz,sx*sy*sz ) ;
#endif
            }
         }
      }
   }}}

   DSET_delete(iset) ;
   if( wset != NULL ) DSET_delete(wset) ;

   /*-- fill the output dataset and blow this burg --*/

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

   printf("-- Writing %d nonzero voxels to dataset %s\n",
          ijk , dset->dblk->diskptr->header_name ) ;

   DSET_write(dset) ;
   exit(0) ;
}

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
