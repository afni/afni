#include "mrilib.h"

/*-------------------------------------------------------------------*/

#undef OV
#undef EP
#undef AN

#define OV(i,j,k) ov[(i)+(j)*ovx+(k)*ovxy]
#define EP(i,j,k) epiar[(i)+(j)*nxepi+(k)*nxyepi]
#define AN(i,j,k) antar[(i)+(j)*nxant+(k)*nxyant]

THD_fvec3 THD_autonudge( THD_3dim_dataset *dsepi, int ivepi,
                         THD_3dim_dataset *dsant, int ivant,
                         float step, int xstep, int ystep, int zstep, int code )
{
   THD_fvec3 fv1,fv2 , fvorg_old,fvorg_new , dxorg ;
   THD_ivec3 iv1,iv2 ;
   float *tar ;
   byte *epiar , *antar ;
   int ii,jj,kk , nxepi,nyepi,nzepi , nxyepi,nxyzepi ;
   int            nxant,nyant,nzant , nxyant,nxyzant ;
   MRI_IMAGE *tim ;
   float xorgepi , yorgepi , zorgepi , xx1,xx2,yy1,yy2,zz1,zz2 ;
   float epiclip , xorgant,yorgant,zorgant , f1,f2,g1,g2,h1,h2 , f,g,h ;
   float tx,ty,tz , dxepi,dyepi,dzepi , dxant,dyant,dzant , z1,z2,y1,y2,x1,x2 ;
   int *ov,*ovp , ovx=2*xstep+1 , ovy=2*ystep+1 , ovz=2*zstep+1 , ovxy=ovx*ovy ;
   int xant,yant,zant , pp,qq,rr , i,j,k , ip,jp,kp , ovtop , kstep ;
   float dxyz_ratio , vsum_thresh , vsum , sx,sy,sz ;
   int verb = ((code & 1) != 0) ;

   /*-- start the action! --*/

ENTRY("THD_autonudge") ;

   /*-- sanity checks --*/

   if( !ISVALID_DSET(dsepi) ||
       !ISVALID_DSET(dsant) ||
       ivepi  < 0           || ivepi >= DSET_NVALS(dsepi) ||
       ivant < 0            || ivant >= DSET_NVALS(dsant) ||
       step <= 0.0 || ovx < 1 || ovy < 1 || ovz < 1 || ovx*ovy*ovz < 3 ){

      fprintf(stderr,"THD_autonudge: bad inputs!\n") ; EXIT(1) ;
   }

   /*-- load chosen sub-brick of epi into local float array --*/

   if( DSET_ARRAY(dsepi,ivepi) == NULL ){
     DSET_load(dsepi) ;
     if( !DSET_LOADED(dsepi) ){
        fprintf(stderr,"THD_autonudge: can't load %s\n",DSET_HEADNAME(dsepi));
        EXIT(1) ;
     }
   }

   nxepi = DSET_NX(dsepi) ;
   nyepi = DSET_NY(dsepi) ; nxyepi  = nxepi  * nyepi ;
   nzepi = DSET_NZ(dsepi) ; nxyzepi = nxyepi * nzepi ;

   tar = (float *) malloc( sizeof(float) * nxyzepi ) ;
   if( tar == NULL ){
      fprintf(stderr,"THD_autonudge: malloc failure for epiar\n"); EXIT(1);
   }

   EDIT_coerce_scale_type( nxyzepi ,
                           DSET_BRICK_FACTOR(dsepi,ivepi) ,
                           DSET_BRICK_TYPE(dsepi,ivepi) ,
                           DSET_ARRAY(dsepi,ivepi) , MRI_float , tar ) ;
   DSET_unload(dsepi) ;

   /*-- clip epi array values --*/

   tim = mri_new_vol_empty( nxepi , nyepi , nzepi , MRI_float ) ;
   mri_fix_data_pointer( tar , tim ) ;
   epiclip = THD_cliplevel( tim , 0.5 ) ;        /* get clip value */
   mri_clear_data_pointer(tim) ; mri_free(tim) ;

   if( epiclip <= 0.0 ){
      fprintf(stderr,"THD_autonudge: can't compute epiclip\n"); EXIT(1);
   }

   if( verb )
     fprintf(stderr,"THD_autonudge: epi clip level=%g\n",epiclip) ;

   epiar = (byte *) malloc(sizeof(byte)*nxyzepi) ;
   if( epiar == 0 ){
      fprintf(stderr,"THD_autonudge: malloc failed for epiar\n"); EXIT(1);
   }

   for( ii=0 ; ii < nxyzepi ; ii++ )      /* mask of supra-clip voxels */
      epiar[ii] = (tar[ii] > epiclip) ;

   free(tar) ;

   /*-- load chosen sub-brick of ant into local float array --*/

   if( DSET_ARRAY(dsant,ivant) == NULL ){
     DSET_load(dsant) ;
     if( !DSET_LOADED(dsant) ){
        fprintf(stderr,"THD_autonudge: can't load %s\n",DSET_HEADNAME(dsant));
        EXIT(1) ;
     }
   }

   nxant = DSET_NX(dsant) ;
   nyant = DSET_NY(dsant) ; nxyant  = nxant  * nyant ;
   nzant = DSET_NZ(dsant) ; nxyzant = nxyant * nzant ;

   tar = (float *) malloc( sizeof(float) * nxyzant ) ;
   if( tar == NULL ){
      fprintf(stderr,"THD_autonudge: malloc failure for antar\n"); EXIT(1);
   }

   EDIT_coerce_scale_type( nxyzant ,
                           DSET_BRICK_FACTOR(dsant,ivant) ,
                           DSET_BRICK_TYPE(dsant,ivant) ,
                           DSET_ARRAY(dsant,ivant) , MRI_float , tar ) ;
   DSET_unload(dsant) ;

   antar = (byte *) malloc(sizeof(byte)*nxyzant) ;
   if( antar == NULL ){
      fprintf(stderr,"THD_autonudge: malloc failure for antar\n"); EXIT(1);
   }

   for( ii=0 ; ii < nxyzant ; ii++ )  /* make mask */
      antar[ii] = (tar[ii] > 0.0) ;

   free(tar) ;

   /*-- find axis in ant that corresponds to x-axis in epi --*/

   LOAD_FVEC3(fv1,0,0,0) ;
   fv1 = THD_3dfind_to_3dmm( dsepi, fv1 ) ; /* coords in dsepi */
   fv1 = THD_3dmm_to_dicomm( dsepi, fv1 ) ; /* DICOM in dsepi  */
   fv1 = THD_dicomm_to_3dmm( dsant, fv1 ) ; /* coords in dsant */
   iv1 = THD_3dmm_to_3dind ( dsant, fv1 ) ; /* index in dsant  */

   LOAD_FVEC3(fv2,nxepi-1,0,0) ;
   fv2 = THD_3dfind_to_3dmm( dsepi , fv2 ) ; /* coords in dsepi */
   fv2 = THD_3dmm_to_dicomm( dsepi , fv2 ) ; /* DICOM in dsepi  */
   fv2 = THD_dicomm_to_3dmm( dsant, fv2 )  ; /* coords in dsant */
   iv2 = THD_3dmm_to_3dind ( dsant, fv2 )  ; /* index in dsant  */

        if( iv1.ijk[0] != iv2.ijk[0] ) xant = 0 ; /* epi x-axis */
   else if( iv1.ijk[1] != iv2.ijk[1] ) xant = 1 ; /* in ant    */
   else if( iv1.ijk[2] != iv2.ijk[2] ) xant = 2 ;
   else {
     fprintf(stderr,"THD_autonudge: incoherent x slicing?!\n");
     DUMP_IVEC3("iv1",iv1); DUMP_IVEC3("iv2",iv2); EXIT(1);
   }

   /*-- find axis in ant that corresponds to y-axis in epi --*/

   LOAD_FVEC3(fv2,0,nyepi-1,0) ;
   fv2 = THD_3dfind_to_3dmm( dsepi, fv2 ) ; /* coords in dsepi */
   fv2 = THD_3dmm_to_dicomm( dsepi, fv2 ) ; /* DICOM in dsepi  */
   fv2 = THD_dicomm_to_3dmm( dsant, fv2 ) ; /* coords in dsant */
   iv2 = THD_3dmm_to_3dind ( dsant, fv2 ) ; /* index in dsant  */

        if( iv1.ijk[0] != iv2.ijk[0] ) yant = 0 ; /* epi y-axis */
   else if( iv1.ijk[1] != iv2.ijk[1] ) yant = 1 ; /* in ant    */
   else if( iv1.ijk[2] != iv2.ijk[2] ) yant = 2 ;
   else {
     fprintf(stderr,"THD_autonudge: incoherent y slicing?!\n");
     DUMP_IVEC3("iv1",iv1); DUMP_IVEC3("iv2",iv2); EXIT(1);
   }

   /*-- find axis in ant that corresponds to z-axis in epi --*/

   LOAD_FVEC3(fv2,0,0,nzepi-1) ;
   fv2 = THD_3dfind_to_3dmm( dsepi, fv2 ) ; /* coords in dsepi */
   fv2 = THD_3dmm_to_dicomm( dsepi, fv2 ) ; /* DICOM in dsepi  */
   fv2 = THD_dicomm_to_3dmm( dsant, fv2 ) ; /* coords in dsant */
   iv2 = THD_3dmm_to_3dind ( dsant, fv2 ) ; /* index in dsant  */

        if( iv1.ijk[0] != iv2.ijk[0] ) zant = 0 ; /* epi z-axis */
   else if( iv1.ijk[1] != iv2.ijk[1] ) zant = 1 ; /* in ant    */
   else if( iv1.ijk[2] != iv2.ijk[2] ) zant = 2 ;
   else {
     fprintf(stderr,"THD_autonudge: incoherent z slicing?!\n");
     DUMP_IVEC3("iv1",iv1); DUMP_IVEC3("iv2",iv2); EXIT(1);
   }

#if 0
   if( verb )
     fprintf(stderr,"  xant=%d yant=%d zant=%d\n",
             xant,yant,zant) ;
#endif

   if( ((1<<xant) | (1<<yant) | (1<<zant)) != 7 ){
      fprintf(stderr,"THD_autonudge: incoherent xyz slicing!\n"
                     "               xant=%d yant=%d zant=%d\n",
              xant,yant,zant) ;
      EXIT(1) ;
   }

   /*-- allocate space for array of overlap counts --*/

   ov = (int *) calloc( sizeof(int) , ovx*ovy*ovz ) ;
   if( ov == NULL ){
      fprintf(stderr,"THD_autonudge: can't malloc space for overlap counts!\n");
      EXIT(1) ;
   }

   /*-- for each origin shift in the xant,yant directions,
        shift origin of ant dataset, then compare to epi dataset --*/

   xorgepi = DSET_XORG(dsepi) ; dxepi = DSET_DX(dsepi) ;
   yorgepi = DSET_YORG(dsepi) ; dyepi = DSET_DY(dsepi) ;
   zorgepi = DSET_ZORG(dsepi) ; dzepi = DSET_DZ(dsepi) ;

   xorgant = DSET_XORG(dsant) ; dxant = DSET_DX(dsant) ;
   yorgant = DSET_YORG(dsant) ; dyant = DSET_DY(dsant) ;
   zorgant = DSET_ZORG(dsant) ; dzant = DSET_DZ(dsant) ;

   dxyz_ratio  = fabs( (dxepi*dyepi*dzepi)/(dxant*dyant*dzant) ) ;
   vsum_thresh = 0.5*dxyz_ratio ;

   LOAD_FVEC3( fvorg_old,xorgant,yorgant,zorgant) ;
   LOAD_FVEC3( dxorg    ,dxant  ,dyant  ,dzant  ) ;

   ovtop = 0 ;
   if( nzepi < 11 ){
      kstep = 2 ;
   } else {
      kstep = (int)(0.2*nzepi+0.5) ;
   }

   for( pp=0 ; pp < ovx ; pp++ ){    /* loop over shifts in 3 directions */
    for( qq=0 ; qq < ovy ; qq++ ){
     for( rr=0 ; rr < ovz ; rr++ ){

     if( verb ) fprintf(stderr,"  starting shift %2d %2d %2d ",
                        pp-xstep,qq-ystep,rr-zstep) ;

     fvorg_new = fvorg_old ;   /* old ant origin */

     fvorg_new.xyz[xant] += (pp-xstep)*step*dxorg.xyz[xant]; /* shift epi-x */
     fvorg_new.xyz[yant] += (qq-ystep)*step*dxorg.xyz[yant]; /* shift epi-y */
     fvorg_new.xyz[zant] += (rr-zstep)*step*dxorg.xyz[zant]; /* shift epi-z */

     xorgant = fvorg_new.xyz[0] ;  /* load new ant origin */
     yorgant = fvorg_new.xyz[1] ;
     zorgant = fvorg_new.xyz[2] ;

     ovp = ov + (pp + qq*ovx + rr*ovxy) ; /* place to store result */

     /*-- foreach voxel in epi dataset,
          find how much of it is filled by nonzero ant voxels --*/

     for( kk=0 ; kk < nzepi ; kk++ ){
      z1 = zorgepi + dzepi*(kk-0.5) ; z2 = zorgepi + dzepi*(kk+0.49999) ;

      if( verb && kk%kstep == 0 ) fprintf(stderr,".") ;

      for( jj=0 ; jj < nyepi ; jj++ ){
       y1 = yorgepi + dyepi*(jj-0.5) ; y2 = yorgepi + dyepi*(jj+0.49999) ;

       for( ii=0 ; ii < nxepi ; ii++ ){
        if( EP(ii,jj,kk) == 0 ) continue ; /* skip voxel */

        x1 = xorgepi + dxepi*(ii-0.5) ; x2 = xorgepi + dxepi*(ii+0.49999) ;

        /* epi voxel covers coords [x1..x2] X [y1..y2] X [z1..z2] */

        /* transform these to ant dataset coords */

        LOAD_FVEC3(fv1,x1,y1,z1) ;                /* coords in epi  */
        fv1 = THD_3dmm_to_dicomm( dsepi, fv1 ) ;  /* DICOM coords   */
        fv1 = THD_dicomm_to_3dmm( dsant, fv1 ) ;  /* coords in ant */
        UNLOAD_FVEC3(fv1,xx1,yy1,zz1) ;

        LOAD_FVEC3(fv2,x2,y2,z2) ;                /* coords in epi  */
        fv2 = THD_3dmm_to_dicomm( dsepi, fv2 ) ;  /* DICOM coords   */
        fv2 = THD_dicomm_to_3dmm( dsant, fv2 ) ;  /* coords in ant */
        UNLOAD_FVEC3(fv2,xx2,yy2,zz2) ;

        /* epi voxel spans ant coords [xx1..xx2] X [yy1..yy2] X [zz1..zz2] */

        /* compute indices into ant dataset voxels */

        f1 = (xx1-xorgant)/dxant + 0.49999 ; f2 = (xx2-xorgant)/dxant + 0.49999 ;
        if( f1 > f2 ){ tx = f1 ; f1 = f2 ; f2 = tx ; }
        if( f1 >= nxant || f2 <= 0.0 ) continue ;
        if( f1 < 0.0 ) f1 = 0.0 ;  if( f2 >= nxant ) f2 = nxant - 0.001 ;

        g1 = (yy1-yorgant)/dyant + 0.49999 ; g2 = (yy2-yorgant)/dyant + 0.49999 ;
        if( g1 > g2 ){ ty = g1 ; g1 = g2 ; g2 = ty ; }
        if( g1 >= nyant || g2 <= 0.0 ) continue ;
        if( g1 < 0.0 ) g1 = 0.0 ;  if( g2 >= nyant ) g2 = nyant - 0.001 ;

        h1 = (zz1-zorgant)/dzant + 0.49999 ; h2 = (zz2-zorgant)/dzant + 0.49999 ;
        if( h1 > h2 ){ tz = h1 ; h1 = h2 ; h2 = tz ; }
        if( h1 >= nzant || h2 <= 0.0 ) continue ;
        if( h1 < 0.0 ) h1 = 0.0 ;  if( h2 >= nzant ) h2 = nzant - 0.001 ;

        /* epi voxel covers voxels [f1..f2] X [g1..g2] X [h1..h2] in ant */

        /* loop over these, and count how much is nonzero */

        vsum = 0.0 ;
        for( f=f1 ; f < f2 ; f = ip ){
         i = (int) f ; ip = i+1 ; tx = MIN(ip,f2) ; sx = tx - f ;
         for( g=g1 ; g < g2 ; g = jp ){
          j = (int) g ; jp = j+1 ; ty = MIN(jp,g2) ; sy = ty - g ;
          for( h=h1 ; h < h2 ; h = kp ){
             k = (int) h ; kp = k+1 ; tz = MIN(kp,h2) ; sz = tz - h ;
             if( AN(i,j,k) ) vsum += sx * sy * sz ;
        }}} /* end of loop over ant voxels */

#if 0
fprintf(stderr," epi=%d %d %d  ant=%6.2f..%6.2f %6.2f..%6.2f %6.2f..%6.2f vsum=%6.2f\n",
        ii,jj,kk , f1,f2 , g1,g2 , h1,h2 , vsum) ;
#endif

        /* add to results for this shift */

        if( vsum > vsum_thresh ) (*ovp)++ ;

      }}} /* end of loop over epi voxels */

      if( verb ) fprintf(stderr," overlap=%d",*ovp) ;
      if( *ovp > ovtop ){
         ovtop = *ovp ; if( verb ) fprintf(stderr," *") ;
      }
      if( verb ) fprintf(stderr,"\n") ;

   }}} /* end of loop over shifts */

   /*-- find best shift in list --*/

   ii = jj = kk = ip = 0 ;

   for( pp=0 ; pp < ovx ; pp++ ){    /* loop over shifts in 3 directions */
    for( qq=0 ; qq < ovy ; qq++ ){
     for( rr=0 ; rr < ovz ; rr++ ){
        if( OV(pp,qq,rr) > ip ){
           ii = pp ; jj = qq ; kk = rr ; ip = OV(pp,qq,rr) ;
        }
   }}}

   fvorg_new.xyz[xant] = (ii-xstep)*step*dxorg.xyz[xant]; /* shift epi-x */
   fvorg_new.xyz[yant] = (jj-ystep)*step*dxorg.xyz[yant]; /* shift epi-y */
   fvorg_new.xyz[zant] = (kk-zstep)*step*dxorg.xyz[zant]; /* shift epi-z */

   if( verb ){
     fprintf(stderr," best shift: %d %d %d overlap=%d\n",
             ii-xstep,jj-ystep,kk-zstep,ip) ;
     DUMP_FVEC3(" best shift",fvorg_new) ;
   }

   free(ov) ; free(antar) ; free(epiar) ;

   RETURN( fvorg_new ) ;
}
