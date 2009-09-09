/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---------------------------------------------------------------------*/
/*! Make a byte mask from mask dataset:
     miv = sub-brick of input
     if( mask_bot <= mask_top ) then
       only nonzero values in this range will be used
     else
       all nonzero values in the mask will be used
   The input dataset should be byte-, short-, or float-valued.

   The output is a byte array with 1s in "hit" locations and 0s in
   other locations.  The number of bytes is DSET_NVOX(mask_dset).
   This array should be free()-d someday.  If NULL is returned,
   some grotesque error transpired.
-----------------------------------------------------------------------*/

byte * THD_makemask( THD_3dim_dataset *mask_dset ,
                     int miv , float mask_bot , float mask_top )
{
   float maxval ;  /* for computing limits for an empty mask */
   byte *mmm = NULL ;
   int nvox , ii ;
   int empty = 0 ; /* do we return an empty mask */

   if( !ISVALID_DSET(mask_dset)    ||
       miv < 0                     ||
       miv >= DSET_NVALS(mask_dset)  ) return NULL ;

   nvox = DSET_NVOX(mask_dset) ;

   DSET_load(mask_dset) ; if( !DSET_LOADED(mask_dset) ) return NULL ;

   mmm = (byte *) calloc( sizeof(byte) * nvox , 1 ) ;

   switch( DSET_BRICK_TYPE(mask_dset,miv) ){
      default:
         free(mmm) ; DSET_unload(mask_dset) ; return NULL ;

      case MRI_short:{
         short mbot , mtop ;
         short *mar = (short *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            /* maybe this mask is empty, allow for rounding */
            maxval = MRI_TYPE_maxval[MRI_short] + 0.5 ;
            if( mask_bot/mfac >= maxval || mask_top/mfac <= -maxval ) empty=1;

            mbot = SHORTIZE(mask_bot/mfac) ;
            mtop = SHORTIZE(mask_top/mfac) ;
         } else {
            mbot = (short) -MRI_TYPE_maxval[MRI_short] ;
            mtop = (short)  MRI_TYPE_maxval[MRI_short] ;
         }
         if( !empty )   /* 6 Jun 2007 */
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 )
                  mmm[ii]=1;
      }
      break ;

      case MRI_byte:{
         byte mbot , mtop ;
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top && mask_top > 0.0 ){
            /* maybe this mask is empty, allow for rounding */
            /* (top <= 0 is flag for full mask)             */
            maxval = MRI_TYPE_maxval[MRI_byte] + 0.5 ;
            if( mask_bot/mfac >= maxval ) empty = 1;

            mbot = BYTEIZE(mask_bot/mfac) ;
            mtop = BYTEIZE(mask_top/mfac) ;
         } else {
            mbot = 0 ;
            mtop = (byte) MRI_TYPE_maxval[MRI_short] ;
         }
         if( !empty )   /* 6 Jun 2007 */
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 )
                  mmm[ii]=1;
      }
      break ;

      case MRI_float:{
         float mbot , mtop ;
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            mbot = (float) (mask_bot/mfac) ;
            mtop = (float) (mask_top/mfac) ;
         } else {
            mbot = -WAY_BIG ;
            mtop =  WAY_BIG ;
         }
         for( ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ) mmm[ii]=1;
      }
      break ;
   }

   return mmm ;
}

/*----------------------------------------------------------------------------*/
/*! Remove isolated voxels from a byte mask [21 May 2009 - RWCox]. */

int THD_mask_remove_isolas( int nx, int ny, int nz , byte *mmm )
{
   int ii,jj,kk , ip,im , jp,jm , kp,km , qq,nxy , niso=0 ;

   if( nx < 1 || ny < 1 || nz < 1 || mmm == NULL ) return 0 ;
   nxy = nx*ny ;

   for( qq=kk=0 ; kk < nz ; kk++ ){  /* qq = voxel index in 1D array */
     km = kk-1 ; kp = kk+1 ;
     for( jj=0 ; jj < ny ; jj++ ){
       jm = jj-1 ; jp = jj+1 ;
       for( ii=0 ; ii < nx ; ii++,qq++ ){
         if( !mmm[qq] ) continue ;              /* already 0 */
         im = ii-1 ; ip = ii+1 ;
         if( im >= 0 && mmm[qq-1]   ) continue ; /* -x nbhr */
         if( ip < nx && mmm[qq+1]   ) continue ; /* +x     */
         if( jm >= 0 && mmm[qq-nx]  ) continue ; /* -y    */
         if( jp < ny && mmm[qq+nx]  ) continue ; /* +y   */
         if( km >= 0 && mmm[qq-nxy] ) continue ; /* -z  */
         if( kp < nz && mmm[qq+nxy] ) continue ; /* +z */
         mmm[qq] = 0 ; niso++ ;
   }}}
   return niso ;
}

/*----------------------------------------------------------------------------*/
/*!
   Similar to THD_makemask except that it turns the dset itself to mask values
   returns (-1) if it fails, number of non-zero voxels if OK
*/

int THD_makedsetmask( THD_3dim_dataset *mask_dset ,
                     int miv , float mask_bot , float mask_top,
                     byte *cmask )
{
   float maxval ;  /* for computing limits for an empty mask */
   int nvox , ii, nonzero=-1 , empty = 0 ;

   if( !ISVALID_DSET(mask_dset)    ||
       miv < 0                     ||
       miv >= DSET_NVALS(mask_dset)  ) return (-1) ;

   nvox = DSET_NVOX(mask_dset) ;

   DSET_mallocize(mask_dset); /* do this or else it could be a read only dset! */
   DSET_load(mask_dset) ; if( !DSET_LOADED(mask_dset) ) return (-1) ;

   nonzero = 0;
   switch( DSET_BRICK_TYPE(mask_dset,miv) ){
      default:
         DSET_unload(mask_dset) ; return (-1) ;

      case MRI_short:{
         short mbot , mtop ;
         short *mar = (short *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            /* maybe this mask is empty, allow for rounding */
            maxval = MRI_TYPE_maxval[MRI_short] + 0.5 ;
            if( mask_bot/mfac >= maxval || mask_top/mfac <= -maxval ) empty=1;

            mbot = SHORTIZE(mask_bot/mfac) ;
            mtop = SHORTIZE(mask_top/mfac) ;
         } else {
            mbot = (short) -MRI_TYPE_maxval[MRI_short] ;
            mtop = (short)  MRI_TYPE_maxval[MRI_short] ;
         }
         if (empty) {  /* if empty, clear result   6 Jun 2007 */
            for( ii=0 ; ii < nvox ; ii++ ) mar[ii] = 0;
         } else if (cmask)  {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 && cmask[ii]) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         }
      }
      break ;

      case MRI_byte:{
         byte mbot , mtop ;
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top && mask_top > 0.0 ){
            /* maybe this mask is empty, allow for rounding */
            /* (top <= 0 is flag for full mask)             */
            maxval = MRI_TYPE_maxval[MRI_byte] + 0.5 ;
            if( mask_bot/mfac >= maxval ) empty = 1;

            mbot = BYTEIZE(mask_bot/mfac) ;
            mtop = BYTEIZE(mask_top/mfac) ;
         } else {
            mbot = 0 ;
            mtop = (byte) MRI_TYPE_maxval[MRI_short] ;
         }
         if (empty) {  /* if empty, clear result   6 Jun 2007 */
            for( ii=0 ; ii < nvox ; ii++ ) mar[ii] = 0;
         } else if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 && cmask[ii]){ mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         }
      }
      break ;

      case MRI_float:{
         float mbot , mtop ;
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            mbot = (float) (mask_bot/mfac) ;
            mtop = (float) (mask_top/mfac) ;
         } else {
            mbot = -WAY_BIG ;
            mtop =  WAY_BIG ;
         }
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 && cmask[ii]) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         }
      }
      break ;
   }

   /* remove any scaling factor ZSS April 24 06*/
   EDIT_BRICK_FACTOR(mask_dset,miv , 0.0);

   return (nonzero) ;
}

/*----------------------------------------------------------------------------*/
/*!
   Returns a list of the unique values in a dataset.
*/

extern int * UniqueInt (int *y, int ysz, int *kunq, int Sorted );
int *THD_unique_vals( THD_3dim_dataset *mask_dset ,
                        int miv,
                        int *n_unique ,
                        byte *cmask)
{
   int nvox , ii, *unq = NULL, *vals=NULL;

   *n_unique = 0;
   unq = NULL ;

   if( !ISVALID_DSET(mask_dset)    ||
       miv < 0                     ||
       miv >= DSET_NVALS(mask_dset)  ) {

      fprintf(stderr,"** Bad mask_dset or sub-brick index.\n");
      return (unq) ;

   }
   nvox = DSET_NVOX(mask_dset) ;

   DSET_load(mask_dset) ; if( !DSET_LOADED(mask_dset) ) return (unq) ;

   vals = (int *)malloc(sizeof(int)*nvox);
   if (!vals) {
      fprintf(stderr,"** Failed to allocate.\n");
      return (unq) ;
   }

   switch( DSET_BRICK_TYPE(mask_dset,miv) ){
      default:
         fprintf(stderr,"** Bad dset type for unique operation.\nOnly Byte and Short dsets are allowed.\n");
         DSET_unload(mask_dset) ; if (vals) free(vals); return (unq) ;

      case MRI_short:{
         short *mar = (short *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;

         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]*mfac); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]*mfac);
         }

      }
      break ;

      case MRI_byte:{
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;

         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]*mfac); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]*mfac);
         }

      }
      break ;

      #if 1 /* bad idea, but necessary in certain cases. We store ints (from NIFTI) as floats.*/
      case MRI_float:{
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;

         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]*mfac); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]*mfac);
         }

      }
      break ;
      #endif
   }

   /* unique */
   unq = UniqueInt (vals, nvox, n_unique, 0 );

   free(vals); vals = NULL;

   return (unq) ;
}

/*----------------------------------------------------------------------------*/
/* returns an nvox int array which represents
the rank of the voxel value in mask_dset
*/

int *THD_unique_rank( THD_3dim_dataset *mask_dset ,
                        int miv,
                        byte *cmask,
                        char *mapname)
{
   int nvox , ii, *unq = NULL, *vals=NULL, *rmap=NULL, imax=0;
   int n_unique, r;
   FILE *fout=NULL;

   n_unique = 0;
   unq = NULL ;

   if( !ISVALID_DSET(mask_dset)    ||
       miv < 0                     ||
       miv >= DSET_NVALS(mask_dset)  ) {

      fprintf(stderr,"** Bad mask_dset or sub-brick index.\n");
      return (vals) ;

   }
   nvox = DSET_NVOX(mask_dset) ;

   DSET_load(mask_dset) ; if( !DSET_LOADED(mask_dset) ) return (vals) ;

   vals = (int *)malloc(sizeof(int)*nvox);
   if (!vals) {
      fprintf(stderr,"** Failed to allocate.\n");
      return (vals) ;
   }

   switch( DSET_BRICK_TYPE(mask_dset,miv) ){
      default:
         fprintf( stderr,
                  "** Bad dset type for unique operation.\n"
                  "Only Byte, Short and float dsets are allowed.\n");
         DSET_unload(mask_dset) ;
         if (vals) free(vals); vals = NULL; return (vals) ;

      case MRI_short:{
         short *mar = (short *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;

         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]*mfac); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]*mfac);
         }

      }
      break ;

      case MRI_byte:{
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;

         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]*mfac); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]*mfac);
         }

      }
      break ;

      case MRI_float:{ /* not an integral type but we store ints (from NIFTI)
                          as floats */
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;

         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]*mfac); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]*mfac);
         }

      }
      break ;
   }

   /* unique */
   unq = UniqueInt (vals, nvox, &n_unique, 0 );
   /* fprintf(stderr,"-- Have %d unique values\n", n_unique); */
   if (!unq) {
      fprintf(stderr,"** Failed to create unique list\n");
      free(vals); return (NULL);
   }

   /*fprintf(stderr,"-- Writing mapping to >>%s<<\n", mapname);*/
   if (mapname[0]) {
      if ((fout = fopen(mapname,"w"))) {
         fprintf(fout, "#Rank Map (%d unique values)\n", n_unique);
         fprintf(fout, "#Col. 0: Rank\n");
         fprintf(fout, "#Col. 1: Input Dset Value\n");
      }
   }
   /* now replace by rank */
   #if 0
   for (r=0; r<n_unique; ++r) {
      /* fprintf(stderr,"-- Doing %d ...\n", unq[r]); */
      if (cmask) {
         for (ii=0; ii<nvox; ii++) {
            if (cmask[ii]) {
               if (vals[ii] == unq[r]) vals[ii] = r;
            } else vals[ii] = 0;
         }
      } else {
         for( ii=0 ; ii < nvox ; ii++ ) if (vals[ii] == unq[r]) vals[ii] = r;
      }
   }
   #else /* faster approach */
   imax=0;
   for (r=0; r<n_unique; ++r) if (imax < unq[r]) imax = unq[r];
   if (!(rmap=(int*)calloc(imax+1, sizeof(int)))) {
      fprintf(stderr,"** Failed to allocate\n");
      free(vals); free(unq); return (NULL);
   }
   for (r=0; r<n_unique; ++r) {
      rmap[unq[r]] = r;
      if (fout) fprintf(fout, "%d   %d\n", r, unq[r]);
   }
   for (ii=0; ii<nvox; ii++)
      if (!cmask || cmask[ii]) vals[ii] = rmap[vals[ii]];
   free(rmap); rmap=NULL;
   #endif

   free(unq); unq = NULL;
   if (fout) fclose(fout); fout = NULL;

   return (vals) ;
}

/*----------------------------------------------------------------------------*/
/* Same as THD_unique_rank but replaces values in mask_dset with rank */

int THD_unique_rank_edit( THD_3dim_dataset *mask_dset ,
                           int miv,
                           byte *cmask,
                           char *mapname)
{
   int *vals=NULL, nvox, mxval, ii;

   if (!(vals = THD_unique_rank(mask_dset, miv, cmask, mapname))) {
      fprintf(stderr,"** Failed to uniquate\n");
      return (0);
   }

   mxval = -1;
   nvox = DSET_NVOX(mask_dset) ;
   for( ii=0 ; ii < nvox ; ii++ ) { if (vals[ii] > mxval) mxval = vals[ii]; }
   /* fprintf (stderr,"-- Have maxval of %d\n", mxval); */

   switch( DSET_BRICK_TYPE(mask_dset,miv) ){
      default:
         fprintf(stderr,"** Bad dset type for unique operation.\n"
                        "Should have been stopped a while ago.\n");
         if (vals) free(vals); vals = NULL; return (0) ;

      case MRI_short:{
         short *mar = (short *) DSET_ARRAY(mask_dset,miv) ;
         if (mxval > MRI_TYPE_maxval[MRI_short]) {
            fprintf(stderr,
                    "** Have too many unique values (%d) for "
                    "datatype short (limit %f)!\n",
                    mxval, MRI_TYPE_maxval[MRI_short]);
            if (vals) free(vals); vals = NULL; return (0) ;
         }
         EDIT_BRICK_FACTOR(mask_dset,miv,0.0);
         for( ii=0 ; ii < nvox ; ii++ )
            mar[ii] = (short)(vals[ii]);
      }
      break ;

      case MRI_byte:{
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         if (mxval > MRI_TYPE_maxval[MRI_byte]) {
            fprintf(stderr,
                    "** Have too many unique values (%d) for "
                    "datatype byte (limit %f)!\n",
                    mxval, MRI_TYPE_maxval[MRI_byte]);
            if (vals) free(vals); vals = NULL; return (0) ;
         }
         EDIT_BRICK_FACTOR(mask_dset,miv,0.0);
         for( ii=0 ; ii < nvox ; ii++ )
            mar[ii] = (byte)(vals[ii]);
      }
      break ;

      case MRI_float:{
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         EDIT_BRICK_FACTOR(mask_dset,miv,0.0);
         for( ii=0 ; ii < nvox ; ii++ )
            mar[ii] = (float)(vals[ii]);
      }
      break ;
   }

   return (1);

}

/*---------------------------------------------------------------------*/
/*! Count the number of nonzero voxels in a mask.
-----------------------------------------------------------------------*/

int THD_countmask( int nvox , byte *mmm )
{
   int ii,mc ;

   if( nvox <= 0 || mmm == NULL ) return 0 ;

   for( ii=mc=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) mc++ ;

   return mc ;
}

/*---------------------------------------------------------------------*/

int THD_parse_boxball( int *boxball_num , float **boxball_dat , char **argv )
{
   int bnum , narg=0 ; float *bdat ;

   if( boxball_num == NULL || boxball_dat == NULL || argv == NULL ) return 0 ;

   bnum = *boxball_num ; if( bnum < 0 ) bnum = 0 ;
   bdat = *boxball_dat ;

   if( strcmp(argv[narg]+2,"box") == 0 ){
     float xbot,xtop , ybot,ytop , zbot,ztop , btyp ; int nn ;
     char code = *(argv[narg]+1) ;   /* should be 'x', 'd' , 'n', or 'i' */
     switch( code ){
       case 'x': btyp = BOX_XYZ ; break ;
       case 'd': btyp = BOX_DIC ; break ;
       case 'n': btyp = BOX_NEU ; break ;
       case 'i': btyp = BOX_IJK ; break ;
       default:  WARNING_message("Unknown 'box' option %s\n",argv[narg]) ; return 0 ;
     }
     nn = sscanf( argv[narg+1] , "%f:%f" , &xbot , &xtop ) ;
     if( nn < 1 ){
       WARNING_message("Can't decode %s after %s\n",argv[narg+1],argv[narg]); return 0 ;
     }
     else if( nn == 1 ) xtop=xbot ;
     nn = sscanf( argv[narg+2] , "%f:%f" , &ybot , &ytop ) ;
     if( nn < 1 ){
       WARNING_message("Can't decode %s after %s\n",argv[narg+2],argv[narg]); return 0 ;
     }
     else if( nn == 1 ) ytop=ybot ;
     nn = sscanf( argv[narg+3] , "%f:%f" , &zbot , &ztop ) ;
     if( nn < 1 ){
       WARNING_message("Can't decode %s after %s\n",argv[narg+3],argv[narg]); return 0 ;
     }
     else if( nn == 1 ) ztop=zbot ;
     bdat = (float *) realloc( bdat , sizeof(float)*BOXLEN*(bnum+1) ) ;
     bdat[0+BOXLEN*bnum] = btyp ;
     bdat[1+BOXLEN*bnum] = xbot ;
     bdat[2+BOXLEN*bnum] = xtop ;
     bdat[3+BOXLEN*bnum] = ybot ;
     bdat[4+BOXLEN*bnum] = ytop ;
     bdat[5+BOXLEN*bnum] = zbot ;
     bdat[6+BOXLEN*bnum] = ztop ;
     bnum++ ; narg = 4 ;

   } else if( strcmp(argv[narg]+2,"ball") == 0 ){
     float xcen,ycen,zcen,rad , btyp ;
     char code = *(argv[narg]+1) ;   /* should be 'x', 'd' , or 'n' */
     switch( code ){
       case 'x': btyp = BALL_XYZ ; break ;
       case 'd': btyp = BALL_DIC ; break ;
       case 'n': btyp = BALL_NEU ; break ;
       default:  WARNING_message("Unknown 'ball' option %s",argv[narg]) ; return 0 ;
     }
     xcen = strtod( argv[narg+1] , NULL ) ;
     ycen = strtod( argv[narg+2] , NULL ) ;
     zcen = strtod( argv[narg+3] , NULL ) ;
     rad  = strtod( argv[narg+4] , NULL ) ;
     if( rad <= 0.0f ){
       WARNING_message("%s radius=%s !?",argv[narg],argv[narg+4]) ; rad = 0.0f;
     }

     bdat = (float *) realloc( bdat , sizeof(float)*BOXLEN*(bnum+1) ) ;
     bdat[0+BOXLEN*bnum] = btyp ;
     bdat[1+BOXLEN*bnum] = xcen ;
     bdat[2+BOXLEN*bnum] = ycen ;
     bdat[3+BOXLEN*bnum] = zcen ;
     bdat[4+BOXLEN*bnum] = rad  ;
     bnum++ ; narg = 5 ;
   }

   *boxball_num = bnum ; *boxball_dat = bdat ; return narg ;
}

/*----------------------------------------------------------------------------*/

byte * THD_boxballmask( THD_3dim_dataset *dset ,
                        int boxball_num , float *boxball_dat )
{
   int nx,ny,nz , nxy,nxyz , ii,jj,kk ;
   byte *bmask ;
   int bb, ibot,itop, jbot,jtop, kbot,ktop , btyp ;
   float xbot,xtop, ybot,ytop, zbot,ztop ;
   float xcen,ycen,zcen , icen,jcen,kcen ;
   float xmin,xmax , ymin,ymax , zmin,zmax , rad,dist , xx,yy,zz ;
   THD_fvec3 dv,xv ;

ENTRY("THD_boxballmask") ;

   if( !ISVALID_DSET(dset) || boxball_num <= 0 || boxball_dat == NULL ) RETURN(NULL) ;

   xmin=dset->daxes->xxmin ; xmax=dset->daxes->xxmax ;
   ymin=dset->daxes->yymin ; ymax=dset->daxes->yymax ;
   zmin=dset->daxes->zzmin ; zmax=dset->daxes->zzmax ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy  = nx*ny ;
   nz = DSET_NZ(dset) ; nxyz = nxy*nz ;

   bmask = (byte *)calloc(sizeof(byte),nxyz) ;

   for( bb=0 ; bb < boxball_num ; bb++ ){

     btyp = boxball_dat[0+BOXLEN*bb] ;

     if( btyp < BALL_XYZ ){  /*---- box ----*/

       xbot = boxball_dat[1+BOXLEN*bb]; xtop = boxball_dat[2+BOXLEN*bb];
       ybot = boxball_dat[3+BOXLEN*bb]; ytop = boxball_dat[4+BOXLEN*bb];
       zbot = boxball_dat[5+BOXLEN*bb]; ztop = boxball_dat[6+BOXLEN*bb];

       if( btyp != BOX_IJK ){            /* convert coords to indexes */

         if( btyp == BOX_NEU ){          /* coords from Neuroscience to DICOM */
           xbot = -xbot; xtop = -xtop; ybot = -ybot; ytop = -ytop; btyp = BOX_DIC;
         }
         if( btyp == BOX_DIC ){          /* coords from DICOM to dataset */
           LOAD_FVEC3(dv,xbot,ybot,zbot) ;
           xv = THD_dicomm_to_3dmm( dset , dv ) ;
           UNLOAD_FVEC3(xv,xbot,ybot,zbot) ;
           LOAD_FVEC3(dv,xtop,ytop,ztop) ;
           xv = THD_dicomm_to_3dmm( dset , dv ) ;
           UNLOAD_FVEC3(xv,xtop,ytop,ztop) ;
         }
         if( xbot < xmin && xtop < xmin ) continue ; /* skip box if outside dataset */
         if( xbot > xmax && xtop > xmax ) continue ;
         if( ybot < ymin && ytop < ymin ) continue ;
         if( ybot > ymax && ytop > ymax ) continue ;
         if( zbot < zmin && ztop < zmin ) continue ;
         if( zbot > zmax && ztop > zmax ) continue ;
         LOAD_FVEC3(dv,xbot,ybot,zbot) ;
         xv = THD_3dmm_to_3dfind( dset , dv ) ;   /* coords from dataset to index */
         UNLOAD_FVEC3(xv,xbot,ybot,zbot) ;
         LOAD_FVEC3(dv,xtop,ytop,ztop) ;
         xv = THD_3dmm_to_3dfind( dset , dv ) ;
         UNLOAD_FVEC3(xv,xtop,ytop,ztop) ;
       }
       ibot = rint(xbot) ; jbot = rint(ybot) ; kbot = rint(zbot) ;  /* round */
       itop = rint(xtop) ; jtop = rint(ytop) ; ktop = rint(ztop) ;
       if( ibot > itop ){ btyp = ibot; ibot = itop; itop = btyp; }  /* flip? */
       if( jbot > jtop ){ btyp = jbot; jbot = jtop; jtop = btyp; }
       if( kbot > ktop ){ btyp = kbot; kbot = ktop; ktop = btyp; }

       /* skip box if outside dataset */
       if ( itop < 0 || ibot >= nx ) continue;
       if ( jtop < 0 || jbot >= ny ) continue;
       if ( ktop < 0 || kbot >= nz ) continue;

       /* constrain values to dataset dimensions */
       if ( ibot < 0 ) ibot = 0;  if ( itop >= nx ) itop = nx-1;
       if ( jbot < 0 ) jbot = 0;  if ( jtop >= ny ) jtop = ny-1;
       if ( kbot < 0 ) kbot = 0;  if ( ktop >= nz ) ktop = nz-1;

       for( kk=kbot ; kk <= ktop ; kk++ )
        for( jj=jbot ; jj <= jtop ; jj++ )
         for( ii=ibot ; ii <= itop ; ii++ ) bmask[ii+jj*nx+kk*nxy] = 1 ;

     } else {  /*---- ball ----*/

       xcen = boxball_dat[1+BOXLEN*bb] ; ycen = boxball_dat[2+BOXLEN*bb] ;
       zcen = boxball_dat[3+BOXLEN*bb] ; rad  = boxball_dat[4+BOXLEN*bb] ;

       /* convert center coords to dataset indexes */

       if( btyp == BALL_NEU ){          /* coords from Neuroscience to DICOM */
         xcen = -xcen; ycen = -ycen; btyp = BALL_DIC;
       }
       if( btyp == BALL_DIC ){          /* coords from DICOM to dataset */
         LOAD_FVEC3(dv,xcen,ycen,zcen) ;
         xv = THD_dicomm_to_3dmm( dset , dv ) ;
         UNLOAD_FVEC3(xv,xcen,ycen,zcen) ;
       }
       if( xcen < xmin || xcen > xmax ) continue ;  /* skip ball if outside */
       if( ycen < ymin || ycen > ymax ) continue ;
       if( zcen < zmin || zcen > zmax ) continue ;
       LOAD_FVEC3(dv,xcen,ycen,zcen) ;
       xv = THD_3dmm_to_3dfind( dset , dv ) ;   /* coords from dataset to index */
       UNLOAD_FVEC3(xv,icen,jcen,kcen) ;

       ibot = rint(icen-rad) ; itop = rint(icen+rad) ; /* box around ball */
       jbot = rint(jcen-rad) ; jtop = rint(jcen+rad) ;
       kbot = rint(kcen-rad) ; ktop = rint(kcen+rad) ;

       rad = rad*rad ;

       for( kk=kbot ; kk <= ktop ; kk++ ){
        for( jj=jbot ; jj <= jtop ; jj++ ){
         for( ii=ibot ; ii <= itop ; ii++ ){
            LOAD_FVEC3( dv , ii,jj,kk ) ;          /* convert to xyz coords */
            xv = THD_3dfind_to_3dmm( dset , dv ) ; /* then test distance^2 */
            UNLOAD_FVEC3( xv , xx,yy,zz ) ;        /* xyz of ball center. */
            dist = SQR(xx-xcen) + SQR(yy-ycen) + SQR(zz-zcen) ;
            if( dist <= rad ) bmask[ii+jj*nx+kk*nxy] = 1 ;
       }}}
     }

   } /*----- end of loop over box/ball list -----*/

   RETURN(bmask) ;
}

/*------- functions moved from vol2surf.c ------------- 13 Nov 2006 [rickr] */

/*----------------------------------------------------------------------
 * thd_mask_from_brick    - create a mask from a sub-brick and threshold
 *
 * return the number of set voxels in the mask
 *----------------------------------------------------------------------
*/

int thd_mask_from_brick(THD_3dim_dataset * dset, int volume, float thresh,
                        byte ** mask, int absolute)
{
    float   factor;
    byte  * tmask;
    int     nvox, type, c, size = 0;

ENTRY("thd_mask_from_brick");

    if ( mask ) *mask = NULL;   /* to be sure */

    if ( !ISVALID_DSET(dset) || ! mask || volume < 0 )
        RETURN(-1);

    if ( volume >= DSET_NVALS(dset) )
    {
        fprintf(stderr,"** tmfb: sub-brick %d out-of-range\n", volume);
        RETURN(-1);
    }

    if( !DSET_LOADED(dset) ) DSET_load(dset);
    nvox = DSET_NVOX(dset);
    type = DSET_BRICK_TYPE(dset, volume);

    if ( type != MRI_byte && type != MRI_short &&
         type != MRI_int && type != MRI_float )
    {
        fprintf(stderr,"** tmfb: invalid dataset type %s, sorry...\n",
                MRI_type_name[type]);
        RETURN(-1);
    }

    tmask = (byte *)calloc(nvox, sizeof(byte));
    if ( ! tmask )
    {
        fprintf(stderr,"** tmfb: failed to allocate mask of %d bytes\n", nvox);
        RETURN(-1);
    }

    factor = DSET_BRICK_FACTOR(dset, volume);

    /* cheat: adjust threshold, not data */
    if ( factor != 0.0 ) thresh /= factor;

    switch( DSET_BRICK_TYPE(dset, volume) )
    {
        case MRI_byte:
        {
            byte * dp  = DSET_ARRAY(dset, volume);
            byte   thr = BYTEIZE(thresh + 0.99999);  /* ceiling */
            for ( c = 0; c < nvox; c++ )
                if ( dp[c] != 0 && ( dp[c] >= thr ) )
                {
                    size++;
                    tmask[c] = 1;
                }
        }
            break;

        case MRI_short:
        {
            short * dp  = DSET_ARRAY(dset, volume);
            short   thr = SHORTIZE(thresh + 0.99999);  /* ceiling */
            for ( c = 0; c < nvox; c++, dp++ )
                if ( *dp != 0 && ( *dp >= thr || (absolute && *dp <= -thr) ) )
                {
                    size++;
                    tmask[c] = 1;
                }
        }
            break;

        case MRI_int:
        {
            int * dp  = DSET_ARRAY(dset, volume);
            int   thr = (int)(thresh + 0.99999);  /* ceiling */
            for ( c = 0; c < nvox; c++, dp++ )
                if ( *dp != 0 && ( *dp >= thr || (absolute && *dp <= -thr) ) )
                {
                    size++;
                    tmask[c] = 1;
                }
        }
            break;

        case MRI_float:
        {
            float * dp = DSET_ARRAY(dset, volume);
            for ( c = 0; c < nvox; c++, dp++ )
                if (*dp != 0 && (*dp >= thresh || (absolute && *dp <= -thresh)))
                {
                    size++;
                    tmask[c] = 1;
                }
        }
            break;

        default:                /* let's be sure */
        {
            fprintf(stderr,"** tmfb: invalid dataset type, sorry...\n");
            free(tmask);
        }
            break;
    }

    *mask = tmask;

    RETURN(size);
}

/*----------------------------------------------------------------------
 * thd_multi_mask_from_brick - create a valued mask from a sub-brick
 *
 * return 0 on success, else failure             10 Nov 2006 [rickr]
 *----------------------------------------------------------------------
*/

int thd_multi_mask_from_brick(THD_3dim_dataset * dset, int volume, byte ** mask)
{
    float   factor;
    byte  * tmask;
    int     nvox, type, c;

ENTRY("thd_mask_from_brick");

    if ( mask ) *mask = NULL;   /* to be sure */

    if ( !ISVALID_DSET(dset) || ! mask || volume < 0 )
        RETURN(-1);

    if ( volume >= DSET_NVALS(dset) )
    {
        fprintf(stderr,"** tmmfb: sub-brick %d out-of-range\n", volume);
        RETURN(-1);
    }

    if( !DSET_LOADED(dset) ) DSET_load(dset);
    nvox = DSET_NVOX(dset);
    type = DSET_BRICK_TYPE(dset, volume);

    if ( type != MRI_byte && type != MRI_short &&
         type != MRI_int && type != MRI_float )
    {
        fprintf(stderr,"** tmmfb: invalid dataset type %s, sorry...\n",
                MRI_type_name[type]);
        RETURN(-1);
    }

    tmask = (byte *)calloc(nvox, sizeof(byte));
    if ( ! tmask )
    {
        fprintf(stderr,"** tmmfb: failed to allocate mask of %d bytes\n", nvox);
        RETURN(-1);
    }

    factor = DSET_BRICK_FACTOR(dset, volume);
    if( factor == 1.0 ) factor = 0.0;

    switch( DSET_BRICK_TYPE(dset, volume) )
    {
        case MRI_byte:
        {
            byte * dp  = DSET_ARRAY(dset, volume);
            if( factor )
                for ( c = 0; c < nvox; c++ )
                    tmask[c] = (byte)(int)rint(dp[c]*factor);
            else
                for ( c = 0; c < nvox; c++ )
                    tmask[c] = dp[c];
        }
            break;

        case MRI_short:
        {
            short * dp  = DSET_ARRAY(dset, volume);
            if( factor )
                for ( c = 0; c < nvox; c++ )
                    tmask[c] = (byte)(int)rint(dp[c]*factor);
            else
                for ( c = 0; c < nvox; c++ )
                    tmask[c] = (byte)dp[c];
        }
            break;

        case MRI_int:
        {
            int * dp  = DSET_ARRAY(dset, volume);
            if( factor )
                for ( c = 0; c < nvox; c++ )
                    tmask[c] = (byte)(int)rint(dp[c]*factor);
            else
                for ( c = 0; c < nvox; c++ )
                    tmask[c] = (byte)dp[c];
        }
            break;

        case MRI_float:
        {
            float * dp = DSET_ARRAY(dset, volume);
            if( factor )
                for ( c = 0; c < nvox; c++ )
                    tmask[c] = (byte)(int)rint(dp[c]*factor);
            else
                for ( c = 0; c < nvox; c++ )
                    tmask[c] = (byte)dp[c];
        }
            break;

        default:                /* let's be sure */
        {
            fprintf(stderr,"** tmmfb: invalid dataset type, sorry...\n");
            free(tmask);
        }
            break;
    }

    *mask = tmask;

    RETURN(0);
}
