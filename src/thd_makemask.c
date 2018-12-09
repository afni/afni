/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "uthash.h"

typedef struct {
    int id;    /* keep it named 'id' to facilitate use of convenience
                  macros in uthash . */
    UT_hash_handle hh;  /* keep name for same reason  */
    int index;
}  INT_HASH_DATUM;

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
         WARNING_message("makemask: bad brick type %d",
                         DSET_BRICK_TYPE(mask_dset,miv));
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
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0
                                   && cmask[ii]) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 )
                    { mar[ii]=1; ++nonzero; }
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
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0
                                   && cmask[ii]){ mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 )
                    { mar[ii]=1; ++nonzero; }
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
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0
                                   && cmask[ii]) { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 )
                    { mar[ii]=1; ++nonzero; }
               else { mar[ii] = 0; }
         }
      }
      break ;
   }

   /* remove any scaling factor ZSS April 24 06*/
   EDIT_BRICK_FACTOR(mask_dset,miv , 0.0);

   return (nonzero) ;
}


/*---------------------------------------------------------------------------*/
/*!  Convert an entire dataset of MRI_byte, and all values to binary
     (set or not).

     return 1 on failure, 0 on success           3 Jun, 2014 [rickr]
*/

int THD_dset_to_mask(THD_3dim_dataset * dset, float mask_bot, float mask_top)
{
   byte * bvol = NULL;
   int    ivol;

   ENTRY("THD_dset_to_mask");

   if( !ISVALID_DSET(dset) ) {
      ERROR_message("dset_to_mask: dset not valid");
      RETURN(1);
   }

   DSET_mallocize(dset); DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) {
      ERROR_message("dset_to_mask: dset not loaded");
      RETURN(1);
   }

   for( ivol = 0; ivol < DSET_NVALS(dset); ivol++ ) {

      bvol = THD_makemask(dset, ivol, mask_bot, mask_top);
      if( !bvol ) {
         ERROR_message("dset_to_mask: failed to mask vol %d", ivol);
         RETURN(1);
      }

      EDIT_substitute_brick(dset, ivol, MRI_byte, bvol);
      EDIT_BRICK_FACTOR(dset, ivol, 0.0);
   }

   RETURN(0);
}

/*
   Zero out voxels vv in dset where cmask[vv]=0
   Returns the number of voxels edited in dset (across all sub-bricks)
      -1 if dset was null
*/
int THD_applydsetmask( THD_3dim_dataset *dset ,  byte *cmask )
{
   int ss, ii, jj, kk, vv, nedited = -1 ;

   ENTRY("THD_applydsetmask");

   if (!dset) RETURN(nedited);

   if (!cmask) RETURN(0);

   DSET_mallocize(dset); DSET_load(dset);
   for (ss=0; ss<DSET_NVALS(dset); ++ss) {
      switch (DSET_BRICK_TYPE(dset,ss)) {
         case MRI_byte:
            {  byte *bv = (byte *)DSET_ARRAY(dset,ss) ;
               vv=0;
               for (kk=0; kk<DSET_NZ(dset); ++kk) {
               for (jj=0; jj<DSET_NY(dset); ++jj) {
               for (ii=0; ii<DSET_NX(dset); ++ii) {
                  if (!cmask[vv]) {
                     bv[vv] = 0;
                     ++nedited;
                  }
                  ++vv;
               } } }
            }
            break;
         case MRI_short:
            {  short *sv = (short *)DSET_ARRAY(dset,ss) ;
               vv=0;
               for (kk=0; kk<DSET_NZ(dset); ++kk) {
               for (jj=0; jj<DSET_NY(dset); ++jj) {
               for (ii=0; ii<DSET_NX(dset); ++ii) {
                  if (!cmask[vv]) {
                     sv[vv] = 0;
                     ++nedited;
                  }
                  ++vv;
               } } }
            }
            break;
         case MRI_float:
            {  float *fv = (float *)DSET_ARRAY(dset,ss) ;
               vv=0;
               for (kk=0; kk<DSET_NZ(dset); ++kk) {
               for (jj=0; jj<DSET_NY(dset); ++jj) {
               for (ii=0; ii<DSET_NX(dset); ++ii) {
                  if (!cmask[vv]) {
                     fv[vv] = 0;
                     ++nedited;
                  }
                  ++vv;
               } } }
            }
            break;
         case MRI_complex:
            {  complex *cv = (complex *)DSET_ARRAY(dset,ss) ;
               vv=0;
               for (kk=0; kk<DSET_NZ(dset); ++kk) {
               for (jj=0; jj<DSET_NY(dset); ++jj) {
               for (ii=0; ii<DSET_NX(dset); ++ii) {
                  if (!cmask[vv]) {
                     cv[vv].i = cv[vv].r = 0.0;
                     ++nedited;
                  }
                  ++vv;
               } } }
            }
            break;
         default:
            ERROR_message(
               "THD_applydsetmask: Dset type %d for subbrick %d not supported\n",
                          DSET_BRICK_TYPE(dset,ss), ss);
            break;
      }
   }

   RETURN(nedited);
}

/*----------------------------------------------------------------------------*/
extern int * UniqueInt (int *y, int ysz, int *kunq, int Sorted );

int is_integral_sub_brick ( THD_3dim_dataset *dset, int isb, int check_values)
{
   float mfac = 0.0;
   void *vv=NULL;

   if(   !ISVALID_DSET(dset)    ||
            isb < 0                     ||
            isb >= DSET_NVALS(dset)  ) {

      fprintf(stderr,"** Bad dset or sub-brick index.\n");
      return (0) ;

   }
   if( !DSET_LOADED(dset) ) DSET_load(dset);

   switch( DSET_BRICK_TYPE(dset,isb) ){
      case MRI_short:
      case MRI_byte:
         if (check_values) {
            mfac = DSET_BRICK_FACTOR(dset,isb) ;
            if (mfac != 0.0f && mfac != 1.0f) return(0);
         }
         break;
      case MRI_double:
      case MRI_complex:
      case MRI_float:
         vv = (void *)DSET_ARRAY(dset,isb);
         mfac = DSET_BRICK_FACTOR(dset,isb) ;
         if (mfac != 0.0f && mfac != 1.0f) return(0);
         if (!vv) {
            fprintf(stderr,"** NULL array!\n");
            return(0);
         }
         return(is_integral_data(DSET_NVOX(dset),
                                 DSET_BRICK_TYPE(dset,isb),
                                 DSET_ARRAY(dset,isb) ) );
         break;
      default:
         return(0);
   }

   return(1);
}

int is_integral_dset ( THD_3dim_dataset *dset, int check_values)
{
   int i=0;

   if(   !ISVALID_DSET(dset)  ) return(0);
   for (i=0; i<DSET_NVALS(dset); ++i) {
      if (!is_integral_sub_brick(dset, i, check_values)) return(0);
   }
   return(1);
}

/*!
   Returns a list of the unique values in a dataset.
*/

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

   if (!is_integral_sub_brick (mask_dset, miv, 0)) {
      fprintf(stderr,"** Sub-brick %d of %s is not of an integral data type.\n",
                  miv, DSET_PREFIX(mask_dset) ? DSET_PREFIX(mask_dset):"NULL");
      return (unq) ;
   }

   vals = (int *)malloc(sizeof(int)*nvox);
   if (!vals) {
      fprintf(stderr,"** Failed to allocate.\n");
      return (unq) ;
   }

   switch( DSET_BRICK_TYPE(mask_dset,miv) ){
      default:
         fprintf(stderr,"** Bad dset type for unique operation.\n"
                        "Only integral valued dsets are allowed.\n");
         DSET_unload(mask_dset) ; if (vals) free(vals); return (unq) ;

      case MRI_short:{
         short *mar = (short *) DSET_ARRAY(mask_dset,miv) ;
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]);
         }

      }
      break ;

      case MRI_byte:{
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]);
         }

      }
      break ;

      case MRI_float:{
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]);
         }

      }
      break ;
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
                        char *mapname,
                        int **unqp, int *N_unq)
{
   int nvox , ii, *unq = NULL, *vals=NULL, imax=0;
   INT_HASH_DATUM *rmap=NULL, *hd=NULL;
   int n_unique, r;
   FILE *fout=NULL;

   n_unique = 0;
   unq = NULL ;

   if (unqp && *unqp!=NULL) {
      fprintf(stderr,"** unqp (%p) not initialized properly to NULL", *unqp);
      return (vals) ;
   }

   if( !ISVALID_DSET(mask_dset)    ||
       miv < 0                     ||
       miv >= DSET_NVALS(mask_dset)  ) {

      fprintf(stderr,"** Bad mask_dset or sub-brick index.\n");
      return (vals) ;

   }
   nvox = DSET_NVOX(mask_dset) ;

   DSET_load(mask_dset) ; if( !DSET_LOADED(mask_dset) ) return (vals) ;

   if (!is_integral_sub_brick (mask_dset, miv, 0)) {
      fprintf(stderr,"** Sub-brick %d of %s is not integral valued.\n",
                  miv, DSET_PREFIX(mask_dset) ? DSET_PREFIX(mask_dset):"NULL");
      return (vals) ;
   }


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
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]);
         }

      }
      break ;

      case MRI_byte:{
         byte *mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]);
         }

      }
      break ;

      case MRI_float:{ /* not an integral type but we store ints (from NIFTI)
                          as floats */
         float *mar = (float *) DSET_ARRAY(mask_dset,miv) ;
         if (cmask) {
            for( ii=0 ; ii < nvox ; ii++ )
               if (cmask[ii]) vals[ii] = (int)(mar[ii]); else vals[ii] = 0;
         } else {
            for( ii=0 ; ii < nvox ; ii++ )
               vals[ii] = (int)(mar[ii]);
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

   if (mapname && mapname[0]) {
      /*fprintf(stderr,"-- Writing mapping to >>%s<<\n", mapname);*/
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
   for (r=0; r<n_unique; ++r) {
      if (imax < unq[r]) imax = unq[r];
      if (fout) fprintf(fout, "%d   %d\n", r, unq[r]);
      hd = (INT_HASH_DATUM*)calloc(1,sizeof(INT_HASH_DATUM));
      hd->id = unq[r];
      hd->index = r;
      HASH_ADD_INT(rmap, id, hd);
   }
   for (ii=0; ii<nvox; ii++)
      if (!cmask || cmask[ii]) {
         HASH_FIND_INT(rmap,&(vals[ii]),hd);
         if (hd)  vals[ii] = hd->index;
         else {
            fprintf(stderr,
                     "** Failed to find key %d inhash table\n",
                     vals[ii]);
            free(vals);
            while (rmap) { hd=rmap; HASH_DEL(rmap,hd); free(hd); }
            return (NULL);
         }
      }
   /* destroy hash */
   while (rmap) {
      hd=rmap;
      HASH_DEL(rmap,hd);
      if (hd) free(hd);
   }

   #endif

   if (!unqp) free(unq); else *unqp = unq; unq = NULL;
   if (N_unq) *N_unq = n_unique;
   if (fout) fclose(fout); fout = NULL;

   return (vals) ;
}


/*----------------------------------------------------------------------------*/
/* Same as THD_unique_rank but replaces values in mask_dset with rank */

int THD_unique_rank_edit( THD_3dim_dataset *mask_dset ,
                           int miv,
                           byte *cmask,
                           char *mapname, int **unqp, int *N_unq)
{
   int *vals=NULL, nvox, mxval, ii;

   if (!(vals = THD_unique_rank(mask_dset, miv, cmask, mapname, unqp, N_unq))) {
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
            if (thresh <= (float)MRI_maxbyte) { /* ZSS: Oct 2011
                  Without this test, a high threshold value might end up
                  equal to MRI_maxbyte when BYTEIZED below, resulting in
                  the highest voxel making it to the mask no matter how
                  much higher the threshold is set.                  */
               byte * dp  = DSET_ARRAY(dset, volume);
               byte   thr = BYTEIZE(thresh + 0.99999);  /* ceiling */
               for ( c = 0; c < nvox; c++ )
                   if ( dp[c] != 0 && ( dp[c] >= thr ) )
                   {
                       size++;
                       tmask[c] = 1;
                   }
            }
        }
            break;

        case MRI_short:
        {
            if (thresh <= (float)MRI_maxshort) { /* ZSS: Oct 2011 */
               short * dp  = DSET_ARRAY(dset, volume);
               short   thr = SHORTIZE(thresh + 0.99999);  /* ceiling */
               for ( c = 0; c < nvox; c++, dp++ )
                   if ( *dp != 0 && ( *dp >= thr || (absolute && *dp <= -thr) ) )
                   {
                       size++;
                       tmask[c] = 1;
                   }
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

ENTRY("thd_multi_mask_from_brick");

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

/****************************************************************************
 ** The functions below are for converting a byte-valued 0/1 mask to/from  **
 ** an ASCII representation.  The ASCII representation is formed like so:  **
 **    1. convert it to binary = 8 bits stored in each byte, rather than 1 **
 **       - this takes the mask from nvox bytes to 1+(nvox-1)/8 bytes      **
 **       - this operation is done in function mask_binarize() [below]     **
 **    2. compress the binarized array with zlib                           **
 **       - this step is done in function array_to_zzb64(), which uses     **
 **         function zz_compress_all() [in zfun.c]                         **
 **    3. express the compressed array into Base64 notation (in ASCII)     **
 **       - this step is also done in function array_to_zzb64(), which     **
 **         uses function B64_to_base64() [in niml/niml_b64.c]             **
 **    4. attach at the end a string to indicate the number of voxels      **
 **         in the mask.                                                   **
 ** + The above steps are done in function mask_to_b64string() [below].    **
 ** + The inverse is done in function mask_from_b64string() [below].       **
 ** + Function mask_b64string_nvox() [below] can be used to get the voxel  **
 **   count from the end of the string, which can be used to check if a    **
 **   mask is compatible with a given dataset for which it is intended.    **
 ****************************************************************************
 * See program 3dMaskToASCII.c for sample usage of these functions.         *
*****************************************************************************/

/*-------------------------------------------------------------------------*/
/*! Convert a byte-value 0/1 mask to an ASCII string in Base64. */

char * mask_to_b64string( int nvox , byte *mful )
{
   byte *mbin ; char *str ; int nstr ;

   if( nvox < 1 || mful == NULL ) return NULL ;          /* bad inputs */

   mbin = mask_binarize( nvox , mful ) ;
   str  = array_to_zzb64( 1+(nvox-1)/8 , mbin , 72 ) ; free(mbin) ;
   if( str == NULL ) return NULL ;              /* should never happen */

   nstr = strlen(str) ;
   str  = (char *)realloc( str , sizeof(char)*(nstr+16) ) ;
   sprintf( str+nstr-1 , "===%d" , nvox ) ;  /* -1 to erase last linefeed */

   return str ;
}

/*-------------------------------------------------------------------------*/
/*! Convert an ASCII string in Base64 to a byte-valued 0/1 mask. */

byte * mask_from_b64string( char *str , int *nvox )
{
   byte *mful , *mbin=NULL ; int nvvv , nstr , ii,ibot , nbin ;

   if( str == NULL || nvox == NULL ) return NULL ;    /* bad inputs */

   nvvv = mask_b64string_nvox(str) ;
   if( nvvv <= 0 ) return NULL ;                            /* WTF? */

   /* decode string to binarized array */

   nbin = zzb64_to_array( str , (char **)&mbin ) ;
   if( nbin <= 0 || mbin == NULL ) return NULL ;      /* bad decode */

   /* decode binarized array to byte mask */

   mful = mask_unbinarize( nvvv , mbin ) ; free(mbin) ;

   *nvox = nvvv ; return mful ;
}

/*-------------------------------------------------------------------------*/

int mask_b64string_nvox( char *str )
{
   int nstr , ii , ibot ;

   if( str == NULL ) return 0 ;
   nstr = strlen(str) ; if( nstr < 7 ) return 0 ;      /* too short */

   /* find the last '=' at the end of the string */

   ibot = nstr-16 ; if( ibot < 3 ) ibot = 3 ;
   for( ii=nstr-1 ; ii > ibot && str[ii] != '=' ; ii-- ) ; /*nada*/
   if( str[ii] != '=' ) return 0 ;               /* badly formatted */

   ibot = (int)strtod(str+ii+1,NULL) ;          /* number of voxels */
   return ibot ;
}

/*-------------------------------------------------------------------------*/
/* Input:  0/1 array of bytes, nvox long.
   Output: compressed by a factor of 8: 1+(nvox-1)/8 bytes long.
*//*-----------------------------------------------------------------------*/

static byte binar[8] = { 1 , 1<<1 , 1<<2 , 1<<3 , 1<<4 , 1<<5 , 1<<6 , 1<<7 } ;

byte * mask_binarize( int nvox , byte *mful )
{
   register byte *mbin ; register int ii ;

   if( nvox < 1 || mful == NULL ) return NULL ;

   mbin = (byte *)calloc(sizeof(byte),1+(nvox-1)/8) ;

   for( ii=0 ; ii < nvox ; ii++ )
     if( mful[ii] != 0 ) mbin[ii>>3] |= binar[ii&0x7] ;

   return mbin ;
}

/*-------------------------------------------------------------------------*/

byte * mask_unbinarize( int nvox , byte *mbin )
{
   register byte *mful ; register int ii ;

   if( nvox < 1 || mbin == NULL ) return NULL ;

   mful = (byte *)calloc(sizeof(byte),nvox) ;

   for( ii=0 ; ii < nvox ; ii++ )
     mful[ii] = ( mbin[ii>>3] & binar[ii&0x7] ) != 0 ;

   return mful ;
}

/*===========================================================================*/
/*! Create a binary byte-valued mask from an input string:
      - a dataset filename
      - a Base64 mask string
      - filename with data containing a Base64 mask string
      - future editions?
*//*-------------------------------------------------------------------------*/

bytevec * THD_create_mask_from_string( char *str )  /* Jul 2010 */
{
   bytevec *bvec=NULL ; int nstr ; char *buf=NULL ;
   int ferr=0;

ENTRY("THD_create_mask") ;

   if( str == NULL || *str == '\0' ) RETURN(NULL) ;

   nstr = strlen(str) ;
   bvec = (bytevec *)malloc(sizeof(bytevec)) ;

   /* try to read it as a dataset */

   if( nstr < THD_MAX_NAME ){
     THD_3dim_dataset *dset = THD_open_one_dataset(str) ;
     if( dset != NULL ){
       bvec->nar = DSET_NVOX(dset) ;
       bvec->ar  = THD_makemask( dset , 0 , 1.0f,0.0f ) ;
       DSET_delete(dset) ;
       if( bvec->ar == NULL ){
         ERROR_message("Can't make mask from dataset '%s'",str) ;
         free(bvec) ; bvec = NULL ;
       }
       RETURN(bvec) ;
     }

     ferr = 1; /* string is short, but failed to open as dataset */
   }

   /* if str is a filename, read that file;
      otherwise, use the string itself to find the mask */

   if( THD_is_file(str) ){
     buf = AFNI_suck_file(str) ;
     if( buf != NULL ) nstr = strlen(buf) ;
   } else {
     buf = str ;
   }

   /* try to read buf as a Base64 mask string */

   if( strrchr(buf,'=') != NULL ){
     int nvox ;
     bvec->ar = mask_from_b64string( buf , &nvox ) ;
     if( bvec->ar != NULL ){
       bvec->nar = nvox ;
     } else {
       /* might be a non-existent file        14 Jan 2014 [rickr] */
       if( ferr ) ERROR_message("Failed to open mask from '%s'", str);
       else       ERROR_message("Can't make mask from string '%.16s' %s",
                                buf,(nstr<=16)?" ":"...") ;
       free(bvec) ; bvec = NULL ;
     }
   } else {
     if( ferr ) ERROR_message("Failed to open mask '%s'", str);
     else       ERROR_message("Don't understand mask string '%.16s'",
                              buf,(nstr<=16)?" ":"...") ;
     free(bvec) ; bvec = NULL ;
   }

   if( buf != str && buf != NULL ) free(buf) ;
   RETURN(bvec) ;
}
