#include "mrilib.h" 

PARAMS_euclid_dist set_euclid_dist_defaults(void)
{

   PARAMS_euclid_dist defopt;

   defopt.input_name = NULL;     
   defopt.mask_name = NULL;     
   defopt.prefix = NULL;         

   defopt.zeros_are_zeroed = 0;
   defopt.zero_region_sign = 1;
   defopt.nz_region_sign   = 1;
   defopt.bounds_are_zero  = 1;
   defopt.ignore_voxdims   = 0;
   defopt.dist_sq          = 0;
   defopt.rimify           = 0.0;

   defopt.edims[0] = 0.0;       
   defopt.edims[1] = 0.0;
   defopt.edims[2] = 0.0;
   defopt.shape[0] = 0; 
   defopt.shape[1] = 0; 
   defopt.shape[2] = 0; 

   defopt.verb = 1;

   defopt.only2D = NULL;
   defopt.axes_to_proc[0] = 1;
   defopt.axes_to_proc[1] = 1;
   defopt.axes_to_proc[2] = 1;

   defopt.binary_only = 0;

   return defopt;
};

// ---------------------------------------------------------------------------

/*
  Minor helper function: we want to go through the axes in the order
  of descending voxel size.

  Ledge : (input) array of 3 vox edge lengths
  ord   : (output) array of 3 indices, describing decreasing vox size
*/
int sort_vox_ord_desc(int N, float *Ledge, int *ord)
{
   int i;
   float far[N];
   int iar[N];

   ENTRY("sort_vox_ord_desc");

   for( i=0 ; i<N ; i++){
      far[i] = Ledge[i];
      iar[i] = i;     // so that we get indices in decreasing order
   }

   // sorts far in increasing order, carrying along iar values
   qsort_floatint( N , far , iar );
   
   // and we want the 'output' to have the reverse order of iar
   for( i=0 ; i<N ; i++)
      ord[i] = iar[N-1-i]; 

   return 0;
}

// ---------------------------------------

/*
  Minor helper function: given a 'which_slice' value (of: cor, axi or
  sag), and a particular dataset, determine which axes to analyze and
  which to skip.  The latter information is 'output' via the
  integer-valued onoff_arr: axes to that are part of the which_slice
  plane should get a 1, and that which isn't should get a 0.

  For example, for a dset with orient=LIP, if the which_slice value is
  'cor', then the onoff_arr should be 110.

  dset         :(dset) just to get orientation info from
  which_slice  :(char array) name of plane
  onoff_arr    :(int arr, len=3) essentially output, made of 1s and 0.
  verb         :(int) do we discuss what we are doing?
  
*/
int choose_axes_for_plane( THD_3dim_dataset *dset, char *which_slice,
                           int *onoff_arr, int verb )
{
   int i;
   char ostr[4];  
   THD_fill_orient_str_3(dset->daxes, ostr);

   if( strcmp(which_slice, "cor") == 0 ){ // LR and IS, not AP
      for( i=0 ; i<3 ; i++ )
         if( !strncmp(ostr+i,"A", 1) || !strncmp(ostr+i,"P", 1) ){
            onoff_arr[i] = 0;
            break;
         }
   }
   else if( strcmp(which_slice, "axi") == 0 ){ // LR and AP, not IS
      for( i=0 ; i<3 ; i++ )
         if( !strncmp(ostr+i,"I", 1) || !strncmp(ostr+i,"S", 1) ){
            onoff_arr[i] = 0;
            break;
         }
   }
   else if( strcmp(which_slice, "sag") == 0 ){ // AP and IS, not LR
      for( i=0 ; i<3 ; i++ )
         if( !strncmp(ostr+i,"R", 1) || !strncmp(ostr+i,"L", 1) ){
            onoff_arr[i] = 0;
            break;
         }
   }
   
   if( verb ) {
      THD_fill_orient_str_3(dset->daxes, ostr);
      INFO_message("Do 2D calc in '%s' plane, and dset orient=%s,\n"
                   "   so the ON/OFF of axes for the EDT calc is: %d%d%d.",
                   which_slice, ostr, onoff_arr[0], 
                   onoff_arr[1], onoff_arr[2]);
   }

   return 0;
};

// ---------------------------------------

/* Primary function to calculate rim per ROIs.  Will pass along any
   labeltable from the original

  dset_rim : dset that gets filled in with values here---basically, a subset
             from the dset_roi at the boundary of each ROI
  dset_edt : the depthmap of each ROI, which is thresholded to determine 
             how deep the rim for each ROI is
  dset_roi : the map of ROIs that was input
  rim_thr  : float, the depth of the rim (can be in terms of mm or number of 
             voxels, depending on what the distance units in dset_edt are);
             negative rim_thr inverts the selection: now keep parts with
             dist >=RIM_THR
  copy_lt  : flag for whether to copy any labeltable that dset_roi has to 
             dset_rim
*/
int calc_EDT_rim(THD_3dim_dataset *dset_rim, THD_3dim_dataset *dset_edt, 
                 THD_3dim_dataset *dset_roi, float rim_thr, int copy_lt)
{
   int idx, nn;
   int nvox, nvals;
   int roi_datum;
   int sign = 1;                // control for when rim_thr<0

   byte  *btmp_arr = NULL;
   short *stmp_arr = NULL;
   float *ftmp_arr = NULL;

   nvox  = DSET_NVOX(dset_rim);
   nvals = DSET_NVALS(dset_rim); 

   if( rim_thr < 0 )
      sign = -1;

   roi_datum = DSET_BRICK_TYPE(dset_roi, 0) ;

   for( nn=0 ; nn<nvals ; nn++ ) {
      // create tmp 'brick' array
      switch( roi_datum ){
      case MRI_byte:
         btmp_arr = (byte *) calloc( nvox, sizeof(byte) );
         if( btmp_arr == NULL ) {
            fprintf(stderr, "\n\n MemAlloc failure.\n\n");
            exit(12);
         }

         // find places to put in nonzero values
         for( idx=0 ; idx<nvox ; idx++ ) {
            if ( sign*fabs(THD_get_voxel(dset_edt, idx, nn)) <= rim_thr  )
               btmp_arr[idx] = THD_get_voxel(dset_roi, idx, nn);
         }
         
         // copy the brick array into the dset
         EDIT_substitute_brick(dset_rim, nn, MRI_byte, btmp_arr); 
         btmp_arr=NULL;

         break;
      case MRI_short:
         stmp_arr = (short *) calloc( nvox, sizeof(short) );
         if( stmp_arr == NULL ) {
            fprintf(stderr, "\n\n MemAlloc failure.\n\n");
            exit(12);
         }

         // find places to put in nonzero values
         for( idx=0 ; idx<nvox ; idx++ ) {
            if ( sign*fabs(THD_get_voxel(dset_edt, idx, nn)) <= rim_thr  )
               stmp_arr[idx] = THD_get_voxel(dset_roi, idx, nn);
         }
         
         // copy the brick array into the dset
         EDIT_substitute_brick(dset_rim, nn, MRI_short, stmp_arr); 
         stmp_arr=NULL;

         break;
      case MRI_float:
         ftmp_arr = (float *) calloc( nvox, sizeof(float) );
         if( ftmp_arr == NULL ) {
            fprintf(stderr, "\n\n MemAlloc failure.\n\n");
            exit(12);
         }

         // find places to put in nonzero values
         for( idx=0 ; idx<nvox ; idx++ ) {
            if ( sign*fabs(THD_get_voxel(dset_edt, idx, nn)) <= rim_thr  )
               ftmp_arr[idx] = THD_get_voxel(dset_roi, idx, nn);
         }
         
         // copy the brick array into the dset
         EDIT_substitute_brick(dset_rim, nn, MRI_float, ftmp_arr); 
         ftmp_arr=NULL;

         break;
      }
   }

   // copy over labeltable/atlas, if either exists
   if( copy_lt ){
      // does dset_roi have a labeltable/atlas?
      char *str    = NULL;
      int iaol_val = 0;
      if ( is_Dset_Atlasy(dset_roi, NULL) ) {
         iaol_val = 1;
      }
      else if ( (str = Dtable_to_nimlstring(DSET_Label_Dtable(dset_roi),
                                            "VALUE_LABEL_DTABLE")) ) {
         // 'else if' for speed
         iaol_val = 1;
         free(str);
      }
      
      if( iaol_val ) 
         INFO_message("Copy over label and/or atlas table");
         if (!THD_copy_labeltable_atr( dset_rim->dblk , dset_roi->dblk )) {
            WARNING_message("Failed to copy labletable attributes");
         }
   }

   return 0;
};

// =========================================================================

/*
  ***This function treats treats in the input "ROI map" as a binary mask***

  dset_edt  :  dset that is filled in here with EDT values
  opts      :  struct containing default/user options 
  dset_roi  :  the ROI map (dataset, basically 'im' in lib_EDT.py)
  dset_mask :  a mask dset to be applied at end (could be NULL)
  ival      :  index value of the subbrick/subvolume to analyze
*/
int calc_EDT_3D_BIN( THD_3dim_dataset *dset_edt, PARAMS_euclid_dist opts,
                     THD_3dim_dataset *dset_roi, THD_3dim_dataset *dset_mask,
                     int ival)
{
   int i, j, k, idx;
   int ii, jj, kk;
   float minmax[2] = {0, 0};
   int nx, ny, nz, nxy, nvox;
   int nx2, ny2, nz2;
   float delta = 1.0;

   float Ledge[3];            // voxel edge lengths ('edims' in lib_EDT.py)
   int vox_ord_rev[3];

   float fldim[3];            // just want max arr len
   int dim_ord_rev[3] = {0,1,2};

   int   dim_max  = -1;
   float *flarr   = NULL;     // store distances along one dim
   float *workarr = NULL;     // has len of flarr, plus 2
   int   *maparr  = NULL;     // store ROI map along one dim

   float ***arr_dist = NULL;  // array that will hold dist values
   float ***arr_distZ = NULL;  // array that will hold dist values in zeros
   float *tmp_arr = NULL;

   ENTRY("calc_EDT_3D_BIN");

   if( opts.verb )
      INFO_message("Binary-mask input version");

   // check if a subbrick is const; there are a couple cases where we
   // would be done at this stage.
   i = THD_subbrick_minmax( dset_roi, ival, 1, &minmax[0], &minmax[1] );

   if ( minmax[0] == minmax[1] ){
      if ( minmax[1] == 0.0 ){
         WARNING_message("Constant (zero) subbrick: %d\n"
                         "\t-> This volume will have all zero values", ival);
         return 0;
      }
      else if( !opts.bounds_are_zero ){
         WARNING_message("Constant (nonzero) subbrick, "
                         "and no bounds_are_zero: %d\n"
                         "\t-> This volume will have all zero values", ival);
         return 0;
      }
   }

   // dset properties we need to know
   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   nxy = nx*ny;
   nvox = DSET_NVOX(dset_roi);
   Ledge[0] = fabs(DSET_DX(dset_roi)); 
   Ledge[1] = fabs(DSET_DY(dset_roi)); 
   Ledge[2] = fabs(DSET_DZ(dset_roi)); 

   // dims of array we proc
   nx2 = nx+2;  
   ny2 = ny+2;
   nz2 = nz+2;

   // create arrays to be used
   tmp_arr = (float *) calloc( nvox, sizeof(float) );
   arr_dist = (float ***) calloc( nx2, sizeof(float **) );
   for ( i=0 ; i<nx2 ; i++ ) 
      arr_dist[i] = (float **) calloc( ny2, sizeof(float *) );
   for ( i=0 ; i<nx2 ; i++ ) 
      for ( j=0 ; j<ny2 ; j++ ) 
         arr_dist[i][j] = (float *) calloc( nz2, sizeof(float) );
   if( tmp_arr == NULL || arr_dist == NULL ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(12);
   }

   // array for the distances in zero regions, if calc'ed
   if( !opts.zeros_are_zeroed ){
      arr_distZ = (float ***) calloc( nx2, sizeof(float **) );
      for ( i=0 ; i<nx2 ; i++ ) 
         arr_distZ[i] = (float **) calloc( ny2, sizeof(float *) );
      for ( i=0 ; i<nx2 ; i++ ) 
         for ( j=0 ; j<ny2 ; j++ ) 
            arr_distZ[i][j] = (float *) calloc( nz2, sizeof(float) );

      if( arr_distZ == NULL ) {
         fprintf(stderr, "\n\n MemAlloc failure.\n\n");
         exit(12);
      }
   }

   /*
     Initialize distances in main part of padded array to EUCLID_BIG:
     where there is a nonzero value, we stick an EUCLID_BIG.
     
     A bit of subtlety, to create ROI boundary conditions: arr_dist is
     basically the padded-by-one-layer version of dset_roi; we cp over
     dset_roi values to the middle of it, and then we need to decide
     whether the ROI boundaries should be extensions of the ROI
     themselves, or remain zero.  To accomplish the first case, we use
     two rounds of ternary logic on the indices, trickily.
   */
   if( !opts.bounds_are_zero ) { // boundaries where ROIs get extensions
      if( opts.verb && ival==0 )
         INFO_message("Copying values WITH extensions for mask boundary.");

      for ( i=-1 ; i<nx+1 ; i++ ) {
         ii = ( i<0    ) ? 0 : i;
         ii = ( ii>=nx ) ? nx-1 : ii;
         for ( j=-1 ; j<ny+1 ; j++ ) {
            jj = ( j<0    ) ? 0 : j;
            jj = ( jj>=ny ) ? ny-1 : jj;
            for ( k=-1 ; k<nz+1 ; k++ ) {
               kk = ( k<0    ) ? 0 : k;
               kk = ( kk>=nz ) ? nz-1 : kk;
               
               idx = THREE_TO_IJK(ii, jj, kk, nx, nxy);
               if( THD_get_voxel(dset_roi, idx, ival))
                  arr_dist[i+1][j+1][k+1] = EUCLID_BIG;
            }}}
   }
   else { // boundaries remain all zero
      if( opts.verb && ival==0 )
         INFO_message("Copying values WITHOUT extensions for mask boundary.");

      for( i=0 ; i<nx ; i++ ) {
         ii = i+1;
         for( j=0 ; j<ny ; j++ ) {
            jj = j+1;
            for( k=0 ; k<nz ; k++ ) {
               kk = k+1;

               idx = THREE_TO_IJK(i, j, k, nx, nxy);
               if( THD_get_voxel(dset_roi, idx, ival))
                  arr_dist[ii][jj][kk] = EUCLID_BIG;
            }}}
   }
   
   // initialize for distance within zeros, if asked for; this simple
   // approach works even for the boundaries, because the boundaries
   // around the zero part are always more zeros (and the boundaries
   // by where the input ROIs were don't matter here)
   if( !opts.zeros_are_zeroed ){
      for( i=0 ; i<nx2 ; i++ ) 
         for( j=0 ; j<ny2 ; j++ ) 
            for( k=0 ; k<nz2 ; k++ ) {
               if( !arr_dist[i][j][k] ){
                  arr_distZ[i][j][k] = EUCLID_BIG;
               }
            }
   }

   // PT note: do we need the complementary array to arr_dist, for
   // filling in the zero-region with mask values?

   // find axis order of decreasing voxel sizes, to avoid pathology in
   // the EDT alg (that miiiight have only existed in earlier calc
   // method, but it still makes sense to do---why not?)
   i = sort_vox_ord_desc(3, Ledge, vox_ord_rev); 

   // len of longest dset dim, so we don't have to keep
   // allocating/freeing
   fldim[0] = nx;
   fldim[1] = ny;
   fldim[2] = nz;
   qsort_floatint( 3 , fldim , dim_ord_rev );
   dim_max = (int) fldim[2];
   flarr   = (float *) calloc( dim_max+2, sizeof(float) ); // NB the +2 here
   workarr = (float *) calloc( dim_max+2, sizeof(float) );
   maparr  = (int *) calloc( dim_max, sizeof(int) );
   if( flarr == NULL || workarr == NULL|| maparr == NULL ) 
      ERROR_exit("MemAlloc failure: flarr/workarr/maparr\n");

   for( i=0 ; i<3 ; i++ ){
      /*
        This next if-condition allows us to run 2D-only EDT, if the
        user asks.
      */
      if( opts.axes_to_proc[vox_ord_rev[i]] ){

         // report on which axes are processed
         if ( !ival && opts.verb ){
            INFO_message("Move along axis %d (delta = %.6f)", 
                         vox_ord_rev[i], 
                         Ledge[vox_ord_rev[i]]);
            if( opts.ignore_voxdims )
               INFO_message("... but this will be ignored: user wants "
                            "distances output as voxel counts");
         }

         switch( vox_ord_rev[i] ){
            // note pairings per case: 0 and nx; 1 and ny; 2 and nz

         case 0 :
            if( !opts.ignore_voxdims )
               delta = fabs(DSET_DX(dset_roi)); 
            j = calc_EDT_3D_BIN_dim0( arr_dist, opts,
                                      nx, ny, nz, delta,
                                      flarr, workarr );
            if( !opts.zeros_are_zeroed )
               j = calc_EDT_3D_BIN_dim0( arr_distZ, opts,
                                         nx, ny, nz, delta,
                                         flarr, workarr );
            break;

         case 1 :
            if( !opts.ignore_voxdims )
               delta = fabs(DSET_DY(dset_roi)); 
            j = calc_EDT_3D_BIN_dim1( arr_dist, opts,
                                      nx, ny, nz, delta,
                                      flarr, workarr );
            if( !opts.zeros_are_zeroed )
               j = calc_EDT_3D_BIN_dim1( arr_distZ, opts,
                                         nx, ny, nz, delta,
                                         flarr, workarr );
            break;

         case 2 :
            if( !opts.ignore_voxdims )
               delta = fabs(DSET_DZ(dset_roi)); 
            j = calc_EDT_3D_BIN_dim2( arr_dist, opts,
                                      nx, ny, nz, delta,
                                      flarr, workarr );
           if( !opts.zeros_are_zeroed )
               j = calc_EDT_3D_BIN_dim2( arr_distZ, opts,
                                         nx, ny, nz, delta,
                                         flarr, workarr );
            break;

         default:
            WARNING_message("Should never be here in EDT prog");
         }

      }
   } // end of looping over axes

   if( flarr) free(flarr);
   if( workarr) free(workarr);
   if( maparr) free(maparr);
 
   // Apply various user post-proc options (zeroing, sign changes, etc.)
   i = apply_opts_to_edt_arr_BIN( arr_dist, arr_distZ, opts, 
                                  nx, ny, nz);

   /*
     At this point, arr_dist should have the correct distance values for
     this 3D volume.
   */

   // Copy arr_dist values to tmp_arr, which will be used to
   // populate the actual dset. 
   for( i=0 ; i<nx ; i++ ) {
      ii = i+1;
      for( j=0 ; j<ny ; j++ ) { 
         jj = j+1;
         for( k=0; k<nz ; k++ ) {
               kk = k+1;

               idx = THREE_TO_IJK(i, j, k, nx, nxy);
               tmp_arr[idx] = arr_dist[ii][jj][kk];
         }}}

   // the mask is only applied after all calcs
   if( dset_mask ){
      for( i=0 ; i<nvox ; i++ ) {
         if( !THD_get_voxel(dset_mask, i, 0))
            tmp_arr[i] = 0.0;
      }
   }

   // provide volume values from the appropriately-sized array
   EDIT_substitute_brick(dset_edt, ival, MRI_float, tmp_arr); 
   tmp_arr=NULL;

   // free arrays
   if(arr_dist){
      for ( i=0 ; i<nx2 ; i++ ) 
         for ( j=0 ; j<ny2 ; j++ ) 
            free(arr_dist[i][j]);
      for ( i=0 ; i<nx2 ; i++ ) 
         free(arr_dist[i]);
      free(arr_dist);
   }
   if(arr_distZ){
      for ( i=0 ; i<nx2 ; i++ ) 
         for ( j=0 ; j<ny2 ; j++ ) 
            free(arr_distZ[i][j]);
      for ( i=0 ; i<nx2 ; i++ ) 
         free(arr_distZ[i]);
      free(arr_distZ);
   }

   return 0;
}



// =======================================================================

int calc_EDT_3D_BIN_dim2( float ***arr_dist, PARAMS_euclid_dist opts,
                          int nx, int ny, int nz, float delta,
                          float *flarr, float *workarr )
{
   int ii, jj, kk, idx;

   ENTRY("calc_EDT_3D_BIN_dim2");

   // make appropriate 1D arrays of dist; note indices in each case
   for( ii=1 ; ii<=nx ; ii++ )
      for( jj=1 ; jj<=ny ; jj++ ) {
         for( kk=0; kk<=nz+1 ; kk++ ) 
            workarr[kk] = arr_dist[ii][jj][kk];  

         flarr = Euclidean_DT_delta(workarr, nz+2, delta);

         // ... and now put those values back into the distance arr
         for( kk=1; kk<=nz ; kk++ ) 
            arr_dist[ii][jj][kk] = flarr[kk];
      }
   
   return 0;
}

// see notes by calc_EDT_3D_BIN_dim2(...)
int calc_EDT_3D_BIN_dim1( float ***arr_dist, PARAMS_euclid_dist opts,
                          int nx, int ny, int nz, float delta,
                          float *flarr, float *workarr )
{
   int ii, jj, kk, idx;

   ENTRY("calc_EDT_3D_BIN_dim1");

   // make appropriate 1D arrays of dist; note indices in each case
   for( ii=1 ; ii<=nx ; ii++ )
      for( kk=1; kk<=nz ; kk++ ) {
         for( jj=0 ; jj<=ny+1 ; jj++ ) 
            workarr[jj] = arr_dist[ii][jj][kk];

         flarr = Euclidean_DT_delta(workarr, ny+2, delta);

         // ... and now put those values back into the distance arr
         for( jj=1; jj<=ny ; jj++ )
            arr_dist[ii][jj][kk] = flarr[jj];
      }
   
   return 0;
}

// see notes by calc_EDT_3D_BIN_dim2(...)
int calc_EDT_3D_BIN_dim0( float ***arr_dist, PARAMS_euclid_dist opts,
                          int nx, int ny, int nz, float delta,
                          float *flarr, float *workarr )
{
   int ii, jj, kk, idx;

   ENTRY("calc_EDT_3D_BIN_dim0");

   // make appropriate 1D arrays of dist; note indices in each case
   for( kk=1; kk<=nz ; kk++ ) 
      for( jj=1 ; jj<=ny ; jj++ ) {
         for( ii=0 ; ii<=nx+1 ; ii++ )
            workarr[ii] = arr_dist[ii][jj][kk];

         flarr = Euclidean_DT_delta(workarr, nx+2, delta);

         // ... and now put those values back into the distance arr
         for( ii=1; ii<=nx ; ii++ )
            arr_dist[ii][jj][kk] = flarr[ii];
      }

   return 0;
}

int apply_opts_to_edt_arr_BIN( float ***arr_dist, float ***arr_distZ, 
                               PARAMS_euclid_dist opts,
                               int nx, int ny, int nz )
{
   int i, j, k;
   int zeros_sign;
   
   ENTRY("apply_opts_to_edt_arr_BIN");

   zeros_sign = opts.zero_region_sign;
      
   // in case user calcs EDT in 2D, replace any remaining EUCLID_BIG
   // values (from initialization) with 0; do this before sqrt of
   // dist**2 or any negating of dists values, for simplicity; padded
   // edge values don't matter here
   if( opts.only2D ) {
      for ( i=1 ; i<=nx ; i++ ) 
         for ( j=1 ; j<=ny ; j++ ) 
            for ( k=1 ; k<=nz ; k++ ){
               if( arr_dist[i][j][k] >= EUCLID_BIG )
                  arr_dist[i][j][k] = 0.0;
            }
      
      if( !opts.zeros_are_zeroed ){
         for ( i=1 ; i<=nx ; i++ ) 
            for ( j=1 ; j<=ny ; j++ ) 
               for ( k=1 ; k<=nz ; k++ ){
                  if( arr_distZ[i][j][k] >= EUCLID_BIG )
                     arr_distZ[i][j][k] = 0.0;
               }
      }
   }
   
   // Output distance-squared, or just distance (sqrt of what we have
   // so-far calc'ed); padded edge values don't matter here
   if( !opts.dist_sq ) {
      for ( i=1 ; i<=nx ; i++ ) 
         for ( j=1 ; j<=ny ; j++ ) 
            for ( k=1 ; k<=nz ; k++ ){
               if( arr_dist[i][j][k] )
                  arr_dist[i][j][k] = (float) sqrt(arr_dist[i][j][k]);
            }
      
      if( !opts.zeros_are_zeroed ){
         for ( i=1 ; i<=nx ; i++ ) 
            for ( j=1 ; j<=ny ; j++ ) 
               for ( k=1 ; k<=nz ; k++ ){
                  if( arr_distZ[i][j][k] )
                     arr_distZ[i][j][k] = (float) sqrt(arr_distZ[i][j][k]);
               }
      }
   }
   
   // negative where input was zero?
   if( opts.nz_region_sign==-1 ){
      for ( i=1 ; i<=nx ; i++ ) 
         for ( j=1 ; j<=ny ; j++ ) 
            for ( k=1 ; k<=nz ; k++ ){
               if ( arr_dist[i][j][k] )
                  arr_dist[i][j][k]*= -1;
            }
   }
   
   // combine two sets of data, if zeroes weren't zeroed: they should
   // be perfectly complementary (where dist is zero in one, it should
   // be nonzero in the other, and vice versa); depending on whether
   // only2D was used, some extra values may be zeroed out, but that
   // should be fine.
   // This step also handles the sign of the zero-region distances
   if( !opts.zeros_are_zeroed ){
      for ( i=1 ; i<=nx ; i++ ) 
         for ( j=1 ; j<=ny ; j++ ) 
            for ( k=1 ; k<=nz ; k++ ){
               if( arr_distZ[i][j][k] ) 
                  arr_dist[i][j][k] = zeros_sign * arr_distZ[i][j][k];
         }
   }
      
   return 0;
}
                           






















// ---------------------------------------------------------------------------

/*
  dset_edt  :  dset that is filled in here with EDT values
  opts      :  struct containing default/user options 
  dset_roi  :  the ROI map (dataset, basically 'im' in lib_EDT.py)
  dset_mask :  a mask dset to be applied at end (could be NULL)
  ival      :  index value of the subbrick/subvolume to analyze
*/
int calc_EDT_3D_GEN( THD_3dim_dataset *dset_edt, PARAMS_euclid_dist opts,
                 THD_3dim_dataset *dset_roi, THD_3dim_dataset *dset_mask,
                 int ival)
{
   int i, j, k, idx;
   float minmax[2] = {0, 0};
   int nx, ny, nz, nxy, nvox;

   float Ledge[3];            // voxel edge lengths ('edims' in lib_EDT.py)
   int vox_ord_rev[3];

   float fldim[3];            // just want max arr len
   int dim_ord_rev[3] = {0,1,2};

   int   dim_max  = -1;
   float *flarr   = NULL;     // store distances along one dim
   float *workarr = NULL;     // has len of flarr, plus 2
   int   *maparr  = NULL;     // store ROI map along one dim

   float ***arr_dist = NULL;  // array that will hold dist values
   float *tmp_arr = NULL;

   ENTRY("calc_EDT_3D_GEN");

   // check if a subbrick is const; there are a couple cases where we
   // would be done at this stage.
   i = THD_subbrick_minmax( dset_roi, ival, 1, &minmax[0], &minmax[1] );
   if ( minmax[0] == minmax[1] ){
      if ( minmax[1] == 0.0 ){
         WARNING_message("Constant (zero) subbrick: %d\n"
                         "\t-> This volume will have all zero values", ival);
         return 0;
      }
      else if( !opts.bounds_are_zero ){
         WARNING_message("Constant (nonzero) subbrick, "
                         "and no bounds_are_zero: %d\n"
                         "\t-> This volume will have all zero values", ival);
         return 0;
      }
   }

   // dset properties we need to know
   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   nxy = nx*ny;
   nvox = DSET_NVOX(dset_roi);
   Ledge[0] = fabs(DSET_DX(dset_roi)); 
   Ledge[1] = fabs(DSET_DY(dset_roi)); 
   Ledge[2] = fabs(DSET_DZ(dset_roi)); 

   // create arrays to be used
   tmp_arr = (float *) calloc( nvox, sizeof(float) );
   arr_dist = (float ***) calloc( nx, sizeof(float **) );
   for ( i=0 ; i<nx ; i++ ) 
      arr_dist[i] = (float **) calloc( ny, sizeof(float *) );
   for ( i=0 ; i<nx ; i++ ) 
      for ( j=0 ; j<ny ; j++ ) 
         arr_dist[i][j] = (float *) calloc( nz, sizeof(float) );
   if( tmp_arr == NULL || arr_dist == NULL ) {
      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
      exit(12);
   }

   // initialize distance array to EUCLID_BIG
   // PT self-question: is this init necessary in this case?
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ) 
               arr_dist[i][j][k] = EUCLID_BIG;

   // find axis order of decreasing voxel sizes, to avoid pathology in
   // the EDT alg (that miiiight have only existed in earlier calc
   // method, but it still makes sense to do---why not?)
   i = sort_vox_ord_desc(3, Ledge, vox_ord_rev); 

   // len of longest dset dim, so we don't have to keep
   // allocating/freeing
   fldim[0] = nx;
   fldim[1] = ny;
   fldim[2] = nz;
   qsort_floatint( 3 , fldim , dim_ord_rev );
   dim_max = (int) fldim[2];
   flarr   = (float *) calloc( dim_max, sizeof(float) );
   workarr = (float *) calloc( dim_max+2, sizeof(float) );
   maparr  = (int *) calloc( dim_max, sizeof(int) );
   if( flarr == NULL || workarr == NULL|| maparr == NULL ) 
      ERROR_exit("MemAlloc failure: flarr/workarr/maparr\n");

   for( i=0 ; i<3 ; i++ ){
      /*
        This next if-condition allows us to run 2D-only EDT, if the
        user asks.
      */
      if( opts.axes_to_proc[vox_ord_rev[i]] ){

         // report on which axes are processed
         if ( !ival && opts.verb ){
            INFO_message("Move along axis %d (delta = %.6f)", 
                         vox_ord_rev[i], 
                         Ledge[vox_ord_rev[i]]);
            if( opts.ignore_voxdims )
               INFO_message("... but this will be ignored: user wants "
                            "distances output as voxel counts");
         }

         switch( vox_ord_rev[i] ){
            // note pairings per case: 0 and nx; 1 and ny; 2 and nz

         case 0 :
            j = calc_EDT_3D_GEN_dim0( arr_dist, opts, dset_roi, ival, 
                                      flarr, workarr, maparr );
            break;

         case 1 :
            j = calc_EDT_3D_GEN_dim1( arr_dist, opts, dset_roi, ival, 
                                      flarr, workarr, maparr );
            break;

         case 2 :
            j = calc_EDT_3D_GEN_dim2( arr_dist, opts, dset_roi, ival, 
                                      flarr, workarr, maparr );
            break;

         default:
            WARNING_message("Should never be here in EDT prog");
         }

      }
   } // end of looping over axes

   if( flarr) free(flarr);
   if( workarr) free(workarr);
   if( maparr) free(maparr);

   // Apply various user post-proc options (zeroing, sign changes, etc.)
   i = apply_opts_to_edt_arr_GEN( arr_dist, opts, dset_roi, ival);

   /*
     At this point, arr_dist should have the correct distance values for
     this 3D volume.
   */

   // Copy arr_dist values to tmp_arr, which will be used to
   // populate the actual dset.  
   for( i=0 ; i<nx ; i++ )
      for( j=0 ; j<ny ; j++ ) 
         for( k=0; k<nz ; k++ ) {
            idx = THREE_TO_IJK(i, j, k, nx, nxy);
            tmp_arr[idx] = arr_dist[i][j][k];
         }

   // the mask is only applied after all calcs
   if( dset_mask ){
      for( i=0 ; i<nvox ; i++ ) {
         if( !THD_get_voxel(dset_mask, i, 0))
            tmp_arr[i] = 0.0;
      }
   }
      
   // provide volume values from the appropriately-sized array
   EDIT_substitute_brick(dset_edt, ival, MRI_float, tmp_arr); 
   tmp_arr=NULL;

   if(arr_dist){
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            free(arr_dist[i][j]);
      for ( i=0 ; i<nx ; i++ ) 
         free(arr_dist[i]);
      free(arr_dist);
   }

   return 0;
}

// ---------------------------------------------------------------------------

/*
  calc_EDT_3D_GEN_dim?(...) are a set of 3 functions that manage walking
  along each of the axes: calc_EDT_3D_dim2(...)  does calcs along the
  [2]th axis, etc. 

  In each, there are "hardwired" aspects for each dimension: loop
  order, using index [0], using 'nx', etc.  So be very careful if you
  want to edit these: make sure you don't forget to get the
  appropriate axis in each.  Likely, each should be edited in parallel.

  By construction, flarr has the correct length for each of the
  dimension calc_EDT_3D_GEN_dim?(...)  funcs from how these funcs are called.

  arr_dist       :3D float array of distances (gets edited/updated here)
  opts           :struct of opts, from default/user specification
  dset_roi       :input map of ROIs, guides the process
  ival           :subbrick/subvolume index (dset_roi could be 4D)
  flarr          :1D array of distances, getting updated in this specific func
                  (from which arr_dist gets updated)
  workarr        :1D array of floats; gets constantly overwritten; has len
                  2+(max dataset dim), so we don't worry about array inds
  maparr         :1D array of ROIs, matched with flarr to guide its calcs
*/

// analyzing along [2]th dimension
int calc_EDT_3D_GEN_dim2( float ***arr_dist, PARAMS_euclid_dist opts,
                          THD_3dim_dataset *dset_roi, int ival,
                          float *flarr, float *workarr, int *maparr )
{
   int ii, jj, kk, idx, ll;
   int nx, ny, nz, nxy;
   float delta = 1.0;    // voxel edge length ('edims' element in lib_EDT.py)

   ENTRY("calc_EDT_3D_GEN_dim2");

   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   nxy = nx*ny;

   if( !opts.ignore_voxdims )
      delta = fabs(DSET_DZ(dset_roi)); 
   
   // make appropriate 1D arrays of dist and ROI maps
   for( ii=0 ; ii<nx ; ii++ )
      for( jj=0 ; jj<ny ; jj++ ) {
         for( kk=0; kk<nz ; kk++ ) {
            idx = THREE_TO_IJK(ii, jj, kk, nx, nxy);
            flarr[kk] = arr_dist[ii][jj][kk];  // note index on flarr
            maparr[kk] = (int) THD_get_voxel(dset_roi, idx, ival);
            }

         // update distance along this 1D line...
         ll = run_EDTD_GEN_per_line( flarr, workarr, maparr, nz, delta,
                                     opts.bounds_are_zero, opts.binary_only );

         // ... and now put those values back into the distance arr
         for( kk=0; kk<nz ; kk++ ) 
            arr_dist[ii][jj][kk] = flarr[kk];
      }

   return 0;
}

// see notes by calc_EDT_3D_GEN_dim2(...)
int calc_EDT_3D_GEN_dim1( float ***arr_dist, PARAMS_euclid_dist opts,
                          THD_3dim_dataset *dset_roi, int ival,
                          float *flarr, float *workarr, int *maparr )
{
   int ii, jj, kk, idx, ll;
   int nx, ny, nz, nxy;
   float delta = 1.0;    // voxel edge length ('edims' element in lib_EDT.py)

   ENTRY("calc_EDT_3D_GEN_dim1");

   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   nxy = nx*ny;

   if( !opts.ignore_voxdims )
      delta = fabs(DSET_DY(dset_roi)); 

   // make appropriate 1D arrays of dist and ROI maps
   for( ii=0 ; ii<nx ; ii++ )
      for( kk=0; kk<nz ; kk++ ) {
         for( jj=0 ; jj<ny ; jj++ ) {
            idx = THREE_TO_IJK(ii, jj, kk, nx, nxy);
            flarr[jj] = arr_dist[ii][jj][kk];  // note index on flarr
            maparr[jj] = (int) THD_get_voxel(dset_roi, idx, ival);
         }

         // update distance along this 1D line...
         ll = run_EDTD_GEN_per_line( flarr, workarr, maparr, ny, delta, 
                                     opts.bounds_are_zero, opts.binary_only );

         // ... and now put those values back into the distance arr
         for( jj=0; jj<ny ; jj++ )
            arr_dist[ii][jj][kk] = flarr[jj];
      }

   return 0;
}

// see notes by calc_EDT_3D_GEN_dim2(...)
int calc_EDT_3D_GEN_dim0( float ***arr_dist, PARAMS_euclid_dist opts,
                          THD_3dim_dataset *dset_roi, int ival,
                          float *flarr, float *workarr, int *maparr )
{
   int ii, jj, kk, idx, ll;
   int nx, ny, nz, nxy;
   float delta = 1.0;    // voxel edge length ('edims' element in lib_EDT.py)

   ENTRY("calc_EDT_3D_GEN_dim0");

   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   nxy = nx*ny;

   if( !opts.ignore_voxdims )
      delta = fabs(DSET_DX(dset_roi)); 

   // make appropriate 1D arrays of dist and ROI maps
   for( kk=0; kk<nz ; kk++ ) 
      for( jj=0 ; jj<ny ; jj++ ) {
         for( ii=0 ; ii<nx ; ii++ ) {
            idx = THREE_TO_IJK(ii, jj, kk, nx, nxy);
            flarr[ii] = arr_dist[ii][jj][kk];  // note index on flarr
            maparr[ii] = (int) THD_get_voxel(dset_roi, idx, ival);
         }

         // update distance along this 1D line...
         ll = run_EDTD_GEN_per_line( flarr, workarr, maparr, nx, delta, 
                                     opts.bounds_are_zero, opts.binary_only );

         // ... and now put those values back into the distance arr
         for( ii=0; ii<nx ; ii++ )
            arr_dist[ii][jj][kk] = flarr[ii];
      }

   return 0;
}

// -----------------------------------------------------------------------

/*
  Manage how each line gets chunked and handed in pieces to the actual
  EDT-calculating function.

  dist2_line      :float array of 'Na' distance values (gets updated here)
  warr            :just a work array, so we don't keep allocating/freeing;
                   len of warr is 2+(max dset dim), so >=2+Na
  roi_line        :int array of 'Na' ROI labels (unchanged)
  Na              :length of 1D arrays used here
  delta           :voxel dim along this 1D array
  bounds_are_zero :option for how to treat FOV boundaries for nonzero ROIs
  binary_only     :if we treat the ROI map as a binary map only
*/
int run_EDTD_GEN_per_line( float *dist2_line, float *warr, int *roi_line, 
                           int Na, float delta, 
                           int bounds_are_zero, int binary_only )
{
   int  idx = 0;
   int  i, m, n;
   float *line_out = NULL;
   int start, stop, inc, roi;
   int npts;

   float *Df = NULL;

   int limit = Na-1;

   Df   = (float *) calloc( Na, sizeof(float) );


   while (idx < Na){
      // get interval of line with current ROI value
      roi = roi_line[idx];
      n = idx;
      while (n < Na){
         if (roi_line[n] != roi){
            break;
         }
         n += 1;
      }
      n -= 1;

      /*
        Now, n stores the index of last matching element; idx stores
        the index of first matching element.  

        Here and below, pay careful attention to indices and offsets
        (offsets used so we can apply boundary conditions to each
        segment of the line).
      */

      npts = n - idx + 1; // bc line interval of line is [n, idx]

      // copy dist values in this interval; note warr index is offset
      // by 1 here
      for( m=idx ; m<=n ; m++ )
         warr[m+1] = dist2_line[m];

      // left bound
      if( idx==0 ){ // on the FOV edge
         if(roi != 0 && bounds_are_zero)
            warr[idx] = 0; // a change of ROI
         else
            warr[idx] = EUCLID_BIG; // pretend like ROI keeps going
      }
      else // inside FOV
         warr[idx] = 0; // a change of ROI

      // right bound
      if( n==limit ){ // on the FOV edge
         if(roi != 0 && bounds_are_zero)
            warr[n+2] = 0; // a change of ROI
         else
            warr[n+2] = EUCLID_BIG; // pretend like ROI keeps going
      }
      else // inside FOV
         warr[n+2] = 0; // a change of ROI

      // now calc EDT starting from appropriate spot in warr
      Df = Euclidean_DT_delta(warr+idx, npts+2, delta);

      // copy dist values in this interval
      for( m=0 ; m<npts ; m++ )
         dist2_line[idx+m] = Df[m+1];

      idx = n+1;
   }

   if( Df )
      free(Df);
   
   return 0;
}

// -----------------------------------------------------------------------

/*
  *Finally*, the actual EDT calculation.

  Classical Euclidean Distance Transform (EDT) of Felzenszwalb and
  Huttenlocher (2012), but for given voxel lengths.
  
  Assumes that: len(f) < sqrt(10**10).
  
  In this version, all voxels should have equal length, and units are
  "edge length" or "number of voxels."
  
  Parameters
  ----------
  
  f0       : 1D array. Either distance**2 values (or, to start,
             values binarized to 0 or EUCLID_BIG).
  
  n        : len of f0 array

  delta    : element edge length along this dimension.
  
  To deal with anisotropic and non-unity-edge-length elements, first
  scale non-EUCLID_BIG distances to be "as if" there were edge=1 voxels, and
  then at end scale back.
  
  [PT] Comment: unlike in earlier thinking (even before current scale
  down/up approach), do NOT want to mult 'EUCLID_BIG' by 'delta', because
  pixels/voxels can be anisotropic here.
*/
float * Euclidean_DT_delta(float *f0, int n, float delta)
{

   int i, q;
   float s;
   float delta2;

   int k = 0;
   int *v = NULL;
   float *z = NULL, *f = NULL;
   float *Df = NULL, *Df0 = NULL;

   v = (int *)calloc(n, sizeof(int));
   f = (float *)calloc(n, sizeof(float));
   z = (float *)calloc(n+1, sizeof(float));
   Df = (float *)calloc(n, sizeof(float));
   Df0 = (float *)calloc(n, sizeof(float));
   if ( v==NULL || f==NULL || z==NULL || Df==NULL || Df0==NULL ) 
      ERROR_exit("MemAlloc issue: v, f, z, Df or Df0.\n");

   z[0] = -EUCLID_BIG;
   z[1] = EUCLID_BIG;

   delta2 = delta * delta;

    // Scale down, if not using unity element
    for( i=0 ; i<n ; i++ )
       f[i] = f0[i];           // copy f0  
    if( delta != 1 ){
        for( i=0 ; i<n ; i++ )
            if(f[i] != EUCLID_BIG )
                f[i]/= delta2;
        }

    for( q=1 ; q<n ; q++ ){
       s = (f[q] + q*q) - (f[v[k]] + v[k]*v[k]);
       s/= 2. * (q - v[k]);

       while ( s <= z[k] ){
          k--;
          s = (f[q] + q*q) - (f[v[k]] + v[k]*v[k]);
          s/= 2. * (q - v[k]);
       }

       k++;
       v[k]   = q;
       z[k]   = s;
       z[k+1] = EUCLID_BIG;
    }

    k = 0;
    for( q=0 ; q<n ; q++ ){
       while( z[k+1] < q )
          k++;
       Df[q] = (q - v[k])*(q - v[k]) + f[v[k]];
    }

    // Scale back up, if not using unity element
    for( i=0 ; i<n ; i++ )
       Df0[i] = Df[i];           // copy Df  
    if (delta != 1 ){
       for( i=0 ; i<n ; i++ )
            if( Df0[i] != EUCLID_BIG )
                Df0[i]*= delta2;
    }

    if( v )
       free(v);
    if( f )
       free(f);
    if( z )
       free(z);
    if( Df )
       free(Df);

    return Df0;
}



// ---------------------------------------------------------------------------

/*
  Basically, walk through the array many times and apply various
  rescalings/negations/zeroings, the options commandeth.

  arr_dist :  3D distance array of floats ('odt' in lib_EDT.py)
              -> what has been filled in here
  opts     :  struct containing default/user options 
  dset_roi :  the ROI map (dataset, basically 'im' in lib_EDT.py)
  ival     :  index value of the subbrick/subvolume to analyze

*/
int apply_opts_to_edt_arr_GEN( float ***arr_dist, PARAMS_euclid_dist opts,
                           THD_3dim_dataset *dset_roi, int ival)
{
   int i, j, k, idx;
   int nx, ny, nz, nxy;

   ENTRY("apply_opts_to_edt_arr_GEN");

   // dset properties we need to know
   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   nxy = nx*ny;

   // in case user calcs EDT in 2D, replace any remaining EUCLID_BIG values
   // (from initialization) with 0; do this before sqrt of dist**2 or
   // any negating of dists values, for simplicity
   if( opts.only2D ){
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ){
               if( arr_dist[i][j][k] >= EUCLID_BIG )
                  arr_dist[i][j][k] = 0.0;
               }
   }

   // Zero out EDT values in "zero" ROI?
   if( opts.zeros_are_zeroed ) {
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ){
               idx = THREE_TO_IJK(i, j, k, nx, nxy);
               if( !THD_get_voxel(dset_roi, idx, ival) )
                  arr_dist[i][j][k] = 0.0;
            }
   } 
 
   // Output distance-squared, or just distance (sqrt of what we have
   // so-far calc'ed)
   if( !opts.dist_sq ) {
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ){
               arr_dist[i][j][k] = (float) sqrt(arr_dist[i][j][k]);
            }
   }
   
   // negative where input was zero?
   if( opts.zero_region_sign == -1 ) { 
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ){
               idx = THREE_TO_IJK(i, j, k, nx, nxy);
               if( !THD_get_voxel(dset_roi, idx, ival) )
                  arr_dist[i][j][k]*= -1.0;
            }
   }

   // negative where input was nonzero?
   if( opts.nz_region_sign == -1 ) { 
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ){
               idx = THREE_TO_IJK(i, j, k, nx, nxy);
               if( THD_get_voxel(dset_roi, idx, ival) )
                  arr_dist[i][j][k]*= -1.0;
            }
   }

   return 0;
}
                           


