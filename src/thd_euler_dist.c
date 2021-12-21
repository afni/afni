#include "mrilib.h" 

PARAMS_euler_dist set_euler_dist_defaults(void)
{

   PARAMS_euler_dist defopt;

   defopt.input_name = NULL;     
   defopt.mask_name = NULL;     
   defopt.prefix = NULL;         

   defopt.zeros_are_zeroed = 0;
   defopt.zeros_are_neg    = 0;
   defopt.nz_are_neg       = 0;
   defopt.bounds_are_zero  = 1;
   defopt.ignore_voxdims   = 0;
   defopt.dist_sq          = 0;

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

// ---------------------------------------------------------------------------

/*
  dset_edt  :  dset that is filled in here with EDT values
  opts      :  struct containing default/user options 
  dset_roi  :  the ROI map (dataset, basically 'im' in lib_EDT.py)
  dset_mask :  a mask dset to be applied at end (could be NULL)
  ival      :  index value of the subbrick/subvolume to analyze
*/
int calc_EDT_3D( THD_3dim_dataset *dset_edt, PARAMS_euler_dist opts,
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

   ENTRY("calc_EDT_3D");

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

   // initialize distance array to EULER_BIG
   if( opts.binary_only ){
      /*
        Treat input dset_roi as a binary mask, and initialize
        EULER_BIG where it is nonzero
      */
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ) {
               idx = THREE_TO_IJK(i, j, k, nx, nxy);
               if( THD_get_voxel(dset_roi, ival, idx))
                  arr_dist[i][j][k] = EULER_BIG;
            }
   }
   else { // PT self-question: is this init necessary in this case?
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ) 
               arr_dist[i][j][k] = EULER_BIG;
   }

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
        This if condition allows us to run 2D-only EDT, if the user
        asks.
       */
      if( opts.axes_to_proc[vox_ord_rev[i]] ){
         if ( !ival && opts.verb ){
            INFO_message("Move along axis %d (delta = %.6f)", 
                         vox_ord_rev[i], 
                         Ledge[vox_ord_rev[i]]);
            if( opts.ignore_voxdims )
               INFO_message("... but this will be ignored,"
                            "at user behest");
         }

         switch( vox_ord_rev[i] ){
            // note pairings per case: 0 and nx; 1 and ny; 2 and nz

         case 0 :
            if( opts.binary_only ){
               INFO_message("TEMP: case 0");
            }
            else
               j = calc_EDT_3D_dim0( arr_dist, opts, dset_roi, ival, 
                                     flarr, workarr, maparr );
            break;

         case 1 :
            if( opts.binary_only ){
               INFO_message("TEMP: case 1");
            }
            else
               j = calc_EDT_3D_dim1( arr_dist, opts, dset_roi, ival, 
                                     flarr, workarr, maparr );
            break;

         case 2 :
            if( opts.binary_only ){
               INFO_message("TEMP: case 2");
            }
            else
               j = calc_EDT_3D_dim2( arr_dist, opts, dset_roi, ival, 
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

   // Apply various user post-proc options
   i = apply_opts_to_edt_arr( arr_dist, opts, dset_roi, ival);

   /*
     At this point, arr_dist should have the correct distance values for
     this 3D volume.
   */

   // Copy arr_dist values to tmp_arr, which will be used to
   // populate the actual dset.  Then reset arr_dist values (to
   // prepare for another iteration)
   for( i=0 ; i<nx ; i++ )
      for( j=0 ; j<ny ; j++ ) 
         for( k=0; k<nz ; k++ ) {
            idx = THREE_TO_IJK(i, j, k, nx, nxy);
            tmp_arr[idx] = arr_dist[i][j][k];
            arr_dist[i][j][k] = 0.0;
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
  calc_EDT_3D_dim?(...) are a set of 3 functions that manage walking
  along each of the axes: calc_EDT_3D_dim2(...)  does calcs along the
  [2]th axis, etc. 

  In each, there are "hardwired" aspects for each dimension: loop
  order, using index [0], using 'nx', etc.  So be very careful if you
  want to edit these: make sure you don't forget to get the
  appropriate axis in each.  Likely, each should be edited in parallel.

  By construction, flarr has the correct length for each of the
  dimension calc_EDT_3D_dim?(...)  funcs from how these funcs are called.

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
int calc_EDT_3D_dim2( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, float *workarr, int *maparr )
{
   int ii, jj, kk, idx, ll;
   int nx, ny, nz, nxy;
   float delta = 1.0;    // voxel edge length ('edims' element in lib_EDT.py)

   ENTRY("calc_EDT_3D_dim2");

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
         ll = run_EDTD_per_line( flarr, workarr, maparr, nz, delta,
                                 opts.bounds_are_zero, opts.binary_only );

         // ... and now put those values back into the distance arr
         for( kk=0; kk<nz ; kk++ ) 
            arr_dist[ii][jj][kk] = flarr[kk];
      }

   return 0;
}

// see notes by calc_EDT_3D_dim2(...)
int calc_EDT_3D_dim1( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, float *workarr, int *maparr )
{
   int ii, jj, kk, idx, ll;
   int nx, ny, nz, nxy;
   float delta = 1.0;    // voxel edge length ('edims' element in lib_EDT.py)

   ENTRY("calc_EDT_3D_dim1");

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
         ll = run_EDTD_per_line( flarr, workarr, maparr, ny, delta, 
                                 opts.bounds_are_zero, opts.binary_only );

         // ... and now put those values back into the distance arr
         for( jj=0; jj<ny ; jj++ )
            arr_dist[ii][jj][kk] = flarr[jj];
      }

   return 0;
}

// see notes by calc_EDT_3D_dim2(...)
int calc_EDT_3D_dim0( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, float *workarr, int *maparr )
{
   int ii, jj, kk, idx, ll;
   int nx, ny, nz, nxy;
   float delta = 1.0;    // voxel edge length ('edims' element in lib_EDT.py)

   ENTRY("calc_EDT_3D_dim0");

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
         ll = run_EDTD_per_line( flarr, workarr, maparr, nx, delta, 
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
int run_EDTD_per_line( float *dist2_line, float *warr, int *roi_line, 
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
            warr[idx] = EULER_BIG; // pretend like ROI keeps going
      }
      else // inside FOV
         warr[idx] = 0; // a change of ROI

      // right bound
      if( n==limit ){ // on the FOV edge
         if(roi != 0 && bounds_are_zero)
            warr[n+2] = 0; // a change of ROI
         else
            warr[n+2] = EULER_BIG; // pretend like ROI keeps going
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
             values binarized to 0 or EULER_BIG).
  
  n        : len of f0 array

  delta    : element edge length along this dimension.
  
  To deal with anisotropic and non-unity-edge-length elements, first
  scale non-EULER_BIG distances to be "as if" there were edge=1 voxels, and
  then at end scale back.
  
  [PT] Comment: unlike in earlier thinking (even before current scale
  down/up approach), do NOT want to mult 'EULER_BIG' by 'delta', because
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

   z[0] = -EULER_BIG;
   z[1] = EULER_BIG;

   delta2 = delta * delta;

    // Scale down, if not using unity element
    for( i=0 ; i<n ; i++ )
       f[i] = f0[i];           // copy f0  
    if( delta != 1 ){
        for( i=0 ; i<n ; i++ )
            if(f[i] != EULER_BIG )
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
       z[k+1] = EULER_BIG;
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
            if( Df0[i] != EULER_BIG )
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
int apply_opts_to_edt_arr( float ***arr_dist, PARAMS_euler_dist opts,
                           THD_3dim_dataset *dset_roi, int ival)
{
   int i, j, k, idx;
   int nx, ny, nz, nxy;

   ENTRY("apply_opts_to_edt_arr");

   // dset properties we need to know
   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   nxy = nx*ny;

   // in case user calcs EDT in 2D, replace any remaining EULER_BIG values
   // (from initialization) with 0; do this before sqrt of dist**2 or
   // any negating of dists values, for simplicity
   if( opts.only2D ){
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ){
               if( arr_dist[i][j][k] >= EULER_BIG )
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
   if( opts.zeros_are_neg ) { 
      for ( i=0 ; i<nx ; i++ ) 
         for ( j=0 ; j<ny ; j++ ) 
            for ( k=0 ; k<nz ; k++ ){
               idx = THREE_TO_IJK(i, j, k, nx, nxy);
               if( !THD_get_voxel(dset_roi, idx, ival) )
                  arr_dist[i][j][k]*= -1.0;
            }
   }

   // negative where input was nonzero?
   if( opts.nz_are_neg ) { 
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
                           


