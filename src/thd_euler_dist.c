#include "mrilib.h" 
#include "thd_euler_dist.h"

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
   defopt.do_sqrt          = 1;           

   defopt.edims[0] = 0.0;       
   defopt.edims[1] = 0.0;
   defopt.edims[2] = 0.0;
   defopt.shape[0] = 0; 
   defopt.shape[1] = 0; 
   defopt.shape[2] = 0; 

   defopt.verb = 1;

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

   float ***arr_dist = NULL;           // array that will hold dist values
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

   // initialize distance array to BIG
   for ( i=0 ; i<nx ; i++ ) 
      for ( j=0 ; j<ny ; j++ ) 
         for ( k=0 ; k<nz ; k++ ) 
            arr_dist[i][j][k] = BIG;

   // find axis order of decreasing voxel sizes, to avoid pathology in
   // the EDT alg (that miiiight have only existed in earlier calc
   // method, but it still makes sense to do---why not?)
   i = sort_vox_ord_desc(3, Ledge, vox_ord_rev); 

   for( i=0 ; i<3 ; i++ ){
      float *flarr=NULL;   // store distances along one dim
      int *maparr=NULL;    // store ROI map along one dim

      if ( !ival && opts.verb ){
         INFO_message("Move along axis %d (delta = %.6f)", 
                      vox_ord_rev[i], 
                      Ledge[vox_ord_rev[i]]);
         if( opts.ignore_voxdims )
            INFO_message("... but this will be ignored, at user behest");
      }

      switch( vox_ord_rev[i] ){
         // note pairings per case: 0 and nx; 1 and ny; 2 and nz

      case 0 :
         flarr = (float *) calloc( nx, sizeof(float) );
         maparr = (int *) calloc( nx, sizeof(int) );
         if( flarr == NULL || maparr == NULL ) 
            ERROR_exit("MemAlloc failure: flarr/maparr\n");
         
         j = calc_EDT_3D_dim0( arr_dist, opts, dset_roi, ival, 
                               flarr, maparr );
         break;

      case 1 :
         flarr = (float *) calloc( ny, sizeof(float) );
         maparr = (int *) calloc( ny, sizeof(int) );
         if( flarr == NULL || maparr == NULL ) 
            ERROR_exit("MemAlloc failure: flarr/maparr\n");

         j = calc_EDT_3D_dim1( arr_dist, opts, dset_roi, ival, 
                               flarr, maparr );
         break;

      case 2 :
         flarr = (float *) calloc( nz, sizeof(float) );
         maparr = (int *) calloc( nz, sizeof(int) );
         if( flarr == NULL || maparr == NULL ) 
            ERROR_exit("MemAlloc failure: flarr/maparr\n");

         j = calc_EDT_3D_dim2( arr_dist, opts, dset_roi, ival, 
                               flarr, maparr );
         break;

      default:
         WARNING_message("Should never be here in EDT prog");
      }

      if( flarr) free(flarr);
      if( maparr) free(maparr);
   } // end of looping over axes

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
  maparr         :1D array of ROIs, matched with flarr to guide its calcs
*/

// analyzing along [2]th dimension
int calc_EDT_3D_dim2( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, int *maparr )
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
         ll = run_EDTD_per_line( flarr, maparr, nz,
                                 delta, opts.bounds_are_zero );

         // ... and now put those values back into the distance arr
         for( kk=0; kk<nz ; kk++ ) 
            arr_dist[ii][jj][kk] = flarr[kk];
      }

   return 0;
}

// see notes by calc_EDT_3D_dim2(...)
int calc_EDT_3D_dim1( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, int *maparr )
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
         ll = run_EDTD_per_line( flarr, maparr, ny,
                                 delta, opts.bounds_are_zero );

         // ... and now put those values back into the distance arr
         for( jj=0; jj<ny ; jj++ )
            arr_dist[ii][jj][kk] = flarr[jj];
      }

   return 0;
}

// see notes by calc_EDT_3D_dim2(...)
int calc_EDT_3D_dim0( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, int *maparr )
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
         ll = run_EDTD_per_line( flarr, maparr, nx,
                                 delta, opts.bounds_are_zero );

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
  roi_line        :int array of 'Na' ROI labels (unchanged)
  Na              :length of 1D arrays used here
  delta           :voxel dim along this 1D array
  bounds_are_zero :option for how to treat FOV boundaries for nonzero ROIs
*/
int run_EDTD_per_line( float *dist2_line, int *roi_line, int Na,
                       float delta, int bounds_are_zero )
{
   int  idx = 0;
   int  i, m, n;
   float *line_out = NULL;
   int start, stop, inc, roi;
   
   float *Df = NULL;
   
   int limit = Na-1;
   size_t  rowLengthInBytes = Na*sizeof(float);
   //printf("---%f---",delta);
   //ENTRY("run_EDTD_per_line");

   if (!(line_out=(float *)malloc(rowLengthInBytes)))
      ERROR_exit("Memory allocation problem: line_out");
   
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
      // n now has the index of last matching element

      float *paddedLine=(float *)calloc(Na+2,sizeof(float));
      // actual ROI is in range of indices [start, stop] in
      // paddedLine.  'inc' will tell us length of distance array
      // put into Euclidean_DT_delta(), which can include padding at
      // either end.
      start = 0;
      stop  = limit; 
      inc   = 0;

      if (idx != 0 || (bounds_are_zero && roi != 0)){
         start = 1;
         inc = 1;
      }
      // put actual values from dist**2 field...
      for ( m=idx; m<=n; ++m ){
         paddedLine[inc++] = dist2_line[m];
      }
      // inc finishes 1 greater than the actual end: is length so far
      stop = inc-1;  // 'stop' is index of actual end of roi
      // pad at end? 
      if (n < limit || (bounds_are_zero && roi != 0)){
         inc+=1;
      }

      // [PT] and 'inc' should already have correct value from
      // above; don't add 1 here
      Df = Euclidean_DT_delta(paddedLine, inc, delta);

      memcpy(&(line_out[idx]), &(Df[start]), (stop-start+1)*sizeof(float));

      if( paddedLine )
         free(paddedLine);
      if( Df )
         free(Df);

      idx = n+1;
   }

   // DEBUG
   for ( i=0; i<Na; ++i ) 
      if (line_out[i]==0){
         fprintf(stderr, "Zero valued distance\n");
      }
   
   memcpy(dist2_line, line_out, rowLengthInBytes);
   if( line_out )
      free(line_out);
   
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
             values binarized to 0 or BIG).
  
  n        : len of f0 array

  delta    : element edge length along this dimension.
  
  To deal with anisotropic and non-unity-edge-length elements, first
  scale non-BIG distances to be "as if" there were edge=1 voxels, and
  then at end scale back.
  
  [PT] Comment: unlike in earlier thinking (even before current scale
  down/up approach), do NOT want to mult 'BIG' by 'delta', because
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

   z[0] = -BIG;
   z[1] = BIG;

   delta2 = delta * delta;

    // Scale down, if not using unity element
    for( i=0 ; i<n ; i++ )
       f[i] = f0[i];           // copy f0  
    if( delta != 1 ){
        for( i=0 ; i<n ; i++ )
            if(f[i] != BIG )
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
       z[k+1] = BIG;
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
            if( Df0[i] != BIG )
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
   if( opts.do_sqrt ) {
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
                           


