#include "mrilib.h" 
#include "thd_euler_dist.h"

PARAMS_euler_dist set_euler_dist_defaults(void)
{

   PARAMS_euler_dist defopt;

   defopt.input_name = NULL;     
   defopt.prefix = NULL;         

   defopt.zeros_are_zeroed = 0;  
   defopt.bounds_are_zero = 1;   
   defopt.do_sqrt = 1;           

   defopt.edims[0] = 0.0;       
   defopt.edims[1] = 0.0;
   defopt.edims[2] = 0.0;

   defopt.shape[0] = 0; 
   defopt.shape[1] = 0; 
   defopt.shape[2] = 0; 

   return defopt;
};

// ---------------------------------------------------------------------------

/*
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
  By construction, flarr has the correct length for each of the
  dimension calc_EDT_3D_dim?(...) funcs; the calc_EDT_3D_dim2(...)
  does calcs along the [2]th axis, etc.
*/

// analyzing along [2]th dimension
int calc_EDT_3D_dim2( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, int *maparr )
{
   int ii, jj, kk, idx, ll;
   int nx, ny, nz, nxy;
   float delta;            // voxel edge length ('edims' element in lib_EDT.py)

   ENTRY("calc_EDT_3D_dim2");

   nx = DSET_NX(dset_roi);
   ny = DSET_NY(dset_roi);
   nz = DSET_NZ(dset_roi);
   nxy = nx*ny;

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
         for( kk=0; kk<nz ; kk++ ) {
            arr_dist[ii][jj][kk] = flarr[kk];
            //if( arr_dist[ii][jj][kk] < 100000 )
              // printf("||%d, %d: %.2f||", ii, jj, arr_dist[ii][jj][kk]);
         }
      }

   return 0;
}


int calc_EDT_3D_dim1( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, int *maparr )
{

   ENTRY("calc_EDT_3D_dim1");

   return 0;
}

int calc_EDT_3D_dim0( float ***arr_dist, PARAMS_euler_dist opts,
                      THD_3dim_dataset *dset_roi, int ival,
                      float *flarr, int *maparr )
{
   
   ENTRY("calc_EDT_3D_dim0");


   return 0;
}

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

float * Euclidean_DT_delta(float *f, int n, float delta)
{
   //    Classical Euclidean Distance Transform (EDT) of Felzenszwalb and
   //        Huttenlocher (2012), but for given voxel lengths.
   //
   //    Assumes that: len(f) < sqrt(10**10).
   //
   //    In this version, all voxels should have equal length, and units
   //    are "edge length" or "number of voxels."
   //
   //    Parameters
   //    ----------
   //
   //    f     : 1D array or list, distance**2 values (or, to start, binarized
   //    between 0 and BIG).
   //
   //    delta : voxel edge length size along a particular direction

   int q;
   int *v=NULL;
   int k = 0;
   float *z=NULL, *Df=NULL;
   float s;

   if (!(v=(int *)calloc(n, sizeof(int))) ||
       !(z=(float *)calloc(n+1, sizeof(float)))){
      if (v) free(v);
      return NULL;
   }
   z[0] = -BIG;
   z[1] =  BIG;

   for ( q = 1; q<n; ++q ) {
      s = f[q] + pow(q*delta, 2.0) - (f[v[k]] + pow(v[k]*delta,2.0));
      s/= 2. * delta * (q - v[k]);
      while (s <= z[k]){
         k-= 1;
         s = f[q] + pow(q*delta,2.0) - (f[v[k]] + pow(v[k]*delta, 2.0));
         s/= 2. * delta * (q - v[k]);
      }
      k+= 1;
      v[k]   = q;
      z[k]   = s;
      z[k+1] = BIG;
   }

   k   = 0;
   if (!(Df=(float *)calloc(n, sizeof(float)))){
      free(v);
      free(z);
      return NULL;
   }
   for ( q=0; q<n; ++q ){
      while (z[k+1] < q * delta) k+= 1;
      Df[q] = pow(delta*(q - v[k]), 2.0) + f[v[k]];
   }

   if( v )
      free(v);
   if( z )
      free(z);

   return Df;
}
