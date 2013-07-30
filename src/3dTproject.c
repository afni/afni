#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*----------------------------------------------------------------------------*/

void TPR_help(void){
  printf(
   "\n"
   "Usage:  3dTproject [options] dataset\n"
   "\n"
   "This program projects (detrends) out various 'nuisance' time series from each\n"
   "voxel in the input dataset.  Note that all the projections are done via linear\n"
   "regression, including the frequency-based options such as '-passband'.  In this\n"
   "way, you can bandpass time-censored data, and at the same time, remove other\n"
   "time series of no interest (e.g., physiological estimates, motion parameters).\n"
   "\n"
   "--------\n"
   "OPTIONS:\n"
   "--------\n"
   " -despike            = Despike each time series before other processing.\n"
   "\n"
   " -censor cname       = As in 3dDeconvolve.\n"
   " -CENSORTR clist     = As in 3dDeconvolve.\n"
   " -cenmode mode       = 'mode' specifies how censored time points are treated in\n"
   "                        the output dataset:\n"
   "                       ++ mode = ZERO ==> put zero values in their place\n"
   "                                      ==> output datset is same length as input\n"
   "                       ++ mode = KILL ==> remove those time points\n"
   "                                      ==> output dataset is shorter than input\n"
   "\n"
   " -ort f.1D           = Remove each column in f.1D\n"
   "                       ++ Multiple -ort options are allowed.\n"
   " -polort pp          = Remove polynomials up to and including degree pp.\n"
   "                       ++ Default value is 2.\n"
   "                       ++ It makes no sense to use a value of pp greater than\n"
   "                          2, if you are bandpassing out the lower frequences!\n"
   " -dsort fset         = Remove the 3D+time time series in dataset fset.\n"
   "                       ++ That is, 'fset' contains a different nuisance time\n"
   "                          series for each voxel (e.g., from AnatICOR).\n"
   "                       ++ Multiple -dsort options are allowed.\n"
   "\n"
   " -passband fbot ftop = Remove all frequences EXCEPT those in the range\n"
   "                        fbot..ftop.\n"
   "                       ++ Only one -passband option is allowed.\n"
   " -stopband sbot stop = Remove all frequencies in the range sbot..stop.\n"
   "                       ++ More than one -stopband option is allowed.\n"
   "                       ++ For example, '-passband 0.01 0.10' is equivalent to\n"
   "                          '-stopband 0 0.0099 -stopband 0.1001 9999'\n"
   " -dt dd              = Use time step dd for the frequency calculations,\n"
   "                        rather than the value stored in the dataset header.\n"
   "\n"
   " -mask mset          = Only operate on voxels nonzero in the mset dataset.\n"
   "                       ++ Use '-mask AUTO' to have the program generate the\n"
   "                          mask automatically.\n"
   "                       ++ Voxels outside the mask will be filled with zeros.\n"
   "\n"
   " -blur fff           = Blur (inside the mask only) with a filter that has\n"
   "                        width (FWHM) of fff millimeters.\n"
   "\n"
   " -norm               = Normalize each output time series to have sum of\n"
   "                        squares = 1.\n"
   "\n"
   " -input dataset      = Alternative way to specify the input dataset.\n"
   " -prefix ppp         = Specifies the output dataset, as usual.\n"
   "\n"
   " -quiet              = Hide the super-fun and thrilling progress messages.\n"
   "\n"
   "------\n"
   "NOTES:\n"
   "------\n"
   "* The input file is treated as one continuous imaging 'run'; no time\n"
   "   discontinuities (breaks) are allowed -- that is, you can't use a\n"
   "   '-concat' option.\n"
   "* All projections are done in one operation.  If you like technical\n"
   "   math jargon (and who doesn't?), this program performs orthogonal\n"
   "   projection onto the null space of the set of input vectors\n"
   "   assembled from the various options.\n"
   "* If option '-dsort' is used, each voxel has a different matrix of\n"
   "   regressors.  For efficiency, the voxel-independent and voxel-\n"
   "   -dependent parts of the projection are calculated separately\n"
   "   and merged using a bordering algorithm.\n"
#ifdef USE_OMP
   "* This version of the program is compiled using OpenMP for speed.\n"
#else
   "* This version of the program is not compiled with OpenMP, but OpenMP\n"
   "   binaries DO exist, and using OpenMP will speed the program up.\n\n"
#endif
   "* Authored by RWCox in a fit of excessive linear algebra [summer 2013].\n"
  ) ;

  PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------------------*/

void project_out_one( int nr, int nc, float *aar, float *par,
                                      float *var, float *qar, float *wsp )
{
   float *ap,*pp,vv ; int ii,jj,kk ;

   for( ii=0 ; ii < nc ; ii++ ) wsp[ii] = 0.0f ;
   for( kk=0 ; kk < nr ; kk++ ){
     pp = par + kk*nc ; vv = qar[kk] = var[kk] ;
     for( ii=0 ; ii < nc ; ii++ ) wsp[ii] += pp[ii]*vv ;
   }
   for( kk=0 ; kk < nc ; kk++ ){
     ap = aar + kk*nr ; vv = wsp[kk] ;
     for( ii=0 ; ii < nr ; ii++ ) qar[ii] -= ap[ii]*vv ;
   }
   return ;
}

/*----------------------------------------------------------------------------*/

void double_project_out( int nr , int nc , float *aar , float *par ,
                                  int nb , float *bar ,
{
   int kk ;

   for( kk=0 ; kk < nb ; kk++ ) project_out_one( nr,nc,aar,par , bar+kk*nr ,

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ct ;
   THD_3dim_dataset         *inset=NULL ;
   THD_3dim_dataset        *outset=NULL ;
   THD_3dim_dataset       *maskset=NULL ;
   int                      polort=2    ;
   int                  do_despike=0    ;
   int                     do_norm=0    ;
   MRI_IMARR                *ortar=NULL ;
   THD_3dim_dataset_array *dsortar=NULL ;
   float_pair            *stopband=NULL ; int num_stopband=0 ;
   float                        dt=0.0f ;
   char                    *prefix="Tproject" ;
   float                      blur=0.0f ;

   /*----------*/

   AFNI_SETUP_OMP(0) ;
   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) TPR_help() ;

   /*----- scan options -----*/

   /*----------*/

   mainENTRY("3dTproject"); machdep();
   AFNI_logger("3dTproject",argc,argv);
   PRINT_VERSION("3dTproject"); AUTHOR("Cox the Algebraic") ;
   ct = NI_clock_time() ;

