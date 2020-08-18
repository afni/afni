#include "mrilib.h"
#include "rsfc.h"
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_sort_vector.h>
#include <gsl/gsl_permutation.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>


/*
OUTDATED! Aug,2016--> use BOBatanh
float FisherZ( double Rcorr)
{
  float Z=0.; 
  double denom=0.;
  
  denom = 1.0-Rcorr;
  if( (Rcorr>=-1) && (denom>0) )
    Z = 0.5 * log( (1.+Rcorr)/denom );
  else
    Z = 0; // primarily for diag Corr Matr values...
  
  return Z;
}
*/

int CalcAveRTS(int *LIST, double *RAT, THD_3dim_dataset *T, 
               int *DIM, int *Nv, float *W)
{
  int i,n;
  double *ts=NULL;

  ts = (double *)calloc( DIM[3],sizeof(double));

  for( n=0; n<DIM[3] ; n++)  // for each time pt
    for( i=0 ; i<Nv[0] ; i++) // for each vox in TS
      ts[n] += THD_get_voxel(T,LIST[i],n);
  
  if ( W ) 
     for( n=0; n<DIM[3] ; n++)
        RAT[n] = (ts[n]*W[n])/Nv[0];
  else
     for( n=0; n<DIM[3] ; n++)
        RAT[n] = ts[n]/Nv[0];

  free(ts);
  
  RETURN(1);
}

// input time series one by one for memory saving

int CalcRanksForReHo(float *IND, int idx, THD_3dim_dataset *T, int *NTIE,
							int TDIM)
{
  int m,mm;
  int ISTIE = -1;
  int LENTIE = 0;
  float TIERANK;
  int *toP=NULL; // to reset permuts
  int *sorted=NULL; // hold sorted time course, assume has been turned into int
  int val;

  // GSL stuff
  gsl_vector *Y = gsl_vector_calloc(TDIM); // will hold time points
  gsl_permutation *P = gsl_permutation_calloc(TDIM); // will hold ranks


  toP = (int *)calloc(TDIM,sizeof(int)); 
  sorted = (int *)calloc(TDIM,sizeof(int)); 

  if( (toP ==NULL) || (sorted ==NULL) ) { 
    fprintf(stderr, "\n\n MemAlloc failure.\n\n");
    exit(122);
    }

  // define time series as gsl vector
  for( m=0 ; m<TDIM ; m++)
    gsl_vector_set(Y,m, THD_get_voxel(T,idx,m));
					
  // perform permutation
  val = gsl_sort_vector_index (P,Y);
  // apply permut to get sorted array values
  for( m=0 ; m<TDIM ; m++) {
    sorted[m] = THD_get_voxel(T,idx,
                              gsl_permutation_get(P,m));
    // information of where it was
    toP[m]= (int) gsl_permutation_get(P,m); 
    // default: just convert perm ind to rank ind:
    // series of rank vals
    IND[gsl_permutation_get(P,m)]=m+1;
  }
					
  // ******** start tie rank adjustment *******
  // find ties in sorted, record how many per time 
  //  series, and fix in IND
  for( m=1 ; m<TDIM ; m++)
    if( (sorted[m]==sorted[m-1]) && LENTIE==0 ) {
      ISTIE = m-1; //record where it starts
      LENTIE = 2;
    }
    else if( (sorted[m]==sorted[m-1]) && LENTIE>0 ) {
      LENTIE+= 1 ;
    }
    else if( (sorted[m]!=sorted[m-1]) && LENTIE>0 ) {
      // end of tie: calc mean index
      TIERANK = 1.0*ISTIE; // where tie started
      TIERANK+= 0.5*(LENTIE-1); // make average rank
      NTIE[idx]+= LENTIE*(LENTIE*LENTIE-1); // record
      // record ave permut ind as rank ind
      for( mm=0 ; mm<LENTIE ; mm++) {
        IND[toP[ISTIE+mm]] = TIERANK+1;
      }
      ISTIE = -1; // reset, prob unnec
      LENTIE = 0; // reset
    } // ******* end of tie rank adjustment ***********
  
  // FREE
  gsl_vector_free(Y);
  gsl_permutation_free(P);
  free(toP);
  free(sorted);
  
  RETURN(1);
}


int FindVoxHood(int *LIST, int **HS,
                int *iam,int *DIM,
                int ***MASK,int VN, int *realHOOD )
{
  int i,j,k,ii,jj,kk;//,m,n;
  int M = 0;// actual size of 'hood, because of possible boundary stuff
  int idx;
	
  for( i=0 ; i<VN ; i++) 
    LIST[i]=0; //wipe clean at start
  
  for( i=0 ; i<VN ; i++) {
    // x,y,z vox indices.
    ii = iam[0]+HS[i][0];
    jj = iam[1]+HS[i][1];
    kk = iam[2]+HS[i][2];
    
    // test possible conditions: inside boundaries and
    // being inside part of mask
    if( (ii>=0) && (ii<DIM[0]) && 
        (jj>=0) && (jj<DIM[1]) &&
        (kk>=0) && (kk<DIM[2])) // inside boundaries
      if (MASK[ii][jj][kk]){ // inside mask
        // idx = THREE_TO_IJK(ii,jj,kk,DIM[0],DIM[0]*DIM[1]);
        LIST[M] = THREE_TO_IJK(ii,jj,kk,DIM[0],DIM[0]*DIM[1]);
        M+=1;
      }
  }
  realHOOD[0]=M; // for friedman chi sq value
	
  RETURN(1);
}


float ReHoIt(int *LIST, float **RANKS, int *TIED, int *DIM, 
             int *realHOOD)
{
  int i,j,k,ii,jj,kk,m,n;
  double W = 0.0;
  int M = realHOOD[0];// actual size of 'hood, which is calc earlier
  int N = DIM[3];
  double miniR;
  double bigR = 0.;
  double fac1,fac2;
  double Tfac = 0.0;
	
  if( (M<1) || (N<2) )
    ERROR_exit("WARNING: either neighborhood size (M=%d) or time series\n"
               "\tlength (N=%d) was too small!",M,N);
	
  for( i=0 ; i<M ; i++)
    Tfac+= TIED[LIST[i]];
	
  fac1 = fac2 = (double) M *  M * N;
  fac1*= (double) 3*(N+1)*(N+1); //fac in numer
  fac2*= (double) (N*N)-1; // fac in denom
   
  // now go back through, and do sums over time series RANKS
  for( n=0; n<N ; n++) {
    miniR = 0.;
    for( i=0 ; i<M ; i++)
      miniR+= RANKS[LIST[i]][n];
    bigR+= miniR*miniR;
  }

  W = 12.*bigR-fac1;
  W/= fac2 - 1.*M*Tfac;
	
  return (float) W;
}

int IntSpherVol(int *RD, float *NR){
  int i,j,k;
  int ct=0;

  for ( i = 0 ; i <3 ; i++ ) 
    RD[i] = (int) ceil(NR[i]);
  for( i=-RD[0] ; i<=RD[0] ; i++)
    for( j=-RD[1] ; j<=RD[1] ; j++) 
      for( k=-RD[2] ; k<=RD[2] ; k++) 
        if( pow(i/NR[0],2)+pow(j/NR[1],2)+pow(k/NR[2],2)<=1 ){
          ct++;
        }

  return ct;
}

// ~silly near duplicate to fill array of values... expediency...
int IntSpherSha(int **HS,int *RD, float *NR){
  int i,j,k;
  int ct=0;

  for ( i = 0 ; i <3 ; i++ ) 
    RD[i] = (int) ceil(NR[i]);
  for( i=-RD[0] ; i<=RD[0] ; i++)
    for( j=-RD[1] ; j<=RD[1] ; j++) 
      for( k=-RD[2] ; k<=RD[2] ; k++) 
        if( pow(i/NR[0],2)+pow(j/NR[1],2)+pow(k/NR[2],2)<=1 ){
          HS[ct][0]=i;
          HS[ct][1]=j;
          HS[ct][2]=k;
          ct++;
        }

  return ct;
}

// [PT: May, 2017]: similar to above, but box-shaped
int IntBoxVol(int *RD, float *NR){
   int i,j,k;
   int ct=0;

   for ( i = 0 ; i <3 ; i++ ) 
      RD[i] = (int) NR[i];
   for( i=-RD[0] ; i<=RD[0] ; i++)
      for( j=-RD[1] ; j<=RD[1] ; j++) 
         for( k=-RD[2] ; k<=RD[2] ; k++) {
            ct++;
         }

   return ct;
}

// ~silly near duplicate to fill array of values... expediency...
int IntBoxSha(int **HS,int *RD, float *NR) {
   int i,j,k;
   int ct=0;

   for ( i = 0 ; i <3 ; i++ ) 
      RD[i] = (int) NR[i];
   for( i=-RD[0] ; i<=RD[0] ; i++)
      for( j=-RD[1] ; j<=RD[1] ; j++) 
         for( k=-RD[2] ; k<=RD[2] ; k++) {
            HS[ct][0]=i;
            HS[ct][1]=j;
            HS[ct][2]=k;
            ct++;
         }
  
   return ct;
}


int WB_netw_corr(int Do_r, 
                 int Do_Z,
                 int HAVE_ROIS, 
                 char *prefix, 
                 int NIFTI_OUT,
                 int *NROI_REF,
                 int *Dim,
                 double ***ROI_AVE_TS,
                 int **ROI_LABELS_REF,
                 char ***ROI_STR_LABELS,
                 int DO_STRLABEL,
                 THD_3dim_dataset *insetTIME,
                 byte *mskd2,
                 int Nmask,
                 int argc,
                 char *argv[])
{
   int i,j,k;
   float **AVE_TS_fl=NULL;    // not great, but another format of TS
   char OUT_indiv0[300];
   char OUT_indiv[300];
   char OUT_indivZ[300];
   MRI_IMAGE *mri=NULL;
   THD_3dim_dataset *OUT_CORR_MAP=NULL;
   THD_3dim_dataset *OUT_Z_MAP=NULL;
   float *zscores=NULL;
   int Nvox;

   char *ftype=NULL;   // default, BRIK/HEAD: can be ".nii.gz"
   char roilab[300];  // will be either int or char str


   Nvox = Dim[0]*Dim[1]*Dim[2];

   // make average time series per voxel
   AVE_TS_fl = calloc( 1,sizeof(AVE_TS_fl));  
   for(i=0 ; i<1 ; i++) 
      AVE_TS_fl[i] = calloc(Dim[3],sizeof(float)); 
   
   if( (AVE_TS_fl == NULL) ) {
      fprintf(stderr, "\n\n MemAlloc failure (time series out).\n\n");
      exit(123);
   }

   // for postfix
   if( NIFTI_OUT )
      ftype = strdup(".nii.gz");
   else
      ftype = strdup("");

   fprintf(stderr,"\nHAVE_ROIS=%d",HAVE_ROIS);
   for( k=0 ; k<HAVE_ROIS ; k++) { // each netw gets own file
      sprintf(OUT_indiv0,"%s_%03d_INDIV", prefix, k);
      mkdir(OUT_indiv0, 0777);
      for( i=0 ; i<NROI_REF[k] ; i++ ) {
         fprintf(stderr,"\nNROI_REF[%d]= %d",k,NROI_REF[k]);
         for( j=0 ; j<Dim[3] ; j++)
            AVE_TS_fl[0][j] = (float) ROI_AVE_TS[k][i][j];

         // use either ROI int value or labeltable value for output name
         if( DO_STRLABEL ) 
            sprintf(roilab, "%s", ROI_STR_LABELS[k][i+1]);
         else
            sprintf(roilab, "%03d", ROI_LABELS_REF[k][i+1]);

         sprintf(OUT_indiv,"%s/WB_CORR_ROI_%s%s",
                 OUT_indiv0, roilab, ftype);

         mri = mri_float_arrays_to_image(AVE_TS_fl,Dim[3],1);
         // [PT: Jan 12, 2018] updated to follow rw cox
         OUT_CORR_MAP = THD_Tcorr1D(insetTIME, mskd2, Nmask,
                                    mri,
                                    "pearson", OUT_indiv, 0,0);
         if(Do_r){
            THD_load_statistics(OUT_CORR_MAP);
            tross_Copy_History( insetTIME , OUT_CORR_MAP ) ;
            tross_Make_History( "3dNetcorr", argc, argv, OUT_CORR_MAP );
            if( !THD_ok_overwrite() && 
                THD_is_ondisk(DSET_HEADNAME(OUT_CORR_MAP)) )
               ERROR_exit("Can't overwrite existing dataset '%s'",
                          DSET_HEADNAME(OUT_CORR_MAP));
            THD_write_3dim_dataset(NULL, NULL, OUT_CORR_MAP, True);
            INFO_message("Wrote dataset: %s\n",DSET_BRIKNAME(OUT_CORR_MAP));

         }
         if(Do_Z){

            sprintf(OUT_indivZ,"%s/WB_Z_ROI_%s%s",
                    OUT_indiv0, roilab, ftype);
            
            OUT_Z_MAP = EDIT_empty_copy(OUT_CORR_MAP);
            EDIT_dset_items( OUT_Z_MAP,
                             ADN_nvals, 1,
                             ADN_datum_all , MRI_float , 
                             ADN_prefix    , OUT_indivZ,
                             ADN_none ) ;
            if( !THD_ok_overwrite() && 
                THD_is_ondisk(DSET_HEADNAME(OUT_Z_MAP)) )
               ERROR_exit("Can't overwrite existing dataset '%s'",
                          DSET_HEADNAME(OUT_Z_MAP));

            zscores = (float *)calloc(Nvox,sizeof(float)); 
            if( (zscores == NULL) ) {
               fprintf(stderr, "\n\n MemAlloc failure (zscores).\n\n");
               exit(123);
            }

            for( j=0 ; j<Nvox ; j++ )
              if( mskd2[j] ) // control for r ==1
                 zscores[j] = BOBatanhf( THD_get_voxel(OUT_CORR_MAP, j, 0) );
                 /*
                 if( THD_get_voxel(OUT_CORR_MAP, j, 0) > MAX_R )
                   zscores[j] = (float) atanh(MAX_R);
                 else if ( THD_get_voxel(OUT_CORR_MAP, j, 0) < -MAX_R )
                   zscores[j] =  (float) atanh(-MAX_R);
                 else
                 zscores[j] = (float) atanh(THD_get_voxel(OUT_CORR_MAP, j, 0));*/
            
            EDIT_substitute_brick(OUT_Z_MAP, 0, MRI_float, zscores); 
            zscores=NULL;

            THD_load_statistics(OUT_Z_MAP);
            tross_Copy_History(insetTIME, OUT_Z_MAP);
            tross_Make_History("3dNetcorr", argc, argv, OUT_Z_MAP);
            THD_write_3dim_dataset(NULL, NULL, OUT_Z_MAP, True);
            INFO_message("Wrote dataset: %s\n",DSET_BRIKNAME(OUT_Z_MAP));

            DSET_delete(OUT_Z_MAP);
            free(OUT_Z_MAP);
            OUT_Z_MAP=NULL;
         }

         DSET_delete(OUT_CORR_MAP);
         free(OUT_CORR_MAP);
         OUT_CORR_MAP=NULL;
      }
   }
   
   free(zscores);
   mri_free(mri);
   for( i=0 ; i<1 ; i++) 
      free(AVE_TS_fl[i]);
   free(AVE_TS_fl);
   free(ftype);

   RETURN(1);
}


int CalcPartCorrMatr( float **OUT, float **OUTB, float **IN, int M)
{
   int BAD=0, BADB=0; // in case of badness in inversion
   int i,j,k;
   float aa,bb;
   gsl_matrix *PC = gsl_matrix_alloc(M,M); // part corr
   gsl_matrix *CC = gsl_matrix_alloc(M,M); // input corr
	gsl_permutation *P = gsl_permutation_alloc(M);

   // Initialize
   gsl_matrix_set_zero(PC);
   for( i=0 ; i<M ; i++)
      for( j=0 ; j<M ; j++)
         gsl_matrix_set(CC, i, j, IN[i][j]);
   
   // perform the LU decomp + inversion
   j = gsl_linalg_LU_decomp(CC, P, &k);
   j = gsl_linalg_LU_invert(CC, P, PC);
   
   
   for( i=0 ; i<M ; i++)
      for( j=0 ; j<M ; j++) {
         OUT[i][j] = OUTB[i][j] = - ((float) gsl_matrix_get(PC, i, j));
         bb = (float) gsl_matrix_get(PC, i, i);
         if( bb )
            OUTB[i][j]/= bb;
         else {
            WARNING_message("Badness in partial correlation beta calculation.\n"
                            "\tNormalizing factor is =0 (how to divide?)!\n"
                            "\t-> making all zeros.");
            BADB = 1;
         }
         aa = (float) gsl_matrix_get(PC, i, i) * 
            (float) gsl_matrix_get(PC, j, j);
         if(aa>0)
            OUT[i][j]/= sqrt(aa);
         else {
            WARNING_message("Badness in partial correlation calculation.\n"
                            "\tNormalizing factor is <=0 (how to take sqrt?)!\n"
                            "\t-> making all zeros.");
            BAD = 1;
         }
      }
   
   if(BAD)
      for( i=0 ; i<M ; i++)
         for( j=0 ; j<M ; j++) 
            OUT[i][j] = 0.;
   if(BADB)
      for( i=0 ; i<M ; i++)
         for( j=0 ; j<M ; j++) 
            OUTB[i][j] = 0.;


   // free
   gsl_matrix_free(PC);
   gsl_matrix_free(CC);
	gsl_permutation_free(P);


   RETURN(1);
}
