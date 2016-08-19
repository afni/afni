#ifndef _CHECKS_AND_BALANCES_HEADER_
#define _CHECKS_AND_BALANCES_HEADER_


int CompareSetDims(THD_3dim_dataset *A, THD_3dim_dataset *B, int Ndim);

int WB_corr_loop(
                 double *X,double *Y,
                 THD_3dim_dataset *A,
                 int *Dim,
                 byte ***mskd,
                 double *mapA
                 );


int THD_extract_double_array( int ind, 
                              THD_3dim_dataset *dset, 
                              double *far );

#endif /* _CHECKS_AND_BALANCES_HEADER_ */
