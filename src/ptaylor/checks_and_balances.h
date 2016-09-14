#ifndef _CHECKS_AND_BALANCES_HEADER_
#define _CHECKS_AND_BALANCES_HEADER_


int CompareSetDims(THD_3dim_dataset *A, THD_3dim_dataset *B, int Ndim);

int WB_corr_loop(
                 float *X,float *Y,
                 THD_3dim_dataset *A,
                 int *Dim,
                 byte ***mskd,
                 float *mapA,
                 int *myloc
                 );

#endif /* _CHECKS_AND_BALANCES_HEADER_ */
