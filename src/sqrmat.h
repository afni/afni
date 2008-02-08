typedef struct { int n ; double *mat ; } sqrmat ;

#undef  MAT
#define MAT(i,j) mat[(i)+(j)*n]
#undef  NAT
#define NAT(i,j) nat[(i)+(j)*n]
#undef  PAT
#define PAT(i,j) pat[(i)+(j)*n]

#undef  INIT_SQRMAT
#define INIT_SQRMAT(ss,nn)                                             \
 do{ (ss) = malloc(sizeof(sqrmat)) ; (ss)->n = (nn) ;                  \
     (ss)->mat = (double *)calloc(sizeof(double),((ss)->n)*((ss)->n)); \
 } while(0)

#undef  KILL_SQRMAT
#define KILL_SQRMAT(ss)                         \
 do{ if( (ss) != NULL ){                        \
       if( (ss)->mat != NULL ) free((ss)->mat); \
       free((ss)) ; (ss) = NULL ;               \
 }} while(0)

#undef  DUMP_SQRMAT
#define DUMP_SQRMAT(str,ss)                                      \
 do{ int yy,zz,n=(ss)->n; double *mat=(ss)->mat;                 \
     printf("%s matrix: %d x %d\n",(str),n,n);           \
     for( yy=0 ; yy < n ; yy++ ){                                \
       printf("%02d:",yy+1) ;                            \
       for( zz=0 ; zz < n ; zz++ ) {                             \
          if(MAT(yy,zz)==0.0)                                    \
            printf(" %8.1f   ",MAT(yy,zz));              \
          else                                                   \
            printf(" %11.4f",MAT(yy,zz));                \
       }                                                         \
       printf("\n") ;                                    \
 }} while(0)

#undef  DUMP_SQRMAT_LABELED
#define DUMP_SQRMAT_LABELED(str,ss,labels)                       \
 do{ int yy,zz,n=(ss)->n; double *mat=(ss)->mat;                 \
     printf("%s matrix: %d x %d\n#     ",(str),n,n);     \
     for( yy=0 ; yy < n ; yy++ )                                 \
        printf("%11.4s ",labels[yy]) ;                   \
     printf("\n");                                       \
     for( yy=0 ; yy < n ; yy++ ){                                \
       printf("%-5.5s ",labels[yy]) ;                    \
       for( zz=0 ; zz < n ; zz++ ) {                             \
          if(MAT(yy,zz)==0.0)                                    \
            printf(" %8.1f   ",MAT(yy,zz));              \
          else                                                   \
            printf(" %11.4f",MAT(yy,zz));                \
       }                                                         \
       printf("\n") ;                                    \
 }} while(0)

#undef  EQUIV_SQRMAT
#define EQUIV_SQRMAT(ss,tt)                                          \
 do{                                                                 \
   memcpy((tt)->mat, (ss)->mat, ((ss)->n)*((ss)->n)*sizeof(double)); \
 } while(0)


sqrmat * sm_iktk( sqrmat *K );
int sm_choleski( sqrmat *A );
double sm_lndet_iktk( sqrmat *K );
sqrmat * sm_transpose( sqrmat *A );
sqrmat * sm_subtract( sqrmat *A , sqrmat *B);
sqrmat * sm_mult( sqrmat *A, sqrmat *B );
sqrmat * sm_copy( sqrmat *A );
sqrmat * sm_identity(int n);
double sm_dot(sqrmat *A, sqrmat *B);
double sm_trace(sqrmat *A);
sqrmat * sm_scale(sqrmat *A, double sc_factor, int newmatrix);
