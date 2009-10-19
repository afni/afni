/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/*#define CLIPSECTIONS */
/* Contrary to the good tradition, this .h file will include */
/* function declarations and definitions. */
/* The reason for this is that the plugin has to be a stand alone*/
/* code and I must provide all the code for the functions. */
/* This is just a way of splitting the code into smaller pieces */
/* This file will hold all support functions for the plugin code */

/*-------------------------------------------------------------------*/
/* taken from #include "/usr/people/ziad/Programs/C/Z/Zlib/prototype.h" */

#if defined(SCO) || defined(SOLARIS)
#define drem remainder
#endif
#ifndef NOWAYXCORCOEF
	#define NOWAYXCORCOEF 0					/* A flag value indicating that something lethal went on */
#endif

/* do not change these three*/
#define METH_SECONDS 0
#define METH_DEGREES 1
#define METH_RADIANS 2


static int Read_file (float *x,char *f_name,int n_points);

static void linear_reg (float *x,float *y,int size,float *a,float *b,int *err);

static int equal_strings (char *s1,char *s2);

static int float_file_size (char *f_name);

static void error_message (char *origin,char *cause,int ext);

static char **allocate2D (int rows,int cols,int element_size);

static void free2D(char **a,int rows);

static void linear_interp (float *x1,float *y1,float *x2,float *y2,float *x,float *y,int ln);

static void float_to_complex (float *x,COMPLEX *xc,int ln);

static void c_conj (COMPLEX *x,COMPLEX *y,int ln);

static void c_get (COMPLEX *x,float *y,int p,int ln);

static void c_scale (COMPLEX *x,COMPLEX *y,float scl,int ln);

static void c_padd (COMPLEX *x,COMPLEX *y,COMPLEX pad_val,int ix,int lnx,int lny);

static void c_mult (COMPLEX *x,COMPLEX *y,COMPLEX *z,int ln);

static void detrend (float *y,float *yd,int lny,float *a,float *b);

static void padd (float *x,float *y,float pad_val,int ix,int lnx,int lny);

static void hanning (float *x,int l,int opt);

static float punwrap (float p,int opt );

static float Lagrange_interp (float *x,float *y,float xi,int ln);

static void f_mult (float *x,float *y,float *z,int ln);	
	
static int hilbertdelay_V2 (float *x,float *y,int lng_full,int Nseg,int Pover,\
                       int opt,int dtrnd, float Dtx, int biasrem,\
                       float *del,float *slp,float *xcor,float *xcorCoef,\
                       float *vx, float *vy);
                       
static void hunwrap (float del, float fs, float T, float slp, int wrp, int unt, float *delu );

static int isarg (int argc, char *argv[], char *probe);

static float mean_array (float *ar,int size);

static void zeromean (float *x, float *y, int ln );

static void disp_comp_vect (COMPLEX *v,int l);

static void disp_vect (float *v,int l);

static int is_vect_null ( float * v , int npts );

static int write_float (float *x,char *f_name,int n_points);


/* definition and declaration part to suport the main algorithm */
/* -----------------------END-----------------------------------*/


/* -------------------------------------------------------------*/
/* support functions declaration for main algorithm             */
/* -----------------------START---------------------------------*/

static void error_message (char *s1,char *s2,int ext)
 
 {
 
   printf ("\n\n\a\33[1mError: \33[0m%s\n",s2);
   printf ("\33[1mError origin:\33[0m %s\n\n",s1);
   if (ext == 1)
        {
                printf ("Exiting Program ..\n\n");
                exit (0);
        }
        else return;
   
  }

/*-----------------------------------------------------------------------------------*/	
/**************************************************************************

allocate2D.c - Make matrix of given size (rows x cols) and type

The type is given by element_size (2 = ints, 4 = floats, 8 = doubles).
Exits if the matrix could not be allocated.

    char **allocate2D(int rows,int cols,int element_size)
SIZE might vary depending on platform used 

This function was adapted from DSP_in_C library functions

                                Ziad Saad       Oct_21_96
*************************************************************************/

static char **allocate2D (int rows,int cols,int element_size)

{
    int i;
    char **A;

/* try to allocate the request */
    switch(element_size) {
        case sizeof(short): {    /* integer matrix */
            short **int_matrix;
            int_matrix = (short **)calloc(rows,sizeof(short *));
            if(!int_matrix) {
                printf("\nError making pointers in %dx%d int matrix\n"
                            ,rows,cols);
                exit(1);
            }
            for(i = 0 ; i < rows ; i++) {
                int_matrix[i] = (short *)calloc(cols,sizeof(short));
                if(!int_matrix[i]) {
                    printf("\nError making row %d in %dx%d int matrix\n"
                            ,i,rows,cols);
                    exit(1);
                }
            }
            A = (char **)int_matrix;
            break;
        }
        case sizeof(float): {    /* float matrix */
            float **float_matrix;
            float_matrix = (float **)calloc(rows,sizeof(float *));
            if(!float_matrix) {
                printf("\nError making pointers in %dx%d float matrix\n"
                            ,rows,cols);
                exit(1);
            }
            for(i = 0 ; i < rows ; i++) {
                float_matrix[i] = (float *)calloc(cols,sizeof(float));
                if(!float_matrix[i]) {
                    printf("\nError making row %d in %dx%d float matrix\n"
                            ,i,rows,cols);
                    exit(1);
                }
            }
            A = (char **)float_matrix;
            break;
        }
        case sizeof(double): {   /* double matrix */
            double **double_matrix;
            double_matrix = (double **)calloc(rows,sizeof(double *));
            if(!double_matrix) {
                printf("\nError making pointers in %dx%d double matrix\n"
                            ,rows,cols);
                exit(1);
            }
            for(i = 0 ; i < rows ; i++) {
                double_matrix[i] = (double *)calloc(cols,sizeof(double));
                if(!double_matrix[i]) {
                    printf("\nError making row %d in %dx%d double matrix\n"
                            ,i,rows,cols);
                    exit(1);
                }
            }
            A = (char **)double_matrix;
            break;
        }
        default:
            printf("\nERROR in matrix_allocate: unsupported type\n");
            exit(1);
    }
    return(A);
}

/*-----------------------------------------------------------------------------------*/	

/**************************************************************************

free2D.c - Free all elements of matrix 

Frees the 2D array (rows and cols) allocated using allocate2D

Error message and exit if improper structure is
passed to it (null pointers or zero size matrix).

    void free2D(char **a, int rows);

This function was adapted from DSP_in_C library functions

                                Ziad Saad                              Oct_22_96
*************************************************************************/

static void free2D(char **a,int rows)
    
{
    int i;
    
/* free each row of data */
    for(i = 0 ; i < rows ; i++) free(a[i]);

/* free each row pointer */
    free((char *)a);
    a = NULL;           /* set to null for error */
    
        return;
}

/*-----------------------------------------------------------------------------------*/	

static void hanning (float *x,int l,int opt)
        {
                int i;
                float arg;
                
                arg = 2.0*3.14159/(l-1);
                if (opt == 0)
                {
                        for (i=0;i<l;++i)
                                x[i]=0.5-0.5*cos(arg*(float)i);
                } else if (opt == 1)
                        {
                         for (i=0;i<l;++i)
                                x[i]=x[i] * (0.5-0.5*cos(arg*(float)i));
                        }
                return;
        } 

/*-----------------------------------------------------------------------------------*/	

static void detrend (float *y,float *yd,int lny,float *a,float *b)

        {
         int i;
         int err;
         float *x;
         
    x = (float *)calloc (lny+1,sizeof(float));
         if (x == NULL)
				{
					printf ("\nFatal Error : Failed to Allocate memory\a\n");
					printf ("Abandon Lab Immediately !\n\n");
					return;
				};
         
         for (i=0;i<lny;++i) /*creating x vector */
                x[i] = (float)i;
         
         linear_reg (x,y,lny,a,b,&err);
         
         for (i=0;i<lny;++i)
                {
                        yd[i] = y[i] - (*a * x[i] + *b);
                }
         
          free (x);
          return;
        }

/*-----------------------------------------------------------------------------------*/	

static void padd (float *x,float *y,float pad_val,int ix,int lnx,int lny)
        {
                int i,di;
                float *tmp;
                
                tmp = (float *) calloc (lnx+2,sizeof(float));
                
            	if (tmp == NULL)
						{
							printf ("\nFatal Error : Failed to Allocate memory\a\n");
							printf ("Abandon Lab Immediately !\n\n");
							return;
						};

                
                di = lny-lnx;
                if (lny < lnx) 
                        {
                                error_message ("padd","lny < lnx !",1);
                                exit(1);
                        }
                if (ix > lnx+1)
                        {
                                error_message ("padd","ix > lnx+1 !",1);
                                exit(1);
                        }
                        
                for (i=0;i<lnx;++i)
                        {
                                tmp[i] = x[i]; /* must use tmp to be safe when in client program function call is made with input and */
                        }
                
                for (i=0;i<(ix-1);++i)
                        {
                                y[i] = tmp[i];
                        }
                for (i=ix-1;i<ix+di-1;++i)
                        {
                                y[i] = pad_val;
                        }
                for (i=ix+di-1;i<lny;++i)
                        {
                                y[i] = tmp[i-di];
                        }
                        
                free (tmp);
                return;
        }

/*-----------------------------------------------------------------------------------*/	

/**************************************************************************

fft - In-place radix 2 decimation in time FFT

Requires pointer to complex array, x and power of 2 size of FFT, m
(size of FFT = 2**m).  Places FFT output on top of input COMPLEX array.

void fft(COMPLEX *x, int m)

*************************************************************************/

static void fft(COMPLEX *x,int m)
{
    static COMPLEX *w;           /* used to store the w complex array */
    static int mstore = 0;       /* stores m for future reference */
    static int n = 1;            /* length of fft stored for future */

    COMPLEX u,temp,tm;
    COMPLEX *xi,*xip,*xj,*wptr;

    int i,j,k,l,le,windex;

    double arg,w_real,w_imag,wrecur_real,wrecur_imag,wtemp_real;

    if(m != mstore) {

/* free previously allocated storage and set new m */

        if(mstore != 0) free(w);
        mstore = m;
        if(m == 0) return;       /* if m=0 then done */

/* n = 2**m = fft length */

        n = 1 << m;
        le = n/2;

/* allocate the storage for w */

        w = (COMPLEX *) calloc(le-1,sizeof(COMPLEX));
        if(!w) {
            printf("\nUnable to allocate complex W array\n");
            exit(1);
        }

/* calculate the w values recursively */

        arg = 4.0*atan(1.0)/le;         /* PI/le calculation */
        wrecur_real = w_real = cos(arg);
        wrecur_imag = w_imag = -sin(arg);
        xj = w;
        for (j = 1 ; j < le ; j++) {
            xj->real = (float)wrecur_real;
            xj->imag = (float)wrecur_imag;
            xj++;
            wtemp_real = wrecur_real*w_real - wrecur_imag*w_imag;
            wrecur_imag = wrecur_real*w_imag + wrecur_imag*w_real;
            wrecur_real = wtemp_real;
        }
    }

/* start fft */

    le = n;
    windex = 1;
    for (l = 0 ; l < m ; l++) {
        le = le/2;

/* first iteration with no multiplies */

        for(i = 0 ; i < n ; i = i + 2*le) {
            xi = x + i;
            xip = xi + le;
            temp.real = xi->real + xip->real;
            temp.imag = xi->imag + xip->imag;
            xip->real = xi->real - xip->real;
            xip->imag = xi->imag - xip->imag;
            *xi = temp;
        }

/* remaining iterations use stored w */

        wptr = w + windex - 1;
        for (j = 1 ; j < le ; j++) {
            u = *wptr;
            for (i = j ; i < n ; i = i + 2*le) {
                xi = x + i;
                xip = xi + le;
                temp.real = xi->real + xip->real;
                temp.imag = xi->imag + xip->imag;
                tm.real = xi->real - xip->real;
                tm.imag = xi->imag - xip->imag;             
                xip->real = tm.real*u.real - tm.imag*u.imag;
                xip->imag = tm.real*u.imag + tm.imag*u.real;
                *xi = temp;
            }
            wptr = wptr + windex;
        }
        windex = 2*windex;
    }            

/* rearrange data by bit reversing */

    j = 0;
    for (i = 1 ; i < (n-1) ; i++) {
        k = n/2;
        while(k <= j) {
            j = j - k;
            k = k/2;
        }
        j = j + k;
        if (i < j) {
            xi = x + i;
            xj = x + j;
            temp = *xj;
            *xj = *xi;
            *xi = temp;
        }
    }
}

/*-----------------------------------------------------------------------------------*/	


/**************************************************************************

ifft - In-place radix 2 decimation in time inverse FFT

Requires pointer to complex array, x and power of 2 size of FFT, m
(size of FFT = 2**m).  Places inverse FFT output on top of input
frequency domain COMPLEX array.

void ifft(COMPLEX *x, int m)

*************************************************************************/

static void ifft(COMPLEX *x,int m)
{
    static COMPLEX *w;           /* used to store the w complex array */
    static int mstore = 0;       /* stores m for future reference */
    static int n = 1;            /* length of ifft stored for future */

    COMPLEX u,temp,tm;
    COMPLEX *xi,*xip,*xj,*wptr;

    int i,j,k,l,le,windex;

    double arg,w_real,w_imag,wrecur_real,wrecur_imag,wtemp_real;
    float scale;

    if(m != mstore) {

/* free previously allocated storage and set new m */

        if(mstore != 0) free(w);
        mstore = m;
        if(m == 0) return;       /* if m=0 then done */

/* n = 2**m = inverse fft length */

        n = 1 << m;
        le = n/2;

/* allocate the storage for w */

        w = (COMPLEX *) calloc(le-1,sizeof(COMPLEX));
        if(!w) {
            printf("\nUnable to allocate complex W array\n");
            exit(1);
        }

/* calculate the w values recursively */

        arg = 4.0*atan(1.0)/le;         /* PI/le calculation */
        wrecur_real = w_real = cos(arg);
        wrecur_imag = w_imag = sin(arg);  /* opposite sign from fft */
        xj = w;
        for (j = 1 ; j < le ; j++) {
            xj->real = (float)wrecur_real;
            xj->imag = (float)wrecur_imag;
            xj++;
            wtemp_real = wrecur_real*w_real - wrecur_imag*w_imag;
            wrecur_imag = wrecur_real*w_imag + wrecur_imag*w_real;
            wrecur_real = wtemp_real;
        }
    }

/* start inverse fft */

    le = n;
    windex = 1;
    for (l = 0 ; l < m ; l++) {
        le = le/2;

/* first iteration with no multiplies */

        for(i = 0 ; i < n ; i = i + 2*le) {
            xi = x + i;
            xip = xi + le;
            temp.real = xi->real + xip->real;
            temp.imag = xi->imag + xip->imag;
            xip->real = xi->real - xip->real;
            xip->imag = xi->imag - xip->imag;
            *xi = temp;
        }

/* remaining iterations use stored w */

        wptr = w + windex - 1;
        for (j = 1 ; j < le ; j++) {
            u = *wptr;
            for (i = j ; i < n ; i = i + 2*le) {
                xi = x + i;
                xip = xi + le;
                temp.real = xi->real + xip->real;
                temp.imag = xi->imag + xip->imag;
                tm.real = xi->real - xip->real;
                tm.imag = xi->imag - xip->imag;             
                xip->real = tm.real*u.real - tm.imag*u.imag;
                xip->imag = tm.real*u.imag + tm.imag*u.real;
                *xi = temp;
            }
            wptr = wptr + windex;
        }
        windex = 2*windex;
    }            

/* rearrange data by bit reversing */

    j = 0;
    for (i = 1 ; i < (n-1) ; i++) {
        k = n/2;
        while(k <= j) {
            j = j - k;
            k = k/2;
        }
        j = j + k;
        if (i < j) {
            xi = x + i;
            xj = x + j;
            temp = *xj;
            *xj = *xi;
            *xi = temp;
        }
    }

/* scale all results by 1/n */
    scale = (float)(1.0/n);
    for(i = 0 ; i < n ; i++) {
        x->real = scale*x->real;
        x->imag = scale*x->imag;
        x++;
    }
}

/************************************************************

rfft - trig recombination real input FFT

Requires real array pointed to by x, pointer to complex
output array, y and the size of real FFT in power of
2 notation, m (size of input array and FFT, N = 2**m).
On completion, the COMPLEX array pointed to by y 
contains the lower N/2 + 1 elements of the spectrum.

void rfft(float *x, COMPLEX *y, int m)

***************************************************************/

static void rfft(float *x,COMPLEX *y,int m)
{
    static    COMPLEX  *cf;
    static    int      mstore = 0;
    int       p,num,k,index;
    float     Realsum, Realdif, Imagsum, Imagdif;
    double    factor, arg;
    COMPLEX   *ck, *xk, *xnk, *cx;

/* First call the fft routine using the x array but with
   half the size of the real fft */

    p = m - 1;
    cx = (COMPLEX *) x;
    fft(cx,p);

/* Next create the coefficients for recombination, if required */

    num = 1 << p;    /* num is half the real sequence length.  */

    if (m!=mstore){
      if (mstore != 0) free(cf);
      cf = (COMPLEX *) calloc(num - 1,sizeof(COMPLEX));
      if(!cf){
        printf("\nUnable to allocate trig recomb coefficients.");
        exit(1);
      }

      factor = 4.0*atan(1.0)/num;
      for (k = 1; k < num; k++){
        arg = factor*k;
        cf[k-1].real = (float)cos(arg);
        cf[k-1].imag = (float)sin(arg);
      }
    }  

/* DC component, no multiplies */
    y[0].real = cx[0].real + cx[0].imag;
    y[0].imag = 0.0;

/* other frequencies by trig recombination */
    ck = cf;
    xk = cx + 1;
    xnk = cx + num - 1;
    for (k = 1; k < num; k++){
      Realsum = ( xk->real + xnk->real ) / 2;
      Imagsum = ( xk->imag + xnk->imag ) / 2;
      Realdif = ( xk->real - xnk->real ) / 2;
      Imagdif = ( xk->imag - xnk->imag ) / 2;

      y[k].real = Realsum + ck->real * Imagsum
                          - ck->imag * Realdif ;

      y[k].imag = Imagdif - ck->imag * Imagsum
                          - ck->real * Realdif ;
      ck++;
      xk++;
      xnk--;
    }
}

/*-----------------------------------------------------------------------------------*/	

static void float_to_complex (float *x,COMPLEX *xc,int ln)

        {
         int i;
      if ((ln-1) == 0)
         {       
              (*xc).real = (*x);
              (*xc).imag = 0.0;
              return;
           }
         else 
              {
                   for (i=0;i<ln;++i)
                       {
                          xc[i].real = x[i];
                          xc[i].imag = 0.0;
                        }
                     return;
                }
         }

/*-----------------------------------------------------------------------------------*/	

static void c_conj (COMPLEX *x,COMPLEX *y,int ln)
        {
                int i;
                if ((ln-1) == 0)
                        {       
                                (*y).real = (*x).real;
                                (*y).imag = -(*x).imag;
                                return;
                        }
                else 
                        {
                                for (i=0;i<ln;++i)
                                        {
                                                y[i].real = x[i].real;
                                                y[i].imag = -x[i].imag;
                                        }
                                return;
                        }
        }

/*-----------------------------------------------------------------------------------*/	

static void c_mult (COMPLEX *x,COMPLEX *y,COMPLEX *z,int ln)
        {
                int i;
                COMPLEX t;
                
                if ((ln-1) == 0)
                        {       
                                t.real = ((*x).real * (*y).real - (*x).imag * (*y).imag);
                                t.imag = ((*x).real * (*y).imag + (*y).real * (*x).imag);
                                (*z).real = t.real;
                                (*z).imag = t.imag;
                                return;
                        }
                else 
                        {
                                for (i=0;i<ln;++i)
                                        {
                                                t.real = (x[i].real * y[i].real - x[i].imag * y[i].imag);
                                                t.imag = (x[i].real * y[i].imag + y[i].real * x[i].imag);
                                                z[i].real = t.real;
                                                z[i].imag = t.imag;
                                        }
                                return;
                        }
        }

/*-----------------------------------------------------------------------------------*/	

static void c_padd (COMPLEX *x,COMPLEX *y,COMPLEX pad_val,int ix,int lnx,int lny)
        {
                int i,di;
                COMPLEX *tmp;
                
                tmp = (COMPLEX *) calloc (lnx+2,sizeof(COMPLEX));
                
                if (tmp == NULL)
						{
							printf ("\nFatal Error : Failed to Allocate memory\a\n");
							printf ("Abandon Lab Immediately !\n\n");
							return;
						};

                di = lny-lnx;
                if (lny < lnx) 
                        {
                                error_message ("c_padd","lny < lnx !",1);
                                exit(1);
                        }
                if (ix > lnx+1)
                        {
                                error_message ("c_padd","ix > lnx+1 !",1);
                                exit(1);
                        }
                        
                for (i=0;i<lnx;++i)
                        {
                                tmp[i].real = x[i].real; /* must do this to be safe when in client program function call is made with input and */
                                tmp[i].imag = x[i].imag; /* output vectors being the same !!! */
                        }
                
                for (i=0;i<(ix-1);++i)
                        {
                                y[i].real = tmp[i].real;
                                y[i].imag = tmp[i].imag;
                        }
                for (i=ix-1;i<ix+di-1;++i)
                        {
                                y[i].real = pad_val.real;
                                y[i].imag = pad_val.imag;
                        }
                for (i=ix+di-1;i<lny;++i)
                        {
                                y[i].real = tmp[i-di].real;
                                y[i].imag = tmp[i-di].imag;
                        }
                        
                free (tmp);
                return;
        }

/*-----------------------------------------------------------------------------------*/	

static void c_scale (COMPLEX *x,COMPLEX *y,float scl,int ln)   
        {
                int i;
                if ((ln-1) == 0)
                        {       
                                (*y).real = (scl) * ((*x).real);
                                (*y).imag = (scl) * ((*x).imag);
                                return;
                        }
                else 
                        {
                                for (i=0;i<ln;++i)
                                        {
                                                y[i].real = (scl) * (x[i].real);
                                                y[i].imag = (scl) * (x[i].imag);
                                        }
                                return;
                        }
        }

/*-----------------------------------------------------------------------------------*/	

static void c_get (COMPLEX *x,float *y,int p,int ln)

        {
                        int i;
                  if ((ln-1) == 0)
                        {
                                if (p == 0) { (*y) = (*x).real; }
                                        else {(*y) = (*x).imag;}
                                return;
                        }
                else 
                        {
                                for (i=0;i<ln;++i)
                                        {
                                          if (p == 0) { y[i] = x[i].real; }
                                                else { y[i] = x[i].imag; }
                                        }
                                return;
                        }
        }

/*-----------------------------------------------------------------------------------*/	

static void linear_interp (float *x1,float *y1,float *x2,float *y2,float *x,float *y,int ln)

   {int i;
        
    if ((ln -1) == 0)       
      {
        if ((*x2 - *x1) == 0.0) 
          {
              error_message ("linear_interp","identical X values in interpolation boundaries, causes division by zero !",1);
              exit (1);
           }
        *y = (*x - *x1)/(*x2 - *x1)*(*y2 - *y1) + *y1;
       }
         else 
        {
          for (i=0;i<ln;++i)
            {
              if ((x2[i] - x1[i]) == 0.0) 
                {
                   error_message ("linear_interp","identical X values in interpolation boundaries, causes division by zero !",1);
                   exit (1);
                 }
           y[i] = (x[i] - x1[i])/(x2[i] - x1[i])*(y2[i] - y1[i]) + y1[i];
             }
         }
                return;
     }

/*-----------------------------------------------------------------------------------*/	

static int equal_strings (char *s1,char *s2)

 {
   int i=0;
   
   if (s1 == NULL && s2 == NULL) return (-2);
   
   if ((s1 == NULL && s2 != NULL) || (s1 != NULL && s2 == NULL)) return (-1);
   
   while (s1[i] == s2[i] 
                        && s1[i] != '\0' && s2[i] != '\0') ++i;
                        
        if (s1[i] == '\0' && s2[i] == '\0') return (1);
         else return (0);
 
 }

/*-----------------------------------------------------------------------------------*/	

/* Fails miserably with commented .1D files */ 
static int float_file_size_junk (char *f_name)
   
    { 
      

     int cnt=0,ex;
     float buf;
     
     FILE*internal_file;
     
     internal_file = fopen (f_name,"r");
     if (internal_file == NULL) {
             printf ("\aCould not open %s \n",f_name);
             printf ("Exiting @ float_file_size function\n");
             exit (0);
                                 }
     ex = fscanf (internal_file,"%f",&buf);                                             
     while (ex != EOF)
      {
        ++cnt;
        ex = fscanf (internal_file,"%f",&buf);
      }
      
      
      fclose (internal_file);
      return (cnt);                                                          
   }

/* inefficient version but more robust */   
static int float_file_size (char *f_name)
   
   { 
      int i=0, ncol = 0, nrow = 0;
      MRI_IMAGE *im = NULL;
      float *far=NULL;

      im = mri_read_1D (f_name);

      if (!im) {
         fprintf(stderr,"Failed to read 1D file\n");
         return(-1);
      }

      far = MRI_FLOAT_PTR(im);
      ncol = im->nx;
      nrow = im->ny;

      mri_free(im); im = NULL;  

      return(ncol);
   }


/*-----------------------------------------------------------------------------------*/	

static int Read_file (float *x,char *f_name,int n_points)
   
    { /* pass a 0 to n_points if you want to read till EOF */
     int cnt=0,ex,dec;
     
     FILE*internal_file;
     
     internal_file = fopen (f_name,"r");
     if (internal_file == NULL) {
                                                                printf ("\aCould not open %s \n",f_name);
                                                                printf ("Exiting @ Read_file function\n");
                                                                exit (0);
                                                        }
     ex = fscanf (internal_file,"%f",&x[cnt]);                                          
     while (ex != EOF)
      {
        ++cnt;
        /* NOT WORKING, RETURNS SIZEOF (FLOAT) .....
        if (sizeof(x) < cnt)
                {
                  printf ("%d = sizeof(x)\n",sizeof(x));
                  printf ("\nNot Enough Memory Allocated \n\a");
                  printf ("Exiting @Read_file function\n");
                  exit (0);
                }
        ............................................ */
        ex = fscanf (internal_file,"%f",&x[cnt]);
        
        if ((n_points != 0) && (cnt == n_points)) ex = EOF;
      }
      
      if (cnt < n_points) 
        {
         printf ("\a\nAttempt to read %d points failed,\n",n_points);
         printf (" file contains %d points only.\n",cnt);
         do {
         
         printf ("End Execution (Yes (1) No (0) ? : ");
         ex=scanf ("%d",&dec);
         } while (ex != 1 || (dec != 1 && dec !=0));
         if (dec)
          {
            printf ("Exiting @ Read_file function\n");
                exit (0);
                }
         else printf ("\nContinuing execution with %d points\n",cnt);

        }
      
      fclose (internal_file);
      return (cnt);                                                          
   }

/*-----------------------------------------------------------------------------------*/	

static void linear_reg (float *x,float *y,int size,float *a,float *b,int *err)

        {/* linear_reg*/
          int i;
          float n,sum_x,sum_y,sum_x2,sum_xy;
          
          *err = 3;
          
          if (size <= 0) {
                                        *err = 1;
                                        return;
                                        }
                                
       sum_x = 0.0;
       sum_y = 0.0;
       sum_xy = 0.0;       
           sum_x2 = 0.0;        
           n = (float)size;   

           for (i=0;i<size;++i)
             {
               sum_x = sum_x + x[i];
               sum_y = sum_y + y[i];
               sum_x2 = sum_x2 + x[i] * x[i];
               sum_xy = sum_xy + x[i] * y[i];
             }
           
           *a = ( n * sum_xy - sum_x * sum_y) / ( n * sum_x2 - sum_x * sum_x);
           
           *b = (sum_y - *a * sum_x) / n;
           
            *err = 0; 
             return ;
        }/* linear_reg */

/*-----------------------------------------------------------------------------------*/	

static float punwrap (float p,int opt )
{/*punwrap*/
 float topi,alf;
 
 if (opt == 0)
                topi = 2.*3.14159;
        else if (opt == 1) topi = 360.0;
                else {
                        error_message ("punwrap","wrong opt parameter",1);
                        exit (1);
                }
 alf = (float) drem ((double)p,(double)topi);
 
 if (alf > topi/2.0) alf = topi/2.0-alf;
 
 return (alf);
}/*punwrap*/

/*-----------------------------------------------------------------------------------*/	

static float Lagrange_interp (float *x,float *y,float xi,int ln)
 
 	{
 		int i,j;
 		float yi,p;
 		
 		yi = 0.0;
 		
 		for (i=0;i<ln;++i)
 		{
 			p = y[i];
 			for (j=0;j<ln;++j)
 				{
 					if (j != i) 
 						{
 							p = p * (xi - x[j]) / (x[i] - x[j]);
 						}
 				}
 			yi=yi+p;
 		}	
 		return (yi);
 	}
 
/*-----------------------------------------------------------------------------------*/	
static int isarg (int argc, char *argv[], char *probe)
{/*isarg*/
 	int i=1;
 		
 	while (i < argc)
 		{
 			if (equal_strings (argv[i],probe) == 1)
 				{
 					return (i);
 				}
 			++i;
 		}
 	
 	return (0);
 	
}/*isarg*/

/*-----------------------------------------------------------------------------------*/	
static float mean_array (float *ar,int size)
	 
	{/*Mean_array ()*/
	 
	 int i;
	 float sum,mean;
	 
	 sum=0;
	 
	 for (i=0;i<size;++i)
	  {
		sum=sum+ar[i];
	  }
	  
	  mean=sum/(float)size;
	  
	  
	  return (mean);
	
	}/*Mean_array ()*/

/*-----------------------------------------------------------------------------------*/	
static void zeromean (float *x, float *y, int ln )
{/*zeromean*/
 	int i;
 	float meanx;
 	
 	meanx = mean_array (x,ln);
 	
 	for (i=0;i<ln;++i)
 		{
 			y[i] = x[i] - meanx;
 		}
 	return;
}/*zeromean*/

/*-----------------------------------------------------------------------------------*/	
static void f_mult (float *x,float *y,float *z,int ln)	
	{
		int i;
		if ((ln-1) == 0)
			{	
				(*z) = (*y) * (*x);
				return;
			}
		else 
			{
				for (i=0;i<ln;++i)
					{
						z[i] = y[i] * x[i];
					}
				return;
			}
	}

/*-----------------------------------------------------------------------------------*/	
/* Important :
	Before you replace this function by a new version, make note of the following
	changes to the function in here: maxdel is changed from lng /3 to lng /2, 
	and the funcion returns a 1, 2 or 3 in case of encoutered errors instead of 
	a regular 1 
	Also the function does not output any warning messages to the screen if the delay is
	larger than one 1/2 a segment length*/

/* The difference between 	hilbertdelay_V2 and hilbertdelay is that the parameter negslp is not used anymore in V2 version */
static int hilbertdelay_V2 (float *x,float *y,int lng_full,int Nseg,int Pover,int opt,int dtrnd, float Dtx, int biasrem, float *del,float *slp,float *xcor,float *xcorCoef, float *vx, float *vy)
	{	
	 static int i_call=0,olng,m,lng_use,lng,strt,nd,sg,cnt,maxdel = 0;
	 static COMPLEX   *fftx,*ffty,*Pxy,*fftyc,*fftxc,*Pxx,*Pyy,
                     *Rxx,*Ryy,**fftyca,**Pxya,**Rxxa,**Ryya;
	 static float  *Px,*Py,*Rxy,*HRxy,*xp,*yp,*xcpy,*ycpy,
                  *tmp_f_vect,*tmp_f_vect2,*ubias,a,
	 			      var_y,varu_y,stdv_y,stdvu_y,var_x,varu_x,stdv_x,stdvu_x;
	 
			  int i,j,k;
			  char buf[100];
			  float *mPxy,tmp,sPx,sPy,alfx,betx,alfy,bety,
						f_i,f_i1,izero=-1.0,reg_pnt,
						NoWayDelay = -100000.0,
						NoWayxcorCoef = NOWAYXCORCOEF;
						
			  COMPLEX tmp_c;
			  char cbuf[30];

if ((opt == 0) && (i_call == 0)) 
	{
		error_message ("hilbertdelay_V2","Nothing to DO !",0);
		return (1); 
	}

	
*del = NoWayDelay;					/* setting the value of delay to an unlikely value ...*/
*xcorCoef = NoWayxcorCoef;			/* setting the cross correlation coefficient to an unlikely value ...*/


if (opt > 0)							/* Execution mode */
{/* opt >0 */

#ifdef DEBUG_ZEO_2		
	printf ("\nFunction call #%d\n",i_call);
#endif	


/*-----------------------------------------------------------------------------------*/		
/* Steps that need to be perfromed the first time the function is called 				 */
/*-----------------------------------------------------------------------------------*/	
	
	if (i_call == 0)
		{/*i_call == 0*/
			if ((Nseg < 1) || (Nseg >= lng_full/5))
				{
					sprintf (buf,"Number of segments (%d) null or too large, or vector length too small (%d) for %d segments ",Nseg,lng_full,Nseg);
					error_message ("hilbertdelay_V2",buf,0);
					return (2); 
				} 
			
			lng = (int)((float)lng_full / (float)Nseg);	/* calculating individual segment length */
			
			maxdel = lng/2;				/* delays should not exceed one third of the segment length (and that's pushing it !)*/
			
			m=0;
			while ((int)pow((double)2,(double)m) < lng) ++m;	/* find closest power of two to the length of the series */
			olng = lng;										/* save old length*/
			lng = (int)pow((double)2,(double)m);	/* set new length as power of two actual padding length will double*/
																/* in order to correct for circular convolution effects */
			lng_use = lng;							/* useful length of spectrum after correction for circular convolution effects */

#ifdef DEBUG_ZEO_2
	printf ("selected m=%d for a padded segment length of %d, old segment length was %d\nVector holds %d segments\n",m,lng,olng,Nseg);	
#endif
		
			fftx = (COMPLEX *) calloc ((2*lng)+2,sizeof(COMPLEX));
			fftxc = (COMPLEX *) calloc ((2*lng)+2,sizeof(COMPLEX));
			ffty = (COMPLEX *) calloc ((2*lng)+2,sizeof(COMPLEX));
			fftyc = (COMPLEX *) calloc ((2*lng)+2,sizeof(COMPLEX));
			Pxy = (COMPLEX *) calloc ((2*lng)+2,sizeof(COMPLEX));
			Px = (float *) calloc ((2*lng)+2,sizeof(float));
			Pxx = (COMPLEX *) calloc ((2*lng)+2,sizeof(COMPLEX));
			Py = (float *) calloc ((2*lng)+2,sizeof(float));
			Pyy = (COMPLEX *) calloc ((2*lng)+2,sizeof(COMPLEX));
			Rxx = (COMPLEX *) calloc ((2*lng)+2,sizeof(COMPLEX));
			Ryy = (COMPLEX *) calloc ((2*lng)+2,sizeof(COMPLEX));
			Rxy = (float *) calloc ((2*lng)+2,sizeof(float));
			HRxy = (float *) calloc ((2*lng)+2,sizeof(float));
			xcpy = (float *) calloc ((2*olng)+2,sizeof(float));
			ycpy = (float *) calloc ((2*olng)+2,sizeof(float));
			xp = (float *) calloc ((2*lng)+2,sizeof(float));
			yp = (float *) calloc ((2*lng)+2,sizeof(float));
			ubias = (float *) calloc ((2*lng)+2,sizeof(float));
			tmp_f_vect = (float *) calloc ((2*lng)+2,sizeof(float));
			tmp_f_vect2 = (float *) calloc ((2*lng)+2,sizeof(float));
			fftyca = (COMPLEX **) allocate2D ((2*lng)+2,Nseg,sizeof(COMPLEX));
			Pxya = (COMPLEX **) allocate2D ((2*lng)+2,Nseg,sizeof(COMPLEX));
			Ryya = (COMPLEX **) allocate2D ((2*lng)+2,Nseg,sizeof(COMPLEX));
			Rxxa = (COMPLEX **) allocate2D ((2*lng)+2,Nseg,sizeof(COMPLEX));	
			
			if (fftx == NULL ||  fftxc == NULL ||  ffty == NULL ||  fftyc == NULL ||  \
			    Pxy == NULL ||  Px == NULL ||  Py == NULL ||  xp == NULL ||  yp == NULL ||  \
				 ubias == NULL ||  tmp_f_vect == NULL ||  Pxx == NULL ||  Pyy == NULL ||  \
				 Rxx == NULL ||  Ryy == NULL ||  fftyca == NULL ||  Pxya == NULL ||  \
				 Ryya == NULL ||  Rxxa == NULL ||  tmp_f_vect2 == NULL)
				{
					printf ("\nFatal Error : Failed to Allocate memory\a\n");
					printf ("Abandon Lab Immediately !\n\n");
					return(2);
				};

			/* creating a vector to remove the bowtie artifact from the auto and cross correlation curves, and set to zero 
							their irrelevant values */
			if (biasrem == 1)
			{
				for (i=0;i<(2*lng);++i)
					{		
						if (i < olng)
							{
								ubias[i] = (float)olng / (float)(olng - i);
							}
						else
							{
								ubias[i] = 0.0;
							}
					}
			}
			
			a = (float)olng; /* Scaling parameter for spectral estimates  */
			
			strt = 0;				/* setting up for segments loop */
			nd = 0;
			for (sg=0;sg<Nseg;++sg)					/* segments counter */
			{/* for sg */
				strt = sg * olng;
				nd = strt + olng;
				
				cnt = 0;
				for (i=strt;i<nd;++i) 
					{
						tmp_f_vect[cnt] = y[i];		/* copying segment number (sg+1) */
						++cnt;
					}
					
				if (dtrnd == 1)
					{/*dtrnd == 1*/
						detrend (tmp_f_vect,ycpy,olng,&alfy,&bety);		/* removing linear trend alf? and bet? are the linear fit coefficients for the reference time course*/
						padd (ycpy,yp,0.0,olng+1,olng,2*lng);					/* Zero padding of detrended reference time course*/
					}/*dtrnd == 1*/
				else
					{/*dtrnd != 1*/
						zeromean (tmp_f_vect,ycpy,olng);					/* removing the mean only */
						padd (ycpy,yp,0.0,olng+1,olng,2*lng);			/* Zero padding of reference time course*/
					}/*dtrnd != 1*/
							
				float_to_complex (yp,ffty,2*lng); 			/* reformatting reference vector into complex vector */
		
				fft (ffty,m+1);									/* Radix 2 Butterfly fft computation of reference vector */
				
				c_conj (ffty,fftyc,2*lng);							/* Computing the conjugate of ffty */
				
				c_mult (ffty,fftyc,Ryy,2*lng);		/* Powerspectrum of y (called Ryy) */
				
				for (i=0;i<2*lng;++i) 						/* copying Power spectrum of y and conjugate of fft of individual segment into storage matrix */
					{
						fftyca[i][sg].real = fftyc[i].real;
						fftyca[i][sg].imag = fftyc[i].imag;
						Ryya[i][sg].real = Ryy[i].real;
						Ryya[i][sg].imag = Ryy[i].imag;
					}
				
			}/* for sg */
			
			for (sg=0;sg<Nseg;++sg)					/* computing sum periodogram of y */
				{/* for sg */
					for (i=0;i<2*lng;++i)
						{/* for i */
							
							if (sg == 0) 
								{
									Ryy[i].real = Ryya[i][sg].real;
									Ryy[i].imag = Ryya[i][sg].imag;
								}
							else 
								{/* sg > 0 */
									Ryy[i].real = Ryy[i].real + Ryya[i][sg].real;
									Ryy[i].imag = Ryy[i].imag + Ryya[i][sg].imag;
								}/* sg > 0 */
							
						}/* for i */
						
				}/* for sg */
			
			c_scale (Ryy,Ryy,1.0/((float)Nseg * a),2*lng);					/* scaling periodogram */
			
			ifft (Ryy,m+1);													/* calculating autocorrelation of y*/
		
		}/*i_call == 0*/

/*-----------------------------------------------------------------------------------*/		
/* Steps that need to be repeated each time the function is called with a new vector */
/*-----------------------------------------------------------------------------------*/	

		strt = 0;							/* setting up for segments loop */
		nd =0;
		for (sg=0;sg<Nseg;++sg)					/* segments counter */
			{/* for sg */
				strt = sg * olng;
				nd = strt + olng;
				
				cnt = 0;
				for (i=strt;i<nd;++i) 
					{
						tmp_f_vect[cnt] = x[i];		/* copying segment number (sg+1) */
						++cnt;
					}

				if (dtrnd == 1)
					{/*dtrnd == 1*/
						detrend (tmp_f_vect,xcpy,olng,&alfx,&betx);		/* removing linear trend alf? and bet? are the linear fit coefficients*/
						padd (xcpy,xp,0.0,olng+1,olng,2*lng);					/* Zero padding of detrended time course*/
					}/*dtrnd == 1*/
				else
					{/*dtrnd != 1*/
					   zeromean (tmp_f_vect,xcpy,olng);							/* removing the mean only*/
						padd (xcpy,xp,0.0,olng+1,olng,2*lng);			/* Zero padding of time course*/
					}/*dtrnd != 1*/
				
				float_to_complex (xp,fftx,2*lng);					/* reformatting vector into complex vector */
		
				fft (fftx,m+1);											/* Radix 2 Butterfly fft computation of vector */
				
				c_conj (fftx,fftxc,2*lng);							/* Computing the conjugate of fftx */
				
				c_mult (fftx,fftxc,Rxx,2*lng);						/* Powerspectrum of x (called Rxx) */
				
#ifdef DEBUG_ZEO_2	
		write_float (xp,"dbg_xdp.txt",2*lng);
		write_float (yp,"dbg_ydp.txt",2*lng);
		printf ("a = %f\n",a);
#endif
				
				for (i=0;i<2*lng;++i)									/* copying fftyc at segment sg from storage array */
					{
						fftyc[i].real = fftyca[i][sg].real;
						fftyc[i].imag = fftyca[i][sg].imag;
					}
				
				c_mult (fftx,fftyc,Pxy,2*lng);					/* Computing the cross power spectrum */
				
				for (i=0;i<2*lng;++i)									/* storing the power spectrum and the cross power spectrum at */
					{														/*different segments */
						Pxya[i][sg].real = Pxy[i].real;
						Pxya[i][sg].imag = Pxy[i].imag;
						Rxxa[i][sg].real = Rxx[i].real;
						Rxxa[i][sg].imag = Rxx[i].imag;
					}

			}/* for sg */
		
		for (sg=0;sg<Nseg;++sg)								/* calculating the sum of the periodograms */
			{/* for sg */
				for (i=0;i<2*lng;++i)
					{/* for i*/
						if (sg == 0)
							{	
								Pxy[i].real = Pxya[i][sg].real;
								Pxy[i].imag = Pxya[i][sg].imag;
								Rxx[i].real = Rxxa[i][sg].real;
								Rxx[i].imag = Rxxa[i][sg].imag;
							}
						else 
							{	
								Pxy[i].real = Pxy[i].real + Pxya[i][sg].real;
								Pxy[i].imag = Pxy[i].imag + Pxya[i][sg].imag;
								Rxx[i].real = Rxx[i].real + Rxxa[i][sg].real;
								Rxx[i].imag = Rxx[i].imag + Rxxa[i][sg].imag;
							}
					}/* for i */
			}/* for sg */
		
		
		c_scale (Rxx,Rxx,1.0/((float)Nseg * a),2*lng); /* calculating average Rxx periodogram	*/
		
		c_scale (Pxy,Pxy,2.0/((float)Nseg * a),2*lng);	/* calculating average Pxy and scaling it by 2 periodogram	*/
			
		tmp_c.real = 0.0;									/*discarding half of the cross power spectrum and padding it back to lng */
		tmp_c.imag = 0.0;
		
		c_padd (Pxy,Pxy,tmp_c,lng_use+1,lng_use,2*lng);
				
		ifft (Pxy,m+1);									/* inverse FFT of the scaled power spectrum */
		
		ifft (Rxx,m+1);									/*calculating autocorrelation of x*/
	  
		c_get (Pxy,Rxy,0,2*lng);					/* seperation of real and imaginary parts, only extract meaningful segment */
		c_get (Pxy,HRxy,1,2*lng);  

		if (biasrem == 1)
			{
				f_mult (ubias,Rxy,Rxy,2*lng);				/* removing bowtie artifact and setting redundant values to zero */
				f_mult (ubias,HRxy,HRxy,2*lng);
			}
		
#ifdef DEBUG_ZEO_2		
		write_float (Rxy,"dbg_Rxy.txt",2*lng);
		write_float (HRxy,"dbg_HRxy.txt",2*lng);
		write_float (ubias,"dbg_ubias.txt",2*lng);
#endif		
		
		for (i=0;i<lng-1;++i)							/* searching for the Zero crossing	*/
			{/* for i */
				if (i > maxdel) 
					{/* i > maxdel */
						/*sprintf (buf,"Delay larger than 1/2 segment length (%d)",maxdel);
						error_message ("hilbertdelay_V2",buf,0);*/
						return (3);	
					}/* i > maxdel */
				
				if (HRxy[i] == 0.0)
					{/* HRxy[i] == 0.0 */	
						
						if (Rxy[i] > 0.0) 
										*slp = 1.0;
								else 
										*slp = -1.0;
										
						izero = (float) i;
						*xcor = Rxy[i];					/* storing value of max covariance */
						i=lng;
					}/* HRxy[i] == 0.0 */
				else 
					{/* HRxy[i] != 0.0 */
						if ((HRxy[i] * HRxy[i+1]) < 0.0)
							{
								/* the sign of slp was used to determine the sign of the cross correlation  */
								/* the sign of slp should be the same as that of Rxy[i] close to izero */
								/* moreover, I think the use of the sign of Rxy[i] is better since with no */
								/*subtraction performed, I am less sensitive to high freq. noise */
								if (Rxy[i] >= 0.0) 
										*slp = 1.0;
								else 
										*slp = -1.0;
								
								if ((*slp > 0.0)) 
									{
										f_i = (float) (i);
										f_i1 = (float) (i+1);
										reg_pnt = 0.0;
										linear_interp (&HRxy[i],&f_i,&HRxy[i+1],&f_i1,&reg_pnt,&izero,1);

										/* find the peak of the cross correlation curve */
										k = 0;
                        		for (j=-3;j<=3;++j)
                         	 		{
                            		 if (((i+j) >= 0) && ((i+j) < lng))
                              	  {
                              	    tmp_f_vect[k] = (float) (i+j);
                              	    tmp_f_vect2[k] = Rxy[i+j];
                              	    ++k;
                              	  }
                            		}
                        		if (k > 1)
                           		{/* at least a 1st order interpolation must be performed */
                              	*xcor = Lagrange_interp (tmp_f_vect,tmp_f_vect2,izero,k);
                           		}


										i = lng;
									}
							}
					}/* HRxy[i] != 0.0 */
			
			}/* for i */
		
				*del = izero + Dtx;		/* delay is in sample units corrected by the sampling time difference*/	
					
				if (Rxx[0].real && Ryy[0].real)
					*xcorCoef = *xcor / sqrt (Rxx[0].real * Ryy[0].real) * *slp; /*correction for sign of cross correlation coefficient (slp = 1.0 or -1.00*/
				else
					{	
						#ifdef DEBUG_ZEO_3		
							printf ("\nZero Variance...\n");
						#endif
					}
				
				/* set vx and vy */
				
				*vx = Rxx[0].real;
				*vy = Ryy[0].real;
				
		++i_call;
		return (0);
		
}/* opt > 0 */										/* Execution mode */
else if (opt == 0)
	{/* opt == 0 */ 

#ifdef DEBUG_ZEO_3		
	printf ("\nCleaning Up...\n");
#endif

		free (fftx);								/*Cleaning up used space*/
		free (fftxc);
		free (ffty);	
		free (fftyc);
		free (Px);
		free (Py);
		free (Pxx);
		free (Pyy);
		free (Pxy);
		free (Rxx);
		free (Ryy);
		free (Rxy);
		free (HRxy);
		free (xp);
		free (yp);
		free (ubias);
		free (xcpy);
		free (ycpy);
		free (tmp_f_vect);
		free (tmp_f_vect2);
		
		free2D ((char **)fftyca,lng+2);
		free2D ((char **)Pxya,lng+2);
		free2D ((char **)Rxxa,lng+2);
		free2D ((char **)Ryya,lng+2);

		i_call = 0;
		
		*del = NoWayDelay;			/* setting variables to out of bound values for safety */
		*xcorCoef = NoWayxcorCoef;
		
		return (0);						
	}/* opt == 0 */

 return(0) ;
}


/*-----------------------------------------------------------------------------------*/	

static void hunwrap (float del, float fs, float T, float slp, int wrp, int unt, float *delu )
{/*hunwrap*/

	float pi = 3.1416, tmp;
	
	
	if (fs > 0.0)
		{	/* delay should be in seconds */
			del = del / fs;
		}
	
	if (T > 0.0)
		{/* Period unwrapping possible */
 			if (slp < 0.0)
 				{/* Unwrapping required to determine correct delay */

 							tmp = del * 360.0 / T;		/* from time to polar angle */
							tmp = punwrap (tmp,1);		/* unwrap */
							tmp = tmp + 180.0;			/* augment by pi */
							del = tmp * T / 360.0;		/* from polar to time */
 				}
 			
 			/* Now for the case where we get negative delays although no wrapping has been
				done, namely because of the sampling time correction. */
			
			if (del < 0.0 || del > T)
				{
					tmp = del * 360.0 / T;		/* from time to polar angle */
					if (del < 0.0)
						{ tmp = tmp + 360.0; }
					else 
						{
							if (del > T)
								tmp = tmp - 360.0;
						}
					del = tmp * T / 360.0;	/* from polar to time */
				}  
			
 			if (wrp == 1)
								{/* map of (0-pi) to (0-pi) and (pi-2pi) to (pi-0) */
									tmp = del * 360.0 / T;		/* from time to polar angle */
									tmp = punwrap (tmp,1);		/* unwrap */
									del = tmp * T / 360.0;		/* from polar to time */
								}/* map of (0-pi) to (0-pi) and (pi-2pi) to (pi-0) */

 			if (unt == METH_DEGREES) del = del * 360.0 / T;		/* from time to polar angle in degrees*/
			if (unt == METH_RADIANS) del = del * 2 * pi / T;		/* from time to polar angle in degrees*/	
	}/* Period unwrapping possible */
	
	*delu = del;
	
	return;
}/*hunwrap*/
 
/*-----------------------------------------------------------------------------------*/	

static void disp_comp_vect (COMPLEX *v,int l)
        {
                int i;

                printf ("\n");
                
                if ((l-1) == 0)
                	{
                		printf ("V = (%f,%f)\n",(*v).real,(*v).imag);
                	}
                else 
                {
                	for (i=0;i<l;++i)
                        {
                                printf ("V[%d] = (%f,%f)\t",i,v[i].real,v[i].imag);
                        }
                printf ("\n");
                }
                return;

        }

/*-----------------------------------------------------------------------------------*/	

static void disp_vect (float *v,int l)
        {
                int i;

                printf ("\n");
                if ((l-1) == 0)
                        {
                                printf ("V = %f\n",*v);
                        }
                else 
                {
                        for (i=0;i<l;++i)
                        {
                                printf ("V[%d] = %f\t",i,v[i]);
                        }
                        printf ("\n");
                }
                return;

        }

/*-----------------------------------------------------------------------------------*/	

static int is_vect_null ( float * v , int npts )
        {/*is_vect_null*/
        
        int i , is_ts_null;
                
        is_ts_null = 1;                 /* Start loop in bad faith */
        
        for (i=0;i<npts;++i) 
                  {
                        if (v[i] != 0.0) 
                                {       
                                        is_ts_null = 0; /* vector is not null */
                                        break;          
                                }
                  }
                 
        return (is_ts_null);    
        
        }/*is_vect_null*/
 
/*-----------------------------------------------------------------------------------*/	


static int write_float (float *x,char *f_name,int n_points)
	
   
    { /*  */
     int i;
     
     FILE*internal_file;
     
     internal_file = fopen (f_name,"w");
     if (internal_file == NULL) {
     								printf ("\aCould not open %s \n",f_name);
     								printf ("Exiting program\n");
     								exit (0);
    						   	}
   
   for (i=0;i<n_points;++i) fprintf (internal_file,"%f\n",x[i]);  
     fclose (internal_file);
      return (i);  							     
   }



/* support functions declaration for main algorithm             */
/* -----------------------END-----------------------------------*/
