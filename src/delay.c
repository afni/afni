/*---------------------------------------------------------------------------*/
/*
  This file contains routines for implementing the 3ddelay functions.
	Based on fim+.c  by B. Douglas Ward

  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.

*/

/*---------------------------------------------------------------------------*/
/*
  Include software for linear regression analysis and sorting numbers.
*/

#include "RegAna.c"
#include "ranks.c"


/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* 
   Initialize independent variable X matrix 
*/

int init_indep_var_matrix 
(
  int q,                      /* number of parameters in the baseline model */
  int p,                      /* number of parameters in the baseline model 
			         plus number of ideals */
  int NFirst,                 /* index of first image to use in the analysis */
  int N,                      /* total number of images used in the analysis */
  int polort,                 /* degree of polynomial ort */
  int num_ort_files,          /* number of ort time series files */
  int num_ideal_files,        /* number of ideal time series files */
  MRI_IMAGE ** ort_array,     /* ort time series arrays */
  int ** ort_list,            /* list of ort time series */
  MRI_IMAGE ** ideal_array,   /* ideal time series arrays */
  int ** ideal_list,          /* list of ideal time series */
  float * x_bot,              /* minimum of stimulus time series */
  float * x_ave,              /* average of stimulus time series */
  float * x_top,              /* maximum of stimulus time series */
  int * good_list,            /* list of good time points */
  matrix * x                  /* independent variable matrix */
)

{
  const int BIG_NUMBER = 33333;
  int i;                    /* time index */
  int m;                    /* X matrix column index */
  int n;                    /* X matrix row index */
  int is;                   /* input ideal index */
  float * far = NULL;
  int nx, ny, iq, nq;
  int Ngood;
  matrix xgood;


  /*----- Initialize X matrix -----*/
  matrix_create (N, p, x);
  matrix_initialize (&xgood);


  /*----- Set up columns of X matrix corresponding to polynomial orts -----*/
  for (m = 0;  m <= polort;  m++)
    for (n = 0;  n < N;  n++)
      {
	i = n + NFirst;
	(*x).elts[n][m] = pow ((double)i, (double)m);
      }


  /*----- Set up columns of X matrix corresponding to ort time series -----*/
  for (is = 0;  is < num_ort_files;  is++)
    {
      far = MRI_FLOAT_PTR (ort_array[is]);
      nx = ort_array[is]->nx;
      ny = ort_array[is]->ny;

      if (ort_list[is] == NULL)
	for (iq = 0;  iq < ny;  iq++)
	  {
	    for (n = 0;  n < N;  n++)
	      {
		i = n + NFirst;
		(*x).elts[n][m] = *(far + iq*nx + i);
	      }
	    m++;
	  }
      else
	{
	  nq = ort_list[is][0];
	  for (iq = 1;  iq <= nq;  iq++)
	    {
	      for (n = 0;  n < N;  n++)
		{
		  i = n + NFirst;
		  (*x).elts[n][m] = *(far + ort_list[is][iq]*nx + i);
		}
	      m++;
	    }
	}
    }


  /*----- Set up columns of X matrix corresponding to ideal time series -----*/
  for (is = 0;  is < num_ideal_files;  is++)
    {
      far = MRI_FLOAT_PTR (ideal_array[is]);
      nx = ideal_array[is]->nx;
      ny = ideal_array[is]->ny;

      if (ideal_list[is] == NULL)
	for (iq = 0;  iq < ny;  iq++)
	  {
	    for (n = 0;  n < N;  n++)
	      {
		i = n + NFirst;
		(*x).elts[n][m] = *(far + iq*nx + i);
	      }
	    
	    m++;
	  }
      else
	{
	  nq = ideal_list[is][0];
	  for (iq = 1;  iq <= nq;  iq++)
	    {
	      for (n = 0;  n < N;  n++)
		{
		  i = n + NFirst;
		  (*x).elts[n][m] = *(far + ideal_list[is][iq]*nx + i);
		}

	      m++;
	    }
	}
    }


  /*----- Remove row if ideal contains value over 33333 -----*/
  Ngood = N;
  m = 0;
  for (n = 0;  n < N;  n++)
    {
      for (is = q;  is < p;  is++)
	{
	  if ((*x).elts[n][is] >= BIG_NUMBER)  break;
	}
      if (is < p)
	{
	  Ngood--;
	}
      else
	{
	  good_list[m] = n;
	  m++;
	}
    }
  matrix_extract_rows ((*x), Ngood, good_list, &xgood);
  matrix_equate (xgood, x);


  /*----- Find min, max, and ave for each column of the X matrix -----*/
  for (is = 0;  is < p;  is++)
    {      
      x_bot[is] = x_top[is] = (*x).elts[0][is];
      x_ave[is] = 0.0;
      for (n = 0;  n < Ngood;  n++)
	{
	  if ((*x).elts[n][is] < x_bot[is])  x_bot[is] = (*x).elts[n][is];  
	  if ((*x).elts[n][is] > x_top[is])  x_top[is] = (*x).elts[n][is];
	  x_ave[is] += (*x).elts[n][is] / Ngood;
	}
    }
  
  
  matrix_destroy (&xgood);

  return (Ngood);

}


/*---------------------------------------------------------------------------*/
/*
  Initialization for the delay analysis.
*/

int init_delay 
(
  int q,                      /* number of parameters in the baseline model */
  int p,                      /* number of parameters in the baseline model 
			         plus number of ideals */
  int N,                      /* total number of images used in the analysis */
  int num_idealts,            /* number of ideal time series */
  matrix xdata,               /* independent variable matrix */
  matrix * x_base,            /* extracted X matrix    for baseline model */
  matrix * xtxinvxt_base,     /* matrix:  (1/(X'X))X'  for baseline model */
  matrix * x_ideal,           /* extracted X matrices  for ideal models */
  matrix * xtxinvxt_ideal,    /* matrix:  (1/(X'X))X'  for ideal models */
  float ** rarray             /* ranked arrays of ideal time series */
)

{
  int * plist = NULL;         /* list of model parameters */
  int ip, it;                 /* parameter indices */
  int is, js;                 /* ideal indices */ 
  int jm;                     /* lag index */
  int ok;                     /* flag for successful matrix calculation */
  matrix xtxinv_temp;         /* intermediate results */
  vector ideal;               /* ideal vector */
  vector coef_temp;           /* intermediate results */
  vector xres;                /* vector of residuals */
  float sse_base;             /* baseline model error sum of squares */ 
        

  /*----- Initialize matrix -----*/
  matrix_initialize (&xtxinv_temp);
  vector_initialize (&ideal);
  vector_initialize (&coef_temp);
  vector_initialize (&xres);


  /*----- Allocate memory -----*/
  plist = (int *) malloc (sizeof(int) * p);   MTEST (plist);


  /*----- Initialize matrices for the baseline model -----*/
  for (ip = 0;  ip < q;  ip++)
    plist[ip] = ip;
  ok = calc_matrices (xdata, q, plist, x_base, &xtxinv_temp, xtxinvxt_base);
  if (!ok)  { matrix_destroy (&xtxinv_temp);  return (0); };


  /*----- Initialize matrices for ideal functions -----*/
  for (is = 0;  is < num_idealts;  is++)
    {
      for (ip = 0;  ip < q;  ip++)
	{
	  plist[ip] = ip;
	}

      plist[q] = q + is;

      ok = calc_matrices (xdata, q+1, plist, 
			  &(x_ideal[is]), &xtxinv_temp, &(xtxinvxt_ideal[is]));
      if (!ok)  { matrix_destroy (&xtxinv_temp);  return (0); };
    }


  /*----- Set up the ranked array for each ideal -----*/
  for (is = 0;  is < num_idealts;  is++)
    {
      /*----- Convert column of matrix to vector -----*/
      column_to_vector (xdata, q+is, &ideal);

      /*----- Calculate regression coefficients for baseline model -----*/
      calc_coef (*xtxinvxt_base, ideal, &coef_temp);

      /*----- Calculate the error sum of squares for the baseline model -----*/
      sse_base = calc_resids (*x_base, coef_temp, ideal, &xres);
    
      /*----- Form rank array from residual array -----*/
      rarray[is] = rank_darray (N, xres.elts);

    }


  /*----- Destroy matrix -----*/
  matrix_destroy (&xtxinv_temp);
  vector_destroy (&ideal);
  vector_destroy (&coef_temp);
  vector_destroy (&xres);


  /*----- Deallocate memory -----*/
  free (plist);   plist = NULL;


  return (1);
}


/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* 
   Calculate the sign function.
*/

float sign (float x)

{
  if (x > 0.0)  return (1.0);
  if (x < 0.0)  return (-1.0);
  return (0.0);
}


/*---------------------------------------------------------------------------*/

int Read_part_file_delay (float *x,
					char *f_name,
					int a,
					int b)
   
    { 
     
     int cnt=0,ex,line_num;
     float buf;
     FILE*file_in;
     
     file_in = fopen (f_name,"r");
     if (file_in == NULL) {
            printf ("\aCould not open %s \n",f_name);
           printf ("Exiting @ Read_file function\n");
            exit (0);
         }
     
     if (a > b || a==0) {
     				printf ("\a\n\33[1mError in (from , to) line numbers\n\33\[0m");
     				printf ("Exiting @Read_part_file function \n");
     				exit (0);
     		   }
     
     line_num = 1;	
     if (a == 1) {
     			ex = fscanf (file_in,"%f",&x[cnt]);	
     			++cnt;
     			}				   	
      else  ex = fscanf (file_in,"%f",&buf);					   	
     ++line_num;
     while (ex != EOF && line_num <= b)
      {
        if (line_num >=a && line_num <=b) 
        {
         ex = fscanf (file_in,"%f",&x[cnt]);
         ++cnt;
         if (ex == EOF) --cnt;
         }
        else 
        {
         ex = fscanf (file_in,"%f",&buf);
         }
        ++line_num;
        
      }
      
      if (ex == EOF) 
      	{
      	    --line_num;
      		printf ("\n\33[1mEOF reached before line \33[0m%d\n",b);
      		printf ("Only %d lines were read, from line %d to %d\n",cnt,a,line_num-1);
      	}
      
      fclose (file_in);
      return (cnt);  							     
   }




/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
  

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/




