#include "SUMA_gen_include.h"
#include "SUMA_suma.h"

/*! Taken from filexists 
returns 1 if file can be read/found
*/
int SUMA_filexists (char *f_name)
{/*SUMA_filexists*/
 	FILE *outfile;
 	
 	outfile = fopen (f_name,"r");
 	if (outfile == NULL)
 		return (0);
 	else 
 		fclose (outfile);
 		return (1);
 		
}/*SUMA_filexists*/

/*!
 
File : Read_file.c
Author : Ziad Saad
Date : 19 Dec. 1994

Purpose : 
			Reads a file sequence of int numbers, one value per line .

Usage : 
	
	int SUMA_Read_dfile (int *x,char *f_name,int n_points);
or int SUMA_Read_file (float *x,char *f_name,int n_points);
	
Input Parameters:
		x, (int*) or (float *) array where the values will be stored.
		f_name, (char)* string holding file name.
		n_points, (int) number of points to be read from file. if  set to 0,
			 then all the file will be read .

Output parameters :
				Number of points read.

Side effects :	
			function does not check for array overflow while reading file.
			
*/	
int SUMA_Read_dfile (int *x,char *f_name,int n_points)
   
    { /* pass a 0 to n_points if you want to read till EOF */
     int cnt=0,ex,dec;
     
     FILE*internal_file;
     
     internal_file = fopen (f_name,"r");
     if (internal_file == NULL) {
     								fprintf(SUMA_STDERR, "\aCould not open %s \n",f_name);
     								fprintf(SUMA_STDERR, "Exiting @ SUMA_Read_file function\n");
     								exit (0);
     						   	}
     ex = fscanf (internal_file,"%d",&x[cnt]);					   	
     while (ex != EOF)
      {
        ++cnt;
        /* NOT WORKING, RETURNS SIZEOF (FLOAT) .....
        if (sizeof(x) < cnt)
        	{
        	  fprintf(SUMA_STDERR, "%d = sizeof(x)\n",sizeof(x));
        	  fprintf(SUMA_STDERR, "\nNot Enough Memory Allocated \n\a");
        	  fprintf(SUMA_STDERR, "Exiting @SUMA_Read_file function\n");
        	  exit (0);
        	}
        ............................................ */
        ex = fscanf (internal_file,"%d",&x[cnt]);
        
        if ((n_points != 0) && (cnt == n_points)) ex = EOF;
      }
      
      if (cnt < n_points) 
      	{
      	 fprintf(SUMA_STDERR, "\a\nAttempt to read %d points failed,\n",n_points);
      	 fprintf(SUMA_STDERR, " file contains %d points only.\n",cnt);
      	 do {
      	 
      	 fprintf(SUMA_STDERR, "End Execution (Yes (1) No (0) ? : ");
      	 ex=scanf ("%d",&dec);
      	 } while (ex != 1 || (dec != 1 && dec !=0));
      	 if (dec)
      	  {
      	    fprintf(SUMA_STDERR, "Exiting @ SUMA_Read_file function\n");
     		exit (0);
     		}
         else fprintf(SUMA_STDERR, "\nContinuing execution with %d points\n",cnt);

      	}
      
      fclose (internal_file);
      return (cnt);  							     
   }
int SUMA_Read_file (float *x,char *f_name,int n_points)
   
    { /* pass a 0 to n_points if you want to read till EOF */
     int cnt=0,ex,dec;
     
     FILE*internal_file;
     
     internal_file = fopen (f_name,"r");
     if (internal_file == NULL) {
     								fprintf(SUMA_STDERR, "\aCould not open %s \n",f_name);
     								fprintf(SUMA_STDERR, "Exiting @ SUMA_Read_file function\n");
     								exit (0);
     						   	}
     ex = fscanf (internal_file,"%f",&x[cnt]);					   	
     while (ex != EOF)
      {
        ++cnt;
        /* NOT WORKING, RETURNS SIZEOF (FLOAT) .....
        if (sizeof(x) < cnt)
        	{
        	  fprintf(SUMA_STDERR, "%d = sizeof(x)\n",sizeof(x));
        	  fprintf(SUMA_STDERR, "\nNot Enough Memory Allocated \n\a");
        	  fprintf(SUMA_STDERR, "Exiting @SUMA_Read_file function\n");
        	  exit (0);
        	}
        ............................................ */
        ex = fscanf (internal_file,"%f",&x[cnt]);
        
        if ((n_points != 0) && (cnt == n_points)) ex = EOF;
      }
      
      if (cnt < n_points) 
      	{
      	 fprintf(SUMA_STDERR, "\a\nAttempt to read %d points failed,\n",n_points);
      	 fprintf(SUMA_STDERR, " file contains %d points only.\n",cnt);
      	 do {
      	 
      	 fprintf(SUMA_STDERR, "End Execution (Yes (1) No (0) ? : ");
      	 ex=scanf ("%d",&dec);
      	 } while (ex != 1 || (dec != 1 && dec !=0));
      	 if (dec)
      	  {
      	    fprintf(SUMA_STDERR, "Exiting @ SUMA_Read_file function\n");
     				exit (0);
     		}
         else fprintf(SUMA_STDERR, "\nContinuing execution with %d points\n",cnt);

      	}
      
      fclose (internal_file);
      return (cnt);  							     
   }

/*!**
 
File : Read_2Dfile.c
Author : Ziad Saad
Date : Sat Nov 14 18:52:31 CST 1998/remix Wed Feb  6 17:22:32 EST 2002

 
Purpose : 
	Reads a file of float numbers, with n_cols values per line
 
 
Usage : 
	n_rows_read = SUMA_Read_2Dfile ( char *f_name, float **x, int n_cols, int n_rows)
 
 
Input paramters : 
		f_name, (char)* string holding file name.
		x, (float)** array where the values will be stored.
		n_cols, (int) number of columns per line.
		n_rows, (int) number of rows . 
 
 
Returns : 
	n_rows_read, (int) number of rows read from file. 
	     -1 if critcial operations fail.
		  if EOF is reached before n_rows, n_rows_read reflects the 
		  number of rows read. 
 
 
Support : 
 
 
 
Side effects : 
 
 
***/
  
 
int SUMA_Read_2Dfile (char *f_name, float **x,  int n_cols, int n_rows)
{/*SUMA_Read_2Dfile*/
  int ir=0, ic=0, ex;
  FILE*internal_file;
  static char FuncName[]={"SUMA_Read_2Dfile"};
  
  
 	 internal_file = fopen (f_name,"r");
     if (internal_file == NULL) {
     								fprintf (SUMA_STDERR,"%s: \aCould not open %s \n",FuncName, f_name);
     								return (-1);
     						   	}
 	  ir = 0;
	  while (ir < n_rows)
      {
 			ic = 0;
			while (ic < n_cols)
				{
					ex = fscanf (internal_file,"%f",&x[ir][ic]);	
					if (ex == EOF)
						{
							fprintf(stderr,"Error SUMA_Read_2Dfile: Premature EOF\n");
							fclose (internal_file);
							return (n_rows);
						}
					++ic;
				}
			++ir;
		}
		
fclose (internal_file);
return (ir);		
		
}/*SUMA_Read_2Dfile*/

/*!
 
Purpose : 
	Reads a file of integer numbers, with n_cols values per line
 
Usage : 
	ans = SUMA_Read_2Ddfile (char *f_name, int **x,int n_rows, int n_cols)
 
 
Input paramters : 
	\param	x, (int)** array where the values will be stored.
	\param	f_name, (char)* string holding file name.
	\param	n_rows, (int) number of rows to be read from file. 
	\param	n_cols, (int) number of columns per line.

	\ret Number of rows read (maybe incomplete rows)
*/
int SUMA_Read_2Ddfile (char *f_name, int **x, int n_rows, int n_cols)
{/*SUMA_Read_2Ddfile*/
  int ir, ic, ex;
  FILE*internal_file;
  static char FuncName[]={"SUMA_Read_2Ddfile"};
     
  internal_file = fopen (f_name,"r");
  if (internal_file == NULL) {
     	fprintf (SUMA_STDERR,"%s: \aCould not open %s \n",FuncName, f_name);
		return (-1);
     	}

  
     
	  ir = 0;
	  while (ir < n_rows)
      {
 			ic = 0;
			while (ic < n_cols)
				{
					ex = fscanf (internal_file,"%d",&x[ir][ic]);	
					if (ex == EOF)
						{
							fprintf(stderr,"Error SUMA_Read_2Ddfile: Premature EOF\n");
							fclose (internal_file);
							return(ir);
						}
					++ic;
				}
			++ir;
		}
		
fclose (internal_file);
return (ir);		
		
}/*SUMA_Read_2Ddfile*/


/*! 
count the number of float values in a file
-1 if the file could not be open
*/ 
int SUMA_float_file_size (char *f_name)
   
    { 
     int cnt=0,ex;
     float buf;
     
     FILE*internal_file;
     
     internal_file = fopen (f_name,"r");
     if (internal_file == NULL) {
     								printf ("\aCould not open %s \n",f_name);
     								return (-1);
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


/*! Taken from SUMA_alloc_problem */
void SUMA_alloc_problem (char *s1)
 
 {
 
   printf ("\n\n\a\33[1mError in memory allocation\33[0m\n");
   printf ("Error origin : %s\n\n",s1);
   printf ("Exiting Program ..\n\n");
   exit (0);
   
  }

/*!

Taken from allocate2D.c - Make matrix of given size (rows x cols) and type

The type is given by element_size (2 = ints, 4 = floats, 8 = doubles).
Exits if the matrix could not be allocated.

    char **allocate2D(int rows,int cols,int element_size)
SIZE might vary depending on platform used !!!

This function was adapted from DSP_in_C functions in 
C Language Algorithms for Digital Signal Processing 
by
Bruce Kimball, Paul Embree and Bruce Kimble 
1991, Prentice Hall

				Ziad Saad						Oct_21_96
*************************************************************************/

char **SUMA_allocate2D (int rows,int cols,int element_size)

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

/*!

Taken from free2D.c - Free all elements of matrix 

Frees the 2D array (rows and cols) allocated using allocate2D

Error message and exit if improper structure is
passed to it (null pointers or zero size matrix).

    void free2D(char **a, int rows);

This function was adapted from DSP_in_C functions in 
C Language Algorithms for Digital Signal Processing 
by
Bruce Kimball, Paul Embree and Bruce Kimble 
1991, Prentice Hall


				Ziad Saad						Oct_22_96
*************************************************************************/
void SUMA_free2D(char **a,int rows)
    
{
    int i;
    
/* free each row of data */
    for(i = 0 ; i < rows ; i++) free(a[i]);

/* free each row pointer */
    free((char *)a);
    a = NULL;           /* set to null for error */
    
	return;
}

/*!
 
Taken from error_message.c
Author : Ziad Saad
Date : 26 Jan 95

Purpose : 
			displays error message, and exits the program if ext = 1;

Usage : 
		void error_message (s1,s2,ext);

Input Parameters:
		s1, (char*) pointer to string to be printed for error location.
		s2, (char*) pointer to string holding error message.
		ext (int) if ext = 1 the program is aborted
		
			
Header Files    */	

void SUMA_error_message (char *s1,char *s2,int ext)
 
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

/*!
 
File : Taken from iswordin.c
Author : Ziad Saad
Date : Mon Sep 22 18:52:28 CDT 1997
 
Purpose : 
 	To find out if an array of characters is an element of another array 
 	of characters
 
 
Input paramters : 
 
 		S (char *) : Mother String (character array)
 		Ssub (char *) : Subset array
 		
 
Usage : 
		int SUMA_iswordin (const char *S, const char *Ssub );
 
 (you could use space characters in the two strings like:
 	SUMA_iswordin ("Hello The Gump","The Gu"); would return a 1
 	SUMA_iswordin ("Hello The Gump",""); would return a 1
 	SUMA_iswordin ("Hello The Gump","Tha"); would return a 0
 	SUMA_iswordin ("Hello The Gump"," "); would return a 1
 	SUMA_iswordin ("Hel","Hello sdsd"); would return a 0 
 	
Returns : 
          returns 1 if Ssub is part of S 
          returns 0 if Ssub is not part of S 
          returns -1 if either Ssub or S is NULL
          returns -2 if both Ssub and S are NULL

 
 
Support : 
 
 
Side effects : 
 
 
 
***/
 
int SUMA_iswordin (const char *sbig, const char *ssub)
{/*SUMA_iswordin*/
 int i=0,j=0;
 
 if (sbig == NULL && ssub == NULL) return (-2);
 if (sbig == NULL || ssub == NULL) return (-1);
 
 if (strlen(sbig) < strlen(ssub))
 		return (0);
 
 j=0;
 while (sbig[i] != '\0' && ssub[j] != '\0')
 	{
 		if (sbig[i] == ssub[j])
 			{
 				++j;
 				/*printf ("j=%d ",j);*/
 			}
 		else j=0;
 	++i;
 	}
 
	if (j == strlen (ssub))
		return (1);
	else 
		return (0);

}/*SUMA_iswordin*/

/*!
 
File : Taken from disp_dmat.c
Author : Ziad Saad
Date : Tue Nov 17 13:19:26 CST 1998
 
Purpose : 
	Displays on the terminal the 2D matrix of integers
 
 
Usage : 
		 SUMA_disp_dmat (int **v,int nr, int nc, int SpcOpt  )
 
 
Input paramters : 
 	v (int **) the 2D matrix to display
	nr (int) the number of rows in v
	nc (int) the number of columns
	SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
	
 
 
Returns : 
 
 
 
Support : 
 
 
 
Side effects : 
 
 
 
***/
 
void SUMA_disp_dmat (int **v,int nr, int nc , int SpcOpt)
{/*SUMA_disp_dmat*/
   char spc [40]; 
 	int i,j;
	
	if (!SpcOpt)
		sprintf(spc," ");
	else if (SpcOpt == 1)
		sprintf(spc,"\t");
	else
		sprintf(spc," , ");
	
	fprintf (SUMA_STDOUT,"\n");
   for (i=0; i < nr; ++i)
		{
			for (j=0; j < nc; ++j)
					fprintf (SUMA_STDOUT,"%d%s",v[i][j],spc);
			fprintf (SUMA_STDOUT,"\n");
		}
}/*SUMA_disp_dmat*/

/*!**
 
File : SUMA_MiscFunc.c
Author : Ziad Saad
Date : Tue Nov 17 13:19:26 CST 1998
 
Purpose : 
	Displays on the terminal the 2D float matrix
 
 
Usage : 
		 SUMA_disp_mat (float **v,int nr, int nc, int SpcOpt )
 
 
Input paramters : 
 	v (float **) the 2D matrix to display
	nr (int) the number of rows in v
	nc (int) the number of columns
	SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
	
 
 
*/ 
void SUMA_disp_mat (float **v,int nr, int nc , int SpcOpt)
{/*SUMA_disp_mat*/
   char spc [40]; 
 	int i,j;
	
	if (!SpcOpt)
		sprintf(spc," ");
	else if (SpcOpt == 1)
		sprintf(spc,"\t");
	else
		sprintf(spc," , ");
	
	fprintf (SUMA_STDOUT,"\n");
   for (i=0; i < nr; ++i)
		{
			for (j=0; j < nc; ++j)
					fprintf (SUMA_STDOUT, "%4.2f%s",v[i][j],spc);
			fprintf (SUMA_STDOUT,"\n");
		}
}/*SUMA_disp_mat*/

/*!
File : SUMA_MiscFunc.c from disp_vect.c
Author : Ziad Saad
Date : 23 Oct 1996

Purpose : 
         displays a variable or vector of type float

Usage : 
       void SUMA_disp_vect (float *v,int l);

        

Input Parameters:
                v, (float *) pointer to input  vector or variable 
                ln, (int) lenght of complex vector, set equal to 1 if vector is a variable

*/
void SUMA_disp_vect (float *v,int l)
        { int i;

                fprintf (SUMA_STDOUT,"\n");
                if ((l-1) == 0)
							fprintf (SUMA_STDOUT,"%f\n",*v);
                else 
                {
                	for (i=0;i<l;++i)
										  fprintf (SUMA_STDOUT,"%f\t",v[i]);
                	fprintf (SUMA_STDOUT,"\n");
                }
                return;
        }

/*
File : SUMA_MiscFunc.c,  from disp_dvect.c
Author : Ziad Saad
Date : 23 Oct 1996

Purpose : 
         displays a variable or vector of type int

Usage : 
       void SUMA_disp_dvect (int *v,int l);

        

Input Parameters:
                v, (int *) pointer to input vector or variable 
                ln, (int) lenght of complex vector, set equal to 1 if vector is a variable

*/
void SUMA_disp_dvect (int *v,int l)
        { int i;

                fprintf (SUMA_STDOUT,"\n");
                if ((l-1) == 0)
							fprintf (SUMA_STDOUT, "%d\n",*v);
                else 
                {
                	for (i=0;i<l;++i)
							fprintf (SUMA_STDOUT,"%d\t",v[i]);
                	
						fprintf (SUMA_STDOUT,"\n");
                }
                return;
        }


/*!
   
File : SUMA_MiscFunc.c from ~Zlib/code/etime.c
Author : Ziad Saad
Date : Mon Dec 28 13:06:36 CST 1998
   
Purpose : 
   computes the time elapsed between different operations, for example:
	
	float delta_t;
	struct  timeval tt;
	 
	SUMA_etime (&tt, 0);  :the zero tells the function to start a new counter 
   
	 :operations are here ...... 
	
	delta_t = SUMA_etime (&tt, 1);  :computes the time between tt the time stamp (set in the previous call) 
									   :delta_t is the elapsed time in seconds
   
Usage : 
	delta_t = SUMA_etime (tt, Report );
   
   
Input paramters : 
   tt (struct  timeval *) : a pointer that holds the time stamp structure
	Report (int ) : a (0/1) flag to signal time reporting or the start of a new timer
	 
   
   
Returns : 
	delta_t (float) : the time elapsed between the time stamp and the call to etime   
   
   
Support : 
   #include <sys/time.h>

   
   
Side effects : 
   
   
   
***/
float SUMA_etime (struct  timeval  *t, int Report  )
{/*SUMA_etime*/
   char FuncName[100]; 
   struct  timeval  tn;
	float Time_Fact = 1000000.0;
	float delta_t;

   /* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_etime");
   
	/* get time */
	gettimeofday(&tn,0);
	
	if (Report)
		{
			delta_t = (((tn.tv_sec - t->tv_sec)*Time_Fact) + (tn.tv_usec - t->tv_usec)) /Time_Fact;
		}
	else
		{
			t->tv_sec = tn.tv_sec;
			t->tv_usec = tn.tv_usec;
			delta_t = 0.0;
		}
		
	return (delta_t);
	
}/*SUMA_etime*/
   
/*!
   
File : SUMA_MiscFunc.c, from ~Zlib/code/isinsphere.c
Author : Ziad Saad
Date : Fri Nov 20 22:56:31 CST 1998
   
Purpose : 
   determines which nodes lie inside a sphere
   
   
Usage : 
		Ret =  SUMA_isinsphere (XYZ, nr, S_cent , S_rad , BoundIn)
   
Input paramters : 
	XYZ (float ** ) : Nx3 matrix containing the XYZ of the nodes to consider
	nr  (int )	: that's N, the number of nodes
	S_cent (float *) : a 3x1 vector containing the XYZ coordinates of the center of the sphere
	S_rad  (float ) : the radius of the sphere
	BoundIn (int) : 0/1 set to 0 for exclusive boundary  
   
   
Returns : 
	a structure of the type SUMA_ISINSPHERE with the following fields
	
	.IsIn    (int *) : a pointer to an [nIsIn x 1] vector will contain the indices into the rows of XYZ that 
	                  locates the nodes inside the sphere. 
	.nIsIn   (int) : the number of nodes in the sphere
   .d (float *) : a pointer to an [nIsIn x 1]  vector containing the distance of those nodes inside the sphere to the center.
   
   
   
Support : 
   
   
   
Side effects : 
   
   
   
***/
SUMA_ISINSPHERE SUMA_isinsphere (float ** XYZ, int nr, float *S_cent , float S_rad , int BoundIn )
{/*SUMA_isinsphere*/
   char FuncName[100]; 
   float *t, t0, t1, t2, ta;
	int k, *IsIn;
	SUMA_ISINSPHERE IsIn_strct;
	
   /* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_isinsphere");
   
	IsIn_strct.nIsIn = 0;
		
	t = (float *) calloc (nr, sizeof(float));
	IsIn = (int *) calloc (nr, sizeof(int));
	
	if (!t || !IsIn)
		{
			SUMA_alloc_problem (FuncName);
			return (IsIn_strct);
		}
	
	
	if (BoundIn) /* split into two to avoid checking for this condition all the time */
		{
			for (k=0; k < nr; ++k)
				{
					/* Net distance to center */
					t0 = XYZ[k][0] - S_cent[0];	
					t1 = XYZ[k][1] - S_cent[1];	
					t2 = XYZ[k][2] - S_cent[2];	

					ta = sqrtf (t0 * t0 + t1 * t1 + t2 * t2);
					
					if (ta <= S_rad)
						{
							IsIn[IsIn_strct.nIsIn] = k;
							t[IsIn_strct.nIsIn] = ta;
							++(IsIn_strct.nIsIn);
						}
				}
		}
	else
		{
			for (k=0; k < nr; ++k)
				{
					/* Net distance to center */
					t0 = XYZ[k][0] - S_cent[0];	
					t1 = XYZ[k][1] - S_cent[1];	
					t2 = XYZ[k][2] - S_cent[2];	

					ta = sqrtf (t0 * t0 + t1 * t1 + t2 * t2);
					
					if (ta < S_rad)
						{
							IsIn[IsIn_strct.nIsIn] = k;
							t[IsIn_strct.nIsIn] = ta;
							++(IsIn_strct.nIsIn);
						}
				}
		}
			
	/* get ridd of extra allocation space*/
	IsIn_strct.d = (float *) calloc (IsIn_strct.nIsIn, sizeof(float));
	IsIn_strct.IsIn = (int *) calloc (IsIn_strct.nIsIn, sizeof(int));
	
	if (!IsIn_strct.d || !IsIn_strct.IsIn )
		{
			IsIn_strct.nIsIn = 0;
			SUMA_alloc_problem(FuncName);
			return (IsIn_strct);
		}
	
	SUMA_COPY_VEC (t, IsIn_strct.d, IsIn_strct.nIsIn, float , float);
	SUMA_COPY_VEC (IsIn, IsIn_strct.IsIn , IsIn_strct.nIsIn, int , int);
	
	free (t);
	free (IsIn);
	
	return (IsIn_strct);
	
}/*SUMA_isinsphere*/

/*!**
   
File : SUMA_MiscFunc from ~Zlib/code/ isinbox.c
Author : Ziad Saad
Date : Fri Nov 20 23:52:52 CST 1998
   
Purpose : 
      determines which nodes lie inside a box

   
   
Usage : 
	Ret = SUMA_isinbox (float ** XYZ, int nr, S_cent , S_dim ,  BoundIn)
   
   
Input paramters : 
 	XYZ (float ** ) : Nx3 matrix containing the XYZ of the nodes to consider
	nr  (int )	: that's N, the number of nodes
	S_cent (float *) : a 3x1 vector containing the XYZ coordinates of the center of the box
	S_dim  (float *) : a 3x1 containing the size of the box from side 
	                 to side along the three dimentions
	BoundIn (int) : 0/1 set to 0 if you want to have exclusive boundary conditions 
   
   
Returns : 
	a structure of the type SUMA_ISINBOX with the following fields
	
	IsIn    (int *) : a pointer to an [nIsIn x 1] vector that will contain indices into the rows of XYZ that 
	                  locates the nodes inside the box. 
	d	(float *): The distance between each of the nodes and the center of the box
	nIsIn   (int) : the number of nodes in the box
  
   
Support : 
   
   
   
Side effects : 
   
   
   
***/
SUMA_ISINBOX SUMA_isinbox (float ** XYZ, int nr, float *S_cent , float *S_dim , int BoundIn )
{/*SUMA_isinbox*/
   
	static char FuncName[]={"SUMA_isinbox"}; 
   float t0, t1, t2, hdim0, hdim1, hdim2, *d;
	int k , *IsIn;
	SUMA_ISINBOX IsIn_strct;

   /*
	fprintf(SUMA_STDOUT,"%f %f %f, %f %f %f, %d, %f, %f, %f\n",\
		S_cent[0], S_cent[1], S_cent[2], S_dim[0], S_dim[1], S_dim[2], nr, XYZ[0][0], XYZ[0][1], XYZ[0][2]);
	*/
		
	IsIn_strct.nIsIn = 0;	

	hdim0 = S_dim[0]/2;
	hdim1 = S_dim[1]/2;
	hdim2 = S_dim[2]/2;
	
	IsIn = (int *) calloc (nr, sizeof(int));
	d = (float *)calloc(nr, sizeof(float));
	
	if (!IsIn || !d)
		{
			SUMA_alloc_problem (FuncName);
			return (IsIn_strct);
		}

	if (BoundIn) /* split into two to avoid checking for this condition all the time */
		{
			/*fprintf(SUMA_STDERR,"%s: inbound\n", FuncName);*/
			for (k=0; k < nr; ++k)
				{
				/*fprintf(SUMA_STDERR,"%s: inbound %d\n", FuncName, k);*/
				/* relative distance to center */
					t0 = hdim0 - fabsf(XYZ[k][0] - S_cent[0]);	
					t1 = hdim1 - fabsf(XYZ[k][1] - S_cent[1]);	
					t2 = hdim2 - fabsf(XYZ[k][2] - S_cent[2]);	
					
					if (t0 >= 0)
						if (t1 >= 0)
							if (t2 >= 0)
								{
									IsIn[IsIn_strct.nIsIn] = k;
									d[IsIn_strct.nIsIn] = sqrt(t0*t0+t1*t1+t2*t2);
									++(IsIn_strct.nIsIn);
								}
				}			
				/*fprintf(SUMA_STDERR,"%s: outbound\n", FuncName);*/

		}
	else
		{
			for (k=0; k < nr; ++k)
				{
					/* relative distance to center */
					t0 = hdim0 - fabsf(XYZ[k][0] - S_cent[0]);	
					t1 = hdim1 - fabsf(XYZ[k][1] - S_cent[1]);	
					t2 = hdim2 - fabsf(XYZ[k][2] - S_cent[2]);	
					
					if (t0 > 0)
						if (t1 > 0)
							if (t2 > 0)
								{
									IsIn[IsIn_strct.nIsIn] = k;
									d[IsIn_strct.nIsIn] = sqrt(t0*t0+t1*t1+t2*t2);
									++(IsIn_strct.nIsIn);
								}
				}
		}
	
	if (IsIn_strct.nIsIn) {
		/*fprintf(SUMA_STDERR,"%s: realloc\n", FuncName);*/

		/* get ridd of extra allocation space*/
		IsIn_strct.IsIn = (int *) calloc (IsIn_strct.nIsIn, sizeof(int));
		IsIn_strct.d = (float *)calloc(IsIn_strct.nIsIn, sizeof(float));

		if (!IsIn_strct.IsIn || !IsIn_strct.d)
			{
				IsIn_strct.nIsIn = 0;
				SUMA_alloc_problem(FuncName);
				return (IsIn_strct);
			}

		SUMA_COPY_VEC (IsIn, IsIn_strct.IsIn , IsIn_strct.nIsIn, int , int);
		SUMA_COPY_VEC (d, IsIn_strct.d, IsIn_strct.nIsIn, float, float);
	} else {
		/*fprintf(SUMA_STDERR,"%s: NADA\n", FuncName);*/
		IsIn_strct.IsIn = NULL;
		IsIn_strct.d = NULL;
	}
	
	/*fprintf(SUMA_STDERR,"%s: freeing\n", FuncName);*/
	free (IsIn);
	free (d);
	/*fprintf(SUMA_STDERR,"%s: freed\n", FuncName);*/

	return (IsIn_strct) ;

}/*SUMA_isinbox*/

/*!
free SUMA_ISINBOX structure contents. 
Structure pointer is not freed
*/
SUMA_Boolean SUMA_Free_IsInBox (SUMA_ISINBOX *IB)
{
	if (IB == NULL) {
		fprintf (SUMA_STDERR,"Error SUMA_Free_IsInBox: pointer to null cannot be freed\n");
		return (NOPE);
	}
	if (IB->IsIn != NULL) free(IB->IsIn);
	if (IB->d != NULL) free (IB->d);
	IB->nIsIn = 0;
	return (YUP);	
}
/*!**
File : SUMA_MiscFunc.c
\author Ziad Saad
Date : Fri Feb 8 16:29:06 EST 2002
   
Purpose : 
   Read SureFit data
   
*/

/*!**  
Function: SUMA_Point_At_Distance 
Usage : 
P2 = SUMA_Point_At_Distance (U, P1, d)
	
Returns the two points that are at a distance d from P1 along the direction of U  
	
Input paramters : 
\param U (float *) 3x1 vector specifying directions  along x, y, z axis       
\param P1 (float *) 3x1 vector containing the XYZ of P1
\param d (float) distance from P1

   
Returns : 
\return  P2 (float **) 2x3 matrix containg XYZ of 2 points equidistant from P1 
					along U (first row) and -U (second row)
			NULL if there are problems in the land of chocolate
   
Support : 
\sa   Point_At_Distance.m
\sa  To free P2, use: SUMA_free2D((char **)P2, 2);
   
***/
float **SUMA_Point_At_Distance(float *U, float *P1, float d)
{/*SUMA_Point_At_Distance*/
   char FuncName[100]; 
	float bf, **P2, P1orig[3], Uorig[3];
	float m, n, p, q, D, A, B, C;
	int flip, i;
	
   /* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_Point_At_Distance");

	if (d == 0) {
		fprintf(SUMA_STDERR,"Error %s: d is 0. Not good, Not good at all.\n", FuncName);
		return (NULL);
	}
	fprintf (SUMA_STDOUT,"%s: U %f, %f, %f, P1 %f %f %f, d %f\n", FuncName,\
			U[0], U[1], U[2], P1[0], P1[1], P1[2], d);
			
	/* store initial values */
	P1orig[0] = P1[0]; 	
	P1orig[1] = P1[1]; 
	P1orig[2] = P1[2]; 

	Uorig[0] = U[0];
	Uorig[1] = U[1];
	Uorig[2] = U[2];
	
	/* normalize U such that U(0) = 1 */
	flip = 0;
	if (U[0] == 0) { /* must flip X with some other coordinate */
		if (U[1] != 0) {/*U[1] != 0; */
			U[0] = U[1]; U[1] = 0;
			bf = P1[0]; P1[0] = P1[1]; P1[1] = bf;
			flip = 1;
		} else {	/*U[1] = 0; */
			if (U[2] != 0) { /* U[2] != 0 */
				U[0] = U[2]; U[2] = 0;
				bf = P1[0]; P1[0] = P1[2]; P1[2] = bf;
				flip = 2;
			} else { /* U[2] = 0 */
				fprintf(SUMA_STDERR, "Error %s: 0 direction vector.\n", FuncName);
				return (NULL);
			}
		}/*U[1] = 0; */
	}/*U[0] = 0; */

	
	U[0] /= U[0]; 
	U[1] /= U[0];
	U[2] /= U[0];

	/* Now U is clean, calculate P2 */	
	m = U[1];
	n = U[2];

	q = P1[1] - m*P1[0];
	p = P1[2] - n*P1[0];

	/* Now find P2 */
	A = (1 + n*n + m*m);
	B = -2 * P1[0] + 2 * m * (q - P1[1]) + 2 * n * (p - P1[2]);
	C = P1[0]*P1[0] + (q - P1[1])*(q - P1[1]) + (p - P1[2])*(p - P1[2]) - d*d;

	D = B*B - 4*A*C;
	if (D < 0) {
		fprintf(SUMA_STDERR, "Error %s: Negative Delta.\n", FuncName);
		return (NULL);
	}

	P2 = (float **)SUMA_allocate2D(2,3, sizeof(float));
	if (P2 == NULL) {
		fprintf(SUMA_STDERR, "Error %s: Could not allocate for 6 floats! What is this? What is the matter with you?!\n", FuncName);
		return (NULL);
	}

	P2[0][0] = (-B + sqrt(D)) / (2 *A);
	P2[1][0] = (-B - sqrt(D)) / (2 *A);

	P2[0][1] = m * P2[0][0] + q;
	P2[1][1] = m * P2[1][0] + q;

	P2[0][2] = n * P2[0][0] + p;
	P2[1][2] = n * P2[1][0] + p;


	/* if flipping was performed, undo it */
	if (flip == 1) {
	 for (i=0; i < 2; ++i) {
		 bf = P2[i][1];
		 P2[i][1] = P2[i][0];
		 P2[i][0] = bf;
		}
	} else if (flip == 2){
	 for (i=0; i < 2; ++i) {
		 bf = P2[i][2]; 
		 P2[i][2] = P2[i][0];
		 P2[i][0] = bf;
		}
	}	

	for (i=0; i < 3; ++i) {
		P1[i] = P1orig[i];
		U[i] = Uorig[i];
	}

	/* make sure 1st point is along the same direction */
	Uorig[0] = P2[0][0] - P1[0]; /* use Uorig, not needed anymore */
	Uorig[1] = P2[0][1] - P1[1];
	Uorig[2] = P2[0][2] - P1[2];

	SUMA_DOTP_VEC(Uorig, P1, bf, 3, float, float)
	if (bf < 0) {
		fprintf(SUMA_STDOUT,"Flipping...\n");
		for (i=0; i< 3; ++i) {
			bf = P2[0][i];
			P2[0][i] = P2[1][i]; P2[1][i] = bf;
		}
	}

return (P2);
	
}/*SUMA_Point_At_Distance*/

/*!

Function: SUMA_Point_To_Line_Distance
Usage : 
Ret = SUMA_Point_To_Line_Distance (float **Points, int N_nodes, float *P1, float *P2, float *d2, float *d2min, int *i2min)
	
Calculates the squared distance between the points in Points and the line formed by P1-P2  
	
Input paramters : 
\param Points (float **) N_nodes x 3 matrix containing XYZ of N_nodes nodes 
\param N_nodes (int) Number of nodes in Points     
\param P1 (float *) 3x1 vector containing the XYZ of P1
\param P2 (float *) 3x1 vector containing the XYZ of P2
\param d2 (float *) N_nodes x 1 vector containing the squared distance of each node in Points to the line P1-P2
       d2 must be pointing to a pre-allocated space
\param d2min (float *) pointer to the smallest squared distance
\param i2min (int *) pointer to the index (into Points) of the node with the shortest distance

The squared distance is returned to save on a square root operation which may not be necessary to compute for all nodes
   
Returns : 
\return  Ret (SUMA_Boolean) YUP/NOPE for success/failure

\sa labbook NIH-2, p 37 
 
*/
SUMA_Boolean SUMA_Point_To_Line_Distance (float **Points, int N_points, float *P1, float *P2, float *d2, float *d2min, int *i2min)
{
	char FuncName[100];
	float U[3], Un, xn, yn, zn, dx, dy, dz;
	int i;
	
	sprintf(FuncName,"SUMA_Point_To_Line_Distance");
	
	if (N_points < 1) {
		fprintf(SUMA_STDERR,"Error %s: N_points is 0.\n",FuncName);
		return (NOPE);
	}
	/* Calculate normalized unit vector of line formed by P1, P2 */
	U[0] = P2[0] - P1[0];
	U[1] = P2[1] - P1[1];
	U[2] = P2[2] - P1[2];
	Un = sqrt(U[0]*U[0] + U[1]*U[1] + U[2]*U[2]);
	
	if (Un == 0) {
		fprintf(SUMA_STDERR,"Error %s: P1 and P2 are identical.\n",FuncName);
		return (NOPE);
	}
	
	U[0] /= Un;
	U[1] /= Un;
	U[2] /= Un;
	
	/* calculate the distances and keep track of the minimum distance while you're at it */
	
	/*bad practise, only returned pointers are allocated for in functions */
	/*
	d2 = (float *)calloc(N_points, sizeof(float)); */
	
	if (d2 == NULL) {
		fprintf(SUMA_STDERR,"Error %s: d2 not allocated for.\n",FuncName);
		return (NOPE);
	}
	
	
	/* do the first point to initialize d2min without an extra if statement */
	 i = 0;
	 xn = Points[i][0] - P1[0];
	 yn = Points[i][1] - P1[1];
	 zn = Points[i][2] - P1[2];
	 
	 dx = (U[1]*zn - yn*U[2]);
	 dy = (U[0]*zn - xn*U[2]);
	 dz = (U[0]*yn - xn*U[1]);
	 
	 d2[i] = dx*dx+dy*dy +dz*dz; /* save the sqrt for speed */
	 *d2min = d2[i];
	 *i2min = i;
	 /* Now do the rest */
	for (i=1; i < N_points; ++i) {
		xn = Points[i][0] - P1[0];
		yn = Points[i][1] - P1[1];
		zn = Points[i][2] - P1[2];

		dx = (U[1]*zn - yn*U[2]);
		dy = (U[0]*zn - xn*U[2]);
		dz = (U[0]*yn - xn*U[1]);

		d2[i] = dx*dx+dy*dy +dz*dz; /* save the sqrt for speed */
		if (d2[i] < *d2min) {
			*d2min = d2[i];
			*i2min = i;
		}
	}
	return (YUP);
}

/*!

Function: SUMA_Point_To_Point_Distance
Usage : 
Ret = SUMA_Point_To_Point_Distance (float **Points, int N_nodes, float *P1, float *d2, float *d2min, int *i2min)
	
Calculates the squared distance between the points in Points and  P1-P2  
	
Input paramters : 
\param Points (float **) N_nodes x 3 matrix containing XYZ of N_nodes nodes 
\param N_nodes (int) Number of nodes in Points     
\param P1 (float *) 3x1 vector containing the XYZ of P1
\param d2 (float *) N_nodes x 1 vector containing the squared distance of each node in Points to P1
       d2 must be pointing to a pre-allocated space
\param d2min (float *) pointer to the smallest squared distance
\param i2min (int *) pointer to the index (into Points) of the node with the shortest distance

The squared distance is returned to save on a square root operation which may not be necessary to compute for all nodes
   
Returns : 
\return  Ret (SUMA_Boolean) YUP/NOPE for success/failure
 
*/
SUMA_Boolean SUMA_Point_To_Point_Distance (float **Points, int N_points, float *P1, float *d2, float *d2min, int *i2min)
{
	char FuncName[100];
	float xn, yn, zn;
	int i;
	
	sprintf(FuncName,"SUMA_Point_To_Point_Distance");
	
	if (N_points < 1) {
		fprintf(SUMA_STDERR,"Error %s: N_points is 0.\n",FuncName);
		return (NOPE);
	}
	
	
	/* calculate the distances and keep track of the minimum distance while you're at it */
	
	if (d2 == NULL) {
		fprintf(SUMA_STDERR,"Error %s: d2 not allocated for.\n",FuncName);
		return (NOPE);
	}
	
	
	/* do the first point to initialize d2min without an extra if statement */
	 i = 0;
	 xn = Points[i][0] - P1[0];
	 yn = Points[i][1] - P1[1];
	 zn = Points[i][2] - P1[2];
	 
	 d2[i] = xn*xn + yn*yn + zn*zn; /* save the sqrt for speed */
	 *d2min = d2[i];
	 *i2min = i;
	 /* Now do the rest */
	for (i=1; i < N_points; ++i) {
		xn = Points[i][0] - P1[0];
		yn = Points[i][1] - P1[1];
		zn = Points[i][2] - P1[2];


	 	d2[i] = xn*xn + yn*yn + zn*zn; /* save the sqrt for speed */
		if (d2[i] < *d2min) {
			*d2min = d2[i];
			*i2min = i;
		}
	}
	return (YUP);
}


/*! Sorting Functions */
#define SUMA_Z_QSORT_structs

	typedef struct {
		float x;
		int Index;
	} SUMA_Z_QSORT_FLOAT;

	typedef struct {
		int x;
		int Index;
	} SUMA_Z_QSORT_INT;

int compare_SUMA_Z_QSORT_FLOAT (SUMA_Z_QSORT_FLOAT *a, SUMA_Z_QSORT_FLOAT *b )
	{
		if (a->x < b->x)
			return (-1);
		else if (a->x == b->x)
			return (0);
		else if (a->x > b->x)
			return (1);
		/* this will never be reached but it will shut the compiler up */
		return (0);
	}
	
	
int compare_SUMA_Z_QSORT_INT (SUMA_Z_QSORT_INT *a, SUMA_Z_QSORT_INT *b )
	{
		if (a->x < b->x)
			return (-1);
		else if (a->x == b->x)
			return (0);
		else if (a->x > b->x)
			return (1);
		/* this will never be reached but it will shut the compiler up */
		return (0);
	}
	
   
/*!**
   
File : from ~/Programs/C/Z/Zlib/code/SUMA_z_qsort.c
Author : Ziad Saad
Date : Fri Nov 20 15:30:55 CST 1998
   
Purpose : 
   A sorting function that uses C library's qsort and returns an index table 
	with it. So, if you're sorting vector x, you'll get y (y is STORED IN x, make a copy of x
	before calling the function if you want to preserve the unsorted version of x), a sorted version of
	x, and I such that x(I) = y;
   
   
Usage : 
	I = SUMA_z_qsort ( x , nx  );
	I = SUMA_z_dqsort ( x , nx );
   
Input paramters : 
   x (*float) vector of  floats, sorted array is returned in x
	nx (int) number of elements in x
	
	If you are sorting integers, use SUMA_z_dqsort where x is an (int *)
	
	
Returns : 
   I (int *) [nx x 1] vector containing the index table
	x, of course, is sorted
   
   
Support : 
   
   
   
Side effects : 
   
   
   
***/
int *SUMA_z_qsort (float *x , int nx )
{/*SUMA_z_qsort*/
   char FuncName[100]; 
   int *I, k;
	SUMA_Z_QSORT_FLOAT *Z_Q_fStrct;
	
   /* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_z_qsort");
   
			/* allocate for the structure */
			Z_Q_fStrct = (SUMA_Z_QSORT_FLOAT *) calloc(nx, sizeof (SUMA_Z_QSORT_FLOAT));
			I = (int *) calloc (nx,sizeof(int));
	
			if (!Z_Q_fStrct || !I)
				{
					fprintf(SUMA_STDERR,"Error %s: Allocation problem.\n",FuncName);
					return (NULL);
				}
			
			for (k=0; k < nx; ++k) /* copy the data into a structure */
				{
					Z_Q_fStrct[k].x = x[k];
					Z_Q_fStrct[k].Index = k;
				}

			/* sort the structure by it's field value */
			qsort(Z_Q_fStrct, nx, sizeof(SUMA_Z_QSORT_FLOAT), (int(*) (const void *, const void *)) compare_SUMA_Z_QSORT_FLOAT);
			
			/* recover the index table */
			for (k=0; k < nx; ++k) /* copy the data into a structure */
				{
					x[k] = Z_Q_fStrct[k].x;
					I[k] = Z_Q_fStrct[k].Index;
				}
				
			/* free the structure */
			free (Z_Q_fStrct);
			
			/* return */
			return (I);
	
		
}/*SUMA_z_qsort*/


int *SUMA_z_dqsort (int *x , int nx )
{/*SUMA_z_dqsort*/
   char FuncName[100]; 
   int *I, k;
	SUMA_Z_QSORT_INT *Z_Q_iStrct;
	
   /* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_z_dqsort");
   
			/* allocate for the structure */
			Z_Q_iStrct = (SUMA_Z_QSORT_INT *) calloc(nx, sizeof (SUMA_Z_QSORT_INT));
			I = (int *) calloc (nx,sizeof(int));
	
			if (!Z_Q_iStrct || !I)
				{
					fprintf(SUMA_STDERR,"Error %s: Allocation problem.\n",FuncName);
					return (NULL);
				}
			
			for (k=0; k < nx; ++k) /* copy the data into a structure */
				{
					Z_Q_iStrct[k].x = x[k];
					Z_Q_iStrct[k].Index = k;
				}

			/* sort the structure by it's field value */
			qsort(Z_Q_iStrct, nx, sizeof(SUMA_Z_QSORT_INT), (int(*) (const void *, const void *)) compare_SUMA_Z_QSORT_INT);
			
			/* recover the index table */
			for (k=0; k < nx; ++k) /* copy the data into a structure */
				{
					x[k] = Z_Q_iStrct[k].x;
					I[k] = Z_Q_iStrct[k].Index;
				}
				
			/* free the structure */
			free (Z_Q_iStrct);
			
			/* return */
			return (I);
	
		
}/*SUMA_z_dqsort*/
   
   
   
/*--------------------- Matrix Sorting functions Begin -----------------------------------*/
   
typedef struct {
		float *x;
		int ncol;
		int Index;
	} SUMA_QSORTROW_FLOAT;


int compare_SUMA_QSORTROW_FLOAT (SUMA_QSORTROW_FLOAT *a, SUMA_QSORTROW_FLOAT *b)
	{
		int k;
		
		for (k=0; k < a->ncol ; ++k)
			{
				if (a->x[k] < b->x[k])
					return (-1);
				else if (a->x[k] > b->x[k])
					return (1);
			}
		return (0); /* They're similar */
	}

   
/*!  
   
Purpose : 
   Sort a matrix of floats by rows
	Imagine that each row is a word, the function sorts the rows as if in a dictionary list
   
	
Usage : 
		int * SUMA_fqsortrow (float **X , int nr, int nc )
   
   \param X (float ** ) matrix to sort by rows (if you need to preserve the original version of X you need to make a copy of it before calling the function )
   \param nr (int)  number of rows
   \param nc (int)  number of columns
	
	\ret ndx (int *) index table, such that Xsorted = X(ndx,:);
   
   
   
	\sa  SUMA_dqsortrow
*/
int * SUMA_fqsortrow (float **X , int nr, int nc  )
{/*SUMA_fqsortrow*/
   char FuncName[]={"SUMA_fqsortrow"}; 
   int k, *I;
	SUMA_QSORTROW_FLOAT *Z_Q_fStrct;
	
	   
	/* allocate for the structure */
	Z_Q_fStrct = (SUMA_QSORTROW_FLOAT *) calloc(nr, sizeof (SUMA_QSORTROW_FLOAT));
	I = (int *) calloc (nr,sizeof(int));
	
	if (!Z_Q_fStrct || !I)
		{
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for Z_Q_fStrct || I\n", FuncName);
		return (NULL);
		}

	for (k=0; k < nr; ++k) /* copy the data into a structure */
		{
			Z_Q_fStrct[k].x = X[k];
			Z_Q_fStrct[k].ncol = nc;
			Z_Q_fStrct[k].Index = k;
		}

	/* sort the structure by comparing the rows in X */
	qsort(Z_Q_fStrct, nr, sizeof(SUMA_QSORTROW_FLOAT), (int(*) (const void *, const void *)) compare_SUMA_QSORTROW_FLOAT);

	/* recover the index table */
	for (k=0; k < nr; ++k) 
		{
			X[k] = Z_Q_fStrct[k].x;
			I[k] = Z_Q_fStrct[k].Index;
		}
	
	/* free the structure */
	free (Z_Q_fStrct);

	/* return */
	return (I);
	
	
}/*SUMA_fqsortrow*/

	typedef struct {
		int *x;
		int ncol;
		int Index;
	} SUMA_QSORTROW_INT;

/* CODE */

int compare_SUMA_QSORTROW_INT (SUMA_QSORTROW_INT *a, SUMA_QSORTROW_INT *b)
	{
		int k;
		
		for (k=0; k < a->ncol ; ++k)
			{
				if (a->x[k] < b->x[k])
					return (-1);
				else if (a->x[k] > b->x[k])
					return (1);
			}
		return (0); /* They're similar */
	}

   
/*!  
   
Purpose : 
   Sort a matrix of ints by rows
	Imagine that each row is a word, the function sorts the rows as if in a dictionary list
   
	
Usage : 
    int * SUMA_dqsortrow (int **X, int nr, int nc)
   
   \param X (int ** ) matrix to sort by rows (if you need to preserve the original version of X you need to make a copy of it before calling the function )
   \param nr (int)  number of rows
   \param nc (int)  number of columns
	
	\ret ndx (int *) index table, such that Xsorted = X(ndx,:);
   
   
   
	\sa  SUMA_fqsortrow
*/

int * SUMA_dqsortrow (int **X , int nr, int nc  )
{/*SUMA_dqsortrow*/
   char FuncName[]={"SUMA_dqsortrow"}; 
   int k,  *I;
	SUMA_QSORTROW_INT *Z_Q_dStrct;
	
	
	/* allocate for the structure */
	Z_Q_dStrct = (SUMA_QSORTROW_INT *) calloc(nr, sizeof (SUMA_QSORTROW_INT));
	I = (int *) calloc (nr,sizeof(int));
	
	if (!Z_Q_dStrct || !I)
		{
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for Z_Q_dStrct || I\n", FuncName);
		return (NULL);
		}

	for (k=0; k < nr; ++k) /* copy the data into a structure */
		{
			Z_Q_dStrct[k].x = X[k];
			Z_Q_dStrct[k].ncol = nc;
			Z_Q_dStrct[k].Index = k;
		}

	/* sort the structure by comparing the rows in X */
	qsort(Z_Q_dStrct, nr, sizeof(SUMA_QSORTROW_INT), (int(*) (const void *, const void *)) compare_SUMA_QSORTROW_INT);

	/* recover the index table */
	for (k=0; k < nr; ++k) 
		{
			X[k] = Z_Q_dStrct[k].x;
			I[k] = Z_Q_dStrct[k].Index;
		}
	
	/* free the structure */
	free (Z_Q_dStrct);

	/* return */
	return (I);
	
	
}/*SUMA_dqsortrow*/


/*--------------------- Matrix Sorting functions END ------------------------*/
   

/* definitions for SUMA_MT_intersect */
#define SUMA_MT_EPSILON 0.000001
#define SUMA_MT_CROSS(dest,v1,v2) \
          dest[0]=v1[1]*v2[2]-v1[2]*v2[1]; \
          dest[1]=v1[2]*v2[0]-v1[0]*v2[2]; \
          dest[2]=v1[0]*v2[1]-v1[1]*v2[0];
#define SUMA_MT_DOT(v1,v2) (v1[0]*v2[0]+v1[1]*v2[1]+v1[2]*v2[2])
#define SUMA_MT_SUB(dest,v1,v2) \
          dest[0]=v1[0]-v2[0]; \
          dest[1]=v1[1]-v2[1]; \
          dest[2]=v1[2]-v2[2]; 
/*!

SUMA_MT_INTERSECT_TRIANGLE *
SUMA_MT_intersect_triangle(float *P0, float *P1, float **NodeList, int N_Node, int **FaceSetList, int N_FaceSet)

\param	P0 (float *) 3x1 containing XYZ of point 0
\param	P1 (float *) 3x1 containing XYZ of point 1
\param	NodeList (float **) N_Node x 3 containing the XYZ of nodes making up FaceSetList
\param	N_Node (int) number of nodes in NodeList
\param	FaceSetList (int **) N_FaceSet x 3 with each row representing a triangle. Triangles are defined
      	by their indices into NodeList 
\param	N_FaceSet (int) number of triangles in FaceSetList

\ret	MTI (SUMA_MT_INTERSECT_TRIANGLE *) pointer to structure containing 
		isHit (SUMA_Boolean *) N_FaceSet x 1 vector. isHit[i] = YUP --> FaceSet i is pierced by ray P0-->P1
		t (float *) distance to the plane in which the triangle lies
		u & v(float *) location withing the triangle of the intersection point

\sa Algorithm from:Moller & Trumbore 97
	Tomas Möller and Ben Trumbore. Fast, minimum storage ray-triangle intersection. 
	Journal of graphics tools, 2(1):21-28, 1997

*/ 
 
SUMA_MT_INTERSECT_TRIANGLE *
SUMA_MT_intersect_triangle(float *P0, float *P1, float **NodeList, int N_Node, int **FaceSetList, int N_FaceSet)
{
   double edge1[3], edge2[3], tvec[3], pvec[3], qvec[3];
   double det,inv_det;
	int iface;
	double vert0[3],vert1[3], vert2[3], dir[3], dirn, orig[3];
	float tmin, tmax, dii;
	SUMA_MT_INTERSECT_TRIANGLE *MTI;
	
	tmin = 10000000.0;
	tmax = 0.0;
	MTI = (SUMA_MT_INTERSECT_TRIANGLE *)malloc(sizeof(SUMA_MT_INTERSECT_TRIANGLE));
	if (MTI == NULL) {
		fprintf(SUMA_STDERR,"Error : Failed to allocate for MTI\n");
		return (MTI);
	}
	MTI->t = NULL;
	MTI->u = NULL;
	MTI->v = NULL;
	MTI->isHit = NULL;
	

	/* direction from two points */
	orig[0] = (double)P0[0];
	orig[1] = (double)P0[1];
	orig[2] = (double)P0[2];
	
	dir[0] = (double)P1[0] - orig[0];
	dir[1] = (double)P1[1] - orig[1];
	dir[2] = (double)P1[2] - orig[2];
	dirn = sqrt(dir[0]*dir[0]+dir[1]*dir[1]+dir[2]*dir[2]);
	dir[0] /= dirn;
	dir[1] /= dirn;
	dir[2] /= dirn;
	
	MTI->isHit = (SUMA_Boolean *)malloc(N_FaceSet*sizeof(SUMA_Boolean));
	MTI->t = (float *)calloc(N_FaceSet, sizeof(float));
	MTI->u = (float *)calloc(N_FaceSet, sizeof(float));
	MTI->v = (float *)calloc(N_FaceSet, sizeof(float));
	
	if (MTI->isHit == NULL || MTI->t == NULL || MTI->u == NULL || MTI->v == NULL) {
		fprintf(SUMA_STDERR,"Error : Failed to allocate for MTI->isHit | MTI->t | MTI->u | MTI->v\n");
		return (MTI);
	}
	MTI->N_hits = 0;
	for (iface= 0; iface < N_FaceSet; ++iface) {/* iface */
   	/* set up the coordinates in a humane nomenclature */
		vert0[0] = (double)NodeList[FaceSetList[iface][0]][0];
    	vert0[1] = (double)NodeList[FaceSetList[iface][0]][1];
   	vert0[2] = (double)NodeList[FaceSetList[iface][0]][2];
		
   	vert1[0] = (double)NodeList[FaceSetList[iface][1]][0];
    	vert1[1] = (double)NodeList[FaceSetList[iface][1]][1];
   	vert1[2] = (double)NodeList[FaceSetList[iface][1]][2];
		
   	vert2[0] = (double)NodeList[FaceSetList[iface][2]][0];
    	vert2[1] = (double)NodeList[FaceSetList[iface][2]][1];
   	vert2[2] = (double)NodeList[FaceSetList[iface][2]][2];
		
		/* find vectors for two edges sharing vert0 */
   	SUMA_MT_SUB(edge1, vert1, vert0);
   	SUMA_MT_SUB(edge2, vert2, vert0);

   	/* begin calculating determinant - also used to calculate U parameter */
   	SUMA_MT_CROSS(pvec, dir, edge2);

   	/* if determinant is near zero, ray lies in plane of triangle */
   	det = SUMA_MT_DOT(edge1, pvec);

	#ifdef SUMA_MT_TEST_CULL           /* define TEST_CULL if culling is desired */
   	if (det < SUMA_MT_EPSILON)
      	MTI->isHit[iface] = NOPE;
		else {
   		/* calculate distance from vert0 to ray origin */
   		SUMA_MT_SUB(tvec, orig, vert0);

   		/* calculate U parameter and test bounds */
   		MTI->u[iface] = (float)SUMA_MT_DOT(tvec, pvec);
   		if (MTI->u[iface] < 0.0 || MTI->u[iface] > det)
      		MTI->isHit[iface] = NOPE;
			else {
   			/* prepare to test V parameter */
   			SUMA_MT_CROSS(qvec, tvec, edge1);

   			 /* calculate V parameter and test bounds */
   			MTI->v[iface] = (float)SUMA_MT_DOT(dir, qvec);
   			if (MTI->v[iface] < 0.0 || MTI->u[iface] + MTI->v[iface] > det)
      			MTI->isHit[iface] = NOPE;
				else {
   				/* calculate t, scale parameters, ray intersects triangle */
   				MTI->t[iface] = (float)SUMA_MT_DOT(edge2, qvec);
   				inv_det = 1.0 / det;
   				MTI->t[iface] *= (float)inv_det;
   				MTI->u[iface] *= (float)inv_det;
   				MTI->v[iface] *= (float)inv_det;      	
					MTI->isHit[iface] = YUP;
					++MTI->N_hits;
					/* store shortest distance triangle info */
					if (MTI->t[iface] < tmin) {
						tmin = MTI->t[iface];
						MTI->ifacemin = iface;
						/* calculate the location of the intersection in XYZ coords */
						MTI->P[0] = vert0[0] + MTI->u[iface] * (vert1[0] - vert0[0] ) + MTI->v[iface] * (vert2[0] - vert0[0] );
						MTI->P[1] = vert0[1] + MTI->u[iface] * (vert1[1] - vert0[1] ) + MTI->v[iface] * (vert2[1] - vert0[1] );
						MTI->P[2] = vert0[2] + MTI->u[iface] * (vert1[2] - vert0[2] ) + MTI->v[iface] * (vert2[2] - vert0[2] );
						/* find out which node is closest to P */
						MTI->inodeminlocal = 0;
						MTI->d = (vert0[0] - MTI->P[0])*(vert0[0] - MTI->P[0]) + (vert0[1] - MTI->P[1])*(vert0[1] - MTI->P[1]) + (vert0[2] - MTI->P[2])*(vert0[2] - MTI->P[2]);
						dii = (vert1[0] - MTI->P[0])*(vert1[0] - MTI->P[0]) + (vert1[1] - MTI->P[1])*(vert1[1] - MTI->P[1]) + (vert1[2] - MTI->P[2])*(vert1[2] - MTI->P[2]);
						if (dii < MTI->d) {
							MTI->d = dii;
							MTI->inodeminlocal = 1;
						}
						dii = (vert2[0] - MTI->P[0])*(vert2[0] - MTI->P[0]) + (vert2[1] - MTI->P[1])*(vert2[1] - MTI->P[1]) + (vert2[2] - MTI->P[2])*(vert2[2] - MTI->P[2]);
						if (dii < MTI->d) {
							MTI->d = dii;
							MTI->inodeminlocal = 2;
						}
						MTI->d = (float)sqrtf((double)MTI->d);
					}
					if (MTI->t[iface] > tmax) {
						tmax = MTI->t[iface];
						MTI->ifacemax = iface;
					}
				}
			}
		}
	#else                    /* the non-culling branch */
   	if (det > -SUMA_MT_EPSILON && det < SUMA_MT_EPSILON)
      	MTI->isHit[iface] = NOPE;
   	else {
			inv_det = 1.0 / det;

   		/* calculate distance from vert0 to ray origin */
   		SUMA_MT_SUB(tvec, orig, vert0);

   		/* calculate U parameter and test bounds */
   		MTI->u[iface] = (float)SUMA_MT_DOT(tvec, pvec) * inv_det;
   		if (MTI->u[iface] < 0.0 || MTI->u[iface] > 1.0)
      		MTI->isHit[iface] = NOPE;
			else {
   			/* prepare to test V parameter */
   			SUMA_MT_CROSS(qvec, tvec, edge1);

   			/* calculate V parameter and test bounds */
   			MTI->v[iface] = (float)SUMA_MT_DOT(dir, qvec) * inv_det;
   			if (MTI->v[iface] < 0.0 || MTI->u[iface] + MTI->v[iface] > 1.0)
      			MTI->isHit[iface] = NOPE;
				else {
   				/* calculate t, ray intersects triangle */
   				MTI->t[iface] = (float)SUMA_MT_DOT(edge2, qvec) * inv_det;      	
					MTI->isHit[iface] = YUP;
					++MTI->N_hits;
					/* store shortest distance triangle info */
					if (MTI->t[iface] < tmin) {
						tmin = MTI->t[iface];
						MTI->ifacemin = iface;
						/* calculate the location of the intersection in XYZ coords */
						MTI->P[0] = vert0[0] + MTI->u[iface] * (vert1[0] - vert0[0] ) + MTI->v[iface] * (vert2[0] - vert0[0] );
						MTI->P[1] = vert0[1] + MTI->u[iface] * (vert1[1] - vert0[1] ) + MTI->v[iface] * (vert2[1] - vert0[1] );
						MTI->P[2] = vert0[2] + MTI->u[iface] * (vert1[2] - vert0[2] ) + MTI->v[iface] * (vert2[2] - vert0[2] );
						/* find out which node is closest to P */
						MTI->inodeminlocal = 0;
						MTI->d = (vert0[0] - MTI->P[0])*(vert0[0] - MTI->P[0]) + (vert0[1] - MTI->P[1])*(vert0[1] - MTI->P[1]) + (vert0[2] - MTI->P[2])*(vert0[2] - MTI->P[2]);
						dii = (vert1[0] - MTI->P[0])*(vert1[0] - MTI->P[0]) + (vert1[1] - MTI->P[1])*(vert1[1] - MTI->P[1]) + (vert1[2] - MTI->P[2])*(vert1[2] - MTI->P[2]);
						if (dii < MTI->d) {
							MTI->d = dii;
							MTI->inodeminlocal = 1;
						}
						dii = (vert2[0] - MTI->P[0])*(vert2[0] - MTI->P[0]) + (vert2[1] - MTI->P[1])*(vert2[1] - MTI->P[1]) + (vert2[2] - MTI->P[2])*(vert2[2] - MTI->P[2]);
						if (dii < MTI->d) {
							MTI->d = dii;
							MTI->inodeminlocal = 2;
						}
						MTI->d = (float)sqrtf((double)MTI->d);
						MTI->inodemin = FaceSetList[iface][MTI->inodeminlocal];
					}
					if (MTI->t[iface] > tmax) {
						tmax = MTI->t[iface];
						MTI->ifacemax = iface;
					}
				}
			}
		}
	#endif
	}/*iface */
	MTI->N_el = N_FaceSet;
	return (MTI);
}

/*!
Show contents of SUMA_MT_INTERSECT_TRIANGLE structure

*/
SUMA_Boolean SUMA_Show_MT_intersect_triangle(SUMA_MT_INTERSECT_TRIANGLE *MTI, FILE *Out)
{
	int MaxShow = 5, i,j;
	
	if (Out == NULL) Out = stdout;
		
	if (MTI == NULL) {
		fprintf (Out, "NULL Surface Object Pointer\n");
		return(NOPE);
	}
	
	fprintf (Out,"\n---------------------------------\n");
	if (!MTI->N_el) {
		fprintf (Out,"Zero elements in structure\n");
		return (YUP);
	}
	
	if (MTI->isHit == NULL) {
		fprintf (SUMA_STDERR,"Error SUMA_Show_MT_intersect_triangle: isHit is NULL\n\n");
		return (NOPE);
	}
	else {
		if (MaxShow > MTI->N_el) MaxShow = MTI->N_el; 
		fprintf (Out, "Intersection results (showing first %d out of %d elements):\n", MaxShow, MTI->N_el);
		for (i=0; i < MaxShow; ++i)	{
			fprintf (Out, "\tisHit: %d t %f u %f v %f", MTI->isHit[i], MTI->t[i], MTI->u[i],MTI->v[i]);
		}
			fprintf (Out, "\n");
		
		if (MTI->N_hits) {
			fprintf (Out, "\n%d hits.\n", MTI->N_hits);
			fprintf (Out, "Minimum Distance: %d t %f u %f v %f\n", \
						MTI->ifacemin, MTI->t[MTI->ifacemin], MTI->u[MTI->ifacemin],MTI->v[MTI->ifacemin]);
			fprintf (Out, "Intersection point P at Minimum Distance FaceSet:\n%f, %f, %f\n", \
						MTI->P[0], MTI->P[1], MTI->P[2]);
			fprintf (Out, "Closest node is number %d in Minimum Distance Faceset (%d in NodeList) at %f distance.\n",\
						MTI->inodeminlocal, MTI->inodemin, MTI->d);									
			fprintf (Out, "Maximum Distance: %d t %f u %f v %f\n\n", \
						MTI->ifacemax, MTI->t[MTI->ifacemax], MTI->u[MTI->ifacemax],MTI->v[MTI->ifacemax]);
			fprintf (Out, "Intersection of ray with surface (showing first %d out of %d elements):\n", MaxShow, MTI->N_el);
			i = 0;
			j = 0;
			while (i< MTI->N_el && j < MTI->N_hits) {
				if (MTI->isHit[i]) {
					++j;
					fprintf (Out, "\tisHit: %d t %f u %f v %f\n", MTI->isHit[i], MTI->t[i], MTI->u[i],MTI->v[i]);
				}
				++i;
			}
			fprintf (Out, "\n");
		} else {
			fprintf (Out, "No Intersection of ray with surface\n");
		}

	}
	return (YUP);
}
/*!
free structure SUMA_MT_INTERSECT_TRIANGLE
*/
SUMA_Boolean SUMA_Free_MT_intersect_triangle(SUMA_MT_INTERSECT_TRIANGLE *MTI)
{
	/*fprintf (SUMA_STDOUT,"Freeing MT_intersect_triangle\n");*/
	if (MTI->t) free(MTI->t);
	if (MTI->u) free(MTI->u);
	if (MTI->v) free(MTI->v);
	if (MTI->isHit) free(MTI->isHit);
	if (MTI) free(MTI);
	return(YUP);
}

/*!
	Code from Tomas Möller, John Hughes 1999:
	Tomas Möller and John F. Hughes. 
	Efficiently building a matrix to rotate one vector to another. 
	Journal of graphics tools, 4(4):1-4, 1999
	
SUMA_Boolean SUMA_FromToRotation (float *v0, float *v1, float **mtx)

determines rotation matrix required to rotate vector from to vector to
\param v0 (float *) 3x1 vector to be rotated into v1 
\param v1 (float *) 3x1 vector 
\param mtx (float *) 4x4 matrix containing 3x3 rotation  matrix in the top left corner

\ret YUP/NOPE	

\sa SUMA_mattoquat  
*/
SUMA_Boolean SUMA_FromToRotation (float *v0, float *v1, float **mtx)
{/* SUMA_FromToRotation */
	char FuncName[100];
	float v[3], vn;
	float e, h, f;

	sprintf(FuncName,"SUMA_FromToRotation");
	
	/*normalize both vectors */
	vn = sqrt(v0[0]*v0[0] + v0[1]*v0[1] + v0[2]*v0[2]);
	if (vn == 0.0) {
		fprintf(SUMA_STDERR,"Error %s: v0 is null.\n",FuncName);
		return (NOPE);
	}
	v0[0] /= vn;
	v0[1] /= vn;
	v0[2] /= vn;

	vn = sqrt(v1[0]*v1[0] + v1[1]*v1[1] + v1[2]*v1[2]);
	if (vn == 0.0) {
		fprintf(SUMA_STDERR,"Error %s: v1 is null.\n",FuncName);
		return (NOPE);
	}
	v1[0] /= vn;
	v1[1] /= vn;
	v1[2] /= vn;

	SUMA_MT_CROSS(v, v0, v1);
	e = SUMA_MT_DOT(v0, v1);
	f = (e < 0)? -e:e;
	if (f > 1.0 - SUMA_MT_EPSILON)     /* "v0" and "v1"-vector almost parallel */
	{
		float u[3], v[3]; /* temporary storage vectors */
		float x[3];       /* vector most nearly orthogonal v1 "v0" */
		float c1, c2, c3; /* coefficients for later use */
		int i, j;

		x[0] = (v0[0] > 0.0)? v0[0] : -v0[0];
		x[1] = (v0[1] > 0.0)? v0[1] : -v0[1];
		x[2] = (v0[2] > 0.0)? v0[2] : -v0[2];

		if (x[0] < x[1]) 
		{
			if (x[0] < x[2]) 
			{
			  x[0] = 1.0; x[1] = x[2] = 0.0;
			}
			else 
			{
			  x[2] = 1.0; x[0] = x[1] = 0.0;
			}
		}
		else 
		{
			if (x[1] < x[2]) 
			{
			  x[1] = 1.0; x[0] = x[2] = 0.0;
			}
			else 
			{
			  x[2] = 1.0; x[0] = x[1] = 0.0;
			}
		}

		u[0] = x[0] - v0[0]; u[1] = x[1] - v0[1]; u[2] = x[2] - v0[2];
		v[0] = x[0] - v1[0];   v[1] = x[1] - v1[1];   v[2] = x[2] - v1[2];

		c1 = 2.0 / SUMA_MT_DOT(u, u);
		c2 = 2.0 / SUMA_MT_DOT(v, v);
		c3 = c1 * c2  * SUMA_MT_DOT(u, v);

		for (i = 0; i < 3; i++) {
			for (j = 0; j < 3; j++) {
			  mtx[i][j] =  - c1 * u[i] * u[j]
               			- c2 * v[i] * v[j]
               			+ c3 * v[i] * u[j];
			}
			mtx[i][i] += 1.0;
		}
	}
	else  /* the most common case, unless "v0"="v1", or "v0"=-"v1" */
	{
		#if 0
			/* unoptimized version - a good compiler will optimize this. */
			h = (1.0 - e)/SUMA_MT_DOT(v, v);
			mtx[0][0] = e + h * v[0] * v[0];  
			mtx[0][1] = h * v[0] * v[1] - v[2]; 
			mtx[0][2] = h * v[0] * v[2] + v[1];

			mtx[1][0] = h * v[0] * v[1] + v[2]; 
			mtx[1][1] = e + h * v[1] * v[1];    
			mtx[1][2] = h * v[1] * v[2] - v[0];

			mtx[2][0] = h * v[0] * v[2] - v[1]; 
			mtx[2][1] = h * v[1] * v[2] + v[0]; 
			mtx[2][2] = e + h * v[2] * v[2];
		#else
			/* ...otherwise use this hand optimized version (9 mults less) */
			float hvx, hvz, hvxy, hvxz, hvyz;
			h = (1.0 - e)/SUMA_MT_DOT(v, v);
			hvx = h * v[0];
			hvz = h * v[2];
			hvxy = hvx * v[1];
			hvxz = hvx * v[2];
			hvyz = hvz * v[1];
			mtx[0][0] = e + hvx * v[0]; 
			mtx[0][1] = hvxy - v[2];     
			mtx[0][2] = hvxz + v[1];

			mtx[1][0] = hvxy + v[2];  
			mtx[1][1] = e + h * v[1] * v[1]; 
			mtx[1][2] = hvyz - v[0];

			mtx[2][0] = hvxz - v[1];  
			mtx[2][1] = hvyz + v[0];     
			mtx[2][2] = e + hvz * v[2];
		#endif
	}	
	
	mtx[0][3] = 0.0;
	mtx[1][3] = 0.0;
	mtx[2][3] = 0.0;
	mtx[3][0] = 0.0;
	mtx[3][1] = 0.0;
	mtx[3][2] = 0.0;
	mtx[3][3] = 1.0;
	return (YUP);
}

/*
From Advanced Animation and Rendering Techniques, by Alan Watt & Mark Watt
Addison & Wesley, 1998, pp 363-364
SUMA_Boolean	SUMA_mattoquat (float **mat, float *q)

transforms a rotation matrix into a quaternion
\param mat (float **) 4x4 rotation matrix 
\param q (float *) 4x1 vector containing the quaternion computed from mat

\ret YUP/NOPE

\sa SUMA_FromToRotation
*/
SUMA_Boolean	SUMA_mattoquat (float **mat, float *q)
{
	double tr, s;
	int i,j,k, nxt[3] = {1, 2, 0};
	char FuncName[100];
	
	sprintf(FuncName,"SUMA_mattoquat");
	
	/* calculate the trace */
	tr = mat[0][0] + mat[1][1] + mat[2][2];
	if (tr > 0.0) {
		s = sqrt(tr + 1.0);
		q[3] = s * 0.5;
		s = 0.5/s;
		
		q[0] = (mat[1][2] - mat[2][1])*s;
		q[1] = (mat[2][0] - mat[0][2])*s;
		q[2] = (mat[0][1] - mat[1][0])*s;
	
	} /* tr > 0.0 */ else {
		i = 0;
		if (mat[1][1] > mat[0][0]) i = 1;
		if (mat[2][2] > mat[i][i]) i = 2;
		j = nxt[i]; k = nxt[j];
	
		s = sqrt( (mat[i][i] - (mat[j][j]+mat[k][k])) + 1.0);
		q[i] = s * 0.5;
		s = 0.5/s;
		q[3] = (mat[j][k] - mat[k][j])*s;
		q[j] = (mat[i][j] + mat[j][i])*s;
		q[k] = (mat[i][k] + mat[k][i])*s;
	} /* tr < 0.0 */
	return (YUP);
}

/*------------------------- Triangle Consistency Functions BEGIN --------------------------------------- */
typedef enum {SUMA_NO_NEIGHB, SUMA_NO_MORE_TO_VISIT, SUMA_VISITED_ALL, SUMA_BAD_SEED} SUMA_TAKE_A_HIKE;

/*!
	This function compares the winding of two triangles, determines their consistency
	and corrects it.
	
	\param T (int *) a b c nodes forming the reference triangle 
	\param t (int *) d c b (or whatever combination you choose, c b d for example)
	\ret 1: Consistent
		 -1: Inconsisten
		  0: less than 2 nodes shared
*/
int SUMA_isConsistent (int *T, int *t)
{
	static char FuncName[]={""};
	static int ic, in, LOC[2], loc[2], d, D;
	
	ic = 0;	/* common node index*/
	in = 0; /* number of node searched in T */
	while (ic < 2 && in < 3) {
		if (t[0] == T[in]) {
			LOC[ic] = in; /* location of icth node in 1st triangle */
			loc[ic] = 0; /* location of icth node in 2nt triangle */
			++ic;
		}else {
			if (t[1] == T[in]) {
				LOC[ic] = in; /* location of icth node in 1st triangle */
				loc[ic] = 1; /* location of icth node in 2nt triangle */
				++ic;
			}else {
				if (t[2] == T[in]) {
					LOC[ic] = in; /* location of icth node in 1st triangle */
					loc[ic] = 2; /* location of icth node in 2nt triangle */
					++ic;
				}
			}
		}
		++in; /* look for next node */
	}
	if (ic != 2) {
		fprintf(SUMA_STDERR,"Error %s: Triangles do not share 2 nodes.\n", FuncName);
		return (0);
	}
	
	D = (LOC[1]-LOC[0]);
	d = (loc[1]-loc[0]);
	/*fprintf(SUMA_STDERR,"%s: T[%d, %d, %d], t[%d, %d, %d]\n", FuncName, T[0], T[1], T[2], t[0], t[1], t[2]);
	fprintf(SUMA_STDERR,"%s: LOC[0,1]=[%d, %d], loc[0,1] = [%d, %d]\n", FuncName, LOC[0], LOC[1], loc[0], loc[1]);
	fprintf(SUMA_STDERR,"%s: D = %d, d = %d\n", FuncName, D, d);*/	
	if (d > 1 || d < -1) d = - d /2 ;
	if (D > 1 || D < -1) D = - D /2 ;
	/*fprintf(SUMA_STDERR,"%s: D = %d, d = %d\n", FuncName, D, d);*/	

	if (d != D) {
		/*fprintf(SUMA_STDERR,"%s: Triangles consistent.\n", FuncName);*/
		return (1);
	}
	
		
	/*fprintf(SUMA_STDERR,"%s: Triangles NOT consistent.\n", FuncName);*/
	in = t[0];
	t[0] = t[2];
	t[2] = in;
	return (-1);
}

#ifdef SUMA_isConsistent_STANDALONE
/*!
test for SUMA_isConsistent
gcc -Wall -Wno-unused-variable -o SUMA_isConsistent SUMA_MiscFunc.c SUMA_lib.a libmri.a -I/usr/X11R6/include -I./ -L/usr/lib -L/usr/X11R6/lib -lm -lGL -lGLU -lGLw -lXmu -lXm -lXt -lXext -lX11 -lMesaGLw -lMesaGLwM -DSUMA_isConsistent_STANDALONE
*/
int main (int argc,char *argv[])
{/* Main */
	int Ta[6][3] ={{1, 2, 3}, /* cCW */
						{2, 3, 1}, /* cCW */
						{3, 1, 2}, /* cCW */
						{1, 3, 2}, /* CW */ 
						{3, 2, 1}, /* CW */
						{2, 1, 3}, /* CW */ };
	int ta[6][3] ={{4, 3, 2}, /* cCW */
						{3, 2, 4}, /* cCW */
						{2, 4, 3}, /* cCW */
						{2, 3, 4}, /* CW */ 
						{3, 4, 2}, /* CW */
						{4, 2, 3}, /* CW */ };	
	
	int T[3], t[3], i, j;
	int tc[3];
	
	/* auto test */
	/* consitent tests */
	printf ("ALL THESE SHOULD BE CONSISTENT:\n");	
	for (i=0; i < 3; ++i) {
		for (j=0; j < 3; ++ j) {
			printf ("\tT=[%d %d %d]\n", Ta[i][0], Ta[i][1], Ta[i][2]);
			printf ("\tt=[%d %d %d]\n", ta[j][0], ta[j][1], ta[j][2]);
			tc[0]=ta[j][0]; /* save ta in its original form for subsequent tests to work! */
			tc[1]=ta[j][1];
			tc[2]=ta[j][2];
			if (SUMA_isConsistent(Ta[i], tc) > 0) {
				printf ("\ttriangles consistent.\n");
			}else {
				printf ("\ttriangles inconsistent.Fixed: %d %d %d\n", tc[0], tc[1], tc[2]);
			}
		}
	}	
	for (i=3; i < 6; ++i) {
		for (j=3; j < 6; ++ j) {
			printf ("\tT=[%d %d %d]\n", Ta[i][0], Ta[i][1], Ta[i][2]);
			printf ("\tt=[%d %d %d]\n", ta[j][0], ta[j][1], ta[j][2]);
			tc[0]=ta[j][0]; /* save ta in its original form for subsequent tests to work! */
			tc[1]=ta[j][1];
			tc[2]=ta[j][2];
			if (SUMA_isConsistent(Ta[i], tc) > 0) {
				printf ("\ttriangles consistent.\n");
			}else {
				printf ("\ttriangles inconsistent.Fixed: %d %d %d\n", tc[0], tc[1], tc[2]);
			}
		}
	}	
	
	
	/* NONT consitent tests */
	printf ("ALL THESE SHOULD BE NOT CONSISTENT:\n");	
	for (i=3; i < 6; ++i) {
		for (j=0; j < 3; ++ j) {
			printf ("\tT=[%d %d %d]\n", Ta[i][0], Ta[i][1], Ta[i][2]);
			printf ("\tt=[%d %d %d]\n", ta[j][0], ta[j][1], ta[j][2]);
			tc[0]=ta[j][0]; /* save ta in its original form for subsequent tests to work! */
			tc[1]=ta[j][1];
			tc[2]=ta[j][2];
			if (SUMA_isConsistent(Ta[i], tc) > 0) {
				printf ("\ttriangles consistent.\n");
			}else {
				printf ("\ttriangles inconsistent.Fixed: %d %d %d\n", tc[0], tc[1], tc[2]);
			}
		}
	}	
	for (i=0; i < 3; ++i) {
		for (j=3; j < 6; ++ j) {
			printf ("\tT=[%d %d %d]\n", Ta[i][0], Ta[i][1], Ta[i][2]);
			printf ("\tt=[%d %d %d]\n", ta[j][0], ta[j][1], ta[j][2]);
			tc[0]=ta[j][0]; /* save ta in its original form for subsequent tests to work! */
			tc[1]=ta[j][1];
			tc[2]=ta[j][2];
			if (SUMA_isConsistent(Ta[i], tc) > 0) {
				printf ("\ttriangles consistent.\n");
			}else {
				printf ("\ttriangles inconsistent.Fixed: %d %d %d\n", tc[0], tc[1], tc[2]);
			}
		}
	}	

	/* manual test */									
	printf ("Enter triangle 1:\n");
	scanf ("%d %d %d", &T[0], &T[1], &T[2]);
	printf ("Enter triangle 2:\n");
	scanf ("%d %d %d", &t[0], &t[1], &t[2]);

	if (SUMA_isConsistent(T, t) > 0) {
		printf ("triangles consistent.\n");
	}else {
		printf ("triangles inconsistent.Fixed: %d %d %d\n", t[0], t[1], t[2]);
	}
	return (0);

}/* Main */

#endif

/*!
	This function returns the best available seed, for use in SUMA_Take_A_Hike
	The first time you call the function, the seed is a triangle with three
	neighbors. The next time you call, the seed is a triangle that was visited by 
	SUMA_Take_A_Hike and has preferably two neighbors left unvisited. 

*/
int SUMA_Next_Best_Seed (SUMA_FACESET_FIRST_EDGE_NEIGHB *SFFN, int * visited, int N_FL)
{
	static int entry = 0, seed=-1;
	int Found1 = -1, Found2 = -1, i, N_NotVisNeighb, itry;
	
	if (!entry) { /* entry = 0 */
		for (i=0; i < N_FL; ++i) {
			if (SFFN->N_Neighb[i] == 3) {
				seed = i; ++entry; return(seed);
			}
			if (SFFN->N_Neighb[i] == 2) Found2 = i;
			if (SFFN->N_Neighb[i] == 1) Found1 = i;
		}	
				
		if (Found2 > 0) {
			++entry;
			return (Found2);
		}
			
		if (Found1 > 0) {
			++entry;
			return (Found1);
		}
		
		return (-1); /* No seeds found */		
	}/* entry = 0 */
	else {/* entry > 0 */
		for (i=0; i < N_FL; ++i) {
			if (visited[i]) { /* a candidate */
				/* count the number of unvisited neighbors */
				N_NotVisNeighb = 0;
				itry = 0;
				while (itry < SFFN->N_Neighb[i]) {
					if (!visited[SFFN->FirstNeighb[i][itry]]) ++N_NotVisNeighb;
					++itry;
				}
				if (N_NotVisNeighb == 2) {
					seed = i; ++entry; return (seed);
				}
				if (N_NotVisNeighb == 1) {
					Found1 = i;
				}
			} /* a candidate */
		}
		if (Found1 > 0) {
			++entry;
			return (Found1);
		}
		return (-1); /* No seeds found */	
	}/* entry > 0 */
}

/*!
	This function starts at one triangle and proceeds from edge to edge without going through the same triangle twice.
	The very first time you call the function, the seed is at a triangle that has not been visited yet.
	The next calls require that the seed triangle is a visited one.
	It is very unlikely to visit all triangles with one seed. So, the function needs to be called multiple
	times with different seeds and because of that, it is very slow. 
	When a triangle is visited, it's winding order is compared the the precursor triangle. If need be, the order is 
	corrected.
	
	\sa SUMA_Next_Best_Seed
	\sa SUMA_isConsistent 
*/
SUMA_TAKE_A_HIKE SUMA_Take_A_Hike (SUMA_FACESET_FIRST_EDGE_NEIGHB *SFFN, int *visited, int *N_visited, int *Consistent, int **FL, int N_FL, int seed)
{
	static char FuncName[]={"SUMA_Take_A_Hike"};
	int NotFound, itry, curface, nxtface;
	static int entry=0;

	curface = seed;
	if (!visited[curface]) { /* a new visit this should only happen on the first call */
		if (!entry) {
			*N_visited += 1;
			visited[curface] = 1;
			Consistent[curface] = 1;
			/*fprintf (SUMA_STDERR, "%s: visited %d\n", FuncName, curface);*/
		}else {
			fprintf (SUMA_STDERR, "Error %s: You should not send unvisited seeds, except at the very first call.\n", FuncName);
			return (SUMA_BAD_SEED);
		}
	}
	if (SFFN->N_Neighb[curface] == 0) {
		return (SUMA_NO_NEIGHB);
	}
	++entry;

	while (*N_visited <= N_FL) {
		/* try the neighbors */
		itry = 0; /* index into neighbors */
		NotFound = 1; /* now new unvisited neighbor found */
		do {
			nxtface = SFFN->FirstNeighb[curface][itry];
			if (visited[nxtface]) {
				/* already visited try another neighbor */
				++itry;
			}else {
				if (SFFN->N_Neighb[nxtface] == 1) {
					/* that's a loner, do it and continue with the neighbors */
					/*fprintf (SUMA_STDERR, "%s: LONER!\n", FuncName);*/
					visited[nxtface] = 1;
					Consistent[nxtface] = SUMA_isConsistent (FL[curface], FL[nxtface]);
					/*fprintf (SUMA_STDERR, "%s: visited %d\n", FuncName, nxtface);*/
					*N_visited = *N_visited+1;
					++itry;
				} else {
					/* that's a good one to follow */
					Consistent[nxtface] = SUMA_isConsistent (FL[curface], FL[nxtface]);
					visited[nxtface] = 1;
					curface = nxtface;
					/*fprintf (SUMA_STDERR, "%s: visited %d\n", FuncName, curface);*/
					*N_visited = *N_visited+1;
					NotFound = 0;
					itry = 100; 
				}
			}
		} while (itry < SFFN->N_Neighb[curface]) ;

		if (NotFound) { /* no more useful neighbors on this walk, get outa here */
			/*fprintf (SUMA_STDERR, "%s:  N_visited = %d, N_tot = %d\n", FuncName, *N_visited, N_FL);*/
			return (SUMA_NO_MORE_TO_VISIT);
		}

	}
	
	return (SUMA_VISITED_ALL);
}

/*!
	SUMA_free_Edge_List (SEL)
	\param SEL (SUMA_EDGE_LIST *)
	
*/
void SUMA_free_Edge_List (SUMA_EDGE_LIST *SEL)
{
	
	if (SEL->EL) SUMA_free2D((char **)SEL->EL, SEL->N_EL);
	if (SEL->ELloc) free(SEL->ELloc);
	if (SEL->ELps) SUMA_free2D((char **)SEL->ELps, SEL->N_EL);
	if (SEL->Tri_limb) SUMA_free2D((char **)SEL->Tri_limb, SEL->N_EL/3);
	if (SEL) free (SEL);
	return;
}

/*! 
	ans = SUMA_Make_Edge_List (FL, N_FL, N_Node);
	
	This function creates a list of all the edges making up the FaceSets
	\param FL (int **) FaceSetList matrix ( N_FL x 3)
	\param N_FL (int) number of facesets (triangles) in FL
	\param N_Node (int) number of nodes forming the mesh
	
	\ret ans (SUMA_EDGE_LIST *) NULL/failure or the following fields
	 	EL (int **) sorted edge list
		 ELps (int **) edge list properties
		N_EL 	(int)  Number of edges
		see SUMA_define.h for more info
		
	to free 	ans, use SUMA_free_Edge_List 					
*/
SUMA_EDGE_LIST * SUMA_Make_Edge_List (int **FL, int N_FL, int N_Node)
{
	static char FuncName[]={"SUMA_Make_Edge_List"};
	int i, ie, *isort_EL, **ELp, lu, ht, *iTri_limb, icur;
	SUMA_EDGE_LIST *SEL;
	

	/* allocate and form the List of edges */
	SEL = (SUMA_EDGE_LIST *) malloc(sizeof(SUMA_EDGE_LIST));

	SEL->N_EL = 3 * N_FL;
	SEL->EL = (int **) SUMA_allocate2D (SEL->N_EL, 2, sizeof(int)); /* edge list */
	SEL->ELloc = (int *)calloc(N_Node, sizeof(int));
	
	ELp = (int **) SUMA_allocate2D (SEL->N_EL, 2, sizeof(int)); /* edge property list */
																			/* 1st column, 1 = is flipped from orientation in triangle, -1 as present in triangle 
																				  2nd column, index of triangle (FaceSet) that edge is a part of */ 	
	SEL->ELps = (int **) SUMA_allocate2D (SEL->N_EL, 3, sizeof(int)); /*sorted edge property list */
	
	/*fprintf(SUMA_STDERR, "%s: SEL->NEL %d\n", FuncName, SEL->N_EL/3);*/
	
	SEL->Tri_limb = (int **) SUMA_allocate2D (SEL->N_EL/3, 3, sizeof(int)); 
	iTri_limb = (int *)calloc (SEL->N_EL/3,sizeof(int)); 
	
	if (SEL == NULL || SEL->EL == NULL || ELp == NULL || SEL->ELps == NULL || SEL->Tri_limb == NULL || iTri_limb== NULL || SEL->ELloc == NULL) {
		fprintf(SUMA_STDERR, "Error %s: Failed to allocate for EL, ELp.\n", FuncName);
		return (NULL);
	}

	/* form the edge list */
	/*fprintf(SUMA_STDERR, "%s: Forming edge list ...\n", FuncName);*/
	for (i=0; i< N_FL; ++i) {/* begin, form edge list */
		/* first edge, 0->1*/
		ie = 3*i;
		if (FL[i][0] > FL[i][1]) {
			/* flip it, to make sorting easier */
			SEL->EL[ie][0] = FL[i][1];
			SEL->EL[ie][1] = FL[i][0];
			/* store parameters */
			ELp[ie][0] = 1; /* flip happened */
		} else { 
			/* no flip necessary */
			SEL->EL[ie][0] = FL[i][0];
			SEL->EL[ie][1] = FL[i][1];
			ELp[ie][0] = -1; /* NO flip happened */
		}
		ELp[ie][1] = i; /* FaceSetMember */
		
		/* second edge, 1->2*/
		ie += 1;
		if (FL[i][1] > FL[i][2]) {
			/* flip it, to make sorting easier */
			SEL->EL[ie][0] = FL[i][2];
			SEL->EL[ie][1] = FL[i][1];
			/* store parameters */
			ELp[ie][0] = 1; /* flip happened */
		} else { 
			/* no flip necessary */
			SEL->EL[ie][0] = FL[i][1];
			SEL->EL[ie][1] = FL[i][2];
			ELp[ie][0] = -1; /* NO flip happened */
		}
		ELp[ie][1] = i; /* FaceSetMember */
		
		/* third edge, 2->0*/
		ie += 1;
		if (FL[i][2] > FL[i][0]) {
			/* flip it, to make sorting easier */
			SEL->EL[ie][0] = FL[i][0];
			SEL->EL[ie][1] = FL[i][2];
			/* store parameters */
			ELp[ie][0] = 1; /* flip happened */
		} else { 
			/* no flip necessary */
			SEL->EL[ie][0] = FL[i][2];
			SEL->EL[ie][1] = FL[i][0];
			ELp[ie][0] = -1; /* NO flip happened */
		}
		ELp[ie][1] = i; /* FaceSetMember */
		
	}/* end, form edge list */
	/*fprintf(SUMA_STDERR,"%s: Edge list done.\n", FuncName);*/
	
	#if 0
		fprintf(SUMA_STDERR,"%s: Node1 Node2 | FlipVal Triangle\n", FuncName); 
		for (i=0; i < SEL->N_EL; ++i) {
			fprintf (SUMA_STDERR, "%d %d | %d %d\n", SEL->EL[i][0], SEL->EL[i][1], ELp[i][0], ELp[i][1]);
		}
	#endif

	/* now sort the Edge list */
	/*fprintf(SUMA_STDERR,"%s: Sorting edge list...\n", FuncName);*/
	isort_EL = SUMA_dqsortrow (SEL->EL, SEL->N_EL, 2);
	
	/* reorder ELp to match sorted EL */
	for (i=0; i< SEL->N_EL; ++i) {
		SEL->ELps[i][0] = ELp[isort_EL[i]][0];
		SEL->ELps[i][1] = ELp[isort_EL[i]][1];
	}
	
	/*fprintf(SUMA_STDERR,"%s: Sorting edge list done.\n", FuncName);*/
	
	if (isort_EL) free(isort_EL);
	isort_EL = NULL;
	
	
	#if 0
		fprintf(SUMA_STDERR,"%s: Node1 Node2 | FlipVal Triangle\n", FuncName); 
		for (i=0; i < SEL->N_EL; ++i) {
			fprintf (SUMA_STDERR, "%d %d | %d %d\n", SEL->EL[i][0], SEL->EL[i][1], SEL->ELps[i][0], SEL->ELps[i][1]);
		}
	#endif
	

	/* free unsorted ELp */
	if (ELp) SUMA_free2D((char **)ELp, SEL->N_EL);
	ELp = NULL;
	
	SEL->max_N_Hosts = -1;
	SEL->min_N_Hosts = 100;
	/* do a search for some funky stuff */
	i=0;
	while (i < SEL->N_EL) {
		/* store the location of this edge for the triangle hosting it */
		ht = SEL->ELps[i][1]; /* host triangle index */
		SEL->Tri_limb[ht][iTri_limb[ht]] = i;
		iTri_limb[ht] += 1;
		
		SEL->ELps[i][2] = 1; /* number of triangles hosting edge */
		lu = 1; 
		while (i+lu < SEL->N_EL) {
			if (SEL->EL[i+lu][0] == SEL->EL[i][0] && SEL->EL[i+lu][1] == SEL->EL[i][1]) {/* found matching edge */
				SEL->ELps[i][2] += 1; /* number of triangles hosting edge */
				SEL->ELps[i+lu][2] = -1; /* flag to mean that this edge is a duplicte in the list */

				/* store the location of this edge for the triangle hosting it */
				ht = SEL->ELps[i+lu][1]; /* host triangle index */
				SEL->Tri_limb[ht][iTri_limb[ht]] = i+lu;
				iTri_limb[ht] += 1;

				++lu;
			}else break;
			
		}
		if (SEL->max_N_Hosts < SEL->ELps[i][2]) SEL->max_N_Hosts = SEL->ELps[i][2];
		if (SEL->min_N_Hosts > SEL->ELps[i][2]) SEL->min_N_Hosts = SEL->ELps[i][2]; 
		i += lu;
	}
	
	fprintf(SUMA_STDERR,"%s: Min/Max number of hosting triangles: [%d/%d] \n", FuncName, SEL->min_N_Hosts, SEL->max_N_Hosts);
	
	#if 0
		fprintf(SUMA_STDERR,"%s:(ELindex) Node1 Node2 | FlipVal Triangle N_hosts\n", FuncName); 
		for (i=0; i < SEL->N_EL; ++i) {
			fprintf (SUMA_STDERR, "(%d) %d %d | %d %d %d\n", i, SEL->EL[i][0], SEL->EL[i][1], SEL->ELps[i][0], SEL->ELps[i][1], SEL->ELps[i][2]);
		}
		fprintf(SUMA_STDERR,"%s:Tri_limb\n", FuncName); 
		for (i=0; i < SEL->N_EL/3; ++i) {
			fprintf (SUMA_STDERR, "%d %d %d\n", SEL->Tri_limb[i][0], SEL->Tri_limb[i][1], SEL->Tri_limb[i][2]);
		}
	#endif
	
	/* store where each node's listing begins
	ELloc is used to quickly find a certain edge in EL
	to find the edge formed by nodes na-nb
	find the minimum of na and nb (say it's nb)
	the first reference of an edge containing nb starts at EL(ELloc(nb),:)
	NOTE: ELloc contains an entry for each node in FaceSetList, except the largest node index since that's never in the 
	first column of EL */
	/* fprintf(SUMA_STDERR, "%s: storing locations ...\n", FuncName); */
	for (i=0; i < N_Node; ++i) SEL->ELloc[i] = -1;
	i = 0;
	icur = SEL->EL[0][0];
	SEL->ELloc[icur] = i; 
	while (i < SEL->N_EL) {
		if (SEL->EL[i][0] != icur) {
			icur = SEL->EL[i][0];
			SEL->ELloc[icur] = i;
		}
		++i;
	}

	return (SEL);
}

/*! finds triangles incident to an edge 

	ans = SUMA_Get_Incident( n1,  n2,  SEL, Incident, N_Incident)
	\param n1 (int) node 1
	\param n2 (int) node 2
	\param SEL (SUMA_EDGE_LIST *) Edge List structure
	\param Incident (int *) a pre-allocated vector where incident triangle indices will be stored. MAKE SURE you allocate enough
	\param N_Incident (int *) pointer where the number of incident triangles is stored
	
	\ret ans (SUMA_Boolean) YUP/NOPE
	
	\sa SUMA_Make_Edge_List
*/
SUMA_Boolean SUMA_Get_Incident(int n1, int n2, SUMA_EDGE_LIST *SEL, int *Incident, int *N_Incident)
{
	static char FuncName[] = {"SUMA_Get_Incident"};
	int nt, in1, iseek, m_N_EL;
	
	/*fprintf(SUMA_STDERR,"Entering %s: n1,n2 =%d,%d ...", FuncName,n1,n2);*/
	if (n1 > n2) {
		/*make the first node be the smallest */
		nt = n1;
		n1 = n2;
		n2 = nt;
	}
	
	/* find the location of the first edge with n1 */
	in1 = SEL->ELloc[n1];
	iseek = in1;
	m_N_EL = SEL->N_EL -1;
	*N_Incident = 0;
	while (SEL->EL[iseek][0] == n1) {
		if (SEL->EL[iseek][1] == n2) {
			Incident[*N_Incident] = SEL->ELps[iseek][1]; /* store the faceset index containing the edge */
			*N_Incident = *N_Incident + 1;
		}
		++iseek;
		if (iseek > m_N_EL) {
			if (!*N_Incident) fprintf(SUMA_STDERR,"Warning %s: No Incident FaceSets found!\n", FuncName);
			return (YUP);
		}
		
	}
	if (!*N_Incident) fprintf(SUMA_STDERR,"Warning %s: No Incident FaceSets found!\n", FuncName);
	/*fprintf(SUMA_STDERR,"Leaving %s.\n", FuncName);*/
	return(YUP);	
}

/*! 
	frees the dyamically allocated pointer of the type SUMA_FACESET_FIRST_EDGE_NEIGHB 
	SUMA_free_FaceSet_Edge_Neighb (S)
	\param S (SUMA_FACESET_FIRST_EDGE_NEIGHB *)
	
	\sa SUMA_allocate_FaceSet_Edge_Neighb
*/ 
void SUMA_free_FaceSet_Edge_Neighb (SUMA_FACESET_FIRST_EDGE_NEIGHB * S)
{
	if (S->FirstNeighb) SUMA_free2D((char **)S->FirstNeighb, S->N_FaceSet);
	if (S->N_Neighb) free(S->N_Neighb);
	if (S) free (S);
	return;
}

/*! Allocate space for SUMA_FACESET_FIRST_EDGE_NEIGHB *
	S = SUMA_allocate_FaceSet_Edge_Neighb (N_FaceSet);
	\param N_FaceSet (int) Number of FaceSets to be searched 
	\ret S (SUMA_FACESET_FIRST_EDGE_NEIGHB *)
	\sa SUMA_free_FaceSet_Edge_Neighb 
*/

SUMA_FACESET_FIRST_EDGE_NEIGHB *SUMA_allocate_FaceSet_Edge_Neighb (int N_FaceSet)
{
	static char FuncName[]={"SUMA_FACESET_FIRST_EDGE_NEIGHB"};
	
	SUMA_FACESET_FIRST_EDGE_NEIGHB *SFFN;
	
	SFFN = malloc(sizeof(SUMA_FACESET_FIRST_EDGE_NEIGHB));
	if (SFFN == NULL) {
		fprintf (SUMA_STDERR, "Error %s: Could not allocate for SFFN.\n", FuncName);
		return (NULL);
	}
	
	SFFN->FirstNeighb = (int **) SUMA_allocate2D(N_FaceSet, SUMA_MAX_FACESET_EDGE_NEIGHB, sizeof(int));
	SFFN->N_Neighb = (int *) calloc (N_FaceSet, sizeof(int));
	if (SFFN->FirstNeighb == NULL || SFFN->N_Neighb == NULL) {
		fprintf (SUMA_STDERR, "Error %s: Could not allocate for FirstNeighb or N_Neighb.\n", FuncName);
		return (NULL);
	} 
	
	SFFN->N_Neighb_max = -1; /* ridiculously low */
	SFFN->N_FaceSet = N_FaceSet;
	SFFN->N_Neighb_min = 100; /* ridiculously high */
	return (SFFN);
}

/*!
	returns the FaceSet neighbor list.
	Neighboring triangles share one edge 
	SFEN = SUMA_FaceSet_Edge_Neighb (EL, ELp, N_EL);
	
	\param EL (int **) sorted edge list, output of SUMA_Make_Edge_List
	\param ELps (int **) accompanying properties matrix, output of SUMA_Make_Edge_List
	\param N_EL (int) number of edges. IT IS ASSUMED THAT N_FACES = N_EL/3
	\ret SFEN (SUMA_FACESET_FIRST_EDGE_NEIGHB *) structure containing the neighbor list
		see its typedef in SUMA_define.h for more info
		To free this pointer, make sure you use SUMA_free_FaceSet_Edge_Neighb first 
*/
SUMA_FACESET_FIRST_EDGE_NEIGHB *SUMA_FaceSet_Edge_Neighb (int **EL, int **ELps, int N_EL)
{	
	static char FuncName[]={"SUMA_FaceSet_Edge_Neighb"};
	int i, i1, F0, F1, in0, in1;
	SUMA_FACESET_FIRST_EDGE_NEIGHB *SFFN;
	
	fprintf (SUMA_STDERR, "%s: Doing FaceSet neighbors....\n", FuncName);	
	
	SFFN = SUMA_allocate_FaceSet_Edge_Neighb(N_EL/3);
	if (SFFN == NULL) {
		fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_allocate_FaceSet_Edge_Neighb.\n", FuncName);
		return (NULL);
	}
	
	i = 0;
	while (i < N_EL-1) {
		i1 = i + 1;
		if (EL[i][0] != EL[i1][0] || EL[i][1] != EL[i1][1]) {
			/* edge is part of one triangle only, a cut edge */
			i += 1; /* move to next edge */
		} else {
			F0 = ELps[i][1]; F1 = ELps[i1][1];
			in0 = SFFN->N_Neighb[F0]; in1 = SFFN->N_Neighb[F1];
			if (in0 > SUMA_MAX_FACESET_EDGE_NEIGHB -1 || in1 > SUMA_MAX_FACESET_EDGE_NEIGHB -1) {
				fprintf (SUMA_STDERR, "Error %s: A faceset has more than three neighbors. Bad surface or non triangular mesh\n", FuncName);
				return (NULL);
			}
			SFFN->FirstNeighb[F0][in0] = F1; 
			SFFN->FirstNeighb[F1][in1] = F0;
			SFFN->N_Neighb[F0] ++;
			SFFN->N_Neighb[F1] ++;
			if (SFFN->N_Neighb[F0] > SFFN->N_Neighb_max) {
				SFFN->N_Neighb_max = SFFN->N_Neighb[F0];
			}
			if (SFFN->N_Neighb[F1] > SFFN->N_Neighb_max) {
				SFFN->N_Neighb_max = SFFN->N_Neighb[F1];
			}
			if (SFFN->N_Neighb[F0] < SFFN->N_Neighb_min) {
				SFFN->N_Neighb_min = SFFN->N_Neighb[F0];
			}
			if (SFFN->N_Neighb[F1] < SFFN->N_Neighb_min) {
				SFFN->N_Neighb_min = SFFN->N_Neighb[F1];
			}
			
			i += 2;
		}
	
	}
	
	fprintf (SUMA_STDERR, "%s: Done with FaceSet neighbors.\nN_Neighb_max = %d, N_Neighb_min = %d.\n", FuncName, SFFN->N_Neighb_max, SFFN->N_Neighb_min);
	#if 0
	for (i=0; i< N_FL; ++i) {
		fprintf (SUMA_STDERR, "%s: Tri %d, %d neighbs = ", FuncName, i, SFFN->N_Neighb[i]);
		for (i1=0; i1<SFFN->N_Neighb[i]; ++i1) {
			fprintf (SUMA_STDERR, "%d, ", SFFN->FirstNeighb[i][i1]); 
		}
		fprintf (SUMA_STDERR, "\n");
	}
	#endif
	
	return (SFFN);
}	

/*!
	Makes sure the triangles in FaceSetList are of a consistent orientation.
	
	ans = SUMA_MakeConsistent (FaceSetList, N_FaceSet, SEL) 
	
	\param FaceSetList (int **) N_FaceSet x 3 matrix containing triangle definition
	\param N_FaceSet int
	\param SEL (SUMA_EDGE_LIST *) pointer Edgelist structure as output by SUMA_Make_Edge_List
	
	\ret ans (SUMA_Boolean) YUP, NOPE 
	
	\sa SUMA_Make_Edge_List
	  
*/
SUMA_Boolean SUMA_MakeConsistent (int **FL, int N_FL, SUMA_EDGE_LIST *SEL) 
{
	/* see for more documentation labbook NIH-2 test mesh  p61 */
	int i, it,  N_flip=0, *isflip, *ischecked, ht0, ht1, NotConsistent, miss, miss_cur, N_iter, EdgeSeed, TriSeed, N_checked;
	static char FuncName[]={"SUMA_MakeConsistent"};
	SUMA_FACESET_FIRST_EDGE_NEIGHB *SFFN;
	
	isflip = (int *)calloc(SEL->N_EL/3, sizeof(int));
	ischecked = (int *)calloc(SEL->N_EL/3, sizeof(int));
	
	if (isflip == NULL || ischecked == NULL ) {
		fprintf(SUMA_STDERR, "Error %s: Failed to allocate for isflip\n", FuncName);
		return (NOPE);
	}
	
	
	/* Now, go through the sorted edge list and flip what needs flipping*/
	N_iter = 0;
	miss = 0;
	miss_cur = SEL->N_EL;
	N_checked = 1;
	while (miss_cur != miss) {
		miss_cur = miss;
		miss = 0;
		
		/* both methods work just fine here */
		#if 0
			/*start with the first edge as an edge seed */
			EdgeSeed = 0;
			i=EdgeSeed;
			ht0 = SEL->ELps[i][1];
		#else		
			/* start with the first edge of the seed triangle as an edge seed */
			TriSeed = 0;
			i = SEL->Tri_limb[TriSeed][0];
			ht0 = TriSeed;
		#endif
		
		ischecked[ht0] = 1;
		while (i < SEL->N_EL) {
			ht0 = SEL->ELps[i][1];
			/* make sure edge is not part of three triangles, if it is, skip it */
			if (SEL->ELps[i][2] > 3) {
				++i;
				fprintf(SUMA_STDERR, "%s: Bad edge (#%d: %d--%d), part of more than 2 triangles, skip it\n", FuncName, i, SEL->EL[i][0], SEL->EL[i][1]); 
				continue;
			}
			if (SEL->ELps[i][2] == 2) {
				/* that's a good edge, see if the next edge after it is consistent */
				NotConsistent = SEL->ELps[i][0] * SEL->ELps[i+1][0]; /* if 1 then edges were either both flipped or not flipped in the list */
				ht1 = SEL->ELps[i+1][1];
				if (ischecked[ht0] && !ischecked[ht1]) {
					if (NotConsistent == 0) {
						fprintf(SUMA_STDERR, "Error %s: NotConsistent = 0 here. This should not be.\n", FuncName);
						return (NOPE);
					}
					if (NotConsistent < 0) {
						/* triangles hosting these edges are consistent */
						/* next triangle needs no flipping */
						ischecked[ht1] = 1;
						++N_checked;
					} else {
						/* triangles hosting these edges are NOT consistent */
						/* flip the next triangle */
						it = FL[ht1][0];
						FL[ht1][0] = FL[ht1][2];
						FL[ht1][2] = it;
						/* Now make sure the flip is reflected in ELps */
						it = SEL->Tri_limb[ht1][0]; SEL->ELps[it][0] *= -1;
						it = SEL->Tri_limb[ht1][1]; SEL->ELps[it][0] *= -1;
						it = SEL->Tri_limb[ht1][2]; SEL->ELps[it][0] *= -1;
						N_flip += 1;
						isflip[ht1] = 1;
						ischecked[ht1] = 1;
						++N_checked;
					}
					++i; 
					continue;
				} 
				
				/*try if next edge's host is in good shape */
				if (ischecked [ht1] && !ischecked[ht0]) {
					if (NotConsistent == 0) {
						fprintf(SUMA_STDERR, "Error %s: NotConsistent = 0 here. This should not be.\n", FuncName);
						return (NOPE);
					}
					if (NotConsistent < 0) {
						/* triangles hosting these edges are consistent */
						/* 1st triangle needs no flipping */
						ischecked[ht0] = 1;
						++N_checked;
					} else {
						/* triangles hosting these edges are NOT consistent */
						/* flip the 1st triangle */
						it = FL[ht0][0];
						FL[ht0][0] = FL[ht0][2];
						FL[ht0][2] = it;
						/* Now make sure the flip is reflected in ELps */
						it = SEL->Tri_limb[ht0][0]; SEL->ELps[it][0] *= -1;
						it = SEL->Tri_limb[ht0][1]; SEL->ELps[it][0] *= -1;
						it = SEL->Tri_limb[ht0][2]; SEL->ELps[it][0] *= -1;
						N_flip += 1;
						isflip[ht0] = 1;
						ischecked[ht0] = 1;
						++N_checked;
					}
					++i; 
					continue;
				} 
				if (!ischecked[ht0] && !ischecked [ht1]) { /* a good lead that was missed on this pass */
					/* fprintf(SUMA_STDERR,"%s: Miss = %d, MissCur = %d\n", FuncName, miss, miss_cur); */
					++miss;
				}
			}
			++i;	
		}
		/*fprintf(SUMA_STDERR,"%s: Miss = %d, MissCur = %d\n", FuncName, miss, miss_cur);*/
		++N_iter;
	}

	fprintf(SUMA_STDERR,"%s: %d iterations required to check the surface.\n", FuncName, N_iter);
	fprintf(SUMA_STDERR,"%s: %d/%d (%f%%) triangles checked.\n", FuncName, N_checked, SEL->N_EL/3, (float)N_checked/(SEL->N_EL/3)*100.0);
	if (N_flip) {
		fprintf(SUMA_STDERR,"%s: %d triangles were flipped to make them consistent with the triangle containing the first edge in the list.\n", FuncName, N_flip);
	} else fprintf(SUMA_STDERR,"%s: All checked triangles were consistent with the triangle containing the first edge in the list.\n", FuncName);
	if (miss) {
		fprintf(SUMA_STDERR,"%s: %d segments with two neighbors were skipped. Not good in general.\n", FuncName, miss);
	}
	#if 0
		/* now show the fixed mesh list */
		fprintf (SUMA_STDERR,"%s: %d triangles were flipped \n", FuncName, N_flip);
		for (i=0; i < SEL->N_EL/3; ++i) {
			if (isflip[i]) fprintf (SUMA_STDERR,"\t%d %d %d\t(%d)\t*\n", FL[i][0], FL[i][1], FL[i][2], ischecked[i]);
				else fprintf (SUMA_STDERR,"\t%d %d %d\t(%d)\n", FL[i][0], FL[i][1], FL[i][2], ischecked[i]);
		}
	#endif
		
	/* freedom */
	fprintf(SUMA_STDERR,"%s: Free time \n", FuncName);
	if (isflip) free(isflip);
	if (ischecked) free(ischecked);
	fprintf(SUMA_STDERR,"%s: returning.\n", FuncName);
	
	return (YUP);
}

#ifdef SUMA_MakeConsistent_STANDALONE
void usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_MakeConsistent <FaceSetList file>\n");
			 printf ("To compile: \ngcc -DSUMA_MakeConsistent_STANDALONE -Wall -o SUMA_MakeConsistent SUMA_MiscFunc.c ");
			 printf ("SUMA_lib.a libmri.a -I/usr/X11R6/include -I./ -L/usr/lib -L/usr/X11R6/lib \n");
			 printf ("-lm -lGL -lGLU -lGLw -lXmu -lXm -lXt -lXext -lX11 -lMesaGLw -lMesaGLwM \n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \t Wed Mar 20 14:23:42 EST 2002\n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
   int **FL, N_FL, i;
	SUMA_EDGE_LIST *SEL;
	
   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_MakeConsistent-Main-");
   
   
   if (argc < 2)
       {
          usage ();
          exit (1);
       }
   
	N_FL = SUMA_float_file_size(argv[1]);
	
	N_FL = N_FL / 3;
	FL = (int **)SUMA_allocate2D(N_FL, 3, sizeof(int));
	
	SUMA_Read_2Ddfile (argv[1], FL,  N_FL, 3);
	
	
	/* make the edge list */
	SEL = SUMA_Make_Edge_List (FL, N_FL);
	if (SEL == NULL) {
		fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_Make_Edge_List.\n", FuncName);
		return (NOPE);
	}

	if (!SUMA_MakeConsistent (FL, N_FL, SEL)) {
		fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_MakeConsistent.\n", FuncName);
		return (1);
	}else {
		fprintf(SUMA_STDERR,"%s: Eeeexcellent.\n", FuncName);
	}
	
		fprintf(SUMA_STDERR,"%s REARRANGED TRIANGLES:\n", FuncName);
	for (i=0; i<N_FL; ++i)
		fprintf(SUMA_STDERR," %d %d %d\n", FL[i][0], FL[i][1], FL[i][2]); 
	
	SUMA_free_Edge_List(SEL);

	return (0);
}/* Main */

#endif

/*------------------------- Triangle Consistency Functions END--------------------------------------- */

/*-------------------------Node Attributes, smoothing functions BEGIN ------------------- */
/*!
	Smooth the attributes of the nodes based on the neighboring values. 
	Nodes are neighbors if they are connected by an edge in the triangulation.

	\param attr (float *) pointer to vector of type tp containing a node's attribute
	\param tp (SUMA_VARTYPE) type of values in attr (SUMA_float, SUMA_int)
	\param attr_sm (float *) pointer to smoothed version of attr. If you pass NULL then
	         the pointer is allocated for and returned from the function. If attr_sm is not
				null then it is assumed to have the required allocated space for the proper type.
	\param fn (SUMA_NODE_FIRST_NEIGHB) structure containing the first order neighbors of the nodes. 
				It is assumed that fn contains the neighbors info for all nodes whose attributes are in attr.
				That is from 0 to N_attr.  
	\ret attr_sm (float *) pointer to smoothed version of attr
	    
*/
float * SUMA_SmoothAttr_Neighb (float *attr, int N_attr, float *attr_sm, SUMA_NODE_FIRST_NEIGHB *fn)
{
	static char FuncName[]={"SUMA_SmoothAttr_Neighb"};
	int i, j;
	 
	if (attr_sm && attr_sm == attr) {
		fprintf (SUMA_STDERR, "Error %s: attr and attr_sm point to the same location. BAD!\n",FuncName);
		return (NULL); 
	}
	if (fn == NULL) {
		fprintf (SUMA_STDERR, "Error %s: fn is null, nothing to do.\n",FuncName);
		return (NULL); 
	}
	if (fn->N_Node != N_attr) {
		fprintf (SUMA_STDERR, "Error %s: N_attr (%d) must be equal to fn->N_Node (%d).\n",FuncName, N_attr, fn->N_Node);
		return (NULL); 
	}
	
	attr_sm = (float *)attr_sm;
	if (attr_sm == NULL) {
		attr_sm = (float *)calloc (N_attr, sizeof(float));
	}
	
	if (attr_sm == NULL)
	{
		fprintf (SUMA_STDERR, "Error %s: Failed to allocate for returning variable.\n", FuncName);
		return (NULL);
	} 
	
	for (i=0; i < N_attr; ++i) {
		/* make sure node id corresponds to i. That is you have a full set of nodes 0..N_attr */
		if (fn->NodeId[i] != i) {
			/* It's OK not to die here. This does occur in patches */
			/*fprintf (SUMA_STDERR, "Warning %s: fn does not seem to contain an explicit list of neighbors, from 0..N_attr. fn->NodeId[i] = %d, i = %d. Skipping node %d.\n", \
				FuncName, fn->NodeId[i], i, i); */
			/*free (attr_sm); 
			attr_sm = NULL;
			return (attr_sm);*/
			continue;
		}
		attr_sm[i] = attr[i];
		for (j=0; j < fn->N_Neighb[i]; ++j)
		{
			attr_sm[i] += attr[fn->FirstNeighb[i][j]]; 
		}	
		attr_sm[i] /= (fn->N_Neighb[i]+1);
	}
	
	return (attr_sm);	
} 
/*-------------------------Node Attributes, smoothing functions END ------------------- */

/*! 
	build the node neighbor structure. Nodes are neighbors is they share an edge
	ans =  SUMA_Build_FirstNeighb (EL, N_Node)

	\param EL (SUMA_EDGE_LIST *) pointer to the EdgeList structure (usually SO->EL)
	\param N_Node (int) total number of nodes (usually SO->N_Node)
	\ret FN (SUMA_NODE_FIRST_NEIGHB *) pointer to the neighbor list structure

*/
SUMA_NODE_FIRST_NEIGHB * SUMA_Build_FirstNeighb (SUMA_EDGE_LIST *el, int N_Node)
{
	static char FuncName[]={"SUMA_BuildFirstNeighb"};
	int i, j, n1, n2,  **FirstNeighb, N_ELm1;
	SUMA_Boolean skp;
	SUMA_NODE_FIRST_NEIGHB *FN;
	
	if (el == NULL || N_Node == 0) {
		fprintf(SUMA_STDERR, "Error %s: el == NULL or N_Node == 0, nothing to do.\n", FuncName);
		return (NULL);
	}	
	
	FN = (SUMA_NODE_FIRST_NEIGHB *)malloc(sizeof(SUMA_NODE_FIRST_NEIGHB));
	if (FN == NULL) {
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space for FN\n", FuncName);
		return (NULL);
	}
	
	/* allocate space for FN's matrices */
	FN->N_Node = N_Node;
	FN->N_Neighb_max = 0;
	
	FN->FirstNeighb = (int **) SUMA_allocate2D(FN->N_Node, SUMA_MAX_NUMBER_NODE_NEIGHB+1, sizeof (int));
	FN->N_Neighb = (int *) calloc (FN->N_Node, sizeof(int));
	FN->NodeId = (int *) calloc (FN->N_Node, sizeof(int));
	
	if (FN->FirstNeighb == NULL || FN->N_Neighb == NULL || FN->NodeId == NULL ){
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space forFN->FirstNeighb &/| FN->N_Neighb &/| FN->NodeId.\n", FuncName);
		return (NULL);
	} 
	
	/*fprintf(SUMA_STDOUT, "%s: Creating list ...\n", FuncName);*/
	
	FN->N_Neighb_max = 0;
	N_ELm1 = el->N_EL-1;
	j=0;
	while (j < el->N_EL) 
	{
		n1 = el->EL[j][0];
		n2 = el->EL[j][1];
		
		if (FN->N_Neighb[n1] > SUMA_MAX_NUMBER_NODE_NEIGHB || FN->N_Neighb[n2] > SUMA_MAX_NUMBER_NODE_NEIGHB) {
			fprintf(SUMA_STDERR, "Error %s: Maximum number of node neighbors exceeded, increase SUMA_MAX_NUMBER_NODE_NEIGHB (%d)", FuncName, SUMA_MAX_NUMBER_NODE_NEIGHB);
			SUMA_Free_FirstNeighb (FN);
			return (NULL);
		}
	
		/*register the neighbors for both nodes*/
		FN->NodeId[n1] = n1; /* this field may come in handy when operations need to be performed on subsets of the nodes making up the surface */
		FN->NodeId[n2] = n2;
		FN->FirstNeighb[n1][FN->N_Neighb[n1]] = n2;
		FN->FirstNeighb[n2][FN->N_Neighb[n2]] = n1;
	
		/* increment neighbor count for nodes in edge */
		FN->N_Neighb[n1] += 1;
		FN->N_Neighb[n2] += 1;
		
		if (FN->N_Neighb[n1] > FN->N_Neighb_max) FN->N_Neighb_max = FN->N_Neighb[n1];
		if (FN->N_Neighb[n2] > FN->N_Neighb_max) FN->N_Neighb_max = FN->N_Neighb[n2];
		
		/* skip duplicate edges */
		if (j < N_ELm1) {
			skp = NOPE;
			do {
				if (el->EL[j+1][0] == el->EL[j][0] && el->EL[j+1][1] == el->EL[j][1]) {
					++j;
				} else {
					skp = YUP;
				}
			} while (!skp && j < N_ELm1);
		}
		
		++j;
	}/* for j */

	/* now reallocate for final FirstNeighb */
	FirstNeighb = (int **) SUMA_allocate2D(FN->N_Node, FN->N_Neighb_max, sizeof (int));
	if (FirstNeighb == NULL){
		fprintf(SUMA_STDERR, "Error %s: Could not allocate space for FirstNeighb\n", FuncName);
		SUMA_Free_FirstNeighb (FN);
		return (NULL);
	} 

	/* crop left over allocated space */
	for (i=0; i < N_Node; ++i) {
		for (j=0; j < FN->N_Neighb[i]; ++j) {
			FirstNeighb[i][j] = FN->FirstNeighb[i][j];
		}
	}
	SUMA_free2D((char **)FN->FirstNeighb, N_Node);
	FN->FirstNeighb = FirstNeighb;
	return (FN);
}

/*!
	frees the Node Neighbor structure formed in SUMA_Build_FirstNeighb
*/ 
SUMA_Boolean SUMA_Free_FirstNeighb (SUMA_NODE_FIRST_NEIGHB *FN)
{
	if (FN->NodeId) free (FN->NodeId);
	if (FN->N_Neighb) free(FN->N_Neighb);
	if (FN->FirstNeighb) SUMA_free2D ((char **)FN->FirstNeighb, FN->N_Node);
	if (FN) free(FN);
	return (YUP);
}

/*!
	Calculate the area of polygons
	A = SUMA_PolySurf3 (NodeXYZ, int N_Node, int **FaceSets, int N_FaceSet, int PolyDim, float **FN)
	\param NodeXYZ (float **) matrix (N_Node x 3) containing XYZ of each node
	\param N_Node number of nodes in NodeXYZ
	\param FaceSets (int **) matrix (N_FaceSet x PolyDim) defining the polygons by their indices into NodeXYZ
	\param N_FaceSet (int) number of polygons
	\param PolyDim (int) dimension of polygons (3 triangles)
	\param FN (float **) N_FaceSet x 3 Normal vector to polygons
	\ret A (float *) vector containing the area of each polygon in FaceSets
	
	Algorithm by Dan Sunday http://geometryalgorithms.com
*/
float * SUMA_PolySurf3 (float **NodeXYZ, int N_Node, int **FaceSets, int N_FaceSet, int PolyDim, float **FN)
{
	static char FuncName[]={"SUMA_PolySurf3"};
	float **V, *A, ax, ay, az, an;
	int i, ii, coord, kk, jj;
	
	A = (float *) calloc (N_FaceSet, sizeof(float));
	V = (float **) SUMA_allocate2D(PolyDim+2, 3, sizeof(float));
	
	if (A == NULL || V == NULL) {
		fprintf(SUMA_STDERR,"Error %s; Failed to allocate for A or V\n", FuncName);
		return (NULL);
	}

	for (i=0; i < N_FaceSet; ++i) {
		if (FN[i][0] > 0) ax = FN[i][0];
			else ax = -FN[i][0];
		
		if (FN[i][1] > 0) ay = FN[i][1];
			else ay = -FN[i][1];
		
		if (FN[i][2] > 0) az = FN[i][2];
			else az = -FN[i][2];
	
	
		coord = 3;
		if (ax > ay) {
			if (ax > az) coord = 1;
		} else {
			if (ay > az) coord = 2;
		}
	
		for (ii=0; ii< PolyDim; ++ii) {
			V[ii][0] = NodeXYZ[FaceSets[i][ii]][0];
			V[ii][1] = NodeXYZ[FaceSets[i][ii]][1];
			V[ii][2] = NodeXYZ[FaceSets[i][ii]][2];
		}
		ii = PolyDim;
		V[ii][0] = V[0][0]; V[ii][1] = V[0][1]; V[ii][2] = V[0][2];
		ii = PolyDim + 1;
		V[ii][0] = V[1][0]; V[ii][1] = V[1][1]; V[ii][2] = V[1][2];
		
		/* compute area of 2D projection */
		jj = 2;
		kk = 0;
		for (ii=1; ii < PolyDim+1; ++ii) {
			switch (coord) {
				case 1:
					A[i] = A[i] + ( V[ii][1] * (V[jj][2] - V[kk][2]) );
					break;
				case 2:
					A[i] = A[i] + ( V[ii][0] * (V[jj][2] - V[kk][2]) );
					break;
				case 3:
					A[i] = A[i] + ( V[ii][0] * (V[jj][1] - V[kk][1]) );
					break;
			}
			
			++jj;
			++kk;
			
		}
		
		/* scale to get area before projection  */
		an = (float) sqrt(ax * ax + ay * ay + az * az);
		switch (coord) {
			case 1:
				A[i] = (A[i] * (an / (2*ax)));
				break;
			case 2:
				A[i] = (A[i] * (an / (2*ay)));
				break;
			case 3:
				A[i] = (A[i] * (an / (2*az)));
				break;
		}
		
	} /* for i*/
	
	SUMA_free2D((char **)V, PolyDim+2);
	return (A);
}

/* choose debug level for SUMA_Surface_Curvature, _1 gvies a pacifier, _2 gives a lot of info, _3 pauses for each node */
#define MAX_INCIDENT_TRI 200
#define DBG_1 

#ifdef DBG_3
	#define DBG_2
#endif

#ifdef DBG_2
	#define DBG_1
#endif
/*! function to calculate the curvature tensor at each node 
	SC = SUMA_Surface_Curvature (NodeList, N_Node, NodeNormList, A, N_FaceSet, FN, SUMA_EDGE_LIST *SEL)
	
	\param NodeList (float **) N_Node x 3 matrix containing the XYZ coordinates of the nodes
	\param N_Node (int)  number of nodes in NodeList
	\param NodeNormList (float **) N_Node x 3 matrix containing the normal vector at each node
	\param A (float *) N_FaceSet x 1 vector containing the area of each triangle making up the mesh
	\param N_FaceSet (int) number of triangles making up the mesh
	\param FN (SUMA_NODE_FIRST_NEIGHB *) structure containing Node Neighbors
	\param SEL (SUMA_EDGE_LIST *) structure containing the Edge List
	
	\ret SC (SUMA_SURFACE_CURVATURE *) structure containing the curvature info, see typedef of struct for more info
	
	\sa SUMA_Free_SURFACE_CURVATURE for freeing SC
	\sa SUMA_Build_FirstNeighb for creating FN
	\sa SUMA_Make_Edge_List for creating SEL
	
	The algorithm is the one presented in G. Taubin Estimating the tensor of curvature of surface from a polyhedral approximation
	see also labbook NIH-2 pp 65 and (Test_)SUMA_Surface_Curvature.m script
*/


SUMA_SURFACE_CURVATURE * SUMA_Surface_Curvature (float **NodeList, int N_Node, float **NodeNormList, float *A, int N_FaceSet, SUMA_NODE_FIRST_NEIGHB *FN, SUMA_EDGE_LIST *SEL)
{ 
	static char FuncName[] = {"SUMA_Surface_Curvature"};
	int i, N_Neighb, j, ji, Incident[MAX_INCIDENT_TRI], N_Incident, kk, ii; 
	float  Ntmp[3],  vi[3], vj[3], *Num, NumNorm, num, denum, sWij, T1e[3], T2e[3], mg, c, s;
	float **fa33, **fb33, **fc33, **Ni, **Nit, *Wij, *Kij, **Tij, **I, **Mi, **Q, **Qt, **fa22, **mMi, **mMir;
	SUMA_Boolean *SkipNode;
	SUMA_SURFACE_CURVATURE *SC;
	
	SC = (SUMA_SURFACE_CURVATURE *)malloc (sizeof(SUMA_SURFACE_CURVATURE));
	if (!SC) {
		fprintf (SUMA_STDERR, "Error %s: Failed to allocate for SC.\n", FuncName);
		return(NULL);
	}
	
	Wij = (float *)calloc (FN->N_Neighb_max, sizeof(float));
	Kij = (float *)calloc (FN->N_Neighb_max, sizeof(float));
	Num = (float *)calloc (3, sizeof(float));
	SkipNode = (SUMA_Boolean *) calloc (N_Node, sizeof(SUMA_Boolean));
	mMi = (float **) SUMA_allocate2D (2,2, sizeof(float));
	mMir =(float **) SUMA_allocate2D (2,2, sizeof(float));
	fa22 =(float **) SUMA_allocate2D (2,2, sizeof(float));
	Tij = (float **) SUMA_allocate2D (FN->N_Neighb_max, 3, sizeof(float));
	Ni =  (float **) SUMA_allocate2D (3, 1, sizeof(float));
	Nit = (float **) SUMA_allocate2D (1, 3, sizeof(float));
	fa33 =(float **) SUMA_allocate2D (3, 3, sizeof(float));
	fb33 =(float **) SUMA_allocate2D (3, 3, sizeof(float));
	fc33 =(float **) SUMA_allocate2D (3, 3, sizeof(float));
	I =   (float **) SUMA_allocate2D (3, 3, sizeof(float));
	Q =   (float **) SUMA_allocate2D (3, 3, sizeof(float));
	Qt =  (float **) SUMA_allocate2D (3, 3, sizeof(float));
	Mi =  (float **) SUMA_allocate2D (3, 3, sizeof(float));
	SC->T1 = (float **) SUMA_allocate2D (N_Node, 3, sizeof(float));
	SC->T2 = (float **) SUMA_allocate2D (N_Node, 3, sizeof(float));
	SC->Kp1 =(float *)calloc (N_Node, sizeof(float));
	SC->Kp2 =(float *)calloc (N_Node, sizeof(float));

	if (!fa22 || !mMir || !mMi || !Wij || !Kij || !Tij || !Ni || !Nit || !fa33 || !fb33 || !fc33 || !I || !Num || !SkipNode || !Mi || !Q || !Qt || !SC->T1 || !SC->T2 || !SC->Kp1 || !SC->Kp2) {
		fprintf (SUMA_STDERR, "Error %s: Failed to allocate for Wij, Kij, Tij.\n", FuncName);
		if (Wij) free (Wij);
		if (Kij) free (Kij);
		if (Num) free (Num);
		if (SkipNode) free(SkipNode);
		if (mMi) SUMA_free2D((char **)mMi, 2);
		if (mMir) SUMA_free2D((char **)mMir, 2);
		if (fa22) SUMA_free2D((char **)fa22, 2);
		if (Tij) SUMA_free2D((char **)Tij, FN->N_Neighb_max);
		if (Ni) SUMA_free2D((char **)Ni, 3);
		if (Nit) SUMA_free2D((char **)Nit, 1);
		if (fa33) SUMA_free2D((char **)fa33, 3);
		if (fb33) SUMA_free2D((char **)fb33, 3);
		if (I) SUMA_free2D((char **)I, 3);
		if (Q) SUMA_free2D((char **)Q, 3);
		if (Qt) SUMA_free2D((char **)Qt, 3);
		if (Mi) SUMA_free2D((char **)Mi, 3);
		if (SC) SUMA_Free_SURFACE_CURVATURE (SC);		
		return(NULL);
	}

	/* 3x3 identity matrix */
	I[0][0] = I[1][1] = I[2][2] = 1.0; I[0][1] = I[0][2] = I[1][0] = I[1][2] = I[2][0] = I[2][1] = 0.0;
	
	/* initialize SC */
	SC->N_SkipNode = 0;
	SC->N_Node = N_Node;
	
	fprintf (SUMA_STDERR, "%s: Beginning curvature computations:\n", FuncName);
	
	SC->N_SkipNode = 0;
	for (i=0; i < N_Node; ++i) { /* for i */
		#ifdef DBG_1
			if (!(i%10000)) {
				fprintf (SUMA_STDERR, "%s: [%d]/[%d] %.2f/100%% completed\n", FuncName, i, N_Node, (float)i / N_Node * 100);
			}
		#endif
		SkipNode[i] = NOPE;
		/* sanity copies */
		N_Neighb = FN->N_Neighb[i];
		Ni[0][0] = NodeNormList[i][0]; Ni[1][0] = NodeNormList[i][1]; Ni[2][0] = NodeNormList[i][2]; /* Normal vector at i*/
		Nit[0][0] = NodeNormList[i][0]; Nit[0][1] = NodeNormList[i][1]; Nit[0][2] = NodeNormList[i][2]; /* transpose of Ni */ 
		vi[0] = NodeList[i][0]; vi[1] = NodeList[i][1]; vi[2] = NodeList[i][2];  /* node coordinate vector */ 
		#ifdef DBG_2
			fprintf (SUMA_STDERR, "%s: Looping over neighbors, i = %d\n", FuncName, i);
		#endif
		j=0;
		sWij = 0.0;
		while (j < N_Neighb) {
			ji = FN->FirstNeighb[i][j]; /* index of the jth first neighbor of i */
			vj[0] = NodeList[ji][0]; vj[1] = NodeList[ji][1]; vj[2] = NodeList[ji][2];  /* node coordinate vector at jth neighbor */
			
			/* calculate Tij */
			#ifdef DBG_2
				fprintf (SUMA_STDERR, "%s: Mat Op j=%d\n", FuncName, j);
			#endif
			
			/* fa33 = Ni*Ni' */
			SUMA_MULT_MAT(Ni,Nit,fa33,3,1,3,float,float,float); 
			
			/* fb33 = I - fa33 */
			SUMA_SUB_MAT(I, fa33, fb33, 3, 3, float, float, float); 

			/* fa33 = vi - vj (only 1st column is meaningful)*/
			fa33[0][0] = vi[0] - vj[0];  fa33[1][0] = vi[1] - vj[1]; fa33[2][0] = vi[2] - vj[2];
			
			/* Num = fc33 = (I - Ni*Ni') * (vi - vj) (only 1st column in fc33 is meaningful)*/
			SUMA_MULT_MAT(fb33, fa33, fc33, 3, 3, 1, float, float, float);
			Num[0] = fc33[0][0]; Num[1] = fc33[1][0]; Num[2] = fc33[2][0];

			/* Calculate Tij at this j, a 3x1 vector unit length normalized projection projection of vj-vi onto the plane perp. to Ni */
			NumNorm = (float)sqrt(Num[0]*Num[0] + Num[1]*Num[1] + Num[2]*Num[2]);
			if (NumNorm == 0) {
				fprintf (SUMA_STDERR, "Warning %s: NumNorm = 0 for node %d.\n", FuncName, i); 
				SkipNode[i] = YUP;
				SC->N_SkipNode++;
				break;
			}
			
			Tij[j][0] = Num[0] / NumNorm; 
			Tij[j][1] = Num[1] / NumNorm; 
			Tij[j][2] = Num[2] / NumNorm;
			
			#ifdef DBG_2
				fprintf(SUMA_STDOUT,"%s: i,j, ji =%d,%d, %d Ni = %f %f %f\n Tij(%d,:) = %f %f %f.\n", \
					FuncName, i, j, ji,Ni[0][0], Ni[1][0], Ni[2][0], j, Tij[j][0], Tij[j][1], Tij[j][2]);
			#endif
			 
			/* calculate Kij(j) the directional curvature along ij*/
			/* fa33 = (vj - vi) (only 1st column is meaningful)*/
			fa33[0][0] = (vj[0] - vi[0]);  fa33[1][0] = (vj[1] - vi[1]); fa33[2][0] = (vj[2] - vi[2]);
			/* Num = fb33 = Ni' * fa33 (only 1st value in fb33 is meaningful)*/
			SUMA_MULT_MAT(Nit, fa33, fb33, 1, 3, 1, float, float, float);
			num = fb33[0][0]; 
			/* denum = sum((vj - vi)^2) */
			denum = fa33[0][0] * fa33[0][0] + fa33[1][0] * fa33[1][0]+ fa33[2][0] * fa33[2][0]; 
			
			Kij[j] = 2 * num / denum;
			#ifdef DBG_2
				fprintf(SUMA_STDOUT,"%s: Kij[%d] = %f\n", FuncName, j, Kij[j]);
			#endif
			
			/* calculate the weights for integration, Wij */
				/* find the incident triangles */
				if (!SUMA_Get_Incident(i, ji, SEL, Incident, &N_Incident))
				{
					fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Get_Incident.\n", FuncName);
					if (Wij) free (Wij);
					if (Kij) free (Kij);
					if (Num) free (Num);
					if (SkipNode) free(SkipNode);
					if (mMi) SUMA_free2D((char **)mMi, 2);
					if (mMir) SUMA_free2D((char **)mMir, 2);
					if (fa22) SUMA_free2D((char **)fa22, 2);
					if (Tij) SUMA_free2D((char **)Tij, FN->N_Neighb_max);
					if (Ni) SUMA_free2D((char **)Ni, 3);
					if (Nit) SUMA_free2D((char **)Nit, 1);
					if (fa33) SUMA_free2D((char **)fa33, 3);
					if (fb33) SUMA_free2D((char **)fb33, 3);
					if (I) SUMA_free2D((char **)I, 3);
					if (Q) SUMA_free2D((char **)Q, 3);
					if (Qt) SUMA_free2D((char **)Qt, 3);
					if (Mi) SUMA_free2D((char **)Mi, 3);
					if (SC) SUMA_Free_SURFACE_CURVATURE (SC);		
					return(NULL);
				}

				#ifdef DBG_2
					fprintf (SUMA_STDERR,"%s: Incidents ...\n", FuncName);
					for (kk=0; kk < N_Incident; ++kk) {
						fprintf (SUMA_STDERR,"\t %d", Incident[kk]);
					}
					fprintf (SUMA_STDERR,"\n");
				#endif

				if (N_Incident != 2 && N_Incident != 1)
				{
					fprintf (SUMA_STDERR,"Warning %s: Unexpected N_Incident = %d at i,j = %d,%d\n", FuncName, N_Incident, i, j);
					SkipNode[i] = YUP;
					++SC->N_SkipNode;
					break;
				}
				Wij[j] = 0.0; 
				for (ii=0; ii < N_Incident; ++ii) {
					Wij[j] = Wij[j] + fabs(A[Incident[ii]]);
				}
				sWij += Wij[j];
				if (Wij[j] == 0.0) {
					fprintf (SUMA_STDERR,"Warning %s: Null Wij[%d] at i,j=%d,%d\n", FuncName, j, i, j);
					SkipNode[i] = YUP;
					++SC->N_SkipNode;
					break; 
				}
			
			++j;
			
		}/* while j*/
		if (!SkipNode[i]) {
				/* make the sum of the weights be equal to 1*/
				#ifdef DBG_2	
					fprintf (SUMA_STDERR,"%s: Wij:\n", FuncName);
				#endif
				for (ii=0; ii < N_Neighb; ++ii) {
					Wij[ii] /= sWij; 
					/*	fprintf (SUMA_STDERR,"Wij[%d]=%f\t", ii, Wij[ii]);*/
				}
				#ifdef DBG_2	
					fprintf (SUMA_STDERR,"\n");
				#endif
				/* calculate Mi */
				Mi[0][0] = Mi[1][0] = Mi[2][0] = Mi[0][1] = Mi[1][1] = Mi[2][1] = Mi[0][2] = Mi[1][2] = Mi[2][2] = 0.0;
				for (j=0; j < N_Neighb; ++j) {
					/* calculate fc33 = Tij(j,:)' * Tij(j,:) transpose on Tij is flipped from equation because Tij(j,:) should be a column vector */
 					fa33[0][0] = Tij[j][0]; fa33[1][0] = Tij[j][1]; fa33[2][0] = Tij[j][2];
					fb33[0][0] = Tij[j][0]; fb33[0][1] = Tij[j][1]; fb33[0][2] = Tij[j][2];
					SUMA_MULT_MAT (fa33, fb33, fc33, 3, 1, 3, float, float, float);
					
					for (ii=0; ii < 3; ++ii) {
						for (kk=0; kk < 3; ++kk) {
							Mi[ii][kk] = Mi[ii][kk] + Wij[j] * Kij[j] * fc33[ii][kk];
						}
					}
				}
				#ifdef DBG_2	
					SUMA_disp_mat (Mi, 3, 3, 1);
				#endif
				/* calculate Householder of Ni */
				Ntmp[0] = Ni[0][0]; Ntmp[1] = Ni[1][0]; Ntmp[2] = Ni[2][0];
				if (!SUMA_Householder(Ntmp, Q)) {
					fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Householder for node %d.\n ", FuncName, i);
					mg = 0.0;
					SkipNode[i] = YUP;
					SC->N_SkipNode++;
				} else {
					T1e[0] = Q[0][1]; T1e[1] = Q[1][1]; T1e[2] = Q[2][1];
					T2e[0] = Q[0][2]; T2e[1] = Q[1][2]; T2e[2] = Q[2][2];  /* T tilda 1, T tilda2 */
					SUMA_TRANSP_MAT (Q, Qt, 3, 3, float, float);
					#ifdef DBG_2	
						SUMA_disp_mat (Q, 3, 3, 1);
						SUMA_disp_mat (Qt, 3, 3, 1);
					#endif
					
					/* Mi (aka fb33) = Q' * Mi * Q; Mi should become a 3x3 with 2x2  non zero minor in lower right */
					SUMA_MULT_MAT (Qt, Mi, fa33, 3, 3, 3, float, float, float);
					SUMA_MULT_MAT (fa33, Q, Mi, 3, 3, 3, float, float, float);
					#ifdef DBG_2	
						SUMA_disp_mat (Mi, 3, 3, 1);
					#endif
					mMi[0][0] = Mi[1][1]; mMi[0][1] = Mi[1][2];
					mMi[1][0] = Mi[2][1]; mMi[1][1] = Mi[2][2]; 
					
					/*compute c ( = cos(theta) ) & s ( = sin(theta) )from the Givens rotation to null out the bottom left element of the non zero minor mMi*/
					mg = sqrtf(mMi[0][0]*mMi[0][0] + mMi[1][0]*mMi[1][0]);
					c = mMi[0][0] / mg;
					s = mMi[1][0] / mg;
					/* rotate mMi */
					fa22[0][0] =  c; fa22[0][1] = s; 
					fa22[1][0] = -s; fa22[1][1] = c; 
					SUMA_MULT_MAT(fa22, mMi, mMir, 2, 2, 2, float, float, float);
					#ifdef DBG_2	
						fprintf (SUMA_STDERR,"%s: mg = %f, c = %f, s = %f\n", FuncName,  mg, c, s);
					#endif	
					/* calculate the principal directions */
					SC->T1[i][0] = c * T1e[0] - s * T2e[0];					
					SC->T1[i][1] = c * T1e[1] - s * T2e[1];					
					SC->T1[i][2] = c * T1e[2] - s * T2e[2];	
									
					SC->T2[i][0] = s * T1e[0] + c * T2e[0];					
					SC->T2[i][1] = s * T1e[1] + c * T2e[1];					
					SC->T2[i][2] = s * T1e[2] + c * T2e[2];	
					
					/* calculate the principal curvatures and mean curvatures etc ... */
					SC->Kp1[i] = 3 * mMir[0][0] - mMir[1][1];
					SC->Kp2[i] = 3 * mMir[1][1] - mMir[0][0];
					#ifdef DBG_2	
						fprintf (SUMA_STDERR,"%s: SC->Kp1[i] = %f, SC->Kp2[i] = %f, mKp[i] = %f\n", FuncName,  SC->Kp1[i], SC->Kp2[i], (SC->Kp1[i]+SC->Kp2[i])/2);
					#endif	
				}
				
			#ifdef DBG_3
				{ int jnk; fprintf(SUMA_STDOUT,"Pausing ..."); jnk = getchar(); fprintf(SUMA_STDOUT,"\n"); }
			#endif
			
		} /* not skipped (yet)*/
		if (SkipNode[i]) {
			SC->T1[i][0] = SC->T1[i][1] = SC->T1[i][2] = 0.0;
			SC->T2[i][0] = SC->T2[i][1] = SC->T2[i][2] = 0.0;
			SC->Kp1[i] = SC->Kp2[i] = 0.0;
		}
	}/* for i */

	/* write out the results to a file (debugging only)*/
	{
		FILE *fid;
		fprintf(SUMA_STDOUT,"%s: Writing Kp1 & Kp2 to Curvs_c.txt ...", FuncName);
		fid = fopen("Curvs_c.txt","w");
		for (ii=0; ii < SC->N_Node; ++ii) {
			/*fprintf(fid,"%f %f\n", (SC->Kp1[ii]+SC->Kp2[ii])/2, SC->Kp1[ii]*SC->Kp2[ii]);*/
			fprintf(fid,"%f %f\n", SC->Kp1[ii], SC->Kp2[ii]);
		}
		fclose (fid);
		
		fprintf(SUMA_STDOUT,"%s: Done.\n", FuncName);
	}
	
	/* free the left overs */
	if (Wij) free (Wij);
	if (Kij) free (Kij);
	if (Num) free (Num);
	if (SkipNode) free(SkipNode);
	if (mMi) SUMA_free2D((char **)mMi, 2);
	if (mMir) SUMA_free2D((char **)mMir, 2);
	if (fa22) SUMA_free2D((char **)fa22, 2);
	if (Tij) SUMA_free2D((char **)Tij, FN->N_Neighb_max);
	if (Ni) SUMA_free2D((char **)Ni, 3);
	if (Nit) SUMA_free2D((char **)Nit, 1);
	if (fa33) SUMA_free2D((char **)fa33, 3);
	if (fb33) SUMA_free2D((char **)fb33, 3);
	if (I) SUMA_free2D((char **)I, 3);
	if (Q) SUMA_free2D((char **)Q, 3);
	if (Qt) SUMA_free2D((char **)Qt, 3);
	if (Mi) SUMA_free2D((char **)Mi, 3);
	
	fprintf (SUMA_STDERR, "%s: Done with curvature computations.\n", FuncName);

	return (SC);
}

/*!
	free the SUMA_SURFACE_CURVATURE structure
*/
void SUMA_Free_SURFACE_CURVATURE (SUMA_SURFACE_CURVATURE *SC)
{
	if (SC == NULL) return;
	if (SC->Kp1) free (SC->Kp1);
	if (SC->Kp2) free (SC->Kp2);
	if (SC->T1) SUMA_free2D ((char **)SC->T1, SC->N_Node);
	if (SC->T2) SUMA_free2D ((char **)SC->T2, SC->N_Node);
	if (SC) free(SC);
	return;
}

/*!
	Computes the householder matrix for a 3x1 vector 
	Vh = Q * V will have all elements but the first = 0
	
	ans = SUMA_Householder (float *V, float **Q)
	
	\param V (float *) 3x1 column vector
	\param Q (float **) 3x3 (pre-allocated) matrix that will contain the Householder matrix
	
	\ret ans (SUMA_Boolean) YUP/NOPE (failure)
	
	The code for this function contains two algorithms, one is identical to 
	Taubin's suggestion and one is a generic Householder algorithm. 
	
*/
#define TAUBIN_Householder
SUMA_Boolean SUMA_Householder (float *Ni, float **Q)
{
	static char FuncName[] = {"SUMA_Householder"};
	float mNi, e[3], b[3], mb;
	int ii;
	#ifdef TAUBIN_Householder
	float d[3], s[3], nd, ns;
	#endif
	
	e[0] = 1.0; e[1] = 0.0; e[2] = 0.0;
	
	#ifndef TAUBIN_Householder
		/* generic algorithm */
		mNi = sqrtf(Ni[0] * Ni[0] + Ni[1] * Ni[1] + Ni[2] * Ni[2]);
		for (ii=0; ii < 3; ++ii) 
			b[ii] = Ni[ii] + mNi * e[ii];
		mb = sqrtf(b[0] * b[0] + b[1] * b[1] + b[2] * b[2]);

		if (mb == 0) {
			fprintf (SUMA_STDERR,"Error %s: mb = 0\n",FuncName);
			return (NOPE);
		}

		b[0] /= mb; b[1] /= mb; b[2] /= mb;
	#else
		/* Taubin's algorithm Estimating the tensor of curvature of a surface from a polyhedral approximation */ 
		/* calculate difference and sum vectors with their norms (save sqrtf for later) */
		
		d[0] = e[0] - Ni[0]; d[1] = e[1] - Ni[1]; d[2] = e[2] - Ni[2]; 
		nd = d[0]*d[0] + d[1]*d[1] + d[2]*d[2];
		
		s[0] = e[0] + Ni[0]; s[1] = e[1] + Ni[1]; s[2] = e[2] + Ni[2];
		ns = s[0]*s[0] + s[1]*s[1] + s[2]*s[2];
		
		if (!nd || !ns) {
			fprintf (SUMA_STDERR,"Error %s: nd || ns = 0\n",FuncName);
			return (NOPE);
		}
		
		if (nd > ns) {
			nd = sqrtf(nd);
			b[0] = d[0] / nd;
			b[1] = d[1] / nd;
			b[2] = d[2] / nd;
			/*Q(:,1) will be equal to -Ni*/
			
		} else {
			ns = sqrtf(ns);
			b[0] = s[0] / ns;
			b[1] = s[1] / ns;
			b[2] = s[2] / ns;
			/*Q(:,1) will be equal to  Ni */
		}
		
	#endif 
	
	/* calc Q = I - 2 b * b' */
	Q[0][0] = 1 - 2 * b[0] * b[0];
	Q[1][0] = - 2 * b[1] * b[0];
	Q[2][0] = - 2 * b[2] * b[0];

	Q[0][1] = - 2 * b[0] * b[1];
	Q[1][1] = 1 - 2 * b[1] * b[1];
	Q[2][1] = - 2 * b[2] * b[1];

	Q[0][2] = - 2 * b[0] * b[2];
	Q[1][2] = - 2 * b[1] * b[2];
	Q[2][2] = 1 - 2 * b[2] * b[2];

	return (YUP);	
}

/*! 
	C = SUMA_Convexity (NodeList, N_Node, NodeNormList, FN)

	\param NodeList (float **) N_Node x 3 matrix containing the coordinates for each node
	\param N_Node (int) number of nodes
	\param NodeNormList (float **) N_Node x 3 matrix containing the unit normals at each node
	\param FN (SUMA_NODE_FIRST_NEIGHB *) first order node neighbor structure
	\ret C (float *) N_Node x 1 vector containing the curvature at each node. The curvature is the 
	sum of the signed distance of all the neighboring nodes to the tangent plane. The sign if C[i] 
	indicates the convexity.
	
	C[i] = Sum(dj/dij) over all neighbors j of i
	dj is the distance of neighboring node j to the tangent plane at i
	dij is the length of the segment ij
	
	You can consider the magnitude of C as a measure of the curvature at the node. 
	Use it wisely. 
	  
	The Normals are assumed to be unit vectors
*/
float * SUMA_Convexity (float **NL, int N_N, float **NNL, SUMA_NODE_FIRST_NEIGHB *FN)
{
	static char FuncName[]={"SUMA_Convexity"};
	float *C, d, D, dij;
	int i, j, in;
	
	C = NULL;
	
	/* allocate for C */
	C = (float *)calloc (N_N, sizeof(float));
	
	if (C == NULL) {
		fprintf (SUMA_STDERR,"Error %s: Could not allocate for C.\n", FuncName);
		return (C);
	}
	
	for (i=0; i < N_N; ++i) {
		/* the plane at node i, having normal NNL(i,:) has the equation A X + B Y + C Z + D = 0
		NNL[i][0] NL[i][0]  + NNL[i][1] NNL[i][1]  + NNL[i][2] NL[i][2] + D = 0 */
		
		D = -NNL[i][0] * NL[i][0] - NNL[i][1] * NL[i][1] - NNL[i][2] * NL[i][2];
		
		for (j=0; j < FN->N_Neighb[i]; ++j) {
			/* find the distance between the neighboring node j and the tangent plane at i 
			d = (A X + B Y + C Z + D ) / (sqrt(A*A + B*B + C*C))
			denominator is norm of Normals which should be 1
			*/
			in = FN->FirstNeighb[i][j];
			d = NNL[i][0] * NL[in][0] + NNL[i][1] * NL[in][1] + NNL[i][2] * NL[in][2] + D ;
			
			/* calculate the distance between node i and it's neighbor */
			dij = sqrtf( (NL[in][0] - NL[i][0]) * (NL[in][0] - NL[i][0]) + (NL[in][1] - NL[i][1]) * (NL[in][1] - NL[i][1]) + (NL[in][2] - NL[i][2]) * (NL[in][2] - NL[i][2]));
			
			/* assuming normals are normalized d is the cosine of the angle between the two vectors */
			/* if d > 0, then angle is > 0..90 degrees */
			
			/* as a measure of curvature, compute the sum of signed distances of negihbors to tangent plane at i.
			use distances normalized by length of segment ij to account for differences in segment length */
			 
			C[i] += d/dij;
		}
	
	}
	
	/* Now write the results to disk just for debugging */
	#if 0
	{
		FILE *fid;
		fprintf(SUMA_STDOUT,"%s: Writing convexity to Conv.txt ...", FuncName);
		fid = fopen("Conv.txt","w");
		for (i=0; i < N_N; ++i) {
			fprintf(fid,"%f\n", C[i]);
		}
		fclose (fid);
		
		fprintf(SUMA_STDOUT,"%s: Done.\n", FuncName);
	}
	#endif
	
	return (C);
} 

/*! 
	Function to pad a string to a certain length
	
		char * SUMA_pad_str (char *str, char pad_val , int pad_ln , int opt)
 
 		str, (char *) string with the original string
 		pad_char, (char )  padding character
 		pad_ln, (int) final padded lenght, 
 		opt, (int) 0 if padding occurs to the left of str (00005)
 						1 if padding occurs to the right of str (50000)
	Returns : 
 		a pointer to the padded string .
 
*/

char * SUMA_pad_str ( char *str, char pad_val , int pad_ln , int opt)
	{/*SUMA_pad_str*/
 		static char FuncName[]={"SUMA_pad_str"};
		int lo,i;
 		char *strp , *buf1;
 		
 		assert (str);
 		
 		lo = (int)strlen(str);
		
		buf1 = (char *)calloc (pad_ln-lo+2,sizeof (char));
 		strp = (char *)calloc (pad_ln+lo+2,sizeof (char));
				
		for (i=0;i<pad_ln-lo;++i)
 			{
 				if (i == 0) sprintf (buf1,"%c",pad_val);
 					else sprintf (buf1,"%s%c",buf1,pad_val);
 						
 			}
 		if (opt == 0)
 			sprintf (strp,"%s%s",buf1,str);
 		else if (opt == 1)
 			{
 				sprintf (strp,"%s%s",str,buf1);
 				
			}			
 			else 
 				{
 					fprintf (SUMA_STDERR, "Error %s: Wrong opt paramter, only (0,1) allowed\n", FuncName);
 					free (strp);
					free (buf1);
					return (NULL);
 				}
 		
 		free (buf1);
 		
 		return (strp);
 		
	}/*SUMA_pad_str*/
