#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 

#define USE_SUMA_ALLOC 
#ifdef USE_SUMA_ALLOC
/* This group of functions will get replaced by Bob's mcw_malloc functions that are more efficient */

   /*!
      ptr = SUMA_malloc_fn (const char *CF,  size );
      \purpose a wrapper around malloc function that allows one to keep track of allocated memory.
      For the tracking to occurr, you need to have SUMA_MEMTRACE_FLAG set to 1 (when compiling) and then turn the flag SUMAg_CF->MemTrace on.
      ifdef SUMA_MEMTRACE_FLAG and SUMAg_CF->MemTrace then when size bytes are allocated to ptr the following happens:
      SUMAg_CF->Mem->Pointers[SUMAg_CF->Mem->N_alloc] = ptr;
      SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc] = size;
      ++SUMAg_CF->Mem->N_alloc; 

      otherwise, only ptr = malloc (size) is executed.
      \param CF (const char *) name of calling function
      \param size (size_t)
      \ret ptr (void *)
   */
   void *SUMA_malloc_fn (const char *CF, size_t size)
   {
      void *ptr;
      static char FuncName[]={"SUMA_malloc_fn"};

      #if SUMA_MEMTRACE_FLAG
         if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
      #endif
      /* The allocation */
      ptr = malloc (size);

      #if SUMA_MEMTRACE_FLAG
         if (SUMAg_CF->MemTrace) {
            ++SUMAg_CF->Mem->N_alloc;
            if (SUMAg_CF->Mem->N_MaxPointers <= SUMAg_CF->Mem->N_alloc) {
               /* must reallocate */
               SUMAg_CF->Mem->N_MaxPointers += SUMA_MEMTRACE_BLOCK;
               SUMAg_CF->Mem->Pointers = (void **)realloc (SUMAg_CF->Mem->Pointers, sizeof(void*) * SUMAg_CF->Mem->N_MaxPointers);
               SUMAg_CF->Mem->Size  = (int *)realloc (SUMAg_CF->Mem->Size, sizeof(int) * SUMAg_CF->Mem->N_MaxPointers);
              if (!SUMAg_CF->Mem->Pointers || !SUMAg_CF->Mem->Pointers) {
                  fprintf (SUMA_STDERR, "Error %s: Failed to reallocate.\nTurning off memory tracing.\n", \
                           FuncName);
                  /* free up allocated space, clean up pointers, turn off memory tracing */
                  if (SUMAg_CF->Mem->Pointers) free(SUMAg_CF->Mem->Pointers); 
                  if (SUMAg_CF->Mem->Size) free(SUMAg_CF->Mem->Size); 
                  SUMAg_CF->MemTrace = 0;
                  SUMAg_CF->Mem->N_alloc = 0;
                  SUMAg_CF->Mem->N_MaxPointers = 0;
                  SUMA_RETURN(ptr);
               }
            }
            SUMAg_CF->Mem->Pointers[SUMAg_CF->Mem->N_alloc-1] = ptr;
            SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc-1] = size;
         }
         SUMA_RETURN(ptr);
      #else
         return(ptr);
      #endif
   }

   void *SUMA_realloc_fn (const char *CF, void *ptr, size_t size) 
   {
      void *ptr2;
      int i;
      static char FuncName[]={"SUMA_realloc_fn"};

      #if SUMA_MEMTRACE_FLAG
         if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
      #endif

      /* The allocation */
      ptr2 = realloc(ptr, size);

      #if SUMA_MEMTRACE_FLAG
         if (SUMAg_CF->MemTrace) {
            SUMA_Boolean Found = NOPE;
            /* find the pointer that's being changed*/
            for (i=0; i < SUMAg_CF->Mem->N_alloc && !Found; ++i) {
               if (SUMAg_CF->Mem->Pointers[i] == ptr) {
                 /* cleanup that one and replace with new*/
                  SUMAg_CF->Mem->Pointers[i] = ptr2;
                  SUMAg_CF->Mem->Size[i] = size;
                  Found = YUP;
               }   
            }

            if (!Found) {
              fprintf (SUMA_STDERR, "Error %s: Pointer %p not found in Mem struct. \n", FuncName,ptr); 
            }
         }

         SUMA_RETURN(ptr2);
      #else
         return(ptr2);
      #endif

   }
   /*!
      This function is very similar to SUMA_malloc_fn except that it uses calloc instead of malloc and 
      SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc] = nmemb*size;
      \param CF (const char *) name of calling function
      \param nmemb (size_t) number of elements
      \param size (size_t) size of an element
      \ret ptr (void *) pointer to nmemb*size allocated space

      \sa SUMA_malloc_fn
   */
   void *SUMA_calloc_fn (const char *CF, size_t nmemb, size_t size) {
      void *ptr;
      static char FuncName[]={"SUMA_calloc_fn"};

      #if SUMA_MEMTRACE_FLAG
         if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
      #endif

      /* The allocation */
      ptr = calloc (nmemb, size);

      /* the block below is also used in SUMA_allocate2D */
      #if SUMA_MEMTRACE_FLAG
         if (SUMAg_CF->MemTrace) {
            ++SUMAg_CF->Mem->N_alloc;
            if (SUMAg_CF->Mem->N_MaxPointers <= SUMAg_CF->Mem->N_alloc) {
               /* must reallocate */
               /* SUMA_ShowMemTrace (SUMAg_CF->Mem, NULL);*/
               SUMAg_CF->Mem->N_MaxPointers += SUMA_MEMTRACE_BLOCK;

               SUMAg_CF->Mem->Pointers = (void **)realloc (SUMAg_CF->Mem->Pointers, sizeof(void*) * SUMAg_CF->Mem->N_MaxPointers);
               SUMAg_CF->Mem->Size  = (int *)realloc ((void *)SUMAg_CF->Mem->Size, sizeof(int) * SUMAg_CF->Mem->N_MaxPointers);
               if (!SUMAg_CF->Mem->Pointers || !SUMAg_CF->Mem->Pointers) {
                  fprintf (SUMA_STDERR, "Error %s: Failed to reallocate.\nTurning off memory tracing.\n", \
                     FuncName);
                  /* free up allocated space, clean up pointers, turn off memory tracing DO NOT USE SUMA_free here*/
                  if (SUMAg_CF->Mem->Pointers) free(SUMAg_CF->Mem->Pointers); SUMAg_CF->Mem->Pointers = NULL;
                  if (SUMAg_CF->Mem->Size) free(SUMAg_CF->Mem->Size); SUMAg_CF->Mem->Size = NULL;
                  SUMAg_CF->MemTrace = 0;
                  SUMAg_CF->Mem->N_alloc = 0;
                  SUMAg_CF->Mem->N_MaxPointers =0;
                  SUMA_RETURN(ptr);
               }
            }
            SUMAg_CF->Mem->Pointers[SUMAg_CF->Mem->N_alloc-1] = ptr;
            SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc-1] = nmemb * size;
         }
         SUMA_RETURN(ptr);
      #else
         return(ptr);
      #endif

   }

   /*!
      ptr = SUMA_free(const char *CF, ptr);

      \param CF (const char *) name of calling function
      \param ptr (void *)
      \return NULL 

      This function is the complement of SUMA_malloc_fn and SUMA_calloc_fn, it keeps track of freed memory.
      Its usage is slightly different from that of C's free function in that the function always returns
      a NULL so that one could with one function call free a pointer and set it to NULL.

      Even if you are not tracking memory usage, this function checks that ptr is !NULL before freeing it. 
      If you are doing massive amounts of freeing you may not want to use this function.

   */
   void* SUMA_free_fn(const char *CF, void *ptr)
   {
      static char FuncName[]={"SUMA_free_fn"};
      int i;

      #if SUMA_MEMTRACE_FLAG
         if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
      #endif


      /* This block is also used in SUMA_free2D */
      #if SUMA_MEMTRACE_FLAG

         if (SUMAg_CF->MemTrace && ptr) {
            SUMA_Boolean Found = NOPE;
            for (i=0; i < SUMAg_CF->Mem->N_alloc && !Found; ++i) {
               if (SUMAg_CF->Mem->Pointers[i] == ptr) {
                  SUMAg_CF->Mem->Pointers[i] = SUMAg_CF->Mem->Pointers[SUMAg_CF->Mem->N_alloc-1];
                  SUMAg_CF->Mem->Size[i] = SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc-1];
                  SUMAg_CF->Mem->Pointers[SUMAg_CF->Mem->N_alloc-1] = NULL;
                  SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc-1] = 0;
                  --SUMAg_CF->Mem->N_alloc;
                  Found = YUP;
               }
            }
            if (!Found) {
              fprintf (SUMA_STDERR, "Error %s: Pointer %p not found in Mem struct. \n\tOffending Calling Function is %s\n", FuncName,ptr, CF); 
            }
         }

         if (ptr) free (ptr); 
         SUMA_RETURN(NULL);
      #else
         if (ptr) free (ptr); 
         return (NULL);
      #endif
   }
#else
   /* classic vanilla */
   void *SUMA_malloc_fn (const char *CF, size_t size)
   {
      return (malloc(size));
   }
   void* SUMA_free_fn(const char *CF, void *ptr)
   {
      free(ptr);
      return (NULL);
   }
   void *SUMA_calloc_fn (const char *CF, size_t nmemb, size_t size) 
   {
      return (calloc(nmemb, size));
   }
   void *SUMA_realloc_fn (const char *CF, void *ptr, size_t size)
   {
      return (realloc(ptr, size));
   }
#endif

SUMA_MEMTRACE_STRUCT * SUMA_Create_MemTrace (void) {
   static char FuncName[]={"SUMA_Create_MemTrace"};
   SUMA_MEMTRACE_STRUCT *Mem;
 
   /* you cannot use SUMAg_CF here because the function that allocates for SUMAg_CF calls that one */
   
   /* DO NOT USE SUMA_malloc function here ! */
   Mem = malloc (sizeof(SUMA_MEMTRACE_STRUCT));
   /* allocate for the Pointers and Size vectors */
   Mem->Pointers = (void **)calloc(SUMA_MEMTRACE_BLOCK, sizeof(void *));
   
   Mem->Size = (int *)calloc(SUMA_MEMTRACE_BLOCK, sizeof(int));
   Mem->N_MaxPointers = SUMA_MEMTRACE_BLOCK;
   Mem->N_alloc = 0;
   
   if (!Mem->Pointers || !Mem->Size) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate.\n", FuncName);
      return (NULL);
   }
   return(Mem);
}


void SUMA_ShowMemTrace (SUMA_MEMTRACE_STRUCT *Mem, FILE *Out) 
{
   static char FuncName[]={"SUMA_ShowMemTrace"};
   int i, *isort = NULL, *mem_sz_sort = NULL, Tot;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!Out) Out = SUMA_STDERR;
   if (!Mem) {
      fprintf (Out,"\nNull struct. Nothing to show.\n");
      SUMA_RETURNe;
   }
   
   fprintf (Out,"\nShowing SUMA_MEMTRACE_STRUCT: %p\n", Mem);    
   fprintf (Out,"->N_alloc: %d allocated elements.\n", Mem->N_alloc);
   fprintf (Out,"->N_MaxPointers: %d\n", Mem->N_MaxPointers);
   
   /* sort the pointers by their sizes */
   /* make a copy of Mem->Size to keep it from getting modified then sort it.
   Do not use SUMA_calloc here because that'll increment N_alloc after space is allocated! */
   mem_sz_sort = (int *)calloc(Mem->N_alloc, sizeof(int));
   if (!mem_sz_sort) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate for mem_sz_sort.\n", FuncName);
      SUMA_RETURNe;
   }
   
   #if 1
   for (i=0; i < Mem->N_alloc; ++i) mem_sz_sort[i] = Mem->Size[i];
   isort = SUMA_z_dqsort_nsc (mem_sz_sort, Mem->N_alloc); /* this version of SUMA_z_dqsort does not use SUMA_calloc for allocation thus keeping the memory trace unchanged */
   
   Tot = 0;
   for (i=0; i < Mem->N_alloc; ++i) {
      fprintf (Out,"->[%d]\tPointer %p\t %d bytes.\n", i, Mem->Pointers[isort[i]], Mem->Size[isort[i]]);
      Tot += Mem->Size[isort[i]];
   }
   #else
     
   Tot = 0;
   for (i=0; i < Mem->N_alloc; ++i) {
      fprintf (Out,"->[%d]\tPointer %p\t %d bytes.\n", i, Mem->Pointers[i], Mem->Size[i]);
      Tot += Mem->Size[i];
   }
   #endif
   
   fprintf (Out,"Total Memory Allocated %f Mbytes.\n", (float)Tot/1000000.0);
   if (mem_sz_sort) free(mem_sz_sort); /* mem_sz_sort should not be freed with SUMA_free */
   if (isort) free(isort); /* isort should not be freed with SUMA_free */
   SUMA_RETURNe;
   
}

SUMA_Boolean SUMA_Free_MemTrace (SUMA_MEMTRACE_STRUCT * Mem) {
   static char FuncName[]={"SUMA_Free_MemTrace"};
         
   /* DO NOT USE SUMA_free function here ! */
   if (Mem->Pointers) free (Mem->Pointers);
   if (Mem->Size) free(Mem->Size);
   if (Mem) free (Mem);
   
   return(YUP);
}

/*! Taken from filexists 
returns 1 if file can be read/found
*/
int SUMA_filexists (char *f_name)
{/*SUMA_filexists*/
    FILE *outfile;
    static char FuncName[]={"SUMA_filexists"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

    outfile = fopen (f_name,"r");
    if (outfile == NULL) {
       SUMA_RETURN(0); 
   }
    else {
       fclose (outfile); 
   }
    
   SUMA_RETURN(1);
       
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
   static char FuncName[]={"SUMA_Read_dfile"};
   FILE*internal_file;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   SUMA_RETURN (cnt);                            
}
int SUMA_Read_file (float *x,char *f_name,int n_points)
   
{ /* pass a 0 to n_points if you want to read till EOF */
   int cnt=0,ex,dec;
   FILE*internal_file;
   static char FuncName[]={"SUMA_Read_file"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   internal_file = fopen (f_name,"r");
   if (internal_file == NULL) {
                          fprintf (SUMA_STDERR,"%s: \aCould not open %s \n",FuncName, f_name);
                          SUMA_RETURN (-1);
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
                  SUMA_RETURN (n_rows);
               }
            ++ic;
         }
      ++ir;
   }

   fclose (internal_file);
   SUMA_RETURN (ir);      
      
}/*SUMA_Read_2Dfile*/

/*! 
   \brief Allocate for irgb structure containing n_el elements in each vector 
   
   \sa SUMA_Free_IRGB
*/
SUMA_IRGB *SUMA_Create_IRGB(int n_el)
{
   SUMA_IRGB *irgb=NULL;
   static char FuncName[]={"SUMA_Create_IRGB"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   irgb = (SUMA_IRGB *)SUMA_malloc(sizeof(SUMA_IRGB));
   
   
   irgb->i = (int *)SUMA_calloc(n_el, sizeof(int));
   irgb->r = (float*)SUMA_calloc(n_el, sizeof(float));
   irgb->g = (float*)SUMA_calloc(n_el, sizeof(float));
   irgb->b = (float*)SUMA_calloc(n_el, sizeof(float));
   irgb->N = n_el;
   if (!irgb->i || !irgb->r || !irgb->g || !irgb->b) {
      SUMA_S_Crit ("Failed to allocate for i, r, g and/or b.");
      if (irgb) SUMA_free(irgb);
      SUMA_RETURN (NULL);
   }
   
   SUMA_RETURN(irgb);
}

/*!
   \brief function to free SUMA_IRGB *
   
   \return NULL
   \sa SUMA_Create_IRGB
   - This function frees all vectors in structure and the structure itself
*/
SUMA_IRGB *SUMA_Free_IRGB(SUMA_IRGB *irgb)
{
   static char FuncName[]={"SUMA_Free_IRGB"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (irgb) {
      if (irgb->i) SUMA_free(irgb->i);
      if (irgb->r) SUMA_free(irgb->r);
      if (irgb->g) SUMA_free(irgb->g);
      if (irgb->b) SUMA_free(irgb->b);
      SUMA_free(irgb);
   }
   
   SUMA_RETURN(NULL);
}
/*!
   \brief Function to read a node color file formatted as:
   i r g b (int float float float)
   
   \param f_name (char *) filename
   \return irgb (SUMA_IRGB *) structure containing irgb data 
   
   \sa SUMA_Create_IRGB
   \sa SUMA_Free_IRGB
*/
SUMA_IRGB *SUMA_Read_IRGB_file (char *f_name)
{
   int i=0, ncol = 0, nrow = 0;
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   SUMA_IRGB *irgb=NULL;
   static char FuncName[]={"SUMA_Read_IRGB_file"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   im = mri_read_1D (f_name);
   
   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(NULL);
   }
   
   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;
   
   if (!ncol) {
      SUMA_SL_Err("Empty file");
      SUMA_RETURN(NULL);
   }
   if (nrow !=  4 ) {
      SUMA_SL_Err("File must have\n"
                  "4 columns.");
      mri_free(im); im = NULL;   /* done with that baby */
      SUMA_RETURN(NULL);
   }
  
   if (!(irgb = SUMA_Create_IRGB(ncol))) {
      fprintf (SUMA_STDERR,"%s: Failed to create irgb.\n",FuncName);
      SUMA_RETURN (NULL);
   }
   
   for (i=0; i < ncol; ++i) {
      irgb->i[i] = (int)far[i];
      irgb->r[i] = far[i+ncol];
      irgb->g[i] = far[i+2*ncol];
      irgb->b[i] = far[i+3*ncol];
   }   
   
   mri_free(im); im = NULL;
   
   SUMA_RETURN (irgb);      
      
}

/*!
 
Purpose : 
   Reads a file of integer numbers, with n_cols values per line
 
Usage : 
   ans = SUMA_Read_2Ddfile (char *f_name, int **x,int n_rows, int n_cols)
 
 
Input paramters : 
   \param   x, (int)** array where the values will be stored.
   \param   f_name, (char)* string holding file name.
   \param   n_rows, (int) number of rows to be read from file. 
   \param   n_cols, (int) number of columns per line.

   \ret Number of rows read (maybe incomplete rows)
*/
int SUMA_Read_2Ddfile (char *f_name, int **x, int n_rows, int n_cols)
{/*SUMA_Read_2Ddfile*/
   int ir, ic, ex;
   FILE*internal_file;
   static char FuncName[]={"SUMA_Read_2Ddfile"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   internal_file = fopen (f_name,"r");
   if (internal_file == NULL) {
      fprintf (SUMA_STDERR,"%s: \aCould not open %s \n",FuncName, f_name);
      SUMA_RETURN (-1);
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
                  SUMA_RETURN(ir);
               }
            ++ic;
         }
      ++ir;
   }

   fclose (internal_file);
   SUMA_RETURN (ir);      
      
}/*SUMA_Read_2Ddfile*/


/*! 
count the number of float values in a file
-1 if the file could not be open
*/ 
int SUMA_float_file_size (char *f_name)
{ 
   int cnt=0,ex;
   float buf;
   static char FuncName[]={"SUMA_float_file_size"};
   FILE*internal_file;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   internal_file = fopen (f_name,"r");
   if (internal_file == NULL) {
                          printf ("\aCould not open %s \n",f_name);
                          SUMA_RETURN (-1);
                          }
   ex = fscanf (internal_file,"%f",&buf);                     
   while (ex != EOF)
   {
     ++cnt;
     ex = fscanf (internal_file,"%f",&buf);
   }


   fclose (internal_file);
   SUMA_RETURN (cnt);                            
}


/*! Taken from SUMA_alloc_problem */
void SUMA_alloc_problem (char *s1)
 
{
   static char FuncName[]={"SUMA_alloc_problem"};
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

            Ziad Saad                  Oct_21_96

This function should not use SUMA_calloc because it can slow things down 
for Nxm arrays where N is very large. 

*************************************************************************/

char **SUMA_allocate2D (int rows,int cols,int element_size)

{
   int i;
   char **A;
   static char FuncName[]={"SUMA_allocate2D"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   #if SUMA_MEMTRACE_FLAG
   if (SUMAg_CF->MemTrace) {
      ++SUMAg_CF->Mem->N_alloc;
      if (SUMAg_CF->Mem->N_MaxPointers <= SUMAg_CF->Mem->N_alloc) {
         /* must reallocate */
         /* SUMA_ShowMemTrace (SUMAg_CF->Mem, NULL);*/
         SUMAg_CF->Mem->N_MaxPointers += SUMA_MEMTRACE_BLOCK;

         SUMAg_CF->Mem->Pointers = (void **)realloc (SUMAg_CF->Mem->Pointers, sizeof(void*) * SUMAg_CF->Mem->N_MaxPointers);
         SUMAg_CF->Mem->Size  = (int *)realloc ((void *)SUMAg_CF->Mem->Size, sizeof(int) * SUMAg_CF->Mem->N_MaxPointers);
         if (!SUMAg_CF->Mem->Pointers || !SUMAg_CF->Mem->Pointers) {
            fprintf (SUMA_STDERR, "Error %s: Failed to reallocate.\nTurning off memory tracing.\n", \
               FuncName);
            /* free up allocated space, clean up pointers, turn off memory tracing DO NOT USE SUMA_free here*/
            if (SUMAg_CF->Mem->Pointers) free(SUMAg_CF->Mem->Pointers); SUMAg_CF->Mem->Pointers = NULL;
            if (SUMAg_CF->Mem->Size) free(SUMAg_CF->Mem->Size); SUMAg_CF->Mem->Size = NULL;
            SUMAg_CF->MemTrace = 0;
            SUMAg_CF->Mem->N_alloc = 0;
            SUMAg_CF->Mem->N_MaxPointers =0;
         }
      }
      SUMAg_CF->Mem->Pointers[SUMAg_CF->Mem->N_alloc-1] = A;
      SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc-1] = rows * cols * element_size;
   }
   #endif

   SUMA_RETURN(A);
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


            Ziad Saad                  Oct_22_96

This function should not use SUMA_free for freeing the pointers making up the matrix.
Doing so would result in very slow execution times.

*************************************************************************/
void SUMA_free2D(char **a,int rows)
{
   int i;
   static char FuncName[]={"SUMA_free2D"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);


      #if SUMA_MEMTRACE_FLAG
         if (SUMAg_CF->MemTrace && a) {
            SUMA_Boolean Found = NOPE;
            for (i=0; i < SUMAg_CF->Mem->N_alloc && !Found; ++i) {
               if (SUMAg_CF->Mem->Pointers[i] == a) {
                  SUMAg_CF->Mem->Pointers[i] = SUMAg_CF->Mem->Pointers[SUMAg_CF->Mem->N_alloc-1];
                  SUMAg_CF->Mem->Size[i] = SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc-1];
                  SUMAg_CF->Mem->Pointers[SUMAg_CF->Mem->N_alloc-1] = NULL;
                  SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc-1] = 0;
                  --SUMAg_CF->Mem->N_alloc;
                  Found = YUP;
               }
            }
            if (!Found) {
              fprintf (SUMA_STDERR, "Error %s: Pointer %p not found in Mem struct. \n", FuncName,a); 
            }
         }
      #endif

   /* free each row of data */
   for(i = 0 ; i < rows ; i++) free(a[i]);

   /* free each row pointer */
   free((char *)a);
   a = NULL;           /* set to null for error */

   SUMA_RETURNe;
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
    static char FuncName[]={"SUMA_error_message"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   printf ("\n\n\a\33[1mError: \33[0m%s\n",s2);
   printf ("\33[1mError origin:\33[0m %s\n\n",s1);
   if (ext == 1)
      {
        printf ("Exiting Program ..\n\n");
         exit (0);
      }
      else SUMA_RETURNe;
   
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
   static char FuncName[]={"SUMA_iswordin"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (sbig == NULL && ssub == NULL) SUMA_RETURN (-2);
   if (sbig == NULL || ssub == NULL) SUMA_RETURN (-1);

   if (strlen(sbig) < strlen(ssub))
       SUMA_RETURN (0);

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

   if (j == strlen (ssub)) {
      SUMA_RETURN (1);
   }
   else {
      SUMA_RETURN (0);
   }

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
   static char FuncName[]={"SUMA_disp_dmat"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   static char FuncName[]={"SUMA_disp_mat"};
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

/*!**
 
File : SUMA_MiscFunc.c
Author : Ziad Saad
Date : Tue Nov 17 13:19:26 CST 1998, modified Tue Aug 20 11:11:29 EDT 2002
 
Purpose : 
   Displays on the terminal a 2D float matrix stored in a vector
 
 
Usage : 
       SUMA_disp_vecmat (float *v,int nr, int nc, int SpcOpt )
 
 
Input paramters : 
    v (float *) (nr x nc) vector containing the 2D matrix to display
   nr (int) the number of rows in v
   nc (int) the number of columns
   SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
   
 
 
*/ 
void SUMA_disp_vecmat (float *v,int nr, int nc , int SpcOpt)
{/*SUMA_disp_vecmat*/
   char spc [40]; 
    int i,j;
   static char FuncName[]={"SUMA_disp_vecmat"};
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
               fprintf (SUMA_STDOUT, "%4.2f%s",v[i*nc+j],spc);
         fprintf (SUMA_STDOUT,"\n");
      }
}/*SUMA_disp_vecmat*/


/*!**
 
File : SUMA_MiscFunc.c
Author : Ziad Saad
Date : Tue Nov 17 13:19:26 CST 1998, modified Tue Aug 20 11:11:29 EDT 2002
 
Purpose : 
   Displays on the terminal a 2D int matrix stored in a 1D vector 
 
 
Usage : 
       SUMA_disp_vecdmat (float *v,int nr, int nc, int SpcOpt )
 
 
Input paramters : 
    v (int *) (nr x nc) vector containing the 2D matrix to display
   nr (int) the number of rows in v
   nc (int) the number of columns
   SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
   
 
 
*/ 
void SUMA_disp_vecdmat (int *v,int nr, int nc , int SpcOpt)
{/*SUMA_disp_vecdmat*/
   char spc [40]; 
    int i,j;
   static char FuncName[]={"SUMA_disp_vectdmat"};
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
               fprintf (SUMA_STDOUT, "%d%s",v[i*nc+j],spc);
         fprintf (SUMA_STDOUT,"\n");
      }
}/*SUMA_disp_vecdmat*/

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
   static char FuncName[]={"SUMA_disp_vect"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   fprintf (SUMA_STDOUT,"\n");
   if ((l-1) == 0)
      fprintf (SUMA_STDOUT,"%f\n",*v);
   else 
   {
   for (i=0;i<l;++i)
                 fprintf (SUMA_STDOUT,"%f\t",v[i]);
   fprintf (SUMA_STDOUT,"\n");
   }
   SUMA_RETURNe;
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
{   int i;
   static char FuncName[]={"SUMA_disp_dvect"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   fprintf (SUMA_STDOUT,"\n");
   if ((l-1) == 0)
      fprintf (SUMA_STDOUT, "%d\n",*v);
   else 
   {
   for (i=0;i<l;++i)
      fprintf (SUMA_STDOUT,"%d\t",v[i]);

   fprintf (SUMA_STDOUT,"\n");
   }
   SUMA_RETURNe;
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
   static char FuncName[]={"SUMA_etime"}; 
   struct  timeval  tn;
   float Time_Fact = 1000000.0;
   float delta_t;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
      
   SUMA_RETURN (delta_t);
   
}/*SUMA_etime*/
   
/*!
   
File : SUMA_MiscFunc.c, from ~Zlib/code/isinsphere.c
Author : Ziad Saad
Date : Fri Nov 20 22:56:31 CST 1998
   
Purpose : 
   determines which nodes lie inside a sphere
   
   
Usage : 
      Ret =  SUMA_isinsphere (NodeList, nr, S_cent , S_rad , BoundIn)
   
Input paramters : 
   NodeList (float * ) : Nx3 vector containing the NodeList of the nodes to consider
   nr  (int )   : that's N, the number of nodes
   S_cent (float *) : a 3x1 vector containing the NodeList coordinates of the center of the sphere
   S_rad  (float ) : the radius of the sphere
   BoundIn (int) : 0/1 set to 0 for exclusive boundary  
   
   
Returns : 
   a structure of the type SUMA_ISINSPHERE with the following fields
   
   .IsIn    (int *) : a pointer to an [nIsIn x 1] vector will contain the indices into the rows of NodeList that 
                     locates the nodes inside the sphere. 
   .nIsIn   (int) : the number of nodes in the sphere
   .d (float *) : a pointer to an [nIsIn x 1]  vector containing the distance of those nodes inside the sphere to the center.
   
   
   
Support : 
   
   
   
Side effects : 
   
   
   
***/
SUMA_ISINSPHERE SUMA_isinsphere (float * NodeList, int nr, float *S_cent , float S_rad , int BoundIn )
{/*SUMA_isinsphere*/
   static char FuncName[]={"SUMA_isinsphere"}; 
   float *t, t0, t1, t2, ta;
   int k, *IsIn, id, ND;
   SUMA_ISINSPHERE IsIn_strct;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ND = 3;
   IsIn_strct.nIsIn = 0;
      
   t = (float *) SUMA_calloc (nr, sizeof(float));
   IsIn = (int *) SUMA_calloc (nr, sizeof(int));
   
   if (!t || !IsIn)
      {
         SUMA_alloc_problem (FuncName);
         SUMA_RETURN (IsIn_strct);
      }
   
   
   if (BoundIn) /* split into two to avoid checking for this condition all the time */
      {
         for (k=0; k < nr; ++k)
            {
               id = ND * k;
               /* Net distance to center */
               t0 = NodeList[id] - S_cent[0];   
               t1 = NodeList[id+1] - S_cent[1];   
               t2 = NodeList[id+2] - S_cent[2];   

               ta = sqrt (t0 * t0 + t1 * t1 + t2 * t2);
               
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
               id = ND * k;
               /* Net distance to center */
               t0 = NodeList[id] - S_cent[0];   
               t1 = NodeList[id+1] - S_cent[1];   
               t2 = NodeList[id+2] - S_cent[2];   

               ta = sqrt (t0 * t0 + t1 * t1 + t2 * t2);
               
               if (ta < S_rad)
                  {
                     IsIn[IsIn_strct.nIsIn] = k;
                     t[IsIn_strct.nIsIn] = ta;
                     ++(IsIn_strct.nIsIn);
                  }
            }
      }
         
   /* get ridd of extra allocation space*/
   IsIn_strct.d = (float *) SUMA_calloc (IsIn_strct.nIsIn, sizeof(float));
   IsIn_strct.IsIn = (int *) SUMA_calloc (IsIn_strct.nIsIn, sizeof(int));
   
   if (!IsIn_strct.d || !IsIn_strct.IsIn )
      {
         IsIn_strct.nIsIn = 0;
         SUMA_alloc_problem(FuncName);
         SUMA_RETURN (IsIn_strct);
      }
   
   SUMA_COPY_VEC (t, IsIn_strct.d, IsIn_strct.nIsIn, float , float);
   SUMA_COPY_VEC (IsIn, IsIn_strct.IsIn , IsIn_strct.nIsIn, int , int);
   
   SUMA_free(t);
   SUMA_free(IsIn);
   
   SUMA_RETURN (IsIn_strct);
   
}/*SUMA_isinsphere*/

/*!**
   
File : SUMA_MiscFunc from ~Zlib/code/ isinbox.c
Author : Ziad Saad
Date : Fri Nov 20 23:52:52 CST 1998
   
Purpose : 
      determines which nodes lie inside a box

   
   
Usage : 
   Ret = SUMA_isinbox (float * XYZ, int nr, S_cent , S_dim ,  BoundIn)
   
   
Input paramters : 
    XYZ (float * ) : Nx3 vector containing the XYZ of the nodes to consider
   nr  (int )   : that's N, the number of nodes
   S_cent (float *) : a 3x1 vector containing the XYZ coordinates of the center of the box
   S_dim  (float *) : a 3x1 containing the size of the box from side 
                    to side along the three dimentions
   BoundIn (int) : 0/1 set to 0 if you want to have exclusive boundary conditions 
   
   
Returns : 
   a structure of the type SUMA_ISINBOX with the following fields
   
   IsIn    (int *) : a pointer to an [nIsIn x 1] vector that will contain indices into the rows of XYZ that 
                     locates the nodes inside the box. 
   d   (float *): The distance between each of the nodes and the center of the box
   nIsIn   (int) : the number of nodes in the box
  
   
Support : 
   
   
   
Side effects : 
   
   
   
***/
SUMA_ISINBOX SUMA_isinbox (float * XYZ, int nr, float *S_cent , float *S_dim , int BoundIn )
{/*SUMA_isinbox*/
   
   static char FuncName[]={"SUMA_isinbox"}; 
   float t0, t1, t2, hdim0, hdim1, hdim2, *d;
   int k , *IsIn, id, ND;
   SUMA_ISINBOX IsIn_strct;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   ND = 3;
   /*
   fprintf(SUMA_STDOUT,"%f %f %f, %f %f %f, %d, %f, %f, %f\n",\
      S_cent[0], S_cent[1], S_cent[2], S_dim[0], S_dim[1], S_dim[2], nr, XYZ[0], XYZ[1], XYZ[2]);
   */
      
   IsIn_strct.nIsIn = 0;   

   hdim0 = S_dim[0]/2;
   hdim1 = S_dim[1]/2;
   hdim2 = S_dim[2]/2;
   
   IsIn = (int *) SUMA_calloc (nr, sizeof(int));
   d = (float *)SUMA_calloc(nr, sizeof(float));
   
   if (!IsIn || !d)
      {
         SUMA_alloc_problem (FuncName);
         SUMA_RETURN (IsIn_strct);
      }

   if (BoundIn) /* split into two to avoid checking for this condition all the time */
      {
         /*fprintf(SUMA_STDERR,"%s: inbound\n", FuncName);*/
         for (k=0; k < nr; ++k)
            {
            /*fprintf(SUMA_STDERR,"%s: inbound %d\n", FuncName, k);*/
            /* relative distance to center */
               id = ND * k;
               t0 = hdim0 - fabs(XYZ[id] - S_cent[0]);   
               t1 = hdim1 - fabs(XYZ[id+1] - S_cent[1]);   
               t2 = hdim2 - fabs(XYZ[id+2] - S_cent[2]);   
               
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
               id = ND * k;
               t0 = hdim0 - fabs(XYZ[id] - S_cent[0]);   
               t1 = hdim1 - fabs(XYZ[id+1] - S_cent[1]);   
               t2 = hdim2 - fabs(XYZ[id+2] - S_cent[2]);   
               
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
      /*fprintf(SUMA_STDERR,"%s: SUMA_realloc\n", FuncName);*/

      /* get ridd of extra allocation space*/
      IsIn_strct.IsIn = (int *) SUMA_calloc (IsIn_strct.nIsIn, sizeof(int));
      IsIn_strct.d = (float *)SUMA_calloc(IsIn_strct.nIsIn, sizeof(float));

      if (!IsIn_strct.IsIn || !IsIn_strct.d)
         {
            IsIn_strct.nIsIn = 0;
            SUMA_alloc_problem(FuncName);
            SUMA_RETURN (IsIn_strct);
         }

      SUMA_COPY_VEC (IsIn, IsIn_strct.IsIn , IsIn_strct.nIsIn, int , int);
      SUMA_COPY_VEC (d, IsIn_strct.d, IsIn_strct.nIsIn, float, float);
   } else {
      /*fprintf(SUMA_STDERR,"%s: NADA\n", FuncName);*/
      IsIn_strct.IsIn = NULL;
      IsIn_strct.d = NULL;
   }
   
   /*fprintf(SUMA_STDERR,"%s: freeing\n", FuncName);*/
   SUMA_free(IsIn);
   SUMA_free(d);
   /*fprintf(SUMA_STDERR,"%s: freed\n", FuncName);*/

   SUMA_RETURN (IsIn_strct) ;

}/*SUMA_isinbox*/

/*!
free SUMA_ISINBOX structure contents. 
Structure pointer is not freed
*/
SUMA_Boolean SUMA_Free_IsInBox (SUMA_ISINBOX *IB)
{
   static char FuncName[]={"SUMA_Free_IsInBox"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (IB == NULL) {
      fprintf (SUMA_STDERR,"Error SUMA_Free_IsInBox: pointer to null cannot be freed\n");
      SUMA_RETURN (NOPE);
   }
   if (IB->IsIn != NULL) SUMA_free(IB->IsIn);
   if (IB->d != NULL) SUMA_free(IB->d);
   IB->nIsIn = 0;
   SUMA_RETURN (YUP);   
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
   static char FuncName[]={"SUMA_Point_At_Distance"}; 
   float bf, **P2, P1orig[3], Uorig[3];
   float m, n, p, q, D, A, B, C;
   int flip, i;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (d == 0) {
      fprintf(SUMA_STDERR,"Error %s: d is 0. Not good, Not good at all.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDOUT,"%s: U %f, %f, %f, P1 %f %f %f, d %f\n", FuncName,\
         U[0], U[1], U[2], P1[0], P1[1], P1[2], d);
   }
         
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
      } else {   /*U[1] = 0; */
         if (U[2] != 0) { /* U[2] != 0 */
            U[0] = U[2]; U[2] = 0;
            bf = P1[0]; P1[0] = P1[2]; P1[2] = bf;
            flip = 2;
         } else { /* U[2] = 0 */
            fprintf(SUMA_STDERR, "Error %s: 0 direction vector.\n", FuncName);
            SUMA_RETURN (NULL);
         }
      }/*U[1] = 0; */
   }/*U[0] = 0; */

   if (LocalHead) fprintf (SUMA_STDERR, "%s: flip = %d\n", FuncName, flip);
      
   if (LocalHead) fprintf (SUMA_STDERR, "%s: U original: %f, %f, %f\n", FuncName, U[0], U[1], U[2]);
   U[1] /= U[0];
   U[2] /= U[0];
   U[0] = 1.0; 
   if (LocalHead) fprintf (SUMA_STDERR, "%s: U normalized: %f, %f, %f\n", FuncName, U[0], U[1], U[2]);
   
   /* Now U is clean, calculate P2 */   
   m = U[1];
   n = U[2];

   q = P1[1] - m*P1[0];
   p = P1[2] - n*P1[0];

   if (LocalHead) fprintf (SUMA_STDERR, "%s: m=%f n=%f, p=%f, q=%f\n", FuncName, m, n, p, q);

   /* Now find P2 */
   A = (1 + n*n + m*m);
   B = -2 * P1[0] + 2 * m * (q - P1[1]) + 2 * n * (p - P1[2]);
   C = P1[0]*P1[0] + (q - P1[1])*(q - P1[1]) + (p - P1[2])*(p - P1[2]) - d*d;

   D = B*B - 4*A*C;
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: A=%f B=%f, C=%f, D=%f\n", FuncName, A, B, C, D);
   
   if (D < 0) {
      fprintf(SUMA_STDERR, "Error %s: Negative Delta.\n", FuncName);
      SUMA_RETURN (NULL);
   }

   P2 = (float **)SUMA_allocate2D(2,3, sizeof(float));
   if (P2 == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Could not allocate for 6 floats! What is this? What is the matter with you?!\n", FuncName);
      SUMA_RETURN (NULL);
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

   if (LocalHead) {
      fprintf(SUMA_STDOUT,"%s: P1 = %f, %f, %f\n  ", \
       FuncName, P1[0], P1[1], P1[2]);
      fprintf(SUMA_STDOUT,"%s: P2 = %f, %f, %f\n    %f, %f, %f\n", \
       FuncName, P2[0][0], P2[0][1], P2[0][2], P2[1][0], P2[1][1], P2[1][2]);
      fprintf(SUMA_STDOUT,"%s: U = %f, %f, %f\n  ", \
       FuncName, U[0], U[1], U[2]);
   }

   
   /* make sure 1st point is along the same direction */
   Uorig[0] = P2[0][0] - P1[0]; /* use Uorig, not needed anymore */
   Uorig[1] = P2[0][1] - P1[1];
   Uorig[2] = P2[0][2] - P1[2];

   SUMA_DOTP_VEC(Uorig, U, bf, 3, float, float)
   if (LocalHead) fprintf(SUMA_STDOUT,"%s: Dot product = %f\n", FuncName, bf);
   if (bf < 0) {
      if (LocalHead) fprintf(SUMA_STDOUT,"%s: Flipping at end...\n", FuncName);
      for (i=0; i< 3; ++i) {
         bf = P2[0][i];
         P2[0][i] = P2[1][i]; P2[1][i] = bf;
      }
   }
   
   if (LocalHead) {
      fprintf(SUMA_STDOUT,"%s: P2 = %f, %f, %f\n    %f, %f, %f\n", \
       FuncName, P2[0][0], P2[0][1], P2[0][2], P2[1][0], P2[1][1], P2[1][2]);
   }
SUMA_RETURN (P2);
   
}/*SUMA_Point_At_Distance*/

/*!

Function: SUMA_Point_To_Line_Distance
Usage : 
Ret = SUMA_Point_To_Line_Distance (float *NodeList, int N_nodes, float *P1, float *P2, float *d2, float *d2min, int *i2min)
   
Calculates the squared distance between the points in NodeList and the line formed by P1-P2  
   
Input paramters : 
\param NodeList (float *) N_nodes x 3 vector containing XYZ of N_nodes nodes 
\param N_nodes (int) Number of nodes in NodeList     
\param P1 (float *) 3x1 vector containing the XYZ of P1
\param P2 (float *) 3x1 vector containing the XYZ of P2
\param d2 (float *) N_nodes x 1 vector containing the squared distance of each node in NodeList to the line P1-P2
       d2 must be pointing to a pre-allocated space
\param d2min (float *) pointer to the smallest squared distance
\param i2min (int *) pointer to the index (into NodeList) of the node with the shortest distance

The squared distance is returned to save on a square root operation which may not be necessary to compute for all nodes
   
Returns : 
\return  Ret (SUMA_Boolean) YUP/NOPE for success/failure

\sa labbook NIH-2, p 37 
 
*/
SUMA_Boolean SUMA_Point_To_Line_Distance (float *NodeList, int N_points, float *P1, float *P2, float *d2, float *d2min, int *i2min)
{
   static char FuncName[]={"SUMA_Point_To_Line_Distance"};
   float U[3], Un, xn, yn, zn, dx, dy, dz;
   int i, id, ND;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   ND = 3;
   if (N_points < 1) {
      fprintf(SUMA_STDERR,"Error %s: N_points is 0.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   /* Calculate normalized unit vector of line formed by P1, P2 */
   U[0] = P2[0] - P1[0];
   U[1] = P2[1] - P1[1];
   U[2] = P2[2] - P1[2];
   Un = sqrt(U[0]*U[0] + U[1]*U[1] + U[2]*U[2]);
   
   if (Un == 0) {
      fprintf(SUMA_STDERR,"Error %s: P1 and P2 are identical.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
   U[0] /= Un;
   U[1] /= Un;
   U[2] /= Un;
   
   /* calculate the distances and keep track of the minimum distance while you're at it */
   
   /*bad practise, only returned pointers are allocated for in functions */
   /*
   d2 = (float *)SUMA_calloc(N_points, sizeof(float)); */
   
   if (d2 == NULL) {
      fprintf(SUMA_STDERR,"Error %s: d2 not allocated for.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
   
   /* do the first point to initialize d2min without an extra if statement */
    i = 0;
    xn = NodeList[0] - P1[0];
    yn = NodeList[1] - P1[1];
    zn = NodeList[2] - P1[2];
    
    dx = (U[1]*zn - yn*U[2]);
    dy = (U[0]*zn - xn*U[2]);
    dz = (U[0]*yn - xn*U[1]);
    
    d2[i] = dx*dx+dy*dy +dz*dz; /* save the sqrt for speed */
    *d2min = d2[i];
    *i2min = i;
    /* Now do the rest */
   for (i=1; i < N_points; ++i) {
      id = ND * i;
      xn = NodeList[id] - P1[0];
      yn = NodeList[id+1] - P1[1];
      zn = NodeList[id+2] - P1[2];

      dx = (U[1]*zn - yn*U[2]);
      dy = (U[0]*zn - xn*U[2]);
      dz = (U[0]*yn - xn*U[1]);

      d2[i] = dx*dx+dy*dy +dz*dz; /* save the sqrt for speed */
      if (d2[i] < *d2min) {
         *d2min = d2[i];
         *i2min = i;
      }
   }
   SUMA_RETURN (YUP);
}

/*!

Function: SUMA_Point_To_Point_Distance
Usage : 
Ret = SUMA_Point_To_Point_Distance (float *NodeList, int N_nodes, float *P1, float *d2, float *d2min, int *i2min)
   
Calculates the squared distance between the points in NodeList and  P1-P2  
   
Input paramters : 
\param NodeList (float *) N_nodes x 3 vector containing XYZ of N_nodes nodes 
\param N_nodes (int) Number of nodes in NodeList     
\param P1 (float *) 3x1 vector containing the XYZ of P1
\param d2 (float *) N_nodes x 1 vector containing the squared distance of each node in NodeList to P1
       d2 must be pointing to a pre-allocated space
\param d2min (float *) pointer to the smallest squared distance
\param i2min (int *) pointer to the index (into NodeList) of the node with the shortest distance

The squared distance is returned to save on a square root operation which may not be necessary to compute for all nodes
   
Returns : 
\return  Ret (SUMA_Boolean) YUP/NOPE for success/failure
 
*/
SUMA_Boolean SUMA_Point_To_Point_Distance (float *NodeList, int N_points, float *P1, float *d2, float *d2min, int *i2min)
{
   static char FuncName[]={"SUMA_Point_To_Point_Distance"};
   float xn, yn, zn;
   int i, id, ND;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ND = 3;
   if (N_points < 1) {
      fprintf(SUMA_STDERR,"Error %s: N_points is 0.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
   
   /* calculate the distances and keep track of the minimum distance while you're at it */
   
   if (d2 == NULL) {
      fprintf(SUMA_STDERR,"Error %s: d2 not allocated for.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   
   
   /* do the first point to initialize d2min without an extra if statement */
    i = 0;
    xn = NodeList[0] - P1[0];
    yn = NodeList[1] - P1[1];
    zn = NodeList[2] - P1[2];
    
    d2[i] = xn*xn + yn*yn + zn*zn; /* save the sqrt for speed */
    *d2min = d2[i];
    *i2min = i;
    /* Now do the rest */
   for (i=1; i < N_points; ++i) {
      id = ND * i;
      xn = NodeList[id] - P1[0];
      yn = NodeList[id+1] - P1[1];
      zn = NodeList[id+2] - P1[2];


       d2[i] = xn*xn + yn*yn + zn*zn; /* save the sqrt for speed */
      if (d2[i] < *d2min) {
         *d2min = d2[i];
         *i2min = i;
      }
   }
   SUMA_RETURN (YUP);
}


/*! Sorting Functions */
#define SUMA_Z_QSORT_structs

/* DO not add debugging in the sorting functions since that might slow them down */

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
    

int SUMA_compare_int (int *a, int *b )
{/*SUMA_compare_int*/
 	if (*a < *b)
		return (-1);
	else if (*a == *b)
		return (0);
	else
		return (1);
	
}/*SUMA_compare_int*/
   
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
   static char FuncName[]={"SUMA_z_qsort"}; 
   int *I, k;
   SUMA_Z_QSORT_FLOAT *Z_Q_fStrct;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate for the structure */
   Z_Q_fStrct = (SUMA_Z_QSORT_FLOAT *) SUMA_calloc(nx, sizeof (SUMA_Z_QSORT_FLOAT));
   I = (int *) SUMA_calloc (nx, sizeof(int));

   if (!Z_Q_fStrct || !I)
      {
         fprintf(SUMA_STDERR,"Error %s: Allocation problem.\n",FuncName);
         SUMA_RETURN (NULL);
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
   SUMA_free(Z_Q_fStrct);

   /* return */
   SUMA_RETURN (I);


}/*SUMA_z_qsort*/


int *SUMA_z_dqsort (int *x , int nx )
{/*SUMA_z_dqsort*/
   static char FuncName[]={"SUMA_z_dqsort"}; 
   int *I, k;
   SUMA_Z_QSORT_INT *Z_Q_iStrct;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate for the structure
 */
   Z_Q_iStrct = (SUMA_Z_QSORT_INT *) SUMA_calloc(nx, sizeof (SUMA_Z_QSORT_INT));
   I = (int *) SUMA_calloc (nx,sizeof(int));

   if (!Z_Q_iStrct || !I)
      {
         fprintf(SUMA_STDERR,"Error %s: Allocation problem.\n",FuncName);
         SUMA_RETURN (NULL);
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
   SUMA_free(Z_Q_iStrct);

   /* return */
   SUMA_RETURN (I);

      
}/*SUMA_z_dqsort*/
   
/*!
   same as SUMA_z_dqsort but does not use SUMA_calloc or SUMA_free functions. 
*/
int *SUMA_z_dqsort_nsc (int *x , int nx )
{/*SUMA_z_dqsort_nsc*/
   static char FuncName[]={"SUMA_z_dqsort_nsc"}; 
   int *I, k;
   SUMA_Z_QSORT_INT *Z_Q_iStrct;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate for the structure
 */
   Z_Q_iStrct = (SUMA_Z_QSORT_INT *) calloc(nx, sizeof (SUMA_Z_QSORT_INT));
   I = (int *) calloc (nx,sizeof(int));

   if (!Z_Q_iStrct || !I)
      {
         fprintf(SUMA_STDERR,"Error %s: Allocation problem.\n",FuncName);
         SUMA_RETURN (NULL);
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
   free(Z_Q_iStrct);

   /* return */
   SUMA_RETURN (I);

      
}/*SUMA_z_dqsort_nsc*/
   
   
/*--------------------- Matrix Sorting functions Begin -----------------------------------*/
   
typedef struct {
      float *x;
      int ncol;
      int Index;
   } SUMA_QSORTROW_FLOAT;

/* DO not add debugging in the sorting functions since that might slow them down */

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
   static char FuncName[]={"SUMA_fqsortrow"}; 
   int k, *I;
   SUMA_QSORTROW_FLOAT *Z_Q_fStrct;
   
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate for the structure */
   Z_Q_fStrct = (SUMA_QSORTROW_FLOAT *) SUMA_calloc(nr, sizeof (SUMA_QSORTROW_FLOAT));
   I = (int *) SUMA_calloc (nr,sizeof(int));
   
   if (!Z_Q_fStrct || !I)
      {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for Z_Q_fStrct || I\n", FuncName);
      SUMA_RETURN (NULL);
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
   SUMA_free(Z_Q_fStrct);

   /* return */
   SUMA_RETURN (I);
   
   
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
   static char FuncName[]={"SUMA_dqsortrow"}; 
   int k,  *I;
   SUMA_QSORTROW_INT *Z_Q_dStrct;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* allocate for the structure */
   Z_Q_dStrct = (SUMA_QSORTROW_INT *) SUMA_calloc(nr, sizeof (SUMA_QSORTROW_INT));
   I = (int *) SUMA_calloc (nr,sizeof(int));
   
   if (!Z_Q_dStrct || !I)
      {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for Z_Q_dStrct || I\n", FuncName);
      SUMA_RETURN (NULL);
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
   SUMA_free(Z_Q_dStrct);

   /* return */
   SUMA_RETURN (I);
   
   
}/*SUMA_dqsortrow*/


/*--------------------- Matrix Sorting functions END ------------------------*/
   
/*
\brief This function is a stripped down version of SUMA_MT_intersect_triangle. It is meant to
work faster when few triangles are to be tested. 

ans = SUMA_MT_isIntersect_Triangle (P0, P1, vert0, vert1, vert2, iP, d, closest_vert);

\param   P0 (float *) 3x1 containing XYZ of point 0
\param   P1 (float *) 3x1 containing XYZ of point 1
\param   vert0 (float *) 3x1 containing XYZ of first node in triangle.
\param   vert1 (float *) 3x1 containing XYZ of second node in triangle.
\param   vert2 (float *) 3x1 containing XYZ of third node in triangle.
\param   iP (float *) 3x1 vector containing XYZ of point of itnersection of P0-P1 with the triangle
\param   d (float *) 3x1 vector containing distance from iP to each of the vertices forming the triangle.
\param   closest_vert (int *) index of node (0, 1 or 2) closest to iP
         d[*closest_vert] is the smallest of d[0], d[1] and d[2]
\return  ans (SUMA_Boolean) YUP (intersects)/NOPE (does not intersect)
         
         NOTE: iP, d and closest_vert are not touched by this function if P0-P1 does not
         intersect the triangle.
         NOTE: If you do not care for iP, d and closest_vert, pass NULL, NULL, NULL as their pointers

   
   \sa SUMA_MT_intersect_triangle
*/

SUMA_Boolean SUMA_MT_isIntersect_Triangle (float *P0, float *P1, float *vert0, float *vert1, float *vert2, float *iP, float *d, int *closest_vert)
{  static char FuncName[]={"SUMA_MT_isIntersect_Triangle"};
   double edge1[3], edge2[3], tvec[3], pvec[3], qvec[3];
   double det,inv_det, u, v, t;
   double dir[3], dirn, orig[3];
   SUMA_Boolean hit = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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

   /* find vectors for two edges sharing vert0 */
   SUMA_MT_SUB(edge1, vert1, vert0);
   SUMA_MT_SUB(edge2, vert2, vert0);

   /* begin calculating determinant - also used to calculate U parameter */
   SUMA_MT_CROSS(pvec, dir, edge2);

   /* if determinant is near zero, ray lies in plane of triangle */
   det = SUMA_MT_DOT(edge1, pvec);
   
   hit = NOPE;
   
      if (det > -SUMA_EPSILON && det < SUMA_EPSILON) {
         /* no hit, will return below */
         hit = NOPE;
      } else {
         inv_det = 1.0 / det;

         /* calculate distance from vert0 to ray origin */
         SUMA_MT_SUB(tvec, orig, vert0);

         /* calculate U parameter and test bounds */
         u = SUMA_MT_DOT(tvec, pvec) * inv_det;
         if (u < 0.0 || u > 1.0) {
            /* no hit, will return below */
            hit = NOPE;
         } else {
            /* prepare to test V parameter */
            SUMA_MT_CROSS(qvec, tvec, edge1);

            /* calculate V parameter and test bounds */
            v = SUMA_MT_DOT(dir, qvec) * inv_det;
            if (v < 0.0 || u + v > 1.0) {
                /* no hit, will return below */
                hit = NOPE;
            } else {
               hit = YUP;
               
               if (iP) {
                  /* calculate t, ray intersects triangle */
                  t = SUMA_MT_DOT(edge2, qvec) * inv_det;         

                  /* calculate the location of the intersection (iP) in XYZ coords */
                  iP[0] = vert0[0] + u * (vert1[0] - vert0[0] ) + v * (vert2[0] - vert0[0] );
                  iP[1] = vert0[1] + u * (vert1[1] - vert0[1] ) + v * (vert2[1] - vert0[1] );
                  iP[2] = vert0[2] + u * (vert1[2] - vert0[2] ) + v * (vert2[2] - vert0[2] );
                  
                  if (d) {
                     /* find out which node is closest to P */
                     d[0] = (vert0[0] - iP[0])*(vert0[0] - iP[0]) + (vert0[1] - iP[1])*(vert0[1] - iP[1]) + (vert0[2] - iP[2])*(vert0[2] - iP[2]);
                     *closest_vert = 0;
                     d[1] = (vert1[0] - iP[0])*(vert1[0] - iP[0]) + (vert1[1] - iP[1])*(vert1[1] - iP[1]) + (vert1[2] - iP[2])*(vert1[2] - iP[2]);
                     if (d[1] < d[*closest_vert]) {
                        *closest_vert = 1;
                     }
                     d[2] = (vert2[0] - iP[0])*(vert2[0] - iP[0]) + (vert2[1] - iP[1])*(vert2[1] - iP[1]) + (vert2[2] - iP[2])*(vert2[2] - iP[2]);
                     if (d[2] < d[*closest_vert]) {
                        *closest_vert = 2;
                     }
                     d[0] = (float)sqrt((double)d[0]);
                     d[1] = (float)sqrt((double)d[1]);
                     d[2] = (float)sqrt((double)d[2]);
                  }
               }

            }
         }
      }
   
   SUMA_RETURN (hit);
}

/*!

SUMA_MT_INTERSECT_TRIANGLE *
SUMA_MT_intersect_triangle(float *P0, float *P1, float *NodeList, int N_Node, int *FaceSetList, int N_FaceSet, SUMA_MT_INTERSECT_TRIANGLE *prevMTI)

\param   P0 (float *) 3x1 containing XYZ of point 0
\param   P1 (float *) 3x1 containing XYZ of point 1
\param   NodeList (float *) N_Node x 3 vector containing the XYZ of nodes making up FaceSetList
\param   N_Node (int) number of nodes in NodeList
\param   FaceSetList (int *) N_FaceSet x 3 with each triplet representing a triangle. Triangles are defined
         by their indices into NodeList 
\param   N_FaceSet (int) number of triangles in FaceSetList
\param   PrevMTI (SUMA_MT_INTERSECT_TRIANGLE *) To keep the function from reallocating for MTI each time you call it, you can pass the previous MTI
         structure to the next call. If the number of facesets is the same as in the previous call and PrevMTI is not NULL then MTI is not reallocated for.
         If PrevMTI is not null and the last N_FaceSet was different from the current, PrevMTI is freed and a new one is returned. This change appears to 
         save about 18% of the function's execution time. Be careful not to free PrevMTI without setting it to NULL and then send it to SUMA_MT_intersect_triangle.

\ret   MTI (SUMA_MT_INTERSECT_TRIANGLE *) pointer to structure containing 
      isHit (SUMA_Boolean *) N_FaceSet x 1 vector. isHit[i] = YUP --> FaceSet i is pierced by ray P0-->P1
      t (float *) signed distance to the plane in which the triangle lies
      u & v(float *) location withing the triangle of the intersection point

\sa Algorithm from:Moller & Trumbore 97
   Tomas Möller and Ben Trumbore. Fast, minimum storage ray-triangle intersection. 
   Journal of graphics tools, 2(1):21-28, 1997

NOTE: 
Tue Jan  7 15:07:05 EST 2003 Shruti noted that problems occured when a ray intersected a node. 
She is correct, if a ray intersects a node, it may or may not be detected and the results are undetermined. 
This is only expected to happen with synthesized data and checking for such situations will slow the function down. 
If you must use such data, I recommend you add a tiny bit of noise to the vertex coordinates
or to your normals. 

*/ 
 
SUMA_MT_INTERSECT_TRIANGLE *
SUMA_MT_intersect_triangle(float *P0, float *P1, float *NodeList, int N_Node, int *FaceSetList, int N_FaceSet, SUMA_MT_INTERSECT_TRIANGLE *PrevMTI)
{
   static char FuncName[]={"SUMA_MT_intersect_triangle"};
   double edge1[3], edge2[3], tvec[3], pvec[3], qvec[3];
   double det,inv_det;
   int iface, ND, id, NP, ip;
   double vert0[3],vert1[3], vert2[3], dir[3], dirn, orig[3];
   float tmin, tmax, dii, disttest;
   static SUMA_MT_INTERSECT_TRIANGLE *MTI = NULL;
   static int N_FaceSet_Previous = 0, entry = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   tmin = 10000000.0;
   tmax = 0.0;
   
   if (!PrevMTI) { /* nothing preallocated */
      entry = 0;
      if (LocalHead) fprintf(SUMA_STDERR,"%s: First entry or nothing pre-allocated.\n", FuncName);
   } else { /* returning a used MTI, check number of facesets */
      if (N_FaceSet_Previous != N_FaceSet) { /* must reallocate */
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Reallocating for MTI, a change in number of FaceSets.\n", FuncName);
         /* free current MTI */
         PrevMTI = SUMA_Free_MT_intersect_triangle (PrevMTI);
         entry = 0;
      }else if (LocalHead) fprintf(SUMA_STDERR,"%s: Reusing.\n", FuncName);
   }
   
   if (!entry) {
      MTI = (SUMA_MT_INTERSECT_TRIANGLE *)SUMA_malloc(sizeof(SUMA_MT_INTERSECT_TRIANGLE));
      if (MTI == NULL) {
         fprintf(SUMA_STDERR,"Error %s: Failed to allocate for MTI\n", FuncName);
         SUMA_RETURN (NULL);
      }
      MTI->t = NULL;
      MTI->u = NULL;
      MTI->v = NULL;
      MTI->isHit = NULL;
   } else {
      MTI = PrevMTI;
   }

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
   
   if (!entry) {
      MTI->isHit = (SUMA_Boolean *)SUMA_malloc(N_FaceSet*sizeof(SUMA_Boolean));
      MTI->t = (float *)SUMA_calloc(N_FaceSet, sizeof(float));
      MTI->u = (float *)SUMA_calloc(N_FaceSet, sizeof(float));
      MTI->v = (float *)SUMA_calloc(N_FaceSet, sizeof(float));
   
      if (MTI->isHit == NULL || MTI->t == NULL || MTI->u == NULL || MTI->v == NULL) {
         fprintf(SUMA_STDERR,"Error : Failed to allocate for MTI->isHit | MTI->t | MTI->u | MTI->v\n");
         SUMA_RETURN (NULL);
      }
   }
   
   MTI->N_hits = 0;
   ND = 3;
   NP = 3;
   for (iface= 0; iface < N_FaceSet; ++iface) {/* iface */
      /* set up the coordinates in a humane nomenclature */
      ip = NP * iface;
      id = ND * FaceSetList[ip];
      vert0[0] = (double)NodeList[id];
       vert0[1] = (double)NodeList[id+1];
      vert0[2] = (double)NodeList[id+2];
      
      id = ND * FaceSetList[ip+1];
      vert1[0] = (double)NodeList[id];
       vert1[1] = (double)NodeList[id+1];
      vert1[2] = (double)NodeList[id+2];
      
      id = ND * FaceSetList[ip+2];
      vert2[0] = (double)NodeList[id];
       vert2[1] = (double)NodeList[id+1];
      vert2[2] = (double)NodeList[id+2];
      
      
      /* find vectors for two edges sharing vert0 */
      SUMA_MT_SUB(edge1, vert1, vert0);
      SUMA_MT_SUB(edge2, vert2, vert0);

      /* begin calculating determinant - also used to calculate U parameter */
      SUMA_MT_CROSS(pvec, dir, edge2);

      /* if determinant is near zero, ray lies in plane of triangle */
      det = SUMA_MT_DOT(edge1, pvec);

   #ifdef SUMA_MT_TEST_CULL           /* define TEST_CULL if culling is desired */
      if (det < SUMA_EPSILON)
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
               if (MTI->t[iface] < 0) disttest = -MTI->t[iface];
                  else disttest = MTI->t[iface];
                   
               if (disttest < tmin) {
                  tmin = disttest;
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
                  MTI->d = (float)sqrt((double)MTI->d);
               }
               if (disttest > tmax) {
                  tmax = disttest;
                  MTI->ifacemax = iface;
               }
            }
         }
      }
   #else                    /* the non-culling branch */
      if (det > -SUMA_EPSILON && det < SUMA_EPSILON)
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
               if (MTI->t[iface] < 0) disttest = -MTI->t[iface];
                  else  disttest = MTI->t[iface];
               
               if (disttest < tmin) {
                  tmin = disttest;
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
                  MTI->d = (float)sqrt((double)MTI->d);
                  ip = NP * iface + MTI->inodeminlocal;
                  MTI->inodemin = FaceSetList[ip];
               }
               if (disttest > tmax) {
                  tmax = disttest;
                  MTI->ifacemax = iface;
               }
            }
         }
      }
   #endif
   }/*iface */
   MTI->N_el = N_FaceSet;
   
   ++entry;
   N_FaceSet_Previous = N_FaceSet;

   SUMA_RETURN (MTI);
}

/*!
Show contents of SUMA_MT_INTERSECT_TRIANGLE structure

*/
SUMA_Boolean SUMA_Show_MT_intersect_triangle(SUMA_MT_INTERSECT_TRIANGLE *MTI, FILE *Out)
{
   static char FuncName[]={"SUMA_Show_MT_intersect_triangle"};
   int MaxShow = 5, i,j;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (Out == NULL) Out = stdout;
      
   if (MTI == NULL) {
      fprintf (Out, "NULL Surface Object Pointer\n");
      SUMA_RETURN(NOPE);
   }
   
   fprintf (Out,"\n---------------------------------\n");
   if (!MTI->N_el) {
      fprintf (Out,"Zero elements in structure\n");
      SUMA_RETURN (YUP);
   }
   
   if (MTI->isHit == NULL) {
      fprintf (SUMA_STDERR,"Error SUMA_Show_MT_intersect_triangle: isHit is NULL\n\n");
      SUMA_RETURN (NOPE);
   }
   else {
      if (MaxShow > MTI->N_el) MaxShow = MTI->N_el; 
      fprintf (Out, "Intersection results (showing first %d out of %d elements):\n", MaxShow, MTI->N_el);
      for (i=0; i < MaxShow; ++i)   {
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
   SUMA_RETURN (YUP);
}
/*!
\brief free structure SUMA_MT_INTERSECT_TRIANGLE, returns NULL so you should use it as such:
MTI = SUMA_Free_MT_intersect_triangle (MTI);

\sa SUMA_MT_intersect_triangle to find out why it is important to set MTI to NULL after freeing it
*/
void * SUMA_Free_MT_intersect_triangle(SUMA_MT_INTERSECT_TRIANGLE *MTI)
{
   static char FuncName[]={"SUMA_Free_MT_intersect_triangle"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (MTI->t) SUMA_free(MTI->t);
   if (MTI->u) SUMA_free(MTI->u);
   if (MTI->v) SUMA_free(MTI->v);
   if (MTI->isHit) SUMA_free(MTI->isHit);
   if (MTI) SUMA_free(MTI);
   SUMA_RETURN(NULL);
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
   char FuncName[]={"SUMA_FromToRotation"};
   float v[3], vn;
   float e, h, f;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /*normalize both vectors */
   vn = sqrt(v0[0]*v0[0] + v0[1]*v0[1] + v0[2]*v0[2]);
   if (vn == 0.0) {
      fprintf(SUMA_STDERR,"Error %s: v0 is null.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   v0[0] /= vn;
   v0[1] /= vn;
   v0[2] /= vn;

   vn = sqrt(v1[0]*v1[0] + v1[1]*v1[1] + v1[2]*v1[2]);
   if (vn == 0.0) {
      fprintf(SUMA_STDERR,"Error %s: v1 is null.\n",FuncName);
      SUMA_RETURN (NOPE);
   }
   v1[0] /= vn;
   v1[1] /= vn;
   v1[2] /= vn;

   SUMA_MT_CROSS(v, v0, v1);
   e = SUMA_MT_DOT(v0, v1);
   f = (e < 0)? -e:e;
   if (f > 1.0 - SUMA_EPSILON)     /* "v0" and "v1"-vector almost parallel */
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
   SUMA_RETURN (YUP);
}

/*
From Advanced Animation and Rendering Techniques, by Alan Watt & Mark Watt
Addison & Wesley, 1998, pp 363-364
SUMA_Boolean   SUMA_mattoquat (float **mat, float *q)

transforms a rotation matrix into a quaternion
\param mat (float **) 4x4 rotation matrix 
\param q (float *) 4x1 vector containing the quaternion computed from mat

\ret YUP/NOPE

\sa SUMA_FromToRotation
*/
SUMA_Boolean   SUMA_mattoquat (float **mat, float *q)
{
   double tr, s;
   int i,j,k, nxt[3] = {1, 2, 0};
   static char FuncName[]={"SUMA_mattoquat"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   SUMA_RETURN (YUP);
}

/*------------------------- Triangle Consistency Functions BEGIN --------------------------------------- */
typedef enum {SUMA_NO_NEIGHB, SUMA_NO_MORE_TO_VISIT, SUMA_VISITED_ALL, SUMA_BAD_SEED} SUMA_TAKE_A_HIKE;

/*!
   \brief This function determines which triangle, if any is formed by the specified nodes
   Tri = SUMA_wichTri (EL, n1, n2, n3);
   \param EL (SUMA_EDGE_LIST *) structure to edge list
   \param n1 (int) first node 
   \param n2 (int) second node
   \param n3 (int) third node
   \return Tri index of triangle containing n1, n2 and n3
         -1 if no such triangle was found 

*/
int SUMA_whichTri (SUMA_EDGE_LIST * EL, int n1, int n2, int n3)
{
   static char FuncName[]={"SUMA_whichTri"};
   int IncTri_E1[100], IncTri_E2[100], N_IncTri_E1 = 0, N_IncTri_E2 = 0, i, j, Tri= -1;
   SUMA_Boolean Found = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* find incident triangles to n1-n2 edge */
   if (!SUMA_Get_Incident(n1, n2, EL, IncTri_E1, &N_IncTri_E1)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Get_Incident.\n", FuncName);
      SUMA_RETURN (-1);
   }
   
   /* find incident triangles to n1-n3 edge */
   if (!SUMA_Get_Incident(n1, n3, EL, IncTri_E2, &N_IncTri_E2)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Get_Incident.\n", FuncName);
      SUMA_RETURN (-1);
   }
   
   /* check that we did not go overboard */
   if (N_IncTri_E1 > 99 || N_IncTri_E2 > 99 ) {
      fprintf (SUMA_STDERR,"Error %s: Exceeded preallocated space.\n", FuncName);
      SUMA_RETURN (-1);
   }

   /* find triangle incident to both edges */
   i=0;
   Found = NOPE;
   while (i < N_IncTri_E1 && !Found) {
      j = 0;
      while (j < N_IncTri_E2 && !Found) {
         if (IncTri_E2[j] == IncTri_E1[i]) { 
            Found = YUP;
            Tri = IncTri_E2[j];
         }
         ++j;
      }
      ++i;
   }
   
   if (!Found) SUMA_RETURN (-1);
   
   SUMA_RETURN (Tri);
}

/*! 
   \brief   This function determines how many nodes two triangles share.
   N_cn = SUMA_isTriLinked (T, t, cn);
   \param T (int *) a b c nodes forming the reference triangle 
   \param t (int *) d c b (or whatever combination you choose, c b d for example)
   \param cn (int *) vector of three elements to contain the indices of nodes 
         common to the two triangles when the function returns.
   \return N_cn (int) number of common nodes. Values in cn beyond N_cn - 1 are undefined.
   
*/
int SUMA_isTriLinked (int*T, int *t, int *cn)
{
   static char FuncName[]={"SUMA_isTriLinked"};
   int ic, in;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ic = 0;   /* common node index*/
   in = 0; /* number of node searched in T */
   while (ic < 2 && in < 3) {
      if (t[0] == T[in]) {
         cn[ic] = t[0]; 
         ++ic;
      }else {
         if (t[1] == T[in]) {
            cn[ic] = t[1]; 
            ++ic;
         }else {
            if (t[2] == T[in]) {
               cn[ic] = t[2]; 
               ++ic;
            }
         }
      }
      ++in; /* look for next node */
   }
   
   SUMA_RETURN (ic);
}

/*!
   This function compares the winding of two triangles, determines their consistency
   and corrects it.
   
   \param T (int *) a b c nodes forming the reference triangle 
   \param t (int *) d c b (or whatever combination you choose, c b d for example)
   \return 1: Consistent
       -1: Inconsisten
        0: less than 2 nodes shared
*/
int SUMA_isConsistent (int *T, int *t)
{
   static char FuncName[]={"SUMA_isConsistent"};
   static int ic, in, LOC[2], loc[2], d, D;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ic = 0;   /* common node index*/
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
      SUMA_RETURN (0);
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
      SUMA_RETURN (1);
   }
   
      
   /*fprintf(SUMA_STDERR,"%s: Triangles NOT consistent.\n", FuncName);*/
   in = t[0];
   t[0] = t[2];
   t[2] = in;
   SUMA_RETURN (-1);
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
   static char FuncName[]={"SUMA_Next_Best_Seed"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!entry) { /* entry = 0 */
      for (i=0; i < N_FL; ++i) {
         if (SFFN->N_Neighb[i] == 3) {
            seed = i; ++entry; SUMA_RETURN(seed);
         }
         if (SFFN->N_Neighb[i] == 2) Found2 = i;
         if (SFFN->N_Neighb[i] == 1) Found1 = i;
      }   
            
      if (Found2 > 0) {
         ++entry;
         SUMA_RETURN (Found2);
      }
         
      if (Found1 > 0) {
         ++entry;
         SUMA_RETURN (Found1);
      }
      
      SUMA_RETURN (-1); /* No seeds found */      
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
               seed = i; ++entry; SUMA_RETURN (seed);
            }
            if (N_NotVisNeighb == 1) {
               Found1 = i;
            }
         } /* a candidate */
      }
      if (Found1 > 0) {
         ++entry;
         SUMA_RETURN (Found1);
      }
      SUMA_RETURN (-1); /* No seeds found */   
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
SUMA_TAKE_A_HIKE SUMA_Take_A_Hike (SUMA_FACESET_FIRST_EDGE_NEIGHB *SFFN, int *visited, int *N_visited, int *Consistent, int *FL, int N_FL, int seed)
{
   static char FuncName[]={"SUMA_Take_A_Hike"};
   int NotFound, itry, curface, nxtface, ipcur, ipnxt, NP;
   static int entry=0;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   NP = 3;
   curface = seed;
   if (!visited[curface]) { /* a new visit this should only happen on the first call */
      if (!entry) {
         *N_visited += 1;
         visited[curface] = 1;
         Consistent[curface] = 1;
         /*fprintf (SUMA_STDERR, "%s: visited %d\n", FuncName, curface);*/
      }else {
         fprintf (SUMA_STDERR, "Error %s: You should not send unvisited seeds, except at the very first call.\n", FuncName);
         SUMA_RETURN (SUMA_BAD_SEED);
      }
   }
   if (SFFN->N_Neighb[curface] == 0) {
      SUMA_RETURN (SUMA_NO_NEIGHB);
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
               Consistent[nxtface] = SUMA_isConsistent (&(FL[curface*NP]), &(FL[nxtface*NP]));
               /*fprintf (SUMA_STDERR, "%s: visited %d\n", FuncName, nxtface);*/
               *N_visited = *N_visited+1;
               ++itry;
            } else {
               /* that's a good one to follow */
               Consistent[nxtface] = SUMA_isConsistent (&(FL[curface*NP]), &(FL[nxtface*NP]));
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
         SUMA_RETURN (SUMA_NO_MORE_TO_VISIT);
      }

   }
   
   SUMA_RETURN (SUMA_VISITED_ALL);
}

/*!
   SUMA_Show_Edge_List (SEL, File *Out)
   \param SEL (SUMA_EDGE_LIST *)
   \param Out (FILE *) file pointer or stdout if Out is NULL 
*/
void SUMA_Show_Edge_List (SUMA_EDGE_LIST *EL, FILE *Out)
{
   static char FuncName[]={"SUMA_Show_Edge_List"};
   int i;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (Out == NULL) Out = stdout;
   
   fprintf(Out,"\nEL contents:\n");
   fprintf(Out,"i-\t[EL[i][0] EL[i][1]]\t[ELps[i][0] ELps[i][1] ELps[i][2] ELps[i][3]]\n");
   for (i=0; i < EL->N_EL; ++i) {
      fprintf(Out,"%d-\t[%d %d]\t[%d %d %d %d]\n", 
               i, EL->EL[i][0], EL->EL[i][1], EL->ELps[i][0], EL->ELps[i][1], EL->ELps[i][2], EL->ELps[i][3]);
   
   }
   fprintf(Out,"\nTriLimb contents:\n");
   fprintf(Out,"ti-\t[Edge1 Edge2 Edge3]\n");
   for (i=0; i < EL->N_EL/3; ++i) { 
      fprintf(Out,"t%d-\t[%d %d %d]\n",
         i, EL->Tri_limb[i][0], EL->Tri_limb[i][1],EL->Tri_limb[i][2]);
   }
   
   SUMA_RETURNe;
}

/*!
   SUMA_free_Edge_List (SEL)
   \param SEL (SUMA_EDGE_LIST *)
   
*/
void SUMA_free_Edge_List (SUMA_EDGE_LIST *SEL)
{
   static char FuncName[]={"SUMA_free_Edge_List"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (SEL->EL) SUMA_free2D((char **)SEL->EL, SEL->N_EL);
   if (SEL->ELloc) SUMA_free(SEL->ELloc);
   if (SEL->ELps) SUMA_free2D((char **)SEL->ELps, SEL->N_EL);
   if (SEL->Tri_limb) SUMA_free2D((char **)SEL->Tri_limb, SEL->N_EL/3);
   if (SEL) SUMA_free(SEL);
   SUMA_RETURNe;
}

/*! 
   ans = SUMA_Make_Edge_List (FL, N_FL, N_Node, NodeList);
   
   This function creates a list of all the edges making up the FaceSets
   \param FL (int *) FaceSetList vector (was matrix, prior to SUMA 1.2 ( N_FL x 3)
   \param N_FL (int) number of facesets (triangles) in FL
   \param N_Node (int) number of nodes forming the mesh
   \param NodeList (float *) vector containing XYZ of each node. This was added to compute
                             the length of each edge.
   \ret ans (SUMA_EDGE_LIST *) NULL/failure or the following fields
       EL (int **) sorted edge list
       ELps (int **) edge list properties
       Le (float *) length of each edge in EL. Since EL can have multiple edges,
                    which are shared by different triangles, Le would also have
                    duplicate length values. This is tolerated for efficiency of
                    indexing.
      N_EL    (int)  Number of edges
      see SUMA_define.h for more info
      
   to free    ans, use SUMA_free_Edge_List
   
   DO NOT MODIFY WHAT THIS FUNCTION RETURNS without serious thought.
   Complicated functions depend on it.                
*/
SUMA_EDGE_LIST * SUMA_Make_Edge_List (int *FL, int N_FL, int N_Node, float *NodeList)
{
   static char FuncName[]={"SUMA_Make_Edge_List"};
   int i, ie, ip, *isort_EL, **ELp, lu, ht, *iTri_limb, icur, in1, in2;
   float dx, dy, dz;
   SUMA_EDGE_LIST *SEL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate and form the List of edges */
   SEL = (SUMA_EDGE_LIST *) SUMA_malloc(sizeof(SUMA_EDGE_LIST));

   SEL->N_EL = 3 * N_FL;
   SEL->EL = (int **) SUMA_allocate2D (SEL->N_EL, 2, sizeof(int)); /* edge list */
   SEL->ELloc = (int *)SUMA_calloc(N_Node, sizeof(int));
   SEL->Le = (float *) SUMA_calloc (SEL->N_EL, sizeof(float)); /* length of each edge */
   ELp = (int **) SUMA_allocate2D (SEL->N_EL, 2, sizeof(int)); /* edge property list */
                                                         /* 1st column, 1 = is flipped from orientation in triangle, -1 as present in triangle 
                                                              2nd column, index of triangle (FaceSet) that edge is a part of */    
   SEL->ELps = (int **) SUMA_allocate2D (SEL->N_EL, 3, sizeof(int)); /*sorted edge property list */
   
   /*fprintf(SUMA_STDERR, "%s: SEL->NEL %d\n", FuncName, SEL->N_EL/3);*/
   
   SEL->Tri_limb = (int **) SUMA_allocate2D (SEL->N_EL/3, 3, sizeof(int)); 
   iTri_limb = (int *)SUMA_calloc (SEL->N_EL/3,sizeof(int)); 
   
   if (SEL == NULL || SEL->EL == NULL || ELp == NULL || SEL->ELps == NULL || SEL->Tri_limb == NULL || iTri_limb== NULL || SEL->ELloc == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Failed to allocate for EL, ELp.\n", FuncName);
      SUMA_RETURN (NULL);
   }

   /* form the edge list */
   /*fprintf(SUMA_STDERR, "%s: Forming edge list ...\n", FuncName);*/
   for (i=0; i< N_FL; ++i) {/* begin, form edge list */
      /* first edge, 0->1*/
      ie = 3*i;
      ip = 3*i;
      if (FL[ip] > FL[ip+1]) {
         /* flip it, to make sorting easier */
         SEL->EL[ie][0] = FL[ip+1];
         SEL->EL[ie][1] = FL[ip];
         /* store parameters */
         ELp[ie][0] = 1; /* flip happened */
      } else { 
         /* no flip necessary */
         SEL->EL[ie][0] = FL[ip];
         SEL->EL[ie][1] = FL[ip+1];
         ELp[ie][0] = -1; /* NO flip happened */
      }
      ELp[ie][1] = i; /* FaceSetMember */

      /* second edge, 1->2*/
      ie += 1;
      if (FL[ip+1] > FL[ip+2]) {
         /* flip it, to make sorting easier */
         SEL->EL[ie][0] = FL[ip+2];
         SEL->EL[ie][1] = FL[ip+1];
         /* store parameters */
         ELp[ie][0] = 1; /* flip happened */
      } else { 
         /* no flip necessary */
         SEL->EL[ie][0] = FL[ip+1];
         SEL->EL[ie][1] = FL[ip+2];
         ELp[ie][0] = -1; /* NO flip happened */
      }
      ELp[ie][1] = i; /* FaceSetMember */
      
      /* third edge, 2->0*/
      ie += 1;
      if (FL[ip+2] > FL[ip]) {
         /* flip it, to make sorting easier */
         SEL->EL[ie][0] = FL[ip];
         SEL->EL[ie][1] = FL[ip+2];
         /* store parameters */
         ELp[ie][0] = 1; /* flip happened */
      } else { 
         /* no flip necessary */
         SEL->EL[ie][0] = FL[ip+2];
         SEL->EL[ie][1] = FL[ip];
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
   
   if (isort_EL) SUMA_free(isort_EL);
   isort_EL = NULL;
   
   
   #if 0
      fprintf(SUMA_STDERR,"%s: Node1 Node2 | FlipVal Triangle\n", FuncName); 
      for (i=0; i < SEL->N_EL; ++i) {
         fprintf (SUMA_STDERR, "%d %d | %d %d\n", SEL->EL[i][0], SEL->EL[i][1], SEL->ELps[i][0], SEL->ELps[i][1]);
      }
   #endif
   
   /* calculate the length of each edge */
   for (ie=0; ie < SEL->N_EL; ++ie) {
      in1 = 3 * SEL->EL[ie][0]; in2 = 3 * SEL->EL[ie][1];
      dx = (NodeList[in2] - NodeList[in1]);
      dy = (NodeList[in2+1] - NodeList[in1+1]);
      dz = (NodeList[in2+2] - NodeList[in1+2]);
      SEL->Le[ie] = (float) sqrt (  dx * dx + dy * dy + dz * dz );
   }
   
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
   
   fprintf(SUMA_STDERR,"%s: Min/Max number of edge hosting triangles: [%d/%d] \n", FuncName, SEL->min_N_Hosts, SEL->max_N_Hosts);
   if (SEL->min_N_Hosts == 1 || SEL->max_N_Hosts == 1) {
      fprintf(SUMA_STDERR,"Warning %s: You have edges that form a border in the surface.\n", FuncName);
   }
   if (SEL->min_N_Hosts > 2 || SEL->max_N_Hosts > 2) {
      fprintf(SUMA_STDERR,"Warning %s: You have edges that belong to more than two triangles. Bad for analysis assuming surface is a 2-manifold..\n", FuncName);
   }
   
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

   SUMA_RETURN (SEL);
}

/*! 
   \brief finds the index of an edge in EL of an edge formed by nodes n1 n2 and belonging to triangle Tri
   eloc = SUMA_FindEdgeInTri (EL, int n1, int n2, int Tri);
   \param EL (SUMA_EDGE_LIST *) Pointer to edge list structure
   \param n1 (int) index of node 1
   \param n2 (int) index of node 2
   \param Tri (int) index of triangle containing the edge you are looking for
   \return eloc (int) index into EL of edge formed by nodes n1, n2 and belonging to Tri
            -1 if no edge is found.
*/
int SUMA_FindEdgeInTri (SUMA_EDGE_LIST *EL, int n1, int n2, int Tri) 
{
   static char FuncName[]={"SUMA_FindEdgeInTri"};
   int eloc;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* make sure n1 is smallest*/
   if (n2 < n1) {
      eloc = n2; 
      n2 = n1; 
      n1 = eloc;
   }
   
   /* first location of edge starting with n1 */
   eloc = EL->ELloc[n1];
   
   /* from there on, look for first occurence of n2 and Tri for hosting triangle*/
   do {
      if (EL->EL[eloc][1] == n2 && EL->ELps[eloc][1] == Tri) SUMA_RETURN (eloc);
      ++eloc;
   } while (eloc < EL->N_EL && EL->EL[eloc][0] == n1); 
   
   /* not found */
   SUMA_RETURN (-1);
}


/*! 
   \brief finds the first occurence in EL of an edge formed by nodes n1 n2
   eloc = SUMA_FindEdge (EL, int n1, int n2);
   \param EL (SUMA_EDGE_LIST *) Pointer to edge list structure
   \param n1 (int) index of node 1
   \param n2 (int) index of node 2
   \return eloc (int) index into EL of first occurence of edge formed by nodes n1, n2
            -1 if no edge is found.
*/
int SUMA_FindEdge (SUMA_EDGE_LIST *EL, int n1, int n2) 
{
   static char FuncName[]={"SUMA_FindEdge"};
   int eloc;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* make sure n1 is smallest*/
   if (n2 < n1) {
      eloc = n2; 
      n2 = n1; 
      n1 = eloc;
   }
   
   /* first location of edge starting with n1 */
   if ((eloc = EL->ELloc[n1]) < 0) {
      SUMA_S_Err ("Edge location of n1 not found. WEIRD");
      SUMA_RETURN (-1);
   }
   
   /* from there on, look for first occurence of n2 */
   do {
      /* if (LocalHead) fprintf (SUMA_STDERR,"%s: eloc %d, N_EL %d\n", FuncName, eloc, EL->N_EL);*/
      if (EL->EL[eloc][1] == n2) SUMA_RETURN (eloc);
      ++eloc;
   } while (eloc < EL->N_EL && EL->EL[eloc][0] == n1); 
   
   /* not found */
   SUMA_RETURN (-1);
}

/*!
   \brief finds triangles incident to a node 
   ans = SUMA_Get_NodeIncident(n1, SEL, Incident, N_Incident);
   
   \param n1 (int) node 1
   \param SO (SUMA_SurfaceObject *) 
   \param Incident (int *) a pre-allocated vector where incident triangle indices will be stored. MAKE SURE you allocate enough
   \param N_Incident (int *) pointer where the number of incident triangles is stored
   
   \ret ans (SUMA_Boolean) YUP/NOPE
   
   \sa SUMA_Make_Edge_List
   \sa SUMA_Get_Incident
*/
SUMA_Boolean SUMA_Get_NodeIncident(int n1, SUMA_SurfaceObject *SO, int *Incident, int *N_Incident)
{
   static char FuncName[] = {"SUMA_Get_NodeIncident"};
   int i, n3, N_Neighb;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   *N_Incident = 0;
   
   N_Neighb = SO->FN->N_Neighb[n1];
   if (N_Neighb < 3) {
      fprintf (SUMA_STDERR, "Warning %s: Node %d has less than 3 neighbors.\n", FuncName, n1);
      /* nothing found */
      SUMA_RETURN(YUP);
   }
   
   i = 0;
   while ((i < N_Neighb )) { 
      if ( i+1 == N_Neighb) n3 = SO->FN->FirstNeighb[n1][0];
      else n3 = SO->FN->FirstNeighb[n1][i+1];
      if ((Incident[*N_Incident] = SUMA_whichTri (SO->EL, n1, SO->FN->FirstNeighb[n1][i], n3)) < 0) {
         fprintf (SUMA_STDERR, "Error %s: Triangle formed by nodes %d %d %d not found.\n", 
            FuncName, n1, SO->FN->FirstNeighb[n1][i], n3);
         SUMA_RETURN(NOPE);
      }
      ++*N_Incident;
      ++i;
   }

   SUMA_RETURN(YUP);   
}

/*! \brief finds triangles incident to an edge 
   ans = SUMA_Get_Incident( n1,  n2,  SEL, Incident, N_Incident);
   
   \param n1 (int) node 1
   \param n2 (int) node 2
   \param SEL (SUMA_EDGE_LIST *) Edge List structure
   \param Incident (int *) a pre-allocated vector where incident triangle indices will be stored. MAKE SURE you allocate enough
   \param N_Incident (int *) pointer where the number of incident triangles is stored
   
   \ret ans (SUMA_Boolean) YUP/NOPE
   
   \sa SUMA_Make_Edge_List
   \sa SUMA_Get_NodeIncident
*/
SUMA_Boolean SUMA_Get_Incident(int n1, int n2, SUMA_EDGE_LIST *SEL, int *Incident, int *N_Incident)
{
   static char FuncName[] = {"SUMA_Get_Incident"};
   int nt, in1, iseek, m_N_EL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
         SUMA_RETURN (YUP);
      }
      
   }
   if (!*N_Incident) fprintf(SUMA_STDERR,"Warning %s: No Incident FaceSets found!\n", FuncName);
   /*fprintf(SUMA_STDERR,"Leaving %s.\n", FuncName);*/
   SUMA_RETURN(YUP);   
}

/*! 
   frees the dyamically allocated pointer of the type SUMA_FACESET_FIRST_EDGE_NEIGHB 
   SUMA_free_FaceSet_Edge_Neighb (S)
   \param S (SUMA_FACESET_FIRST_EDGE_NEIGHB *)
   
   \sa SUMA_allocate_FaceSet_Edge_Neighb
*/ 
void SUMA_free_FaceSet_Edge_Neighb (SUMA_FACESET_FIRST_EDGE_NEIGHB * S)
{
   static char FuncName[]={"SUMA_free_FaceSet_Edge_Neighb"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (S->FirstNeighb) SUMA_free2D((char **)S->FirstNeighb, S->N_FaceSet);
   if (S->N_Neighb) SUMA_free(S->N_Neighb);
   if (S) SUMA_free(S);
   SUMA_RETURNe;
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SFFN = SUMA_malloc(sizeof(SUMA_FACESET_FIRST_EDGE_NEIGHB));
   if (SFFN == NULL) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate for SFFN.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   SFFN->FirstNeighb = (int **) SUMA_allocate2D(N_FaceSet, SUMA_MAX_FACESET_EDGE_NEIGHB, sizeof(int));
   SFFN->N_Neighb = (int *) SUMA_calloc (N_FaceSet, sizeof(int));
   if (SFFN->FirstNeighb == NULL || SFFN->N_Neighb == NULL) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate for FirstNeighb or N_Neighb.\n", FuncName);
      SUMA_RETURN (NULL);
   } 
   
   SFFN->N_Neighb_max = -1; /* ridiculously low */
   SFFN->N_FaceSet = N_FaceSet;
   SFFN->N_Neighb_min = 100; /* ridiculously high */
   SUMA_RETURN (SFFN);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   
   SFFN = SUMA_allocate_FaceSet_Edge_Neighb(N_EL/3);
   if (SFFN == NULL) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_allocate_FaceSet_Edge_Neighb.\n", FuncName);
      SUMA_RETURN (NULL);
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
            SUMA_RETURN (NULL);
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
   
   SUMA_RETURN (SFFN);
}   

/*!
   Makes sure the triangles in FaceSetList are of a consistent orientation.
   
   ans = SUMA_MakeConsistent (FaceSetList, N_FaceSet, SEL) 
   
   \param FaceSetList (int *) N_FaceSet x 3 vector (was matrix prior to SUMA 1.2) containing triangle definition
   \param N_FaceSet int
   \param SEL (SUMA_EDGE_LIST *) pointer Edgelist structure as output by SUMA_Make_Edge_List
   
   \ret ans (SUMA_Boolean) YUP, NOPE 
   
   \sa SUMA_Make_Edge_List
     
*/
SUMA_Boolean SUMA_MakeConsistent (int *FL, int N_FL, SUMA_EDGE_LIST *SEL) 
{
   /* see for more documentation labbook NIH-2 test mesh  p61 */
   int i, it, NP, ip, N_flip=0, *isflip, *ischecked, ht0, ht1, NotConsistent, miss, miss_cur, N_iter, EdgeSeed, TriSeed, N_checked;
   static char FuncName[]={"SUMA_MakeConsistent"};
   SUMA_FACESET_FIRST_EDGE_NEIGHB *SFFN;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   NP = 3;
   isflip = (int *)SUMA_calloc(SEL->N_EL/3, sizeof(int));
   ischecked = (int *)SUMA_calloc(SEL->N_EL/3, sizeof(int));
   
   if (isflip == NULL || ischecked == NULL ) {
      fprintf(SUMA_STDERR, "Error %s: Failed to allocate for isflip\n", FuncName);
      SUMA_RETURN (NOPE);
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
                  SUMA_RETURN (NOPE);
               }
               if (NotConsistent < 0) {
                  /* triangles hosting these edges are consistent */
                  /* next triangle needs no flipping */
                  ischecked[ht1] = 1;
                  ++N_checked;
               } else {
                  /* triangles hosting these edges are NOT consistent */
                  /* flip the next triangle */
                  ip = NP * ht1;
                  it = FL[ip];
                  FL[ip] = FL[ip+2];
                  FL[ip+2] = it;
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
                  SUMA_RETURN (NOPE);
               }
               if (NotConsistent < 0) {
                  /* triangles hosting these edges are consistent */
                  /* 1st triangle needs no flipping */
                  ischecked[ht0] = 1;
                  ++N_checked;
               } else {
                  /* triangles hosting these edges are NOT consistent */
                  /* flip the 1st triangle */
                  ip = NP * ht0;
                  it = FL[ip];
                  FL[ip] = FL[ip+2];
                  FL[ip+2] = it;
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
               if (LocalHead) fprintf(SUMA_STDERR,"%s: Miss = %d, MissCur = %d\n", FuncName, miss, miss_cur); 
               ++miss;
            }
         }
         ++i;   
      }
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Miss = %d, MissCur = %d\n", FuncName, miss, miss_cur);
      ++N_iter;
   }

   if (LocalHead) fprintf(SUMA_STDERR,"%s: %d iterations required to check the surface.\n", FuncName, N_iter);
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
         ip = NP * i;
         if (isflip[i]) fprintf (SUMA_STDERR,"\t%d %d %d\t(%d)\t*\n", FL[ip], FL[ip+1], FL[ip+2], ischecked[i]);
            else fprintf (SUMA_STDERR,"\t%d %d %d\t(%d)\n", FL[ip], FL[ip+1], FL[ip+2], ischecked[i]);
      }
   #endif
      
   /* freedom */
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Free time \n", FuncName);
   if (isflip) SUMA_free(isflip);
   if (ischecked) SUMA_free(ischecked);
   if (LocalHead) fprintf(SUMA_STDERR,"%s: returning.\n", FuncName);
   
   SUMA_RETURN (YUP);
}

#ifdef SUMA_MakeConsistent_STANDALONE
void usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_MakeConsistent <FaceSetList file> <NodeList file>\n");
          printf ("To compile: \ngcc -DSUMA_MakeConsistent_STANDALONE -Wall -o SUMA_MakeConsistent SUMA_MiscFunc.c ");
          printf ("SUMA_lib.a libmri.a -I/usr/X11R6/include -I./ -L/usr/lib -L/usr/X11R6/lib \n");
          printf ("-lm -lGL -lGLU -lGLw -lXmu -lXm -lXt -lXext -lX11 -lMesaGLw -lMesaGLwM \n");
          printf ("\t\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \t Wed Mar 20 14:23:42 EST 2002\n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
   float *NodeList;
   int *FL, N_FL, i, ip, N_Node;
   SUMA_EDGE_LIST *SEL;
   
   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_MakeConsistent-Main-");
   
   
   if (argc < 2)
       {
          usage ();
          exit (1);
       }
   
   N_FL = SUMA_float_file_size(argv[1]);
   N_Node = SUMA_float_file_size(argv[2]);
   
   N_FL = N_FL / 3;
   FL = (int *)SUMA_calloc(N_FL * 3, sizeof(int));
   
   N_Node = N_Node / 3;
   NodeList = (float *)SUMA_calloc(N_Node *3, sizeof(float));
   
   SUMA_Read_dfile (argv[1], FL,  N_FL * 3);
   SUMA_Read_file (argv[2], NodeList, N_Node *3);
   
   /* make the edge list */
   SEL = SUMA_Make_Edge_List (FL, N_FL, N_Node, NodeList);
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
   for (i=0; i<N_FL; ++i) {
      ip = 3 * i;
      fprintf(SUMA_STDERR," %d %d %d\n", FL[ip], FL[ip+1], FL[ip+2]); 
   }
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
    
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (attr_sm && attr_sm == attr) {
      fprintf (SUMA_STDERR, "Error %s: attr and attr_sm point to the same location. BAD!\n",FuncName);
      SUMA_RETURN (NULL); 
   }
   if (fn == NULL) {
      fprintf (SUMA_STDERR, "Error %s: fn is null, nothing to do.\n",FuncName);
      SUMA_RETURN (NULL); 
   }
   if (fn->N_Node != N_attr) {
      fprintf (SUMA_STDERR, "Error %s: N_attr (%d) must be equal to fn->N_Node (%d).\n",FuncName, N_attr, fn->N_Node);
      SUMA_RETURN (NULL); 
   }
   
   attr_sm = (float *)attr_sm;
   if (attr_sm == NULL) {
      attr_sm = (float *)SUMA_calloc (N_attr, sizeof(float));
   }
   
   if (attr_sm == NULL)
   {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for returning variable.\n", FuncName);
      SUMA_RETURN (NULL);
   } 
   
   for (i=0; i < N_attr; ++i) {
      /* make sure node id corresponds to i. That is you have a full set of nodes 0..N_attr */
      if (fn->NodeId[i] != i) {
         /* It's OK not to die here. This does occur in patches */
         /*fprintf (SUMA_STDERR, "Warning %s: fn does not seem to contain an explicit list of neighbors, from 0..N_attr. fn->NodeId[i] = %d, i = %d. Skipping node %d.\n", \
            FuncName, fn->NodeId[i], i, i); */
         /*SUMA_free(attr_sm); 
         attr_sm = NULL;
         SUMA_RETURN (attr_sm);*/
         continue;
      }
      attr_sm[i] = attr[i];
      for (j=0; j < fn->N_Neighb[i]; ++j)
      {
         attr_sm[i] += attr[fn->FirstNeighb[i][j]]; 
      }   
      attr_sm[i] /= (fn->N_Neighb[i]+1);
   }
   
   SUMA_RETURN (attr_sm);   
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
   static char FuncName[]={"SUMA_Build_FirstNeighb"};
   int i, j, n1, n2,  **FirstNeighb, N_ELm1, jj, tmp, TessErr_Cnt=0;
   SUMA_Boolean skp;
   SUMA_NODE_FIRST_NEIGHB *FN;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (el == NULL || N_Node == 0) {
      fprintf(SUMA_STDERR, "Error %s: el == NULL or N_Node == 0, nothing to do.\n", FuncName);
      SUMA_RETURN (NULL);
   }   
   
   FN = (SUMA_NODE_FIRST_NEIGHB *)SUMA_malloc(sizeof(SUMA_NODE_FIRST_NEIGHB));
   if (FN == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Could not allocate space for FN\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* allocate space for FN's matrices */
   FN->N_Node = N_Node;
   FN->N_Neighb_max = 0;
   
   FN->FirstNeighb = (int **) SUMA_allocate2D(FN->N_Node, SUMA_MAX_NUMBER_NODE_NEIGHB+1, sizeof (int));
   FN->N_Neighb = (int *) SUMA_calloc (FN->N_Node, sizeof(int));
   FN->NodeId = (int *) SUMA_calloc (FN->N_Node, sizeof(int));
   
   if (FN->FirstNeighb == NULL || FN->N_Neighb == NULL || FN->NodeId == NULL ){
      fprintf(SUMA_STDERR, "Error %s: Could not allocate space forFN->FirstNeighb &/| FN->N_Neighb &/| FN->NodeId.\n", FuncName);
      SUMA_RETURN (NULL);
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
         fprintf(SUMA_STDERR, "Critical Error %s\a: Maximum number of node neighbors for node %d or node %d exceeds %d (SUMA_MAX_NUMBER_NODE_NEIGHB)\n SUMA will try to launch but some functions may not work properly.\n", FuncName, n1, n2, SUMA_MAX_NUMBER_NODE_NEIGHB);
      }else {
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
      }
      
      ++j;
   }/* for j */

   /* now SUMA_reallocate for final FirstNeighb */
   FirstNeighb = (int **) SUMA_allocate2D(FN->N_Node, FN->N_Neighb_max, sizeof (int));
   if (FirstNeighb == NULL){
      fprintf(SUMA_STDERR, "Error %s: Could not allocate space for FirstNeighb\n", FuncName);
      SUMA_Free_FirstNeighb (FN);
      SUMA_RETURN (NULL);
   } 

   /* crop left over allocated space and rearrange neighboring nodes in order */
   for (i=0; i < N_Node; ++i) {
      #ifdef NoOrder
      for (j=0; j < FN->N_Neighb[i]; ++j) {
          FirstNeighb[i][j] = FN->FirstNeighb[i][j];
      }
      #else /* ordered nodes, Tue Jan  7 13:21:57 EST 2003 */
        /* copy first node */
        FirstNeighb[i][0] = FN->FirstNeighb[i][0];
        j = 1;
        jj = 1;
        while (j < FN->N_Neighb[i]) {
            if (SUMA_whichTri (el, i, FirstNeighb[i][jj-1], FN->FirstNeighb[i][j]) >= 0) {
               FirstNeighb[i][jj] = FN->FirstNeighb[i][j];
               /* now swap in FN->FirstNeighb[i] the positions of jj and j */
               tmp =  FN->FirstNeighb[i][jj];
               FN->FirstNeighb[i][jj] = FN->FirstNeighb[i][j];
               FN->FirstNeighb[i][j] = tmp;
               ++jj;
               j = jj;
            } else {
               ++j;
            }
        }
        if (jj != FN->N_Neighb[i]) {
            if (!TessErr_Cnt) {
               fprintf (SUMA_STDERR, "Error %s: Failed in copying neighbor list! jj=%d, FN->N_Neighb[%d]=%d\n", 
                  FuncName, jj, i, FN->N_Neighb[i]);
               fprintf (SUMA_STDERR, "\tIf this is a closed surface, the problem is likely due to a tessellation error.\n\tOne or more edges may not be part of 2 and only 2 triangles.\n\tNeighbor list for node %d will not be ordered as connected vertices.\n", 
                  i);
               fprintf (SUMA_STDERR, "\tFurther occurences of this error will not be reported.\n");
            }
            ++TessErr_Cnt;
            while (jj < FN->N_Neighb[i]) {
               FirstNeighb[i][jj] = FN->FirstNeighb[i][jj];
               ++jj;
            }
        }    
      #endif
   }
   if (TessErr_Cnt) {
      fprintf (SUMA_STDERR, "\t%d similar occurences of the error above were found in this mesh.\n", TessErr_Cnt);
   }
   SUMA_free2D((char **)FN->FirstNeighb, N_Node);
   FN->FirstNeighb = FirstNeighb;
   /* SUMA_disp_dmat (FN->FirstNeighb, N_Node, FN->N_Neighb_max, 0); */
   SUMA_RETURN (FN);
}

/*!
   frees the Node Neighbor structure formed in SUMA_Build_FirstNeighb
*/ 
SUMA_Boolean SUMA_Free_FirstNeighb (SUMA_NODE_FIRST_NEIGHB *FN)
{
   static char FuncName[]={"SUMA_Free_FirstNeighb"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (FN->NodeId) SUMA_free(FN->NodeId);
   if (FN->N_Neighb) SUMA_free(FN->N_Neighb);
   if (FN->FirstNeighb) SUMA_free2D ((char **)FN->FirstNeighb, FN->N_Node);
   if (FN) SUMA_free(FN);
   SUMA_RETURN (YUP);
}

/*! calculate the normal to a triangle 
   A = SUMA_TriNorm (n1, n2, n3, normal)
   \param n1 (float *)pointer to vector containing XYZ of node 1
   \param n2 (float *)pointer to vector containing XYZ of node 2
   \param n3 (float *)pointer to vector containing XYZ of node 3
   \param normal (float *)pointer to vector to contain normal of triangle. 
   \return A (SUMA_Boolean) NOPE if the norm of the normal = 0. In that case, occuring with FreeSurfer surfaces, normal is 1.0 1.0 1.0 
   \sa SUMA_SurfNorm
   
*/
SUMA_Boolean SUMA_TriNorm (float *n0, float *n1, float *n2, float *norm)
{
   static char FuncName[]={"SUMA_TriNorm"};
   int i;
   float d1[3], d2[3], d;
   
   for (i=0; i<3; ++i) {
         d1[i] = n0[i] - n1[i];
         d2[i] = n1[i] - n2[i];
   }
   norm[0] = d1[1]*d2[2] - d1[2]*d2[1];
	norm[1] = d1[2]*d2[0] - d1[0]*d2[2];
	norm[2] = d1[0]*d2[1] - d1[1]*d2[0];  
   
   d = sqrt(norm[0] * norm[0] + norm[1] * norm[1] + norm[2] * norm[2]);
   
   if (d==0.0) {
      norm[0] = norm[1] = norm[2] = 1.0;
      SUMA_RETURN (NOPE);
   }else {
      for (i=0; i<3; ++i) norm[i] /= d;
      SUMA_RETURN (YUP);
   }
}

/*! calculate the area of a triangle
   A = SUMA_TriSurf3 (n1, n2, n3, normal)
   \param n1 (float *)pointer to vector containing XYZ of node 1
   \param n2 (float *)pointer to vector containing XYZ of node 2
   \param n3 (float *)pointer to vector containing XYZ of node 3
   \param normal (float *)pointer to vector containing normal of triangle. 
   \return A (float) area of triangle  
   \sa SUMA_PolySurf3
   \sa SUMA_TriNorm
   \sa SUMA_TRI_AREA for macro version
*/

float SUMA_TriSurf3 (float *n0, float *n1, float *n2)
{
   static char FuncName[]={"SUMA_TriSurf3"};
   float dv[3], dw[3], cross[3], A; 
   int i, ii, coord, kk, jj;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SUMA_MT_SUB (dv, n1, n0);
   SUMA_MT_SUB (dw, n2, n0);
   SUMA_MT_CROSS(cross,dv,dw);
   SUMA_NORM(A, cross);
   A *= 0.5;
   
   SUMA_RETURN (A); 
}

/*! calculate the area of a triangle
   A = SUMA_TriSurf3v (NodeList, FaceSets, N_FaceSet, )
   \param NodeList (float *)pointer to vector containing XYZ of nodes  (typically SO->NodeList)
   \param FaceSets (int *) pointer to vector (3*N_FaceSet long) containing triangle indices  (typically SO->FaceSetList)
   \param N_FaceSet (int) number of triangles, (typically SO->N_FaceSet)
   \return A (float *) vector of triangle areas (N_FaceSet elements long) 

   \sa SUMA_PolySurf3
   \sa SUMA_TriNorm
   \sa SUMA_TRI_AREA for macro version
*/

float * SUMA_TriSurf3v (float *NodeList, int *FaceSets, int N_FaceSet)
{
   static char FuncName[]={"SUMA_TriSurf3v"};
   float *A = NULL, *n0, *n1, *n2, a;
   int i, i3;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   A = (float *) SUMA_calloc (N_FaceSet, sizeof(float));
   if (A == NULL ) {
      fprintf(SUMA_STDERR,"Error %s; Failed to allocate for A \n", FuncName);
      SUMA_RETURN (NULL);
   }  
   
   for (i=0;  i<N_FaceSet; ++i) {
      i3 = 3*i;
      n0 = &(NodeList[3*FaceSets[i3]]);
      n1 = &(NodeList[3*FaceSets[i3+1]]);
      n2 = &(NodeList[3*FaceSets[i3+2]]);
      SUMA_TRI_AREA( n0, n1, n2, A[i]); 
      /* A[i] = SUMA_TriSurf3 (n0, n1, n2); */
   }
   
   SUMA_RETURN (A);
}

/*!
   Calculate the area of planar polygons
   A = SUMA_PolySurf3 (NodeList, int N_Node, int *FaceSets, int N_FaceSet, int PolyDim, float *FaceNormList, SUMA_Boolean SignedArea)
   \param NodeList (float *)  (N_Node x 3) vector containing XYZ of each node
   \param N_Node number of nodes in NodeList
   \param FaceSets (int *) vector (matrix, prior to SUMA 1.2) (N_FaceSet x PolyDim) defining the polygons by their indices into NodeList
   \param N_FaceSet (int) number of polygons
   \param PolyDim (int) dimension of polygons (3 triangles)
   \param FaceNormList (float *) N_FaceSet x 3 vector of normals to polygons
   \param SignedArea (SUMA_Boolean) signed or unsigned areas
      positive means the vertices are oriented counterclockwise around the polygon when viewed from the side of the plane poited to by the normal 
   \return A (float *) vector containing the area of each polygon in FaceSets
  

   \sa SUMA_TriSurf3
   
   Algorithm by Dan Sunday http://geometryalgorithms.com
*/
float * SUMA_PolySurf3 (float *NodeList, int N_Node, int *FaceSets, int N_FaceSet, int PolyDim, float *FaceNormList, SUMA_Boolean SignedArea)
{
   static char FuncName[]={"SUMA_PolySurf3"};
   float **V, *A, ax, ay, az, an;
   int i, ii, coord, kk, jj, id, ND, ip, NP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ND = 3;
   NP = PolyDim;
   A = (float *) SUMA_calloc (N_FaceSet, sizeof(float));
   V = (float **) SUMA_allocate2D(PolyDim+2, 3, sizeof(float));
   
   if (A == NULL || V == NULL) {
      fprintf(SUMA_STDERR,"Error %s; Failed to allocate for A or V\n", FuncName);
      SUMA_RETURN (NULL);
   }

   for (i=0; i < N_FaceSet; ++i) {
      ip = NP * i;
      if (FaceNormList[ip] > 0) ax = FaceNormList[ip];
         else ax = -FaceNormList[ip];
      
      if (FaceNormList[ip+1] > 0) ay = FaceNormList[ip+1];
         else ay = -FaceNormList[ip+1];
      
      if (FaceNormList[ip+2] > 0) az = FaceNormList[ip+2];
         else az = -FaceNormList[ip+2];
   
   
      coord = 3;
      if (ax > ay) {
         if (ax > az) coord = 1;
      } else {
         if (ay > az) coord = 2;
      }
   
      for (ii=0; ii< PolyDim; ++ii) {
         ip = NP * i;
         id = ND * FaceSets[ip+ii];
         V[ii][0] = NodeList[id];
         V[ii][1] = NodeList[id+1];
         V[ii][2] = NodeList[id+2];
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
      
      if (!SignedArea) {
         if (A[i] < 0) A[i] = -A[i];
      }
   } /* for i*/
   
   SUMA_free2D((char **)V, PolyDim+2);
   SUMA_RETURN (A);
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
   
   \param NodeList (float *) N_Node x 3 vector containing the XYZ coordinates of the nodes
   \param N_Node (int)  number of nodes in NodeList
   \param NodeNormList (float *) N_Node x 3 vector (was matrix prior to SUMA 1.2) containing the normal vector at each node
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


SUMA_SURFACE_CURVATURE * SUMA_Surface_Curvature (float *NodeList, int N_Node, float *NodeNormList, float *A, int N_FaceSet, SUMA_NODE_FIRST_NEIGHB *FN, SUMA_EDGE_LIST *SEL)
{ 
   static char FuncName[] = {"SUMA_Surface_Curvature"};
   int i, N_Neighb, j, ji, Incident[MAX_INCIDENT_TRI], N_Incident, kk, ii, id, ND; 
   float  Ntmp[3],  vi[3], vj[3], *Num, NumNorm, num, denum, sWij, T1e[3], T2e[3], mg, c, s;
   float **fa33, **fb33, **fc33, **Ni, **Nit, *Wij, *Kij, **Tij, **I, **Mi, **Q, **Qt, **fa22, **mMi, **mMir;
   SUMA_Boolean *SkipNode;
   SUMA_SURFACE_CURVATURE *SC;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!A || !NodeList || !NodeNormList || !FN || !SEL) {
      fprintf (SUMA_STDERR, "Error %s: One of your inputs is NULL.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   
   SC = (SUMA_SURFACE_CURVATURE *)SUMA_malloc (sizeof(SUMA_SURFACE_CURVATURE));
   if (!SC) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for SC.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   
   Wij = (float *)SUMA_calloc (FN->N_Neighb_max, sizeof(float));
   Kij = (float *)SUMA_calloc (FN->N_Neighb_max, sizeof(float));
   Num = (float *)SUMA_calloc (3, sizeof(float));
   SkipNode = (SUMA_Boolean *) SUMA_calloc (N_Node, sizeof(SUMA_Boolean));
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
   SC->Kp1 =(float *)SUMA_calloc (N_Node, sizeof(float));
   SC->Kp2 =(float *)SUMA_calloc (N_Node, sizeof(float));

   if (!fa22 || !mMir || !mMi || !Wij || !Kij || !Tij || !Ni || !Nit || !fa33 || !fb33 || !fc33 || !I || !Num || !SkipNode || !Mi || !Q || !Qt || !SC->T1 || !SC->T2 || !SC->Kp1 || !SC->Kp2) {
      fprintf (SUMA_STDERR, "Error %s: Failed to allocate for Wij, Kij, Tij.\n", FuncName);
      if (Wij) SUMA_free(Wij);
      if (Kij) SUMA_free(Kij);
      if (Num) SUMA_free(Num);
      if (SkipNode) SUMA_free(SkipNode);
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
      SUMA_RETURN(NULL);
   }

   /* 3x3 identity matrix */
   I[0][0] = I[1][1] = I[2][2] = 1.0; I[0][1] = I[0][2] = I[1][0] = I[1][2] = I[2][0] = I[2][1] = 0.0;
   
   /* initialize SC */
   SC->N_SkipNode = 0;
   SC->N_Node = N_Node;
   
   fprintf (SUMA_STDERR, "%s: Beginning curvature computations:\n", FuncName);
   
   ND = 3;
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
      id = ND * i;
      Ni[0][0] = NodeNormList[id]; Ni[1][0] = NodeNormList[id+1]; Ni[2][0] = NodeNormList[id+2]; /* Normal vector at i*/
      Nit[0][0] = NodeNormList[id]; Nit[0][1] = NodeNormList[id+1]; Nit[0][2] = NodeNormList[id+2]; /* transpose of Ni */ 
      vi[0] = NodeList[id]; vi[1] = NodeList[id+1]; vi[2] = NodeList[id+2];  /* node coordinate vector */ 
      #ifdef DBG_2
         fprintf (SUMA_STDERR, "%s: Looping over neighbors, i = %d\n", FuncName, i);
      #endif
      j=0;
      sWij = 0.0;
      while (j < N_Neighb) {
         ji = FN->FirstNeighb[i][j]; /* index of the jth first neighbor of i */
         id = ND * ji;
         vj[0] = NodeList[id]; vj[1] = NodeList[id+1]; vj[2] = NodeList[id+2];  /* node coordinate vector at jth neighbor */
         
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
               if (Wij) SUMA_free(Wij);
               if (Kij) SUMA_free(Kij);
               if (Num) SUMA_free(Num);
               if (SkipNode) SUMA_free(SkipNode);
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
               SUMA_RETURN(NULL);
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
               /*   fprintf (SUMA_STDERR,"Wij[%d]=%f\t", ii, Wij[ii]);*/
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
               mg = sqrt(mMi[0][0]*mMi[0][0] + mMi[1][0]*mMi[1][0]);
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
   if (Wij) SUMA_free(Wij);
   if (Kij) SUMA_free(Kij);
   if (Num) SUMA_free(Num);
   if (SkipNode) SUMA_free(SkipNode);
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

   SUMA_RETURN (SC);
}

/*!
   free the SUMA_SURFACE_CURVATURE structure
*/
void SUMA_Free_SURFACE_CURVATURE (SUMA_SURFACE_CURVATURE *SC)
{
   static char FuncName[]={"SUMA_Free_SURFACE_CURVATURE"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (SC == NULL) SUMA_RETURNe;
   if (SC->Kp1) SUMA_free(SC->Kp1);
   if (SC->Kp2) SUMA_free(SC->Kp2);
   if (SC->T1) SUMA_free2D ((char **)SC->T1, SC->N_Node);
   if (SC->T2) SUMA_free2D ((char **)SC->T2, SC->N_Node);
   if (SC) SUMA_free(SC);
   SUMA_RETURNe;
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   e[0] = 1.0; e[1] = 0.0; e[2] = 0.0;
   
   #ifndef TAUBIN_Householder
      /* generic algorithm */
      mNi = sqrt(Ni[0] * Ni[0] + Ni[1] * Ni[1] + Ni[2] * Ni[2]);
      for (ii=0; ii < 3; ++ii) 
         b[ii] = Ni[ii] + mNi * e[ii];
      mb = sqrt(b[0] * b[0] + b[1] * b[1] + b[2] * b[2]);

      if (mb == 0) {
         fprintf (SUMA_STDERR,"Error %s: mb = 0\n",FuncName);
         SUMA_RETURN (NOPE);
      }

      b[0] /= mb; b[1] /= mb; b[2] /= mb;
   #else
      /* Taubin's algorithm Estimating the tensor of curvature of a surface from a polyhedral approximation */ 
      /* calculate difference and sum vectors with their norms (save sqrt for later) */
      
      d[0] = e[0] - Ni[0]; d[1] = e[1] - Ni[1]; d[2] = e[2] - Ni[2]; 
      nd = d[0]*d[0] + d[1]*d[1] + d[2]*d[2];
      
      s[0] = e[0] + Ni[0]; s[1] = e[1] + Ni[1]; s[2] = e[2] + Ni[2];
      ns = s[0]*s[0] + s[1]*s[1] + s[2]*s[2];
      
      if (!nd || !ns) {
         fprintf (SUMA_STDERR,"Error %s: nd || ns = 0\n",FuncName);
         SUMA_RETURN (NOPE);
      }
      
      if (nd > ns) {
         nd = sqrt(nd);
         b[0] = d[0] / nd;
         b[1] = d[1] / nd;
         b[2] = d[2] / nd;
         /*Q(:,1) will be equal to -Ni*/
         
      } else {
         ns = sqrt(ns);
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

   SUMA_RETURN (YUP);   
}

/*! 
   C = SUMA_Convexity (NodeList, N_Node, NodeNormList, FN)

   \param NodeList (float *) N_Node x 3 vector containing the coordinates for each node
   \param N_Node (int) number of nodes
   \param NodeNormList (float *) N_Node x 3 vector (was matrix prior to SUMA 1.2) containing the unit normals at each node
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
float * SUMA_Convexity (float *NL, int N_N, float *NNL, SUMA_NODE_FIRST_NEIGHB *FN)
{
   static char FuncName[]={"SUMA_Convexity"};
   float *C, d, D, dij;
   int i, j, in, id, ind, ND;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   C = NULL;
   
   /* allocate for C */
   C = (float *)SUMA_calloc (N_N, sizeof(float));
   
   if (C == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for C.\n", FuncName);
      SUMA_RETURN (C);
   }
   
   ND = 3;
   
   for (i=0; i < N_N; ++i) {
      id = ND * i;
      /* the plane at node i, having normal [NNL(id), NNL(id+1), NNL(id+2)] (id = 3*i) has the equation A X + B Y + C Z + D = 0
      NNL[id] NL[id]  + NNL[id+1] NNL[id+1]  + NNL[id+2] NL[id+2] + D = 0 */
      
      D = -NNL[id] * NL[id] - NNL[id+1] * NL[id+1] - NNL[id+2] * NL[id+2];
      
      for (j=0; j < FN->N_Neighb[i]; ++j) {
         /* find the distance between the neighboring node j and the tangent plane at i 
         d = (A X + B Y + C Z + D ) / (sqrt(A*A + B*B + C*C))
         denominator is norm of Normals which should be 1
         */
         in = FN->FirstNeighb[i][j];
         ind = in * ND;
         d = NNL[id] * NL[ind] + NNL[id+1] * NL[ind+1] + NNL[id+2] * NL[ind+2] + D ;
         
         /* calculate the distance between node i and it's neighbor */
         dij = sqrt( (NL[ind] - NL[id]) * (NL[ind] - NL[id]) + (NL[ind+1] - NL[id+1]) * (NL[ind+1] - NL[id+1]) + (NL[ind+2] - NL[id+2]) * (NL[ind+2] - NL[id+2]));
         
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
   
   SUMA_RETURN (C);
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

    assert (str);

    lo = (int)strlen(str);

   buf1 = (char *)SUMA_calloc (pad_ln-lo+2,sizeof (char));
    strp = (char *)SUMA_calloc (pad_ln+lo+2,sizeof (char));

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
             SUMA_free(strp);
            SUMA_free(buf1);
            SUMA_RETURN (NULL);
          }

    SUMA_free(buf1);

    SUMA_RETURN (strp);

}/*SUMA_pad_str*/

/*! 
   Function to get a bunch of numbers from stdin
   
   int SUMA_ReadNumStdin (float *fv, int nv)
   
    \param fv (float *) pointer to nv x 1 vector that will hold the input. 
   \param nr (int) number of values to be read and stored in fv
   \ret nvr (int) number of values actually read from stdin
   -1 in case of error
    
*/


int SUMA_ReadNumStdin (float *fv, int nv)
{   
   int i=0, nvr = 0;
   char *endp, *strtp, s[SUMA_MAX_STRING_LENGTH], cbuf;
   static char FuncName[]={"SUMA_ReadNumStdin"};
   SUMA_Boolean eos, LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   fflush (stdin);
   
   while ((cbuf = getc(stdin)) != '\n' && i < SUMA_MAX_STRING_LENGTH-1) {
      if (cbuf == ',' || cbuf == '\t') {/* change , and tab  to space*/
         cbuf = ' ';
      }
         s[i] = cbuf;
         ++ i;
   }
   
   if (i == SUMA_MAX_STRING_LENGTH-1) {
      fprintf(SUMA_STDERR,"Error %s: No more than %d characters are allowed on stdin.\n", FuncName, SUMA_MAX_STRING_LENGTH-1);
      fflush(stdin);
      SUMA_RETURN(-1);
   }
   
   s[i] = '\0';
   
   if (!i) SUMA_RETURN(0);
   
   /* parse s */
   strtp = s;
   endp = NULL;
   nvr = 0;
   eos = NOPE;
   while (nvr < nv && !eos) {
      fv[nvr] = strtod(strtp, &endp);
      if (LocalHead) fprintf (SUMA_STDERR, "Local Debug %s: ERANGE: %d, EDOM %d, errno %d\n", FuncName, ERANGE, EDOM, errno); 
      
      if (endp == strtp) { 
         eos = YUP;
      } else {
         ++nvr;
         strtp = endp;
      }
   }
   
   if (eos && nvr < nv) {
      fprintf (SUMA_STDERR, "Warning %s: Expected to read %d elements, read only %d.\n", FuncName, nv, nvr);
   }
   
   SUMA_RETURN(nvr);
}

/*!
   \brief function that tests whether a string contains N numbers
   
   \param str (char *) null terminated string
   \param N (void *) This is an integer in disguise
   \return YUP: If str is NULL or N numbers were found in str
*/
SUMA_Boolean SUMA_isNumString (char *s, void *p)
{
   static char FuncName[]={"SUMA_isNumString"};
   char *endp, *strtp;
   int nd, N;
   SUMA_Boolean eos, FoundTip;
   double d;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!s) SUMA_RETURN(YUP); 
   
   N = (int)p;
   
   /* clean s by removing trailing junk then replacing non characters by space*/
   FoundTip = NOPE;
   for (nd=strlen(s)-1; nd >=0; --nd) {
      if (!isdigit(s[nd]) && s[nd] != '.'  && s[nd] != '-' && s[nd] != '+') {
         if (!FoundTip) {
            s[nd]= '\0'; /* remove */
         } else {
            s[nd] = ' '; /* blank */
         }
      }else {
         FoundTip = YUP;
      }
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: string now:%s:\n", FuncName, s);
   
   /* parse s */
   strtp = s;
   endp = NULL;
   nd = 0;
   eos = NOPE;
   while (!eos) {
      d = strtod(strtp, &endp);
      if (LocalHead) fprintf (SUMA_STDERR, "%s: value %f, ERANGE: %d, EDOM %d, errno %d\n", FuncName, d, ERANGE, EDOM, errno); 
      

      if (endp == strtp && *endp=='\0') { 
         eos = YUP;
      } else {
         strtp = endp;
         ++nd;
      }
   }
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Read %d/%d values.\n", FuncName, nd,N);
   if (N != nd) {
      SUMA_RETURN(NOPE);
   } else {
      SUMA_RETURN(YUP);
   }
   
}   

/*!
   \brief function that parses a string of numbers into a float vector
   
   \param str (char *) null terminated string
   \param fv (float*) vector where values will be stored
   \param N (int) This is the number of values desired
   \return int: This is the number of values read. 
      The function will register in fv more that N values 
      (to keep from running over preallocated space), but it
      will return the full number of values found.
      
      -1 in case of error
   \sa SUMA_isNumString
*/
int SUMA_StringToNum (char *s, float *fv, int N)
{
   static char FuncName[]={"SUMA_StringToNum"};
   char *endp, *strtp;
   int nd;
   SUMA_Boolean eos, FoundTip;
   double d;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!s) SUMA_RETURN(0); 
      
   /* clean s by removing trailing junk then replacing non characters by space*/
   FoundTip = NOPE;
   for (nd=strlen(s)-1; nd >=0; --nd) {
      if (!isdigit(s[nd]) && s[nd] != '.' && s[nd] != '-' && s[nd] != '+') {
         if (!FoundTip) {
            s[nd]= '\0'; /* remove */
         } else {
            s[nd] = ' '; /* blank */
         }
      }else {
         FoundTip = YUP;
      }
   }
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: string now:%s:\n", FuncName, s);
   
   /* parse s */
   strtp = s;
   endp = NULL;
   nd = 0;
   eos = NOPE;
   while (!eos) {
      d = strtod(strtp, &endp);
      if (LocalHead) fprintf (SUMA_STDERR, "%s: value %f, ERANGE: %d, EDOM %d, errno %d\n", FuncName, d, ERANGE, EDOM, errno); 
      
      if (endp == strtp && *endp=='\0') { 
         eos = YUP;
      } else {
         if (nd < N) fv[nd] = (float)d;
         strtp = endp;
         ++nd;
      }
   }
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Read %d/%d values.\n", FuncName, nd, N);
   
   SUMA_RETURN(nd);
   
}   
        
/***
 
File : SUMA_Find_inIntVect.c
Author : Ziad Saad
Date : Thu Nov 12 21:57:12 CST 1998
 
Purpose : 
 
 
 
Input paramters : 
   x      int *      :   vector containing integer values
   xsz   int      :   number of elements in x
   val   int      :   value to look for
   nValLocation   int *      :   integer containing the number of points in the SUMA_RETURNed vector
 
 
Usage : 
   ValLocation   = SUMA_Find_inIntVect (int *x, int xsz, int val, int *nValLocation)
 
 
Returns : 
   ValLocation   int * :
   
   a pointer to a vector of integers that contains the indices into x where val was found
   the vector contains *nValLocation elements
   
 
 
Support : 
 
 
 
Side effects : 
   The function does not use any fast searching mechanisms, might want to make it faster in 
   the future (use binary searches and such)
 
 
***/
int * SUMA_Find_inIntVect (int *x, int xsz, int val, int *nValLocation)
{/*SUMA_Find_inIntVect*/
   int k, *tmp, *ValLocation;
   static char FuncName[]={"SUMA_Find_inIntVect"};
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate the maximum  space for ValLocation */
   tmp = (int *) SUMA_calloc(xsz,sizeof(int));

   *nValLocation = 0;
   for (k = 0 ; k < xsz ; ++k)
   {
      if (x[k] == val)
         {
            tmp[*nValLocation] = k;
            ++*nValLocation;
         }

   }

   if (!*nValLocation)
      {
         SUMA_free (tmp);
         SUMA_RETURN (NULL);
      }

   /* Now, allocate just enough space for the SUMA_RETURNing vector */
      ValLocation = (int *) SUMA_calloc(*nValLocation,sizeof(int));
   /*copy the data into ValLocation*/
      SUMA_SCALE_VEC(tmp,ValLocation,1,*nValLocation,int,int);
   /* get rid of big array */
      SUMA_free(tmp);

   SUMA_RETURN (ValLocation);

}/*SUMA_Find_inIntVect*/

/***
 
File : SUMA_UniqueInt.c
Author : Ziad Saad
Date : Fri Nov 13 16:07:23 CST 1998
 
Purpose : 
 
 
 
Input paramters : 
   x      int *      : a pointer to a vector of integers
   xsz   int       : a scalar indicating the number of elements in x
   kunq  int *      : a pointer to an integer that will tell you the number 
                  of unique elements in x (length of kunq)
   Sorted   int   : a falg indicating whether x is sorted or not
                     if x is sorted, use 1 otherwise use 0
                    
 
 
Usage : 
      xunq = SUMA_UniqueInt (int *x, int xsz, int *kunq, int Sorted );
 
 
Returns : 
   xunq   int *   : a pointer to the vector containing the unique values of x
 
 
Support : 
 
 
 
Side effects : 
 
 
 
***/
int * SUMA_UniqueInt (int *y, int xsz, int *kunq, int Sorted )
{/*SUMA_UniqueInt*/
   int *xtmp, *xunq, k ,*x;
   SUMA_Boolean LocalHead = NOPE;
   static char FuncName[]={"SUMA_UniqueInt"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   *kunq = 0;

   if (!xsz)
    {
      SUMA_RETURN(NULL);
   }
   if (!Sorted)
    {/* must sort y , put in a new location so that y is not disturbed*/
      x = (int *)SUMA_calloc(xsz, sizeof(int));
      if (!x)
         {
            fprintf (SUMA_STDERR,"Error %s: Failed to allocate for x.", FuncName);
            SUMA_RETURN (NULL);
         }
      for (k=0; k < xsz; ++k)
         x[k] = y[k];
      qsort(x,xsz,sizeof(int), (int(*) (const void *, const void *)) SUMA_compare_int);
   }
   else
      x = y;

   if (!xsz)   /* Nothing sent ! */
    SUMA_RETURN (NULL);

   xtmp = (int *) SUMA_calloc(xsz,sizeof(int));
   if (xtmp == NULL)
    {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate memory", FuncName);
      SUMA_RETURN (NULL);
   }

   *kunq = 0;
   xtmp[0] = x[0];
   for (k=1;k<xsz;++k)
    {
      if ((x[k] != x[k - 1]))
         {
            ++*kunq;
            xtmp[*kunq] = x[k];   
         }
   }
   ++*kunq;
   
   
   /* get rid of extra space allocated */
   xunq = (int *) SUMA_calloc(*kunq,sizeof(int));
   SUMA_COPY_VEC(xtmp,xunq,*kunq,int,int);

   SUMA_free(xtmp); 

   if (!Sorted)
      SUMA_free (x);

   SUMA_RETURN (xunq);
}/*SUMA_UniqueInt*/

/*!
   \brief In addition to returning the unique set of values,
   The function creates a vector of indices specifying
   which values in y were retained 
   
   yu = SUMA_UniqueInt_ind (y, N_y, N_yu, Sorted, iu);
   
   \param y (int *) SORTED input vector
   \param N_y (int) number of elements in y
   \param N_yu (int *) to contain number of elements in yu
   \param iu (int **) to contain pointer to vector containing
                      indices into y of the values retained in 
                      yu
   
   \sa SUMA_UniqueInt_ind
   \sa SUMA_z_dqsort
   
   -Make sure y is sorted ahead of time
   -remember to free yu and *iu after you are done with them
*/
int * SUMA_UniqueInt_ind (int *ys, int N_y, int *kunq, int **iup)
{/*SUMA_UniqueInt*/
   int *yu=NULL, k ,*iu=NULL;
   SUMA_Boolean LocalHead = NOPE;
   static char FuncName[]={"SUMA_UniqueInt_ind"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   *kunq = 0;

   if (!N_y)
    {
      SUMA_RETURN(NULL);
   }

   if (!N_y)   /* Nothing sent ! */
    SUMA_RETURN (NULL);

   yu = (int *) SUMA_calloc(N_y,sizeof(int));
   iu = (int *) SUMA_calloc(N_y,sizeof(int));
   if (!yu || !iu)
    {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate memory", FuncName);
      SUMA_RETURN (NULL);
   }

   *kunq = 0;
   yu[0] = ys[0];
   iu[0] = 0;
   for (k=1;k<N_y;++k)
    {
      if ((ys[k] != ys[k - 1]))
         {
            ++*kunq;
            yu[*kunq] = ys[k];   
            iu[*kunq] = k;
         }
   }
   ++*kunq;
   
   
   /* get rid of extra space allocated */
   yu = (int *) SUMA_realloc(yu, *kunq*sizeof(int));
   iu = (int *) SUMA_realloc(iu, *kunq*sizeof(int));

   *iup = iu;
   SUMA_RETURN (yu);
}/*SUMA_UniqueInt_ind*/

/*!
   \brief creates a reordered version of a vector 
   yr = SUMA_reorder(y, isort, N_isort);
   
   \param y (int *) vector
   \param isort (int *) vector containing sorting order
   \param N_isort (int ) number of elements in isort
   \return yr (int *) reordered version of y where:
                     yr[i] = y[isort[i]];
                     
   - you should free yr with SUMA_free(yr) when done with it
   - obviously it's your business to ensure that
            isort[i] cannot be larger than then number
            of elements in y 
*/
int *SUMA_reorder(int *y, int *isort, int N_isort)
{
   static char FuncName[]={"SUMA_reorder"};
   int i = 0, *yr = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!y || !isort || N_isort <= 0) SUMA_RETURN(yr);
   
   yr = (int *)SUMA_calloc( N_isort, sizeof(int));
   if (!yr) SUMA_RETURN(yr);
   
   for (i=0; i<N_isort; ++i) yr[i] = y[isort[i]];
   
   SUMA_RETURN(yr);
}

/*!
   \brief Appends newstring to string in SS->s while taking care of resizing space allocated for s
   
   \param SS (SUMA_STRING *) pointer to string structure
   \param newstring (char *) pointer to string to add to SS
   \return SS (SUMA_STRING *) pointer to string structure with SS->s now containing newstring
   - When SS is null, 1000 characters are allocated for s (initialization) and s[0] = '\0';
   - When newstring is NULL, space allocated for SS->s is resized to the correct dimension and 
   a null character is placed at the end.
*/
SUMA_STRING * SUMA_StringAppend (SUMA_STRING *SS, char *newstring)
{
   static char FuncName[]={"SUMA_StringAppend"};
   int N_inc = 0, N_cur = 0;
   int N_chunk = 1000;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!SS) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Allocating for SS.\n", FuncName);
      SS = (SUMA_STRING *) SUMA_malloc (sizeof(SUMA_STRING));
      SS->s = (char *) SUMA_calloc (N_chunk, sizeof(char));
      SS->s[0] = '\0';
      SS->N_alloc = N_chunk;
      SUMA_RETURN (SS);
   }
   
   if (newstring) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Appending to SS->s.\n", FuncName);
      N_inc = strlen (newstring);
      N_cur = strlen (SS->s);
      if (SS->N_alloc < N_cur+N_inc+1) { /* must reallocate */
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Must reallocate for SS->s.\n", FuncName);
         SS->N_alloc = N_cur+N_inc+N_chunk+1;
         SS->s = (char *)SUMA_realloc (SS->s, sizeof(char)*SS->N_alloc);
         if (!SS->s) {
            fprintf (SUMA_STDERR, "Error %s: Failed to reallocate for s.\n", FuncName);
            SUMA_RETURN (NULL);
         }
      }
      /* append */
      sprintf (SS->s, "%s%s", SS->s, newstring);
   }else {
      /* shrink SS->s to small size */
      N_cur = strlen (SS->s);
      if (SS->N_alloc > N_cur+1) {
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Shrink realloc for SS->s.\n", FuncName);
         SS->N_alloc = N_cur+1;
         SS->s = (char *)SUMA_realloc (SS->s, sizeof(char)*SS->N_alloc);
         if (!SS->s) {
            fprintf (SUMA_STDERR, "Error %s: Failed to reallocate for s.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /*put a null at the end */
         SS->s[SS->N_alloc-1] = '\0';
      }
   }
   
   SUMA_RETURN (SS);

}

