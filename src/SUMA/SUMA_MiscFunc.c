#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF;

#ifdef USE_SUMA_MALLOC
   /* NO LONGER SUPPORTED Apr. 09 04 */
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
         SUMA_ENTRY;
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
         SUMA_ENTRY;
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
         SUMA_ENTRY;
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

               SUMAg_CF->Mem->Pointers =
                  (void **)realloc (SUMAg_CF->Mem->Pointers, sizeof(void*) *
                                    SUMAg_CF->Mem->N_MaxPointers);
               SUMAg_CF->Mem->Size  =
                  (int *)realloc((void *)SUMAg_CF->Mem->Size, sizeof(int) *
                                         SUMAg_CF->Mem->N_MaxPointers);
               if (!SUMAg_CF->Mem->Pointers || !SUMAg_CF->Mem->Pointers) {
                  fprintf (SUMA_STDERR,
                        "Error %s: Failed to reallocate.\n"
                        "Turning off memory tracing.\n",
                     FuncName);
                  /* free up allocated space, clean up pointers, turn off memory
                     tracing DO NOT USE SUMA_free here*/
                  if (SUMAg_CF->Mem->Pointers) free(SUMAg_CF->Mem->Pointers);
                  SUMAg_CF->Mem->Pointers = NULL;
                  if (SUMAg_CF->Mem->Size) free(SUMAg_CF->Mem->Size);
                  SUMAg_CF->Mem->Size = NULL;
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
         SUMA_ENTRY;
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

   #ifdef USE_SUMA_MALLOC
   SUMA_SL_Err("NO LONGER SUPPORTED");
   return(NULL);
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
   #else
   return(NULL);
   #endif
}

/*!
   \brief Calculate factorial
*/
double SUMA_factorial (int n)
{
   static char FuncName[]={"SUMA_factorial"};
   double f;
   int c;

   SUMA_ENTRY;

   if (n<0) { SUMA_S_Errv("Factorial of negative number (%d)!\n", n); SUMA_RETURN(0); }
   if (n==0) SUMA_RETURN(1);
   f = 1;
   c = 1;
   do {
      f *= c;
      ++c;
   } while (c<=n);

   SUMA_RETURN(f);
}

/*!
   \brief Calculate factorials from [0 to n]
   results are retuned in a long long vector (n+1) elements long long
*/
double *SUMA_factorial_array (int n)
{
   static char FuncName[]={"SUMA_factorial_array"};
   double *f;
   int c;
   SUMA_ENTRY;
   if (n<0) { SUMA_S_Errv("Factorial of negative number (%d)!\n", n); SUMA_RETURN(NULL); }
   f = (double *)SUMA_calloc(n+1, sizeof(double));
   if (!f) {
      SUMA_S_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }
   f[0] = 1;
   c = 1;
   while (c<=n) {
      f[c] = c*f[c-1];
      ++c;
   }

   SUMA_RETURN(f);
}

/*!
   \brief Kronecker product
*/
SUMA_MX_VEC *SUMA_KronProd(SUMA_MX_VEC *A, SUMA_MX_VEC *B)
{
   static char FuncName[]={"SUMA_KronProd"};
   SUMA_MX_VEC *P = NULL;
   int pdims[2], a0, a1, b0, b1, p0, p1;

   SUMA_ENTRY;

   if (!A || !B) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NULL);
   }
   if (A->N_dims != B->N_dims || A->N_dims != 2) {
      SUMA_S_Err("Bad input matrix ndims");
      SUMA_RETURN(NULL);
   }
   if (A->tp != SUMA_double || A->tp != B->tp) {
      SUMA_S_Err("Only for SUMA_double matrices for the moment.");
      SUMA_RETURN(NULL);
   }
   pdims[0] = A->dims[0] * B->dims[0];
   pdims[1] = A->dims[1] * B->dims[1];


   if (!(P = SUMA_NewMxVec(A->tp, 2, pdims, 1))) {
      SUMA_S_Err("Failed to create output matrix.");
      SUMA_RETURN(NULL);
   }

   if (P->tp == SUMA_double) {
      double Aij, Bij;
      for (a0=0; a0<A->dims[0]; ++a0) {
         for (a1=0; a1<A->dims[1]; ++a1) {
            Aij = mxvd2(A, a0, a1);
            for (b0=0; b0<B->dims[0]; ++b0) {
               for (b1=0; b1<B->dims[1]; ++b1) {
                  Bij = mxvd2(B, b0, b1);
                  p0 = B->dims[0]*a0+b0;
                  p1 = B->dims[1]*a1+b1;
                  mxvd2(P, p0, p1) = Aij*Bij;
               }
            }
         }
      }
   }

   SUMA_RETURN(P);
}

/*!
   \brief turn a SUMA_MX_VEC into an equivalent matrix

*/
int SUMA_MxVecBuildMat(SUMA_MX_VEC *mxv)
{
   static char FuncName[]={"SUMA_MxVecBuildMat"};
   int i = 0, j=0;

   SUMA_ENTRY;

   if (!mxv) {
      SUMA_S_Err("NULL mxv");
      SUMA_RETURN(0);
   }
   if (mxv->tp != SUMA_double) {
      SUMA_S_Err("This function is only for double types");
   }

   if (!mxv->fdf) {
      SUMA_S_Err("Will not work if fdf != 1");
      SUMA_RETURN(0);
   }
   if (!mxv->N_dims || mxv->N_dims > 2) {
      SUMA_S_Err("MxVec not in matriceable form!");
      SUMA_RETURN(0);
   }

   if (mxv->m) {
      SUMA_S_Err("m is not null here");
      SUMA_RETURN(0);
   }

   mxv->m = (matrix *)SUMA_malloc(sizeof(matrix));
   matrix_initialize(mxv->m);

   /* create matrix using trickery to avoid duplicating data(reflects matrix_create)*/
   mxv->m->rows = mxv->dims[0];
   if (mxv->N_dims == 2) mxv->m->cols = mxv->dims[1];
   else mxv->m->cols = 1;

   mxv->m->elts = (double **) malloc (sizeof(double *) * mxv->m->rows);
   if (mxv->m->elts == NULL) {
      SUMA_S_Err("Failed to allocate for elts");
      SUMA_RETURN(0);
   }

   for (i = 0;  i < mxv->m->rows;  i++) {
      mxv->m->elts[i] = (double *)malloc(sizeof(double)*mxv->m->cols);
      if (mxv->m->elts[i] == NULL) {
         SUMA_S_Err("Failed to allocate for elts[i]");
         SUMA_RETURN(0);
      }
   }

   /* fill it up , explicit copy! MxVec does not store matrices row by row*/
   for (i = 0;  i < mxv->m->rows;  i++) {
      for (j = 0;  j < mxv->m->cols;  j++) {
         mxv->m->elts[i][j] = mxvd2(mxv, i,j);
      }
   }

   SUMA_RETURN(1);
}

/*!
   Note: contents of input matrix c are destroyed here
   ONLY works for double matrices
*/
SUMA_MX_VEC *SUMA_matrix2MxVec(matrix c)
{
   static char FuncName[]={"SUMA_matrix2MxVec"};
   SUMA_MX_VEC *mxv;
   int N_dims=2, dims[2], i, j;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   dims[0] = c.rows; dims[1] = c.cols;
   mxv = SUMA_NewMxNullVec(SUMA_double, N_dims,  dims,  1);
   /* have to create a new vector, MxVec does not store row by row*/
   mxv->dv = (double *)SUMA_malloc(c.cols*c.rows*sizeof(double));
   mxv->v = (void*)mxv->dv;
   if (!mxv->dv) {
         SUMA_S_Crit("Failed to allocate");
         SUMA_RETURN(NULL);
   }

   SUMA_LHv("Filling %d rows, %d cols", c.rows, c.cols);
   for (i=0; i<c.rows; ++i) {
      for (j=0; j<c.cols; ++j) {
         mxvd2(mxv,i,j) = c.elts[i][j];
      }
   }

   matrix_destroy(&c);

   SUMA_RETURN(mxv);
}

SUMA_MX_VEC *SUMA_CoerceMxVec(SUMA_MX_VEC *va, SUMA_VARTYPE tp,
                              int abs, SUMA_MX_VEC *recycle)
{
   static char FuncName[]={"SUMA_CoerceMxVec"};
   SUMA_MX_VEC *vt=NULL;
   int i;

   SUMA_ENTRY;

   if (  (va->tp != SUMA_double && va->tp != SUMA_complex) ||
         (tp != SUMA_double && tp != SUMA_complex) ) {
      SUMA_S_Err("Only complex and double types allowed.");
      SUMA_RETURN(NULL);
   }

   if (recycle) {
      if (!SUMA_MxVecSameDims(va, recycle)) {
         SUMA_S_Err("Bad recycle");
         SUMA_RETURN(NULL);
      }
      if (recycle->tp != tp) {
         SUMA_S_Errv("Mismatch between recycle->tp=%d and tp=%d\n",
                     recycle->tp , tp);
         SUMA_RETURN(NULL);
      }
      vt = recycle;
   } else {
      if (!(vt = SUMA_NewMxVec(tp, va->N_dims, va->dims, 1))) {
         SUMA_S_Err("Failed to allocate");
         SUMA_RETURN(NULL);
      }
   }

   if (va->tp == SUMA_complex  && tp == SUMA_double) {
       if (abs) {
         for (i=0; i<va->N_vals; ++i) {
            mxvd1(vt, i) = (double)SUMA_COMPLEX_ABS(mxvc1(va,i));
         }
       } else {
         for (i=0; i<va->N_vals; ++i) {
            mxvd1(vt, i) = (double)mxvc1(va,i).r;
         }
       }
   } else if (va->tp == SUMA_double  && tp == SUMA_complex) {
      if (abs) {
         for (i=0; i<va->N_vals; ++i) {
            mxvc1(vt, i).r = (float)SUMA_ABS(mxvd1(va,i));
            mxvc1(vt, i).i = 0.0;
         }
      } else {
         for (i=0; i<va->N_vals; ++i) {
            mxvc1(vt, i).r = (float)(mxvd1(va,i));
            mxvc1(vt, i).i = 0.0;
         }
      }
   } else {
      SUMA_S_Err("Type combo not supported, should not be here");
      vt = SUMA_FreeMxVec(vt);
   }

   SUMA_RETURN(vt);
}

SUMA_MX_VEC *SUMA_MxVecAdd(SUMA_MX_VEC *va, SUMA_MX_VEC *vb, int sign, SUMA_MX_VEC *recycle)
{
   static char FuncName[]={"SUMA_MxVecAdd"};
   SUMA_MX_VEC *vt=NULL;
   int i, j;
   SUMA_VARTYPE tp;
   int dims[SUMA_MX_VEC_MAX_DIMS];

   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (va->N_dims != vb->N_dims) {
      SUMA_S_Err("Mismatch in N_dims");
      SUMA_RETURN(vt);
   }
   if (va->N_vals != vb->N_vals) {
      SUMA_S_Err("Mismatch in N_vals");
      SUMA_RETURN(vt);
   }
   for (i=0; i<va->N_dims; ++i) {
      dims[i] = va->dims[i];
      if (va->dims[i] != vb->dims[i]) {
         SUMA_S_Err("Mismatch in dims of va and vb\n");
         SUMA_RETURN(vt);
      }
   }
   if (  (va->tp != SUMA_complex && va->tp != SUMA_double) ||
         (vb->tp != SUMA_complex && vb->tp != SUMA_double)  ) {
      SUMA_S_Err("Only complex and double allowed.");
      SUMA_RETURN(vt);
   }
   if (va->tp == SUMA_complex || vb->tp == SUMA_complex) tp = SUMA_complex;
   else tp = SUMA_double;

   if (recycle ) {
      if (recycle->tp != tp) {
         SUMA_S_Errv("Recycled vector of type %d, type %d needed.\n", recycle->tp, va->tp);
         SUMA_RETURN(vt);
      }
      if (recycle->N_vals != va->N_vals) {
         SUMA_S_Errv("Recycled vector of N_vals %d, N_vals %d needed.\n", recycle->N_vals, va->N_vals);
         SUMA_RETURN(vt);
      }
      if (recycle->N_dims != va->N_dims) {
         SUMA_S_Errv("Recycled vector of N_dims %d, N_dims of %d needed.\n", recycle->N_dims, va->N_dims);
         SUMA_RETURN(vt);
      }
      for (i=0; i<va->N_dims; ++i) {
         if (recycle->dims[i] != dims[i]) {
            SUMA_S_Errv("Recycled vector dims[%d]=%d, dims[%d]=%d needed.\n", i, recycle->dims[i], i, dims[i]);
            SUMA_RETURN(vt);
         }
      }
      vt = recycle;
   } else {
      vt = SUMA_NewMxVec(tp, va->N_dims, dims, 1);
   }

   /* here goes */
   if (va->tp == SUMA_complex && vb->tp == SUMA_complex) {
      if (sign > 0){ for (i=0; i<va->N_vals; ++i) {
                        mxvc1(vt, i).r = mxvc1(va, i).r + mxvc1(vb, i).r;
                        mxvc1(vt, i).i = mxvc1(va, i).i + mxvc1(vb, i).i;
                     }
      } else  {      for (i=0; i<va->N_vals; ++i) {
                        mxvc1(vt, i).r = mxvc1(va, i).r - mxvc1(vb, i).r;
                        mxvc1(vt, i).i = mxvc1(va, i).i - mxvc1(vb, i).i;
                     }
      }
   } else if (va->tp == SUMA_complex && vb->tp == SUMA_double) {
      if (sign > 0){ for (i=0; i<va->N_vals; ++i) {
                        mxvc1(vt, i).r = mxvc1(va, i).r + mxvd1(vb, i);
                        mxvc1(vt, i).i = mxvc1(va, i).i ;
                     }
      } else  {      for (i=0; i<va->N_vals; ++i) {
                        mxvc1(vt, i).r = mxvc1(va, i).r - mxvd1(vb, i);
                        mxvc1(vt, i).i = mxvc1(va, i).i ;
                     }
      }
   } else if (va->tp == SUMA_double && vb->tp == SUMA_complex) {
      if (sign > 0){ for (i=0; i<va->N_vals; ++i) {
                        mxvc1(vt, i).r = mxvd1(va, i)   + mxvc1(vb, i).r;
                        mxvc1(vt, i).i =                  mxvc1(vb, i).i;
                     }
      } else  {      for (i=0; i<va->N_vals; ++i) {
                        mxvc1(vt, i).r = mxvd1(va, i)   - mxvc1(vb, i).r;
                        mxvc1(vt, i).i =                - mxvc1(vb, i).i;
                     }
      }
   } else if (va->tp == SUMA_double && vb->tp == SUMA_double) {
      if (sign > 0){ for (i=0; i<va->N_vals; ++i) {
                        mxvd1(vt, i)   = mxvd1(va, i)   + mxvd1(vb, i)  ;
                     }
      } else  {      for (i=0; i<va->N_vals; ++i) {
                        mxvd1(vt, i)   = mxvd1(va, i)   - mxvd1(vb, i)  ;
                     }
      }
   } else {
      SUMA_S_Err("Bad combo");
      vt = SUMA_FreeMxVec(vt);
   }

   SUMA_RETURN(vt);
}

/*!
   \brief Return the transpose of MxVec
*/
SUMA_MX_VEC * SUMA_MxVecTranspose(SUMA_MX_VEC *va, SUMA_MX_VEC *recycle)
{
   static char FuncName[]={"SUMA_MxVecTranspose"};
   SUMA_MX_VEC *vt=NULL;
   int i, j;
   int dims[2];

   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;

   if (va->N_dims > 2 || va->N_dims < 0) {
      SUMA_S_Errv("Bad N_dims (%d)\n", va->N_dims);
   }
   if (va->N_dims == 1) va->dims[1] = 1;
   dims[0] = va->dims[1];
   dims[1] = va->dims[0];
   if (recycle ) {
      if (recycle->tp != va->tp) {
         SUMA_S_Errv("Recycled vector of type %d, type %d needed.\n", recycle->tp, va->tp);
         SUMA_RETURN(vt);
      }
      if (recycle->dims[0] != dims[0]) {
         SUMA_S_Errv("Recycled vector dims[0]=%d, dims[0]=%d needed.\n", recycle->dims[0], dims[0]);
         SUMA_RETURN(vt);
      }
      if (recycle->dims[1] != dims[1]) {
         SUMA_S_Errv("Recycled vector dims[1]=%d, dims[1]=%d needed.\n", recycle->dims[1], dims[1]);
         SUMA_RETURN(vt);
      }
      vt = recycle;
   } else {
      vt = SUMA_NewMxVec(va->tp, 2, dims, 1);
   }
   switch (va->tp) {
      case SUMA_double:
         if (va->dims[0] > va->dims[1]){  /* an optimization that is significant for stretched matrices */
            for (i=0;i<va->dims[0];++i) {
               for (j=0; j<va->dims[1]; ++j) {
                  mxvd2(vt,j,i) = mxvd2(va,i,j);
               }
            }
         } else {
            for (j=0; j<va->dims[1]; ++j) {
               for (i=0;i<va->dims[0];++i) {
                  mxvd2(vt,j,i) = mxvd2(va,i,j);
               }
            }
         }
         break;
      case SUMA_complex:
         if (va->dims[0] > va->dims[1]){  /* an optimization that is significant for stretched matrices */
            for (i=0;i<va->dims[0];++i) {
               for (j=0; j<va->dims[1]; ++j) {
                  mxvc2(vt,j,i).r = mxvc2(va,i,j).r;
                  mxvc2(vt,j,i).i = -mxvc2(va,i,j).i;
               }
            }
         } else {
            for (j=0; j<va->dims[1]; ++j) {
               for (i=0;i<va->dims[0];++i) {
                  mxvc2(vt,j,i).r = mxvc2(va,i,j).r;
                  mxvc2(vt,j,i).i = -mxvc2(va,i,j).i;
               }
            }
         }
         break;
      default:
         SUMA_S_Err("Lazy did not implement this one.\n");
         SUMA_RETURN(NULL);
         break;
   }
   SUMA_RETURN(vt);
}

SUMA_MX_VEC * SUMA_MxVecSetIdentity(SUMA_MX_VEC *thisone)
{
   static char FuncName[]={"SUMA_MxVecSetIdentity"};
   SUMA_ENTRY;
   SUMA_RETURN(SUMA_MxVecIdentity(thisone->tp, thisone->N_dims, thisone->dims, thisone));
}

SUMA_MX_VEC * SUMA_MxVecIdentity(SUMA_VARTYPE tp, int n, int dims[], SUMA_MX_VEC *thisone)
{
   static char FuncName[]={"SUMA_MxVecIdentity"};
   SUMA_MX_VEC *vi=NULL;
   int i,j;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (n != 2) {
      SUMA_S_Err("Function only for matrices");
      SUMA_RETURN(vi);
   }
   if (tp != SUMA_double && tp != SUMA_complex) {
      SUMA_S_Err("Function only for double or complex matrices");
      SUMA_RETURN(vi);
   }

   if (thisone) {
      if (thisone->tp != tp) {
         SUMA_S_Err("Incompatible type for recycled vector");
         SUMA_RETURN(vi);
      }
      if (thisone->N_dims != n) {
         SUMA_S_Err("Incompatible number of dimensions for recycled vector");
         SUMA_RETURN(vi);
      }
      if (thisone->dims[0] != dims[0]) {
         SUMA_S_Err("Incompatible 0th dimensions for recycled vector");
         SUMA_RETURN(vi);
      }
      if (thisone->dims[1] != dims[1]) {
         SUMA_S_Err("Incompatible 1st dimensions for recycled vector");
         SUMA_RETURN(vi);
      }
      vi = thisone;
   } else {
      vi = SUMA_NewMxVec(tp, n, dims, 1);
   }
   switch (tp) {
      case SUMA_double:
         for (i=0; i<dims[0]; ++i) {
            for (j=0; j<dims[0]; ++j) {
               if (i==j) mxvd2(vi, i, j) = 1.0;
               else mxvd2(vi, i, j) = 0.0;
            }
         }
         break;
      case SUMA_complex:
         for (i=0; i<dims[0]; ++i) {
            for (j=0; j<dims[0]; ++j) {
               if (i==j) mxvc2(vi, i, j).r = 1.0;
               else mxvc2(vi, i, j).r = 0.0;
               mxvc2(vi, i, j).i = 0.0;
            }
         }
         break;
      default:
         SUMA_S_Err("Bad type Raul");
         break;
   }

   SUMA_RETURN(vi);
}

int SUMA_MxVecSameDims(SUMA_MX_VEC *va,SUMA_MX_VEC *vb)
{
   static char FuncName[]={"SUMA_MxVecSameDims"};
   int i;
   SUMA_ENTRY;
   if (va->N_dims != vb->N_dims) SUMA_RETURN(0);
   for (i=0;i<va->N_dims; ++i) if (va->dims[i]!=vb->dims[i]) SUMA_RETURN(0);
   SUMA_RETURN(1);
}
int SUMA_MxVecSameDims2(int N_dims, int *dims, SUMA_MX_VEC *va)
{
   static char FuncName[]={"SUMA_MxVecSameDims2"};
   int i;
   SUMA_ENTRY;
   if (va->N_dims != N_dims) SUMA_RETURN(0);
   for (i=0;i<va->N_dims; ++i) if (va->dims[i]!=dims[i]) SUMA_RETURN(0);
   SUMA_RETURN(1);
}

SUMA_MX_VEC * SUMA_MxVecCopy(SUMA_MX_VEC *va, SUMA_MX_VEC *recycle)
{
   static char FuncName[]={"SUMA_MxVecCopy"};
   SUMA_MX_VEC *vi=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (recycle) {
      if (recycle->tp != va->tp || !SUMA_MxVecSameDims(va,recycle)) {
         SUMA_S_Err("Bad recycled MxVec");
         SUMA_RETURN(NULL);
      }
      vi = recycle;
   } else {
      vi = SUMA_NewMxVec(va->tp, va->N_dims, va->dims, 1);
   }
   memcpy(vi->v, va->v, va->N_vals*SUMA_SizeOf(va->tp));

   SUMA_RETURN(vi);
}

SUMA_MX_VEC * SUMA_MxVecRand(SUMA_VARTYPE tp, int N_dims, int *dims, SUMA_MX_VEC *recycle)
{
   static char FuncName[]={"SUMA_MxVecRand"};
   SUMA_MX_VEC *vi=NULL;
   int i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (recycle) {
      if (recycle->tp != tp || !SUMA_MxVecSameDims2(N_dims, dims,recycle)) {
         SUMA_S_Err("Bad recycled MxVec");
         SUMA_RETURN(NULL);
      }
      vi = recycle;
   } else {
      vi = SUMA_NewMxVec(tp, N_dims, dims, 1);
   }

   switch (tp){
      case SUMA_complex:
         for (i=0; i<vi->N_vals; ++i) { mxvc1(vi, i).r = (float)rand()/(float)RAND_MAX; mxvc1(vi, i).i = (float)rand()/(float)RAND_MAX; }
         break;
      case SUMA_double:
         for (i=0; i<vi->N_vals; ++i) mxvd1(vi, i) = (double)rand()/(double)RAND_MAX;
         break;
      default:
         SUMA_S_Err("MxVec Type not supported");
         vi = SUMA_FreeMxVec(vi);
         break;
   }
   SUMA_RETURN(vi);
}

/*!
   \brief swap two rows in a matrix (not very efficient because matrix is stored column_major (fdf = 1)
*/
int SUMA_MxVecSwapRows(SUMA_MX_VEC *va, int r1, int r2)
{
   static char FuncName[]={"SUMA_MxVecSwapRows"};
   int j;
   double d;
   complex c;

   SUMA_ENTRY;

   if (r1 >= va->dims[0] || r2 >= va->dims[0]) {
      SUMA_S_Err("Rows exceed matrix dimension");
      SUMA_RETURN(0);
   }

   if (va->tp != SUMA_double && va->tp != SUMA_complex) {
      SUMA_S_Err("No type support");
      SUMA_RETURN(0);
   }


   switch(va->tp) {
      case SUMA_double:
         for (j=0;j<va->dims[1];++j) {
            d = mxvd2(va, r1, j);
            mxvd2(va, r1, j) = mxvd2(va, r2, j);
            mxvd2(va, r2, j) = d;
         }
         break;
      case SUMA_complex:
         for (j=0;j<va->dims[1];++j) {
            c.r = mxvc2(va, r1, j).r;
            c.i = mxvc2(va, r1, j).i;
            mxvc2(va, r1, j).r = mxvc2(va, r2, j).r;
            mxvc2(va, r1, j).i = mxvc2(va, r2, j).i;
            mxvc2(va, r2, j).r = c.r;
            mxvc2(va, r2, j).i = c.i;
         }
         break;
      default:
         SUMA_S_Err("Should not be here");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

/*!
   \brief Return the inverse of MxVec
*/
SUMA_MX_VEC * SUMA_MxVecInverse(SUMA_MX_VEC *va, SUMA_MX_VEC *recycle)
{
   static char FuncName[]={"SUMA_MxVecInverse"};
   SUMA_MX_VEC *ainv=NULL, *tmp=NULL;
   matrix a, c;
   int i, j=0, ii, n;
   double fmax, fval;
   const double epsilon = 1.0e-10;
   complex cval, cmult;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (LocalHead) SUMA_etime2(FuncName, NULL, NULL);

   if (va->dims[0] != va->dims[1]) {
      SUMA_S_Err("Cannot invert non square matrix");
      SUMA_RETURN(ainv);
   }

   #if 0    /* not much faster using matrix functions...*/
   if (recycle) {
      SUMA_S_Err("No recycling in this case");
      SUMA_RETURN(ainv);
   }
   if (va->tp == SUMA_double) {
      if (!va->m) {
         SUMA_MxVecBuildMat(va);
         if (LocalHead) SUMA_ShowMxVec(va,va->N_vals, NULL, "\ndbg va\n");
      }
      if (LocalHead) SUMA_etime2(FuncName, "Matrix build time",FuncName);
      if (!va->m) {
         SUMA_S_Err("Failed to create matrix");
         SUMA_RETURN(ainv);
      }
      a = *(va->m);
      matrix_initialize (&c);
      if (!matrix_inverse (a,&c)) {
         SUMA_S_Err("Failed in inverse");
         SUMA_RETURN(ainv);
      }
      if (LocalHead) SUMA_etime2(FuncName, "Matrix inverse time",FuncName);
      SUMA_LH("Going from matrix to vector");
      ainv = SUMA_matrix2MxVec(c); /* c is destroyed inside SUMA_matrix2MxVec */
      if (LocalHead) SUMA_etime2(FuncName, "Matrix copy time",FuncName);
      SUMA_S_Note("va now contains duplicate of vector data in m");
   } else {
      SUMA_S_Err("Not ready yet ");
   }
   SUMA_RETURN(ainv);
   #else
   /* do it in house (after matrix_inverse)*/
   n = va->dims[0];
   if (recycle) {
      SUMA_LH("Reusing vector");
      if (recycle->tp != va->tp) {
         SUMA_S_Errv("Recycled vector of type %d, type %d needed.\n", recycle->tp, va->tp);
         SUMA_RETURN(ainv);
      }
      if (recycle->dims[0] != va->dims[0]) {
         SUMA_S_Errv("Recycled vector dims[0]=%d, dims[0]=%d needed.\n", recycle->dims[0], va->dims[0]);
         SUMA_RETURN(ainv);
      }
      if (recycle->dims[1] != va->dims[1]) {
         SUMA_S_Errv("Recycled vector dims[1]=%d, dims[1]=%d needed.\n", recycle->dims[1], va->dims[1]);
         SUMA_RETURN(ainv);
      }
      ainv = recycle;
      if (!(ainv = SUMA_MxVecSetIdentity(ainv))) {
         SUMA_S_Err("Failed to set identity");
         SUMA_RETURN(ainv);
      }
   } else {
      ainv = SUMA_MxVecIdentity(va->tp, 2, va->dims, NULL);
   }
   tmp = SUMA_MxVecCopy(va, NULL);

   switch (va->tp) {
      case SUMA_double:
         for (i = 0;  i < n;  i++) {
            fmax = fabs(mxvd2(tmp,i,i));
            for (j = i+1;  j < n;  j++) {
               if (fabs(mxvd2(tmp,j,i)) > fmax) {
                  fmax = fabs(mxvd2(tmp,j,i));
                  SUMA_MxVecSwapRows(tmp, i, j);
                  SUMA_MxVecSwapRows(ainv, i, j);
               }
            }
            if (fmax < epsilon) {
               SUMA_S_Err("Near singular or badly scaled");
               ainv = SUMA_FreeMxVec(ainv);
               SUMA_RETURN(ainv);
            }
            fval = 1.0 / mxvd2(tmp,i,i);   /* RWCox: change division by this to */
            for (j = 0;  j < n;  j++)  {   /*        multiplication by 1.0/this */
	            mxvd2(tmp,i,j) *= fval;
	            mxvd2(ainv,i,j) *= fval;
	         }
            for (ii = 0;  ii < n;  ii++) {
	            if (ii != i) {
	               fval = mxvd2(tmp,ii,i);
	               for (j = 0;  j < n;  j++) {
		               mxvd2(tmp,ii,j) -= fval*mxvd2(tmp,i,j);
		               mxvd2(ainv,ii,j) -= fval*mxvd2(ainv,i,j);
	               }
	            }
            }
         }
         break;
      case SUMA_complex:
         for (i = 0;  i < n;  i++) {
            fmax = SUMA_COMPLEX_ABS(mxvc2(tmp,i,i));
            for (j = i+1;  j < n;  j++) {
               if (SUMA_COMPLEX_ABS(mxvc2(tmp,j,i)) > fmax) {
                  fmax = SUMA_COMPLEX_ABS(mxvc2(tmp,j,i));
                  SUMA_MxVecSwapRows(tmp, i, j);
                  SUMA_MxVecSwapRows(ainv, i, j);
               }
            }
            if (fmax < epsilon) {
               SUMA_S_Err("Near singular or badly scaled");
               ainv = SUMA_FreeMxVec(ainv);
               SUMA_RETURN(ainv);
            }
            SUMA_COMPLEX_INV(mxvc2(tmp,i,i), cval);
            for (j = 0;  j < n;  j++)  {
	            cmult.r = mxvc2(tmp,i,j).r;
               cmult.i = mxvc2(tmp,i,j).i;
               SUMA_COMPLEX_MULT(cmult, cval, mxvc2(tmp,i,j));
	            cmult.r = mxvc2(ainv,i,j).r;
               cmult.i = mxvc2(ainv,i,j).i;
               SUMA_COMPLEX_MULT(cmult, cval, mxvc2(ainv,i,j));
	         }
            for (ii = 0;  ii < n;  ii++) {
	            if (ii != i) {
	               cval = mxvc2(tmp,ii,i);
	               for (j = 0;  j < n;  j++) {
		               SUMA_COMPLEX_MULT(cval, mxvc2(tmp,i,j), cmult);
                     mxvc2(tmp,ii,j).r = mxvc2(tmp,ii,j).r - cmult.r;
                     mxvc2(tmp,ii,j).i = mxvc2(tmp,ii,j).i - cmult.i;
		               SUMA_COMPLEX_MULT(cval, mxvc2(ainv,i,j), cmult);
                     mxvc2(ainv,ii,j).r = mxvc2(ainv,ii,j).r - cmult.r;
                     mxvc2(ainv,ii,j).i = mxvc2(ainv,ii,j).i - cmult.i;
	               }
	            }
            }
         }
         break;
      default:
         SUMA_S_Err("Unsupported type");
         SUMA_RETURN(SUMA_FreeMxVec(ainv));
         break;
   }
   tmp = SUMA_FreeMxVec(tmp);
   if (LocalHead) SUMA_etime2(FuncName, "Matrix inverse time", FuncName);
   SUMA_RETURN(ainv);
   #endif
}

/*!
   \brief Multiply two MX_VEC matrices where one dimension is much larger than the other
   Main difference with SUMA_MxVecMult is that here, the sum is
   mxvd2(vp, i,j) += mxvd2(vt,k,i) * mxvd2(vb, k,j);
   instead of
   mxvd2(vp, i,j) += mxvd2(va,i,k) * mxvd2(vb, k,j);

*/
SUMA_MX_VEC *SUMA_MxVecMultRect(SUMA_MX_VEC *va, SUMA_MX_VEC *vb, SUMA_MX_VEC *recycle, int InfoMask)
{
   static char FuncName[]={"SUMA_MxVecMultRect"};
   SUMA_ENTRY;
   SUMA_RETURN(SUMA_MxVecMultRect_Engine(va, vb, recycle, NULL, NULL, InfoMask));
}
/*! Passing transpose of va (if you need it elsewhere) would save on waisted computations. vbt is useless */
SUMA_MX_VEC *SUMA_MxVecMultRect_Engine(SUMA_MX_VEC *va, SUMA_MX_VEC *vb, SUMA_MX_VEC *recycle, SUMA_MX_VEC *vat, SUMA_MX_VEC *vbt, int InfoMask)
{
   static char FuncName[]={"SUMA_MxVecMultRect_Engine"};
   SUMA_MX_VEC *vp=NULL, *vt=NULL;
   SUMA_VARTYPE tp=SUMA_notypeset;
   int dims[2], N_dims, i, j, k;
   complex ctmp;
   struct timeval tt;
   int symm = 0;
   matrix a, b, c;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!va || va->N_dims != 2 || !vb || vb->N_dims != 2) {
      SUMA_S_Err("inappropriate");
      SUMA_RETURN(NULL);
   }

   SUMA_LHv("Fast rectangular matrix multiplication mode. va is %d x %d\n", va->dims[0], va->dims[1]);

   SUMA_etime2(FuncName, NULL, NULL);

   if (!vat && InfoMask & MATRIX_B_IS_AT) vat = vb;

   if (!vat) vt = SUMA_MxVecTranspose(va, NULL);
   else vt = vat;

   if (InfoMask & MATRIX_B_IS_AT || InfoMask & MATRIX_OUT_SYMMETRIC) {
      symm = 1;
   } else {
      symm = 0;
   }

   if (  va->N_dims > 2 || va->N_dims < 1 ||
         vb->N_dims > 2 || vb->N_dims < 1 ) {
      SUMA_S_Err("Bad N_dims");
      if (vt != vat) vt = SUMA_FreeMxVec(vt);
      SUMA_RETURN(vp);
   }
   if (va->fdf != 1 || vb->fdf != 1) {
      SUMA_S_Err("Only first dimension first");
      if (vt != vat) vt = SUMA_FreeMxVec(vt);
      SUMA_RETURN(vp);
   }
   if (va->N_dims == 1) va->dims[1] = 1;
   if (vb->N_dims == 1) vb->dims[1] = 1;
   if (va->dims[1] != vb->dims[0]) {
      SUMA_S_Err("Incompatible dimensions for matrix multiplications");
      if (vt != vat) vt = SUMA_FreeMxVec(vt);
      SUMA_RETURN(vp);
   }
   if (  (vb->tp != SUMA_complex && vb->tp != SUMA_double) ||
         (va->tp != SUMA_complex && va->tp != SUMA_double) ) {
      SUMA_S_Err("vectors must either be complex or double");
      if (vt != vat) vt = SUMA_FreeMxVec(vt);
      SUMA_RETURN(vp);
   }

   dims[0] = va->dims[0]; dims[1] = vb->dims[1];
   if (va->tp == SUMA_complex || vb->tp == SUMA_complex) {
      tp = SUMA_complex;
   } else if (va->tp == SUMA_double && vb->tp == SUMA_double) {
      tp = SUMA_double;
   } else {
      SUMA_S_Err("vectors must either be complex or double");
      if (vt != vat) vt = SUMA_FreeMxVec(vt);
      SUMA_RETURN(vp);
   }
   SUMA_LHv("tp=%d, dims[0]=%d, dims[1]=%d\n", tp, dims[0], dims[1]);
   if (recycle) {
      SUMA_LH("Reusing vector");
      if (recycle->tp != tp) {
         SUMA_S_Errv("Recycled vector of type %d, type %d needed.\n", recycle->tp, tp);
         if (vt != vat) vt = SUMA_FreeMxVec(vt);
         SUMA_RETURN(vp);
      }
      if (recycle->dims[0] != dims[0]) {
         SUMA_S_Errv("Recycled vector dims[0]=%d, dims[0]=%d needed.\n", recycle->dims[0], dims[0]);
         if (vt != vat) vt = SUMA_FreeMxVec(vt);
         SUMA_RETURN(vp);
      }
      if (recycle->dims[1] != dims[1]) {
         SUMA_S_Errv("Recycled vector dims[1]=%d, dims[1]=%d needed.\n", recycle->dims[1], dims[1]);
         if (vt != vat) vt = SUMA_FreeMxVec(vt);
         SUMA_RETURN(vp);
      }
      vp = recycle;
   } else {
      vp = SUMA_NewMxVec(tp, 2, dims, 1);
   }
   if (!vp) {
      SUMA_S_Err("Failed to create output vector");
      if (vt != vat) vt = SUMA_FreeMxVec(vt);
      SUMA_RETURN(vp);
   }
   switch(tp) {
      case SUMA_complex:
         if (vb->tp == va->tp) { /* both are complex */
            SUMA_LH("Both complex");
            if (symm) {
               for (i = 0;  i < vp->dims[0];  i++) {
                  for (j = 0;  j < vp->dims[1];  j++) {
                     if (j<i) {
                        mxvc2(vp, i,j).r = mxvc2(vp, j,i).r ;
                        mxvc2(vp, i,j).i = mxvc2(vp, j,i).i ;
                     } else {
                        mxvc2(vp, i,j).r = 0.0 ;
                        mxvc2(vp, i,j).i = 0.0 ;
	                     for (k = 0;  k < va->dims[1];  k++) {
	                        SUMA_COMPLEX_MULT(mxvc2(vt,k,i), mxvc2(vb, k,j), ctmp);
                           mxvc2(vp, i,j).r += ctmp.r;
                           mxvc2(vp, i,j).i += ctmp.i;
                        }
                     }
                  }
               }
            } else {
               for (i = 0;  i < vp->dims[0];  i++) {
                  for (j = 0;  j < vp->dims[1];  j++) {
                     mxvc2(vp, i,j).r = 0.0 ;
                     mxvc2(vp, i,j).i = 0.0 ;
	                  for (k = 0;  k < va->dims[1];  k++) {
	                     SUMA_COMPLEX_MULT(mxvc2(vt,k,i), mxvc2(vb, k,j), ctmp);
                        mxvc2(vp, i,j).r += ctmp.r;
                        mxvc2(vp, i,j).i += ctmp.i;
                     }
                  }
               }
            }
         } else { /* only one is complex */
            if (va->tp == SUMA_complex) {
               SUMA_LH("va complex");
               if (symm) {
                  for (i = 0;  i < vp->dims[0];  i++) {
                     for (j = 0;  j < vp->dims[1];  j++) {
                        if (j<i) {
                           mxvc2(vp, i,j).r = mxvc2(vp, j,i).r ;
                           mxvc2(vp, i,j).i = mxvc2(vp, j,i).i ;
                        } else {
                           mxvc2(vp, i,j).r = 0.0 ;
                           mxvc2(vp, i,j).i = 0.0 ;
	                        for (k = 0;  k < va->dims[1];  k++) {
	                           SUMA_COMPLEX_SCALE(mxvc2(vt,k,i), mxvd2(vb, k,j), ctmp);
                              mxvc2(vp, i,j).r += ctmp.r;
                              mxvc2(vp, i,j).i += ctmp.i;
                           }
                        }
                     }
                  }
               } else {
                  for (i = 0;  i < vp->dims[0];  i++) {
                     for (j = 0;  j < vp->dims[1];  j++) {
                        mxvc2(vp, i,j).r = 0.0 ;
                        mxvc2(vp, i,j).i = 0.0 ;
	                     for (k = 0;  k < va->dims[1];  k++) {
	                        SUMA_COMPLEX_SCALE(mxvc2(vt,k,i), mxvd2(vb, k,j), ctmp);
                           mxvc2(vp, i,j).r += ctmp.r;
                           mxvc2(vp, i,j).i += ctmp.i;
                        }
                     }
                  }
               }
            } else {
               SUMA_LH("vb complex");
               if (symm) {
                  for (i = 0;  i < vp->dims[0];  i++) {
                     for (j = 0;  j < vp->dims[1];  j++) {
                        if (j<i) {
                           mxvc2(vp, i,j).r =  mxvc2(vp, j,i).r;
                           mxvc2(vp, i,j).i =  mxvc2(vp, j,i).i;
                        } else {
                           mxvc2(vp, i,j).r = 0.0 ;
                           mxvc2(vp, i,j).i = 0.0 ;
	                        for (k = 0;  k < va->dims[1];  k++) {
	                           SUMA_COMPLEX_SCALE(mxvc2(vb, k,j), mxvd2(vt,k,i), ctmp);
                              mxvc2(vp, i,j).r += ctmp.r;
                              mxvc2(vp, i,j).i += ctmp.i;
                           }
                        }
                     }
                  }
               } else {
                  for (i = 0;  i < vp->dims[0];  i++) {
                     for (j = 0;  j < vp->dims[1];  j++) {
                        mxvc2(vp, i,j).r = 0.0 ;
                        mxvc2(vp, i,j).i = 0.0 ;
	                     for (k = 0;  k < va->dims[1];  k++) {
	                        SUMA_COMPLEX_SCALE(mxvc2(vb, k,j), mxvd2(vt,k,i), ctmp);
                           mxvc2(vp, i,j).r += ctmp.r;
                           mxvc2(vp, i,j).i += ctmp.i;
                        }
                     }
                  }
               }
            }
         }
         break;
      case SUMA_double:
         {
            {
                  if (LocalHead) SUMA_etime2(FuncName, "Fast Matrix transpose time", FuncName);
                  if (symm) {
                     for (i = 0;  i < vp->dims[0];  i++) {
                        for (j = 0;  j < vp->dims[1];  j++) {
                           if(j<i) {
                              mxvd2(vp, i,j) = mxvd2(vp, j,i);
                           } else {
                              mxvd2(vp, i,j) = 0.0 ;
	                           for (k = 0;  k < va->dims[1];  k++) {
                                 mxvd2(vp, i,j) += mxvd2(vt,k,i) * mxvd2(vb, k,j);
                              }
                           }
                        }
                     }
                  } else {
                     for (i = 0;  i < vp->dims[0];  i++) {
                        for (j = 0;  j < vp->dims[1];  j++) {
                           mxvd2(vp, i,j) = 0.0 ;
	                        for (k = 0;  k < va->dims[1];  k++) {
                              mxvd2(vp, i,j) += mxvd2(vt,k,i) * mxvd2(vb, k,j);
                           }
                        }
                     }
                  }

                  if (LocalHead) SUMA_etime2(FuncName, "Fast Matrix Mult. time", FuncName);
            }
         }

         break;
      default:
         SUMA_S_Err("Bad types");
         SUMA_RETURN(NULL);
   }

   if (vt != vat) vt = SUMA_FreeMxVec(vt);

   SUMA_RETURN(vp);
}

/*!
   \brief Multiply two MX_VEC matrices
*/
SUMA_MX_VEC *SUMA_MxVecMult(SUMA_MX_VEC *va, SUMA_MX_VEC *vb, SUMA_MX_VEC *recycle, int InfoMask)
{
   static char FuncName[]={"SUMA_MxVecMult"};
   SUMA_ENTRY;
   SUMA_RETURN(SUMA_MxVecMult_Engine(va, vb, recycle, NULL, NULL, InfoMask));
}
/*!
   if va->dims[0] << va->dims[1], passing vat also would speed things up . vbt is useless
*/
SUMA_MX_VEC *SUMA_MxVecMult_Engine(SUMA_MX_VEC *va, SUMA_MX_VEC *vb, SUMA_MX_VEC *recycle, SUMA_MX_VEC *vat, SUMA_MX_VEC *vbt, int InfoMask)
{
   static char FuncName[]={"SUMA_MxVecMult_Engine"};
   SUMA_MX_VEC *vp=NULL;
   SUMA_VARTYPE tp=SUMA_notypeset;
   int dims[2], symm, N_dims, i, j, k, ij, ik, kj;
   complex ctmp;
   struct timeval tt;
   matrix a, b, c;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if ( va->N_dims == 2 && ((float)va->dims[1]/(float)va->dims[0] > 100 && va->dims[1] > 1000)) {
      SUMA_LH("The fast mode");
      SUMA_RETURN(SUMA_MxVecMultRect_Engine(va, vb,recycle, vat, vbt, InfoMask));
      SUMA_LH("***************OUT");
   }

   if (InfoMask & MATRIX_B_IS_AT || InfoMask & MATRIX_OUT_SYMMETRIC) {
      symm = 1;
      SUMA_S_Note("No optimization for symmetric output yet!");
   } else {
      symm = 0;
   }
   if (!vb) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NULL);
   }
   if (LocalHead) SUMA_etime2(FuncName, NULL, NULL);

   if (  va->N_dims > 2 || va->N_dims < 1 ||
         vb->N_dims > 2 || vb->N_dims < 1 ) {
      SUMA_S_Err("Bad N_dims");
      SUMA_RETURN(vp);
   }
   if (va->fdf != 1 || vb->fdf != 1) {
      SUMA_S_Err("Only first dimension first");
      SUMA_RETURN(vp);
   }
   if (va->N_dims == 1) va->dims[1] = 1;
   if (vb->N_dims == 1) vb->dims[1] = 1;
   if (va->dims[1] != vb->dims[0]) {
      SUMA_S_Err("Incompatible dimensions for matrix multiplications");
      SUMA_RETURN(vp);
   }
   if (  (vb->tp != SUMA_complex && vb->tp != SUMA_double) ||
         (va->tp != SUMA_complex && va->tp != SUMA_double) ) {
      SUMA_S_Err("vectors must either be complex or double");
      SUMA_RETURN(vp);
   }

   dims[0] = va->dims[0]; dims[1] = vb->dims[1];
   if (va->tp == SUMA_complex || vb->tp == SUMA_complex) {
      tp = SUMA_complex;
   } else if (va->tp == SUMA_double && vb->tp == SUMA_double) {
      tp = SUMA_double;
   } else {
      SUMA_S_Err("vectors must either be complex or double");
      SUMA_RETURN(vp);
   }
   SUMA_LHv("tp=%d, dims[0]=%d, dims[1]=%d\n", tp, dims[0], dims[1]);
   if (recycle) {
      SUMA_LH("Reusing vector");
      if (recycle->tp != tp) {
         SUMA_S_Errv("Recycled vector of type %d, type %d needed.\n", recycle->tp, tp);
         SUMA_RETURN(vp);
      }
      if (recycle->dims[0] != dims[0]) {
         SUMA_S_Errv("Recycled vector dims[0]=%d, dims[0]=%d needed.\n", recycle->dims[0], dims[0]);
         SUMA_RETURN(vp);
      }
      if (recycle->dims[1] != dims[1]) {
         SUMA_S_Errv("Recycled vector dims[1]=%d, dims[1]=%d needed.\n", recycle->dims[1], dims[1]);
         SUMA_RETURN(vp);
      }
      vp = recycle;
   } else {
      vp = SUMA_NewMxVec(tp, 2, dims, 1);
   }
   if (!vp) {
      SUMA_S_Err("Failed to create output vector");
      SUMA_RETURN(vp);
   }
   switch(tp) {
      case SUMA_complex:
         if (vb->tp == va->tp) { /* both are complex */
            SUMA_LH("Both complex");
            for (j = 0;  j < vp->dims[1];  j++) {
               for (i = 0;  i < vp->dims[0];  i++) {
                  mxvc2(vp, i,j).r = 0.0 ;
                  mxvc2(vp, i,j).i = 0.0 ;
	               for (k = 0;  k < va->dims[1];  k++) {
	                  SUMA_COMPLEX_MULT(mxvc2(va,i,k), mxvc2(vb, k,j), ctmp);
                     mxvc2(vp, i,j).r += ctmp.r;
                     mxvc2(vp, i,j).i += ctmp.i;
                  }
               }
            }
         } else { /* only one is complex */
            if (va->tp == SUMA_complex) {
               SUMA_LH("va complex");
               for (j = 0;  j < vp->dims[1];  j++) {
                  for (i = 0;  i < vp->dims[0];  i++) {
                     mxvc2(vp, i,j).r = 0.0 ;
                     mxvc2(vp, i,j).i = 0.0 ;
	                  for (k = 0;  k < va->dims[1];  k++) {
	                     SUMA_COMPLEX_SCALE(mxvc2(va,i,k), mxvd2(vb, k,j), ctmp);
                        mxvc2(vp, i,j).r += ctmp.r;
                        mxvc2(vp, i,j).i += ctmp.i;
                     }
                  }
               }
            } else {
               SUMA_LH("vb complex");
               for (j = 0;  j < vp->dims[1];  j++) {
                  for (i = 0;  i < vp->dims[0];  i++) {
                     mxvc2(vp, i,j).r = 0.0 ;
                     mxvc2(vp, i,j).i = 0.0 ;
	                  for (k = 0;  k < va->dims[1];  k++) {
	                     SUMA_COMPLEX_SCALE(mxvc2(vb, k,j), mxvd2(va,i,k), ctmp);
                        mxvc2(vp, i,j).r += ctmp.r;
                        mxvc2(vp, i,j).i += ctmp.i;
                     }
                  }
               }
            }
         }
         break;
      case SUMA_double:
         #if 0 /* using matrix functions, not much faster */
         if (recycle) {
            /* kill the recycled puppy BAD BAD BAD */
            SUMA_S_Warn("Carefull, with that one! Caller may not know this happened");
            vp = SUMA_FreeMxVec(vp);
         }
         if (!va->m) {
            SUMA_MxVecBuildMat(va);
            if (LocalHead) SUMA_ShowMxVec(va,va->N_vals, NULL, "\ndbg va\n");
         }
         if (!vb->m) {
            SUMA_MxVecBuildMat(vb);
            if (LocalHead) SUMA_ShowMxVec(vb,vb->N_vals, NULL, "\ndbg vb\n");
         }
         if (LocalHead) SUMA_etime2(FuncName, "Matrix build time",FuncName);
         if (!vb->m || !va->m) {
            SUMA_S_Err("Failed to create matrix");
            SUMA_RETURN(NULL);
         }
         a = *(va->m); b = *(vb->m);
         matrix_initialize(&c);
         matrix_multiply (a,b, &c);
         if (LocalHead) SUMA_etime2(FuncName, "Matrix multiply time",FuncName);
         SUMA_LH("Going from matrix to vector");
         vp = SUMA_matrix2MxVec(c); /* c is destroyed inside SUMA_matrix2MxVec */
         if (LocalHead) SUMA_etime2(FuncName, "Matrix copy time",FuncName);
         SUMA_S_Note("va and vb now contain duplicates of vector data in m");
         #else
         {
            for (j = 0;  j < vp->dims[1];  j++) {     /* j, then i is faster on mac OS X G5, at least for spharm computations */
               for (i = 0;  i < vp->dims[0];  i++) {
                  mxvd2(vp, i,j) = 0.0 ;
                  ij = i+vp->dims[0]*j;
	               for (k = 0;  k < va->dims[1];  k++) {
                     ik = i+va->dims[0]*k;
                     kj = k+vb->dims[0]*j;
                     vp->dv[ij] += va->dv[ik] * vb->dv[kj];
                  }
               }
            }
         }
         #endif
         break;
      default:
         SUMA_S_Err("Bad types");
         SUMA_RETURN(NULL);
   }

   SUMA_RETURN(vp);
}

/*
   a function to test matrix operations using MxVec structures
*/
void SUMA_TestMxVecMatOps(void)
{
   static char FuncName[]={"SUMA_TestMxVecMatOps"};
   SUMA_MX_VEC *da, *db, *dc, *dat, *dbt, *dct;
   int dd[2]={2,3}, N_dims, dims[50];
   struct timeval tt;
   int i, j;
   char stmp[100];
   matrix a, b, c;

   SUMA_ENTRY;
   SUMA_S_Note("Testing matrix speed");


   matrix_initialize(&a);
   matrix_create(60, 40962, &a);
   matrix_initialize(&b);
   matrix_create(40962, 60, &b);
   srand(123);
   for (i=0; i<40962; ++i) {
      for (j=0; j<60;++j) {
         a.elts[j][i] = (double)rand()/(double)RAND_MAX;
         b.elts[i][j] = a.elts[j][i];
      }
   }
   da = SUMA_matrix2MxVec(a);
   db = SUMA_matrix2MxVec(b);
   SUMA_ShowMxVec(da, 1, NULL, "\nInitial da\n");
   SUMA_etime2(FuncName, NULL, NULL);
   dat = SUMA_MxVecTranspose(da, NULL);
   da = SUMA_FreeMxVec(da);
   SUMA_etime2(FuncName, "Vector Transpose 1(60*40962)", FuncName);
   da = SUMA_MxVecTranspose(dat, NULL);
   dat = SUMA_FreeMxVec(dat);
   SUMA_etime2(FuncName, "Vector Transpose 2(40962*60)", FuncName);
   SUMA_ShowMxVec(da, 1, NULL, "\n(da')'\n");
   SUMA_etime2(FuncName, "Next is multiplication.", FuncName);
   dc = SUMA_MxVecMult(da, db, NULL, MATRIX_B_IS_AT);
   SUMA_etime2(FuncName, "Vector multiplication test (60*40962 X 40962 * 60)", FuncName);
   SUMA_ShowMxVec(dc, 1, NULL, "\nMult via MxVec\n");
   dc = SUMA_FreeMxVec(dc);
   dc = SUMA_MxVecMult(da, db, NULL, MATRIX_OUT_SYMMETRIC);   /* try the a ainv trick */
   SUMA_etime2(FuncName, "Vector multiplication test (60*40962 X 40962 * 60)", FuncName);
   SUMA_ShowMxVec(dc, 1, NULL, "\nMult via MxVec, mode 2\n");

   matrix_initialize(&c);
   SUMA_S_Note("Testing matrix speed with 'matrix' calls");
   if (!da->m) SUMA_MxVecBuildMat(da);
   if (!db->m) SUMA_MxVecBuildMat(db);
   a = *(da->m); b = *(db->m);
   matrix_multiply(a, b, &c);
   SUMA_etime2(FuncName, "Vector multiplication test 2 (60*40962 X 40962 * 60)", FuncName);
   SUMA_S_Notev("c is (%d x %d)\n", c.rows, c.cols);
   da = SUMA_FreeMxVec(da);
   db = SUMA_FreeMxVec(db);
   dc = SUMA_FreeMxVec(dc);
   dc = SUMA_matrix2MxVec(c);
   SUMA_ShowMxVec(dc, 1, NULL, "\nMult via 'matrix'\n");
   dc = SUMA_FreeMxVec(dc);

   matrix_initialize(&a);
   matrix_create(129, 129, &a);
   matrix_initialize(&b);
   matrix_create(129, 40962, &b);
   srand(123);
   for (i=0; i<129; ++i) {
      for (j=0; j<129;++j) {
         a.elts[i][j] = (double)rand()/(double)RAND_MAX;
      }
   }
   for (i=0; i<40962; ++i) {
      for (j=0; j<129;++j) {
         b.elts[j][i] = (double)rand()/(double)RAND_MAX;
      }
   }
   da = SUMA_matrix2MxVec(a);
   db = SUMA_matrix2MxVec(b);
   SUMA_etime2(FuncName, NULL, NULL);
   dc = SUMA_MxVecMult(da, db, NULL, 0);
   SUMA_etime2(FuncName, "Vector multiplication test 3 (129*129 X 129 * 40962 )", FuncName);
   SUMA_ShowMxVec(dc, 1, NULL, "\nMult via MxVec\n");
   dc = SUMA_FreeMxVec(dc);
   dbt = SUMA_MxVecTranspose(db, NULL);
   dat = SUMA_MxVecTranspose(da, NULL);
   SUMA_etime2(FuncName, "Vector multiplication test 4 (built transposes)(129*129 X 129 * 40962 )", FuncName);
   dct = SUMA_MxVecMult(dbt, dat, NULL, 0);
   dc = SUMA_MxVecTranspose(dct, NULL);
   SUMA_etime2(FuncName, "Vector multiplication test 4 (129*129 X 129 * 40962 )", FuncName);
   SUMA_ShowMxVec(dc, 1, NULL, "\nMult via tranposed MxVec\n");
   da = SUMA_FreeMxVec(da);
   db = SUMA_FreeMxVec(db);
   dc = SUMA_FreeMxVec(dc);
   dat = SUMA_FreeMxVec(dat);
   dbt = SUMA_FreeMxVec(dbt);
   dct = SUMA_FreeMxVec(dct);


   SUMA_RETURNe;

   SUMA_S_Note("Testing transpose");
   da = SUMA_NewMxVec(SUMA_double, 2, dd, 1);
   mxvd2(da,0,0) = 1.0;
   mxvd2(da,0,1) = 2.0;
   mxvd2(da,0,2) = 3.0;
   mxvd2(da,1,0) = -1.0;
   mxvd2(da,1,1) = -4.0;
   mxvd2(da,1,2) = -3.0;
   db = SUMA_MxVecTranspose(da, NULL);
   SUMA_ShowMxVec(da, da->N_vals, NULL, "\nda\n");
   SUMA_ShowMxVec(db, db->N_vals, NULL, "\ntransp(da)\n");

   SUMA_S_Note("Testing multiplication");
   mxvd2(db,0,0) = 1.5;
   mxvd2(db,0,1) = 2.3;
   mxvd2(db,1,0) = 3.1;
   mxvd2(db,1,1) = -1.3;
   mxvd2(db,2,0) = -2.8;
   mxvd2(db,2,1) = -3.1;
   SUMA_ShowMxVec(da, da->N_vals, NULL, "\nda\n");
   SUMA_ShowMxVec(db, db->N_vals, NULL, "\ndb\n");
   dc = SUMA_MxVecMult(da, db, NULL, 0);
   SUMA_ShowMxVec(dc, dc->N_vals, NULL, "\nda*db\n");

   SUMA_S_Note("Testing Real Inversion\n");
   db = SUMA_FreeMxVec(db);
   db = SUMA_MxVecInverse(dc, NULL);
   SUMA_ShowMxVec(dc, dc->N_vals, NULL, "\ndc\n");
   SUMA_ShowMxVec(db, db->N_vals, NULL, "\ninv(dc)\n");

   SUMA_S_Note("Testing complex multiplication, b complex");
   db = SUMA_FreeMxVec(db);
   dc = SUMA_FreeMxVec(dc);
   dd[0]= 3; dd[1] = 2;
   db = SUMA_NewMxVec(SUMA_complex, 2, dd, 1);
   mxvc2(db,0,0).r = 1.5;
   mxvc2(db,0,1).r = 2.3;
   mxvc2(db,1,0).r = 3.1;
   mxvc2(db,1,1).r = -1.3;
   mxvc2(db,2,0).r = -2.8;
   mxvc2(db,2,1).r = -3.1;
   mxvc2(db,0,0).i = 3.5;
   mxvc2(db,0,1).i = 6.3;
   mxvc2(db,1,0).i = 9.1;
   mxvc2(db,1,1).i = 3.3;
   mxvc2(db,2,0).i = 2.8;
   mxvc2(db,2,1).i = -6.1;
   SUMA_ShowMxVec(da, da->N_vals, NULL, "\nda\n");
   SUMA_ShowMxVec(db, db->N_vals, NULL, "\ndb\n");
   dc = SUMA_MxVecMult(da, db, NULL,0);
   SUMA_ShowMxVec(dc, dc->N_vals, NULL, "\nda*db\n");

   SUMA_S_Note("Testing complex multiplication, a,b complex");
   da = SUMA_FreeMxVec(da);
   dc = SUMA_FreeMxVec(dc);
   dd[0]= 2; dd[1] = 3;
   da = SUMA_NewMxVec(SUMA_complex, 2, dd, 1);
   mxvc2(da,0,0).r = 1.0;
   mxvc2(da,0,1).r = 2.0;
   mxvc2(da,0,2).r = 3.0;
   mxvc2(da,1,0).r = -1.0;
   mxvc2(da,1,1).r = -2.0;
   mxvc2(da,1,2).r = -3.0;
   mxvc2(da,0,0).i = 3.0;
   mxvc2(da,0,1).i = -2.0;
   mxvc2(da,0,2).i = 6.0;
   mxvc2(da,1,0).i = -0.20;
   mxvc2(da,1,1).i = 3.0;
   mxvc2(da,1,2).i = 9.0;
   SUMA_ShowMxVec(da, da->N_vals, NULL, "\nda\n");
   SUMA_ShowMxVec(db, db->N_vals, NULL, "\ndb\n");
   dc = SUMA_MxVecMult(da, db, NULL, 0);
   SUMA_ShowMxVec(dc, dc->N_vals, NULL, "\nda*db\n");

   SUMA_S_Note("Testing Complex Inversion\n");
   db = SUMA_FreeMxVec(db);
   db = SUMA_MxVecInverse(dc, NULL);
   SUMA_ShowMxVec(dc, dc->N_vals, NULL, "\ndc\n");
   SUMA_ShowMxVec(db, db->N_vals, NULL, "\ninv(dc)\n");

   SUMA_S_Note("Testing complex multiplication, a complex");
   db = SUMA_FreeMxVec(db);
   dc = SUMA_FreeMxVec(dc);
   dd[0]= 3; dd[1] = 2;
   db = SUMA_NewMxVec(SUMA_double, 2, dd, 1);
   mxvd2(db,0,0) = 4.0;
   mxvd2(db,0,1) = 2.0;
   mxvd2(db,1,0) = 6.0;
   mxvd2(db,1,1) = -9.0;
   mxvd2(db,2,0) = -1.0;
   mxvd2(db,2,1) = -2.0;
   SUMA_ShowMxVec(da, da->N_vals, NULL, "\nda\n");
   SUMA_ShowMxVec(db, db->N_vals, NULL, "\ndb\n");
   dc = SUMA_MxVecMult(da, db, NULL, 0);
   SUMA_ShowMxVec(dc, dc->N_vals, NULL, "\nda*db\n");

   SUMA_S_Note("Testing complex transpose\n");
   dc = SUMA_FreeMxVec(dc);
   dc = SUMA_MxVecTranspose(da, NULL);
   SUMA_ShowMxVec(da, da->N_vals, NULL, "\nda\n");
   SUMA_ShowMxVec(dc, dc->N_vals, NULL, "\nTranspose(da)\n");


   da = SUMA_FreeMxVec(da);
   db = SUMA_FreeMxVec(db);
   dc = SUMA_FreeMxVec(dc);
   SUMA_RETURNe;
}

/*!
   \brief Function to change a bunch of spherical coordinates to
    cartesian ones
   \param sph (float *) Nval*3 [rho, theta(azimuth), phi(elevation)] spherical  coords
   \param Nval (int) number of coord triplets
   \param center (float *) 3x1 XYZ of center (CARTESIAN). If NULL center is 0 0 0
                           center is ADDED to each coord triplet
                           after xformation to cartesian
   \return coord (float *) Nval*3 XYZ coords

   \sa SUMA_SPH_2_CART
*/
float * SUMA_Sph2Cart (double *sph, int Nval, float *center )
{
   static char FuncName[]={"SUMA_Sph2Cart"};
   double v[3], *f;
   int i, i3;
   float *coord=NULL;

   SUMA_ENTRY;

   if (Nval <= 0) {
      SUMA_RETURN(NULL);
   }

   coord = (float *)SUMA_malloc(Nval*sizeof(float)*3);
   if (!coord) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }

   for (i=0; i<Nval; ++i) {
      i3 = 3*i;
      f = &(sph[i3]);
      SUMA_SPH_2_CART(f, v);

      if (center) {
         coord[i3+0] = v[0] + center[0];
         coord[i3+1] = v[1] + center[1];
         coord[i3+2] = v[2] + center[2];
      } else {
         coord[i3+0] = v[0];
         coord[i3+1] = v[1];
         coord[i3+2] = v[2];
      }

   }

   SUMA_RETURN(coord);
}


/*!
   \brief Function to change a bunch of cartesian coordinates to
   spherical ones
   \param coord (float *) Nval*3 XYZ coords
   \param Nval (int) number of coord triplets
   \param center (float *) 3x1 XYZ of center (CARTESIAN). If NULL center is 0 0 0
                           center is subtracted from each coord triplet
                           before xformation
   \return sph (float *) Nval*3 [rho, theta(azimuth), phi(elevation)] spherical  coords

   \sa SUMA_CART_2_SPH
*/
double * SUMA_Cart2Sph (float *coord, int Nval, float *center )
{
   static char FuncName[]={"SUMA_Cart2Sph"};
   double v[3], *f;
   int i, i3;
   double *sph=NULL;

   SUMA_ENTRY;

   if (Nval <= 0) {
      SUMA_RETURN(NULL);
   }

   sph = (double *)SUMA_malloc(Nval*sizeof(double)*3);
   if (!sph) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(NULL);
   }

   for (i=0; i<Nval; ++i) {
      i3 = 3*i;
      if (center) {
         v[0] = coord[i3+0] - center[0];
         v[1] = coord[i3+1] - center[1];
         v[2] = coord[i3+2] - center[2];
      } else {
         v[0] = coord[i3+0];
         v[1] = coord[i3+1];
         v[2] = coord[i3+2];
      }
      f = &(sph[i3]);
      SUMA_CART_2_SPH(v,f);
   }

   SUMA_RETURN(sph);
}

void SUMA_ShowMemTrace (SUMA_MEMTRACE_STRUCT *Mem, FILE *Out)
{
   static char FuncName[]={"SUMA_ShowMemTrace"};
   int i, *isort = NULL, *mem_sz_sort = NULL, Tot;

   SUMA_ENTRY;

   SUMA_S_Err("No longer in use, use mcw_malloc_dump_sort() instead");

   if (!Out) Out = SUMA_STDERR;

   #ifdef USE_SUMA_MALLOC
   SUMA_SL_Err("NO LONGER SUPPORTED");
   SUMA_RETURNe;
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
   isort = SUMA_z_dqsort_nsc (mem_sz_sort, Mem->N_alloc); /* this version of
         SUMA_z_dqsort does not use SUMA_calloc for allocation thus
         keeping the memory trace unchanged */

   Tot = 0;
   for (i=0; i < Mem->N_alloc; ++i) {
      fprintf (Out,"->[%d]\tPointer %p\t %d bytes.\n",
               i, Mem->Pointers[isort[i]], Mem->Size[isort[i]]);
      Tot += Mem->Size[isort[i]];
   }
   #else

   Tot = 0;
   for (i=0; i < Mem->N_alloc; ++i) {
      fprintf (Out,"->[%d]\tPointer %p\t %d bytes.\n",
               i, Mem->Pointers[i], Mem->Size[i]);
      Tot += Mem->Size[i];
   }
   #endif

   fprintf (Out,"Total Memory Allocated %f Mbytes.\n", (float)Tot/1000000.0);
   if (mem_sz_sort) free(mem_sz_sort); /* mem_sz_sort should not be freed with SUMA_free */
   if (isort) free(isort); /* isort should not be freed with SUMA_free */

   #endif

   SUMA_RETURNe;

}

SUMA_Boolean SUMA_Free_MemTrace (SUMA_MEMTRACE_STRUCT * Mem) {
   static char FuncName[]={"SUMA_Free_MemTrace"};

   #ifdef USE_SUMA_MALLOC
   SUMA_SL_Err("NO LONGER SUPPORTED");
   return(NOPE);
   /* DO NOT USE SUMA_free function here ! */
   if (Mem->Pointers) free (Mem->Pointers);
   if (Mem->Size) free(Mem->Size);
   if (Mem) free (Mem);
   #endif
   return(YUP);
}

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

   SUMA_ENTRY;

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

   SUMA_ENTRY;

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

   SUMA_ENTRY;

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

   SUMA_ENTRY;

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

   SUMA_ENTRY;

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

   SUMA_ENTRY;

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

   SUMA_ENTRY;

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
1D reading based.
*/
int SUMA_float_file_size_1D(char *f_name)
{
   static char FuncName[]={"SUMA_float_file_size_1D"};
   int i=0, ncol = 0, nrow = 0;
   MRI_IMAGE *im = NULL;
   float *far=NULL;

   SUMA_ENTRY;

   im = mri_read_1D (f_name);

   if (!im) {
      SUMA_SLP_Err("Failed to read 1D file");
      SUMA_RETURN(-1);
   }

   far = MRI_FLOAT_PTR(im);
   ncol = im->nx;
   nrow = im->ny;

   mri_free(im); im = NULL;   /* done with that baby */

   SUMA_RETURN(ncol);
}

/* dumber version of SUMA_float_file_size_1D */
int SUMA_float_file_size (char *f_name)
{
   int cnt=0,ex;
   float buf;
   static char FuncName[]={"SUMA_float_file_size"};
   FILE*internal_file;

   SUMA_ENTRY;

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
   SUMA_ENTRY;

   printf ("\n\n\aError in memory allocation\n");
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
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (rows <= 0 || cols < 0) {
      SUMA_S_Errv("Allocate2D with rows = %d and cols = %d!\n", rows, cols);
      SUMA_RETURN(NULL);
   }
   SUMA_LHv("nrows = %d, ncols = %d\n", rows, cols);
   #ifdef USE_SUMA_MALLOC
      SUMA_SL_Err("NO LONGER SUPPORTED");
      SUMA_RETURN(NULL);
   #else
      pause_mcw_malloc();
   #endif

   /* try to allocate the request */
   switch(element_size) {
     case sizeof(short): {    /* short matrix */
         short **int_matrix;
         int_matrix = (short **)calloc(rows,sizeof(short *));
         if(!int_matrix) {
             printf("\nError making pointers in %dx%d short matrix\n"
                         ,rows,cols);
             SUMA_RETURN(NULL);
         }
         for(i = 0 ; i < rows ; i++) {
             int_matrix[i] = (short *)calloc(cols,sizeof(short));
             if(!int_matrix[i]) {
                 printf("\nError making row %d in %dx%d short matrix\n"
                         ,i,rows,cols);
                 SUMA_RETURN(NULL);
             }
         }
         A = (char **)int_matrix;
         break;
     }
     case sizeof(char): {    /* char matrix */
         char **char_matrix;
         char_matrix = (char **)calloc(rows,sizeof(char *));
         if(!char_matrix) {
             printf("\nError making pointers in %dx%d char matrix\n"
                         ,rows,cols);
             SUMA_RETURN(NULL);
         }
         for(i = 0 ; i < rows ; i++) {
             char_matrix[i] = (char *)calloc(cols,sizeof(char));
             if(!char_matrix[i]) {
                 printf("\nError making row %d in %dx%d char matrix\n"
                         ,i,rows,cols);
                 SUMA_RETURN(NULL);
             }
         }
         A = (char **)char_matrix;
         break;
     }
     case sizeof(float): {    /* float, int,  matrix */
         float **float_matrix;
         float_matrix = (float **)calloc(rows,sizeof(float *));
         if(!float_matrix) {
             printf("\nError making pointers in %dx%d float matrix\n"
                         ,rows,cols);
             SUMA_RETURN(NULL);
         }
         for(i = 0 ; i < rows ; i++) {
             float_matrix[i] = (float *)calloc(cols,sizeof(float));
             if(!float_matrix[i]) {
                 printf("\nError making row %d in %dx%d float matrix\n"
                         ,i,rows,cols);
                 SUMA_RETURN(NULL);
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
             SUMA_RETURN(NULL);
         }
         for(i = 0 ; i < rows ; i++) {
             double_matrix[i] = (double *)calloc(cols,sizeof(double));
             if(!double_matrix[i]) {
                 printf("\nError making row %d in %dx%d double matrix\n"
                         ,i,rows,cols);
                 SUMA_RETURN(NULL);
             }
         }
         A = (char **)double_matrix;
         break;
     }

     default:
         printf("\nERROR in matrix_allocate: unsupported type\n");
         SUMA_RETURN(NULL);
   }

   #ifdef USE_SUMA_MALLOC
   SUMA_SL_Err("NO LONGER SUPPORTED");
   SUMA_RETURN(NULL);

   #if SUMA_MEMTRACE_FLAG
   if (SUMAg_CF->MemTrace) {
      ++SUMAg_CF->Mem->N_alloc;
      if (SUMAg_CF->Mem->N_MaxPointers <= SUMAg_CF->Mem->N_alloc) {
         /* must reallocate */
         /* SUMA_ShowMemTrace (SUMAg_CF->Mem, NULL);*/
         SUMAg_CF->Mem->N_MaxPointers += SUMA_MEMTRACE_BLOCK;

         SUMAg_CF->Mem->Pointers = (void **)realloc(SUMAg_CF->Mem->Pointers,
                                 sizeof(void*) * SUMAg_CF->Mem->N_MaxPointers);
         SUMAg_CF->Mem->Size  = (int *)realloc((void *)SUMAg_CF->Mem->Size,
                                 sizeof(int) * SUMAg_CF->Mem->N_MaxPointers);
         if (!SUMAg_CF->Mem->Pointers || !SUMAg_CF->Mem->Pointers) {
            fprintf (SUMA_STDERR,
               "Error %s: Failed to reallocate.\nTurning off memory tracing.\n",
               FuncName);
            /* free up allocated space, clean up pointers, turn off memory
               tracing DO NOT USE SUMA_free here*/
            if (SUMAg_CF->Mem->Pointers) free(SUMAg_CF->Mem->Pointers);
            SUMAg_CF->Mem->Pointers = NULL;
            if (SUMAg_CF->Mem->Size) free(SUMAg_CF->Mem->Size);
            SUMAg_CF->Mem->Size = NULL;
            SUMAg_CF->MemTrace = 0;
            SUMAg_CF->Mem->N_alloc = 0;
            SUMAg_CF->Mem->N_MaxPointers =0;
         }
      }
      SUMAg_CF->Mem->Pointers[SUMAg_CF->Mem->N_alloc-1] = A;
      SUMAg_CF->Mem->Size[SUMAg_CF->Mem->N_alloc-1] = rows * cols * element_size;
   }
   #endif

   #else
      resume_mcw_malloc();
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

   SUMA_ENTRY;

   if (!a) SUMA_RETURNe;

      #ifdef USE_SUMA_MALLOC
         SUMA_SL_Err("NO LONGER SUPPORTED");
         SUMA_RETURNe;

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
      #else
         pause_mcw_malloc();
      #endif

   /* free each row of data */
   for(i = 0 ; i < rows ; i++) if (a[i]) free(a[i]);

   /* free each row pointer */
   free((char *)a);
   a = NULL;           /* set to null for error */

   #ifdef USE_SUMA_MALLOC
      /* don't use ifndef, keep it parallel with stuff above */
      SUMA_SL_Err("NO LONGER SUPPORTED");
      SUMA_RETURNe;

   #else
      resume_mcw_malloc();
   #endif

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

   SUMA_ENTRY;

   printf ("\n\n\aError: %s\n",s2);
   printf ("Error origin: %s\n\n",s1);
   if (ext == 1)
      {
        printf ("Exiting Program ..\n\n");
         exit (0);
      }
      else SUMA_RETURNe;

  }

/*!
   Return a string that is a catenation
   of the characters that differ between s1 and s2
   s1 and s2 are switched in the function so that
   s1 is always the longest of the two
*/
char *SUMA_StringDiff(char *s1, char *s2)
{
   static char FuncName[]={"SUMA_StringDiff"};
   char *sd=NULL;
   int ns1=0, ns2=0, ns=0, i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   SUMA_LHv("Will diff on %p and %p\n", s1, s2);
   if (!s1 && !s2) {
      SUMA_RETURN(sd);
   }
   if (!s1 && s2) {
      SUMA_RETURN(SUMA_copy_string(s2));
   }
   if (s1 && !s2) {
      SUMA_RETURN(SUMA_copy_string(s1));
   }
   ns1 = strlen(s1);
   ns2 = strlen(s2);
   if (ns1 < ns2) {
      sd = s1; ns = ns1;
      s1 = s2;
      s2 = sd; sd = NULL;
      ns1 = ns2;
      ns2 = ns; ns = 0;
   }

   /* OK, have s1, and s2, and s1 is the longest */
   sd = (char *)calloc(ns1+1, sizeof(char));
   ns = 0;
   for (i=0; i < ns2; ++i) {
      if (s1[i] != s2[i]) {
         sd[ns]=s1[i];++ns;
      }
   }
   for (i=ns2; i < ns1; ++i) {
      sd[ns]=s1[i];++ns;
   }
   sd[ns]='\0';

   SUMA_LHv("Diff of %s and %s is\n%s\n",
               s1, s2, sd);
   RETURN(sd);
}

/*!
   Return a string that contains matching
   characters between s1 and s2
   s1 and s2 are switched in the function so that
   s1 is always the longest of the two
   firstdiff: if 1, then stop at the firt difference
   filler : if filler != '\0' then fill differing spots with 'filler'
            Otherwise differing characters are dropped from the output.
*/
char *SUMA_StringMatch(char *s1, char *s2, int firstdiff, char filler)
{
   static char FuncName[]={"SUMA_StringMatch"};
   char *sm=NULL;
   int ns1=0, ns2=0, ns=0, i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   SUMA_LHv("Will match on %p and %p\n", s1, s2);
   if (!s1 && !s2) {
      SUMA_RETURN(sm);
   }
   if (!s1 && s2) {
      SUMA_RETURN(sm);
   }
   if (s1 && !s2) {
      SUMA_RETURN(sm);
   }
   ns1 = strlen(s1);
   ns2 = strlen(s2);
   if (ns1 < ns2) {
      sm = s1; ns = ns1;
      s1 = s2;
      s2 = sm; sm = NULL;
      ns1 = ns2;
      ns2 = ns; ns = 0;
   }

   /* OK, have s1, and s2, and s1 is the longest */
   sm = (char *)calloc(ns1+1, sizeof(char));
   ns = 0;
   for (i=0; i < ns2; ++i) {
      if (s1[i] != s2[i]) {
         if (firstdiff) {
            sm[ns] = '\0';
            RETURN(sm);
         } else {
            if (filler != '\0') {
               sm[ns] = filler; ++ns;
            }
         }
      } else {
         sm[ns]=s1[i];++ns;
      }
   }
   if (filler != '\0') {
      for (i=ns2; i < ns1; ++i) {
         sm[ns]=filler;++ns;
      }
   }
   sm[ns]='\0';

   SUMA_LHv("Match of %s and %s (firstdiff=%d, filler=%c) is\n%s\n",
               s1, s2, firstdiff, filler, sm);
   RETURN(sm);
}


/*!
   \brief case insensitive version of SUMA_iswordin
*/
int SUMA_iswordin_ci ( const char *sbig, const char *ssub)
{
   static char FuncName[]={"SUMA_iswordin_ci"};
   char *sbigc, *ssubc;
   int ans;

   SUMA_ENTRY;
   sbigc = SUMA_copy_string((char *)sbig);
   ssubc = SUMA_copy_string((char *)ssub);

   SUMA_TO_LOWER(sbigc);
   SUMA_TO_LOWER(ssubc);

   ans = SUMA_iswordin (sbigc, ssubc);
   if (sbigc) SUMA_free(sbigc); sbigc = NULL;
   if (ssubc) SUMA_free(ssubc); ssubc = NULL;

   SUMA_RETURN(ans);

}

int SUMA_wordswap_ci ( const char *sbig, const char *ssub,
                       const char *sswap, char *sout)
{
   static char FuncName[]={"SUMA_wordswap_ci"};
   char *sbigc, *ssubc, *sswapc, *sfn=NULL;
   int ans, i, n, k;

   SUMA_ENTRY;
   sbigc = SUMA_copy_string((char *)sbig);
   ssubc = SUMA_copy_string((char *)ssub);
   sswapc = SUMA_copy_string((char *)sswap);

   SUMA_TO_LOWER(sbigc);
   SUMA_TO_LOWER(ssubc);
   SUMA_TO_LOWER(sswapc);

   ans = SUMA_iswordin (sbigc, ssubc);
   k = 0;
   if (ans) {
      sfn = strstr(sbigc, ssubc);
      i = 0;
      while (i < (int)(sfn-sbigc)) { sout[k++] = sbigc[i++]; }/* Copy to swap */
      if (sswapc) {
         n = 0;
         while (n < strlen(sswapc)) { sout[k++] = sswapc[n++]; }
      }
      i += strlen(ssub);
      while (i < strlen(sbigc)) { sout[k++] = sbigc[i++]; } /* Copy left over */
      sout[k] = '\0';
   }

   if (sbigc) SUMA_free(sbigc); sbigc = NULL;
   if (sswapc) SUMA_free(sswapc); sswapc = NULL;
   if (ssubc) SUMA_free(ssubc); ssubc = NULL;

   SUMA_RETURN(ans);

}

/*!
   \brief case insensitive version of SUMA_iswordsame
*/
int SUMA_iswordsame_ci ( const char *sbig, const char *ssub)
{
   static char FuncName[]={"SUMA_iswordsame_ci"};
   int ans;

   SUMA_ENTRY;
   if ( (ans = SUMA_iswordin_ci(sbig, ssub)) == 1 &&
        strlen(sbig) != strlen(ssub) ) ans = 0;

   SUMA_RETURN(ans);
}

int SUMA_iswordsame ( const char *sbig, const char *ssub)
{
   static char FuncName[]={"SUMA_iswordsame"};
   int ans;

   SUMA_ENTRY;
   if ( (ans = SUMA_iswordin(sbig, ssub)) == 1 &&
        strlen(sbig) != strlen(ssub) ) ans = 0;

   SUMA_RETURN(ans);
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

\sa  SUMA_iswordin_ci
***/
#define IS_TRACE 0
int SUMA_iswordin (const char *sbig, const char *ssub)
{/*SUMA_iswordin*/
   int i=0,j=0;
   static char FuncName[]={"SUMA_iswordin"};

   #if IS_TRACE
   SUMA_ENTRY;
   #endif

   if (sbig == NULL && ssub == NULL) {
      #if IS_TRACE
         SUMA_RETURN (-2);
      #else
         return(-2);
      #endif
   }
   if (sbig == NULL || ssub == NULL) {
      #if IS_TRACE
         SUMA_RETURN (-1);
      #else
         return(-1);
      #endif
   }
   if (strlen(sbig) < strlen(ssub)) {
      #if IS_TRACE
         SUMA_RETURN (0);
      #else
         return(0);
      #endif
   }

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
      #if IS_TRACE
         SUMA_RETURN (1);
      #else
         return(1);
      #endif
   }
   else {
      #if IS_TRACE
         SUMA_RETURN (0);
      #else
         return(0);
      #endif
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

   SUMA_ENTRY;

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
   SUMA_RETURNe;
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

   SUMA_ENTRY;

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
   SUMA_RETURNe;
}/*SUMA_disp_mat*/

/*!**
Purpose :
   Displays on the terminal a 2D double matrix stored in a vector


Usage :
       SUMA_disp_vecdoubmat (double *v,int nr, int nc, int SpcOpt, d_order, Out )


Input paramters :
    v (double *) (nr x nc) vector containing the 2D matrix to display
   nr (int) the number of rows in v
   nc (int) the number of columns
   SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
   d_order (SUMA_INDEXING_ORDER): Indicates how multiple values per node are stored in fin
                        SUMA_ROW_MAJOR: The data in fin is stored in *** Row Major *** order.
                        The ith value (start at 0) for node n is at index fin[vpn*n+i]
                        SUMA_COLUMN_MAJOR: The data in fin is stored in *** Column Major *** order.
                        The ith (start at 0) value for node n is at index fin[n+SO->N_Node*i];
                        etc...
   AddRowInd (SUMA_Boolean) YUP  = add the row index in the first column
   Out (FILE *) pointer to output file. If NULL then output is to stdout.


*/
void SUMA_disp_vecdoubmat (double *v,int nr, int nc , int SpcOpt,
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd)
{/*SUMA_disp_vecdoubmat*/
   char spc [40];
   int i,j;
   FILE *foutp;
   static char FuncName[]={"SUMA_disp_vecdoubmat"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!fout) foutp = stdout;
   else foutp = fout;

   if (LocalHead) fprintf(SUMA_STDERR,"%s:\nExpecting to write %d rows/%d columns\n", FuncName, nr, nc);

   if (!SpcOpt)
      sprintf(spc," ");
   else if (SpcOpt == 1)
      sprintf(spc,"\t");
   else
      sprintf(spc," , ");

   if (!fout) fprintf (SUMA_STDOUT,"\n"); /* a blank 1st line when writing to screen */
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%lf%s",v[i*nc+j],spc);
            fprintf (foutp,"\n");
         }
         break;
      case SUMA_COLUMN_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%lf%s",v[i+j*nr],spc);
            fprintf (foutp,"\n");
         }
         break;
      default:
         SUMA_SL_Err("Bad order.\n");
         SUMA_RETURNe;
         break;
   }

   SUMA_RETURNe;
}/*SUMA_disp_vecdoubmat*/

/*!**

File : SUMA_MiscFunc.c
Author : Ziad Saad
Date : Tue Nov 17 13:19:26 CST 1998, modified Tue Aug 20 11:11:29 EDT 2002

Purpose :
   Displays on the terminal a 2D byte matrix stored in a vector


Usage :
       SUMA_disp_vecbytemat (byte *v,int nr, int nc, int SpcOpt, d_order, Out )


Input paramters :
    v (byte *) (nr x nc) vector containing the 2D matrix to display
   nr (int) the number of rows in v
   nc (int) the number of columns
   SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
   d_order (SUMA_INDEXING_ORDER): Indicates how multiple values per node are stored in fin
                        SUMA_ROW_MAJOR: The data in fin is stored in *** Row Major *** order.
                        The ith value (start at 0) for node n is at index fin[vpn*n+i]
                        SUMA_COLUMN_MAJOR: The data in fin is stored in *** Column Major *** order.
                        The ith (start at 0) value for node n is at index fin[n+SO->N_Node*i];
                        etc...
   AddRowInd (SUMA_Boolean) YUP  = add the row index in the first column
   Out (FILE *) pointer to output file. If NULL then output is to stdout.


*/
void SUMA_disp_vecbytemat (byte *v,int nr, int nc , int SpcOpt,
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd)
{/*SUMA_disp_vecbytemat*/
   char spc [40];
   int i,j;
   FILE *foutp;
   static char FuncName[]={"SUMA_disp_vecbytemat"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!fout) foutp = stdout;
   else foutp = fout;

   if (LocalHead) fprintf(SUMA_STDERR,"%s:\nExpecting to write %d rows/%d columns\n", FuncName, nr, nc);

   if (!SpcOpt)
      sprintf(spc," ");
   else if (SpcOpt == 1)
      sprintf(spc,"\t");
   else
      sprintf(spc," , ");

   if (!fout) fprintf (SUMA_STDOUT,"\n"); /* a blank 1st line when writing to screen */
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i*nc+j],spc);
            fprintf (foutp,"\n");
         }
         break;
      case SUMA_COLUMN_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i+j*nr],spc);
            fprintf (foutp,"\n");
         }
         break;
      default:
         SUMA_SL_Err("Bad order.\n");
         SUMA_RETURNe;
         break;
   }

   SUMA_RETURNe;
}/*SUMA_disp_vecbytemat*/

/*!**

File : SUMA_MiscFunc.c
Author : Ziad Saad
Date : Tue Nov 17 13:19:26 CST 1998, modified Tue Aug 20 11:11:29 EDT 2002

Purpose :
   Displays on the terminal a 2D short matrix stored in a vector


Usage :
       SUMA_disp_vecshortmat (short *v,int nr, int nc, int SpcOpt, d_order, Out )


Input paramters :
    v (short *) (nr x nc) vector containing the 2D matrix to display
   nr (int) the number of rows in v
   nc (int) the number of columns
   SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
   d_order (SUMA_INDEXING_ORDER): Indicates how multiple values per node are stored in fin
                        SUMA_ROW_MAJOR: The data in fin is stored in *** Row Major *** order.
                        The ith value (start at 0) for node n is at index fin[vpn*n+i]
                        SUMA_COLUMN_MAJOR: The data in fin is stored in *** Column Major *** order.
                        The ith (start at 0) value for node n is at index fin[n+SO->N_Node*i];
                        etc...
   AddRowInd (SUMA_Boolean) YUP  = add the row index in the first column
   Out (FILE *) pointer to output file. If NULL then output is to stdout.


*/
void SUMA_disp_vecshortmat (short *v,int nr, int nc , int SpcOpt,
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd)
{/*SUMA_disp_vecshortmat*/
   char spc [40];
   int i,j;
   FILE *foutp;
   static char FuncName[]={"SUMA_disp_vecshortmat"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!fout) foutp = stdout;
   else foutp = fout;

   if (LocalHead) fprintf(SUMA_STDERR,"%s:\nExpecting to write %d rows/%d columns\n", FuncName, nr, nc);

   if (!SpcOpt)
      sprintf(spc," ");
   else if (SpcOpt == 1)
      sprintf(spc,"\t");
   else
      sprintf(spc," , ");

   if (!fout) fprintf (SUMA_STDOUT,"\n"); /* a blank 1st line when writing to screen */
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i*nc+j],spc);
            fprintf (foutp,"\n");
         }
         break;
      case SUMA_COLUMN_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i+j*nr],spc);
            fprintf (foutp,"\n");
         }
         break;
      default:
         SUMA_SL_Err("Bad order.\n");
         SUMA_RETURNe;
         break;
   }

   SUMA_RETURNe;
}/*SUMA_disp_vecshortmat*/
/*!**

File : SUMA_MiscFunc.c
Author : Ziad Saad
Date : Tue Nov 17 13:19:26 CST 1998, modified Tue Aug 20 11:11:29 EDT 2002

Purpose :
   Displays on the terminal a 2D complex matrix stored in a vector


Usage :
       SUMA_disp_veccompmat (complex *v,int nr, int nc, int SpcOpt, d_order, Out )


Input paramters :
    v (complex *) (nr x nc) vector containing the 2D matrix to display
   nr (int) the number of rows in v
   nc (int) the number of columns
   SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
   d_order (SUMA_INDEXING_ORDER): Indicates how multiple values per node are stored in fin
                        SUMA_ROW_MAJOR: The data in fin is stored in *** Row Major *** order.
                        The ith value (start at 0) for node n is at index fin[vpn*n+i]
                        SUMA_COLUMN_MAJOR: The data in fin is stored in *** Column Major *** order.
                        The ith (start at 0) value for node n is at index fin[n+SO->N_Node*i];
                        etc...
   AddRowInd (SUMA_Boolean) YUP  = add the row index in the first column
   Out (FILE *) pointer to output file. If NULL then output is to stdout.


*/
void SUMA_disp_veccompmat (complex *v,int nr, int nc , int SpcOpt,
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd)
{/*SUMA_disp_veccompmat*/
   char spc [40];
   int i,j;
   FILE *foutp;
   static char FuncName[]={"SUMA_disp_veccompmat"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!fout) foutp = stdout;
   else foutp = fout;

   if (LocalHead) fprintf(SUMA_STDERR,"%s:\nExpecting to write %d rows/%d columns\n", FuncName, nr, nc);

   if (!SpcOpt)
      sprintf(spc," ");
   else if (SpcOpt == 1)
      sprintf(spc,"\t");
   else
      sprintf(spc," , ");

   if (!fout) fprintf (SUMA_STDOUT,"\n"); /* a blank 1st line when writing to screen */
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%f %+fi%s",v[i*nc+j].r, v[i*nc+j].i,spc);
            fprintf (foutp,"\n");
         }
         break;
      case SUMA_COLUMN_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%f %+fi%s",v[i+j*nr].r, v[i+j*nr].i,spc);
            fprintf (foutp,"\n");
         }
         break;
      default:
         SUMA_SL_Err("Bad order.\n");
         SUMA_RETURNe;
         break;
   }

   SUMA_RETURNe;
}/*SUMA_disp_veccompmat*/

/*!**

File : SUMA_MiscFunc.c
Author : Ziad Saad
Date : Tue Nov 17 13:19:26 CST 1998, modified Tue Aug 20 11:11:29 EDT 2002

Purpose :
   Displays on the terminal a 2D float matrix stored in a vector


Usage :
       SUMA_disp_vecmat (float *v,int nr, int nc, int SpcOpt, d_order, Out )


Input paramters :
    v (float *) (nr x nc) vector containing the 2D matrix to display
   nr (int) the number of rows in v
   nc (int) the number of columns
   SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
   d_order (SUMA_INDEXING_ORDER): Indicates how multiple values per node are stored in fin
                        SUMA_ROW_MAJOR: The data in fin is stored in *** Row Major *** order.
                        The ith value (start at 0) for node n is at index fin[vpn*n+i]
                        SUMA_COLUMN_MAJOR: The data in fin is stored in *** Column Major *** order.
                        The ith (start at 0) value for node n is at index fin[n+SO->N_Node*i];
                        etc...
   AddRowInd (SUMA_Boolean) YUP  = add the row index in the first column
   Out (FILE *) pointer to output file. If NULL then output is to stdout.


*/
void SUMA_disp_vecmat (float *v,int nr, int nc , int SpcOpt,
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd)
{/*SUMA_disp_vecmat*/
   char spc [40];
   int i,j;
   FILE *foutp;
   static char FuncName[]={"SUMA_disp_vecmat"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!fout) foutp = stdout;
   else foutp = fout;

   if (LocalHead) fprintf(SUMA_STDERR,"%s:\nExpecting to write %d rows/%d columns\n", FuncName, nr, nc);

   if (!SpcOpt)
      sprintf(spc," ");
   else if (SpcOpt == 1)
      sprintf(spc,"\t");
   else
      sprintf(spc," , ");

   if (!fout) fprintf (SUMA_STDOUT,"\n"); /* a blank 1st line when writing to screen */
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%f%s",v[i*nc+j],spc);
            fprintf (foutp,"\n");
         }
         break;
      case SUMA_COLUMN_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%f%s",v[i+j*nr],spc);
            fprintf (foutp,"\n");
         }
         break;
      default:
         SUMA_SL_Err("Bad order.\n");
         SUMA_RETURNe;
         break;
   }

   SUMA_RETURNe;
}/*SUMA_disp_vecmat*/
/*!**

File : SUMA_MiscFunc.c
Author : Ziad Saad
Date : Tue Nov 17 13:19:26 CST 1998, modified Tue Aug 20 11:11:29 EDT 2002

Purpose :
   Displays on the terminal a 2D int matrix stored in a 1D vector


Usage :
       SUMA_disp_vecdmat (float *v,int nr, int nc, int SpcOpt, d_order, Out,  AddRowInd)


Input paramters :
    v (int *) (nr x nc) vector containing the 2D matrix to display
   nr (int) the number of rows in v
   nc (int) the number of columns
   SpcOpt (int) : spacing option (0 for space, 1 for tab and 2 for comma)
   d_order (SUMA_INDEXING_ORDER): Indicates how multiple values per node are stored in fin
                        SUMA_ROW_MAJOR: The data in fin is stored in *** Row Major *** order.
                        The ith value (start at 0) for node n is at index fin[vpn*n+i]
                        SUMA_COLUMN_MAJOR: The data in fin is stored in *** Column Major *** order.
                        The ith (start at 0) value for node n is at index fin[n+SO->N_Node*i];
                        etc...
   Out (FILE *) pointer to output file. If NULL then output is to stdout.
   AddRowInd (SUMA_Boolean) YUP  = add the row index in the first column


   \sa SUMA_disp_vecucmat
*/
void SUMA_disp_vecdmat (int *v,int nr, int nc , int SpcOpt,
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd)
{/*SUMA_disp_vecdmat*/
   char spc [40];
   int i,j;
   FILE *foutp;
   static char FuncName[]={"SUMA_disp_vecdmat"};

   SUMA_ENTRY;

   if (!fout) foutp = stdout;
   else foutp = fout;

   if (!SpcOpt)
      sprintf(spc," ");
   else if (SpcOpt == 1)
      sprintf(spc,"\t");
   else
      sprintf(spc," , ");

   if (!fout) fprintf (SUMA_STDOUT,"\n"); /* a blank 1st line when writing to screen */
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i*nc+j],spc);
            fprintf (foutp,"\n");
         }
         break;
      case SUMA_COLUMN_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i+j*nr],spc);
            fprintf (foutp,"\n");
         }
         break;
      default:
         SUMA_SL_Err("Bad order.\n");
         SUMA_RETURNe;
         break;
   }
   SUMA_RETURNe;
}/*SUMA_disp_vecdmat*/

/*!
   \brief same as SUMA_disp_vecdmat, except with unsigned char * instead of int *
*/
void SUMA_disp_vecucmat (unsigned char *v,int nr, int nc , int SpcOpt,
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd)
{/*SUMA_disp_vecucmat*/
   char spc [40];
   int i,j;
   FILE *foutp;
   static char FuncName[]={"SUMA_disp_vecucmat"};

   SUMA_ENTRY;

   if (!fout) foutp = stdout;
   else foutp = fout;

   if (!SpcOpt)
      sprintf(spc," ");
   else if (SpcOpt == 1)
      sprintf(spc,"\t");
   else
      sprintf(spc," , ");

   if (!fout) fprintf (SUMA_STDOUT,"\n"); /* a blank 1st line when writing to screen */
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i*nc+j],spc);
            fprintf (foutp,"\n");
         }
         break;
      case SUMA_COLUMN_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i+j*nr],spc);
            fprintf (foutp,"\n");
         }
         break;
      default:
         SUMA_SL_Err("Bad order.\n");
         SUMA_RETURNe;
         break;
   }
   SUMA_RETURNe;
}/*SUMA_disp_vecucmat*/
void SUMA_disp_veccmat (char *v,int nr, int nc , int SpcOpt,
                        SUMA_INDEXING_ORDER d_order, FILE *fout,
                        SUMA_Boolean AddRowInd)
{/*SUMA_disp_veccmat*/
   char spc [40];
   int i,j;
   FILE *foutp;
   static char FuncName[]={"SUMA_disp_veccmat"};

   SUMA_ENTRY;

   if (!fout) foutp = stdout;
   else foutp = fout;

   if (!SpcOpt)
      sprintf(spc," ");
   else if (SpcOpt == 1)
      sprintf(spc,"\t");
   else
      sprintf(spc," , ");

   if (!fout) fprintf (SUMA_STDOUT,"\n"); /* a blank 1st line when writing to screen */
   switch (d_order) {
      case SUMA_ROW_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i*nc+j],spc);
            fprintf (foutp,"\n");
         }
         break;
      case SUMA_COLUMN_MAJOR:
         for (i=0; i < nr; ++i) {
            if (AddRowInd) fprintf (foutp, "%d%s", i, spc);
            for (j=0; j < nc; ++j) fprintf (foutp, "%d%s",v[i+j*nr],spc);
            fprintf (foutp,"\n");
         }
         break;
      default:
         SUMA_SL_Err("Bad order.\n");
         SUMA_RETURNe;
         break;
   }
   SUMA_RETURNe;
}/*SUMA_disp_vecucmat*/

/*!
   Set *N_dims to -1 if you don't have dims setup and are willing to take whatever is in Name
*/
SUMA_MX_VEC *SUMA_Read1DMxVec(SUMA_VARTYPE tp, char *Name, int *dims, int *N_dims)
{
   static char FuncName[]={"SUMA_Read1DMxVec"};
   SUMA_MX_VEC *v=NULL;
   float *fv=NULL;
   double *dv = NULL;
   int ncol, nrow, i, nvals;
   complex *cv = NULL;
   SUMA_Boolean LocalHead   = NOPE;

   SUMA_ENTRY;

   if (*N_dims > 0) {
      /* user has format in mind */
      nvals = dims[0];
      for (i=1;i<*N_dims;++i) nvals = nvals * dims[i];
   } else {
      nvals = -1;
   }
   switch (tp) {
      case SUMA_complex:
         cv = SUMA_LoadComplex1D_eng (Name, &ncol, &nrow, 0, 0);
         if (!cv) {
            SUMA_S_Errv("Failed to load %s\n", Name);
            SUMA_RETURN(v);
         }
         if (nvals >= 0) {
            if (ncol*nrow != nvals) {
               SUMA_S_Errv("User wants a total of %d values, %d found in file.\n", nvals, ncol*nrow);
               SUMA_RETURN(v);
            }
         } else {
            nvals = ncol*nrow;
            dims[0] = nrow; dims[1] = ncol;
            *N_dims = 2;
         }
         SUMA_LHv("nvals = %d, dims=[%d,%d], ncol=%d, *Ndims=%d\n", nvals, dims[0], dims[1], ncol, *N_dims);

         v = SUMA_VecToMxVec(SUMA_complex, *N_dims, dims, 1, (void *)cv); cv = NULL; /* cv should be nulled, pointer copied into output*/
         break;

      #if 0
      case SUMA_double:
         fv = SUMA_Load1D_eng (Name, &ncol, &nrow, 0, 0);
         if (!fv) {
            SUMA_S_Errv("Failed to load %s\n", Name);
            SUMA_RETURN(v);
         }
         if (nvals >= 0) {
            if (ncol*nrow != nvals) {
               SUMA_S_Errv("User wants a total of %d values, %d (%dx%d) found in file.\n", nvals, ncol*nrow, ncol, nrow);
               SUMA_RETURN(v);
            }
         } else {
            nvals = ncol*nrow;
            dims[0] = nrow; dims[1] = ncol;
            *N_dims = 2;
         }
         v = SUMA_NewMxVec(tp, *N_dims,  dims,  1);
         for (i=0; i<nvals; ++i) {
            mxvd1(v,i) = (double)fv[i];
         }
         SUMA_free(fv); fv = NULL;
         break;
      #else
      case SUMA_double:
         dv = SUMA_LoadDouble1D_eng(Name, &ncol, &nrow, 0, 0);
         if (!dv) {
            SUMA_S_Errv("Failed to load %s\n", Name);
            SUMA_RETURN(v);
         }
         if (nvals >= 0) {
            if (ncol*nrow != nvals) {
               SUMA_S_Errv("User wants a total of %d values, %d found in file.\n", nvals, ncol*nrow);
               SUMA_RETURN(v);
            }
         } else {
            nvals = ncol*nrow;
            dims[0] = nrow; dims[1] = ncol;
            *N_dims = 2;
         }
         SUMA_LHv("nvals = %d, dims=[%d,%d], ncol=%d, *Ndims=%d\n", nvals, dims[0], dims[1], ncol, *N_dims);

         v = SUMA_VecToMxVec(SUMA_double, *N_dims, dims, 1, (void *)dv); dv = NULL; /* dv should be nulled, pointer copied into output*/
         break;
      #endif
      default:
         SUMA_S_Err("Not ready for this type");
         break;
   }

   SUMA_RETURN(v);
}

/*!
   \brief a function to write MxVec to a file, mostly
   for debugging. No overwrite protection provided
   If Name is NULL then output is to stdout
*/
int SUMA_WriteMxVec(SUMA_MX_VEC *mxv, char *Name, char *title)
{
   static char FuncName[]={"SUMA_WriteMxVec"};
   FILE *out = NULL;
   int i, d0, d1;

   SUMA_ENTRY;

   if (!Name) out = stdout;
   else {
      out = fopen(Name,"w");
      if (!out) {
         SUMA_S_Err("Could not open file for output");
         SUMA_RETURN(0);
      }
   }
   if (title) {
      if (title[0] != '#') fprintf(out,"#");
      fprintf(out,"%s", title);
      if (title[strlen(title)] != '\n'); fprintf(out,"\n");
   }

   if (mxv->N_dims > 2) {
      fprintf(out,"#MxVec is %d dimensional, writing results in column major (first dimension first) array form.\n", mxv->N_dims);
      d0 = mxv->N_vals;
      d1 = 1;
   } else if (mxv->N_dims == 1) {
      d0 = mxv->N_vals;
      d1 = 1;
   } else {
      d0 = mxv->dims[0];
      d1 = mxv->dims[1];
   }

   if (mxv->fdf != 1) {
      SUMA_S_Err("Not ready for vectors that are not first dimension first");
      SUMA_RETURN(0);
   }

   fprintf(out,"#Dimensions are: [");
   for (i=0;i<mxv->N_dims;++i) fprintf(out,"%d, ", mxv->dims[i]);
   fprintf(out,"]\n");

   switch (mxv->tp) {
      case SUMA_byte:
         SUMA_disp_vecbytemat((byte *)mxv->v, d0, d1, 1,
                              SUMA_COLUMN_MAJOR, out, 0);
         break;
      case SUMA_short:
         SUMA_disp_vecshortmat((short *)mxv->v, d0, d1, 1,
                                 SUMA_COLUMN_MAJOR, out, 0);
         break;
      case SUMA_int:
         SUMA_disp_vecdmat((int *)mxv->v, d0, d1, 1,
                           SUMA_COLUMN_MAJOR, out, 0);
         break;
      case SUMA_float:
         SUMA_disp_vecmat((float *)mxv->v, d0, d1, 1,
                           SUMA_COLUMN_MAJOR, out, 0);
         break;
      case SUMA_double:
         SUMA_disp_vecdoubmat((double *)mxv->v, d0, d1, 1,
                              SUMA_COLUMN_MAJOR, out, 0);
         break;
      case SUMA_complex:
         SUMA_disp_veccompmat((complex *)mxv->v, d0, d1, 1,
                              SUMA_COLUMN_MAJOR, out, 0);
         break;
      default:
         SUMA_SL_Err("Type not supported");
         SUMA_RETURN(0);
   }

   if (Name) fclose(out); out = NULL;
   SUMA_RETURN(1);
}


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

   SUMA_ENTRY;

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
void SUMA_disp_doubvect (double *v,int l)
{ int i;
   static char FuncName[]={"SUMA_disp_doubvect"};

   SUMA_ENTRY;

   fprintf (SUMA_STDOUT,"\n");
   if ((l-1) == 0)
      fprintf (SUMA_STDOUT,"%g\n",*v);
   else
   {
   for (i=0;i<l;++i)
                 fprintf (SUMA_STDOUT,"%g\t",v[i]);
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

   SUMA_ENTRY;

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
   SUMA_etime2(char *name, char *str, char *strloc)
   SUMA_etime2(name, NULL, NULL) initialize time stamp
   SUMA_etime2(name, str, strloc) show elapsed time from last call,
                           use strings str and strloc in report.
                           str must be non null.
*/
int SUMA_etime2(char *name, char *str, char *strloc)
{
   static char FuncName[]={"SUMA_etime2"};
   int i;
   double dt;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!name) {
      SUMA_LH("Resetting all timers");
      /* reset all */
      for (i=0; i<SUMA_MAX_N_TIMER; ++i) {
         SUMAg_CF->Timer[i].name[0]='\0';
         SUMAg_CF->Timer[i].lastcall = -1.0;
      }
      SUMAg_CF->N_Timer = 0;
      SUMA_RETURN(-1);
   }else {
      /* find the time */
      SUMA_LHv("Locating timer %s\n", name);
      i = 0;
      while (i < SUMAg_CF->N_Timer && strcmp(SUMAg_CF->Timer[i].name, name)) {
         ++i;
      }
      if (i+1 >= SUMA_MAX_N_TIMER) {
         SUMA_S_Errv("Cannot add a new timer %s\n", name);
         SUMA_RETURN(-1);
      } else if (i == SUMAg_CF->N_Timer) { /* add the new timer */
         SUMA_LHv("Adding new timer %s at i=%d\n", name, i);
         sprintf(SUMAg_CF->Timer[i].name, "%s", name);
         SUMAg_CF->Timer[i].lastcall = -1.0;
         ++SUMAg_CF->N_Timer;
      } else {
         SUMA_LHv("Timer %s found at i = %d\n", SUMAg_CF->Timer[i].name, i);
      }
      SUMA_LHv("Timer is %s at %d\n", SUMAg_CF->Timer[i].name, i);
      if (str) { /* have something to say, not first call */
         if (SUMAg_CF->Timer[i].lastcall < 0) {
            dt = 0.0;
         } else {
            dt = SUMA_etime(&(SUMAg_CF->Timer[i].tt),1) - SUMAg_CF->Timer[i].lastcall ;
         }
         SUMAg_CF->Timer[i].lastcall = SUMA_etime(&(SUMAg_CF->Timer[i].tt),1);
         if (strloc) fprintf (SUMA_STDERR,"Timer %s, in %s: %s\n"
                                          "      Time from last stamp %fs (%.2fmin)\n"
                                          "      Total time from init. %fs (%.2fmin)\n",
                                                SUMAg_CF->Timer[i].name,
                                                strloc, str, dt, dt/60.0,
                                                SUMAg_CF->Timer[i].lastcall,
                                                SUMAg_CF->Timer[i].lastcall/60.0);
         else        fprintf (SUMA_STDERR,"Timer %s, %s\n"
                                          "      Time from last stamp %fs (%.2fmin)\n"
                                          "      Total time from init. %fs (%.2fmin)\n",
                                                 SUMAg_CF->Timer[i].name,
                                                 str, dt, dt/60.0,
                                                 SUMAg_CF->Timer[i].lastcall,
                                                 SUMAg_CF->Timer[i].lastcall/60.0);

         SUMA_RETURN(i);
      } else { /* Reset timer */
         SUMA_etime(&(SUMAg_CF->Timer[i].tt), 0);
         SUMAg_CF->Timer[i].lastcall = -1.0;
         SUMA_RETURN(i);
      }
   }
   SUMA_LH("Going home");
   SUMA_RETURN(-1);
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

   SUMA_ENTRY;

   /* get time */
   gettimeofday(&tn, NULL);

   if (Report)
      {
         /* fprintf(stderr,"%s: Reporting from %d sec to %d sec\n",
                        FuncName, t->tv_sec, tn.tv_sec);  */
         delta_t = (((float)(tn.tv_sec - t->tv_sec)*Time_Fact) +
                     (float)(tn.tv_usec - t->tv_usec)) /Time_Fact;
      }
   else
      {
         t->tv_sec = tn.tv_sec;
         t->tv_usec = tn.tv_usec;
         /*fprintf(stderr,"%s: Initialized to %f sec \n",
                           FuncName, (float)tn.tv_sec); */
         delta_t = 0.0;
      }

   SUMA_RETURN (delta_t);

}/*SUMA_etime*/

/*
   Return time stamp in a string.
   yymmdd_hhmmss.MMM
   with MMM being a 3 digit msec stamp that
   is useless except for minimizing the chance of similar stamps
   in repeated calls. You can't rely on the msec part to be accurate.

   Do not free returned string.
   \sa SUMA_time
*/
char *SUMA_time_stamp(void )
{
   struct  timeval  tn;
   time_t tnow = time(NULL) ;
   static char res[64], ttt[31];

   gettimeofday(&tn, NULL);
   strftime(ttt,31*sizeof(char),"%y%m%d_%H%M%S", localtime(&tnow));
   snprintf(res,63*sizeof(char),"%s.%03d", ttt, (int)(tn.tv_usec/1000));
   return(res);
}

char *SUMA_time(void)
{
   static char dt[32]={"??:??:??"}, *tm;
   time_t tnow = time(NULL) ;

   strftime(dt,31*sizeof(char),"%H:%M:%S", localtime(&tnow));

   return(dt);
}

/*!

File : SUMA_MiscFunc.c, from ~Zlib/code/isinsphere.c
Author : Ziad Saad
Date : Fri Nov 20 22:56:31 CST 1998

Purpose :
   determines which nodes lie inside a sphere


Usage :
      Ret =  SUMA_isinsphere (NodeList, nr, S_cent , S_rad , BoundIn)

Input paramters :
   NodeList (float * ) : Nx3 vector containing the NodeList of the nodes
                        to consider
   nr  (int )   : that's N, the number of nodes
   S_cent (float *) : a 3x1 vector containing the
                      coordinates of the center of the sphere
   S_rad  (float ) : the radius of the sphere
   BoundIn (int) : 0/1 set to 0 for exclusive boundary


Returns :
   a structure of the type SUMA_ISINSPHERE with the following fields

   .IsIn    (int *) : a pointer to an [nIsIn x 1] vector will contain
                     the indices into the rows of NodeList that
                     locates the nodes inside the sphere.
   .nIsIn   (int) : the number of nodes in the sphere
   .d (float *) : a pointer to an [nIsIn x 1]  vector containing
                  the distance of those nodes inside the sphere to the center.

Support :

Side effects :

***/
SUMA_ISINSPHERE SUMA_isinsphere (float * NodeList, int nr, float *S_cent ,
                                 float S_rad , int BoundIn )
{/*SUMA_isinsphere*/
   static char FuncName[]={"SUMA_isinsphere"};
   float *t, t0, t1, t2, ta;
   int k, *IsIn, id, ND;
   SUMA_ISINSPHERE IsIn_strct;

   SUMA_ENTRY;

   ND = 3;
   IsIn_strct.nIsIn = 0;
   IsIn_strct.dXYZ = NULL;
   IsIn_strct.IsIn = NULL;
   IsIn_strct.d = NULL;

   t = (float *) SUMA_calloc (nr, sizeof(float));
   IsIn = (int *) SUMA_calloc (nr, sizeof(int));

   if (!t || !IsIn)
      {
         SUMA_alloc_problem (FuncName);
         SUMA_RETURN (IsIn_strct);
      }


   if (BoundIn) /* split into two to avoid checking for this
                  condition all the time */
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

/* Same as SUMA_isinsphere but return a byte mask */
byte *SUMA_isinsphere_bm (float * NodeList, int nr, float *S_cent ,
                                 float S_rad , int BoundIn )
{/*SUMA_isinsphere_bm*/
   static char FuncName[]={"SUMA_isinsphere_bm"};
   int k;
   SUMA_ISINSPHERE IsIn_strct;
   byte *bm = NULL;

   SUMA_ENTRY;

   if (!NodeList || !nr) SUMA_RETURN(bm);
   IsIn_strct = SUMA_isinsphere(NodeList, nr, S_cent, S_rad, BoundIn);
   bm = (byte *)calloc(nr, sizeof(byte));
   for (k=0; k<IsIn_strct.nIsIn;++k) bm[IsIn_strct.IsIn[k]]=1;
   SUMA_Free_IsInSphere(&IsIn_strct);

   SUMA_RETURN(bm);
}

/*!
free SUMA_ISINSPHERE structure contents.
Structure pointer is not freed
*/
SUMA_Boolean SUMA_Free_IsInSphere (SUMA_ISINSPHERE *IB)
{
   static char FuncName[]={"SUMA_Free_IsInSphere"};

   SUMA_ENTRY;

   if (IB == NULL) {
      fprintf (SUMA_STDERR,
               "Error SUMA_Free_IsInSphere: pointer to null cannot be freed\n");
      SUMA_RETURN (NOPE);
   }
   if (IB->IsIn != NULL) SUMA_free(IB->IsIn);
   if (IB->d != NULL) SUMA_free(IB->d);
   IB->nIsIn = 0;
   SUMA_RETURN (YUP);
}

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
SUMA_ISINBOX SUMA_isinbox (float * XYZ, int nr,
                           float *S_cent , float *S_dim , int BoundIn )
{/*SUMA_isinbox*/

   static char FuncName[]={"SUMA_isinbox"};
   float t0, t1, t2, hdim0, hdim1, hdim2, *d;
   int k , *IsIn, id, ND;
   SUMA_ISINBOX IsIn_strct;

   SUMA_ENTRY;

   ND = 3;
   /*
   fprintf(SUMA_STDOUT,"%f %f %f, %f %f %f, %d, %f, %f, %f\n",
      S_cent[0], S_cent[1], S_cent[2], S_dim[0], S_dim[1], S_dim[2],
      nr, XYZ[0], XYZ[1], XYZ[2]);
   */

   IsIn_strct.nIsIn = 0;
   IsIn_strct.dXYZ = NULL;
   IsIn_strct.d = NULL;
   IsIn_strct.IsIn = NULL;

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

   if (BoundIn) {/* split to avoid checking for this condition all the time */
      /*fprintf(SUMA_STDERR,"%s: inbound\n", FuncName);*/
      for (k=0; k < nr; ++k)
         {
         /*fprintf(SUMA_STDERR,"%s: inbound %d\n", FuncName, k);*/
         /* relative distance to center */
            id = ND * k;
            t0 = hdim0 - fabs(XYZ[id] - S_cent[0]);

            if (t0 >= 0) {
               t1 = hdim1 - fabs(XYZ[id+1] - S_cent[1]);
               if (t1 >= 0) {
                  t2 = hdim2 - fabs(XYZ[id+2] - S_cent[2]);
                  if (t2 >= 0)
                     {
                        IsIn[IsIn_strct.nIsIn] = k;
                        d[IsIn_strct.nIsIn] = sqrt(t0*t0+t1*t1+t2*t2);
                        ++(IsIn_strct.nIsIn);
                     }
               }
            }
         }
         /*fprintf(SUMA_STDERR,"%s: outbound\n", FuncName);*/
   } else {
      for (k=0; k < nr; ++k)
         {
            /* relative distance to center */
            id = ND * k;
            t0 = hdim0 - fabs(XYZ[id] - S_cent[0]);

            if (t0 > 0) {
               t1 = hdim1 - fabs(XYZ[id+1] - S_cent[1]);
               if (t1 > 0) {
                  t2 = hdim2 - fabs(XYZ[id+2] - S_cent[2]);
                  if (t2 > 0)
                     {
                        IsIn[IsIn_strct.nIsIn] = k;
                        d[IsIn_strct.nIsIn] = sqrt(t0*t0+t1*t1+t2*t2);
                        ++(IsIn_strct.nIsIn);
                     }
               }
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

/*! much faster than isinbox, send NULL for dinsq if you don't care for it
   n = SUMA_nodesinbox2(xyz, nr, s_cent, s_dim, nodein, dinsq);
   \param xyz (float *) xyz triplets
   \param nr (int) number of triplets
   \param s_cent (float *) xyz for boxes' center
   \param s_dim (float *) side to side dimensions of the box
   \param nodesin (int *) to contain indices of nodes in box.
                           if nodesin[i]=nnn when i < Nin, then
                           node nnn is inside the box.
   \param dinsq (float *) to contain the squared distance from a node
                          to the center of the box. Send NULL if you do not
                          care for it.
   \return Nin (int) number of nodes inside box.
         You must pre-allocate nr values for each of nodesin and dinsq .
         But on Nin values are meaningful
*/
int SUMA_nodesinbox2 (float *XYZ, int nr,
                      float *S_cent , float *S_dim ,
                      int *nodesin, float *dinsq)
{
   static char FuncName[]={"SUMA_nodesinbox2"};
   int nin = -1;
   float hdim0, hdim1, hdim2, t0, t1, t2;
   int k, id;
   SUMA_ENTRY;

   hdim0 = S_dim[0]/2.0;
   hdim1 = S_dim[1]/2.0;
   hdim2 = S_dim[2]/2.0;

   /* Inclusive boundary mode */

         nin = 0;
         /*fprintf(SUMA_STDERR,"%s: inbound\n", FuncName);*/
         for (k=0; k < nr; ++k)
            {
            /*fprintf(SUMA_STDERR,"%s: inbound %d\n", FuncName, k);*/
            /* relative distance to center */
               id = 3 * k;
               t0 = hdim0 - SUMA_ABS(XYZ[id] - S_cent[0]);

               if (t0 >= 0) {
                  t1 = hdim1 - SUMA_ABS(XYZ[id+1] - S_cent[1]);
                  if (t1 >= 0) {
                     t2 = hdim2 - SUMA_ABS(XYZ[id+2] - S_cent[2]);
                     if (t2 >= 0)
                        {
                           nodesin[nin] = k;
                           if (dinsq) dinsq[nin] = (t0*t0+t1*t1+t2*t2);
                           ++nin;
                        }
                  }
               }
            }
            /*fprintf(SUMA_STDERR,"%s: outbound\n", FuncName);*/

   SUMA_RETURN(nin);
}
/* Same as SUMA_nodesinbox2 but return a byte mask */
byte *SUMA_nodesinbox2_bm (float *NodeList, int nr,
                        float *S_cent , float *S_edge,
                        byte *bmu)
{/*SUMA_nodesinbox2_bm*/
   static char FuncName[]={"SUMA_nodesinbox2_bm"};
   int k, nin;
   int *nodesin=NULL;
   byte *bm = NULL;

   SUMA_ENTRY;

   if (!NodeList || !nr) SUMA_RETURN(bm);
   nodesin = (int *)SUMA_calloc(nr, sizeof(int));
   nin = SUMA_nodesinbox2(NodeList, nr, S_cent, S_edge, nodesin, NULL);
   if (!bmu) bm = (byte *)calloc(nr, sizeof(byte));
   else bm = bmu;
   for (k=0; k<nin;++k) bm[nodesin[k]]=1;
   SUMA_free(nodesin); nodesin = NULL;

   SUMA_RETURN(bm);
}



/* same as nodesinbox2 only sdim is one float, specifying the RADIUS,
see SUMA_NODESINSPHERE2 for slimmed, slightly faster version*/
int SUMA_nodesinsphere2 (float *XYZ, int nr, float *S_cent , float S_dim ,
                         int *nodesin, float *dinsq)
{
   static char FuncName[]={"SUMA_nodesinsphere2"};
   int k;
   int nin = -1, id;
   float t0, t1, t2, d2, r2;

   SUMA_ENTRY;

   r2 = S_dim*S_dim;
   nin = 0;
         /*fprintf(SUMA_STDERR,"%s: inbound\n", FuncName);*/
         for (k=0; k < nr; ++k)
            {
            /*fprintf(SUMA_STDERR,"%s: inbound %d\n", FuncName, k);*/
            /* relative distance to center */
               id = 3 * k;
               t0 = SUMA_ABS(XYZ[id] - S_cent[0]);

               if (t0 <= S_dim) {
                  t1 = SUMA_ABS(XYZ[id+1] - S_cent[1]);
                  if (t1 <= S_dim) {
                     t2 = SUMA_ABS(XYZ[id+2] - S_cent[2]);
                     if (t2 <= S_dim)
                        {
                           /* in box, is it in sphere? */
                           d2 = (t0*t0+t1*t1+t2*t2);
                           if (d2 <=r2) {
                              nodesin[nin] = k;
                              if (dinsq) dinsq[nin] = d2;
                              ++nin;
                           }
                        }
                  }
               }
            }
            /*fprintf(SUMA_STDERR,"%s: outbound\n", FuncName);*/

   SUMA_RETURN(nin);
}
/* Same as SUMA_nodesinsphere2 but return a byte mask */
byte *SUMA_nodesinsphere2_bm (float * NodeList, int nr,
                           float *S_cent , float S_rad,
                           byte *bmu)
{/*SUMA_nodesinsphere2_bm*/
   static char FuncName[]={"SUMA_nodesinsphere2_bm"};
   int k, nin;
   int *nodesin=NULL;
   byte *bm = NULL;

   SUMA_ENTRY;

   if (!NodeList || !nr) SUMA_RETURN(bm);
   nodesin = (int *)SUMA_calloc(nr, sizeof(int));
   nin = SUMA_nodesinsphere2(NodeList, nr, S_cent, S_rad, nodesin, NULL);
   if (!bmu) bm = (byte *)calloc(nr, sizeof(byte));
   else bm = bmu;
   for (k=0; k<nin;++k) bm[nodesin[k]]=1;
   SUMA_free(nodesin); nodesin = NULL;

   SUMA_RETURN(bm);
}



/*!
free SUMA_ISINBOX structure contents.
Structure pointer is not freed
*/
SUMA_Boolean SUMA_Free_IsInBox (SUMA_ISINBOX *IB)
{
   static char FuncName[]={"SUMA_Free_IsInBox"};

   SUMA_ENTRY;

   if (IB == NULL) {
      fprintf (SUMA_STDERR,"Error SUMA_Free_IsInBox: pointer to null cannot be freed\n");
      SUMA_RETURN (NOPE);
   }
   if (IB->IsIn != NULL) SUMA_free(IB->IsIn);
   if (IB->d != NULL) SUMA_free(IB->d);
   IB->nIsIn = 0;
   SUMA_RETURN (YUP);
}

/*!
   \brief Determines is a point in 2D is inside a polygon with no holes.
   The function's parameters are abit strange because of the intended use.

   \param P (float *) 3x1 vector containing XYZ of the point to test
   \param NodeList (float *) the proverbial nodelist xyz xyz xyz etc.
   \param FaceSetList (int *) the proverbial FaceSetList defining polygons
   \param N_FaceSet (int) number of polygons in each list
   \param FaceSetDim (int) number of points forming polygon
   \param dims (int) 2x1 vector indicating which dimensions to consider.
                     Recall this a 2D inclusion function! For example,
                     if dims = [ 0 1] then the z coordinate (2) is not considered
                     if dims = [ 0 2] then the y coordinate (1) is not considered
                     if dims = [ 2 1] then the x coordinate (0) is not considered
   \params N_in (int *) to contain the number of polygons that contain p
   \param usethis (byte *) use this vector to store results (see return param)
   \return isin (byte *) if isin[iv] the point P is in polygon iv


   core based on code by Paul Bourke, see copyright notice in suma -sources
   Does not work for polys with holes
*/
byte * SUMA_isinpoly(float *P, float *NodeList,
                     int *FaceSetList, int N_FaceSet,
                     int FaceSetDim, int *dims, int *N_in,
                     byte *usethis, byte *culled)
{
   static char FuncName[]={"SUMA_isinpoly"};
   byte *isin=NULL;
   int iv, i, ip, counter, ni;
   double xinters;
   float p1[2], p2[2], p[2], poly[300];
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   *N_in = 0;
   if (!usethis) {
      isin = (byte *)SUMA_malloc(sizeof(byte)*N_FaceSet);
      if (!isin) {
         SUMA_SL_Crit("Failed to allocate!");
         SUMA_RETURN(NOPE);
      }
   } else isin = usethis;
   if (FaceSetDim > 99) {
      SUMA_SL_Err("max FaceSetDim = 99");
      SUMA_RETURN(NULL);
   }
   if (dims[0] < 0 || dims[0] > 2 || dims[1] < 0 || dims[1] > 2) {
      SUMA_SL_Err("dims is a 2x1 vector with allowed values of 0 1 or 2 only.");
      SUMA_RETURN(NULL);
   }

   p[0] = P[dims[0]]; p[1] = P[dims[1]]; /* the point of interest */
   for (iv = 0; iv < N_FaceSet; ++iv) {
      counter = 0;
      for (i=0; i<FaceSetDim; ++i) { /* form the polygon coordinate vector */
         ni = FaceSetList[FaceSetDim*iv+i];
         poly[3*i] = NodeList[3*ni];
         poly[3*i+1] = NodeList[3*ni+1]; poly[3*i+2] = NodeList[3*ni+2];
      }
      if (culled) if (culled[iv]) continue;

      p1[0] = poly[dims[0]]; p1[1] = poly[dims[1]]; /* the very first point */
      for (i=1; i <=FaceSetDim; ++i) {
         ip = i % FaceSetDim;
         p2[0] = poly[3*ip+dims[0]]; p2[1] = poly[3*ip+dims[1]];
         if (p[1] > SUMA_MIN_PAIR(p1[1], p2[1])) {
            if (p[1] <= SUMA_MAX_PAIR(p1[1], p2[1])) {
               if (p[0] <= SUMA_MAX_PAIR(p1[0], p2[0])) {
                  if (p1[1] != p2[1]) {
                     xinters = (p[1]-p1[1])*(p2[0]-p1[0])/(p2[1]-p1[1])+p1[0];
                     if (p1[0] == p2[0] || p[0] <= xinters) {
                        counter++;
                     }
                  }
               }
            }
         }
         p1[0] = p2[0]; p1[1] = p2[1];
      }

      if (counter % 2 == 0) {
         isin[iv] = 0;
      } else {
         isin[iv] = 1; ++(*N_in); /* p is inside polygon iv */
         #if 0
         if (LocalHead)
         {
            int kk;
            fprintf(SUMA_STDERR,"\n%%hit!\nPoly = [");
            for (kk=0; kk < FaceSetDim; ++kk) {
               fprintf(SUMA_STDERR,"%.2f %.2f; ", poly[3*kk+dims[0]] , poly[3*kk+dims[1]]);
            } fprintf(SUMA_STDERR,"%.2f %.2f] \np = [%.3f %.3f];", poly[dims[0]], poly[dims[1]], p[0], p[1]);
         }
         #endif
      }
   }

   SUMA_RETURN(isin);
}

/*!
          mandatory input
   P (float *)3 coords of point P in center of wedge
   C (float *)3 coords of center of 2 spheres of radii r1 and r2
   rr1 (float) squared radius of inner sphere
   rr2 (float) squared radius of outer sphere
   coshalpha (float) cosine of half angle of wedge
   Q (float *)3 coords of point in question
         Optional
   uCP(float *) if not NULL, then this is the unit vector CP
         Pass it to speed up computations

          returned
   rrQ (float*) square of distance from Q to C
                -1.0 when nothing is computed
   cosaQ (float *) cosine of angle PCQ (using -2.0 for flag of no
                   angle computed. Happens when point is outside)

   return value: 1 for in, 0 for out
*/
int is_in_wedge(float *P, float *C, float rr1, float rr2, float coshalpha,
                float *Q, float *uCP, float *rrQ, float *cosaQ)
{
   float dp, dot, rr, rrP;
   float CP[3], CQ[3];

   if (!P || !C || rr2<=0.0) {
      if (cosaQ) *cosaQ=-2.0;
      if (rrQ) *rrQ=-1.0;
      return(0);
   }

   /* Distance check */
   CQ[0] = Q[0]-C[0];
   CQ[1] = Q[1]-C[1];
   CQ[2] = Q[2]-C[2];
   rr = CQ[0]*CQ[0]+CQ[1]*CQ[1]+CQ[2]*CQ[2];
   if (rrQ) *rrQ=rr;

   if (rr<rr1 || rr>rr2 || rr == 0.0) {
      if (cosaQ) *cosaQ=-2.0;
      return(0);
   }
   /* angle check */
   rr = sqrtf(rr);
   CQ[0] /= rr;  CQ[1] /= rr; CQ[2] /= rr;/* Normalize CQ */

   if (!uCP) {
      CP[0] = P[0]-C[0];
      CP[1] = P[1]-C[1];
      CP[2] = P[2]-C[2];
      rrP = sqrtf(CP[0]*CP[0]+CP[1]*CP[1]+CP[2]*CP[2]);
      CQ[0] /= rrP;  CQ[1] /= rrP; CQ[2] /= rrP;/* Normalize CP */
      dot = SUMA_MT_DOT(CP,CQ);
   } else {
      dot = SUMA_MT_DOT(uCP,CQ);
   }
   if (dot >= coshalpha) {
      if (cosaQ) *cosaQ=dot;
      return(1);
   } else {
      if (cosaQ) *cosaQ=2.0;
   }
   return(0);
}

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

\sa SUMA_POINT_AT_DISTANCE and SUMA_POINT_AT_DISTANCE_NORM macro

***/
float **SUMA_Point_At_Distance(float *U, float *P1, float d)
{/*SUMA_Point_At_Distance*/
   static char FuncName[]={"SUMA_Point_At_Distance"};
   float bf, **P2, P1orig[3], Uorig[3];
   float m, n, p, q, D, A, B, C, epsi = 0.0001;
   int flip, i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   SUMA_SL_Warn ("useless piece of junk, use SUMA_POINT_AT_DISTANCE instead!");

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
   if (fabs(U[0]) < epsi) { /* must flip X with some other coordinate */
      if (fabs(U[1]) > epsi) {/*U[1] != 0; */
         U[0] = U[1]; U[1] = 0;
         bf = P1[0]; P1[0] = P1[1]; P1[1] = bf;
         flip = 1;
      } else {   /*U[1] = 0; */
         if (fabs(U[2]) > epsi) { /* U[2] != 0 */
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
      fprintf(SUMA_STDERR, "Error %s: Negative Delta: %f.\n"
                           "Input values were: \n"
                           "U :[%f %f %f]\n"
                           "P1:[%f %f %f]\n"
                           "d :[%f]\n"
                           , FuncName, D, Uorig[0], Uorig[1], Uorig[2],
                           P1orig[0], P1orig[1], P1orig[2], d);
      SUMA_RETURN(NULL);
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
double **SUMA_dPoint_At_Distance(double *U, double *P1, double d)
{/*SUMA_dPoint_At_Distance*/
   static char FuncName[]={"SUMA_dPoint_At_Distance"};
   double bf, **P2, P1orig[3], Uorig[3];
   double m, n, p, q, D, A, B, C, epsi = 0.000001;
   int flip, i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   SUMA_SL_Warn ("useless piece of junk, use SUMA_POINT_AT_DISTANCE instead!");

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
   if (fabs(U[0]) < epsi) { /* must flip X with some other coordinate */
      if (fabs(U[1]) > epsi) {/*U[1] != 0; */
         U[0] = U[1]; U[1] = 0;
         bf = P1[0]; P1[0] = P1[1]; P1[1] = bf;
         flip = 1;
      } else {   /*U[1] = 0; */
         if (fabs(U[2]) > epsi) { /* U[2] != 0 */
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
      fprintf(SUMA_STDERR, "Error %s: Negative Delta: %f.\n"
                           "Input values were: \n"
                           "U :[%f %f %f]\n"
                           "P1:[%f %f %f]\n"
                           "d :[%f]\n"
                           , FuncName, D, Uorig[0], Uorig[1], Uorig[2],
                           P1orig[0], P1orig[1], P1orig[2], d);
      SUMA_RETURN(NULL);
   }

   P2 = (double **)SUMA_allocate2D(2,3, sizeof(double));
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

   SUMA_DOTP_VEC(Uorig, U, bf, 3, double, double)
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

}/*SUMA_dPoint_At_Distance*/

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

   SUMA_ENTRY;

   ND = 3;
   if (N_points < 1) {
      fprintf(SUMA_STDERR,"Error %s: N_points is 0.\n",FuncName);
      SUMA_RETURN (NOPE);
   }

   SUMA_UNIT_VEC(P1, P2, U, Un);
   if (Un == 0) {
      fprintf(SUMA_STDERR,"Error %s: P1 and P2 are identical.\n",FuncName);
      SUMA_RETURN (NOPE);
   }



   /* calculate the distances and keep track of the minimum distance while you're at it */

   /*bad practice, only returned pointers are allocated for in functions */
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

   SUMA_ENTRY;

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

#define SUMA_DOT3(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
/*!
   Find the shortest distance from each point in 3D space to
   a triangle.
   Based on algorithm by David Eberly per document:
   http://www.geometrictools.com/Documentation/DistancePoint3Triangle3.pdf

   \param Points (float *) N_points x 3 vector of coordinates
   \param N_points (int) number of points in Points
   \param P0, P1, P2 (float *) XYZ coords of each vertex in the triangle
   \param itri (int) an integer representing the triangle's ID
   \param distp (float **) pointer to array which will contain the (squared if city==0)
                           distance from each point to the triangle <P0, P1, P2>
                if (*distp == NULL) it is allocated for and initialized and
                                    *distp[i] contains SD, the shortest distance
                                    of point i to the triangle itri.
               else *distp[i] = SD if (SD < *distp[i])
                                    otherwise leave distp[i] alone
   \param closestp (int **) pointer to array wich will contain for each point i
                                    the index of the triangle itri which
                                    resulted in the value of *distp[i]
   \param city (byte) 1 == City block distance, 0 == Euclidian distance squared
   \return NOPE on FAILURE, YUP on SUCCESS.
   This function is meant to be called repeatedly for each new triangle.
   See SUMA_Shortest_Point_To_Triangles_Distance()
*/
int SUMA_Point_To_Triangle_Distance (float *Points, int N_points,
                                     float *P0, float *P1, float *P2, int itri,
                                     float *tnorm,
                                     float **distp, int **closestp, byte **sgnp,
                                     byte city)
{
   static char FuncName[]={"SUMA_Point_To_Triangle_Distance"};
   float *dist=NULL, *P=NULL;
   double E0[3], E1[3], a, b, c, d, e, f, B[3], BmP[3], nd,
         s, t, det, idet, numer, denom, I[3], sd, tmp0, tmp1;
   int in=0, reg, in3, *closest=NULL;
   byte *sgn=NULL;
   static int icall;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (*distp == NULL) {
      dist = (float *)SUMA_calloc(N_points, sizeof(float));
      *distp = dist;
      for (in=0; in<N_points; ++in) dist[in]=-1.0;
      SUMA_LHv("icall %d, init done, itri=%d, %d points, dist=%p\n",
                     icall, itri, N_points, dist);
   } else {
      dist = *distp;
      if (icall < 3) SUMA_LHv("icall %d, reusing, itri=%d, dist=%p\n",
                              icall, itri, dist);
   }
   if (closestp) {
      if (*closestp == NULL) {
         closest = (int *)SUMA_calloc(N_points, sizeof(int));
         *closestp = closest;
         for (in=0; in<N_points; ++in) closest[in]=-1;
         SUMA_LHv("icall %d, init closest done\n", icall);
      } else {
         if (icall < 3) SUMA_LHv("icall %d, reusing closest\n", icall);
         closest = *closestp;
      }
   }
   if (sgnp) {
      if (*sgnp == NULL) {
         sgn = (byte *)SUMA_calloc(N_points, sizeof(byte));
         *sgnp = sgn;
      } else {
         sgn = *sgnp;
      }
   }

   E0[0] = P1[0]-P0[0]; E0[1] = P1[1]-P0[1]; E0[2] = P1[2]-P0[2];
   E1[0] = P2[0]-P0[0]; E1[1] = P2[1]-P0[1]; E1[2] = P2[2]-P0[2];
   B[0] = P0[0]; B[1] = P0[1]; B[2] = P0[2];

   a = SUMA_DOT3(E0, E0); b = SUMA_DOT3(E0, E1); c = SUMA_DOT3(E1, E1);
   for (in=0; in < N_points; ++in) {
      in3 = 3*in; P = Points+in3;
      BmP[0] = B[0]-P[0]; BmP[1] = B[1]-P[1]; BmP[2] = B[2]-P[2];
      d = SUMA_DOT3(E0, BmP);
      e = SUMA_DOT3(E1, BmP);
      f = SUMA_DOT3(BmP, BmP);
      det = a*c-b*b; s = b*e-c*d; t=b*d-a*e;
      reg = -1;
      if (s+t <= det) {
         if (s < 0) {
            if (t < 0) reg = 4; else reg = 3;
         } else if (t < 0) {
            reg = 5;
         } else {
            reg = 0;
         }
      } else {
         if (s < 0) reg = 2;
         else if (t < 0) reg = 6;
         else reg = 1;
      }
      switch (reg) {
         case 0:
            idet = 1.0/det;
            s *= idet;
            t *= idet;
            break;
         case 1:
            numer = c+e-b-d;
            if (numer <= 0) {
               s = 0;
            } else {
               denom = a-2*b+c;
               s = ( numer > denom ? 1 : numer/denom);
            }
            t = 1-s;
            break;
         case 3:
         case 5:
            s = 0;
            t = ( e >= 0 ? 0 : ( -e >= c ? 1: -e/c ) );
            break;
         case 2:
         case 4:
         case 6:
            tmp0 = b+d;
            tmp1 = c+e;
            if (tmp1 > tmp0) {
               numer = tmp1 - tmp0;
               denom = a- 2*b + c;
               s = (numer >= denom ? 1:numer/denom);
               t = 1-s;
            } else {
               s = 0;
               t = (tmp1 <= 0 ? 1 : ( e >= 0 ? 0 : -e/c));
            }
            break;
         default:
            SUMA_S_Errv("Reg %d not good\n", reg);
            RETURN(NOPE);
      }
      SUMA_FROM_BARYCENTRIC(s, t, P0, P1, P2, I);
      I[0] = I[0]-P[0]; I[1] = I[1]-P[1]; I[2] = I[2]-P[2];
      if (city) {
         sd = SUMA_ABS(I[0])+SUMA_ABS(I[1])+SUMA_ABS(I[2]);
      } else {
         sd = I[0]*I[0]+I[1]*I[1]+I[2]*I[2];
      }
      if (dist[in] < 0) {
         dist[in] = (float)sd;
         if (closest) closest[in] = itri;
         if (tnorm && sgn) {
            nd = SUMA_DOT3(I,tnorm);
            if (SUMA_SIGN(nd) <0) sgn[in] = 1;
            else sgn[in] = 2;
         }
      } else if (dist[in] > (float)sd) {
         dist[in] = (float)sd;
         if (closest) closest[in] = itri;
         if (tnorm && sgn) {
            nd = SUMA_DOT3(I,tnorm);
            if (SUMA_SIGN(nd) <0) sgn[in] = 1;
            else sgn[in] = 2;
         }
      }
      SUMA_LHv("reg = %d, s=%f, t=%f, P=[%f %f %f], "
               "I=[%f %f %f] dist2[%d]=%f, sign=%d\n",
               reg, s, t, P[0], P[1], P[2],
               I[0], I[1], I[2], in, dist[in],
               (sgn && sgn[in]==1) ? -1:1);
   } /* for each point */
   ++icall;
   SUMA_RETURN(YUP);
}

void Bad_Optimizer_Bad_Bad() {
   static int icall=0;/* Need to do something stupid, else I get crash on OSX*/
   if (!icall) {
      fprintf(stderr,"\n");
      ++icall;
   }
   return;
}

/* Square of distance is computed, if city == 0 */
SUMA_Boolean SUMA_Shortest_Point_To_Triangles_Distance(
         float *Points, int N_points,
         float *NodeList, int *FaceSetList, int N_FaceSet,
         float *FaceNormList,
         float **distp, int **closestp, byte **sgnp,
         byte city ) {
   static char FuncName[]={"SUMA_Shortest_Point_To_Triangles_Distance"};
   float  *P0, *P1, *P2;
   int i=0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   for (i=0; i<N_FaceSet; ++i) {
      P0 = NodeList + 3*FaceSetList[3*i  ];
      P1 = NodeList + 3*FaceSetList[3*i+1];
      P2 = NodeList + 3*FaceSetList[3*i+2];
      Bad_Optimizer_Bad_Bad();
      SUMA_LHv("Tri %d\n"
               "[%f %f %f]\n"
               "[%f %f %f]\n"
               "[%f %f %f]\n"
               , i,
               P0[0], P0[1], P0[2],
               P1[0], P1[1], P1[2],
               P2[0], P2[1], P2[2]);
      if (!SUMA_Point_To_Triangle_Distance(Points, N_points,
                                           P0, P1, P2, i,
                                           FaceNormList+3*i,
                                           distp, closestp, sgnp,
                                           city)) {
         SUMA_S_Errv("Failed at triangle %d\n", i);
         SUMA_RETURN(NOPE);
      }
   }
   SUMA_RETURN(YUP);
}


/*! Sorting Functions */
#define SUMA_Z_QSORT_structs

/* DO not add debugging in the sorting functions since that might slow them down */

   typedef struct {
      float x;
      int Index;
   } SUMA_Z_QSORT_FLOAT;

   typedef struct {
      double x;
      int Index;
   } SUMA_Z_QSORT_DOUBLE;

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

int compare_SUMA_Z_QSORT_DOUBLE (SUMA_Z_QSORT_DOUBLE *a, SUMA_Z_QSORT_DOUBLE *b )
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

int SUMA_compare_float (float *a, float *b )
{/*SUMA_compare_float*/
    if (*a < *b)
      return (-1);
   else if (*a == *b)
      return (0);
   else
      return (1);

}/*SUMA_compare_float*/

int SUMA_compare_double (double *a, double *b )
{/*SUMA_compare_double*/
    if (*a < *b)
      return (-1);
   else if (*a == *b)
      return (0);
   else
      return (1);

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
   static char FuncName[]={"SUMA_z_qsort"};
   int *I, k;
   SUMA_Z_QSORT_FLOAT *Z_Q_fStrct;

   SUMA_ENTRY;

   /* allocate for the structure */
   Z_Q_fStrct = (SUMA_Z_QSORT_FLOAT *)
               SUMA_calloc(nx, sizeof (SUMA_Z_QSORT_FLOAT));
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
   qsort(Z_Q_fStrct, nx, sizeof(SUMA_Z_QSORT_FLOAT),
         (int(*) (const void *, const void *)) compare_SUMA_Z_QSORT_FLOAT);

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

int *SUMA_z_doubqsort (double *x , int nx )
{/*SUMA_z_qsort*/
   static char FuncName[]={"SUMA_z_doubqsort"};
   int *I, k;
   SUMA_Z_QSORT_DOUBLE *Z_Q_doubStrct;

   SUMA_ENTRY;

   /* allocate for the structure */
   Z_Q_doubStrct = (SUMA_Z_QSORT_DOUBLE *) SUMA_calloc(nx, sizeof (SUMA_Z_QSORT_DOUBLE));
   I = (int *) SUMA_calloc (nx, sizeof(int));

   if (!Z_Q_doubStrct || !I)
      {
         fprintf(SUMA_STDERR,"Error %s: Allocation problem.\n",FuncName);
         SUMA_RETURN (NULL);
      }

   for (k=0; k < nx; ++k) /* copy the data into a structure */
      {
         Z_Q_doubStrct[k].x = x[k];
         Z_Q_doubStrct[k].Index = k;
      }

   /* sort the structure by it's field value */
   qsort(Z_Q_doubStrct, nx, sizeof(SUMA_Z_QSORT_DOUBLE), (int(*) (const void *, const void *)) compare_SUMA_Z_QSORT_DOUBLE);

   /* recover the index table */
   for (k=0; k < nx; ++k) /* copy the data into a structure */
      {
         x[k] = Z_Q_doubStrct[k].x;
         I[k] = Z_Q_doubStrct[k].Index;
      }

   /* free the structure */
   SUMA_free(Z_Q_doubStrct);

   /* return */
   SUMA_RETURN (I);


}/*SUMA_z_doubqsort*/

int *SUMA_z_dqsort (int *x , int nx )
{/*SUMA_z_dqsort*/
   static char FuncName[]={"SUMA_z_dqsort"};
   int *I, k;
   SUMA_Z_QSORT_INT *Z_Q_iStrct;

   SUMA_ENTRY;

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

   SUMA_ENTRY;

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


/*--------------------- Matrix Sorting functions Begin ------------------------*/

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


   SUMA_ENTRY;

   /* allocate for the structure */
   Z_Q_fStrct = (SUMA_QSORTROW_FLOAT *)
                  SUMA_calloc(nr, sizeof (SUMA_QSORTROW_FLOAT));
   I = (int *) SUMA_calloc (nr,sizeof(int));

   if (!Z_Q_fStrct || !I)
      {
      fprintf(SUMA_STDERR,
              "Error %s: Failed to allocate for Z_Q_fStrct || I\n", FuncName);
      SUMA_RETURN (NULL);
      }

   for (k=0; k < nr; ++k) /* copy the data into a structure */
      {
         Z_Q_fStrct[k].x = X[k];
         Z_Q_fStrct[k].ncol = nc;
         Z_Q_fStrct[k].Index = k;
      }

   /* sort the structure by comparing the rows in X */
   qsort(Z_Q_fStrct, nr, sizeof(SUMA_QSORTROW_FLOAT),
         (int(*) (const void *, const void *)) compare_SUMA_QSORTROW_FLOAT);

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

   SUMA_ENTRY;

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
static int VoxIntersDbg = 0;
void SUMA_Set_VoxIntersDbg(int v)
{
   VoxIntersDbg = v;
   return;
}

/*!
   \brief Returns the coordinates of a voxel corner
   \param center (float *): center of voxel
   \param dxyz (float *): dimensions of voxel
   \param en (int): corner of voxel (0 -- 7)
   \param P0 (float *): corner coords (set by macro)
   See labbook NIH-6 pp 201 for a recent diagram of edge and corner numbers
*/
#define SUMA_CORNER_OF_VOXEL(center, dxyz, cn, P){\
   switch(cn) {   \
      case 0:  \
         P[0] =  dxyz[0]; P[1] = -dxyz[1]; P[2] =   dxyz[2];   \
         break;   \
      case 1:  \
         P[0] =  dxyz[0]; P[1] = -dxyz[1]; P[2] =  -dxyz[2];   \
         break;   \
      case 2:  \
         P[0] =  dxyz[0]; P[1] =  dxyz[1]; P[2] =  -dxyz[2];   \
         break;  \
      case 3:  \
         P[0] =  dxyz[0]; P[1] =  dxyz[1]; P[2] =   dxyz[2];   \
         break;   \
      case 4:  \
         P[0] = -dxyz[0]; P[1] = -dxyz[1]; P[2] =   dxyz[2];   \
         break;   \
      case 5:  \
         P[0] = -dxyz[0]; P[1] = -dxyz[1]; P[2] =  -dxyz[2];   \
         break;   \
      case 6:  \
         P[0] = -dxyz[0]; P[1] =  dxyz[1]; P[2] =  -dxyz[2];   \
         break;  \
      case 7:  \
         P[0] = -dxyz[0]; P[1] =  dxyz[1]; P[2] =   dxyz[2];   \
         break;  \
      default: \
         P[0] = 0; P[1] = 0; P[2] = 0; \
         SUMA_SL_Err("Bad corner index. Returning Center of voxel.");\
   }  \
   /* add center coord */  \
   P[0] = center[0] + 0.5 * P[0];   \
   P[1] = center[1] + 0.5 * P[1];   \
   P[2] = center[2] + 0.5 * P[2];   \
}
/*!
   \brief Returns the coordinates of the two points that form an edge of a voxel
   \param center (float *): center of voxel
   \param dxyz (float *): dimensions of voxel
   \param en (int): edge of voxel (0 -- 11)
   \param P0 (float *): first point forming edge (set by macro)
   \param P1 (float *): second point forming edge (set by macro)
   See labbook NIH-6 pp 201 for a recent diagram of edge and corner numbers
*/
#define SUMA_EDGE_OF_VOXEL(center, dxyz, en, P0, P1){ \
   switch(en) {   \
      case 0:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 0, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 1, P1);   \
         break;   \
      case 1:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 0, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 3, P1);   \
         break;   \
      case 2:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 0, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 4, P1);   \
         break;   \
      case 3:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 1, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 5, P1);   \
         break;   \
      case 4:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 1, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 2, P1);   \
         break;   \
      case 5:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 2, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 6, P1);   \
         break;   \
      case 6:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 2, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 3, P1);   \
         break;   \
      case 7:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 3, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 7, P1);   \
         break;   \
      case 8:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 4, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 7, P1);   \
         break;   \
      case 9:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 4, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 5, P1);   \
         break;   \
      case 10:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 5, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 6, P1);   \
         break;   \
      case 11:  \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 6, P0);   \
         SUMA_CORNER_OF_VOXEL(center, dxyz, 7, P1);   \
         break;   \
      default: \
         P0[0] = center[0]; P0[1] = center[1]; P0[2] = center[2]; \
         P1[0] = center[0]; P1[1] = center[1]; P1[2] = center[2]; \
         SUMA_SL_Err("Bad edge index. Returning Centers of voxel.");\
   }  \
}

/*!
   \brief does a voxel intersect a triangle ? (i.e. one of the edges intersects a triangle)
   \param center (float *) center of voxel
   \param dxyz (float *) dimensions of voxel
   \param vert0 (float *) xyz first vertex of triangle
   \param vert1 (float *) xyz of second vertex of triangle
   \param vert2 (float *) xyz of third vertex of triangle
   \return YUP == intersects
*/
SUMA_Boolean SUMA_isVoxelIntersect_Triangle
      (float *center, float *dxyz,
       float *vert0, float *vert1, float *vert2)
{
   static char FuncName[]={"SUMA_isVoxelIntersect_Triangle"};
   int i = 0;
   float P0[3], P1[3], iP[3];

   SUMA_ENTRY;

   /* loop accross all 12 edges and find out which pierces the triangle */
   for (i=0; i<12; ++i) {
      SUMA_EDGE_OF_VOXEL(center, dxyz, i, P0, P1);
      if (SUMA_MT_isIntersect_Triangle (P0, P1, vert0, vert1, vert2,
                                        iP, NULL, NULL)) {
         #if 0
            if (VoxIntersDbg)
               fprintf(SUMA_STDERR,
                       "%s: intersection detected (dxyz [%f %f %f], edge %d\n",
                           FuncName, dxyz[0], dxyz[1], dxyz[2], i);
         #endif
         /* intersects, make sure intersection is between P0 and P1 */
         if (SUMA_IS_POINT_IN_SEGMENT(iP, P0, P1)) {
            #if 0
            if (VoxIntersDbg) fprintf(SUMA_STDERR, "%s:\n"
                                                "Triangle %.3f, %.3f, %.3f\n"
                                                "         %.3f, %.3f, %.3f\n"
                                                "         %.3f, %.3f, %.3f\n"
                                                "Intersects voxel at:\n"
                                                "         %.3f, %.3f, %.3f\n",
                                                FuncName,
                                                vert0[0], vert0[1], vert0[2],
                                                vert1[0], vert1[1], vert1[2],
                                                vert2[0], vert2[1], vert2[2],
                                                center[0], center[1], center[2]);
            #endif
            SUMA_RETURN(YUP);
         }
      }
   }
   SUMA_RETURN(NOPE);
}

/*
\brief This function is a stripped down version of SUMA_MT_intersect_triangle.
       It is meant to work faster when few triangles are to be tested.

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
{
   static char FuncName[]={"SUMA_MT_isIntersect_Triangle"};
   double edge1[3], edge2[3], tvec[3], pvec[3], qvec[3];
   double det,inv_det, u, v, t;
   double dir[3], dirn, orig[3];
   SUMA_Boolean hit = NOPE;

   SUMA_ENTRY;

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
SUMA_MT_intersect_triangle(float *P0, float *P1,
                           float *NodeList, int N_Node,
                           int *FaceSetList, int N_FaceSet,
                           SUMA_MT_INTERSECT_TRIANGLE *prevMTI,
                           int posonly)

\param   P0 (float *) 3x1 containing XYZ of point 0
\param   P1 (float *) 3x1 containing XYZ of point 1
\param   NodeList (float *) N_Node x 3 vector containing the XYZ of nodes
         making up FaceSetList
\param   N_Node (int) number of nodes in NodeList
\param   FaceSetList (int *) N_FaceSet x 3 with each triplet representing
         a triangle. Triangles are defined by their indices into NodeList
\param   N_FaceSet (int) number of triangles in FaceSetList
\param   PrevMTI (SUMA_MT_INTERSECT_TRIANGLE *) To keep the function from
               reallocating for MTI each time you call it, you can pass
               the previous MTI structure to the next call.
               If the number of facesets is the same as in the previous
               call and PrevMTI is not NULL then MTI is not reallocated for.

               If PrevMTI is not null and the last N_FaceSet was different
               from the current, PrevMTI is freed and a new one is returned.
               This change appears to save about 18% of the function's
               execution time. Be careful not to free PrevMTI without setting
               it to NULL and then send it to SUMA_MT_intersect_triangle.
\param   posonly if 1 then search only in the positive direction. P0 --> P1
                 if 0 then abs min
                 if -1 then only in neg direction
                 Note that MTI->N_poshits is only properly set with
                 posonly == 0 or posonly == 1
\ret   MTI (SUMA_MT_INTERSECT_TRIANGLE *) pointer to structure containing
               isHit (SUMA_Boolean *) N_FaceSet x 1 vector.
               isHit[i] = YUP --> FaceSet i is pierced by ray P0-->P1
      t (float *) signed distance to the plane in which the triangle lies
      u & v(float *) location withing the triangle of the intersection point

\sa Algorithm from:Moller & Trumbore 97
