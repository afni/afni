/*============================================================================*/
/**** Short sorting functions from http://pages.ripco.net/~jgamble/nw.html ****/
/****......................................................................****/
/**** This file is mean to be #include-ed into another file, after DTYPE   ****/
/**** is #define-d (e.g., #define DTYPE float); the resulting functions    ****/
/**** will have names of the form qsortX_DTYPE, for X=2..21 (inclusive).   ****/
/**** You can #include this file more than once, if DTYPE is changed.      ****/
/**** See mri_percents.c for an example of how this file can be used.      ****/
/*----------------------------------------------------------------------------*/

#ifndef DTYPE
#error "Cannot compile cs_qsort_small.h, since DTYPE is undefined."
#endif

/* macro to swap data points to ensure that ar[i] <= ar[j] */

#undef  SW
#define SW(i,j) if( ar[i] > ar[j] ){ t=ar[i]; ar[i]=ar[j]; ar[j]=t; }

/* macro to combine two lexical items into one (for function names) */

#ifndef TWO_TWO
# define TWO_ONE(x,y) x ## y
# define TWO_TWO(x,y) TWO_ONE(x,y)
#endif

/* define the function names for array lengths 2..21 (plus 25 and 27) */

#define FUNC2 TWO_TWO(qsort2_,DTYPE)
#define FUNC3 TWO_TWO(qsort3_,DTYPE)
#define FUNC4 TWO_TWO(qsort4_,DTYPE)
#define FUNC5 TWO_TWO(qsort5_,DTYPE)
#define FUNC6 TWO_TWO(qsort6_,DTYPE)
#define FUNC7 TWO_TWO(qsort7_,DTYPE)
#define FUNC8 TWO_TWO(qsort8_,DTYPE)
#define FUNC9 TWO_TWO(qsort9_,DTYPE)

#define FUNC10 TWO_TWO(qsort10_,DTYPE)
#define FUNC11 TWO_TWO(qsort11_,DTYPE)
#define FUNC12 TWO_TWO(qsort12_,DTYPE)
#define FUNC13 TWO_TWO(qsort13_,DTYPE)
#define FUNC14 TWO_TWO(qsort14_,DTYPE)
#define FUNC15 TWO_TWO(qsort15_,DTYPE)
#define FUNC16 TWO_TWO(qsort16_,DTYPE)
#define FUNC17 TWO_TWO(qsort17_,DTYPE)
#define FUNC18 TWO_TWO(qsort18_,DTYPE)
#define FUNC19 TWO_TWO(qsort19_,DTYPE)

#define FUNC20 TWO_TWO(qsort20_,DTYPE)  /* 03 Sep 2014 */
#define FUNC21 TWO_TWO(qsort21_,DTYPE)
#define FUNC25 TWO_TWO(qsort25_,DTYPE)  /* for 5x5 2D patches */
#define FUNC27 TWO_TWO(qsort27_,DTYPE)  /* for 3x3 3D patches */

/*----------------------------------------------------------------------------*/

static void FUNC2( DTYPE *ar ){
  register DTYPE t; SW(0,1);     /* that was easy */
}

static void FUNC3( DTYPE *ar ){
  register DTYPE t; SW(1,2); SW(0,2); SW(0,1);
}

static void FUNC4( DTYPE *ar ){
  register DTYPE t; SW(0,1); SW(2,3); SW(0,2); SW(1,3); SW(1,2);
}

static void FUNC5( DTYPE *ar ){
  register DTYPE t;
  SW(0,1); SW(3,4); SW(2,4); SW(2,3); SW(1,4); SW(0,3); SW(0,2); SW(1,3); SW(1,2);
}

static void FUNC6( DTYPE *ar ){
  register DTYPE t;
  SW(1,2); SW(4,5); SW(0,2); SW(3,5); SW(0,1); SW(3,4); SW(2,5);
  SW(0,3); SW(1,4); SW(2,4); SW(1,3); SW(2,3);
}

static void FUNC7( DTYPE *ar ){ /* this function fixed on 02 Sep 2014 (oops) */
  register DTYPE t;
  SW(1,2); SW(0,2); SW(0,1); SW(3,4); SW(5,6); SW(3,5);
  SW(4,6); SW(4,5); SW(0,4); SW(0,3); SW(1,5); SW(2,6);
  SW(2,5); SW(1,3); SW(2,4); SW(2,3);
}

static void FUNC8( DTYPE *ar ){
  register DTYPE t;
  SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(0,2); SW(1,3); SW(4,6); SW(5,7);
  SW(1,2); SW(5,6); SW(0,4); SW(3,7); SW(1,5); SW(2,6);
  SW(1,4); SW(3,6); SW(2,4); SW(3,5); SW(3,4);
}

static void FUNC9( DTYPE *ar ){
  register DTYPE t;
  SW(0,1); SW(3,4); SW(6,7); SW(1,2); SW(4,5); SW(7,8);
  SW(0,1); SW(3,4); SW(6,7); SW(2,5); SW(0,3); SW(1,4); SW(5,8);
  SW(3,6); SW(4,7); SW(2,5); SW(0,3); SW(1,4); SW(5,7); SW(2,6);
  SW(1,3); SW(4,6); SW(2,4); SW(5,6); SW(2,3);
}

static void FUNC10( DTYPE *ar ){
  register DTYPE t;
  SW(4,9); SW(3,8); SW(2,7); SW(1,6); SW(0,5);
  SW(1,4); SW(6,9); SW(0,3); SW(5,8); SW(0,2); SW(3,6); SW(7,9);
  SW(0,1); SW(2,4); SW(5,7); SW(8,9); SW(1,2); SW(4,6); SW(7,8); SW(3,5);
  SW(2,5); SW(6,8); SW(1,3); SW(4,7); SW(2,3); SW(6,7);
  SW(3,4); SW(5,6); SW(4,5);
}

static void FUNC11( DTYPE *ar ){
  register DTYPE t;
  SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9);
  SW(1,3); SW(5,7); SW(0,2); SW(4,6); SW(8,10);
  SW(1,2); SW(5,6); SW(9,10); SW(0,4); SW(3,7);
  SW(1,5); SW(6,10); SW(4,8); SW(5,9); SW(2,6); SW(0,4); SW(3,8);
  SW(1,5); SW(6,10); SW(2,3); SW(8,9); SW(1,4); SW(7,10); SW(3,5); SW(6,8);
  SW(2,4); SW(7,9); SW(5,6); SW(3,4); SW(7,8);
}

static void FUNC12( DTYPE *ar ){
  register DTYPE t;
  SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11);
  SW(1,3); SW(5,7); SW(9,11); SW(0,2); SW(4,6); SW(8,10);
  SW(1,2); SW(5,6); SW(9,10); SW(0,4); SW(7,11);
  SW(1,5); SW(6,10); SW(3,7); SW(4,8);
  SW(5,9); SW(2,6); SW(0,4); SW(7,11); SW(3,8);
  SW(1,5); SW(6,10); SW(2,3); SW(8,9); SW(1,4); SW(7,10); SW(3,5); SW(6,8);
  SW(2,4); SW(7,9); SW(5,6); SW(3,4); SW(7,8);
}

static void FUNC13( DTYPE *ar ){
  register DTYPE t;
  SW(1,7); SW(9,11); SW(3,4); SW(5,8); SW(0,12); SW(2,6);
  SW(0,1); SW(2,3); SW(4,6); SW(8,11); SW(7,12); SW(5,9);
  SW(0,2); SW(3,7); SW(10,11); SW(1,4); SW(6,12);
  SW(7,8); SW(11,12); SW(4,9); SW(6,10);
  SW(3,4); SW(5,6); SW(8,9); SW(10,11); SW(1,7);
  SW(2,6); SW(9,11); SW(1,3); SW(4,7); SW(8,10); SW(0,5);
  SW(2,5); SW(6,8); SW(9,10); SW(1,2); SW(3,5); SW(7,8); SW(4,6);
  SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(3,4); SW(5,6);
}

static void FUNC14( DTYPE *ar ){
  register DTYPE t;
  SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13);
  SW(0,2); SW(4,6); SW(8,10); SW(1,3); SW(5,7); SW(9,11);
  SW(0,4); SW(8,12); SW(1,5); SW(9,13); SW(2,6); SW(3,7);
  SW(0,8); SW(1,9); SW(2,10); SW(3,11); SW(4,12); SW(5,13);
  SW(5,10); SW(6,9); SW(3,12); SW(7,11); SW(1,2); SW(4,8);
  SW(1,4); SW(7,13); SW(2,8); SW(5,6); SW(9,10);
  SW(2,4); SW(11,13); SW(3,8); SW(7,12); SW(6,8); SW(10,12); SW(3,5); SW(7,9);
  SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12); SW(6,7); SW(8,9);
}

static void FUNC15( DTYPE *ar ){
  register DTYPE t;
  SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13);
  SW(0,2); SW(4,6); SW(8,10); SW(12,14); SW(1,3); SW(5,7); SW(9,11);
  SW(0,4); SW(8,12); SW(1,5); SW(9,13); SW(2,6); SW(10,14); SW(3,7);
  SW(0,8); SW(1,9); SW(2,10); SW(3,11); SW(4,12); SW(5,13); SW(6,14);
  SW(5,10); SW(6,9); SW(3,12); SW(13,14); SW(7,11); SW(1,2); SW(4,8);
  SW(1,4); SW(7,13); SW(2,8); SW(11,14); SW(5,6); SW(9,10);
  SW(2,4); SW(11,13); SW(3,8); SW(7,12); SW(6,8); SW(10,12); SW(3,5); SW(7,9);
  SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12); SW(6,7); SW(8,9);
}

static void FUNC16( DTYPE *ar ){
  register DTYPE t;
  SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13); SW(14,15);
  SW(0,2); SW(4,6); SW(8,10); SW(12,14); SW(1,3); SW(5,7); SW(9,11); SW(13,15);
  SW(0,4); SW(8,12); SW(1,5); SW(9,13); SW(2,6); SW(10,14); SW(3,7); SW(11,15);
  SW(0,8); SW(1,9); SW(2,10); SW(3,11); SW(4,12); SW(5,13); SW(6,14); SW(7,15);
  SW(5,10); SW(6,9); SW(3,12); SW(13,14); SW(7,11); SW(1,2); SW(4,8);
  SW(1,4); SW(7,13); SW(2,8); SW(11,14); SW(5,6); SW(9,10);
  SW(2,4); SW(11,13); SW(3,8); SW(7,12); SW(6,8); SW(10,12); SW(3,5); SW(7,9);
  SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12); SW(6,7); SW(8,9);
}

static void FUNC17( DTYPE *ar ){
  register DTYPE t;
  SW(0,16); SW(0,8); SW(1,9); SW(2,10); SW(3,11);
  SW(4,12); SW(5,13); SW(6,14); SW(7,15); SW(8,16);
  SW(0,4); SW(1,5); SW(2,6); SW(3,7); SW(8,12);
  SW(9,13); SW(10,14); SW(11,15); SW(4,16); SW(4,8);
  SW(5,9); SW(6,10); SW(7,11); SW(12,16); SW(0,2);
  SW(1,3); SW(4,6); SW(5,7); SW(8,10); SW(9,11);
  SW(12,14); SW(13,15); SW(2,16); SW(2,8); SW(3,9);
  SW(6,12); SW(7,13); SW(10,16); SW(2,4); SW(3,5);
  SW(6,8); SW(7,9); SW(10,12); SW(11,13); SW(14,16);
  SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9);
  SW(10,11); SW(12,13); SW(14,15); SW(1,16); SW(1,8);
  SW(3,10); SW(5,12); SW(7,14); SW(9,16); SW(1,4);
  SW(3,6); SW(5,8); SW(7,10); SW(9,12); SW(11,14);
  SW(13,16); SW(1,2); SW(3,4); SW(5,6); SW(7,8);
  SW(9,10); SW(11,12); SW(13,14); SW(15,16);
}

static void FUNC18( DTYPE *ar ){
  register DTYPE t;
  SW(0,16); SW(1,17); SW(2,10); SW(3,11); SW(4,12); SW(5,13); SW(6,14); SW(7,15);
  SW(0,8); SW(1,9); SW(2,6); SW(3,7); SW(10,14); SW(11,15);
  SW(8,16); SW(9,17); SW(0,4); SW(1,5); SW(6,10); SW(7,11);
  SW(8,12); SW(9,13); SW(4,16); SW(5,17); SW(0,2); SW(1,3);
  SW(4,8); SW(5,9); SW(12,16); SW(13,17); SW(0,1);
  SW(4,6); SW(5,7); SW(8,10); SW(9,11); SW(12,14); SW(13,15); SW(2,16); SW(3,17);
  SW(2,8); SW(3,9); SW(6,12); SW(7,13); SW(10,16); SW(11,17);
  SW(2,4); SW(3,5); SW(6,8); SW(7,9); SW(10,12); SW(11,13); SW(14,16); SW(15,17);
  SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13); SW(14,15); SW(16,17);
  SW(1,16); SW(3,10); SW(5,12); SW(7,14);
  SW(1,8); SW(9,16); SW(3,6); SW(7,10); SW(11,14);
  SW(1,4); SW(5,8); SW(9,12); SW(13,16);
  SW(1,2); SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12); SW(13,14); SW(15,16);
}

static void FUNC19( DTYPE *ar ){
  register DTYPE t;
  SW(0,16); SW(1,17); SW(2,18); SW(3,11); SW(4,12); SW(5,13); SW(6,14); SW(7,15);
  SW(0,8); SW(1,9); SW(2,10); SW(3,7); SW(11,15);
  SW(8,16); SW(9,17); SW(10,18); SW(0,4); SW(1,5); SW(2,6); SW(7,11);
  SW(8,12); SW(9,13); SW(10,14); SW(4,16); SW(5,17); SW(6,18); SW(0,2); SW(1,3);
  SW(4,8); SW(5,9); SW(6,10); SW(12,16); SW(13,17); SW(14,18); SW(0,1);
  SW(4,6); SW(5,7); SW(8,10); SW(9,11); SW(12,14); SW(13,15); SW(16,18); SW(3,17);
  SW(2,16); SW(3,9); SW(6,12); SW(7,13); SW(11,17);
  SW(2,8); SW(10,16); SW(3,5); SW(7,9); SW(11,13); SW(15,17);
  SW(2,4); SW(6,8); SW(10,12); SW(14,16);
  SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13); SW(14,15); SW(16,17);
  SW(1,16); SW(3,18); SW(5,12); SW(7,14); SW(1,8); SW(3,10); SW(9,16); SW(11,18);
  SW(1,4); SW(3,6); SW(5,8); SW(7,10); SW(9,12); SW(11,14); SW(13,16); SW(15,18);
  SW(1,2); SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12); SW(13,14); SW(15,16);
  SW(17,18);
}

static void FUNC20( DTYPE *ar ){  /* 03 Sep 2014 */
  register DTYPE t ;
  SW(0,16); SW(1,17); SW(2,18); SW(3,19); SW(0,8); SW(1,9); SW(2,10); 
  SW(3,11); SW(4,12); SW(5,13); SW(6,14); SW(7,15); SW(8,16); SW(9,17); 
  SW(10,18); SW(11,19); SW(0,4); SW(1,5); SW(2,6); SW(3,7); SW(8,12); 
  SW(9,13); SW(10,14); SW(11,15); SW(4,16); SW(5,17); SW(6,18); SW(7,19); 
  SW(4,8); SW(5,9); SW(6,10); SW(7,11); SW(12,16); SW(13,17); SW(14,18); 
  SW(15,19); SW(0,2); SW(1,3); SW(4,6); SW(5,7); SW(8,10); SW(9,11); 
  SW(12,14); SW(13,15); SW(16,18); SW(17,19); SW(2,16); SW(3,17); SW(2,8); 
  SW(3,9); SW(6,12); SW(7,13); SW(10,16); SW(11,17); SW(2,4); SW(3,5); 
  SW(6,8); SW(7,9); SW(10,12); SW(11,13); SW(14,16); SW(15,17); SW(0,1); 
  SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13); SW(14,15); 
  SW(16,17); SW(18,19); SW(1,16); SW(3,18); SW(1,8); SW(3,10); SW(5,12); 
  SW(7,14); SW(9,16); SW(11,18); SW(1,4); SW(3,6); SW(5,8); SW(7,10); 
  SW(9,12); SW(11,14); SW(13,16); SW(15,18); SW(1,2); SW(3,4); SW(5,6); 
  SW(7,8); SW(9,10); SW(11,12); SW(13,14); SW(15,16); SW(17,18); 
}

static void FUNC21( DTYPE *ar ){  /* 03 Sep 2014 */
  register DTYPE t ;
  SW(0,16); SW(1,17); SW(2,18); SW(3,19); SW(4,20); SW(0,8); SW(1,9); 
  SW(2,10); SW(3,11); SW(4,12); SW(5,13); SW(6,14); SW(7,15); SW(8,16); 
  SW(9,17); SW(10,18); SW(11,19); SW(12,20); SW(0,4); SW(1,5); SW(2,6); 
  SW(3,7); SW(8,12); SW(9,13); SW(10,14); SW(11,15); SW(16,20); SW(4,16); 
  SW(5,17); SW(6,18); SW(7,19); SW(4,8); SW(5,9); SW(6,10); SW(7,11); 
  SW(12,16); SW(13,17); SW(14,18); SW(15,19); SW(0,2); SW(1,3); SW(4,6); 
  SW(5,7); SW(8,10); SW(9,11); SW(12,14); SW(13,15); SW(16,18); SW(17,19); 
  SW(2,16); SW(3,17); SW(6,20); SW(2,8); SW(3,9); SW(6,12); SW(7,13); 
  SW(10,16); SW(11,17); SW(14,20); SW(2,4); SW(3,5); SW(6,8); SW(7,9); 
  SW(10,12); SW(11,13); SW(14,16); SW(15,17); SW(18,20); SW(0,1); SW(2,3); 
  SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13); SW(14,15); SW(16,17); 
  SW(18,19); SW(1,16); SW(3,18); SW(5,20); SW(1,8); SW(3,10); SW(5,12); 
  SW(7,14); SW(9,16); SW(11,18); SW(13,20); SW(1,4); SW(3,6); SW(5,8); 
  SW(7,10); SW(9,12); SW(11,14); SW(13,16); SW(15,18); SW(17,20); SW(1,2); 
  SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12); SW(13,14); SW(15,16); 
  SW(17,18); SW(19,20); 
}

static void FUNC25( DTYPE *ar ){  /* 03 Sep 2014 */
  register DTYPE t ;
  SW(0,16); SW(1,17); SW(2,18); SW(3,19); SW(4,20); SW(5,21); SW(6,22); 
  SW(7,23); SW(8,24); SW(0,8); SW(1,9); SW(2,10); SW(3,11); SW(4,12); 
  SW(5,13); SW(6,14); SW(7,15); SW(16,24); SW(8,16); SW(9,17); SW(10,18); 
  SW(11,19); SW(12,20); SW(13,21); SW(14,22); SW(15,23); SW(0,4); SW(1,5); 
  SW(2,6); SW(3,7); SW(8,12); SW(9,13); SW(10,14); SW(11,15); SW(16,20); 
  SW(17,21); SW(18,22); SW(19,23); SW(4,16); SW(5,17); SW(6,18); SW(7,19); 
  SW(12,24); SW(4,8); SW(5,9); SW(6,10); SW(7,11); SW(12,16); SW(13,17); 
  SW(14,18); SW(15,19); SW(20,24); SW(0,2); SW(1,3); SW(4,6); SW(5,7); 
  SW(8,10); SW(9,11); SW(12,14); SW(13,15); SW(16,18); SW(17,19); SW(20,22); 
  SW(21,23); SW(2,16); SW(3,17); SW(6,20); SW(7,21); SW(10,24); SW(2,8); 
  SW(3,9); SW(6,12); SW(7,13); SW(10,16); SW(11,17); SW(14,20); SW(15,21); 
  SW(18,24); SW(2,4); SW(3,5); SW(6,8); SW(7,9); SW(10,12); SW(11,13); 
  SW(14,16); SW(15,17); SW(18,20); SW(19,21); SW(22,24); SW(0,1); SW(2,3); 
  SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13); SW(14,15); SW(16,17); 
  SW(18,19); SW(20,21); SW(22,23); SW(1,16); SW(3,18); SW(5,20); SW(7,22); 
  SW(9,24); SW(1,8); SW(3,10); SW(5,12); SW(7,14); SW(9,16); SW(11,18); 
  SW(13,20); SW(15,22); SW(17,24); SW(1,4); SW(3,6); SW(5,8); SW(7,10); 
  SW(9,12); SW(11,14); SW(13,16); SW(15,18); SW(17,20); SW(19,22); SW(21,24); 
  SW(1,2); SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12); SW(13,14); 
  SW(15,16); SW(17,18); SW(19,20); SW(21,22); SW(23,24); 
}

static void FUNC27( DTYPE *ar ){  /* 03 Sep 2014 */
  register DTYPE t ;
  SW(0,16); SW(1,17); SW(2,18); SW(3,19); SW(4,20); SW(5,21); SW(6,22); 
  SW(7,23); SW(8,24); SW(9,25); SW(10,26); SW(0,8); SW(1,9); SW(2,10); 
  SW(3,11); SW(4,12); SW(5,13); SW(6,14); SW(7,15); SW(16,24); SW(17,25); 
  SW(18,26); SW(8,16); SW(9,17); SW(10,18); SW(11,19); SW(12,20); SW(13,21); 
  SW(14,22); SW(15,23); SW(0,4); SW(1,5); SW(2,6); SW(3,7); SW(8,12); 
  SW(9,13); SW(10,14); SW(11,15); SW(16,20); SW(17,21); SW(18,22); SW(19,23); 
  SW(4,16); SW(5,17); SW(6,18); SW(7,19); SW(12,24); SW(13,25); SW(14,26); 
  SW(4,8); SW(5,9); SW(6,10); SW(7,11); SW(12,16); SW(13,17); SW(14,18); 
  SW(15,19); SW(20,24); SW(21,25); SW(22,26); SW(0,2); SW(1,3); SW(4,6); 
  SW(5,7); SW(8,10); SW(9,11); SW(12,14); SW(13,15); SW(16,18); SW(17,19); 
  SW(20,22); SW(21,23); SW(24,26); SW(2,16); SW(3,17); SW(6,20); SW(7,21); 
  SW(10,24); SW(11,25); SW(2,8); SW(3,9); SW(6,12); SW(7,13); SW(10,16); 
  SW(11,17); SW(14,20); SW(15,21); SW(18,24); SW(19,25); SW(2,4); SW(3,5); 
  SW(6,8); SW(7,9); SW(10,12); SW(11,13); SW(14,16); SW(15,17); SW(18,20); 
  SW(19,21); SW(22,24); SW(23,25); SW(0,1); SW(2,3); SW(4,5); SW(6,7); 
  SW(8,9); SW(10,11); SW(12,13); SW(14,15); SW(16,17); SW(18,19); SW(20,21); 
  SW(22,23); SW(24,25); SW(1,16); SW(3,18); SW(5,20); SW(7,22); SW(9,24); 
  SW(11,26); SW(1,8); SW(3,10); SW(5,12); SW(7,14); SW(9,16); SW(11,18); 
  SW(13,20); SW(15,22); SW(17,24); SW(19,26); SW(1,4); SW(3,6); SW(5,8); 
  SW(7,10); SW(9,12); SW(11,14); SW(13,16); SW(15,18); SW(17,20); SW(19,22); 
  SW(21,24); SW(23,26); SW(1,2); SW(3,4); SW(5,6); SW(7,8); SW(9,10); 
  SW(11,12); SW(13,14); SW(15,16); SW(17,18); SW(19,20); SW(21,22); SW(23,24); 
  SW(25,26);
}

/*----------------------------------------------------------------------------*/
#undef SW
#undef FUNC2
#undef FUNC3
#undef FUNC4
#undef FUNC5
#undef FUNC6
#undef FUNC7
#undef FUNC8
#undef FUNC9
#undef FUNC10
#undef FUNC11
#undef FUNC12
#undef FUNC13
#undef FUNC14
#undef FUNC15
#undef FUNC16
#undef FUNC17
#undef FUNC18
#undef FUNC19
#undef FUNC20
#undef FUNC21
#undef FUNC25
#undef FUNC27
/*============================================================================*/
