/*============================================================================*/
/**** Short sorting functions from http://pages.ripco.net/~jgamble/nw.html ****/
/**** This file is mean to be #include-ed into another file!               ****/

#undef  SW
#define SW(i,j) if( ar[i] > ar[j] ){ t=ar[i]; ar[i]=ar[j]; ar[j]=t; }

static void qsort_float5( float *ar )
{
   register float t ;
   SW(0,1); SW(3,4); SW(2,4);
   SW(2,3); SW(1,4); SW(0,3);
   SW(0,2); SW(1,3); SW(1,2);
}

static void qsort_float7( float *ar )
{
   register float t ;
   SW(0,1); SW(3,4); SW(6,7);
   SW(1,2); SW(4,5); SW(7,8);
   SW(0,1); SW(3,4); SW(6,7); SW(2,5);
   SW(0,3); SW(1,4); SW(5,8);
   SW(3,6); SW(4,7); SW(2,5);
   SW(0,3); SW(1,4); SW(5,7); SW(2,6);
   SW(1,3); SW(4,6);
   SW(2,4); SW(5,6); SW(2,3);
}

static void qsort_float9( float *ar )
{
   register float t ;
   SW(0,1); SW(3,4); SW(6,7);
   SW(1,2); SW(4,5); SW(7,8);
   SW(0,1); SW(3,4); SW(6,7); SW(2,5);
   SW(0,3); SW(1,4); SW(5,8);
   SW(3,6); SW(4,7); SW(2,5);
   SW(0,3); SW(1,4); SW(5,7); SW(2,6);
   SW(1,3); SW(4,6);
   SW(2,4); SW(5,6); SW(2,3);
}

static void qsort_float11( float *ar )
{
   register float t ;
   SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9);
   SW(1,3); SW(5,7); SW(0,2); SW(4,6); SW(8,10);
   SW(1,2); SW(5,6); SW(9,10); SW(0,4); SW(3,7);
   SW(1,5); SW(6,10); SW(4,8);
   SW(5,9); SW(2,6); SW(0,4); SW(3,8);
   SW(1,5); SW(6,10); SW(2,3); SW(8,9);
   SW(1,4); SW(7,10); SW(3,5); SW(6,8);
   SW(2,4); SW(7,9); SW(5,6);
   SW(3,4); SW(7,8);
}

static void qsort_float13( float *ar )
{
   register float t ;
   SW(1,7); SW(9,11); SW(3,4); SW(5,8); SW(0,12); SW(2,6);
   SW(0,1); SW(2,3); SW(4,6); SW(8,11); SW(7,12); SW(5,9);
   SW(0,2); SW(3,7); SW(10,11); SW(1,4); SW(6,12);
   SW(7,8); SW(11,12); SW(4,9); SW(6,10);
   SW(3,4); SW(5,6); SW(8,9); SW(10,11); SW(1,7);
   SW(2,6); SW(9,11); SW(1,3); SW(4,7); SW(8,10); SW(0,5);
   SW(2,5); SW(6,8); SW(9,10);
   SW(1,2); SW(3,5); SW(7,8); SW(4,6);
   SW(2,3); SW(4,5); SW(6,7); SW(8,9);
   SW(3,4); SW(5,6);
}

static void qsort_float15( float *ar )
{
   register float t ;
   SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13);
   SW(0,2); SW(4,6); SW(8,10); SW(12,14); SW(1,3); SW(5,7); SW(9,11);
   SW(0,4); SW(8,12); SW(1,5); SW(9,13); SW(2,6); SW(10,14); SW(3,7);
   SW(0,8); SW(1,9); SW(2,10); SW(3,11); SW(4,12); SW(5,13); SW(6,14);
   SW(5,10); SW(6,9); SW(3,12); SW(13,14); SW(7,11); SW(1,2); SW(4,8);
   SW(1,4); SW(7,13); SW(2,8); SW(11,14); SW(5,6); SW(9,10);
   SW(2,4); SW(11,13); SW(3,8); SW(7,12);
   SW(6,8); SW(10,12); SW(3,5); SW(7,9);
   SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12);
   SW(6,7); SW(8,9);
}

static void qsort_float17( float *ar )
{
   register float t ;
   SW(0, 16); SW(0, 8); SW(1, 9); SW(2, 10); SW(3, 11);
   SW(4, 12); SW(5, 13); SW(6, 14); SW(7, 15); SW(8, 16);
   SW(0, 4); SW(1, 5); SW(2, 6); SW(3, 7); SW(8, 12);
   SW(9, 13); SW(10, 14); SW(11, 15); SW(4, 16); SW(4, 8);
   SW(5, 9); SW(6, 10); SW(7, 11); SW(12, 16); SW(0, 2);
   SW(1, 3); SW(4, 6); SW(5, 7); SW(8, 10); SW(9, 11);
   SW(12, 14); SW(13, 15); SW(2, 16); SW(2, 8); SW(3, 9);
   SW(6, 12); SW(7, 13); SW(10, 16); SW(2, 4); SW(3, 5);
   SW(6, 8); SW(7, 9); SW(10, 12); SW(11, 13); SW(14, 16);
   SW(0, 1); SW(2, 3); SW(4, 5); SW(6, 7); SW(8, 9);
   SW(10, 11); SW(12, 13); SW(14, 15); SW(1, 16); SW(1, 8);
   SW(3, 10); SW(5, 12); SW(7, 14); SW(9, 16); SW(1, 4);
   SW(3, 6); SW(5, 8); SW(7, 10); SW(9, 12); SW(11, 14);
   SW(13, 16); SW(1, 2); SW(3, 4); SW(5, 6); SW(7, 8);
   SW(9, 10); SW(11, 12); SW(13, 14); SW(15, 16);
}

static void qsort_float19( float *ar )
{
   register float t ;
   SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13);
   SW(0,2); SW(4,6); SW(8,10); SW(12,14); SW(1,3); SW(5,7); SW(9,11);
   SW(0,4); SW(8,12); SW(1,5); SW(9,13); SW(2,6); SW(10,14); SW(3,7);
   SW(0,8); SW(1,9); SW(2,10); SW(3,11); SW(4,12); SW(5,13); SW(6,14);
   SW(5,10); SW(6,9); SW(3,12); SW(13,14); SW(7,11); SW(1,2); SW(4,8);
   SW(1,4); SW(7,13); SW(2,8); SW(11,14); SW(5,6); SW(9,10);
   SW(2,4); SW(11,13); SW(3,8); SW(7,12);
   SW(6,8); SW(10,12); SW(3,5); SW(7,9);
   SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12);
   SW(6,7); SW(8,9);
}

static void qsort_float2( float *ar )
{
   register float t ;
   SW(0,1) ;
}

static void qsort_float3( float *ar )
{
   register float t ;
   SW(1,2); SW(0,2); SW(0,1);
}

static void qsort_float4( float *ar )
{
   register float t ;
   SW(0,1); SW(2,3);
   SW(0,2); SW(1,3);
   SW(1,2);
}

static void qsort_float6( float *ar )
{
   register float t ;
   SW(1,2); SW(4,5);
   SW(0,2); SW(3,5);
   SW(0,1); SW(3,4); SW(2,5);
   SW(0,3); SW(1,4);
   SW(2,4); SW(1,3);
   SW(2,3);
}

static void qsort_float8( float *ar )
{
   register float t ;
   SW(0,1); SW(2,3); SW(4,5); SW(6,7);
   SW(0,2); SW(1,3); SW(4,6); SW(5,7);
   SW(1,2); SW(5,6); SW(0,4); SW(3,7);
   SW(1,5); SW(2,6);
   SW(1,4); SW(3,6);
   SW(2,4); SW(3,5);
   SW(3,4);
}

static void qsort_float10( float *ar )
{
   register float t ;
   SW(4,9); SW(3,8); SW(2,7); SW(1,6); SW(0,5);
   SW(1,4); SW(6,9); SW(0,3); SW(5,8);
   SW(0,2); SW(3,6); SW(7,9);
   SW(0,1); SW(2,4); SW(5,7); SW(8,9);
   SW(1,2); SW(4,6); SW(7,8); SW(3,5);
   SW(2,5); SW(6,8); SW(1,3); SW(4,7);
   SW(2,3); SW(6,7);
   SW(3,4); SW(5,6);
   SW(4,5);
}

static void qsort_float12( float *ar )
{
   register float t ;
   SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11);
   SW(1,3); SW(5,7); SW(9,11); SW(0,2); SW(4,6); SW(8,10);
   SW(1,2); SW(5,6); SW(9,10); SW(0,4); SW(7,11);
   SW(1,5); SW(6,10); SW(3,7); SW(4,8);
   SW(5,9); SW(2,6); SW(0,4); SW(7,11); SW(3,8);
   SW(1,5); SW(6,10); SW(2,3); SW(8,9);
   SW(1,4); SW(7,10); SW(3,5); SW(6,8);
   SW(2,4); SW(7,9); SW(5,6);
   SW(3,4); SW(7,8);
}

static void qsort_float14( float *ar )
{
   register float t ;
   SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13);
   SW(0,2); SW(4,6); SW(8,10); SW(1,3); SW(5,7); SW(9,11);
   SW(0,4); SW(8,12); SW(1,5); SW(9,13); SW(2,6); SW(3,7);
   SW(0,8); SW(1,9); SW(2,10); SW(3,11); SW(4,12); SW(5,13);
   SW(5,10); SW(6,9); SW(3,12); SW(7,11); SW(1,2); SW(4,8);
   SW(1,4); SW(7,13); SW(2,8); SW(5,6); SW(9,10);
   SW(2,4); SW(11,13); SW(3,8); SW(7,12);
   SW(6,8); SW(10,12); SW(3,5); SW(7,9);
   SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12);
   SW(6,7); SW(8,9);
}

static void qsort_float16( float *ar )
{
   register float t ;
   SW(0,1); SW(2,3); SW(4,5); SW(6,7); SW(8,9); SW(10,11); SW(12,13); SW(14,15);
   SW(0,2); SW(4,6); SW(8,10); SW(12,14); SW(1,3); SW(5,7); SW(9,11); SW(13,15);
   SW(0,4); SW(8,12); SW(1,5); SW(9,13); SW(2,6); SW(10,14); SW(3,7); SW(11,15);
   SW(0,8); SW(1,9); SW(2,10); SW(3,11); SW(4,12); SW(5,13); SW(6,14); SW(7,15);
   SW(5,10); SW(6,9); SW(3,12); SW(13,14); SW(7,11); SW(1,2); SW(4,8);
   SW(1,4); SW(7,13); SW(2,8); SW(11,14); SW(5,6); SW(9,10);
   SW(2,4); SW(11,13); SW(3,8); SW(7,12);
   SW(6,8); SW(10,12); SW(3,5); SW(7,9);
   SW(3,4); SW(5,6); SW(7,8); SW(9,10); SW(11,12);
   SW(6,7); SW(8,9);
}

static void qsort_float18( float *ar )
{
   register float t ;
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

#undef SW
/*============================================================================*/
