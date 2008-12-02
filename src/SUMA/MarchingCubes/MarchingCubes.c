//------------------------------------------------
// MarchingCubes
//------------------------------------------------
//
// MarchingCubes Algorithm
// Version 0.2 - 12/08/2002
//
// Thomas Lewiner thomas.lewiner@polytechnique.org
// Math Dept, PUC-Rio
//
//
// Translated to C by Ziad S. Saad November 30/04
//________________________________________________


#include <stdio.h>
#include <math.h>
#include <time.h>
#include <memory.h>
#include <float.h>
#include <stdlib.h>
#include <math.h>
#include "MarchingCubes.h"
#include "LookUpTable.h"

// step size of the arrays of vertices and triangles
#define ALLOC_SIZE 65536

static int debug;
void set_suma_debug(int dbg)
{
   debug = dbg;
   return;
}
//_____________________________________________________________________________
// print cube for debug
void print_cube(MCB *mcb) { printf( "\t%f %f %f %f %f %f %f %f\n", mcb->cube[0], mcb->cube[1], mcb->cube[2], mcb->cube[3], mcb->cube[4], mcb->cube[5], mcb->cube[6], mcb->cube[7]) ; }
//_____________________________________________________________________________

void set_resolution( MCB *mcb, int size_x,  int size_y,  int size_z ) 
{ 
   mcb->size_x = size_x ;  mcb->size_y = size_y ;  mcb->size_z = size_z ; 
   return;
}
void set_method    ( MCB *mcb, int originalMC ) {
    /* originalMC = false is the default */ 
    mcb->originalMC = originalMC ; 
    return;
}

  // Data access
float get_data  (  MCB *mcb, int i,  int j,  int k )  { 
   return (mcb->data[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y]) ; 
}
void  set_data  (  MCB *mcb, float val,  int i,  int j,  int k ) {
  (mcb->data[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y] = val) ; 
}
int   get_x_vert(  MCB *mcb , int i,  int j,  int k )  { return (mcb->x_verts[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y] ); }
int   get_y_vert(  MCB *mcb , int i,  int j,  int k )  { return (mcb->y_verts[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y] ); }
int   get_z_vert(  MCB *mcb , int i,  int j,  int k )  { return (mcb->z_verts[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y] ); }
void  set_x_vert(  MCB *mcb , int val,  int i,  int j,  int k ) { (mcb->x_verts[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y] = val ); }
void  set_y_vert(  MCB *mcb , int val,  int i,  int j,  int k ) { (mcb->y_verts[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y] = val ); }
void  set_z_vert(  MCB *mcb , int val,  int i,  int j,  int k ) { (mcb->z_verts[ i + j*mcb->size_x + k*mcb->size_x*mcb->size_y] = val ); }


//_____________________________________________________________________________
// Constructor
MCB *MarchingCubes( int size_x, int size_y , int size_z  )
{
// defaults are -1 for all size_ -----------------------------------------------------------------------------
  MCB *mcb=NULL;
  mcb = (MCB *)malloc(sizeof(MCB));
  mcb->originalMC = false;
  mcb->size_x = size_x;
  mcb->size_y = size_y;
  mcb->size_z = size_z;
  mcb->data    =  (float*)NULL;
  mcb->x_verts = ( int *)NULL;
  mcb->y_verts = ( int *)NULL;
  mcb->z_verts =  ( int *)NULL;
  mcb->nverts  =  0;
  mcb->ntrigs  =  0;
  mcb->Nverts  =  0;
  mcb->Ntrigs  =  0;
  mcb->vertices = ( Vertex *)NULL;
  mcb->triangles =(Triangle*)NULL;
  mcb->_case = 0;             /* Was uninitialized and causing weird crashes on linux! ZSS: Oct 06 */
  return(mcb);
}
//_____________________________________________________________________________



//_____________________________________________________________________________
// Destructor
void FreeMarchingCubes(MCB *mcb)
//-----------------------------------------------------------------------------
{
  clean_all(mcb) ;
  return;
}
//_____________________________________________________________________________



//_____________________________________________________________________________
// main algorithm
void run(MCB *mcb)
//-----------------------------------------------------------------------------
{
   int p;
   if (debug) printf("Marching Cubes begin: cpu %ld\n", clock() ) ;

   compute_intersection_points( mcb) ;

   for( mcb->k = 0 ; mcb->k < mcb->size_z-1 ; mcb->k++ )
   for( mcb->j = 0 ; mcb->j < mcb->size_y-1 ; mcb->j++ )
   for( mcb->i = 0 ; mcb->i < mcb->size_x-1 ; mcb->i++ )
   {
    mcb->lut_entry = 0 ;
    for(  p = 0 ; p < 8 ; ++p )
    {
      mcb->cube[p] = get_data( mcb, mcb->i+((p^(p>>1))&1), mcb->j+((p>>1)&1), mcb->k+((p>>2)&1) ) ;
      if( fabs( mcb->cube[p] ) < FLT_EPSILON ) mcb->cube[p] = FLT_EPSILON ;
      if( mcb->cube[p] > 0 ) mcb->lut_entry += 1 << p ;
    }
   /*
    if( ( mcb->cube[0] = get_data( mcb, mcb->i , mcb->j , mcb->k ) ) > 0 ) mcb->lut_entry +=   1 ;
    if( ( mcb->cube[1] = get_data(mcb, mcb->i+1, mcb->j , mcb->k ) ) > 0 ) mcb->lut_entry +=   2 ;
    if( ( mcb->cube[2] = get_data(mcb, mcb->i+1,mcb->j+1, mcb->k ) ) > 0 ) mcb->lut_entry +=   4 ;
    if( ( mcb->cube[3] = get_data(mcb,  mcb->i ,mcb->j+1, mcb->k ) ) > 0 ) mcb->lut_entry +=   8 ;
    if( ( mcb->cube[4] = get_data(mcb,  mcb->i , mcb->j ,mcb->k+1) ) > 0 ) mcb->lut_entry +=  16 ;
    if( ( mcb->cube[5] = get_data(mcb, mcb->i+1, mcb->j ,mcb->k+1) ) > 0 ) mcb->lut_entry +=  32 ;
    if( ( mcb->cube[6] = get_data(mcb, mcb->i+1,mcb->j+1,mcb->k+1) ) > 0 ) mcb->lut_entry +=  64 ;
    if( ( mcb->cube[7] = get_data(mcb,  mcb->i ,mcb->j+1,mcb->k+1) ) > 0 ) mcb->lut_entry += 128 ;
   */
    process_cube( mcb) ;
   }

   if (debug) { 
      printf("Marching Cubes end: cpu %ld\n", clock() ) ;
      for( mcb->i = 0 ; mcb->i < 15 ; mcb->i++ )
      {
       printf("  %7d cases %d\n", mcb->N[mcb->i], mcb->i ) ;
      }
   }
}
//_____________________________________________________________________________



//_____________________________________________________________________________
// init temporary structures (must set sizes before call)
void init_temps(MCB *mcb)
//-----------------------------------------------------------------------------
{
  mcb->data = (float*)calloc(mcb->size_x * mcb->size_y * mcb->size_z, sizeof(float)) ;
  mcb->x_verts = (int*)calloc(mcb->size_x * mcb->size_y * mcb->size_z, sizeof(int)) ;
  mcb->y_verts = (int*)calloc(mcb->size_x * mcb->size_y * mcb->size_z, sizeof(int));
  mcb->z_verts = (int*)calloc(mcb->size_x * mcb->size_y * mcb->size_z, sizeof(int)) ;

  memset( mcb->x_verts, -1, mcb->size_x * mcb->size_y * mcb->size_z * sizeof( int ) ) ;
  memset( mcb->y_verts, -1, mcb->size_x * mcb->size_y * mcb->size_z * sizeof( int ) ) ;
  memset( mcb->z_verts, -1, mcb->size_x * mcb->size_y * mcb->size_z * sizeof( int ) ) ;

  memset( mcb->N, 0, 15 * sizeof(int) ) ;
}
//_____________________________________________________________________________



//_____________________________________________________________________________
// init all structures (must set sizes before call)
void init_all (MCB *mcb)
//-----------------------------------------------------------------------------
{
  init_temps(mcb) ;

  mcb->nverts = mcb->ntrigs = 0 ;
  mcb->Nverts = mcb->Ntrigs = ALLOC_SIZE ;
  mcb->vertices  = (Vertex*)calloc(mcb->Nverts, sizeof(Vertex)) ;
  mcb->triangles = (Triangle*)calloc(mcb->Ntrigs, sizeof(Triangle));
}
//_____________________________________________________________________________



//_____________________________________________________________________________
// clean temporary structures
void clean_temps(MCB *mcb)
//-----------------------------------------------------------------------------
{
  free(mcb->data); 
  free(mcb->x_verts);
  free(mcb->y_verts);
  free(mcb->z_verts);

  mcb->data     = (float*)NULL ;
  mcb->x_verts  = (int*)NULL ;
  mcb->y_verts  = (int*)NULL ;
  mcb->z_verts  = (int*)NULL ;
}
//_____________________________________________________________________________



//_____________________________________________________________________________
// clean all structures
void clean_all(MCB *mcb)
//-----------------------------------------------------------------------------
{
  clean_temps(mcb) ;
  free(mcb->vertices)  ;
  free(mcb->triangles) ;
  mcb->vertices  = (Vertex   *)NULL ;
  mcb->triangles = (Triangle *)NULL ;
  mcb->nverts = mcb->ntrigs = 0 ;
  mcb->Nverts = mcb->Ntrigs = 0 ;

  mcb->size_x = mcb->size_y = mcb->size_z = -1 ;
}
//_____________________________________________________________________________



//_____________________________________________________________________________
//_____________________________________________________________________________


//_____________________________________________________________________________
// Compute the intersection points
void compute_intersection_points(MCB *mcb )
//-----------------------------------------------------------------------------
{
  for( mcb->k = 0 ; mcb->k < mcb->size_z ; mcb->k++ )
  for( mcb->j = 0 ; mcb->j < mcb->size_y ; mcb->j++ )
  for( mcb->i = 0 ; mcb->i < mcb->size_x ; mcb->i++ )
  {
    mcb->cube[0] = get_data( mcb, mcb->i, mcb->j, mcb->k ) ;
    if( mcb->i < mcb->size_x - 1 ) mcb->cube[1] = get_data(mcb, mcb->i+1, mcb->j , mcb->k ) ;
    else                 mcb->cube[1] = mcb->cube[0] ;

    if( mcb->j < mcb->size_y - 1 ) mcb->cube[3] = get_data( mcb, mcb->i ,mcb->j+1, mcb->k ) ;
    else                 mcb->cube[3] = mcb->cube[0] ;

    if( mcb->k < mcb->size_z - 1 ) mcb->cube[4] = get_data( mcb, mcb->i , mcb->j ,mcb->k+1) ;
    else                 mcb->cube[4] = mcb->cube[0] ;

    if( fabs( mcb->cube[0] ) < FLT_EPSILON ) mcb->cube[0] = FLT_EPSILON ;
    if( fabs( mcb->cube[1] ) < FLT_EPSILON ) mcb->cube[1] = FLT_EPSILON ;
    if( fabs( mcb->cube[3] ) < FLT_EPSILON ) mcb->cube[3] = FLT_EPSILON ;
    if( fabs( mcb->cube[4] ) < FLT_EPSILON ) mcb->cube[4] = FLT_EPSILON ;

    if( mcb->cube[0] < 0 )
    {
      if( mcb->cube[1] > 0 ) set_x_vert( mcb, add_x_vertex( mcb), mcb->i,mcb->j,mcb->k ) ;
      if( mcb->cube[3] > 0 ) set_y_vert( mcb, add_y_vertex( mcb), mcb->i,mcb->j,mcb->k ) ;
      if( mcb->cube[4] > 0 ) set_z_vert( mcb, add_z_vertex( mcb), mcb->i,mcb->j,mcb->k ) ;
    }
    else
    {
      if( mcb->cube[1] < 0 ) set_x_vert( mcb, add_x_vertex( mcb), mcb->i,mcb->j,mcb->k ) ;
      if( mcb->cube[3] < 0 ) set_y_vert( mcb, add_y_vertex( mcb), mcb->i,mcb->j,mcb->k ) ;
      if( mcb->cube[4] < 0 ) set_z_vert( mcb, add_z_vertex( mcb), mcb->i,mcb->j,mcb->k ) ;
    }
  }
}
//_____________________________________________________________________________





//_____________________________________________________________________________
// Test a face
// if face>0 return true if the face contains a part of the surface
int test_face( MCB *mcb, schar face )
//-----------------------------------------------------------------------------
{
  float A,B,C,D ;

  switch( face )
  {
  case -1 : case 1 :  A = mcb->cube[0] ;  B = mcb->cube[4] ;  C = mcb->cube[5] ;  D = mcb->cube[1] ;  break ;
  case -2 : case 2 :  A = mcb->cube[1] ;  B = mcb->cube[5] ;  C = mcb->cube[6] ;  D = mcb->cube[2] ;  break ;
  case -3 : case 3 :  A = mcb->cube[2] ;  B = mcb->cube[6] ;  C = mcb->cube[7] ;  D = mcb->cube[3] ;  break ;
  case -4 : case 4 :  A = mcb->cube[3] ;  B = mcb->cube[7] ;  C = mcb->cube[4] ;  D = mcb->cube[0] ;  break ;
  case -5 : case 5 :  A = mcb->cube[0] ;  B = mcb->cube[3] ;  C = mcb->cube[2] ;  D = mcb->cube[1] ;  break ;
  case -6 : case 6 :  A = mcb->cube[4] ;  B = mcb->cube[7] ;  C = mcb->cube[6] ;  D = mcb->cube[5] ;  break ;
  default : printf( "Invalid face code %d\n", face ) ;  print_cube(mcb) ;  A = B = C = D = 0 ;
  };

  return (face * A * ( A*C - B*D ) >= 0)  ;  // face and A invert signs
}
//_____________________________________________________________________________





//_____________________________________________________________________________
// Test the interior of a cube
// if s == 7, return true  if the interior is empty
// if s ==-7, return false if the interior is empty
int test_interior( MCB *mcb, schar s )
//-----------------------------------------------------------------------------
{
  float t, At=0, Bt=0, Ct=0, Dt=0, a, b ;
  char  test =  0 ;
  char  edge = -1 ; // reference edge of the triangulation

  switch( mcb->_case )
  {
  case  4 :
  case 10 :
    a = ( mcb->cube[4] - mcb->cube[0] ) * ( mcb->cube[6] - mcb->cube[2] ) - ( mcb->cube[7] - mcb->cube[3] ) * ( mcb->cube[5] - mcb->cube[1] ) ;
    b =  mcb->cube[2] * ( mcb->cube[4] - mcb->cube[0] ) + mcb->cube[0] * ( mcb->cube[6] - mcb->cube[2] )
             - mcb->cube[1] * ( mcb->cube[7] - mcb->cube[3] ) - mcb->cube[3] * ( mcb->cube[5] - mcb->cube[1] ) ;
    t = - b / (2*a) ;
    if( t<0 || t>1 ) return s>0 ;

    At = mcb->cube[0] + ( mcb->cube[4] - mcb->cube[0] ) * t ;
    Bt = mcb->cube[3] + ( mcb->cube[7] - mcb->cube[3] ) * t ;
    Ct = mcb->cube[2] + ( mcb->cube[6] - mcb->cube[2] ) * t ;
    Dt = mcb->cube[1] + ( mcb->cube[5] - mcb->cube[1] ) * t ;
    break ;

  case  6 :
  case  7 :
  case 12 :
  case 13 :
    switch( mcb->_case )
    {
    case  6 : edge = tiling6 [mcb->config][15] ; break ;
    case  7 : edge = tiling7 [mcb->config][13] ; break ;
    case 12 : edge = tiling12[mcb->config][14] ; break ;
    case 13 : edge = tiling13_5_1[mcb->config][mcb->subconfig][2] ; break ;
    }
    switch( edge )
    {
    case  0 :
      t  = mcb->cube[0] / ( mcb->cube[0] - mcb->cube[1] ) ;
      At = 0 ;
      Bt = mcb->cube[3] + ( mcb->cube[2] - mcb->cube[3] ) * t ;
      Ct = mcb->cube[7] + ( mcb->cube[6] - mcb->cube[7] ) * t ;
      Dt = mcb->cube[4] + ( mcb->cube[5] - mcb->cube[4] ) * t ;
      break ;
    case  1 :
      t  = mcb->cube[1] / ( mcb->cube[1] - mcb->cube[2] ) ;
      At = 0 ;
      Bt = mcb->cube[0] + ( mcb->cube[3] - mcb->cube[0] ) * t ;
      Ct = mcb->cube[4] + ( mcb->cube[7] - mcb->cube[4] ) * t ;
      Dt = mcb->cube[5] + ( mcb->cube[6] - mcb->cube[5] ) * t ;
      break ;
    case  2 :
      t  = mcb->cube[2] / ( mcb->cube[2] - mcb->cube[3] ) ;
      At = 0 ;
      Bt = mcb->cube[1] + ( mcb->cube[0] - mcb->cube[1] ) * t ;
      Ct = mcb->cube[5] + ( mcb->cube[4] - mcb->cube[5] ) * t ;
      Dt = mcb->cube[6] + ( mcb->cube[7] - mcb->cube[6] ) * t ;
      break ;
    case  3 :
      t  = mcb->cube[3] / ( mcb->cube[3] - mcb->cube[0] ) ;
      At = 0 ;
      Bt = mcb->cube[2] + ( mcb->cube[1] - mcb->cube[2] ) * t ;
      Ct = mcb->cube[6] + ( mcb->cube[5] - mcb->cube[6] ) * t ;
      Dt = mcb->cube[7] + ( mcb->cube[4] - mcb->cube[7] ) * t ;
      break ;
    case  4 :
      t  = mcb->cube[4] / ( mcb->cube[4] - mcb->cube[5] ) ;
      At = 0 ;
      Bt = mcb->cube[7] + ( mcb->cube[6] - mcb->cube[7] ) * t ;
      Ct = mcb->cube[3] + ( mcb->cube[2] - mcb->cube[3] ) * t ;
      Dt = mcb->cube[0] + ( mcb->cube[1] - mcb->cube[0] ) * t ;
      break ;
    case  5 :
      t  = mcb->cube[5] / ( mcb->cube[5] - mcb->cube[6] ) ;
      At = 0 ;
      Bt = mcb->cube[4] + ( mcb->cube[7] - mcb->cube[4] ) * t ;
      Ct = mcb->cube[0] + ( mcb->cube[3] - mcb->cube[0] ) * t ;
      Dt = mcb->cube[1] + ( mcb->cube[2] - mcb->cube[1] ) * t ;
      break ;
    case  6 :
      t  = mcb->cube[6] / ( mcb->cube[6] - mcb->cube[7] ) ;
      At = 0 ;
      Bt = mcb->cube[5] + ( mcb->cube[4] - mcb->cube[5] ) * t ;
      Ct = mcb->cube[1] + ( mcb->cube[0] - mcb->cube[1] ) * t ;
      Dt = mcb->cube[2] + ( mcb->cube[3] - mcb->cube[2] ) * t ;
      break ;
    case  7 :
      t  = mcb->cube[7] / ( mcb->cube[7] - mcb->cube[4] ) ;
      At = 0 ;
      Bt = mcb->cube[6] + ( mcb->cube[5] - mcb->cube[6] ) * t ;
      Ct = mcb->cube[2] + ( mcb->cube[1] - mcb->cube[2] ) * t ;
      Dt = mcb->cube[3] + ( mcb->cube[0] - mcb->cube[3] ) * t ;
      break ;
    case  8 :
      t  = mcb->cube[0] / ( mcb->cube[0] - mcb->cube[4] ) ;
      At = 0 ;
      Bt = mcb->cube[3] + ( mcb->cube[7] - mcb->cube[3] ) * t ;
      Ct = mcb->cube[2] + ( mcb->cube[6] - mcb->cube[2] ) * t ;
      Dt = mcb->cube[1] + ( mcb->cube[5] - mcb->cube[1] ) * t ;
      break ;
    case  9 :
      t  = mcb->cube[1] / ( mcb->cube[1] - mcb->cube[5] ) ;
      At = 0 ;
      Bt = mcb->cube[0] + ( mcb->cube[4] - mcb->cube[0] ) * t ;
      Ct = mcb->cube[3] + ( mcb->cube[7] - mcb->cube[3] ) * t ;
      Dt = mcb->cube[2] + ( mcb->cube[6] - mcb->cube[2] ) * t ;
      break ;
    case 10 :
      t  = mcb->cube[2] / ( mcb->cube[2] - mcb->cube[6] ) ;
      At = 0 ;
      Bt = mcb->cube[1] + ( mcb->cube[5] - mcb->cube[1] ) * t ;
      Ct = mcb->cube[0] + ( mcb->cube[4] - mcb->cube[0] ) * t ;
      Dt = mcb->cube[3] + ( mcb->cube[7] - mcb->cube[3] ) * t ;
      break ;
    case 11 :
      t  = mcb->cube[3] / ( mcb->cube[3] - mcb->cube[7] ) ;
      At = 0 ;
      Bt = mcb->cube[2] + ( mcb->cube[6] - mcb->cube[2] ) * t ;
      Ct = mcb->cube[1] + ( mcb->cube[5] - mcb->cube[1] ) * t ;
      Dt = mcb->cube[0] + ( mcb->cube[4] - mcb->cube[0] ) * t ;
      break ;
    default : printf( "Invalid edge %d\n", edge ) ;  print_cube(mcb) ;  break ;
    }
    break ;

  default : printf( "Invalid ambiguous case %d\n", mcb->_case ) ;  print_cube(mcb) ;  break ;
  }

  if( At >= 0 ) test ++ ;
  if( Bt >= 0 ) test += 2 ;
  if( Ct >= 0 ) test += 4 ;
  if( Dt >= 0 ) test += 8 ;
  switch( test )
  {
  case  0 : return (s>0) ;
  case  1 : return (s>0) ;
  case  2 : return (s>0) ;
  case  3 : return (s>0) ;
  case  4 : return (s>0) ;
  case  5 : if( At * Ct <  Bt * Dt ) return (s>0) ; break ;
  case  6 : return (s>0) ;
  case  7 : return (s<0) ;
  case  8 : return (s>0) ;
  case  9 : return (s>0) ;
  case 10 : if( At * Ct >= Bt * Dt ) return (s>0) ; break ;
  case 11 : return (s<0) ;
  case 12 : return (s>0) ;
  case 13 : return (s<0) ;
  case 14 : return (s<0) ;
  case 15 : return (s<0) ;
  }

  return (s<0) ;
}
//_____________________________________________________________________________




//_____________________________________________________________________________
// Process a unit cube
void process_cube( MCB *mcb)
//-----------------------------------------------------------------------------
{
  int   v12 = -1 ;
  /* print_cube(mcb) ; 
  fprintf (stderr,"_case=%d\n", mcb->_case);
  fprintf (stderr,"N=%d\n", mcb->N[mcb->_case]);*/
  if (mcb->_case >= N_MAX) {
   fprintf (stderr,"Unexpected _case value of %d\nResetting to 0.\n",mcb->_case);
   mcb->_case = 0; 
  }
  mcb->N[mcb->_case]++ ;

  if( mcb->originalMC )
  {
    char nt = 0 ;
    while( casesClassic[mcb->lut_entry][3*nt] != -1 ) nt++ ;
    add_triangle(mcb, casesClassic[mcb->lut_entry], nt , -1) ;
    return ;
  }

  mcb->_case   = cases[mcb->lut_entry][0] ;
  mcb->config = cases[mcb->lut_entry][1] ;
  mcb->subconfig = 0 ;

  switch( mcb->_case )
  {
  case  0 :
    break ;

  case  1 :
    add_triangle(mcb, tiling1[mcb->config], 1 , -1) ;
    break ;

  case  2 :
    add_triangle(mcb, tiling2[mcb->config], 2 , -1) ;
    break ;

  case  3 :
    if( test_face(mcb, test3[mcb->config]) )
      add_triangle(mcb, tiling3[mcb->config]     , 4 , -1) ; // 3.2
    else
      add_triangle(mcb, tiling3[mcb->config] + 12, 2 , -1) ; // 3.1
    break ;

  case  4 :
    if( test_interior(mcb, test4[mcb->config]) )
      add_triangle(mcb, tiling4[mcb->config]     , 2 , -1) ; // 4.1.1
    else
      add_triangle(mcb, tiling4[mcb->config] +  6, 6 , -1) ; // 4.1.2
    break ;

  case  5 :
    add_triangle(mcb, tiling5[mcb->config], 3 , -1) ;
    break ;

  case  6 :
    if( test_face(mcb, test6[mcb->config][0]) )
      add_triangle(mcb, tiling6[mcb->config], 5 , -1) ; // 6.2
    else
    {
      if( test_interior(mcb, test6[mcb->config][1]) )
        add_triangle(mcb, tiling6[mcb->config] + 15, 3 , -1) ; // 6.1.1
      else
        add_triangle(mcb, tiling6[mcb->config] + 24, 7 , -1) ; // 6.1.2
    }
    break ;

  case  7 :
    if( test_face(mcb, test7[mcb->config][0] ) ) mcb->subconfig +=  1 ;
    if( test_face(mcb, test7[mcb->config][1] ) ) mcb->subconfig +=  2 ;
    if( test_face(mcb, test7[mcb->config][2] ) ) mcb->subconfig +=  4 ;
    switch( subconfig7[mcb->subconfig] )
      {
      case 0 :
        if( test_interior(mcb, test7[mcb->config][3]) )
          add_triangle(mcb, tiling7[mcb->config] + 15, 9 , -1) ;
        else
          add_triangle(mcb, tiling7[mcb->config]     , 5 , -1) ;
        break ;
      case 1 :
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling7[mcb->config] +  42, 9, v12 ) ; break ;
      case 2 :
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling7[mcb->config] +  69, 9, v12 ) ; break ;
      case 3 :
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling7[mcb->config] +  96, 9, v12 ) ; break ;
      case 4 :
        add_triangle(mcb, tiling7[mcb->config] + 123, 5 , -1) ; break ;
      case 5 :
        add_triangle(mcb, tiling7[mcb->config] + 138, 5 , -1) ; break ;
      case 6 :
        add_triangle(mcb, tiling7[mcb->config] + 153, 5 , -1) ; break ;
      case 7 :
        add_triangle(mcb, tiling7[mcb->config] + 168, 3 , -1) ; break ;
      };
    break ;

  case  8 :
    add_triangle(mcb, tiling8[mcb->config], 2 , -1) ;
    break ;

  case  9 :
    add_triangle(mcb, tiling9[mcb->config], 4 , -1) ;
    break ;

  case 10 :
    if( test_face(mcb, test10[mcb->config][0]) )
    {
      if( test_face(mcb, test10[mcb->config][1]) )
        add_triangle(mcb, tiling10[mcb->config] + 60, 4 , -1) ; // 10.1.1
      else
      {
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling10[mcb->config]     , 8, v12 ) ; // 10.2
      }
    }
    else
    {
      if( test_face(mcb, test10[mcb->config][1]) )
      {
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling10[mcb->config] + 24, 8, v12 ) ; // 10.2
      }
      else
      {
        if( test_interior(mcb, test10[mcb->config][2]) )
          add_triangle(mcb, tiling10[mcb->config] + 48, 4 , -1) ; // 10.1.1
        else
          add_triangle(mcb, tiling10[mcb->config] + 72, 8 , -1) ; // 10.1.2
      }
    }
    break ;

  case 11 :
    add_triangle(mcb, tiling11[mcb->config], 4 , -1) ;
    break ;

  case 12 :
    if( test_face(mcb, test12[mcb->config][0]) )
    {
      if( test_face(mcb, test12[mcb->config][1]) )
        add_triangle(mcb, tiling12[mcb->config] + 60, 4 , -1) ; // 12.1.1
      else
      {
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling12[mcb->config]     , 8, v12 ) ; // 12.2
      }
    }
    else
    {
      if( test_face(mcb, test12[mcb->config][1]) )
      {
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling12[mcb->config] + 24, 8, v12 ) ; // 12.2
      }
      else
      {
        if( test_interior(mcb, test12[mcb->config][2]) )
          add_triangle(mcb, tiling12[mcb->config] + 48, 4 , -1) ; // 12.1.1
        else
          add_triangle(mcb, tiling12[mcb->config] + 72, 8 , -1) ; // 12.1.2
      }
    }
    break ;

  case 13 :
    if( test_face(mcb, test13[mcb->config][0] ) ) mcb->subconfig +=  1 ;
    if( test_face(mcb, test13[mcb->config][1] ) ) mcb->subconfig +=  2 ;
    if( test_face(mcb, test13[mcb->config][2] ) ) mcb->subconfig +=  4 ;
    if( test_face(mcb, test13[mcb->config][3] ) ) mcb->subconfig +=  8 ;
    if( test_face(mcb, test13[mcb->config][4] ) ) mcb->subconfig += 16 ;
    if( test_face(mcb, test13[mcb->config][5] ) ) mcb->subconfig += 32 ;
    switch( subconfig13[mcb->subconfig] )
    {
      case 0 :/* 13.1 */
        add_triangle(mcb, tiling13_1[mcb->config], 4 , -1) ; break ;

      case 1 :/* 13.2 */
        add_triangle(mcb, tiling13_2[mcb->config][0], 6 , -1) ; break ;
      case 2 :/* 13.2 */
        add_triangle(mcb, tiling13_2[mcb->config][1], 6 , -1) ; break ;
      case 3 :/* 13.2 */
        add_triangle(mcb, tiling13_2[mcb->config][2], 6 , -1) ; break ;
      case 4 :/* 13.2 */
        add_triangle(mcb, tiling13_2[mcb->config][3], 6 , -1) ; break ;
      case 5 :/* 13.2 */
        add_triangle(mcb, tiling13_2[mcb->config][4], 6 , -1) ; break ;
      case 6 :/* 13.2 */
        add_triangle(mcb, tiling13_2[mcb->config][5], 6 , -1) ; break ;

      case 7 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][0], 10, v12 ) ; break ;
      case 8 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][1], 10, v12 ) ; break ;
      case 9 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][2], 10, v12 ) ; break ;
      case 10 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][3], 10, v12 ) ; break ;
      case 11 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][4], 10, v12 ) ; break ;
      case 12 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][5], 10, v12 ) ; break ;
      case 13 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][6], 10, v12 ) ; break ;
      case 14 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][7], 10, v12 ) ; break ;
      case 15 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][8], 10, v12 ) ; break ;
      case 16 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][9], 10, v12 ) ; break ;
      case 17 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][10], 10, v12 ) ; break ;
      case 18 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3[mcb->config][11], 10, v12 ) ; break ;

      case 19 :/* 13.4 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_4[mcb->config][0], 12, v12 ) ; break ;
      case 20 :/* 13.4 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_4[mcb->config][1], 12, v12 ) ; break ;
      case 21 :/* 13.4 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_4[mcb->config][2], 12, v12 ) ; break ;
      case 22 :/* 13.4 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_4[mcb->config][3], 12, v12 ) ; break ;

      case 23 :/* 13.5 */
        mcb->subconfig = 0 ;
        if( test_interior(mcb, test13[mcb->config][6] ) )
          add_triangle(mcb, tiling13_5_1[mcb->config][0], 6 , -1) ;
        else
          add_triangle(mcb, tiling13_5_2[mcb->config][0], 10 , -1) ;
        break ;
      case 24 :/* 13.5 */
        mcb->subconfig = 1 ;
        if( test_interior(mcb, test13[mcb->config][6] ) )
          add_triangle(mcb, tiling13_5_1[mcb->config][1], 6 , -1) ;
        else
          add_triangle(mcb, tiling13_5_2[mcb->config][1], 10 , -1) ;
        break ;
      case 25 :/* 13.5 */
        mcb->subconfig = 2 ;
        if( test_interior(mcb, test13[mcb->config][6] ) )
          add_triangle(mcb, tiling13_5_1[mcb->config][2], 6 , -1) ;
        else
          add_triangle(mcb, tiling13_5_2[mcb->config][2], 10 , -1) ;
        break ;
      case 26 :/* 13.5 */
        mcb->subconfig = 3 ;
        if( test_interior(mcb, test13[mcb->config][6] ) )
          add_triangle(mcb, tiling13_5_1[mcb->config][3], 6 , -1) ;
        else
          add_triangle(mcb, tiling13_5_2[mcb->config][3], 10 , -1) ;
        break ;

      case 27 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][0], 10, v12 ) ; break ;
      case 28 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][1], 10, v12 ) ; break ;
      case 29 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][2], 10, v12 ) ; break ;
      case 30 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][3], 10, v12 ) ; break ;
      case 31 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][4], 10, v12 ) ; break ;
      case 32 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][5], 10, v12 ) ; break ;
      case 33 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][6], 10, v12 ) ; break ;
      case 34 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][7], 10, v12 ) ; break ;
      case 35 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][8], 10, v12 ) ; break ;
      case 36 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][9], 10, v12 ) ; break ;
      case 37 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][10], 10, v12 ) ; break ;
      case 38 :/* 13.3 */
        v12 = add_c_vertex(mcb) ;
        add_triangle(mcb, tiling13_3_[mcb->config][11], 10, v12 ) ; break ;

      case 39 :/* 13.2 */
        add_triangle(mcb, tiling13_2_[mcb->config][0], 6 , -1) ; break ;
      case 40 :/* 13.2 */
        add_triangle(mcb, tiling13_2_[mcb->config][1], 6 , -1) ; break ;
      case 41 :/* 13.2 */
        add_triangle(mcb, tiling13_2_[mcb->config][2], 6 , -1) ; break ;
      case 42 :/* 13.2 */
        add_triangle(mcb, tiling13_2_[mcb->config][3], 6 , -1) ; break ;
      case 43 :/* 13.2 */
        add_triangle(mcb, tiling13_2_[mcb->config][4], 6 , -1) ; break ;
      case 44 :/* 13.2 */
        add_triangle(mcb, tiling13_2_[mcb->config][5], 6 , -1) ; break ;

      case 45 :/* 13.1 */
        add_triangle(mcb, tiling13_1_[mcb->config], 4 , -1) ; break ;

      default :
        printf("Marching Cubes: Impossible case 13?\n" ) ;  print_cube(mcb) ;
      }
      break ;

  case 14 :
    add_triangle(mcb, tiling14[mcb->config], 4 , -1) ;
    break ;
  };
}
//_____________________________________________________________________________



//_____________________________________________________________________________
// Adding triangles
void add_triangle( MCB *mcb , const char* trig, char n, int v12 )
//-----------------------------------------------------------------------------
{
  int   t, tv[3] ;

  for( t = 0 ; t < 3*n ; t++ )
  {
    switch( trig[t] )
    {
    case  0 : tv[ t % 3 ] = get_x_vert(mcb,  mcb->i  , mcb->j , mcb->k ) ; break ;
    case  1 : tv[ t % 3 ] = get_y_vert(mcb, mcb->i +1, mcb->j , mcb->k ) ; break ;
    case  2 : tv[ t % 3 ] = get_x_vert(mcb,  mcb->i  ,mcb->j+1, mcb->k ) ; break ;
    case  3 : tv[ t % 3 ] = get_y_vert(mcb,  mcb->i  , mcb->j , mcb->k ) ; break ;
    case  4 : tv[ t % 3 ] = get_x_vert(mcb,  mcb->i  , mcb->j ,mcb->k+1) ; break ;
    case  5 : tv[ t % 3 ] = get_y_vert(mcb, mcb->i +1, mcb->j ,mcb->k+1) ; break ;
    case  6 : tv[ t % 3 ] = get_x_vert(mcb,  mcb->i  ,mcb->j+1,mcb->k+1) ; break ;
    case  7 : tv[ t % 3 ] = get_y_vert(mcb,  mcb->i  , mcb->j ,mcb->k+1) ; break ;
    case  8 : tv[ t % 3 ] = get_z_vert(mcb,  mcb->i  , mcb->j , mcb->k ) ; break ;
    case  9 : tv[ t % 3 ] = get_z_vert(mcb, mcb->i +1, mcb->j , mcb->k ) ; break ;
    case 10 : tv[ t % 3 ] = get_z_vert(mcb, mcb->i +1,mcb->j+1, mcb->k ) ; break ;
    case 11 : tv[ t % 3 ] = get_z_vert(mcb,  mcb->i  ,mcb->j+1, mcb->k ) ; break ;
    case 12 : tv[ t % 3 ] = v12 ; break ;
    default : break ;
    }

    if( tv[t%3] == -1 )
    {
      printf("Marching Cubes: invalid triangle %d\n", mcb->ntrigs+1) ;
      print_cube(mcb) ;
    }

    if( t%3 == 2 )
    { 
      Triangle *T = NULL;
      if( mcb->ntrigs >= mcb->Ntrigs )
      {
        Triangle *temp = mcb->triangles ;
        mcb->triangles = (Triangle*)malloc(2*mcb->Ntrigs * sizeof(Triangle));
        memcpy( mcb->triangles, temp, mcb->Ntrigs*sizeof(Triangle) ) ;
        free(temp) ; temp = NULL;
        if (debug) printf("%d allocated triangles\n", mcb->Ntrigs) ;
        mcb->Ntrigs *= 2 ;
      }

      T = mcb->triangles + mcb->ntrigs++ ;
      T->v1    = tv[0] ;
      T->v2    = tv[1] ;
      T->v3    = tv[2] ;
    }
  }
}
//_____________________________________________________________________________



//_____________________________________________________________________________
// Calculating gradient

float get_x_grad( MCB *mcb,  int i,  int j,  int k ) 
//-----------------------------------------------------------------------------
{
  if( i > 0 )
  {
    if ( i < mcb->size_x - 1 )
      return (( get_data( mcb, i+1, j, k ) - get_data( mcb, i-1, j, k ) ) / 2) ;
    else
      return (get_data( mcb, i, j, k ) - get_data( mcb, i-1, j, k )) ;
  }
  else
    return (get_data( mcb, i+1, j, k ) - get_data( mcb, i, j, k )) ;
}
//-----------------------------------------------------------------------------

float get_y_grad( MCB *mcb,  int i,  int j,  int k ) 
//-----------------------------------------------------------------------------
{
  if( j > 0 )
  {
    if ( j < mcb->size_y - 1 )
      return (( get_data( mcb, i, j+1, k ) - get_data( mcb, i, j-1, k ) ) / 2) ;
    else
      return (get_data( mcb, i, j, k ) - get_data( mcb, i, j-1, k )) ;
  }
  else
    return (get_data(mcb,  i, j+1, k ) - get_data(mcb, i, j, k )) ;
}
//-----------------------------------------------------------------------------

float get_z_grad( MCB *mcb, int i,  int j,  int k ) 
//-----------------------------------------------------------------------------
{
  if( k > 0 )
  {
    if ( k < mcb->size_z - 1 )
      return (( get_data( mcb, i, j, k+1 ) - get_data( mcb, i, j, k-1 ) ) / 2) ;
    else
      return (get_data( mcb, i, j, k ) - get_data( mcb, i, j, k-1 )) ;
  }
  else
    return (get_data( mcb, i, j, k+1 ) - get_data( mcb, i, j, k )) ;
}
//_____________________________________________________________________________


//_____________________________________________________________________________
// Adding vertices

void test_vertex_addition(MCB *mcb)
{
  if( mcb->nverts >= mcb->Nverts )
  {
    Vertex *temp = mcb->vertices ;
    mcb->vertices =  (Vertex*)malloc(mcb->Nverts*2 * sizeof(Vertex)) ;
    memcpy( mcb->vertices, temp, mcb->Nverts*sizeof(Vertex) ) ;
    free(temp); temp = NULL;
    if (debug) printf("%d allocated vertices\n", mcb->Nverts) ;
    mcb->Nverts *= 2 ;
  }
}


int add_x_vertex(MCB *mcb )
//-----------------------------------------------------------------------------
{
   Vertex *vert;
   float   u;
  
  test_vertex_addition(mcb) ;
  vert = mcb->vertices + mcb->nverts++ ;

  u = ( mcb->cube[0] ) / ( mcb->cube[0] - mcb->cube[1] ) ;

  vert->x      = (float)mcb->i+u;
  vert->y      = (float) mcb->j ;
  vert->z      = (float) mcb->k ;

  vert->nx = (1-u)*get_x_grad(mcb, mcb->i,mcb->j,mcb->k) + u*get_x_grad(mcb, mcb->i+1,mcb->j,mcb->k) ;
  vert->ny = (1-u)*get_y_grad(mcb, mcb->i,mcb->j,mcb->k) + u*get_y_grad(mcb, mcb->i+1,mcb->j,mcb->k) ;
  vert->nz = (1-u)*get_z_grad(mcb, mcb->i,mcb->j,mcb->k) + u*get_z_grad(mcb, mcb->i+1,mcb->j,mcb->k) ;

  u = (float) sqrt( vert->nx * vert->nx + vert->ny * vert->ny +vert->nz * vert->nz ) ;
  
  if( u > 0 )
  {
    vert->nx /= u ;
    vert->ny /= u ;
    vert->nz /= u ;
  }


  return (mcb->nverts-1) ;
}
//-----------------------------------------------------------------------------

int add_y_vertex( MCB *mcb)
//-----------------------------------------------------------------------------
{  Vertex *vert;
   float u;
  test_vertex_addition(mcb) ;
  vert = mcb->vertices + mcb->nverts++ ;

  u = ( mcb->cube[0] ) / ( mcb->cube[0] - mcb->cube[3] ) ;

  vert->x      = (float) mcb->i ;
  vert->y      = (float)mcb->j+u;
  vert->z      = (float) mcb->k ;

  vert->nx = (1-u)*get_x_grad(mcb, mcb->i,mcb->j,mcb->k) + u*get_x_grad(mcb, mcb->i,mcb->j+1,mcb->k) ;
  vert->ny = (1-u)*get_y_grad(mcb, mcb->i,mcb->j,mcb->k) + u*get_y_grad(mcb, mcb->i,mcb->j+1,mcb->k) ;
  vert->nz = (1-u)*get_z_grad(mcb, mcb->i,mcb->j,mcb->k) + u*get_z_grad(mcb, mcb->i,mcb->j+1,mcb->k) ;

  u = (float) sqrt( vert->nx * vert->nx + vert->ny * vert->ny +vert->nz * vert->nz ) ;
  if( u > 0 )
  {
    vert->nx /= u ;
    vert->ny /= u ;
    vert->nz /= u ;
  }

  return (mcb->nverts-1) ;
}
//-----------------------------------------------------------------------------

int add_z_vertex(MCB *mcb )
//-----------------------------------------------------------------------------
{  Vertex *vert;
   float u;
  test_vertex_addition(mcb) ;
  vert = mcb->vertices + mcb->nverts++ ;

  u = ( mcb->cube[0] ) / ( mcb->cube[0] - mcb->cube[4] ) ;

  vert->x      = (float) mcb->i ;
  vert->y      = (float) mcb->j ;
  vert->z      = (float)mcb->k+u;

  vert->nx = (1-u)*get_x_grad(mcb, mcb->i,mcb->j,mcb->k) + u*get_x_grad(mcb, mcb->i,mcb->j,mcb->k+1) ;
  vert->ny = (1-u)*get_y_grad(mcb, mcb->i,mcb->j,mcb->k) + u*get_y_grad(mcb, mcb->i,mcb->j,mcb->k+1) ;
  vert->nz = (1-u)*get_z_grad(mcb, mcb->i,mcb->j,mcb->k) + u*get_z_grad(mcb, mcb->i,mcb->j,mcb->k+1) ;

  u = (float) sqrt( vert->nx * vert->nx + vert->ny * vert->ny +vert->nz * vert->nz ) ;
  if( u > 0 )
  {
    vert->nx /= u ;
    vert->ny /= u ;
    vert->nz /= u ;
  }

  return (mcb->nverts-1) ;
}


int add_c_vertex( MCB *mcb)
//-----------------------------------------------------------------------------
{  Vertex *vert, v;
   float u;
   int   vid ;
  test_vertex_addition(mcb) ;
  vert = mcb->vertices + mcb->nverts++ ;

  u = 0 ;

  vert->x = vert->y = vert->z =  vert->nx = vert->ny = vert->nz = 0 ;

  // Computes the average of the intersection points of the cube
  vid = get_x_vert( mcb, mcb->i , mcb->j , mcb->k ) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_y_vert(mcb, mcb->i+1, mcb->j , mcb->k ) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_x_vert( mcb, mcb->i ,mcb->j+1, mcb->k ) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_y_vert( mcb, mcb->i , mcb->j , mcb->k ) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_x_vert( mcb, mcb->i , mcb->j ,mcb->k+1) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_y_vert(mcb, mcb->i+1, mcb->j ,mcb->k+1) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_x_vert( mcb, mcb->i ,mcb->j+1,mcb->k+1) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_y_vert(mcb,  mcb->i , mcb->j ,mcb->k+1) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_z_vert( mcb, mcb->i , mcb->j , mcb->k ) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_z_vert(mcb, mcb->i+1, mcb->j , mcb->k ) ;
  if( vid != -1 ) { ++u ;  v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_z_vert(mcb, mcb->i+1,mcb->j+1, mcb->k ) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }
  vid = get_z_vert( mcb, mcb->i ,mcb->j+1, mcb->k ) ;
  if( vid != -1 ) { ++u ;   v = mcb->vertices[vid] ; vert->x += v.x ;  vert->y += v.y ;  vert->z += v.z ;  vert->nx += v.nx ; vert->ny += v.ny ; vert->nz += v.nz ; }

  vert->x  /= u ;
  vert->y  /= u ;
  vert->z  /= u ;

  u = (float) sqrt( vert->nx * vert->nx + vert->ny * vert->ny +vert->nz * vert->nz ) ;
  if( u > 0 )
  {
    vert->nx /= u ;
    vert->ny /= u ;
    vert->nz /= u ;
  }

  return (mcb->nverts-1) ;
}
//_____________________________________________________________________________



//_____________________________________________________________________________
//_____________________________________________________________________________




//_____________________________________________________________________________
// Grid exportation
void writeISO(MCB *mcb, const char *fn )
//-----------------------------------------------------------------------------
{
  unsigned char buf[sizeof(float)] ;
   int i, j, k;
  FILE *fp = fopen( fn, "wb" ) ;

  // header
  * (int*) buf = mcb->size_x ;
  fwrite(buf, sizeof(float), 1, fp);
  * (int*) buf = mcb->size_y ;
  fwrite(buf, sizeof(float), 1, fp);
  * (int*) buf = mcb->size_z ;
  fwrite(buf, sizeof(float), 1, fp);

  for(  i = 0 ; i < mcb->size_x ; i++ )
  {
    for(  j = 0 ; j < mcb->size_y ; j++ )
    {
      for(  k = 0 ; k < mcb->size_z ; k++ )
      {
        * (float*) buf = get_data( mcb, i,j,k ) ;
        fwrite(buf, sizeof(float), 1, fp);
      }
    }
  }

  fclose(fp) ;
}
//_____________________________________________________________________________



#if 0
/* no need here*/
//_____________________________________________________________________________
// PLY exportation
typedef struct PlyFace
{
  unsigned char nverts;    /* number of Vertex indices in list */
  int *verts;              /* Vertex index list */
} PlyFace;


PlyProperty vert_props[]  = { /* list of property information for a PlyVertex */
  {"x", Float32, Float32, offsetof( Vertex,x ), 0, 0, 0, 0},
  {"y", Float32, Float32, offsetof( Vertex,y ), 0, 0, 0, 0},
  {"z", Float32, Float32, offsetof( Vertex,z ), 0, 0, 0, 0},
  {"nx", Float32, Float32, offsetof( Vertex,nx ), 0, 0, 0, 0},
  {"ny", Float32, Float32, offsetof( Vertex,ny ), 0, 0, 0, 0},
  {"nz", Float32, Float32, offsetof( Vertex,nz ), 0, 0, 0, 0}
};

PlyProperty face_props[]  = { /* list of property information for a PlyFace */
  {"vertex_indices", Int32, Int32, offsetof( PlyFace,verts ),
    1, Uint8, Uint8, offsetof( PlyFace,nverts )},
};

void writePLY(MCB *mcb, const char *fn, int bin )
//-----------------------------------------------------------------------------
{
  PlyFile    *ply;
  FILE       *fp = fopen( fn, "w" );

  int          i ;
  PlyFace     face ;
  int         verts[3] ;
  char       *elem_names[]  = { "vertex", "face" };
  printf("Marching Cubes::writePLY(%s)...", fn ) ;
  ply = write_ply ( fp, 2, elem_names, bin? PLY_BINARY_LE : PLY_ASCII );

  /* describe what properties go into the PlyVertex elements */
  describe_element_ply ( ply, "vertex", mcb->nverts );
  describe_property_ply ( ply, &vert_props[0] );
  describe_property_ply ( ply, &vert_props[1] );
  describe_property_ply ( ply, &vert_props[2] );
  describe_property_ply ( ply, &vert_props[3] );
  describe_property_ply ( ply, &vert_props[4] );
  describe_property_ply ( ply, &vert_props[5] );

  /* describe PlyFace properties (just list of PlyVertex indices) */
  describe_element_ply ( ply, "face", mcb->ntrigs );
  describe_property_ply ( ply, &face_props[0] );

  header_complete_ply ( ply );

  /* set up and write the PlyVertex elements */
  put_element_setup_ply ( ply, "vertex" );
  for ( i = 0; i < mcb->nverts; i++ )
    put_element_ply ( ply, ( void * ) &(mcb->vertices[i]) );
  printf("   %d vertices written\n", mcb->nverts ) ;

  /* set up and write the PlyFace elements */
  put_element_setup_ply ( ply, "face" );
  face.nverts = 3 ;
  face.verts  = verts ;
  for ( i = 0; i < mcb->ntrigs; i++ )
  {
    face.verts[0] = mcb->triangles[i].v1 ;
    face.verts[1] = mcb->triangles[i].v2 ;
    face.verts[2] = mcb->triangles[i].v3 ;
    put_element_ply ( ply, ( void * ) &face );
  }
  printf("   %d triangles written\n", mcb->ntrigs ) ;

  close_ply ( ply );
  free_ply ( ply );
  fclose( fp ) ;
}
//_____________________________________________________________________________



//_____________________________________________________________________________
// Open Inventor / VRML 1.0 ascii exportation
void writeIV(MCB *mcb, const char *fn )
//-----------------------------------------------------------------------------
{
  FILE *fp = fopen( fn, "w" ) ;
  int   i ;

  printf("Marching Cubes::exportIV(%s)...", fn) ;

  fprintf( fp, "#Inventor V2.1 ascii \n\nSeparator { \n    ShapeHints {\n        vertexOrdering  COUNTERCLOCKWISE\n        shapeType       UNKNOWN_SHAPE_TYPE\n        creaseAngle     0.0\n    }\n Coordinate3 { \n point [  \n" ) ;
  for ( i = 0; i < mcb->nverts; i++ )
    fprintf( fp, " %f %f %f,\n", mcb->vertices[i].x, mcb->vertices[i].y, mcb->vertices[i].z ) ;
  printf("   %d vertices written\n", mcb->nverts ) ;

  fprintf( fp, "\n ] \n} \nNormal { \nvector [ \n" ) ;
  for ( i = 0; i < mcb->nverts; i++ )
    fprintf( fp, " %f %f %f,\n", mcb->vertices[i].nx, mcb->vertices[i].ny, mcb->vertices[i].nz ) ;

  fprintf( fp, "\n ] \n} \nIndexedFaceSet { \ncoordIndex [ \n" ) ;
  for ( i = 0; i < mcb->ntrigs; i++ )
    fprintf( fp, "%d, %d, %d, -1,\n", mcb->triangles[i].v1, mcb->triangles[i].v2, mcb->triangles[i].v3 ) ;

  fprintf( fp, " ] \n } \n } \n" ) ;
  fclose( fp ) ;
  printf("   %d triangles written\n", mcb->ntrigs ) ;
}
//_____________________________________________________________________________

#endif

void write1Dmcb(MCB *mcb)
{
   FILE *fp = fopen( "testNodes.1D", "w" ) ;
   int   i ;

   printf("Marching Cubes: export1D(test*.1D)...") ;
   for ( i = 0; i < mcb->nverts; i++ )
      fprintf( fp, " %f %f %f\n", mcb->vertices[i].x, mcb->vertices[i].y, mcb->vertices[i].z ) ;
   printf("\n   %d vertices written\n", mcb->nverts ) ;
   fclose(fp);
   
   fp = fopen( "testFaces.1D", "w" ) ;
   for ( i = 0; i < mcb->ntrigs; i++ )
    fprintf( fp, "%d %d %d\n", mcb->triangles[i].v3, mcb->triangles[i].v2, mcb->triangles[i].v1 ) ;
   printf("   %d triangles written\n", mcb->ntrigs ) ;
   printf("Suggested command for viewing:\n");
   printf("quickspec -tn 1D testNodes.1D testFaces.1D && suma -spec quick.spec\n"); 
   fclose(fp);
}

//_____________________________________________________________________________
// Compute data
void compute_data( MCB mc , int obj_type)
//-----------------------------------------------------------------------------
{
  float x,y,z      ;
  float sx,sy,sz   ;
  float tx,ty,tz , val = 0 ;
  int i, j, k;
  float r,R, a, b, c, d;
  int WriteVol = debug;
  FILE *fid = NULL;
  
  if (obj_type < 0 || obj_type > 9) {
   fprintf(stderr,"Bad obj_type. Value must be between 0 and 9\n");
   return;
  }
  
  if (WriteVol) {
   char vname[200],vnamepref[200];
   sprintf(vnamepref,"mc_shape_%d_vol%d", obj_type, mc.size_x);
   sprintf(vname,"%s.1D", vnamepref);
   printf(  "Creating object %d and writing its volume to %s.\n"
            "To view the volume, use:\n"
            "3dUndump -ijk -dimen %d %d %d -prefix %s %s && afni %s+orig.HEAD\n "
            , obj_type, vname, mc.size_x, mc.size_y, mc.size_z, vnamepref, vname, vnamepref);
   fid = fopen(vname, "w");
  }
 
  r = 1.85f ;
  R = 4 ;

  sx     = (float) mc.size_x / 16 ;
  sy     = (float) mc.size_y / 16 ;
  sz     = (float) mc.size_z / 16 ;
  tx     = (float) mc.size_x / (2*sx) ;
  ty     = (float) mc.size_y / (2*sy) + 1.5f ;
  tz     = (float) mc.size_z / (2*sz) ;

  for(  k = 0 ; k < mc.size_z ; k++ )
  {
    z = ( (float) k ) / sz  - tz ;

    for(  j = 0 ; j < mc.size_y ; j++ )
    {
      y = ( (float) j ) / sy  - ty ;

      for(  i = 0 ; i < mc.size_x ; i++ )
      {
        x = ( (float) i ) / sx - tx ;
        switch(obj_type) {
         case 0: //cushin
            val = z*z*x*x - z*z*z*z - 2*z*x*x + 2*z*z*z + x*x - z*z - (x*x - z)*(x*x - z) - y*y*y*y - 2*x*x*y*y - y*y*z*z + 2*y*y*z + y*y;
            break;
         case 1: //sphere
            val = ( (x-2)*(x-2) + (y-2)*(y-2) + (z-2)*(z-2) - 1 ) * ( (x+2)*(x+2) + (y-2)*(y-2) + (z-2)*(z-2) - 1 ) * ( (x-2)*(x-2) + (y+2)*(y+2) + (z-2)*(z-2) - 1 );
            break;
         case 2: // plane
            val = x+y+z -3;
            break;
         case 3: // cassini
            val = (x*x + y*y + z*z + 0.45f*0.45f)*(x*x + y*y + z*z + 0.45f*0.45f) - 16*0.45f*0.45f*(x*x + z*z) - 0.5f*0.5f;
            break;
         case 4: // blooby
            val = x*x*x*x - 5*x*x+ y*y*y*y - 5*y*y + z*z*z*z - 5*z*z + 11.8;
            break;
         case 5: // chair
            val = (x*x+y*y+z*z-0.95f*25)*(x*x+y*y+z*z-0.95f*25)-0.8f*((z-5)*(z-5)-2*x*x)*((z+5)*(z+5)-2*y*y);
            break;
         case 6: // cyclide
            b = 2; d = 6; a = 2; c = 3;
            val = ( x*x + y*y + z*z + b*b - d*d ) * ( x*x + y*y + z*z + b*b - d*d ) - 4 * ( ( a*x - c*d ) * ( a*x - c*d ) + b*b * y*y );
            break;
         case 7: // 2 torus
            val = ( ( x*x + y*y + z*z + R*R - r*r ) * ( x*x + y*y + z*z + R*R - r*r ) - 4 * R*R * ( x*x + y*y ) ) *
                  ( ( x*x + (y+R)*(y+R) + z*z + R*R - r*r ) * ( x*x + (y+R)*(y+R) + z*z + R*R - r*r ) - 4 * R*R * ( (y+R)*(y+R) + z*z ) ) ;
            break;
         case 8: // mc case
            val =    - 26.5298*(1-x)*(1-y)*(1-z) + 81.9199*x*(1-y)*(1-z) - 100.68*x*y*(1-z) + 3.5498*(1-x)*y*(1-z)
                     + 24.1201*(1-x)*(1-y)*  z   - 74.4702*x*(1-y)*  z   + 91.5298*x*y*  z  - 3.22998*(1-x)*y*  z ;
            break;
         case 9: // Drip
            val = x*x + y*y - 0.5*( 0.995*z*z + 0.005 - z*z*z ) +0.0025;  // -0.0754+0.01, -0.0025 + 0.01, grid 40^3, [-1.5,1.5];
            break;
         /*case : // 
            val = ;
            break;
         case : // 
            val = ;
            break;*/
        }
        set_data( &mc, val, i,j,k ) ;
         if (WriteVol) { if (fid) fprintf(fid, "%d %d %d %f\n", i, j, k, val); }
      }
    }
  }
   if (WriteVol) {
      fclose(fid); fid = NULL;
   }
}
//_____________________________________________________________________________

//_____________________________________________________________________________


void z_compute_data( MCB mc , char *fname)
{
   FILE *fid=NULL;
   float *v = NULL;
   int cnt = 0, nv, ir, jr, kr;
   
   nv = mc.size_z*mc.size_y*mc.size_x;
   v = (float *)malloc(sizeof(float)*nv);
   fid = fopen(fname, "r");
   if (!fid) {
      fprintf(stderr,"Failed to open file\n");
      exit(1);
   }
   for (cnt=0; cnt < nv; ++cnt) {
      fscanf(fid, "%d %d %d %f\n", &ir, &jr, &kr, &(v[cnt]));
      #if 0
         fprintf(stderr, "%d %d %d %f\n", ir, jr, kr, (v[cnt]));
      #endif
      set_data( &mc, v[cnt], ir, jr, kr); 
   }
  
  
    
  fclose(fid); fid = NULL; 
  free(v); v = NULL;
}


