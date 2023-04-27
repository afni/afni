
#include <stdio.h>
#include <math.h>
#include <time.h>
#include <memory.h>
#include <float.h>
#include <stdlib.h>
#include <math.h>

#include "MarchingCubes.h"
#include "mc_ziad.h"

//_____________________________________________________________________________
// Grid exportation
void writeISO(MCB *mcb, const char *fn )
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
{
  float x,y,z      ;
  float sx,sy,sz   ;
  float tx,ty,tz , val = 0 ;
  int i, j, k;
  float r,R, a, b, c, d;
  int WriteVol = debug;  /* change to function */
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


