#ifndef Z_MARCHINGCUBES_H_
#define Z_MARCHINGCUBES_H_

#if 0 /* Not needed */
int nverts(MCB *mcb)  { return (mcb->nverts) ; }
int ntrigs(MCB *mcb)  { return (mcb->ntrigs) ; }
int size_x(MCB *mcb)  { return (mcb->size_x) ; }
int size_y(MCB *mcb)  { return (mcb->size_y) ; }
int size_z(MCB *mcb)  { return (mcb->size_z) ; }

Vertex   *vertices (MCB *mcb) { return (mcb->vertices)  ; }
Triangle *triangles(MCB *mcb) { return (mcb->triangles) ; }

Vertex   * vert( MCB *mcb, const int i )  { if( i < 0  || i >= mcb->nverts ) return (( Vertex *)NULL) ; return (mcb->vertices  + i) ; }
Triangle * trig( MCB *mcb, const int i )  { if( i < 0  || i >= mcb->ntrigs ) return ((Triangle*)NULL) ; return (mcb->triangles + i) ; }

// PLY exportation
void writePLY( MCB *mcb , const char *fn, int bin) ; /* bin = false is the default */

// VRML / Open Inventor exportation
void writeIV ( MCB *mcb , const char *fn ) ;

#endif

// Grid exportation
void writeISO( MCB *mcb, const char *fn ) ;

//1D deal
void write1Dmcb ( MCB *mcb  );

void compute_data( MCB mc , int obj_type) ;
void z_compute_data( MCB mc, char *fname ) ;

#endif // Z_MARCHINGCUBES_H_
