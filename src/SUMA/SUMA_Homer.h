#ifndef SUMA_SUMA_HOMER_INCLUDED
#define SUMA_SUMA_HOMER_INCLUDED
/* Converted to C/C++ by Crossroads V1.0*/


typedef struct Point2Struct {
	double x, y;
} Point2;

typedef struct Point3Struct {
	double x, y, z;
} Point3;

float * SUMA_HomerVertex(Point3 *Vert, int *N);
int * SUMA_HomerFace(long *face, int *N);




#endif
