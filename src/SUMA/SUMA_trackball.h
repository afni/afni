#ifndef SUMA_TRACKBALL_INCLUDED
#define SUMA_TRACKBALL_INCLUDED

/*! functions defined in SUMA_trackball.c */
void trackball(float q[4], float p1x, float p1y, float p2x, float p2y);
void add_quats(float *q1, float *q2, float *dest);
void build_rotmatrix(float m[4][4], float q[4]);
void axis_to_quat(float a[3], float phi, float q[4]);

#endif
