#ifndef SUMA_TRACKBALL_INCLUDED
#define SUMA_TRACKBALL_INCLUDED

/*! functions defined in SUMA_trackball.c */
void trackball(float q[4], float p1x, float p1y, float p2x, float p2y);
void trackball_Phi(float q[4], float p1x, float p1y, float p2x, float p2y, float phi);
void add_quats(float *q1, float *q2, float *dest);
void inverse_quat(float q[4]);
void SUMA_build_rotmatrix(float m[4][4], float q[4]);
void axis_to_quat(float a[3], float phi, float q[4]);

#endif
