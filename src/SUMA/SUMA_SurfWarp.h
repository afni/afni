#ifndef SUMA_SURFWARP_INCLUDED
#define SUMA_SURFWARP_INCLUDED
typedef struct
   {
      int dbg_flag;
      int N_sub;           /* Number of subdivisions. (For 2D objects, it is the number of nodes.) */
      int N_step;          /* Number of steps.  Related to dt.  This is the inverse of dt. */
      int dom_dim;
      double dt;           /* Used to store step size which is calculated using N_step. */
      char *ctrl;          /* A pointer copy of argv containing filename. Do not free. */
      int N_ctrl_points;
      int *CtrlPts_iim;    /* Index of a particular ctrl point in the mesh. It's not necessary
                              for calculations but useful for debugging. An index of -1 is used
                              if ctrl point does not overlap with a node */
      double *CtrlPts;
      double *CtrlPts_i;   /* Specified by user. Will not change. */
      double *CtrlPts_f;
      double *Dtheta;
      double *Nrm;         /* Axis of rotation used for calculated velocity at the control points. */
      double Center[3];
      double Radius;
      int renew_weights;
      int adjust; 
      int dim;
      int dot;
      int neighb_adjust;   /* Check distance to nearest node and adjust step size accordingly. */
      int neighb_check;    /* Check nearest neighbor distances, but don't make a dt adjustment. */
      char outfile[500];
      int pause;
      double sigma;        /* If using sigma option to calculate weights, need to find new way to
                              determine velocity field for the entire surface.  Old way won't work. */
      int M_time_steps;    /* Number of internal time steps used in optimization.  M total steps used in Bob's equations. */
      int sin_kern;        /* Option for turning on the sine kernal.  Using sin(theta)/theta as part of the expansion factor. */
   } MyCircleOpt;

typedef struct
   {
      int N_Node;
      double *NodeList;
      double *VelocityField;
      double *Vf_Step;
      double *VelocityMagnitude;
      double *NewNodeList;
      double *Theta;
      vector Wv;
   } MyCircle;


SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_toy_circle_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps, MyCircleOpt *popt);
SUMA_Boolean Debug_Weights( MyCircle *C, MyCircleOpt *opt, matrix M, matrix Mi, vector Vv) ;
SUMA_Boolean FindSplineWeights (MyCircle *C, MyCircleOpt *opt);
SUMA_Boolean Velocity( MyCircle *C, MyCircleOpt *opt) ;
SUMA_Boolean Debug_Move( MyCircle *C, MyCircleOpt *opt, SUMA_SurfaceObject *SO, double dt, int niter, int m, int a_niter, int first_bad_niter) ;
SUMA_Boolean Neighbor( MyCircle *C, MyCircleOpt *opt, SUMA_SurfaceObject *SO, int niter, int a_niter) ;
SUMA_Boolean Calculate_Step (MyCircle *C, MyCircleOpt *opt, double dt) ;
SUMA_Boolean Move_Points (MyCircle *C, MyCircleOpt *opt) ;
SUMA_Boolean Set_up_Control_Curve( MyCircleOpt *opt, SUMA_MX_VEC *ControlCurve );
SUMA_Boolean Perturbations( MyCircleOpt *opt, SUMA_MX_VEC *ControlCurve, SUMA_MX_VEC *MaxStep, SUMA_MX_VEC *Perturb_Vec );
SUMA_Boolean Print_Matrix( MyCircleOpt *opt, matrix M );
SUMA_Boolean Rotation_Matrix( MyCircleOpt *opt, vector X, matrix M);
SUMA_Boolean Change_in_Energy( MyCircleOpt *opt, SUMA_MX_VEC *ControlCurve, SUMA_MX_VEC *Perturb_Vec, SUMA_MX_VEC *Del_Sl );
double S_energy( MyCircleOpt *opt, SUMA_MX_VEC *VecX );
double Find_Lamda( MyCircleOpt *opt, SUMA_MX_VEC *ControlCurve, SUMA_MX_VEC *MaxStep, SUMA_MX_VEC *Perturb_Vec, SUMA_MX_VEC *Del_S, SUMA_MX_VEC *X_Lamda );
#endif
