#include "SUMA_suma.h"
#include "matrix.h"
#include "SUMA_SurfWarp.h"

extern SUMA_SurfaceViewer *SUMAg_SVv;

/******************* Begin optimizing functions here ************************************************/
/* DON'T FORGET TO ADD FUNCTIONS TO THE SUMA_SURFWARP.H FILE!!! */

double Matrix_Condition_Num (matrix M, FILE *condition_num)
{
   static char FuncName[]={"Matrix_Condition_Num"};
   double *ev, ebot, emin, emax, cond = 0.0; 
   int i, nsmall=0 ;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   #ifndef PSINV_EPS
   #define PSINV_EPS 1.e-12
   #endif
   
   ev = matrix_singvals( M ) ;
   emax = 1.e-38 ;
   for( i=0 ; i < M.cols ; i++ ){
      if( ev[i] > emax ) emax = ev[i] ;
   }
   
   fprintf(condition_num, "E = [\n");
   for (i=0 ; i < M.cols ; i++ ){
      fprintf(condition_num, "%f\n", ev[i]);
   }
   fprintf(condition_num, "];\n \n");
   
   ebot = sqrt(PSINV_EPS)*emax ; emin = 1.e+38 ;
   for( i=0 ; i < M.cols ; i++ ){
      if( ev[i] >= ebot && ev[i] < emin ) emin = ev[i] ;
      if( ev[i] <  ebot ) nsmall++ ;
   }
   if( nsmall > 0 ){
      fprintf(stderr,
        "** WARNING: Largest singular value=%g;"
        "            Implies strong collinearity in the input regressors\n", emax ) ;
   }
   
   if(LocalHead) fprintf(stderr,"min ev=%g  max ev=%g\n",emin,emax ) ;

   free((void *)ev) ;
   if( emin <= 0.0 || emax <= 0.0 ){
      fprintf(condition_num,"** Matrix condition:  UNDEFINED: "
                     "min ev=%g  max ev=%g  ** VERY BAD **\n",emin,emax ) ;
   } else {
      cond = emax/emin ;
      fprintf(condition_num,"Matrix condition : %f\n", cond) ;
   }
 SUMA_RETURN(cond);
}

double Optimization_Kernel(MyCircleOpt *opt, double theta)
{
   static char FuncName[]={"Optimization_Kernel"};
   double expand = 0.0;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
  
   if(opt->sin_kern) {
         if(theta > opt->Zero) { expand = SUMA_POW2((sin(theta)/theta))*exp(-0.5*theta*theta); 
         } else { expand = 1.0; }
   }
  
   expand = exp(-0.5*theta*theta); 
  
   SUMA_RETURN(expand);
}

double Deformation_Kernel(MyCircleOpt *opt, double theta)
{
   static char FuncName[]={"Deformation_Kernel"};
   double expand = 0.0;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   if(opt->sin_kern) {
         if(theta > opt->Zero) { expand = SUMA_POW2((sin(theta)/theta))*exp(-0.5*theta*theta); 
         } else { expand = 1.0; }
   }
   
   SUMA_RETURN(expand); 
}

SUMA_Boolean Set_up_Control_Curve( MyCircleOpt *opt, SUMA_MX_VEC *ControlCurve )
{
   static char FuncName[]={"Set_up_Control_Curve"};
   int i = 0, j, k, i3, j3, k2;
   double arc_theta, arc_nrm[3], mag_arc_nrm;
   double *dp = NULL, *dp2 = NULL;
   double theta = 0.0, nrm[3] = {0.0, 0.0, 0.0};
   
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   for (j=0; j<opt->N_ctrl_points; ++j) {
      j3 = 3*j;
   
      /* Store intial location of control point. */
      k=0; 
      dp = mxvdp3(ControlCurve, i, j, k);
      dp[0] = opt->CtrlPts_i[j3  ];
      dp[1] = opt->CtrlPts_i[j3+1];
      dp[2] = opt->CtrlPts_i[j3+2];
      dp = NULL;
     
      /* Calculate and store intermediate steps of control point path. */
      SUMA_ANGLE_DIST_NC((&(opt->CtrlPts_f[j3])), (&(opt->CtrlPts_i[j3])), arc_theta, arc_nrm);
      arc_theta = arc_theta/opt->M_time_steps;
      mag_arc_nrm = sqrt( SUMA_POW2(arc_nrm[0]) + SUMA_POW2(arc_nrm[1]) + SUMA_POW2(arc_nrm[2]) );
      if(mag_arc_nrm > opt->Zero) { 
         arc_nrm[0] *= (1.0/mag_arc_nrm);
         arc_nrm[1] *= (1.0/mag_arc_nrm);
         arc_nrm[2] *= (1.0/mag_arc_nrm);
      }
      dp = mxvdp3(ControlCurve, i, j, k);
      
      for (k=1; k<opt->M_time_steps; ++k) {
         dp2 = mxvdp3(ControlCurve, i, j, k);
         SUMA_ROTATE_ABOUT_AXIS( (&(dp[0])), arc_nrm, arc_theta, (&(dp2[0])) );
         if(LocalHead) fprintf(SUMA_STDERR, "Rotated to k=1 location: %f %f %f\n", dp2[0], dp2[1], dp2[2]);
         dp = dp2;
      }
      dp = NULL; dp2 = NULL;
      
      /* Store final location of control point. */
      k=opt->M_time_steps;
      dp = mxvdp3(ControlCurve, i, j, k);
      dp[0] = opt->CtrlPts_f[j3  ];
      dp[1] = opt->CtrlPts_f[j3+1];
      dp[2] = opt->CtrlPts_f[j3+2];
      dp = NULL;
   }

#if 0   
   /* For comparing the angular distance between the initial and final control points. */
   FILE *angle_dist = NULL;  
   angle_dist = fopen( "angle_dist_check.1D", "w" );
   for (j=0; j<opt->N_ctrl_points; ++j) {
      j3 = 3*j;
      k=0; k2=opt->M_time_steps;
      dp = mxvdp3(ControlCurve, i, j, k);
      dp2 = mxvdp3(ControlCurve, i, j, k2);
      
      theta = 0.0; nrm[0] = 0.0; nrm[1] = 0.0; nrm[2] = 0.0;
      SUMA_ANGLE_DIST_NC((&(dp2[0])), (&(dp[0])), theta, nrm)
      fprintf(angle_dist, "%d %f\n", j, theta);
   }
   fclose (angle_dist); angle_dist = NULL;
#endif
   
   SUMA_RETURN(YUP);
}
SUMA_SegmentDO *SUMA_Perturbation2SDO(SUMA_MX_VEC *ControlCurve, SUMA_MX_VEC *Perturb_Vec, char *Label, int p, double scale)
{
   static char FuncName[]={"SUMA_Perturbation2SDO"};
   SUMA_SegmentDO *SDO=NULL;
   int N_n,  oriented,  NodeBased,  Stipple, i, m, d, kcc;
   char *idcode_str=NULL,  *Parent_idcode_str=NULL;
   float LineWidth;
   double *dp=NULL, *dp_new=NULL;
   int *NodeId=NULL;
   float *n0=NULL,  *n1=NULL;
   float *colv=NULL, *thickv=NULL;
   float acol[4], LineCol[4] = { 1.000000, 0.300000, 1.000000, 1.000000 };

   SUMA_ENTRY;

   oriented = 1;
   Stipple = 0;
   NodeBased = 0;
   LineWidth = 8;
   SUMA_NEW_ID(idcode_str, Label);

   N_n = ControlCurve->dims[1] * (ControlCurve->dims[2]);
   n0 = (float *) SUMA_malloc(sizeof(float)*N_n*3);
   n1 = (float *) SUMA_malloc(sizeof(float)*N_n*3);
   colv = (float *) SUMA_malloc(sizeof(float)*N_n*4);
   thickv = (float *) SUMA_malloc(sizeof(float)*N_n);

   
   kcc = 0;
   for(i=0; i<ControlCurve->dims[1]; ++i) {
      for (m=0; m<ControlCurve->dims[2]; ++m) {
         if(1) {
            dp = mxvdp3(ControlCurve, 0, i, m);
            dp_new = mxvdp4(Perturb_Vec, 0, i, m, p);
            for (d=0;d<3;++d) {
               n0[3*kcc+d] = dp[d];
               n1[3*kcc+d] = dp[d]+scale*dp_new[d];
            }
         }                            
         if (p==0) {
            colv[4*kcc+0] = 1.0; colv[4*kcc+1] = colv[4*kcc+2] = colv[4*kcc+3] = 0.0;  
         } else{
            colv[4*kcc+1] = 1.0; colv[4*kcc+0] = colv[4*kcc+2] = colv[4*kcc+3] = 0.0;  
         }
         thickv[kcc] = 2;   
         ++kcc;
         dp = NULL; dp_new = NULL;          
      }
   }

   SDO = SUMA_CreateSegmentDO( N_n, oriented, NodeBased, Stipple,
                               Label, idcode_str, Parent_idcode_str,
                               LineWidth, LineCol,
                               NodeId, NULL, n0, n1,
                               colv, thickv 
                              );

   if (idcode_str) SUMA_free(idcode_str); idcode_str = NULL;
   if (n0) SUMA_free(n0); n0 = NULL;
   if (n1) SUMA_free(n1); n1 = NULL;
   if (colv) SUMA_free(colv); colv = NULL;
   if (thickv) SUMA_free(thickv); thickv = NULL;

   SUMA_RETURN(SDO);
}

SUMA_Boolean Perturbations(   MyCircleOpt *opt, SUMA_MX_VEC *ControlCurve, 
                              SUMA_MX_VEC *MaxStep, SUMA_MX_VEC *Perturb_Vec, 
                              SUMA_GENERIC_ARGV_PARSE *ps )
{
   static char FuncName[]={"Perturbations"};
   int i, j, k, p;
   double theta= 0.0, theta_2 = 0.0, nrm[3], p1[3], p1_mag, nrm_mag, theta_fac;
   double *dp = NULL, *dp2 = NULL, *dp_m1 = NULL, *dp_m2 = NULL, *dp_p1 = NULL, *dp_p2 = NULL;   
   
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   for (k=1; k<opt->M_time_steps; ++k) {  /* Compare the ith c.p. to all other c.p., at time k. */
      for(i=0; i<opt->N_ctrl_points; ++i) { 
         dp = mxvdp3(ControlCurve, 0, i, k);  /* Pointer to control point of interest, at time k. */
         if(LocalHead) fprintf(SUMA_STDERR, "Control Point(i=%d,k= %d) = %f %f %f\n", i, k, dp[0], dp[1], dp[2]); 
         
         /* Compare control point of interest to all other control points. */
         for (j=0; j<opt->N_ctrl_points; ++j) {
            dp2 = mxvdp3(ControlCurve, 0, j, k);
            if(LocalHead) fprintf(SUMA_STDERR, "Control Point(j=%d, k=%d) = %f %f %f\n", j, k, dp2[0], dp2[1], dp2[2]); 
            
            if( j<1 ) SUMA_ANGLE_DIST_NC((&(dp2[0])), (&(dp[0])), theta, nrm);
            if(j>0) SUMA_ANGLE_DIST_NC((&(dp2[0])), (&(dp[0])), theta_2, nrm);
            if(LocalHead) {
               if(theta) fprintf(SUMA_STDERR, "Theta(i=%d,j=%d,k=%d) = %f\n", i, j, k, theta);
               if(theta_2) fprintf(SUMA_STDERR, "Theta_2(i=%d,j=%d,k=%d) = %f\n", i, j, k, theta_2); 
            }            
            if(theta<opt->Zero) theta = theta_2;
            else if(theta_2<opt->Zero) theta = theta; 
            else if(theta_2 && theta) { 
               if(theta_2 < theta) theta = theta_2;
               else theta = theta;
            }            
            if(LocalHead) fprintf(SUMA_STDERR, "Theta_final(i=%d,j=%d,k=%d) = %f\n", i, j, k, theta);
            theta_2 = 0.0;
         }/* Now, theta is smallest angle between the ith cp and all j other cp, at time m. */
   
         /* Compare location  of control point at time k to location of that control point at time k-1. */
         dp_m1 = mxvdp3(ControlCurve, 0, i, k-1);
         SUMA_ANGLE_DIST_NC((&(dp[0])), (&(dp_m1[0])), theta_2, nrm);
         if(LocalHead)  if(theta_2) fprintf(SUMA_STDERR, "Theta_k-1(i=%d,j=%d,k=%d) = %f\n", i, j, k, theta_2); 
         if(theta<opt->Zero) theta = theta_2;
         else if(theta_2<opt->Zero) theta = theta; 
         else if(theta_2 && theta) { 
            if(theta_2 < theta) theta = theta_2;
            else theta = theta;
         }
         nrm[0] = 0.0; nrm[1] = 0.0; nrm[2] = 0.0;        
         
         /* Compare location  of control point at time k to location of that control point at time k+1. */
         dp_m2 = mxvdp3(ControlCurve, 0, i, k+1);
         SUMA_ANGLE_DIST_NC((&(dp_m2[0])), (&(dp[0])), theta_2, nrm);
         if(LocalHead)  if(theta_2) fprintf(SUMA_STDERR, "Theta_k+1(i=%d,j=%d,k=%d) = %f\n", i, j, k, theta_2); 
         if(theta<opt->Zero) theta = theta_2;
         else if(theta_2<opt->Zero) theta = theta; 
         else if(theta_2 && theta) { 
            if(theta_2 < theta) theta = theta_2;
            else theta = theta;
         }            
         if(LocalHead) fprintf(SUMA_STDERR, "Min Theta (i=%d, k=%d) = %f\n", i, k, theta);
         
         /* This is the theta that I need to store!!  There's one for every i and m. The m's are the "knots", counted by index k.*/
         mxvd2(MaxStep, i, k) = theta;
         
         /* Directed along the curve. */
         dp_p1 = mxvdp4(Perturb_Vec, 0, i, k, 0);
         SUMA_MT_CROSS(p1, (&(nrm[0])), (&(dp[0])) );
         p1_mag = sqrt( SUMA_POW2(p1[0]) + SUMA_POW2(p1[1]) + SUMA_POW2(p1[2]) );
         if(p1_mag > opt->Zero) { 
            p1[0] *= (1.0/p1_mag);
            p1[1] *= (1.0/p1_mag);
            p1[2] *= (1.0/p1_mag);
         }
         if(LocalHead) fprintf( SUMA_STDERR, "Direction along curve - checking if unit vec. %f %f %f\n", p1[0], p1[1], p1[2]);
         theta_fac = (1.0/8.0)*theta;  /* This factor only affects the perturbation vector's magnitude */
         dp_p1[0] = theta_fac*p1[0];
         dp_p1[1] = theta_fac*p1[1];
         dp_p1[2] = theta_fac*p1[2];
         p1[0] = 0.0; p1[1] = 0.0; p1[2] = 0.0; 
         if(LocalHead) {
            fprintf( SUMA_STDERR, "Direction along curve - complete vector. %f %f %f\n", dp_p1[0], dp_p1[1], dp_p1[2]);    
            fprintf(SUMA_STDERR, "Perturb_Vec_along_curve(i=%d, k=%d, p=0) = %f %f %f\n", i, k, 
                              mxvd4(Perturb_Vec, 0, i, k, 0), mxvd4(Perturb_Vec, 1, i, k, 0), mxvd4(Perturb_Vec, 2, i, k, 0) );
         }
         
         /* Directed transverse to the curve. */
         dp_p2 = mxvdp4(Perturb_Vec, 0, i, k, 1);
         nrm_mag = sqrt( SUMA_POW2(nrm[0]) + SUMA_POW2(nrm[1]) + SUMA_POW2(nrm[2]) );
         if(nrm_mag > opt->Zero) { 
            nrm[0] *= (1.0/nrm_mag);
            nrm[1] *= (1.0/nrm_mag);
            nrm[2] *= (1.0/nrm_mag);
         }
         if(LocalHead) fprintf( SUMA_STDERR, "Direction perpendicular to curve - checking if unit vec. %f %f %f\n", nrm[0], nrm[1], nrm[2]);
         dp_p2[0] = theta_fac*nrm[0];
         dp_p2[1] = theta_fac*nrm[1];
         dp_p2[2] = theta_fac*nrm[2];
         
         /* Check perturbation vectors.  These vectors should be perpendicular. */
         if(LocalHead) fprintf( SUMA_STDERR, "Dot product of the two perturbation vectors = %g\n", SUMA_MT_DOT((&(dp_p1[0])), (&(dp_p2[0]))) );
         
         dp = dp2 = dp_m1 = dp_m2 = dp_p1 = dp_p2 = NULL;
         theta = theta_2 = 0.0;
         nrm[0] = 0.0; nrm[1] = 0.0; nrm[2] = 0.0;        
         nrm_mag = p1_mag = 0.0;
      }  
   }
  
   if (ps->cs->talk_suma) {
      SUMA_SegmentDO *sdo=NULL;
      NI_group *ngr = NULL;
      int suc;
      /* Send  Perturbation vector to SUMA */
      sdo = SUMA_Perturbation2SDO(opt->ControlCurve, Perturb_Vec, "PV0", 0, 5.0);
      /* change that thing to NIML */
      ngr = SUMA_SDO2niSDO(sdo);
      #if 0
      /* write it to diks, for kicks */
      sprintf(stmp, "file:PV0_%d.niml.SDO", opt->iter_count);
      NEL_WRITE_TX(ngr, stmp, suc);
      #endif
      /* send it to suma */
      if (!SUMA_SendToSuma (opt->SO, ps->cs, (void *)ngr, SUMA_SEGMENT_OBJECT, 1)) {
         SUMA_S_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         ps->cs->talk_suma = NOPE;
      }
      SUMA_free_SegmentDO(sdo); sdo = NULL;
      NI_free(ngr); ngr = NULL; 
      /* again for direction 2 */
      sdo = SUMA_Perturbation2SDO(opt->ControlCurve, Perturb_Vec, "PV1", 1, 5.0);
      /* change that thing to NIML */
      ngr = SUMA_SDO2niSDO(sdo);
      #if 0
      /* write it to diks, for kicks */
      sprintf(stmp, "file:PV1_%d.niml.SDO", opt->iter_count);
      NEL_WRITE_TX(ngr, stmp, suc);
      #endif
      /* send it to suma */
      if (!SUMA_SendToSuma (opt->SO, ps->cs, (void *)ngr, SUMA_SEGMENT_OBJECT, 1)) {
         SUMA_S_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         ps->cs->talk_suma = NOPE;
      }
      SUMA_free_SegmentDO(sdo); sdo = NULL;
      NI_free(ngr); ngr = NULL; 
      if (  opt->pause > 1) {
            SUMA_PAUSE_PROMPT("Pausing after perturbation directions\nDo something to proceed.\n");
      }
   }
   SUMA_RETURN(YUP);
}

SUMA_Boolean Print_Matrix( MyCircleOpt *opt, matrix M, FILE *fp ) /* Assumes # of rows and # of columns depend on # of control points. */
{
   static char FuncName[]={"Print_Matrix"};
   int r, c, r3, c3, k;
   FILE *fpl = NULL;  /* fp local */
   
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   if(fp) fpl = fp;
   else fpl = SUMA_STDERR;
   
   fprintf(fpl, "[\n");
   for (r=0; r<opt->N_ctrl_points; ++r) {
      for (k=0; k<3; ++k) {
         for(c=0; c<opt->N_ctrl_points; ++c) { 
            fprintf (fpl,"%11.8f   %11.8f   %11.8f   ", 
                                    M.elts [ (3*r+k) ][ (3*c  ) ],
                                    M.elts [ (3*r+k) ][ (3*c+1) ],
                                    M.elts [ (3*r+k) ][ (3*c+2) ]);
         }
         fprintf(fpl,"\n");
      }
   }
   fprintf(fpl, "];\n\n"); 
   
   fpl = NULL;
   SUMA_RETURN(YUP);
}

SUMA_Boolean Rotation_Matrix( MyCircleOpt *opt, vector X, matrix M )
{
   static char FuncName[]={"Rotation_Matrix"};
   int r, c, r3, c3, k;
   double Mcr[3][3], t_cr = 0.0, nrm_cr[3], expand_cr = 0.0; 
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   for(r=0; r<opt->N_ctrl_points; ++r) {        /* r for row. */
      for(c=0; c<opt->N_ctrl_points; ++c) {     /* c for column. */
         r3 = 3*r; c3 = 3*c;                                      
         
         t_cr = 0.0; expand_cr = 0.0; 
         /* Create the rotation matrix. */
         SUMA_3D_Rotation_Matrix( (&(X.elts[c3])), (&(X.elts[r3])), Mcr, t_cr, nrm_cr);

         expand_cr = Optimization_Kernel(opt, t_cr);
         
         /* Assemble the matrix and include the expansion factor. */
         M.elts[ (r3  ) ][ (c3  ) ] = expand_cr * Mcr[0][0];
         M.elts[ (r3  ) ][ (c3+1) ] = expand_cr * Mcr[0][1];
         M.elts[ (r3  ) ][ (c3+2) ] = expand_cr * Mcr[0][2];
         M.elts[ (r3+1) ][ (c3  ) ] = expand_cr * Mcr[1][0];
         M.elts[ (r3+1) ][ (c3+1) ] = expand_cr * Mcr[1][1];
         M.elts[ (r3+1) ][ (c3+2) ] = expand_cr * Mcr[1][2];
         M.elts[ (r3+2) ][ (c3  ) ] = expand_cr * Mcr[2][0];
         M.elts[ (r3+2) ][ (c3+1) ] = expand_cr * Mcr[2][1];
         M.elts[ (r3+2) ][ (c3+2) ] = expand_cr * Mcr[2][2];     
      }
   } 
   
   if(LocalHead) Print_Matrix(opt, M, NULL);
   
   SUMA_RETURN(YUP);
}

/* See Snip_2.c for Sm2SD0 function.  Function for plotting Sm, but Sm isn't what we want to plot. */

SUMA_Boolean Change_in_Energy(   MyCircleOpt *opt, SUMA_MX_VEC *ControlCurve, SUMA_MX_VEC *Perturb_Vec, 
                                 SUMA_MX_VEC *Del_S, FILE *condition_num, FILE *condition_num_only) 
{
   static char FuncName[]={"Change_in_Energy"};
   matrix Kern, Kern_p, delKern, KernI, delKernI, Xm_t, A, B;
   matrix *nullptr = NULL;
   vector Xm, Xm_mid, Xm_p, Pert, del_S1, del_S2, del_SF; 
   int nr, nc, i, i3, q, m, p, r, c, k, j, j3; 
   double mag_mid, mag_p, cond = 0.0;
   double *Cp_list = NULL, *dp = NULL, *dp_m1 = NULL, *dp_q = NULL;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
 
   if (opt->dbg_flag > 2) LocalHead = YUP;
   
   nr = 3 * opt->N_ctrl_points;  /* number of rows */
   nc = 3 * opt->N_ctrl_points;  /* number of columns */
  
   vector_initialize(&Xm);
   vector_create( (3*opt->N_ctrl_points), &Xm );
   vector_initialize(&Xm_mid);
   vector_create( (3*opt->N_ctrl_points), &Xm_mid );
   vector_initialize(&Xm_p);
   vector_create( (3*opt->N_ctrl_points), &Xm_p );
   vector_initialize(&Pert);
   vector_create( (3*opt->N_ctrl_points), &Pert );
   
   vector_initialize(&del_S1);
   vector_create(1, &del_S1);
   vector_initialize(&del_S2);
   vector_create(1, &del_S2);
   vector_initialize(&del_SF);
   vector_create(1, &del_SF);
   
   if(LocalHead) fprintf( SUMA_STDERR, "%s: Finished allocating matrices and vectors.\n", FuncName);
   
   for(p=0; p<2; ++p) {
      for(q=0; q<2; ++q) {
         for(m=0; m<opt->M_time_steps; ++m) {
            
            if(LocalHead) fprintf(SUMA_STDERR, "WHAT'S GOING ON? q = %d, p = %d, m = %d\n", q, p, m);
            
            matrix_initialize(&Xm_t);
            matrix_create(1, (3*opt->N_ctrl_points), &Xm_t); 
            
            /* Set vectors to zero, so can put in new values. */
            for(i=0; i<3*opt->N_ctrl_points; ++i) { 
               Xm.elts[i] = 0.0; 
               Xm_mid.elts[i] = 0.0; 
               Xm_p.elts[i] = 0.0; 
               Xm_t.elts[0][i] = 0.0; 
               Pert.elts[i] = 0.0;
            }
            
            if(LocalHead) fprintf(SUMA_STDERR, "Control Point Locations:\n" );
            for(i=0; i<opt->N_ctrl_points; ++i) {
               i3 = 3*i;
               dp = mxvdp3(ControlCurve, 0, i, m); 
               dp_m1 = mxvdp3(ControlCurve, 0, i, m+1);
               Xm_mid.elts[i3  ] = 0.5*(dp_m1[0] + dp[0]);
               Xm_mid.elts[i3+1] = 0.5*(dp_m1[1] + dp[1]);
               Xm_mid.elts[i3+2] = 0.5*(dp_m1[2] + dp[2]);
               
               /* Project onto the sphere. */
               mag_mid = sqrt( SUMA_POW2(Xm_mid.elts[i3  ]) + SUMA_POW2(Xm_mid.elts[i3+1]) + SUMA_POW2(Xm_mid.elts[i3+2]));
               if(mag_mid) {
                  Xm_mid.elts[i3  ] = Xm_mid.elts[i3  ]/mag_mid;
                  Xm_mid.elts[i3+1] = Xm_mid.elts[i3+1]/mag_mid;
                  Xm_mid.elts[i3+2] = Xm_mid.elts[i3+2]/mag_mid;
               }
             
               Xm.elts[i3  ] = (dp_m1[0] - dp[0]);
               Xm.elts[i3+1] = (dp_m1[1] - dp[1]);
               Xm.elts[i3+2] = (dp_m1[2] - dp[2]); 
                  
               /* Need transpose of Xm difference. This value is not projected to the sphere, I don't think. */
               /* Subtracting two locations does not give a location that makes sense here. I think this comes from 
                  the derivative estimation of the velocity. */
               Xm_t.elts[0][i3  ] = (dp_m1[0] - dp[0]);
               Xm_t.elts[0][i3+1] = (dp_m1[1] - dp[1]);
               Xm_t.elts[0][i3+2] = (dp_m1[2] - dp[2]); 
            
               if(LocalHead) {
                  fprintf(SUMA_STDERR, "dp = [%f; %f; %f];\n", dp[0], dp[1], dp[2]);
                  fprintf(SUMA_STDERR, "dp_m1 = [%f; %f; %f];\n", dp_m1[0], dp_m1[1], dp_m1[2]);
               }            
            }
         
            
            /* Need to perturb one point at a time. */
            for(i=0; i<opt->N_ctrl_points; ++i) { 
               
               if(LocalHead) fprintf(SUMA_STDERR, "BEGINNING OF LOOP, q=%d, p=%d, m=%d, i=%d\n", q, p, m, i);
               /* Create temporary matrices each time through loop. */
               matrix_initialize(&Kern);      
               matrix_create(nr, nc, &Kern);   
               matrix_initialize(&Kern_p);      /* p for perturbation */
               matrix_create(nr, nc, &Kern_p);   
               matrix_initialize(&delKern);     /* del for delta */
               matrix_create(nr, nc, &delKern);   
               matrix_initialize(&KernI);       /* I for inverse */
               matrix_create(nr, nc, &KernI);
               matrix_initialize(&delKernI);
               matrix_create(nr, nc, &delKernI);
            
            
               j3 = 3*j;
               i3 = 3*i;
               if(q == 0) dp_q = mxvdp4(Perturb_Vec, 0, i, m, p);      /* q=0 */
               if(q == 1) dp_q = mxvdp4(Perturb_Vec, 0, i, m+1, p);    /* q=1 */
               
               for(j=0; j<3*opt->N_ctrl_points; ++j) {Pert.elts[j] = 0.0;}
               /* Pert.elts is all zeros except for one perturbation vector.  Only three rows have values.  x-y-z */   
               Pert.elts[i3  ] = 0.5*dp_q[0];
               Pert.elts[i3+1] = 0.5*dp_q[1];   /* 0.5 used for calculations, will store perturbation vectors with out this scaler */
               Pert.elts[i3+2] = 0.5*dp_q[2];
               
               vector_add( Xm_mid, Pert, &Xm_p); /* Xm_p is the perturbed midpoint vector. */
               
               if(LocalHead) fprintf(SUMA_STDERR, "Perturbation Vector: [%f; %f; %f]\n", dp_q[0], dp_q[1], dp_q[2]);
               
               /* Project onto the sphere. */
               for(j=0; j<opt->N_ctrl_points; ++j) {
                  j3 = 3*j;  
                  mag_p = sqrt( SUMA_POW2(Xm_p.elts[j3  ]) + SUMA_POW2(Xm_p.elts[j3+1]) + SUMA_POW2(Xm_p.elts[j3+2]));
                  if(mag_p) {
                     Xm_p.elts[j3  ] = Xm_p.elts[j3  ]/mag_p;
                     Xm_p.elts[j3+1] = Xm_p.elts[j3+1]/mag_p;
                     Xm_p.elts[j3+2] = Xm_p.elts[j3+2]/mag_p;
                  }
               }
               
               Pert.elts[i3  ] = dp_q[0];   /* Need perturbation vector for later that is not scaled by 0.5 */
               Pert.elts[i3+1] = dp_q[1];
               Pert.elts[i3+2] = dp_q[2];
            
               if(LocalHead) {
                  fprintf(SUMA_STDERR, "Check outside of loop. m = %d\n", m);
                  for(j=0; j<opt->N_ctrl_points; ++j) {
                     j3 = 3*j;  
                     fprintf(SUMA_STDERR, "Xm_mid(%d) = [%f;   %f;    %f]\n", j, Xm_mid.elts[j3  ], Xm_mid.elts[j3+1], Xm_mid.elts[j3+2]);
                     fprintf(SUMA_STDERR, "Xm_p(%d) = [%f;   %f;    %f]\n", j, Xm_p.elts[i3  ], Xm_p.elts[j3+1], Xm_p.elts[i3+2]);
                     fprintf(SUMA_STDERR, "Pert(%d) = [%f;   %f;    %f]\n", j, Pert.elts[j3  ], Pert.elts[j3+1], Pert.elts[j3+2]);
                  }
               }
               
               if(LocalHead){
                  fprintf(SUMA_STDERR, "PERT.ELTS:\n");
                  for(j=0; j<3*opt->N_ctrl_points; ++j) {fprintf(SUMA_STDERR, "%f\n", Pert.elts[j]);}
               }

               Rotation_Matrix(opt, Xm_mid, Kern);
               
               /* This is the matrix whose condition number should be checked. */
               if(i<1 && q<1 && p<1) {
                  fprintf(condition_num, "Kern (m=%d) = ", m);
                  Print_Matrix(opt, Kern, condition_num);
                  cond = Matrix_Condition_Num( Kern, condition_num );
                  fprintf(condition_num, "%f\n", cond);
                  fprintf(condition_num_only, "%f\n", cond);
               }
               
               
               
               matrix_psinv(Kern, nullptr, &KernI);
               if(LocalHead) { 
                  fprintf(SUMA_STDERR, "\nKern = ");  Print_Matrix(opt, Kern, NULL); 
                  fprintf(SUMA_STDERR, "\nKernI = "); Print_Matrix(opt, KernI, NULL); 
               }
   
               Rotation_Matrix(opt, Xm_p, Kern_p);
               if(LocalHead) { fprintf(SUMA_STDERR, "\nKern_p = ");  Print_Matrix(opt, Kern_p, NULL); }

               /* Calculate delta K */
               matrix_subtract(Kern_p, Kern, &delKern);

               if(LocalHead) { fprintf(SUMA_STDERR, "\ndelKern = "); Print_Matrix(opt, delKern, NULL); }
               if(LocalHead) { fprintf(SUMA_STDERR, "\nKernI = "); Print_Matrix(opt, KernI, NULL); }

               /*****************************/
               matrix_initialize(&A);
               matrix_create(nr, nc, &A);
               matrix_initialize(&B);
               matrix_create(nr, nc, &B);
               
               /* Use K and delta K to calculate inverse delta K */
               matrix_multiply(KernI, delKern, &A); 
               matrix_multiply(A, KernI, &B);
               matrix_scale(-1.0, B, &delKernI);

               if(LocalHead) { fprintf(SUMA_STDERR, "\ndelKernI = "); Print_Matrix(opt, delKernI, NULL); }

               matrix_destroy(&A);
               matrix_destroy(&B);

               /****************************/
               /* Put it all together in the delta S equation. */        
               matrix_initialize(&A);
               matrix_create(1, (3*opt->N_ctrl_points), &A);
               matrix_initialize(&B);
               matrix_create(1, (3*opt->N_ctrl_points), &B);
            
               /* JUST CHANGED THIS.  11/07/06.  BEFORE HAD  q==1 ASSOCIATED WITH A -2 SCALER. */
               /* q describes which point perturbed.  If q = 0, then the m point is perturbed.  If q = 1, then the 
                     m+1 point is perturbed.  */
               /* MUST BE A +2 SCALER WHEN USING M+1 POINT FOR PERTURBATION. */
               if(q == 0) matrix_scale(-2.0, Xm_t, &A);
               if(q == 1) matrix_scale(+2.0, Xm_t, &A);
               
               matrix_multiply(A, KernI, &B);
               vector_multiply(B, Pert, &del_S1);

               if(LocalHead) {
                  fprintf(SUMA_STDERR, "Xm_t = [\n");
                  for(j=0; j<(3*opt->N_ctrl_points); ++j)  fprintf(SUMA_STDERR, "%f    ", (Xm_t.elts[0][j])); 
                  fprintf(SUMA_STDERR, "\n];\n");
                  fprintf(SUMA_STDERR, "del_S1 = %f\n", del_S1.elts[0]);
               }

               matrix_destroy(&A);
               matrix_destroy(&B);

               /****************************/
               matrix_initialize(&B);
               matrix_create(1, (3*opt->N_ctrl_points), &B);

               matrix_multiply(Xm_t, delKernI, &B);
               vector_multiply(B, Xm, &del_S2);

               vector_add(del_S1, del_S2, &del_SF);

               if(LocalHead) {
                  fprintf(SUMA_STDERR, "Xm = [\n");
                  for(j=0; j<(3*opt->N_ctrl_points); ++j)  fprintf(SUMA_STDERR, "%f;\n", (Xm.elts[j])); 
                  fprintf(SUMA_STDERR, "\n];\n");
                  fprintf(SUMA_STDERR, "del_S2 = %f\n", del_S2.elts[0]);
                  fprintf(SUMA_STDERR, "del_SF = %f\n", del_SF.elts[0]);
               }
               
               if(LocalHead) fprintf(SUMA_STDERR, "CHECK THE INDICIES!!!i = %d, m = %d, q = %d, p = %d\n", i, m, q, p);
               mxvd4(Del_S, i, m, q, p) = del_SF.elts[0]; 
               if(LocalHead) fprintf(SUMA_STDERR, "del_SF = %11.8f\n", del_SF.elts[0]);
               matrix_destroy(&B);

               /* Clean up */
               del_S1.elts[0] = 0.0;  del_S2.elts[0] = 0.0;  del_SF.elts[0] = 0.0;  
               
               matrix_destroy(&Kern);
               matrix_destroy(&KernI);
               matrix_destroy(&Kern_p);
               matrix_destroy(&delKern);
               matrix_destroy(&delKernI);
            }
               
            matrix_destroy(&Xm_t); 
         }   
      }
   }
   if(LocalHead) fprintf( SUMA_STDERR, "%s: Finished del_S loop.\n", FuncName);
   
   vector_destroy(&Xm);
   vector_destroy(&Xm_mid);
   vector_destroy(&Xm_p);
   vector_destroy(&Pert);
   vector_destroy(&del_S1);
   vector_destroy(&del_S2);
   vector_destroy(&del_SF);
   
   if(LocalHead) fprintf( SUMA_STDERR, "\n\n\n\n\n");
   
   SUMA_RETURN(YUP);
}

double S_energy( MyCircleOpt *opt, SUMA_MX_VEC *VecX , SUMA_GENERIC_ARGV_PARSE *ps)    /* Be very careful!  Order matters with index of VecX. */
{
   static char FuncName[]={"S_energy"};
   int j, i, m, i3;
   double *dp_m = NULL, *dp_m1 = NULL, S_grad = 0.0, Xm_mag;
   vector Xm, Xm_mid, Sm;
   matrix *nullptr = NULL;
   matrix Xm_t, Kern, KernI, A;

   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if(LocalHead) {
      fprintf(SUMA_STDERR, "%s: Control Point Locations:\n", FuncName);
      for(m=0; m<opt->M_time_steps; ++m) {   
         for(j=0; j<opt->N_ctrl_points; ++j) { 
            dp_m = mxvdp3(VecX, 0, j, m);
            fprintf(SUMA_STDERR, "%s:  m=%d, i=%d, [%f; %f; %f];\n", FuncName, m, j, dp_m[0], dp_m[1], dp_m[2]);
            dp_m = NULL;
         }
      }
   }
   
   for(m=0; m<opt->M_time_steps; ++m) {  
      vector_initialize(&Xm);
      vector_create((3*opt->N_ctrl_points), &Xm);  
      vector_initialize(&Xm_mid);
      vector_create((3*opt->N_ctrl_points), &Xm_mid); 
      matrix_initialize(&Xm_t);
      matrix_create(1, (3*opt->N_ctrl_points), &Xm_t); 
      matrix_initialize(&Kern);
      matrix_create((3*opt->N_ctrl_points), (3*opt->N_ctrl_points), &Kern); 
      matrix_initialize(&KernI);
      matrix_create((3*opt->N_ctrl_points), (3*opt->N_ctrl_points), &KernI); 
      matrix_initialize(&A);
      matrix_create(1,(3*opt->N_ctrl_points), &A); 
      vector_initialize(&Sm);
      vector_create(1, &Sm);  
   
      for(j=0; j<3*opt->N_ctrl_points; ++j) { 
         Xm.elts[j] = 0.0;
         Xm_mid.elts[j] = 0.0;
         Xm_t.elts[0][j] = 0.0;
      }
      
      for(i=0; i<opt->N_ctrl_points; ++i) { 
         i3 = 3*i;
         dp_m = mxvdp3(VecX, 0, i, m);
         dp_m1 = mxvdp3(VecX, 0, i, (m+1));
         
         Xm.elts[i3  ] = dp_m1[0] - dp_m[0];
         Xm.elts[i3+1] = dp_m1[1] - dp_m[1];
         Xm.elts[i3+2] = dp_m1[2] - dp_m[2];
         
         Xm_t.elts[0][i3  ] = dp_m1[0] - dp_m[0];     
         Xm_t.elts[0][i3+1] = dp_m1[1] - dp_m[1];
         Xm_t.elts[0][i3+2] = dp_m1[2] - dp_m[2];

      /* This Xm_mid must be projected to the sphere!!! */
         Xm_mid.elts[i3  ] = 0.5*(dp_m1[0] + dp_m[0]);
         Xm_mid.elts[i3+1] = 0.5*(dp_m1[1] + dp_m[1]);
         Xm_mid.elts[i3+2] = 0.5*(dp_m1[2] + dp_m[2]);
         
         Xm_mag = sqrt(SUMA_POW2(Xm_mid.elts[i3  ]) + SUMA_POW2(Xm_mid.elts[i3+1]) + SUMA_POW2(Xm_mid.elts[i3+2]) );
         if(Xm_mag) {
            Xm_mid.elts[i3  ] =  Xm_mid.elts[i3  ]/Xm_mag;
            Xm_mid.elts[i3+1] =  Xm_mid.elts[i3+1]/Xm_mag;
            Xm_mid.elts[i3+2] =  Xm_mid.elts[i3+2]/Xm_mag;
         }
      }
      
      if(LocalHead) {
         fprintf(SUMA_STDERR, "\n %s:\n", FuncName);
         fprintf(SUMA_STDERR, "Xm.elts = [\n");
         for(j=0; j<3*opt->N_ctrl_points; ++j) fprintf(SUMA_STDERR, "%f\n", Xm.elts[j]);
         fprintf(SUMA_STDERR, "]\n");
         fprintf(SUMA_STDERR, "Xm_mid.elts = [\n");
         for(j=0; j<3*opt->N_ctrl_points; ++j) fprintf(SUMA_STDERR, "%f\n", Xm_mid.elts[j]);
         fprintf(SUMA_STDERR, "]\n");
         fprintf(SUMA_STDERR, "Xm_t.elts = [\n");
         for(j=0; j<3*opt->N_ctrl_points; ++j) fprintf(SUMA_STDERR, "%f  ", Xm_t.elts[0][j]);
         fprintf(SUMA_STDERR, "]\n");
      }
               
      Rotation_Matrix( opt, Xm_mid, Kern );
      matrix_psinv(Kern, nullptr, &KernI);
      
      matrix_multiply(Xm_t, KernI, &A);
      vector_multiply(A, Xm, &Sm);
      
      #if 0
      if (ps->cs->talk_suma) {
         SUMA_SegmentDO *sdo=NULL;
         NI_group *ngr = NULL;
         int suc;
         /* Send  Perturbation vector to SUMA */
         sdo = SUMA_Sm2SDO(opt->ControlCurve, Sm, "Sm", 6.0);
         /* change that thing to NIML */
         ngr = SUMA_SDO2niSDO(sdo);
         #if 0
         /* write it to diks, for kicks */
         sprintf(stmp, "file:PV0_%d.niml.SDO", opt->iter_count);
         NEL_WRITE_TX(ngr, stmp, suc);
         #endif
         /* send it to suma */
         if (!SUMA_SendToSuma (opt->SO, ps->cs, (void *)ngr, SUMA_SEGMENT_OBJECT, 1)) {
            SUMA_S_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
            ps->cs->talk_suma = NOPE;
         }
         
         /* SUMA_free_SegmentDO(sdo); sdo = NULL;
         NI_free(ngr); ngr = NULL; 
         if (  opt->pause > 1) {
               SUMA_PAUSE_PROMPT("Pausing after Sm trial\nDo something to proceed.\n");
         }*/
      }
      #endif
      
      if(LocalHead) {
         fprintf(SUMA_STDERR, "\n %s:\n", FuncName);
         fprintf(SUMA_STDERR, "Kern = \n");
         Print_Matrix(opt, Kern, NULL);
         fprintf(SUMA_STDERR, "KernI = \n");
         Print_Matrix(opt, KernI, NULL);
         fprintf(SUMA_STDERR, "A = \n");
         matrix_print(A);
         fprintf(SUMA_STDERR, "Sm = %f\n", Sm.elts[0]); 
      }  
      
      S_grad = S_grad + Sm.elts[0];  
      
      if(LocalHead) fprintf(SUMA_STDERR, "%s: S_grad = %f\n", FuncName, S_grad);   
         
      vector_destroy(&Xm);
      matrix_destroy(&Xm_t);
      matrix_destroy(&Kern);
      matrix_destroy(&KernI);
      matrix_destroy(&A);
      vector_destroy(&Sm);
      vector_destroy(&Xm_mid);
   }
   
   SUMA_RETURN(S_grad);
}

SUMA_SegmentDO *SUMA_G2SDO(vector G, SUMA_MX_VEC *ControlCurve, 
                           char *Label, double scl)
{
   static char FuncName[]={"SUMA_G2SDO"};
   SUMA_SegmentDO *SDO=NULL;
   int N_n,  oriented,  NodeBased,  Stipple, i, m, d, kcc;
   char *idcode_str=NULL,  *Parent_idcode_str=NULL;
   float LineWidth;
   double *dp=NULL, *dp_new=NULL;
   int *NodeId=NULL;
   float *n0=NULL,  *n1=NULL;
   float *colv=NULL, *thickv=NULL;
   float acol[4], LineCol[4] = { 1.000000, 0.300000, 1.000000, 1.000000 };
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   oriented = 0;
   Stipple = 0;
   NodeBased = 0;
   LineWidth = 4;
   SUMA_NEW_ID(idcode_str, Label);

   N_n = ControlCurve->dims[1] * (ControlCurve->dims[2]-2);
   n0 = (float *) SUMA_malloc(sizeof(float)*N_n*3);
   n1 = (float *) SUMA_malloc(sizeof(float)*N_n*3);
   colv = (float *) SUMA_malloc(sizeof(float)*N_n*4);
   thickv = (float *) SUMA_malloc(sizeof(float)*N_n);

   
   for(i=0; i<ControlCurve->dims[1]; ++i) {  /* ControlCurve->dims[1] = 
                                                opt->N_ctrl_points */
      SUMA_a_good_col("ROI_i256", i+2, acol);/* get a decent colormap */  
               /* Add 2 to the color map index to avoid getting purple. */
      for (m=0; m<ControlCurve->dims[2]-2; ++m) {     /* ControlCurve->dims[2] = 
                                             opt->M_time_steps+1 */
         kcc = m*ControlCurve->dims[1]+i;
         if(1) {
            dp = mxvdp3(ControlCurve, 0, i, m+1);
            for (d=0;d<3;++d) {
               n0[3*kcc+d] = dp[d];
               n1[3*kcc+d] = dp[d]+scl * G.elts[3*kcc+d];
               if (LocalHead) 
                  fprintf( SUMA_STDERR,"%d/%d\n", 
                           3*kcc+d, 
                           3*ControlCurve->dims[1] * (ControlCurve->dims[2]-2));
            }
         }                             
         for (d=0;d<4;++d)   colv[4*kcc+d] = acol[d];
         thickv[kcc] = 3;   
         dp = NULL; dp_new = NULL;          
      }
   }

   SDO = SUMA_CreateSegmentDO( N_n, oriented, NodeBased, Stipple,
                               Label, idcode_str, Parent_idcode_str,
                               LineWidth, LineCol,
                               NodeId, NULL, n0, n1,
                               colv, thickv 
                              );

   if (idcode_str) SUMA_free(idcode_str); idcode_str = NULL;
   if (n0) SUMA_free(n0); n0 = NULL;
   if (n1) SUMA_free(n1); n1 = NULL;
   if (colv) SUMA_free(colv); colv = NULL;
   if (thickv) SUMA_free(thickv); thickv = NULL;
   SUMA_RETURN(SDO);
}

double Find_Lamda( MyCircleOpt *opt, SUMA_MX_VEC *ControlCurve, SUMA_MX_VEC *MaxStep, 
                  SUMA_MX_VEC *Perturb_Vec, SUMA_MX_VEC *Del_S, SUMA_MX_VEC *X_Lamda,
                  SUMA_GENERIC_ARGV_PARSE *ps)
{
   
   static char FuncName[]={"Find_Lamda"};
   int m, i, p, j, nr, nc, m3, m2, r, c, i3, repeat = 0, descent_small = 0;
   int C_N_dims = 3;
   int C_dims[10] = { opt->N_ctrl_points, (opt->M_time_steps - 1), 2};  /* time steps, p */
   double *dp_m0 = NULL, *dp_m1 = NULL, *dp = NULL, *dp_LG = NULL;
   double Sx, SxL, Lda = 0.0, mag_G, Theta_step, Theta_step_min, mag_LG;
   char stmp[500];
   vector Change, G;
   matrix E, Et, EtE, EtEI, R;
   matrix *nullptr = NULL;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (opt->dbg_flag > 2) LocalHead = YUP;
   
   vector_initialize(&Change);
   vector_create( (2*opt->N_ctrl_points*(opt->M_time_steps-1)), &Change );
   
   /* Calculate Values for C vector. */
   SUMA_MX_VEC *Change_S = NULL;
   Change_S = SUMA_NewMxVec( SUMA_double, C_N_dims, C_dims, 1 );
   
   for(p=0; p<2; ++p) {
      for(m=1; m<(opt->M_time_steps); ++m) {
         for(i=0; i<opt->N_ctrl_points; ++i) {
            dp_m0 = mxvdp4(Del_S, i, m, 0, p);
            dp_m1 = mxvdp4(Del_S, i, (m-1), 1, p);

            mxvd3(Change_S, i, m-1, p) = dp_m0[0] + dp_m1[0];
            
            if(LocalHead) { fprintf(SUMA_STDERR, "p=%d, m=%d, i=%d\n dp_m0 = %f, dp_m1 = %f\n C = %f\n", 
                                                   p, m, i, dp_m0[0], dp_m1[0], mxvd3(Change_S, i, m-1, p)); }
            dp_m0 = NULL; dp_m1 = NULL;
         }
      }  
   }
   if(LocalHead) fprintf(SUMA_STDERR, "Show Change_S:\n");
   
   if (opt->dbg_flag > 1) SUMA_ShowMxVec(Change_S, -1, NULL, "\nChange_S\n");
   
   /* Fill vector C with the values. Index in order, m, i, p*/
   /* BE CAREFUL, M=0 IS NOT THE ENDPOINT, IT IS ACTUALLY THE FIRST STEP WHICH WAS PREVIOUSLY CALLED M=1. */
   j=0;
   for(m=0; m<(opt->M_time_steps-1); ++m) {
      for(i=0; i<opt->N_ctrl_points; ++i) {
         for(p=0; p<2; ++p) {
            Change.elts[j] = mxvd3(Change_S, i, m, p);
            if(LocalHead) fprintf(SUMA_STDERR, "m=%d, i=%d, p=%d\n %f\n", m, i, p, Change.elts[j]); 
            ++j;
         }
      }
   }
   
   if(LocalHead) {
      fprintf(SUMA_STDERR, "Change:\n");
      for(j=0; j<(2*opt->N_ctrl_points*(opt->M_time_steps-1)); ++j) fprintf(SUMA_STDERR, "%f\n", Change.elts[j]);
   }

   /* Form E matrices that store the perturbation vectors. */
   nr = 3*opt->N_ctrl_points*(opt->M_time_steps-1);
               fprintf(SUMA_STDERR,"nr = %d\n", nr);
   nc = 2*opt->N_ctrl_points*(opt->M_time_steps-1);
   matrix_initialize(&E);
   matrix_create(nr, nc, &E);
   
   for(r=0; r<nr; ++r) { for(c=0; c<nc; ++c) { E.elts[r][c] = 0.00; } }
   
   for(m=0; m<(opt->M_time_steps-1); ++m) {
      for(i=0; i<opt->N_ctrl_points; ++i) {
         for(p=0; p<2; ++p) {
            i3 = 3*i;
            m3 = 3*opt->N_ctrl_points*m; 
            m2 = 2*opt->N_ctrl_points*m; 
            dp = mxvdp4(Perturb_Vec, 0, i, (m+1), p);
                        
            E.elts[i3 + m3    ] [p + 2*i + m2] = dp[0];
            E.elts[i3 + m3 + 1] [p + 2*i + m2] = dp[1];
            E.elts[i3 + m3 + 2] [p + 2*i + m2] = dp[2];
         
            dp = NULL;
         }
      }
   }
   
   if(LocalHead) { 
      fprintf(SUMA_STDERR, "E = [\n");
      matrix_print(E);
      fprintf(SUMA_STDERR, "];\n");
   }
   
   matrix_initialize(&Et);
   matrix_create(nr, nc, &Et);
   matrix_initialize(&EtE);
   matrix_create(nr, nc, &EtE);
   matrix_initialize(&EtEI);
   matrix_create(nr, nc, &EtEI);
   matrix_initialize(&R);
   matrix_create(nr, nc, &R);
   vector_initialize(&G);
   vector_create(nr, &G);
   
   matrix_transpose(E, &Et);
   matrix_multiply(Et, E, &EtE);
   matrix_psinv(EtE, nullptr, &EtEI);
   matrix_multiply(E, EtEI, &R);       /* A is temporary. */
   vector_multiply(R, Change, &G);     /* G for gradient. */
   
   if(LocalHead) {
      fprintf(SUMA_STDERR, "Et = [\n");
      matrix_print(Et);
      fprintf(SUMA_STDERR, "];\n");
      fprintf(SUMA_STDERR, "EtE = [\n");
      matrix_print(EtE);
      fprintf(SUMA_STDERR, "];\n");
      fprintf(SUMA_STDERR, "EtEI = [\n");
      matrix_print(EtEI);
      fprintf(SUMA_STDERR, "];\n");
      fprintf(SUMA_STDERR, "R = [\n");
      matrix_print(R);
      fprintf(SUMA_STDERR, "];\n"); 
   }
   
   if(LocalHead) {
      fprintf(SUMA_STDERR, "G = [\n");
      for(j=0; j<nr; ++j) fprintf(SUMA_STDERR, "%f\n", G.elts[j]);
      fprintf(SUMA_STDERR, "];\n");
   }
   
   /* Need to make G be just the direction vectors. Want unit vectors! */
   for(i=0; i< (opt->N_ctrl_points*(opt->M_time_steps-1)); ++i) {
      i3 = 3*i;
      mag_G = 0.0;
      mag_G = sqrt(  SUMA_POW2(G.elts[i3  ]) + 
                     SUMA_POW2(G.elts[i3+1]) + 
                     SUMA_POW2(G.elts[i3+2]) );             

      G.elts[i3  ] = G.elts[i3  ]/mag_G; 
      G.elts[i3+1] = G.elts[i3+1]/mag_G;
      G.elts[i3+2] = G.elts[i3+2]/mag_G;
   }
      
   if (ps->cs->talk_suma) {
      SUMA_SegmentDO *sdo=NULL;
      NI_group *ngr = NULL;
      int suc;
      /* Send G to SUMA */
      sdo = SUMA_G2SDO(G, ControlCurve, "G", -0.1);  /* Set scaler as negative so can visualize greatest decent. */
      /* change that thing to NIML */
      ngr = SUMA_SDO2niSDO(sdo);
      #if 0
      /* write it to diks, for kicks */
      sprintf(stmp, "file:G_%d.niml.SDO", opt->iter_count);
      NEL_WRITE_TX(ngr, stmp, suc);
      #endif
      /* send it to suma */
      if (!SUMA_SendToSuma (opt->SO, ps->cs, (void *)ngr, SUMA_SEGMENT_OBJECT, 1)) {
         SUMA_S_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
         ps->cs->talk_suma = NOPE;
      }
      SUMA_free_SegmentDO(sdo); sdo = NULL;
      NI_free(ngr); ngr = NULL; 
   }

   if(LocalHead){
      fprintf(SUMA_STDERR, "G = [\n");
      for(j=0; j<nr; ++j) fprintf(SUMA_STDERR, "%f\n", G.elts[j]);
      fprintf(SUMA_STDERR, "];\n");
   }
   
   /* Find energy of sphere using unadjusted path. */
   Sx = S_energy(opt, ControlCurve, ps);
   
   /* Find smallest "maximum allowable step" so know how big lamda can be. */
   if(opt->dbg_flag > 1) fprintf(SUMA_STDERR, "*************THETA_STEP_MIN:\n");
   Theta_step_min = mxvd2(MaxStep, 0, 1);
   if(LocalHead) fprintf(SUMA_STDERR, "%f\n", Theta_step_min);
   for(m=1; (m<opt->M_time_steps); ++m) {
      for(i=0; i<opt->N_ctrl_points; ++i) {
         
         Theta_step = mxvd2(MaxStep, i, m);
         if(LocalHead) fprintf(SUMA_STDERR, "%f\n", Theta_step);
         if(Theta_step < Theta_step_min) Theta_step_min = Theta_step;
      }
   }
   if(opt->dbg_flag > 1) fprintf(SUMA_STDERR, "Final Theta_step_min: %f\n", Theta_step_min);
   
   /* Set the smallest "max allowable movement", Theta_step_min as the starting point for Lda. */
   /* Now search for the largest Lda that gives lower energy than with an Lda of zero. */
   
   if(LocalHead) fprintf(SUMA_STDERR, "Final Theta_step_min: %f\n", Theta_step_min);
   
   Lda = Theta_step_min/1.0;
   
   do{
   
      /* Form list of path points with adjustment.  Perturbation magnitude, lamda, is what we are looking for. */
      for(m=0; (m<opt->M_time_steps+1); ++m) {
         for(i=0; i<opt->N_ctrl_points; ++i) {
            i3 = 3*i;
            m3 = 3*opt->N_ctrl_points*(m-1);  
            dp = NULL;
            dp = mxvdp3(ControlCurve, 0, i, m);

            if(m<1) {    
               mxvd3(X_Lamda, 0, i, m) = dp[0];
               mxvd3(X_Lamda, 1, i, m) = dp[1];
               mxvd3(X_Lamda, 2, i, m) = dp[2];  
            } else if(m == opt->M_time_steps){
               mxvd3(X_Lamda, 0, i, m) = dp[0];
               mxvd3(X_Lamda, 1, i, m) = dp[1];
               mxvd3(X_Lamda, 2, i, m) = dp[2];  
            } else {
               if(opt->dbg_flag > 1 &&  Lda == Theta_step_min) {
                  fprintf(SUMA_STDERR, "ControlCurve(%d, %d) = [%f %f %f];\n", m, i, dp[0], dp[1], dp[2]);
                  fprintf(SUMA_STDERR, "G.elts(%d, %d, %d) = [%f %f %f];\n", (i3 + m3), (i3 + m3 +1), (i3 + m3 +2), 
                                                   G.elts[i3 + m3    ], G.elts[i3 + m3 + 1], G.elts[i3 + m3 + 2]); 
               } 
               /* This must be projected on to the sphere! */ 
               mxvd3(X_Lamda, 0, i, m) = dp[0] - Lda*G.elts[i3 + m3    ];
               mxvd3(X_Lamda, 1, i, m) = dp[1] - Lda*G.elts[i3 + m3 + 1];
               mxvd3(X_Lamda, 2, i, m) = dp[2] - Lda*G.elts[i3 + m3 + 2]; 
               
               dp_LG = mxvdp3(X_Lamda, 0, i, m);
               mag_LG = sqrt(SUMA_POW2(dp_LG[0]) + SUMA_POW2(dp_LG[1]) + SUMA_POW2(dp_LG[2]));
               dp_LG[0] = dp_LG[0]/mag_LG;
               dp_LG[1] = dp_LG[1]/mag_LG;
               dp_LG[2] = dp_LG[2]/mag_LG;
               dp_LG = NULL;  
            }
            dp = NULL; 
         } 
      }
 
      /*if(LocalHead) {
         fprintf(SUMA_STDERR, "Show X_Lamda:\n");
         SUMA_ShowMxVec(X_Lamda, -1, NULL, "\nkkgjjg\n");
      }*/
      
      if(repeat<1) {
         if(opt->dbg_flag > 1) fprintf(SUMA_STDERR, "Lda before comparison: %f\n", Lda);
         /* Find energy of sphere with adjusted path. */
         SxL = 0.0;
         SxL = S_energy(opt, X_Lamda, ps);
         if( SxL < Sx ) repeat = 1; 
         if( SxL >= Sx ) { /* IMPROVEMENT_NOTE: Perhaps start from  a better Lda, looks like we have to go down often. */
            /* if(Lda>0.02) Lda = Lda - 0.005;
            if(Lda<=0.02) Lda = Lda - 0.0001; */
            Lda = Lda - Theta_step_min * 0.02;
            if(Lda<0.0001) { Lda = 0.0; repeat = 1; }
         } 
         if(opt->dbg_flag > 1) fprintf(SUMA_STDERR, "SxL = %.12f, Sx = %.12f, Lda = %f\n", SxL, Sx, Lda);
      }
   } while(repeat < 1); 

 
   Change_S = SUMA_FreeMxVec( Change_S );
   vector_destroy(&Change);
   matrix_destroy(&E);
   matrix_destroy(&Et);
   matrix_destroy(&EtE);
   matrix_destroy(&EtEI);
   matrix_destroy(&R);
   vector_destroy(&G);
   
   SUMA_RETURN(Lda);
}

/*********************************************** Begin Mesh walking functions here ************************************************/

SUMA_Boolean Debug_Weights( MyCircle *C, MyCircleOpt *opt, matrix M, matrix Mi, vector Vv) 
{
   static char FuncName[]={"Debug_Weights"};
   int i, i3, j, j3, r, k, c, idm;
 
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   FILE *output_matrix = NULL;  /*for sending the matrix to a file to be read by matlab. */
   output_matrix = fopen( "output_matrix.m", "w" );
   /* Print Matrix */
   if (opt->dot) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "M = [\n", FuncName);
      for (r=0; r<opt->N_ctrl_points; ++r) {
         for (k=0; k<(3+opt->N_ctrl_points); ++k) {
            for(c=0; c<opt->N_ctrl_points; ++c) { 
               fprintf (SUMA_STDERR,"%.8f   %.8f   %.8f   ", 
                                       M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c  ) ],
                                       M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+1) ],
                                       M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+2) ]);
            }
            fprintf (SUMA_STDERR,"\n");
         }
      }
      fprintf(SUMA_STDERR, "];\n"); 
   } else { 
      fprintf(SUMA_STDERR, "%s:\n"
                           "M = [\n", FuncName);
      for (r=0; r<opt->N_ctrl_points; ++r) {
         for (k=0; k<3; ++k) {
            for(c=0; c<opt->N_ctrl_points; ++c) { 
               fprintf (SUMA_STDERR,"%.5f   %.5f   %.5f   ", 
                                       M.elts [ (3*r+k) ][ (3*c  ) ],
                                       M.elts [ (3*r+k) ][ (3*c+1) ],
                                       M.elts [ (3*r+k) ][ (3*c+2) ]);
            }
            fprintf (SUMA_STDERR,"\n");
         }
      }
      fprintf(SUMA_STDERR, "];\n");
   }

   /* Write matrix to a file so can be read by matlab. */
   if (opt->dot) {
      fprintf(output_matrix, "M = [\n");
         for (r=0; r<opt->N_ctrl_points; ++r) {
            for (k=0; k<(3+opt->N_ctrl_points); ++k) {
               for (c=0; c<opt->N_ctrl_points; ++c) { 
                  fprintf (output_matrix,"%11.8f   %11.8f   %11.8f   ", 
                                          M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c  ) ],
                                          M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+1) ],
                                          M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+2) ]);
               }
               fprintf (output_matrix,"\n");
            } }
      fprintf(output_matrix, "]; \n \n"); 
   } else { 
      fprintf(output_matrix,  "%s:\n"
                              "M = [\n", FuncName);
         for (r=0; r<opt->N_ctrl_points; ++r) {
            for (k=0; k<3; ++k) {
               for(c=0; c<opt->N_ctrl_points; ++c) { 
                  fprintf (output_matrix,"%11.8f   %11.8f   %11.8f   ", 
                                          M.elts [ (3*r+k) ][ (3*c  ) ],
                                          M.elts [ (3*r+k) ][ (3*c+1) ],
                                          M.elts [ (3*r+k) ][ (3*c+2) ]);
               }
               fprintf (output_matrix,"\n");
            } }
      fprintf(output_matrix, "]; \n \n");  
   } 
  
   
   /*Print the Inverse Coefficient Matrix.*/
   if(opt->dot) {   
      fprintf(SUMA_STDERR, "%s:\n"  "Mi = [\n", FuncName);
      for (r=0; r<opt->N_ctrl_points; ++r) {
         for (k=0; k<3; ++k) {
            for(c=0; c<opt->N_ctrl_points; ++c) { 
               fprintf (SUMA_STDERR,"%.5f   %.5f   %.5f   %.5f ", 
                                       Mi.elts [ (3*r+k) ][ (4*c  ) ],
                                       Mi.elts [ (3*r+k) ][ (4*c+1) ],
                                       Mi.elts [ (3*r+k) ][ (4*c+2) ],
                                       Mi.elts [ (3*r+k) ][ (4*c+3) ]);
            }
            fprintf (SUMA_STDERR,"\n");
         }
      }
      fprintf(SUMA_STDERR, "];\n");
   } else {
      fprintf(SUMA_STDERR, "%s:\n"  "Mi = [\n", FuncName);
      for (r=0; r<opt->N_ctrl_points; ++r) {
         for (k=0; k<3; ++k) {
            for(c=0; c<opt->N_ctrl_points; ++c) { 
               fprintf (SUMA_STDERR,"%.5f   %.5f   %.5f   ", 
                                       Mi.elts [ (3*r+k) ][ (3*c  ) ],
                                       Mi.elts [ (3*r+k) ][ (3*c+1) ],
                                       Mi.elts [ (3*r+k) ][ (3*c+2) ]);
            }
            fprintf (SUMA_STDERR,"\n");
         }
      }
      fprintf(SUMA_STDERR, "];\n");
   }

   /* Print velocity vectors. */
   fprintf(SUMA_STDERR, "%s:\n"
                        "V = [ \n", FuncName );
   for (i=0; i < opt->N_ctrl_points; ++i) {
      if(opt->dot) { 
         idm = (opt->dim+opt->N_ctrl_points)*i; 
         fprintf(SUMA_STDERR, "  %10.8f;  %10.8f;  %10.8f; ",
                              Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2] ); 
         for(k=0; k < opt->N_ctrl_points; ++k) { fprintf(SUMA_STDERR, "  0.0000;" ); } 
         fprintf(SUMA_STDERR, "\n" );  
      } else { 
         idm = opt->dim*i; 
         fprintf(SUMA_STDERR, "  %10.8f;  %10.8f;  %10.8f \n",
                              Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2]); 
      }
   }
   fprintf(SUMA_STDERR, "];\n \n" );

   /* Send velocity vector to output file for checking math in matlab. */
   fprintf(output_matrix, "V = [ \n" );
   for (i=0; i < opt->N_ctrl_points; ++i) {
      if(opt->dot) { 
         idm = (opt->dim+opt->N_ctrl_points)*i; 
         fprintf(output_matrix, "  %11.20f;  %11.20f;  %11.20f; ",
                              Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2] ); 
         for(k=0; k < opt->N_ctrl_points; ++k) { fprintf(output_matrix, "  0.0000;" ); } 
         fprintf(output_matrix, "\n" );  
      } else { 
         idm = opt->dim*i; 
         fprintf(output_matrix, "  %11.20f;  %11.20f;  %11.20f \n",
                              Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2]); 
      }
   }
   fprintf(output_matrix, "];\n \n" );

   /* Print Weights. */
   fprintf(SUMA_STDERR, "%s:\n"
                        "Wv = [\n", FuncName);
   for (r=0; r<opt->N_ctrl_points; ++r) {
      if (opt->dim == 3) {
         fprintf (SUMA_STDERR,"%.20f;   %.20f;   %.20f;  \n",
                               C->Wv.elts[3*r], C->Wv.elts[3*r+1], C->Wv.elts[3*r+2]);   
      }else {
          fprintf (SUMA_STDERR,"%.20f;   %.20f;     \n", C->Wv.elts[2*r], C->Wv.elts[2*r+1]);   
      }
   }
   fprintf (SUMA_STDERR,"];\n");
   for (i=0; i<opt->N_ctrl_points; ++i) {
      fprintf (SUMA_STDERR,"Dot_Wv (%d) = %.24f \n"
                           "  W(%d) =  [%f;   %f;    %f]\n"
                           "  Control Point(%d) = [%f;   %f;    %f]\n", 
                              i, SUMA_MT_DOT( (&(opt->CtrlPts[3*i])), (&(C->Wv.elts[3*i])) ),
                              i, C->Wv.elts[3*i], C->Wv.elts[3*i+1], C->Wv.elts[3*i+2],
                              i, opt->CtrlPts[3*i], opt->CtrlPts[3*i+1],  opt->CtrlPts[3*i+2]); 
   }

   /* Also send weights to output file for use in matlab. */
   fprintf(output_matrix, "Wv = [\n" );
   for (r=0; r<opt->N_ctrl_points; ++r) {
      if (opt->dim == 3) {
         fprintf (output_matrix,"  %11.20f;   %11.20f;   %11.20f;  \n",
                               C->Wv.elts[3*r], C->Wv.elts[3*r+1], C->Wv.elts[3*r+2]);   
      }else {
          fprintf (output_matrix,"  %11.20f;   %11.20f;  \n", C->Wv.elts[2*r], C->Wv.elts[2*r+1]);   
      }
   }
   fprintf (output_matrix,"];\n \n");
   
   fclose (output_matrix); output_matrix = NULL;
  
   SUMA_RETURN(YUP);
}


SUMA_Boolean FindSplineWeights (MyCircle *C, MyCircleOpt *opt, FILE *condition_num, FILE *condition_num_only)
{
   static char FuncName[]={"FindSplineWeights"};
   vector Vv;
   static matrix M, Mi;
   double Mcr[3][3], nrm_cr[3] = {0.0, 0.0, 0.0};
   double check_a, check_cr[3] = {0.0, 0.0, 0.0}, tan_v_mag;
   int i, idm=0.0, k, r, c, nr, nc, i3, c3, r3, r4, f, row;
   double V_row1[3], V_row2[3], V_row3[3], nrm[3]={0.0,0.0,0.0};
   double  *t=NULL;
   double t_rc = 0.0, nrm_rc[3]={0.0,0.0,0.0};
   double tan_v[3]={0.0,0.0,0.0};  /* Meaning tangent velocity vector. Stores cross product when calculating given velocity. */
   double Vv_mag = 0.0, t_cr, expand_cr = 0.0; 
   matrix *nullptr = NULL;
   double I[3][3], sI[3][3], cond;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      fprintf(SUMA_STDERR, "%s:\n"
                           "Number of control points: %d\n", 
                     	    FuncName, opt->N_ctrl_points);
   }
   
   if (opt->dim !=2 && opt->dim != 3) {
      SUMA_S_Err("Stupid");
      SUMA_RETURN(NOPE);
   }
   
   t = (double *)SUMA_calloc(opt->N_ctrl_points, sizeof(double)); 
   
   if (opt->Dtheta) {
      SUMA_S_Err("Expecting no Dtheta yet!");
      SUMA_RETURN(NOPE);
   } else {
      opt->Dtheta = (double *)SUMA_calloc(opt->N_ctrl_points , sizeof (double));  /* angular displacement between t = 0 and t = 1 for each node.
                                                                                     One does not need to store for the algorithm but it makes
                                                                                     debugging easier. */
   }
   
   opt->Nrm = (double *)SUMA_calloc(opt->N_ctrl_points * 3 , sizeof (double));

   /* CALCULATE GIVEN VELOCITY AT CONTROL POINTS USING DESIRED ANGLE AND AXIS OF ROTATION. */
   vector_initialize (&Vv);
   if (opt->dot) { vector_create ((opt->dim*opt->N_ctrl_points + opt->N_ctrl_points*opt->N_ctrl_points), &Vv); }
      else { vector_create (opt->dim*opt->N_ctrl_points, &Vv); }
   
   for (i=0; i < opt->N_ctrl_points; ++i) {
      if(opt->dot) { idm = (opt->dim + opt->N_ctrl_points)*i; }
         else { idm = opt->dim*i; }
      i3 = 3*i;
      /* Distance between control point and desired destination of control point */ 
      /* nrm is the axis of rotation from intial control point location to final. */       
      /* BEWARE: USING _NC HERE, MEANING NO CENTER.  WILL THE CENTER ALWAYS BE THE ORIGIN? */
      SUMA_ANGLE_DIST_NC((&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts[i3])), opt->Dtheta[i], (&(opt->Nrm[i3])) );

      if (LocalHead) {
         fprintf(SUMA_STDERR, "%s: Point %d, Dtheta = %.12f rad (%.12f deg)\n",
                                 FuncName, i, opt->Dtheta[i], SUMA_R2D(opt->Dtheta[i]));                            
      }

      SUMA_MT_CROSS(tan_v, (&(opt->Nrm[i3])), (&(opt->CtrlPts[i3])) );
      
      /* Need to normalize because using the direction (unit vector) of this tangent vector in the velocity calculation. */
      tan_v_mag = sqrt( tan_v[0]*tan_v[0] + tan_v[1]*tan_v[1] + tan_v[2]*tan_v[2] );
      if ( tan_v_mag > opt->Zero) {
         tan_v[0] = tan_v[0]/tan_v_mag;
         tan_v[1] = tan_v[1]/tan_v_mag;
         tan_v[2] = tan_v[2]/tan_v_mag; }
         
      /*if (LocalHead) {
         fprintf(SUMA_STDERR, "  TAN_V normalized = [ %.12f;    %.12f;    %.12f ]\n"
                              "  Dot product( tan_v, control point) = %.12f\n"
                              "  Dtheta = %f\n", 
                              tan_v[0], tan_v[1], tan_v[2],
                              SUMA_MT_DOT(tan_v, (&(opt->CtrlPts[i3])) ),
                              opt->Dtheta[i] ); } */
      
      Vv.elts[idm  ] = opt->Dtheta[i]*tan_v[0];
      Vv.elts[idm+1] = opt->Dtheta[i]*tan_v[1];
         if (opt->dim > 2) { Vv.elts[idm+2] = opt->Dtheta[i]*tan_v[2]; }
      
      /* Dot Product Restriction.  Since want tangent velocity contributions, need dot(radius, weight) = 0. */
      /* Need row with a zero for each control point. */
      if( opt->dot ) { 
         for(k=3; k < (opt->dim + opt->N_ctrl_points); ++k ) { Vv.elts[idm+k] = 0.00000; } }  
   }
   
   if( LocalHead ) {
      fprintf(SUMA_STDERR, "Given Velocity Vectors for control points:\n");
      for (i=0; i < opt->N_ctrl_points; ++i) {
         if (opt->dim == 2) {
            fprintf(SUMA_STDERR, "  %.18f    %.18f \n",
                                 Vv.elts[idm  ], Vv.elts[idm+1]); 
         } else {
            if(opt->dot ) { 
               idm = (opt->dim + opt->N_ctrl_points)*i; 
               fprintf(SUMA_STDERR, "  V(%d) = [ %f;   %f;   %f;    %f;    %f ] \n",
                                    i, Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2], Vv.elts[idm+3], Vv.elts[idm+4] );   
            } else { 
               idm = opt->dim*i; 
               fprintf(SUMA_STDERR, "  V(%d) = [ %f;   %f;   %f ] \n",
                                    i, Vv.elts[idm  ], Vv.elts[idm+1], Vv.elts[idm+2] ); 
            }
         }
      }
   }
   
   if( LocalHead ) {
      fprintf(SUMA_STDERR, "Dot product with radius:\n");
      for (i=0; i < opt->N_ctrl_points; ++i) {
         if(opt->dot ) { idm = (opt->dim + opt->N_ctrl_points)*i; }
         else { idm = opt->dim*i; } 
         fprintf(SUMA_STDERR, "  Dot(%d) = %.12f\n", i, SUMA_MT_DOT( (&(Vv.elts[idm])), (&(C->NewNodeList[ 3*opt->CtrlPts_iim[i]]))) );
      }
   }
 
   if( opt->dot ) { nr = opt->N_ctrl_points * (opt->dim + opt->N_ctrl_points); } /* Need extra rows for dot product. */
      else { nr = opt->dim * opt->N_ctrl_points; } /* number of rows */
   nc = opt->dim * opt->N_ctrl_points; /* number of columns */
   matrix_initialize(&M);
   matrix_create(nr, nc, &M);
   
   if (!M.elts) {
      SUMA_S_Crit("Failed to allocate");
      exit(1);
   }
   
   if( !opt->dot ) {
      for(r=0; r<opt->N_ctrl_points; ++r) {        /* r for row. For the first three rows, r represents the first control point. */
            for(c=0; c<opt->N_ctrl_points; ++c) {  /* c for column. While r is held constant, c cycles through all control points
                                                      so that for the first 3 rows (first 3 C.P.) are compared to all the rest.  */
               c3 = 3*c; r3 = 3*r;
               
               t_cr = 0.0; expand_cr = 0.0;
               /* Create the rotation matrix. */
               SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts[c3])), (&(opt->CtrlPts[r3])), Mcr, t_cr, nrm_cr);
          
               expand_cr = Deformation_Kernel(opt, t_cr);
               
               /* Assemble the matrix and include the expansion factor. */
               M.elts[ (r3  ) ][ (c3  ) ] = expand_cr * Mcr[0][0];
               M.elts[ (r3  ) ][ (c3+1) ] = expand_cr * Mcr[0][1];
               M.elts[ (r3  ) ][ (c3+2) ] = expand_cr * Mcr[0][2];
               M.elts[ (r3+1) ][ (c3  ) ] = expand_cr * Mcr[1][0];
               M.elts[ (r3+1) ][ (c3+1) ] = expand_cr * Mcr[1][1];
               M.elts[ (r3+1) ][ (c3+2) ] = expand_cr * Mcr[1][2];
               M.elts[ (r3+2) ][ (c3  ) ] = expand_cr * Mcr[2][0];
               M.elts[ (r3+2) ][ (c3+1) ] = expand_cr * Mcr[2][1];
               M.elts[ (r3+2) ][ (c3+2) ] = expand_cr * Mcr[2][2]; 
         }
      }
   }

   if( opt->dot ) {
      for(r=0; r<opt->N_ctrl_points; ++r) {     /* r for row.  For the first three rows, r represents the first control point.*/
         for(c=0; c<opt->N_ctrl_points; ++c) {  /* c for column. While r is held constant, c cycles through all control points
                                                   so that for the first 3 rows (first 3 C.P.) are compared to all the rest.  */
            row = (3 + opt->N_ctrl_points)*r;   /* Need extra rows for dot product restriction. */
            c3 = 3*c; r3 = 3*r;
            
            t_cr = 0.0; expand_cr = 0.0;
            /* Create the rotation matrix. */
            SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts[c3])), (&(opt->CtrlPts[r3])), Mcr, t_cr, nrm_cr );
            
            expand_cr = Deformation_Kernel(opt, t_cr);
            
            /* Assemble the matrix and include the expansion factor. */
            #if 0
            if(LocalHead) {
               if( r == 0 ) {
                  fprintf(SUMA_STDERR, "ROTATION MATRIX: \n"
                                       "  p1 = [ %f      %f    %f ] \n"
                                       "  p2 = [ %f      %f    %f ] \n"
                                       "     alpha(%d, %d) = %.12f  \n"
                                       "     u (%d, %d) = [ %.12f    %.12f    %.12f ] \n"
                                       "     expansion = %f \n",
                                       opt->CtrlPts[c3  ], opt->CtrlPts[c3+1], opt->CtrlPts[c3+2],
                                       opt->CtrlPts[r3  ], opt->CtrlPts[r3+1], opt->CtrlPts[r3+2],                                
                                       r, c, t_cr, 
                                       r, c, nrm_cr[0], nrm_cr[1], nrm_cr[2],
                                       expand_cr ); } }
            #endif
            
            M.elts[ (row  ) ][ (c3  ) ] = expand_cr * Mcr[0][0];
            M.elts[ (row  ) ][ (c3+1) ] = expand_cr * Mcr[0][1];
            M.elts[ (row  ) ][ (c3+2) ] = expand_cr * Mcr[0][2];
            M.elts[ (row+1) ][ (c3  ) ] = expand_cr * Mcr[1][0];
            M.elts[ (row+1) ][ (c3+1) ] = expand_cr * Mcr[1][1];
            M.elts[ (row+1) ][ (c3+2) ] = expand_cr * Mcr[1][2];
            M.elts[ (row+2) ][ (c3  ) ] = expand_cr * Mcr[2][0];
            M.elts[ (row+2) ][ (c3+1) ] = expand_cr * Mcr[2][1];
            M.elts[ (row+2) ][ (c3+2) ] = expand_cr * Mcr[2][2]; 
            
            for(k=3; k < (3 + opt->N_ctrl_points); ++k) {
               if( k-3 == c && k-c == 3) {
                  M.elts[ (row+k) ][ (c3  ) ] = (opt->CtrlPts[r3  ] * (expand_cr * Mcr[0][0]) )
                                              + (opt->CtrlPts[r3+1] * (expand_cr * Mcr[1][0]) )
                                              + (opt->CtrlPts[r3+2] * (expand_cr * Mcr[2][0]) ); 
                  M.elts[ (row+k) ][ (c3+1) ] = (opt->CtrlPts[r3  ] * (expand_cr * Mcr[0][1]) )
                                              + (opt->CtrlPts[r3+1] * (expand_cr * Mcr[1][1]) )
                                              + (opt->CtrlPts[r3+2] * (expand_cr * Mcr[2][1]) );
                  M.elts[ (row+k) ][ (c3+2) ] = (opt->CtrlPts[r3  ] * (expand_cr * Mcr[0][2]) )
                                              + (opt->CtrlPts[r3+1] * (expand_cr * Mcr[1][2]) )
                                              + (opt->CtrlPts[r3+2] * (expand_cr * Mcr[2][2]) ); } 
               else {
                  M.elts[ (row+k) ][ (c3  ) ] = 0.0;
                  M.elts[ (row+k) ][ (c3+1) ] = 0.0;
                  M.elts[ (row+k) ][ (c3+2) ] = 0.0; }
            }   
         }
      }  
   }
   
   /* Check condition number for matrix M.  Send to file for plotting in Matlab. */
   fprintf(condition_num, "M = ");
   Print_Matrix(opt, M, condition_num);
   cond = Matrix_Condition_Num( M, condition_num );
   fprintf(condition_num, "%f\n", cond);
   fprintf(condition_num_only, "%f\n", cond);
   
   SUMA_LH("Calculating inverse...");
   matrix_initialize(&Mi);
   matrix_create(nr, nc, &Mi);
   matrix_psinv (M, nullptr, &Mi); 
   SUMA_LH("   Done."); 

   /* if( LocalHead ) Debug_Weights(C, opt, M, Mi, Vv); */
   if( LocalHead) fprintf(SUMA_STDERR, "%s: Done printing inverse matrix.\n", FuncName);

   SUMA_LH("Calculating weights...\n");
   vector_initialize(&(C->Wv));
   vector_multiply(Mi, Vv, &(C->Wv));
   SUMA_LH("   Done."); 
   
   if( LocalHead ) Debug_Weights(C, opt, M, Mi, Vv);
   
   if (t) SUMA_free(t); t=NULL;
   matrix_destroy (&M);
   matrix_destroy (&Mi);
   vector_destroy (&Vv);
   
   opt->Dtheta = NULL;
   
   SUMA_RETURN(YUP);   
}

SUMA_Boolean Velocity( MyCircle *C, MyCircleOpt *opt) 
{
   static char FuncName[]={"Velocity"};
   char outfile_test_expansion[50];
   byte repeat;   
   int i, i3, j, j3, r;
   double v_alpha = 0.0, v_cr[3]={0.0,0.0,0.0}, v_expand, cr_mag; 
   vector Wr;  /*W rotated*/
   double v_M[3][3];
 
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* Compute velocity at each node using contributions from the control points. */
   vector_initialize (&Wr);
   vector_create (3*opt->N_ctrl_points, &Wr);
   
   if (LocalHead) { 
      fprintf(SUMA_STDERR, "******************************************************\n"
                           "%s: CHECKING THE FUNCTION, First control point: \n", FuncName); }
   
   FILE *test_expansion = NULL;
   sprintf(outfile_test_expansion, "test_expansion_factor_0.1D");
   test_expansion = fopen (outfile_test_expansion, "w"); 

   for (i=0; i< C->N_Node; ++i) {  /* i for all points, j for the control points. */
      i3 = 3*i;
      
      for (j=0; j<opt->N_ctrl_points; ++j){  
         j3 = 3*j;
        
         #if 0
         /* Need to comment out when using SUMA_3D_Rotation_Matrix to calculate initial velocity field. */
         SUMA_ANGLE_DIST_NC( (&(C->NewNodeList[i3])), (&(opt->CtrlPts[j3])), v_alpha, v_cr );
        
         cr_mag = sqrt( v_cr[0]*v_cr[0] + v_cr[1]*v_cr[1] + v_cr[2]*v_cr[2]); 
         if (LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) { fprintf(SUMA_STDERR, "Magnitude of Cross Product = %.12f \n", cr_mag); }} 
   
         if(cr_mag > opt->Zero){ v_cr[0] = v_cr[0]/cr_mag; v_cr[1] = v_cr[1]/cr_mag; v_cr[2] = v_cr[2]/cr_mag; }  
         
         if (LocalHead) {
            if (i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "Using SUMA_ROTATE_ABOUT AXIS to calculate velocity field.\n" ); } }
         
         v_expand = 0.0; v_alpha = 0.0; 
         /* ROTATE THE WEIGHTS USING DISPLACEMENT ANGLE AND AXIS OF ROTATION CALCULATED ABOVE. */
         SUMA_ROTATE_ABOUT_AXIS( (&(C->Wv.elts[j3])), v_cr, v_alpha, (&(Wr.elts[j3])) );       
         v_expand = Deformation_Kernel(opt, v_alpha);
         #endif
        
         v_alpha = 0.0; v_expand = 0.0;
         SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts[j3])), (&(C->NewNodeList[i3])), v_M, v_alpha, v_cr);
         v_expand = Deformation_Kernel(opt, v_alpha);
         
         if( j==0 ) fprintf(test_expansion, "%d %f  %f\n", i, v_alpha, v_expand);
           
         if(LocalHead) { 
            if (i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "AFTER ROTATION MATRIX CALCULATED: \n"
                                    "  v_alpha = %f\n"
                                    "  expansion = %.5f\n"
                                    "  p1 = [ %f     %f    %f ]; \n"
                                    "  p2 = [ %f     %f    %f ]; \n"
                                    "  Angle to be rotated = %f \n"
                                    "  Axis of rotation: \n"
                                    "     %.12f    %.12f    %.12f \n"
                                    "  Weight = %f %f %f\n", 
                                    v_alpha, v_expand,
                                    opt->CtrlPts[j3  ], opt->CtrlPts[j3+1], opt->CtrlPts[j3+2],
                                    C->NewNodeList[i3  ], C->NewNodeList[i3+1], C->NewNodeList[i3+2],
                                    v_alpha, 
                                    v_cr[0], v_cr[1], v_cr[2],
                                    C->Wv.elts[j3], C->Wv.elts[j3+1], C->Wv.elts[j3+2]); 
            }
            if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "%s:\n"
                                    "v_M = [\n", FuncName);
               for (r=0; r<3; ++r) {  
                     fprintf (SUMA_STDERR,"  %.5f   %.5f   %.5f \n", 
                                             v_expand*v_M [ (r) ][ (0) ],
                                             v_expand*v_M [ (r) ][ (1) ],
                                             v_expand*v_M [ (r) ][ (2) ]);
               }
               fprintf(SUMA_STDERR, "];\n");
            }
         }
          
         Wr.elts[j3  ] = (v_M[0][0]*C->Wv.elts[j3  ] + v_M[0][1]*C->Wv.elts[j3+1] + v_M[0][2]*C->Wv.elts[j3+2]);
         Wr.elts[j3+1] = (v_M[1][0]*C->Wv.elts[j3  ] + v_M[1][1]*C->Wv.elts[j3+1] + v_M[1][2]*C->Wv.elts[j3+2]);
         Wr.elts[j3+2] = (v_M[2][0]*C->Wv.elts[j3  ] + v_M[2][1]*C->Wv.elts[j3+1] + v_M[2][2]*C->Wv.elts[j3+2]); 

         /* Check rotated weights. */
         if(LocalHead) { 
            if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "%s:  \n"
                                    "  W rotated = %f     %f    %f \n"
                                    "  expansion factor = %f \n", 
                                    FuncName, Wr.elts[j3], Wr.elts[j3+1], Wr.elts[j3+2], v_expand); } 
            if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "  Dot Product of Rotated Weights with Radius = %.12f \n"
                                    "  Mag_Wv_rotated = %f \n", 
                                    SUMA_MT_DOT( (&(Wr.elts[j3])), (&(C->NewNodeList[i3])) ), 
                                    sqrt(   SUMA_POW2(C->Wv.elts[j3  ]) 
                                          + SUMA_POW2(C->Wv.elts[j3+1]) 
                                          + SUMA_POW2(C->Wv.elts[j3+2]) ) ); 
            }
         }
         
         Wr.elts[j3  ] *= v_expand;
         Wr.elts[j3+1] *= v_expand;
         Wr.elts[j3+2] *= v_expand;
       
         if(LocalHead) {
            if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "%s:\n"
                                    "v_M = [\n", FuncName);
               for (r=0; r<3; ++r) {  
                     fprintf (SUMA_STDERR,"  %.5f   %.5f   %.5f \n", 
                                             v_expand*v_M [ (r) ][ (0) ],
                                             v_expand*v_M [ (r) ][ (1) ],
                                             v_expand*v_M [ (r) ][ (2) ]);
               }
               fprintf(SUMA_STDERR, "];\n");
            }
         }
         
         if(LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) {
               fprintf(SUMA_STDERR, "Velocity Field Components as stored in Wr.elts: \n"
                                    "     %f    %f    %f \n",
                                   Wr.elts[j3  ], Wr.elts[j3+1], Wr.elts[j3+2]); } 
         }
   
         C->VelocityField[i3  ] += Wr.elts[j3  ];
         C->VelocityField[i3+1] += Wr.elts[j3+1];
         C->VelocityField[i3+2] += Wr.elts[j3+2]; 
      }
      
      if(LocalHead) { 
         if(i == opt->CtrlPts_iim[0]) {
            fprintf(SUMA_STDERR, "%s: Initial Velocity Field, no adjustment: \n"
                                 "     %f    %f    %f \n",
                                 FuncName, C->VelocityField[i3  ], C->VelocityField[i3+1], C->VelocityField[i3+2]); }

         if(i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "  Dot Product of Velocity Field with Radius = %.12f \n",
                                    SUMA_MT_DOT( (&(C->VelocityField[i3])), (&(C->NewNodeList[i3])) ) ); }
      }
   }
   fclose (test_expansion); test_expansion = NULL;
  
   vector_destroy (&Wr);

   SUMA_RETURN(YUP);
}

SUMA_Boolean Debug_Move( MyCircle *C, MyCircleOpt *opt, SUMA_SurfaceObject *SO, double dt, int niter, int m, int a_niter, int first_bad_niter) 
{
   static char FuncName[]={"Debug_Move"};
   int i, i3, j;
   char outfile[] = {"Coords_0.txt"}, outfile_speed[] = {"Plot_Speed0.txt"}, outfile_test[50]; 
   char outfile_segments[50], outfile_Vmag[50], outfile_tri_area[50];
   double sideA[3], sideB[3], sideC[3], height[3], side[3], t_area;
   int f, g, h, f3, g3, h3;
   double Vf_segment[3];
   double Point_at_Distance[2][3] = { {0.0, 0.0, 0.0},{ 0.0, 0.0, 0.0} }, V_Mag = 0.0;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if(LocalHead) { fprintf(stderr,"%s: niter = %d, dt = %.3f.\n", FuncName, niter, dt); }
   

   /* Test_move.1D -- Write nodelist and velocity field to file. */
   if(!first_bad_niter || niter < first_bad_niter+5){ 
      if(!a_niter) { /* For now, only write to file when niter changes, not when a_niter changes. */
         FILE *test = NULL;
         sprintf(outfile_test, "%s%d.1D", opt->outfile, (niter));
         test = fopen (outfile_test, "w"); 
         
         if(opt->dom_dim > 2) {
            fprintf (test, "col#0: Node Index\n"
                           "col#1,2,3: Node Coordinates at Beginning of Iteration.\n"
                           "col#4,5,6: Calculated Velocity Field to be Used in this Iteration.\n"
                           "col#7: Step Size Used in the Move. (magnitude of velocity*dt)\n"
                           "     dt = %f     Niter = %d\n", dt, niter );  
         }
         for (i = 0; i < C->N_Node; ++i) {
            i3 = 3*i;
            /* Calculate magnitude of step size.  Storing in C->Theta[i3+2] because this array has already been created
               and is not being used for anything. Written to fourth column of test_move.1D file. */
            C->Theta[i3+2] = sqrt(  SUMA_POW2(dt*C->VelocityField[i3  ]) + 
                                    SUMA_POW2(dt*C->VelocityField[i3+1]) +                  
                                    SUMA_POW2(dt*C->VelocityField[i3+2]) ); 
            fprintf (test, "%d   %11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f\n", 
               i, C->NewNodeList[i3  ], C->NewNodeList[i3+1], C->NewNodeList[i3+2], 
               C->VelocityField[i3  ], C->VelocityField[i3+1], C->VelocityField[i3+2], C->Theta[i3+2]);
         }
         i = 0; i3 = 3*i;  /* In Matlab, to graph the circle, need last line to be the same as the first line. */
         fprintf (test, "%d   %11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f\n", 
            i, C->NewNodeList[i3  ], C->NewNodeList[i3+1], C->NewNodeList[i3+2], 
            C->VelocityField[i3  ], C->VelocityField[i3+1], C->VelocityField[i3+2], C->Theta[i3+2]); 
         fclose (test); test = NULL;   
      } 

      if(opt->dom_dim > 2) {
         /* For calculating base and height of each facet and step size.  Base and Height of the triangle
            are the minimum distances when considering the movement of the nodes that form the vertices of the triangle.
            Need triangles to not flip, so must check to see that move (step size) is not greater than the 
            distance across the triangle, which is at least the length of the base or height. */
         if(!first_bad_niter){ 
            FILE *tri_area = NULL;
            sprintf(outfile_tri_area, "tri_area%d_%d.1D", m, niter);
            tri_area = fopen (outfile_tri_area, "w"); 
            fprintf (tri_area, "col#0: Facet Index\n"
                           "col#1: Triangle base.\n"
                           "col#2: Triangle height.\n"
                           "col#3: Move distance/size of step for the three nodes of the facet.\n"
                           "       Nodes of facet listed in random order.\n"
                           "col#4: Triangle area.\n"
                           "dt = %11f\n", dt);
            for (i = 0; i < SO->N_FaceSet; ++i) {
               i3 = 3*i;
               f = SO->FaceSetList[i3  ];
               g = SO->FaceSetList[i3+1];
               h = SO->FaceSetList[i3+2];
               f3 = 3*f; g3 = 3*g, h3 = 3*h; 

               SUMA_TRI_AREA( (&(C->NewNodeList[f3])), (&(C->NewNodeList[g3])), (&(C->NewNodeList[h3])), t_area );

               /* Determine the vectors that makes up the sides of the triangle by subtracting node locations.*/
               SUMA_MT_SUB( sideA, (&(C->NewNodeList[f3])), (&(C->NewNodeList[g3])) );
               SUMA_MT_SUB( sideB, (&(C->NewNodeList[f3])), (&(C->NewNodeList[h3])) );
               SUMA_MT_SUB( sideC, (&(C->NewNodeList[g3])), (&(C->NewNodeList[h3])) );

               /* Find the length of the side by finding the magnitude of the side vectors. 
                  These values represent the possible bases for calculating the area of a triangle 
                     if area = 0.5*base*height. */
               side[0] = sqrt( SUMA_POW2(sideA[0]) + SUMA_POW2(sideA[1]) + SUMA_POW2(sideA[2]) );
               side[1] = sqrt( SUMA_POW2(sideB[0]) + SUMA_POW2(sideB[1]) + SUMA_POW2(sideB[2]) );
               side[2] = sqrt( SUMA_POW2(sideC[0]) + SUMA_POW2(sideC[1]) + SUMA_POW2(sideC[2]) );

               /* Calculate the possible heights; the perpendicular bisectors of the triangle; 
                  the minimal distance across the triangle. */             
               height[0] = (2*t_area) / side[0];
               height[1] = (2*t_area) / side[1];
               height[2] = (2*t_area) / side[2];

               fprintf(tri_area, "%d   %f    %f    %f    %f\n"
                                 "     %f    %f    %f\n"
                                 "     %f    %f    %f\n", i,
                                 side[0], height[0], C->Theta[f3+2], t_area, 
                                 side[1], height[1], C->Theta[g3+2], 
                                 side[2], height[2], C->Theta[h3+2] );
            }
            fclose(tri_area); tri_area = NULL;
         } 

         /* TO PLOT VELOCITY FIELD AT ANY ITERATION. */
         /* Write oriented segment file for plotting in SUMA. */
         if(!niter){    /* Only write to file at the beginning of every m-loop. */
            if(!first_bad_niter || niter < first_bad_niter+5) { 
               FILE *plot_segments = NULL;
               sprintf(outfile_segments, "SUMA_segments_m%d.1D.dset", m); 
               plot_segments = fopen (outfile_segments, "w"); 
               fprintf (plot_segments, "#oriented_segments\n");
               for (i = 0; i < C->N_Node; ++i) {
                  i3 = 3*i;
                  /* To plot end of segment, must calculate the location of the point of the velocity vector. 
                     This is done by adding the position vector of the node to the velocity vector at that node. */
                  Vf_segment[0] = C->VelocityField[i3  ];
                  Vf_segment[1] = C->VelocityField[i3+1];
                  Vf_segment[2] = C->VelocityField[i3+2];

                  Vf_segment[0] = C->NewNodeList[i3  ] + Vf_segment[0];
                  Vf_segment[1] = C->NewNodeList[i3+1] + Vf_segment[1];
                  Vf_segment[2] = C->NewNodeList[i3+2] + Vf_segment[2];

                  fprintf (plot_segments, "%11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f 0.0  0.0  1.0  1.0 1.5\n",
                                          C->NewNodeList[i3], C->NewNodeList[i3+1], C->NewNodeList[i3+2], 
                                          Vf_segment[0], Vf_segment[1], Vf_segment[2] );
               }
               fclose (plot_segments); plot_segments = NULL; 
            } 
         }
      } 
   }
   
   if(!niter) {
      /* Write velocity magnitudes to file for plotting in SUMA. */  
      FILE *plot_Vmag = NULL;
      sprintf(outfile_Vmag, "SUMA_Vmag_m%d.1D.dset", m); 
      plot_Vmag = fopen (outfile_Vmag, "w"); 
      for (i = 0; i < C->N_Node; ++i) {
         i3 = 3*i;
         fprintf (plot_Vmag, "%11.8f\n", sqrt(  SUMA_POW2(C->VelocityField[i3  ]) + 
                                                SUMA_POW2(C->VelocityField[i3+1]) + 
                                                SUMA_POW2(C->VelocityField[i3+2]) ));
      }
      fclose (plot_Vmag); plot_Vmag = NULL;
   }  
   
   /* Send output to files for graphing in matlab. */
   if(LocalHead) {
      if(opt->dom_dim == 2) {
         FILE *plot_circle = NULL; 
            sprintf(outfile, "Coords_%d.txt", (niter+1));
            plot_circle = fopen (outfile, "w");   
         for (i = 0; i < C->N_Node; ++i) {
            i3 = 3*i;
            fprintf (plot_circle, "%11.8f  %11.8f  %11.8f\n", C->NewNodeList[i3], C->NewNodeList[i3+1], C->NewNodeList[i3+2]);
         }
         fclose (plot_circle); plot_circle = NULL; 
      
         /* Create file for plotting magnitude of velocity field around the circle. */
         FILE *plot_speed = NULL; 
         sprintf(outfile_speed, "Plot_Speed%d.txt", (niter+1));
         plot_speed = fopen(outfile_speed, "w");
         for (i = 0; i < C->N_Node; ++i) {
            i3 = 3*i;
            V_Mag = 0.5*sqrt(  SUMA_POW2(C->VelocityField[i3  ]) +  
                               SUMA_POW2(C->VelocityField[i3+1]) + 
                               SUMA_POW2(C->VelocityField[i3+2])  );
            /* Using half the magnitude to make matlab plot easier to see. */
            SUMA_POINT_AT_DISTANCE_NORM( (&(C->NewNodeList[i3])), (&(C->NewNodeList[i3])), V_Mag, Point_at_Distance );
            fprintf(plot_speed, "%11.8f   %11.8f   %11.8f \n", 
                    Point_at_Distance[0][0], Point_at_Distance[0][1], Point_at_Distance[0][2]);
         }
         fclose(plot_speed); plot_speed = NULL;    
      }
   }
  
   SUMA_RETURN(YUP);
}
 
SUMA_Boolean Neighbor( MyCircle *C, MyCircleOpt *opt, SUMA_SurfaceObject *SO, int niter, int a_niter) 
{
   static char FuncName[]={"Neighbor"};
   char outfile_neighb[50];
   int k, i, j, s, i3, j3, s4;
   double distance = 0.0;
   int close_neighb = 0;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 

   if(LocalHead) fprintf(SUMA_STDERR,"%s: Begin loop for checking neighbors.\n", FuncName);
   
   FILE *neighb = NULL;
   if(opt->neighb_adjust) { sprintf(outfile_neighb, "Neighbor_Check%d_%d.txt", niter, a_niter); }
   else  { sprintf(outfile_neighb, "Neighbor_Check%d.txt", niter); }
   neighb = fopen (outfile_neighb, "w");  

   for(i = 0; i < C->N_Node; ++i) {  
      s4 = 4*i;
      fprintf(neighb, "Node = %d, Stepsize = %f\n", i, C->Vf_Step[s4+3]);
      for(k=0; k < SO->FN->N_Neighb[i]; ++k) {   
         j = SO->FN->FirstNeighb[i][k];  /*Index of node neighbor.*/
         i3 = 3*i;
         j3 = 3*j;
         distance = sqrt(  SUMA_POW2(C->NewNodeList[j3  ] - C->NewNodeList[i3  ]) + 
                           SUMA_POW2(C->NewNodeList[j3+1] - C->NewNodeList[i3+1]) + 
                           SUMA_POW2(C->NewNodeList[j3+2] - C->NewNodeList[i3+2]) ); 
         /* Consider keeping steps smaller than one half of the distance. */
         distance = 0.5*distance; 

         fprintf(neighb,"     Neighbor(%d) = %f\n", j, distance); 
         if(C->Vf_Step[s4+3] > distance) {
            fprintf(neighb,"     Stepsize too big! Need to adjust.\n");
            if(!close_neighb) close_neighb = i;
            if(LocalHead) {
               fprintf(SUMA_STDERR, "%s:  Stepsize too big! Node = %d, Neighbor = %d\n"
                                    "     distance to nearest node = %f\n"
                                    "     step size = %f\n",
                                    FuncName, i, j, distance, C->Vf_Step[s4+3] ); 
            }
         }
         
         if(opt->neighb_adjust) {
            if(C->Vf_Step[s4+3] > distance)  close_neighb = i; break; 
         }
      }
      if(opt->neighb_check) fprintf(neighb,"**********************************\n");
      if(opt->neighb_adjust && close_neighb) break;
   }
   fclose (neighb); neighb = NULL;

   if(LocalHead) fprintf(SUMA_STDERR,"%s: End loop for checking neighbors.\n", FuncName);

   if(opt->neighb_adjust) SUMA_RETURN(close_neighb);
   else SUMA_RETURN(YUP);
}

SUMA_Boolean Calculate_Step (MyCircle *C, MyCircleOpt *opt, double dt) 
{
   static char FuncName[]={"Calculate_Step"};
   int i, i3, s, s4;
   double um;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 

   /* Calculate step size using dt set at beginning of this loop and make step magnitude adjustment. */
   for (i = 0; i < C->N_Node; ++i) {  
      i3 = i*3;
      s4 = i*4; 

      C->Vf_Step[s4  ] = 0.0;
      C->Vf_Step[s4+1] = 0.0;
      C->Vf_Step[s4+2] = 0.0;

      C->Vf_Step[s4  ] = C->VelocityField[i3  ] * dt; 
      C->Vf_Step[s4+1] = C->VelocityField[i3+1] * dt; 
      C->Vf_Step[s4+2] = C->VelocityField[i3+2] * dt;

      if(LocalHead) {
         if(i == opt->CtrlPts_iim[0]) { /*  debug when node is 1st control point */
            fprintf(SUMA_STDERR, "%s, first control point,before adjustment: \n"
                                 "   Node = %d\n"
                                 "   dot(u,rad)  = [%.18f]\n"
                                 "   u(%d) = Vf*dt = [%f  %f   %f]\n"
                                 "   mag(u) = %.8f\n", 
                                 FuncName, i, SUMA_MT_DOT( (&(C->Vf_Step[s4])),(&(C->NewNodeList[i3]))), 
                                 i, C->Vf_Step[s4  ], C->Vf_Step[s4+1], C->Vf_Step[s4+2],
                                 sqrt( SUMA_POW2(C->Vf_Step[s4  ])+SUMA_POW2(C->Vf_Step[s4+1])+SUMA_POW2(C->Vf_Step[s4+2]) ) ); 
         }
      }                                     

      if (opt->adjust) {
         /* must turn the magnitude of u to that of uc, where |uc| / |u| = tan(a) / a; 
            since for unit circle/sphere a = |u|, |uc| = tan(a) */
         /* um = |u| */
         um = sqrt(  SUMA_POW2(C->Vf_Step[s4  ]) + 
                     SUMA_POW2(C->Vf_Step[s4+1]) + 
                     SUMA_POW2(C->Vf_Step[s4+2]));
         /* rescale |u| to make it |uc| */
         if (um > opt->Zero){
            C->Vf_Step[s4  ] *=  tan(um)/um;
            C->Vf_Step[s4+1] *=  tan(um)/um;            
            C->Vf_Step[s4+2] *=  tan(um)/um;            
         }             
      }

      /* Find the magnitude of the step.  This is the step size. */
      C->Vf_Step[s4+3] = sqrt(   SUMA_POW2(C->Vf_Step[s4  ]) + 
                                 SUMA_POW2(C->Vf_Step[s4+1]) + 
                                 SUMA_POW2(C->Vf_Step[s4+2]) );
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean Move_Points (MyCircle *C, MyCircleOpt *opt) 
{
   static char FuncName[]={"Move_Points"};
   int i, i3, s, s4;
   double mv_mag, mv_alpha, mv_nrm[3], mv_nrm_mag, newnode_mag;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 
   
   /* Use array C->NewNodeList_temp to store the new node locations while checking to see if dt is small enough. */
   /* When happy with dt and the move, set C->NewNodeList equal to C->NewNodeList_temp. */
   
   for (i = 0; i < 3*C->N_Node; ++i) C->NewNodeList_temp[i] = 0.0;  /* Reset temporary node list storage. */  
   
   /* MOVE THE POINTS USING ROTATION MACRO. */
   for (i = 0; i < C->N_Node; ++i) {  
      s4 = 4*i;
      i3 = 3*i;
      mv_mag = sqrt( SUMA_POW2(C->Vf_Step[s4  ]) +
                     SUMA_POW2(C->Vf_Step[s4+1]) +
                     SUMA_POW2(C->Vf_Step[s4+2]) );
      if( mv_mag > opt->Zero) { mv_alpha = atan( mv_mag ); }
      else { mv_alpha = 0.0; }
      SUMA_MT_CROSS ( mv_nrm,(&(C->NewNodeList[i3])), (&(C->Vf_Step[s4])) ); 
      mv_nrm_mag = sqrt( mv_nrm[0]*mv_nrm[0] + mv_nrm[1]*mv_nrm[1] +  mv_nrm[2]*mv_nrm[2] );
      if (mv_nrm_mag > opt->Zero) {
         mv_nrm[0] = mv_nrm[0]/mv_nrm_mag; mv_nrm[1] = mv_nrm[1]/mv_nrm_mag;  mv_nrm[2] = mv_nrm[2]/mv_nrm_mag; }

      /* Move the points a small step using the Rotation macro. */
      SUMA_ROTATE_ABOUT_AXIS( (&(C->NewNodeList[i3])), (&(mv_nrm[0])), mv_alpha, (&(C->NewNodeList_temp[i3])) );

      /* Project point back onto the circle. */
      newnode_mag = sqrt(  SUMA_POW2(C->NewNodeList_temp[i3  ]) + 
                           SUMA_POW2(C->NewNodeList_temp[i3+1]) + 
                           SUMA_POW2(C->NewNodeList_temp[i3+2]) );
      if (newnode_mag > opt->Zero) {
         C->NewNodeList_temp[i3  ] = opt->Radius*(C->NewNodeList_temp[i3  ])/( newnode_mag );
         C->NewNodeList_temp[i3+1] = opt->Radius*(C->NewNodeList_temp[i3+1])/( newnode_mag );
         C->NewNodeList_temp[i3+2] = opt->Radius*(C->NewNodeList_temp[i3+2])/( newnode_mag ); }
   }
   /* END OF MOVE. */
      
   #if 0      
   /* OLD MOVE METHOD. MOVE ALONG VELOCITY VECTOR. */ 
   for (i = 0; i < C->N_Node; ++i) {  
      i3 = 3*i;  
      C->NewNodeList[i3  ] = C->NewNodeList[i3  ] + C->Vf_Step[s4  ];
      C->NewNodeList[i3+1] = C->NewNodeList[i3+1] + C->Vf_Step[s4+1];
      C->NewNodeList[i3+2] = C->NewNodeList[i3+2] + C->Vf_Step[s4+2];

      newnode_mag = sqrt(  SUMA_POW2(C->NewNodeList[i3  ]) + 
                           SUMA_POW2(C->NewNodeList[i3+1]) + 
                           SUMA_POW2(C->NewNodeList[i3+2]) );

      /*if (opt->dbg_flag) { fprintf(stderr,"mag initial: %f\n", mag); } */
      if ( newnode_mag > 0.000000001 ) {
         C->NewNodeList[i3  ] = opt->Radius * (C->NewNodeList[i3  ])/( newnode_mag );
         C->NewNodeList[i3+1] = opt->Radius * (C->NewNodeList[i3+1])/( newnode_mag );
         C->NewNodeList[i3+2] = opt->Radius * (C->NewNodeList[i3+2])/( newnode_mag ); }  
   } 
   /* END OLD METHOD. */ 
   #endif
   
   SUMA_RETURN(YUP);
}
