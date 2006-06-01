#include "SUMA_suma.h"
#include "matrix.h"
#include "matrix.c"
#include "SUMA_SurfWarp.h"

/******************* Begin optimizing functions here ************************************************/








/*********************************************** Begin Mesh walking functions here ************************************************/

SUMA_Boolean Debug_Weights( MyCircle *C, MyCircleOpt *opt, matrix M, matrix Mi, vector Vv) 
{
   static char FuncName[]={"FindSplineWeights"};
   static int matrix_ncall;
   int i, i3, j, j3, r, k, c, idm;
 
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if(matrix_ncall == 3) matrix_ncall = 0;

   FILE *output_matrix = NULL;  /*for sending the matrix to a file to be read by matlab. */
   output_matrix = fopen( "output_matrix.m", "w" );
   /* Print Matrix */
   if(matrix_ncall == 0) {
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
      #if 0
      if (opt->dot) {
         fprintf(output_matrix, "M = [\n");
            for (r=0; r<opt->N_ctrl_points; ++r) {
               for (k=0; k<(3+opt->N_ctrl_points); ++k) {
                  for(c=0; c<opt->N_ctrl_points; ++c) { 
                     fprintf (output_matrix,"%11.8f   %11.8f   %11.8f   ", 
                                             M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c  ) ],
                                             M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+1) ],
                                             M.elts [ ((3+opt->N_ctrl_points) * r+k) ][ (3*c+2) ]);
                  }
                  fprintf (output_matrix,"\n");
               } }
         fprintf(output_matrix, "]; \n \n"); 
      } else { 
         fprintf(output_matrix, "M = [\n");
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
         fprintf(output_matrix, "]; \n \n");  } 
      #endif
   }
   
   /*Print the Inverse Coefficient Matrix.*/
   if(matrix_ncall == 1) {
      if(opt->dot) {   
         if (opt->dim == 3) {
            fprintf(SUMA_STDERR, "%s:\n"
                                 "Mi = [\n", FuncName);
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
            fprintf(SUMA_STDERR, "%s:\n"
                                 "Mi = [\n", FuncName);
            for (r=0; r<opt->N_ctrl_points; ++r) {
               for (k=0; k<3; ++k) {
                  for(c=0; c<opt->N_ctrl_points; ++c) { 
                     fprintf (SUMA_STDERR,"%.5f   %.5f  ", 
                                             Mi.elts [ (3*r+k) ][ (2*c  ) ],
                                             Mi.elts [ (3*r+k) ][ (2*c+1) ]);
                  }
                  fprintf (SUMA_STDERR,"\n");
               }
            }
            fprintf(SUMA_STDERR, "];\n");
         }
      } else {   
         if (opt->dim == 3) {
            fprintf(SUMA_STDERR, "%s:\n"
                                 "Mi = [\n", FuncName);
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
         } else {
            fprintf(SUMA_STDERR, "%s:\n"
                                 "Mi = [\n", FuncName);
            for (r=0; r<opt->N_ctrl_points; ++r) {
               for (k=0; k<2; ++k) {
                  for(c=0; c<opt->N_ctrl_points; ++c) { 
                     fprintf (SUMA_STDERR,"%.5f   %.5f  ", 
                                             Mi.elts [ (2*r+k) ][ (2*c  ) ],
                                             Mi.elts [ (2*r+k) ][ (2*c+1) ]);
                  }
                  fprintf (SUMA_STDERR,"\n");
               }
            }
            fprintf(SUMA_STDERR, "];\n");
         }
      }
   }
   
   /* Print velocity vectors. */
   if(matrix_ncall == 2) {
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
                                 i, SUMA_MT_DOT( (&(opt->CtrlPts_i[3*i])), (&(C->Wv.elts[3*i])) ),
                                 i, C->Wv.elts[3*i], C->Wv.elts[3*i+1], C->Wv.elts[3*i+2],
                                 i, opt->CtrlPts_i[3*i], opt->CtrlPts_i[3*i+1],  opt->CtrlPts_i[3*i+2]); 
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
   } 
   
   fclose (output_matrix); output_matrix = NULL;
  
   ++matrix_ncall;
   SUMA_RETURN(YUP);
}



SUMA_Boolean FindSplineWeights (MyCircle *C, MyCircleOpt *opt)
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
   static matrix *nullptr = NULL;
   double I[3][3], sI[3][3];
   
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
      SUMA_ANGLE_DIST_NC((&(opt->CtrlPts_f[i3])), (&(opt->CtrlPts_i[i3])), opt->Dtheta[i], (&(opt->Nrm[i3])) );

      if (LocalHead) {
         fprintf(SUMA_STDERR, "%s: Point %d, Dtheta = %.12f rad (%.12f deg)\n",
                                 FuncName, i, opt->Dtheta[i], SUMA_R2D(opt->Dtheta[i]));                            
      }

      SUMA_MT_CROSS(tan_v, (&(opt->Nrm[i3])), (&(opt->CtrlPts_i[i3])) );
      
      /* Need to normalize because using the direction (unit vector) of this tangent vector in the velocity calculation. */
      tan_v_mag = sqrt( tan_v[0]*tan_v[0] + tan_v[1]*tan_v[1] + tan_v[2]*tan_v[2] );
      if ( tan_v_mag > 0.000000001) {
         tan_v[0] = tan_v[0]/tan_v_mag;
         tan_v[1] = tan_v[1]/tan_v_mag;
         tan_v[2] = tan_v[2]/tan_v_mag; }
         
      /*if (LocalHead) {
         fprintf(SUMA_STDERR, "  TAN_V normalized = [ %.12f;    %.12f;    %.12f ]\n"
                              "  Dot product( tan_v, control point) = %.12f\n"
                              "  Dtheta = %f\n", 
                              tan_v[0], tan_v[1], tan_v[2],
                              SUMA_MT_DOT(tan_v, (&(opt->CtrlPts_i[i3])) ),
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

               /* Create the rotation matrix. */
               SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts_i[c3])), (&(opt->CtrlPts_i[r3])), Mcr, t_cr, nrm_cr);
          
               expand_cr = exp(-0.5*t_cr*t_cr);    
               
               /* Assemble the matrix and include the expansion factor. */
               #if 0
               if(LocalHead) {
                  if( r == 0 || r == 1 ) {
                     fprintf(SUMA_STDERR, "ROTATION MATRIX: \n"
                                          "  p1 = [ %f      %f    %f ] \n"
                                          "  p2 = [ %f      %f    %f ] \n"
                                          "     alpha(%d, %d) = %.12f  \n"
                                          "     u (%d, %d) = [ %.12f    %.12f    %.12f ] \n"
                                          "     expansion = %f \n",
                                          opt->CtrlPts_i[c3  ], opt->CtrlPts_i[c3+1], opt->CtrlPts_i[c3+2],
                                          opt->CtrlPts_i[r3  ], opt->CtrlPts_i[r3+1], opt->CtrlPts_i[r3+2],                                
                                          r, c, t_cr, 
                                          r, c, nrm_cr[0], nrm_cr[1], nrm_cr[2],
                                          expand_cr ); } }
               #endif 
              
               if( opt->sigma && r == c) {   /* Need to add sigma squared times the identity matrix to each 3X3. */
                  M.elts[ (r3  ) ][ (c3  ) ] = expand_cr * Mcr[0][0] + (opt->sigma)*(opt->sigma);
                  M.elts[ (r3  ) ][ (c3+1) ] = expand_cr * Mcr[0][1];
                  M.elts[ (r3  ) ][ (c3+2) ] = expand_cr * Mcr[0][2];
                  M.elts[ (r3+1) ][ (c3  ) ] = expand_cr * Mcr[1][0];
                  M.elts[ (r3+1) ][ (c3+1) ] = expand_cr * Mcr[1][1] + (opt->sigma)*(opt->sigma);
                  M.elts[ (r3+1) ][ (c3+2) ] = expand_cr * Mcr[1][2];
                  M.elts[ (r3+2) ][ (c3  ) ] = expand_cr * Mcr[2][0];
                  M.elts[ (r3+2) ][ (c3+1) ] = expand_cr * Mcr[2][1];
                  M.elts[ (r3+2) ][ (c3+2) ] = expand_cr * Mcr[2][2] + (opt->sigma)*(opt->sigma); 
               } else {
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
      }
   
   if( opt->dot ) {
      for(r=0; r<opt->N_ctrl_points; ++r) {     /* r for row.  For the first three rows, r represents the first control point.*/
         for(c=0; c<opt->N_ctrl_points; ++c) {  /* c for column. While r is held constant, c cycles through all control points
                                                   so that for the first 3 rows (first 3 C.P.) are compared to all the rest.  */
            row = (3 + opt->N_ctrl_points)*r;   /* Need extra rows for dot product restriction. */
            c3 = 3*c; r3 = 3*r;
            
            /* Create the rotation matrix. */
            SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts_i[c3])), (&(opt->CtrlPts_i[r3])), Mcr, t_cr, nrm_cr );
            
            expand_cr = exp( -0.5*t_cr*t_cr ); 
            
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
                                       opt->CtrlPts_i[c3  ], opt->CtrlPts_i[c3+1], opt->CtrlPts_i[c3+2],
                                       opt->CtrlPts_i[r3  ], opt->CtrlPts_i[r3+1], opt->CtrlPts_i[r3+2],                                
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
                  M.elts[ (row+k) ][ (c3  ) ] = (opt->CtrlPts_i[r3  ] * (expand_cr * Mcr[0][0]) )
                                              + (opt->CtrlPts_i[r3+1] * (expand_cr * Mcr[1][0]) )
                                              + (opt->CtrlPts_i[r3+2] * (expand_cr * Mcr[2][0]) ); 
                  M.elts[ (row+k) ][ (c3+1) ] = (opt->CtrlPts_i[r3  ] * (expand_cr * Mcr[0][1]) )
                                              + (opt->CtrlPts_i[r3+1] * (expand_cr * Mcr[1][1]) )
                                              + (opt->CtrlPts_i[r3+2] * (expand_cr * Mcr[2][1]) );
                  M.elts[ (row+k) ][ (c3+2) ] = (opt->CtrlPts_i[r3  ] * (expand_cr * Mcr[0][2]) )
                                              + (opt->CtrlPts_i[r3+1] * (expand_cr * Mcr[1][2]) )
                                              + (opt->CtrlPts_i[r3+2] * (expand_cr * Mcr[2][2]) ); } 
               else {
                  M.elts[ (row+k) ][ (c3  ) ] = 0.0;
                  M.elts[ (row+k) ][ (c3+1) ] = 0.0;
                  M.elts[ (row+k) ][ (c3+2) ] = 0.0; }
            }   
         }
      }  
   }
   
   if( LocalHead ) Debug_Weights(C, opt, M, Mi, Vv);
 
   SUMA_LH("Calculating inverse...");
   matrix_initialize(&Mi);
   matrix_create(nr, nc, &Mi);
   matrix_psinv (M, nullptr, &Mi); 
   SUMA_LH("   Done."); 

   if( LocalHead ) Debug_Weights(C, opt, M, Mi, Vv);
   fprintf(SUMA_STDERR, "Done printing inverse matrix.");

   SUMA_LH("Calculating weights...");
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
   byte repeat;   
   int i, i3, j, j3, r;
   static double v_alpha = 0.0, v_cr[3]={0.0,0.0,0.0}, v_expand, cr_mag; 
   vector Wr;  /*W rotated*/
   static double v_M[3][3];
 
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* Compute velocity at each node using contributions from the control points. */
   vector_initialize (&Wr);
   vector_create (3*opt->N_ctrl_points, &Wr);
   
   if (LocalHead) { 
      fprintf(SUMA_STDERR, "******************************************************\n"
                           "%s: CHECKING THE FUNCTION, First control point: \n", FuncName); }
   
   for (i=0; i< C->N_Node; ++i) {  /* i for all points, j for the control points. */
      i3 = 3*i;

      for (j=0; j<opt->N_ctrl_points; ++j){  
         j3 = 3*j;
        
         #if 0
         /* Need to comment out when using SUMA_3D_Rotation_Matrix to calculate initial velocity field. */
         SUMA_ANGLE_DIST_NC( (&(C->NewNodeList[i3])), (&(opt->CtrlPts_i[j3])), v_alpha, v_cr );
        
         cr_mag = sqrt( v_cr[0]*v_cr[0] + v_cr[1]*v_cr[1] + v_cr[2]*v_cr[2]); 
         if (LocalHead) { 
            if( i == opt->CtrlPts_iim[0] ) { fprintf(SUMA_STDERR, "Magnitude of Cross Product = %.12f \n", cr_mag); }} 
   
         if(cr_mag > 0.000000001){ v_cr[0] = v_cr[0]/cr_mag; v_cr[1] = v_cr[1]/cr_mag; v_cr[2] = v_cr[2]/cr_mag; }  
         
         if (LocalHead) {
            if (i == opt->CtrlPts_iim[0]) {
               fprintf(SUMA_STDERR, "Using SUMA_ROTATE_ABOUT AXIS to calculate velocity field.\n" ); } }
         
         /* ROTATE THE WEIGHTS USING DISPLACEMENT ANGLE AND AXIS OF ROTATION CALCULATED ABOVE. */
         SUMA_ROTATE_ABOUT_AXIS( (&(C->Wv.elts[j3])), v_cr, v_alpha, (&(Wr.elts[j3])) );       
                            
         v_expand = exp( -0.5*v_alpha*v_alpha ); 
         #endif
        
         SUMA_3D_Rotation_Matrix( (&(opt->CtrlPts_i[j3])), (&(C->NewNodeList[i3])), v_M, v_alpha, v_cr);
         
         v_expand = exp( -0.5*v_alpha*v_alpha );   
       
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
                                    opt->CtrlPts_i[j3  ], opt->CtrlPts_i[j3+1], opt->CtrlPts_i[j3+2],
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
         
         if(opt->sigma) { 
            Wr.elts[j3  ] += SUMA_POW2( opt->sigma )*C->Wv.elts[j3  ];
            Wr.elts[j3+1] += SUMA_POW2( opt->sigma )*C->Wv.elts[j3+1];
            Wr.elts[j3+2] += SUMA_POW2( opt->sigma )*C->Wv.elts[j3+2];
         }
         
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
      
   vector_destroy (&Wr);

   SUMA_RETURN(YUP);
}

SUMA_Boolean Debug_Move( MyCircle *C, MyCircleOpt *opt, SUMA_SurfaceObject *SO, double dt, int niter, int a_niter, int first_bad_niter) 
{
   static char FuncName[]={"Debug_Move"};
   int i, i3, j;
   char outfile[] = {"Coords_0.txt"}, outfile_speed[] = {"Plot_Speed0.txt"}, outfile_test[50]; 
   char outfile_segments[50], outfile_Vmag[50], outfile_tri_area[50];
   static double sideA[3], sideB[3], sideC[3], height[3], side[3], t_area;
   static int f, g, h, f3, g3, h3;
   static double Vf_segment[3];
   static float Point_at_Distance[2][3] = { {0.0, 0.0, 0.0},{ 0.0, 0.0, 0.0} }, V_Mag = 0.0;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if(LocalHead) { fprintf(stderr,"%s: niter = %d, dt = %.3f.\n", FuncName, niter, dt); }
   
   /* Test_move.1D -- Write nodelist and velocity field to file. */
   if(!first_bad_niter || niter < first_bad_niter+5){ 
      if(!a_niter) { /* For now, only write to file when niter changes, not when a_niter changes. */
         FILE *test = NULL;
         sprintf(outfile_test, "%s%d.1D", opt->outfile, (niter));
         test = fopen (outfile_test, "w"); 
         fprintf (test, "col#0: Node Index\n"
                        "col#1,2,3: Node Coordinates at Beginning of Iteration.\n"
                        "col#4,5,6: Calculated Velocity Field to be Used in this Iteration.\n"
                        "col#7: Step Size Used in the Move. (magnitude of velocity*dt)\n"
                        "     dt = %f     Niter = %d\n", dt, niter );  
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
         fclose (test); test = NULL;   
      } 
  
      /* For calculating base and height of each facet and step size.  Base and Height of the triangle
         are the minimum distances when considering the movement of the nodes that form the vertices of the triangle.
         Need triangles to not flip, so must check to see that move (step size) is not greater than the 
         distance across the triangle, which is at least the length of the base or height. */
      if(!first_bad_niter || niter < first_bad_niter+5){ 
         FILE *tri_area = NULL;
         sprintf(outfile_tri_area, "tri_area%d.1D", niter);
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
      if(!first_bad_niter || niter < first_bad_niter+5) { 
         FILE *plot_segments = NULL;
         sprintf(outfile_segments, "SUMA_segments%d.1D.dset", niter); 
         plot_segments = fopen (outfile_segments, "w"); 
         fprintf (plot_segments, "#oriented_segments\n");
         for (i = 0; i < C->N_Node; ++i) {
            i3 = 3*i;
            /* To plot end of segment, must calculate the location of the point of the velocity vector. 
               This is done by adding the position vector of the node to the velocity vector at that node. */
            Vf_segment[0] = C->VelocityField[i3  ];
            Vf_segment[1] = C->VelocityField[i3+1];
            Vf_segment[2] = C->VelocityField[i3+2];

            Vf_segment[0] = C->NewNodeList[i3  ] + 0.05*Vf_segment[0];
            Vf_segment[1] = C->NewNodeList[i3+1] + 0.05*Vf_segment[1];
            Vf_segment[2] = C->NewNodeList[i3+2] + 0.05*Vf_segment[2];

            fprintf (plot_segments, "%11.8f  %11.8f  %11.8f  %11.8f  %11.8f  %11.8f 0.0  0.0  1.0  1.0 1.5\n",
                                    C->NewNodeList[i3], C->NewNodeList[i3+1], C->NewNodeList[i3+2], 
                                    Vf_segment[0], Vf_segment[1], Vf_segment[2] );
         }
         fclose (plot_segments); plot_segments = NULL; 
      } 

      /* Write velocity magnitudes to file for plotting in SUMA. */  
      FILE *plot_Vmag = NULL;
      sprintf(outfile_Vmag, "SUMA_Vmag%d.1D.dset", niter); 
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
   static double distance = 0.0;
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
   static int i, i3, s, s4;
   static double um;
   
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
         if (um > 0.0000001){
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
   static int i, i3, s, s4;
   static double mv_mag, mv_alpha, mv_nrm[3], mv_nrm_mag, newnode_mag;
   
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY; 
   
   /* MOVE THE POINTS USING ROTATION MACRO. */
   for (i = 0; i < C->N_Node; ++i) {  
      s4 = 4*i;
      i3 = 3*i;
      mv_mag = sqrt( SUMA_POW2(C->Vf_Step[s4  ]) +
                     SUMA_POW2(C->Vf_Step[s4+1]) +
                     SUMA_POW2(C->Vf_Step[s4+2]) );
      if( mv_mag > 0.000000001) { mv_alpha = atan( mv_mag ); }
      else { mv_alpha = 0.0; }
      SUMA_MT_CROSS ( mv_nrm,(&(C->NewNodeList[i3])), (&(C->Vf_Step[s4])) ); 
      mv_nrm_mag = sqrt( mv_nrm[0]*mv_nrm[0] + mv_nrm[1]*mv_nrm[1] +  mv_nrm[2]*mv_nrm[2] );
      if (mv_nrm_mag > 0.000000001) {
         mv_nrm[0] = mv_nrm[0]/mv_nrm_mag; mv_nrm[1] = mv_nrm[1]/mv_nrm_mag;  mv_nrm[2] = mv_nrm[2]/mv_nrm_mag; }

      /* Move the points a small step using the Rotation macro. */
      SUMA_ROTATE_ABOUT_AXIS( (&(C->NewNodeList[i3])), (&(mv_nrm[0])), mv_alpha, (&(C->NewNodeList[i3])) );

      /* Project point back onto the circle. */
      newnode_mag = sqrt(  SUMA_POW2(C->NewNodeList[i3  ]) + 
                           SUMA_POW2(C->NewNodeList[i3+1]) + 
                           SUMA_POW2(C->NewNodeList[i3+2]) );
      if (newnode_mag > 0.000000001) {
         C->NewNodeList[i3  ] = opt->Radius*(C->NewNodeList[i3  ])/( newnode_mag );
         C->NewNodeList[i3+1] = opt->Radius*(C->NewNodeList[i3+1])/( newnode_mag );
         C->NewNodeList[i3+2] = opt->Radius*(C->NewNodeList[i3+2])/( newnode_mag ); }
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
