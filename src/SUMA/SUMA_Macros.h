#ifndef SUMA_MACROSm_INCLUDED
#define SUMA_MACROSm_INCLUDED

      
#define IS_STRICT_POS(a)   ( ((a) > 0) ? 1 : 0 )

#define IS_POS(a)   ( ((a) >= 0) ? 1 : 0 )

#define IS_STRICT_NEG(a)   ( ((a) < 0) ? 1 : 0 )

#define SUMA_IS_NEG(a)   ( ((a) <= 0) ? 1 : 0 )

/* largest absolute value */
#define SUMA_LARG_ABS(a, b) ( ( fabs((double)(a)) > fabs((double)(b)) ) ? fabs((double)(a)) : fabs((double)(b)) )
 
/*! the 1D index of element 
   [r][c] in a row major matrix 
   of nc columns */ 
#define SRM(r,c,nc) ((nc)*(r)+(c))
/*! the 1D index of element 
   [r][c] in a column major matrix 
   of nr rows */ 
#define SCM(r,c,nr) ((r)+(nr)*(c))


#define SUMA_WRAP_VALUE(v, min, max)   \
   {  \
      if (v > max) v = min;   \
      else if (v < min) v = max; \
   }

#define SUMA_CLIP_VALUE(v, min, max)   \
   {  \
      if (v > max) v = max;   \
      else if (v < min) v = min; \
   }

#define SUMA_CLIP_UB(v, max)   \
   {  \
      if (v > max) v = max;   \
   }

#define SUMA_CLIP_LB(v, min)   \
   {  \
      if (v < min) v = min; \
   }
   
#define SUMA_SET_GL_RENDER_MODE(m_PolyMode)  \
   {  \
      switch (m_PolyMode) {   \
               case SRM_Fill: \
                  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   \
                  break;   \
               case SRM_Line:  \
                  glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);   \
                  break;   \
               case SRM_Points:  \
                  glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);  \
                  break;   \
               case SRM_ViewerDefault: \
                  break;   \
               default: \
                  fprintf (SUMA_STDERR, "Error %s: Wrong Rendering Mode.\n", FuncName);   \
                  break;   \
            }  \
   }         

/*!
   \brief calculates the average 'radius' of a surface.
   avg(dist(node_i,center));
*/
#define SUMA_SO_RADIUS(SO, r){ \
   int m_i, m_i3; \
   float m_dx, m_dy, m_dz; \
   r = 0.0; \
   for (m_i=0; m_i<SO->N_Node; ++m_i) {   \
      m_i3 = 3 * m_i;  \
      m_dx = SO->NodeList[m_i3  ] - SO->Center[0];   \
      m_dy = SO->NodeList[m_i3+1] - SO->Center[1];   \
      m_dz = SO->NodeList[m_i3+2] - SO->Center[2];   \
      r += sqrt( (m_dx * m_dx) + (m_dy * m_dy) + (m_dz * m_dz)); \
   }  \
   r /= (float)SO->N_Node; \
}

/*!
   A macro to calculate a surface object's normals 
*/
#define SUMA_RECOMPUTE_NORMALS(SO){ \
   SUMA_SURF_NORM m_SN;   \
   if (SO->NodeNormList) SUMA_free(SO->NodeNormList); SO->NodeNormList = NULL;   \
   if (SO->FaceNormList) SUMA_free(SO->FaceNormList); SO->FaceNormList = NULL;   \
   m_SN = SUMA_SurfNorm(SO->NodeList,  SO->N_Node, SO->FaceSetList, SO->N_FaceSet );  \
   SO->NodeNormList = m_SN.NodeNormList; \
   SO->FaceNormList = m_SN.FaceNormList; \
   SO->glar_NodeNormList = (GLfloat *) SO->NodeNormList; /* just copy the pointer, not the data */\
}

/*!
   A macro to recalculate a surface's center and its bounding box 
*/
#define SUMA_DIM_CENTER(SO){  \
   SUMA_MIN_MAX_SUM_VECMAT_COL (SO->NodeList, SO->N_Node, SO->NodeDim, SO->MinDims, SO->MaxDims, SO->Center);  \
   SO->Center[0] /= SO->N_Node;  \
   SO->Center[1] /= SO->N_Node;  \
   SO->Center[2] /= SO->N_Node;  \
   SUMA_MIN_VEC (SO->MinDims, 3, SO->aMinDims );   \
   SUMA_MAX_VEC (SO->MaxDims, 3, SO->aMaxDims);    \
}

/*! calculate the centroid of a triangular faceset */
#define SUMA_FACE_CENTROID(SO, ifc, c){   \
   static int m_n1, m_n2, m_n3;  \
   m_n1 =  SO->FaceSetList[3*ifc]; m_n2 = SO->FaceSetList[3*ifc+1]; m_n3 = SO->FaceSetList[3*ifc+2];   \
   c[0] = (SO->NodeList[3*m_n1]   + SO->NodeList[3*m_n2]   + SO->NodeList[3*m_n3]  )/3; \
   c[1] = (SO->NodeList[3*m_n1+1] + SO->NodeList[3*m_n2+1] + SO->NodeList[3*m_n3+1])/3; \
   c[2] = (SO->NodeList[3*m_n1+2] + SO->NodeList[3*m_n2+2] + SO->NodeList[3*m_n3+2])/3; \
}

/*! 
   A macro version of SUMA_FindEdge
   Use function for robust error checking.
   Here you are on your own.
   
   you should make sure n1 < n2
   you should initialize iseg to -1 before calling the macro
   
*/

#define SUMA_FIND_EDGE(m_EL, m_n1, m_n2, m_iseg)  \
{  int m_eloc ;   \
   m_eloc = m_EL->ELloc[m_n1];  \
   do {  \
      if (m_EL->EL[m_eloc][1] == m_n2) m_iseg = m_eloc;   \
      ++m_eloc;  \
   } while (m_EL->EL[m_eloc][0] == m_n1 && m_eloc < m_EL->N_EL);  \
} 

/*!
   Macro that sets Ntail to be the index of the last node of the last ROIdatum in a 
   DrawnROI. Ntail is -1 if the macro fails
*/
#define SUMA_DRAWN_ROI_TAIL_NODE(D_ROI, Ntail)  \
   {\
      DListElmt *m_Tail=NULL; \
      SUMA_ROI_DATUM *m_ROId=NULL; \
      Ntail = -1; \
      m_Tail = dlist_tail(D_ROI->ROIstrokelist); \
      if (m_Tail) {  \
         m_ROId = (SUMA_ROI_DATUM *)m_Tail->data;  \
         if (m_ROId->N_n) Ntail = m_ROId->nPath[m_ROId->N_n-1];   \
      }  \
   }

/*!
   Macro that sets Nhead to be the index of the 1st node of the 1st ROIdatum in a 
   DrawnROI. Nhead is -1 if the macro fails
*/
#define SUMA_DRAWN_ROI_HEAD_NODE(D_ROI, Nhead)  \
   {\
      DListElmt *m_Head=NULL; \
      SUMA_ROI_DATUM *m_ROId=NULL; \
      Nhead = -1; \
      m_Head = dlist_head(D_ROI->ROIstrokelist); \
      if (m_Head) {  \
         m_ROId = (SUMA_ROI_DATUM *)m_Head->data;  \
         if (m_ROId->N_n) Nhead = m_ROId->nPath[0];   \
      }  \
   }



   
/* definitions for SUMA_MT_intersect */
#define SUMA_MT_CROSS(m_MTCR_dest,m_MTCR_v1,m_MTCR_v2) \
          m_MTCR_dest[0]=m_MTCR_v1[1]*m_MTCR_v2[2]-m_MTCR_v1[2]*m_MTCR_v2[1]; \
          m_MTCR_dest[1]=m_MTCR_v1[2]*m_MTCR_v2[0]-m_MTCR_v1[0]*m_MTCR_v2[2]; \
          m_MTCR_dest[2]=m_MTCR_v1[0]*m_MTCR_v2[1]-m_MTCR_v1[1]*m_MTCR_v2[0];
#define SUMA_MT_DOT(m_MTDOT_v1,m_MTDOT_v2) (m_MTDOT_v1[0]*m_MTDOT_v2[0]+m_MTDOT_v1[1]*m_MTDOT_v2[1]+m_MTDOT_v1[2]*m_MTDOT_v2[2])
#define SUMA_MT_SUB(m_MTSUB_dest,m_MTSUB_v1,m_MTSUB_v2) \
          m_MTSUB_dest[0]=m_MTSUB_v1[0]-m_MTSUB_v2[0]; \
          m_MTSUB_dest[1]=m_MTSUB_v1[1]-m_MTSUB_v2[1]; \
          m_MTSUB_dest[2]=m_MTSUB_v1[2]-m_MTSUB_v2[2]; 
#define SUMA_NORM(m_NORM_dest, m_NORM_v1) \
          m_NORM_dest= sqrt(m_NORM_v1[0]*m_NORM_v1[0]+m_NORM_v1[1]*m_NORM_v1[1]+m_NORM_v1[2]*m_NORM_v1[2]);
#define SUMA_TRI_AREA(m_TRIAREA_n0,m_TRIAREA_n1,m_TRIAREA_n2, m_TRIAREA_A)  {\
      float m_TRIAREA_dv[3], m_TRIAREA_dw[3], m_TRIAREA_cross[3];  \
      SUMA_MT_SUB (m_TRIAREA_dv, m_TRIAREA_n1, m_TRIAREA_n0);  \
      SUMA_MT_SUB (m_TRIAREA_dw, m_TRIAREA_n2, m_TRIAREA_n0);  \
      SUMA_MT_CROSS(m_TRIAREA_cross,m_TRIAREA_dv,m_TRIAREA_dw);   \
      SUMA_NORM(m_TRIAREA_A, m_TRIAREA_cross); \
      m_TRIAREA_A *= 0.5; \
   }
         

/*!
   \brief SUMA_EULER_SO (SO, eu)
   computes the euler number = N  - E + F
   eu = SO->N_Node - SO->EL->N_Distinct_Edges + SO->N_FaceSet
   eu = 2 for closed surfaces
   -1000 --> NULL SO
   -1001 --> NULL SO->EL 
*/

#define SUMA_EULER_SO(SO, eu) { \
   if (!SO) { eu = -1000; }   \
   else if (!SO->EL) { eu = -1001; }   \
   else eu = SO->N_Node - SO->EL->N_Distinct_Edges + SO->N_FaceSet; \
}
   
/*!

   \brief   SUMA_IS_IN_VEC(vec, nel, val, loc);
   \param vec pointer to vector of at least nel elements
   \param nel (int) number of elements to check in vec
   \param val value to look for in vec
   \param loc (int) index in vec where val was found. -1 if nothing was found
*/
#define SUMA_IS_IN_VEC(m_vec, m_nel, m_val, m_loc) { \
   int m_i=0;\
   m_loc = -1;\
   while (m_i < m_nel) {   \
      if (m_vec[m_i] == m_val) { \
         m_loc = m_i;   \
         m_i = m_nel;   \
      }  else {   \
         ++ m_i;  \
      }  \
   }  \
}

#define SUMA_DBG_IN_NOTIFY(m_fname) { \
   int m_i;\
   ++SUMAg_CF->InOut_Level;   \
   for (m_i=0; m_i < SUMAg_CF->InOut_Level; ++m_i) fprintf (SUMA_STDERR," ");\
   fprintf (SUMA_STDERR,"--dbg: Entered %s (lvl %d).\n", m_fname, SUMAg_CF->InOut_Level); \
}

#define SUMA_DBG_OUT_NOTIFY(m_fname) { \
   int m_i;\
   for (m_i=0; m_i < SUMAg_CF->InOut_Level; ++m_i) fprintf (SUMA_STDERR," ");\
   fprintf (SUMA_STDERR,"--dbg: Left    %s (lvl %d).\n", m_fname, SUMAg_CF->InOut_Level); \
   --SUMAg_CF->InOut_Level;   \
}


/*! \def SUMA_REPORT_WICH_WIDGET_SV(m_w)
\brief SUMA_REPORT_WICH_WIDGET_SV macro for determining what type of widget m_w is and which sv it belongs to
*/
#define SUMA_REPORT_WICH_WIDGET_SV(m_w) {\
   int m_i = 0;   \
   SUMA_SurfaceViewer *m_sv = NULL;   \
   int m_svi = -1;   \
   while (m_i < SUMA_MAX_SURF_VIEWERS) {   \
      /* the series of if statements should be else-ifs but I left them like that for debugging purposes */ \
      if (SUMAg_SVv[m_i].X->GLXAREA == m_w) {   \
         fprintf (SUMA_STDERR,"SUMA_REPORT_WICH_WIDGET_SV: %p is GLXAREA widget in Surface Viewer %d.\n", m_w, m_i); \
         m_svi = m_i;   \
      }  \
      if (SUMAg_SVv[m_i].X->TOPLEVEL == m_w) {  \
         fprintf (SUMA_STDERR,"SUMA_REPORT_WICH_WIDGET_SV: %p is TOPLEVEL widget in Surface Viewer %d.\n", m_w, m_i); \
         m_svi = m_i;   \
      }  \
      if (SUMAg_SVv[m_i].X->FORM == m_w) {   \
         fprintf (SUMA_STDERR,"SUMA_REPORT_WICH_WIDGET_SV: %p is FORM widget in Surface Viewer %d.\n", m_w, m_i); \
         m_svi = m_i;   \
      }  \
      if (SUMAg_SVv[m_i].X->FRAME == m_w) {   \
         fprintf (SUMA_STDERR,"SUMA_REPORT_WICH_WIDGET_SV: %p is FRAME widget in Surface Viewer %d.\n", m_w, m_i); \
         m_svi = m_i;   \
      }   \
         ++m_i;   \
   }   \
}   


/*! \def SUMA_ANY_WIDGET2SV(m_w, m_sv, m_svi)
\brief SUMA_ANY_WIDGET2SV macro for determining the SurfaceViewer structure containing any of the following widgets: GLXAREA, TOPLEVEL, FORM, FRAME. The macro searches all the SurfaceViewer structures in SUMAg_SVv.

   m_w the widget in question
   m_sv a pointer to SUMA_SurfaceViewer structure. This pointer is NULL if no matching SurfaceViewer structure is found in SUMAg_SVv
   m_svi the index of m_sv in SUMAg_SVv vector of Surface Viewer structures. m_sv = &(SUMAg_SVv[m_svi]). -1 if no match was found 
*/

#define SUMA_ANY_WIDGET2SV(m_w, m_sv, m_svi) {\
   int m_i = 0;   \
   m_sv = NULL;   \
   m_svi = -1;   \
   while (m_i < SUMA_MAX_SURF_VIEWERS) {   \
      if (SUMAg_SVv[m_i].X->GLXAREA == m_w || SUMAg_SVv[m_i].X->TOPLEVEL == m_w || SUMAg_SVv[m_i].X->FORM == m_w || SUMAg_SVv[m_i].X->FRAME == m_w) {   \
         m_svi = m_i;   \
         m_sv = &(SUMAg_SVv[m_i]);   \
         m_i = SUMA_MAX_SURF_VIEWERS; \
      }  else {   \
         ++m_i;   \
      }   \
   }   \
}

/*! \def SUMA_GLXAREA_WIDGET2SV(m_w, m_sv, m_svi)
\brief SUMA_GLXAREA_WIDGET2SV macro for determining the SurfaceViewer structure containing the widget: GLXAREA. The macro searches all the SurfaceViewer structures in SUMAg_SVv.

   m_w the widget in question
   m_sv a pointer to SUMA_SurfaceViewer structure. This pointer is NULL if no matching SurfaceViewer structure is found in SUMAg_SVv
   m_svi the index of m_sv in SUMAg_SVv vector of Surface Viewer structures. m_sv = &(SUMAg_SVv[m_svi]). -1 if no match was found 
*/

#define SUMA_GLXAREA_WIDGET2SV(m_w, m_sv, m_svi) {\
   int m_i = 0;   \
   m_sv = NULL;   \
   m_svi = -1;   \
   while (m_i < SUMA_MAX_SURF_VIEWERS) {   \
      if (SUMAg_SVv[m_i].X->GLXAREA == m_w) {   \
         m_svi = m_i;   \
         m_sv = &(SUMAg_SVv[m_i]);   \
         m_i = SUMA_MAX_SURF_VIEWERS;   \
      }  else {   \
         ++m_i;   \
      }   \
   }   \
}


/* Many of these macros are taken from DSP_in_C examples in
C Language Algorithms for Digital Signal Processing 
by
Bruce Kimball, Paul Embree and Bruce Kimble 
1991, Prentice Hall
*/

/*! \def SUMA_SEG_LENGTH(a,b, dist)
\brief SUMA_SEG_LENGTH macro for a segment's length 
   a pointer to xyz coordinates
   b pointer to xyz coordinates
   dist (float) sqrt( (m_b[0] - m_a[0]) * (m_b[0] - m_a[0])     
                     +(m_b[1] - m_a[1]) * (m_b[1] - m_a[1])     
                     +(m_b[2] - m_a[2]) * (m_b[2] - m_a[2]) ); 
*/
#define SUMA_SEG_LENGTH(m_a, m_b, m_dist) {         \
   m_dist = sqrt(  (m_b[0] - m_a[0]) * (m_b[0] - m_a[0])     \
                  +(m_b[1] - m_a[1]) * (m_b[1] - m_a[1])     \
                  +(m_b[2] - m_a[2]) * (m_b[2] - m_a[2]) );    \
}

/*! \def SUMA_SEG_LENGTH_SQ(a,b, dist)
\brief SUMA_SEG_LENGTH_SQ macro for a segment's squared length 
   a pointer to xyz coordinates
   b pointer to xyz coordinates
   dist (float)      ( (m_b[0] - m_a[0]) * (m_b[0] - m_a[0])     
                     +(m_b[1] - m_a[1]) * (m_b[1] - m_a[1])     
                     +(m_b[2] - m_a[2]) * (m_b[2] - m_a[2]) ); 
*/
#define SUMA_SEG_LENGTH_SQ(m_a, m_b, m_dist) {         \
   m_dist =   (m_b[0] - m_a[0]) * (m_b[0] - m_a[0])     \
             +(m_b[1] - m_a[1]) * (m_b[1] - m_a[1])     \
             +(m_b[2] - m_a[2]) * (m_b[2] - m_a[2]) ;    \
}

/*! \def SUMA_NORM_VEC(a,nel, norm)
\brief SUMA_NORM_VEC macro for vectors's norm (sqrt of sum of squares)
   a pointer to vector
   nel number of elements in vector
   norm (float) norm of a 
*/
#define SUMA_NORM_VEC(a,nel,norm) { \
   int m_I; \
   norm = 0.0; \
   for (m_I = 0; m_I < nel; m_I++) { \
      norm += a[m_I]*a[m_I];    \
   } \
   norm = sqrt(norm); \
}


/*! \def SUMA_MIN_VEC(a,nel, amin)
\brief SUMA_MIN_VEC macro for minimum
   a pointer to vector
   nel number of elements in vector
   amin minimum of a (make sure types of a and amin match)
*/
#define SUMA_MIN_VEC(a,nel,amin) { \
   int m_I; \
   amin = a[0]; \
   for (m_I = 1; m_I < nel; m_I++) { \
      if (a[m_I] < amin) amin = a[m_I];    \
   } \
}

/*! \def SUMA_MIN_LOC_VEC(a,nel, amin, minloc)
\brief SUMA_MIN_VEC macro for minimum identification and location
   a pointer to vector
   nel (int) number of elements in vector
   amin minimum of a (make sure types of a and amin match)
   minloc (int) index into a where the minimum was found
*/
#define SUMA_MIN_LOC_VEC(a,nel,amin, minloc) { \
   int m_I; \
   amin = a[0]; \
   minloc = 0; \
   for (m_I = 1; m_I < nel; m_I++) { \
      if (a[m_I] < amin) { \
         amin = a[m_I];    \
         minloc = m_I; \
      } \
   } \
}

/*! \def SUMA_MAX_VEC(a,nel,amax)
\brief SUMA_MAX_VEC macro for minimum
   a pointer to vector
   nel number of elements in vector
   amax maximum of a (make sure types of a and amax match)
*/
#define SUMA_MAX_VEC(a,nel,amax) { \
   int m_I; \
   amax = a[0]; \
   for (m_I = 1; m_I < nel; m_I++) { \
      if (a[m_I] > amax) amax = a[m_I];    \
   } \
}

/*! \def SUMA_MIN_MAX_VEC(a,nel,amin, amax, aminloc, amaxloc)
\brief SUMA_MIN_MAX_VEC macro for minimum and maximum 
   a pointer to vector
   nel number of elements in vector
   amin minimum of a (make sure types of a and amin match)
   amax maximum of a (make sure types of a and amax match)
   aminloc index where minimum is found
   amaxloc index where maximum is found
*/
#define SUMA_MIN_MAX_VEC(a,nel, amin, amax, aminloc, amaxloc) { \
   int m_I; \
   amaxloc = 0; \
   amax = a[0]; \
   aminloc = 0; \
   amin = a[0];\
   for (m_I = 1; m_I < nel; m_I++) { \
      if (a[m_I] > amax) { amax = a[m_I]; amaxloc = m_I; }    \
      else { if (a[m_I] < amin) { amin = a[m_I]; aminloc = m_I; } }    \
   } \
}

#define SUMA_MIN_MAX_VEC_STRIDE(a,nel, amin, amax, aminloc, amaxloc, stride) { \
   int m_I; \
   amaxloc = 0; \
   amax = a[0]; \
   aminloc = 0; \
   amin = a[0];\
   for (m_I = stride; m_I < nel; m_I = m_I+stride) { \
      if (a[m_I] > amax) { amax = a[m_I]; amaxloc = m_I; }    \
      else { if (a[m_I] < amin) { amin = a[m_I]; aminloc = m_I; } }    \
   } \
}

/*
SUMA_ADD_VEC macro:

ADDS TWO VECTORS (a,b) POINT BY POINT (PROMOTING AS REQUIRED) AND 
PUTS THE RESULT IN THE c VECTOR (DEMOTING IF REQUIRED).

SUMA_ADD_VEC(a,b,c,len,typea,typeb,typec)

    a       pointer to first vector.
    b       pointer to second vector.
    c       pointer to result vector.
    len     length of vectors (integer).
    typea   legal C type describing the type of a data.
    typeb   legal C type describing the type of b data.
    typec   legal C type describing the type of c data.
*/

#ifndef SUMA_ADD_VEC
#define SUMA_ADD_VEC(a,b,c,len,typea,typeb,typec) {  \
                  typea *_PTA = a;  \
                  typeb *_PTB = b;  \
                  typec *_PTC = c;  \
                  int m_IX;  \
                      for(m_IX = 0 ; m_IX < (len) ; m_IX++)  \
                          *_PTC++ = (typec)((*_PTA++) + (*_PTB++));  \
                  }

#endif

/*
SUMA_SUB_VEC macro:

SUBTRACTS TWO VECTORS (a,b) POINT BY POINT (PROMOTING AS REQUIRED) AND
PUTS THE RESULT IN THE c VECTOR (DEMOTING IF REQUIRED).

SUMA_SUB_VEC(a,b,c,len,typea,typeb,typec)

    a       pointer to first vector.
    b       pointer to second vector.
    c       pointer to result vector.
    len     length of vectors (integer).
    typea   legal C type describing the type of a data.
    typeb   legal C type describing the type of b data.
    typec   legal C type describing the type of c data.

*/

#ifndef SUMA_SUB_VEC
#define SUMA_SUB_VEC(a,b,c,len,typea,typeb,typec) {  \
                  typea *_PTA = a;  \
                  typeb *_PTB = b;  \
                  typec *_PTC = c;  \
                  int m_IX;  \
                      for(m_IX = 0 ; m_IX < (len) ; m_IX++)  \
                          *_PTC++ = (typec)((*_PTA++) - (*_PTB++));  \
                  }
#endif
/*
SUMA_MULT_VEC macro:

MULTIPLIES TWO VECTORS (a,b) POINT BY POINT (PROMOTING AS REQUIRED) AND
PUTS THE RESULT IN THE c VECTOR (DEMOTING IF REQUIRED).

SUMA_MULT_VEC(a,b,c,len,typea,typeb,typec)

    a       pointer to first vector.
    b       pointer to second vector.
    c       pointer to result vector.
    len     length of vectors (integer).
    typea   legal C type describing the type of a data.
    typeb   legal C type describing the type of b data.
    typec   legal C type describing the type of c data.

WARNING: The input data vectors are not cast to the type of c.
         This means that at least one of the input types must
         be able to represent the individual products without
         overflow.

*/

#ifndef SUMA_MULT_VEC
#define SUMA_MULT_VEC(a,b,c,len,typea,typeb,typec) {  \
                   typea *_PTA = a;  \
                   typeb *_PTB = b;  \
                   typec *_PTC = c;  \
                   int m_IX;  \
                       for(m_IX = 0 ; m_IX < (len) ; m_IX++)  \
                           *_PTC++ = (typec)((*_PTA++) * (*_PTB++));  \
                   }
#endif

/*
SUMA_SUM_VEC macro:

FORMS THE SUM THE VECTOR a AND PUT THE RESULT IN THE
PREVIOUSLY DEFINED VARIABLE s.

SUMA_SUM_VEC(a,s,len,typea)

    a       pointer to first vector.
    s       variable used to store result (not a pointer).
    len     length of vector (integer).
    typea   legal C type describing the type of a data.

*/

#ifndef SUMA_SUM_VEC
#define SUMA_SUM_VEC(a,s,len,typea) {  \
                       typea *_PTA = a;  \
                       int m_IX;  \
                       s = (*_PTA++);  \
                       for(m_IX = 1 ; m_IX < (len) ; m_IX++)  \
                           s += (*_PTA++);  \
                   }
#endif

/*
SUMA_SCALE_VEC macro:

SCALES AND/OR CONVERTS (PROMOTES OR DEMOTES) THE INPUT VECTOR a
(of typea) AND COPIES THE SCALED VECTOR INTO ANOTHER VECTOR b
(of typeb).

SUMA_SCALE_VEC(a,b,s,len,typea,typeb)

    a       pointer to input vector.
    b       pointer to output vector.
    s       variable used to scale output vector (not a pointer).
    len     length of vectors (integer).
    typea   legal C type describing the type of a data.
    typeb   legal C type describing the type of b data.

*/

#ifndef SUMA_SCALE_VEC
#define SUMA_SCALE_VEC(a,b,s,len,typea,typeb) {  \
                       typea *_PTA = (typea *)a;  \
                       typeb *_PTB = (typeb *)b;  \
                       int m_IX;  \
                       for(m_IX = 0 ; m_IX < (len) ; m_IX++)  \
                           *(_PTB)++ = (typeb)(s * (*(_PTA)++));  \
                    }
#endif                   

/* SUMA_EXTRACT_VEC macro:

copies a[ind] into b where ind is a vector of indices

SUMA_EXTRACT_VEC (a,b,ind,len)

   a      pointer to input vector
   b     pointer to output vector
   ind   vector containing the indices of the values in a to copy to b
   len   the length of ind (which is that of b too)
   
   DO NOT SEND TWO POINTERS THAT POINT TO THE SAME LOCATION !
*/


#define SUMA_EXTRACT_VEC(a,b,ind,len,typea,typeb) {  \
                       typea *_PTA = (typea *)a;  \
                       typeb *_PTB = (typeb *)b;  \
                       int m_IX;  \
                       for(m_IX = 0 ; m_IX < (len) ; m_IX++)  \
                         _PTB[m_IX] = _PTA[ind[m_IX]];   \
                    }


/* SUMA_CAT_VEC macro :
concatenates two vectors and b together such that a = [a b];

SUMA_CAT_VEC(a,b, catata, lenb,typea,typeb) 
   a       pointer to first vector 
   b       pointer to second vector
   catata  index indicating where b is to be concatenated
           to a at. if catata = 6, then a[6] = b[0]
           and a[7] = b[1] etc ...
           make sure that you allocate enough space for a
   lenb  number of values in b
   
    typea   legal C type describing the type of a data.
    typeb   legal C type describing the type of b data.

*/

#define SUMA_CAT_VEC(a,b, catata, lenb,typea,typeb) { \
                       typea *_PTA = (typea *)a;  \
                       typeb *_PTB = (typeb *)b;  \
                       int m_IX;  \
                       _PTA = _PTA + catata; \
                       for(m_IX = 0 ; m_IX < (lenb) ; m_IX++)  \
                           *(_PTA)++ = (typea)(*(_PTB)++);  \
                    }
                    

/*!
SUMA_GET_MAT_ROW MACRO FOR GETTING A ROW FROM A MATRIX:

SUMA_GET_MAT_ROW(a,b,row,cols,typea,typeb)

    a       pointer to input 2D matrix.
    b       pointer to resultant 1D vector.
    row     index of row to extract from a
    cols    number of columns in matrix a
    typea   legal C type describing the type of a
    typeb   legal C type describing the type of b

*/
#define SUMA_GET_MAT_ROW(a,b,row,cols,typea,typeb) {  \
                 typea **_AMX = (typea **)a;  \
                 typeb *_PTB = (typeb *)b;  \
                 typea *_PTA;  \
                 int _JX;  \
                     _PTA = _AMX[row];  \
                     for(_JX = 0 ; _JX < cols ; _JX++)  \
                         *_PTB++ = (typeb) (*_PTA++);  \
             }    

/*!
SUMA_GET_MAT_COL MACRO FOR GETTING A COLUMN FROM A MATRIX:

SUMA_GET_MAT_COL(a,b, col, rows,typea,typeb)

    a       pointer to input 2D matrix.
    b       pointer to resultant 1D vector.
    col     index of column in matrix a to extract to b
    rows     number of rows to  in a
    typea   legal C type describing the type of a
    typeb   legal C type describing the type of b

*/

#define SUMA_GET_MAT_COL(a,b,col , rows, typea,typeb) {  \
                 typea **_AMX = (typea **)a;  \
                 typeb *_PTB = (typeb *)b;  \
                 typea *_PTA;  \
                 int m_IX,_JX;  \
                 for(m_IX = 0 ; m_IX < rows ; m_IX++) {  \
                     _PTA = _AMX[m_IX ] ;  \
                     for(_JX = 0 ; _JX < col ; _JX++)  \
                         _PTA++; \
                     *_PTB++ = (typeb)(*_PTA++);  \
                 }  \
             }    


/*! \def SUMA_MIN_MAT_COL(a, rows, cols, amin)
\brief SUMA_MIN_MAT_COL macro for minimum of each column in a matrix
   a pointer to matrix (**)
   rows number of rows
   cols number of cols
   amin minimum of each column in a (make sure types of a and amin match)
*/
#define SUMA_MIN_MAT_COL(a, rows, cols, amin) { \
                  int m_IX, _JX;   \
                  for (m_IX = 0; m_IX < cols ; m_IX++) {   \
                     amin[m_IX]=a[0][m_IX];   \
                     for (_JX = 1 ; _JX < rows ; _JX++)  \
                        if (a[_JX][m_IX] < amin[m_IX]) amin[m_IX] = a[_JX][m_IX];\
                     }\
                  }
                  

/*! \def SUMA_MAX_MAT_COL(a, rows, cols, amax)
\brief SUMA_MAX_MAT_COL macro for maximum of each column in a matrix
   a pointer to matrix (**)
   rows number of rows
   cols number of cols
   amax maximum of each column in a (make sure types of a and amin match)
*/
#define SUMA_MAX_MAT_COL(a, rows, cols, amax) { \
                  int m_IX, _JX;   \
                  for (m_IX = 0; m_IX < cols ; m_IX++) {   \
                     amax[m_IX]=a[0][m_IX];   \
                     for (_JX = 1 ; _JX < rows ; _JX++)  \
                        if (a[_JX][m_IX] > amax[m_IX]) amax[m_IX] = a[_JX][m_IX];\
                  }\
               }
                  
/*! \def SUMA_MIN_MAX_SUM_MAT_COL(a, rows, cols, amin, amax, asum)
\brief  SUMA_MIN_MAX_SUM_MAT_COL macro for minimum, maximum and sum of each column in a matrix
   a pointer to matrix (**)
   rows number of rows
   cols number of cols
   amin minimum of each column in a (make sure types of a and amin match)
   amax maximum of each column in a (make sure types of a and amin match)
   asum sum of each column in a (the mean is not computed because the / operation would then depend on the type of a)

*/
#define SUMA_MIN_MAX_SUM_MAT_COL(a, rows, cols, amin, amax, asum) { \
                  int m_IX, _JX;   \
                  for (m_IX = 0; m_IX < cols ; m_IX++) {   \
                     amax[m_IX]=a[0][m_IX];   \
                     amin[m_IX]=a[0][m_IX];   \
                     asum[m_IX]=a[0][m_IX];   \
                     for (_JX = 1 ; _JX < rows ; _JX++) { \
                        if (a[_JX][m_IX] > amax[m_IX]) amax[m_IX] = a[_JX][m_IX];\
                        if (a[_JX][m_IX] < amin[m_IX]) amin[m_IX] = a[_JX][m_IX];\
                        asum[m_IX] += a[_JX][m_IX];   \
                     }   \
                  }\
               }

/*! \def SUMA_MIN_MAX_SUM_VECMAT_COL(a, rows, cols, amin, amax, asum)
\brief  SUMA_MIN_MAX_SUM_VECMAT_COL macro for minimum, maximum and sum of each column in a matrix stored in vector format
   matrix    1 2 3 
            4 5 6 
   is stored as 1 2 3 4 5 6 ...
   
   a pointer to vector containing rwos x cols elements
   rows number of rows
   cols number of cols
   amin minimum of each column in a (make sure types of a and amin match)
   amax maximum of each column in a (make sure types of a and amin match)
   asum sum of each column in a (the mean is not computed because the / operation would then depend on the type of a)

*/
#define SUMA_MIN_MAX_SUM_VECMAT_COL(a, rows, cols, amin, amax, asum) { \
                  int m_IX, m_JX, m_id;   \
                  for (m_IX = 0; m_IX < cols ; m_IX++) {   \
                     amax[m_IX]=a[m_IX];   \
                     amin[m_IX]=a[m_IX];   \
                     asum[m_IX]=a[m_IX];   \
                     for (m_JX = 1 ; m_JX < rows ; m_JX++) { \
                        m_id = cols * m_JX + m_IX;   \
                        if (a[m_id] > amax[m_IX]) amax[m_IX] = a[m_id];\
                        if (a[m_id] < amin[m_IX]) amin[m_IX] = a[m_id];\
                        asum[m_IX] += a[m_id];   \
                     }   \
                  }   \
               }

/*!
SUMA_MAT_TO_VEC MACRO rearranges a matrix into a vector
one row after the other:

SUMA_MAT_TO_VEC(a,b,rows,cols,typea,typeb)

    a       pointer to input 2D matrix.
    b       pointer to resultant D matrix.
    rows    number of rows in matrix a
    cols    number of columns in matrix a
    typea   legal C type describing the type of a
    typeb   legal C type describing the type of b

*/
#define SUMA_MAT_TO_VEC(a,b,rows,cols,typea,typeb) {  \
                 typea **_AMX = (typea **)a;  \
                 typeb *_PTB = (typeb *)b;  \
                 typea *_PTA;  \
                 int m_IX,_JX;  \
                 for(m_IX = 0 ; m_IX < rows ; m_IX++) {  \
                     _PTA = _AMX[m_IX ];  \
                     for(_JX = 0 ; _JX < cols ; _JX++)  \
                         *_PTB++ = (typeb) (*_PTA++);  \
                 }  \
             }    


/*
SUMA_COPY_VEC macro: copies the contents of vector a into vector b

SUMA_COPY_VEC(a,b,len,typea,typeb)

    a       pointer to input vector.
    b       pointer to output vector.
    len     length of vectors (integer).
    typea   legal C type describing the type of a data.
    typeb   legal C type describing the type of b data.

*/

#define SUMA_COPY_VEC(a,b,len,typea,typeb) {  \
                       typea *_PTA = (typea *)a;  \
                       typeb *_PTB = (typeb *)b;  \
                       int m_IX;  \
                       for(m_IX = 0 ; m_IX < (len) ; m_IX++)  \
                           *(_PTB)++ = (typeb)(*(_PTA)++);  \
                    }

/*!
   SUMA_INIT_VEC macro: initializes values in a vector
   SUMA_INIT_VEC(a,len,val,typea)
*/
#define SUMA_INIT_VEC(a,len,val,typea) {  \
   int m_i; \
   for (m_i = 0; m_i < (len) ; m_i ++) \
      a[m_i] = (typea)val; \
}

/*!
SUMA_DOTP_VEC macro:

FORMS THE SUM OF PRODUCTS OF TWO VECTORS (a,b) AND
PUTS THE RESULT IN THE PREVIOUSLY DEFINED VARIABLE s.

SUMA_DOTP_VEC(a,b,s,len,typea,typeb)

    a       pointer to first vector.
    b       pointer to second vector.
    s       variable used to store result (not a pointer).
    len     length of vectors (integer).
    typea   legal C type describing the type of a data.
    typeb   legal C type describing the type of b data.

WARNING: The input data vectors are not cast to the type of s.
         This means that at least one of the input types must
         be able to represent the individual products without
         overflow.

*/

#define SUMA_DOTP_VEC(a,b,s,len,typea,typeb) {  \
                       typea *_PTA = a;  \
                       typeb *_PTB = b;  \
                       int m_IX;  \
                       s = (*_PTA++) * (*_PTB++);  \
                       for(m_IX = 1 ; m_IX < (len) ; m_IX++)  \
                           s += (*_PTA++) * (*_PTB++);  \
                   }

/*!
   \brief Macro to calculate the unit direction vector U from P1-->P2 and
   the distance Un between P1 and P2.
   If Un is 0, U is all zeros
   \param P1/P2 (float *) 3-elements arrays containing XYZ of P1 and P2
   \param U (float *) 3-elements array to contain unit direction vector
   \param Un (float) the norm of |P1--P2|
*/ 
#define SUMA_UNIT_VEC(P1, P2, U, Un){  \
      /* Calculate normalized unit vector of line formed by P1, P2 */   \
      U[0] = P2[0] - P1[0];   \
      U[1] = P2[1] - P1[1];   \
      U[2] = P2[2] - P1[2];   \
      Un = sqrt(U[0]*U[0] + U[1]*U[1] + U[2]*U[2]);   \
      if (Un) {   \
         U[0] /= Un; U[1] /= Un; U[2] /= Un; \
      }else {  \
         U[0] = U[1] = U[2] = 0; \
      }  \
   }  \
   

   /*!
   SUMA_MULT_MAT MACRO FOR MATRIX MULTIPLICATION:

   SUMA_MULT_MAT(a,b,c,rowsa,colsa,colsb,typea,typeb,typec)

       a       pointer to first matirx.
       b       pointer to second matrix.
       c       pointer to result matrix.
       rowsa   number of rows in matrix a
       colsa   number of columns in matrix a
       colsb   number of columns in matrix b
       typea   legal C type describing the type of a
       typeb   legal C type describing the type of b
       typec   legal C type describing the type of c

   */

   #define SUMA_MULT_MAT(a,b,c,rowsa,colsa,colsb,typea,typeb,typec) {  \
                    typea **_AMX = (typea **)a;  \
                    typeb **_BMX = (typeb **)b;  \
                    typec **_CMX = (typec **)c;  \
                    typea *_PTA;  \
                    typeb *_PTB;  \
                    typec *_PTC;  \
                    int m_IX,_JX,_KX;  \
                    for(m_IX = 0 ; m_IX < rowsa ; m_IX++) {  \
                        _PTC = _CMX[m_IX];  \
                        _PTB = _BMX[0];  \
                        for(_JX = 0 ; _JX < colsb ; _JX++) {  \
                            _PTA = _AMX[m_IX];  \
                            *_PTC = (*_PTA++) * (*_PTB++);  \
                            for(_KX = 1 ; _KX < colsa ; _KX++)  \
                                *_PTC += (*_PTA++)* _BMX[_KX][_JX];  \
                            _PTC++;  \
                        }  \
                    }  \
                }    


   /*!
   SUMA_ADD_MAT MACRO FOR MATRIX ADDITION:

   SUMA_ADD_MAT(a,b,c,rowsa,colsa,typea,typeb,typec)

       a       pointer to first matirx.
       b       pointer to second matrix.
       c       pointer to result matrix.
       rowsa   number of rows in matrix a
       colsa   number of columns in matrix a
       typea   legal C type describing the type of a
       typeb   legal C type describing the type of b
       typec   legal C type describing the type of c

   */

   #define SUMA_ADD_MAT(a,b,c,rowsa,colsa,typea,typeb,typec) {  \
                    typea **_AMX = (typea **)a;  \
                    typeb **_BMX = (typeb **)b;  \
                    typec **_CMX = (typec **)c;  \
                    int m_IX,_JX;  \
                    for(m_IX = 0 ; m_IX < rowsa ; m_IX++) {  \
                        for(_JX = 0 ; _JX < colsa ; _JX++) {  \
                            _CMX[m_IX][_JX] = _AMX[m_IX][_JX] + _BMX[m_IX][_JX];  \
                        }  \
                    }  \
                }    

   /*!
   SUMA_SUB_MAT MACRO FOR MATRIX SUBTRACTION:

   SUMA_SUB_MAT(a,b,c,rowsa,colsa,typea,typeb,typec)

       a       pointer to first matirx.
       b       pointer to second matrix.
       c       pointer to result matrix.
       rowsa   number of rows in matrix a
       colsa   number of columns in matrix a
       typea   legal C type describing the type of a
       typeb   legal C type describing the type of b
       typec   legal C type describing the type of c

   */

   #define SUMA_SUB_MAT(a,b,c,rowsa,colsa,typea,typeb,typec) {  \
                    typea **_AMX = (typea **)a;  \
                    typeb **_BMX = (typeb **)b;  \
                    typec **_CMX = (typec **)c;  \
                    int m_IX,_JX;  \
                    for(m_IX = 0 ; m_IX < rowsa ; m_IX++) {  \
                        for(_JX = 0 ; _JX < colsa ; _JX++) {  \
                            _CMX[m_IX][_JX] = _AMX[m_IX][_JX] - _BMX[m_IX][_JX];  \
                        }  \
                    }  \
                }    


   /*!
   SUMA_TRANSP_MAT MACRO FOR MATRIX TRANSPOSE:

   SUMA_TRANSP_MAT(a,b,rowsa,colsa,typea,typeb)

       a       pointer to first matirx.
       b       pointer to result matrix.
       rowsa   number of rows in matrix a
       colsa   number of columns in matrix a
       typea   legal C type describing the type of a
       typeb   legal C type describing the type of b

   */

   #define SUMA_TRANSP_MAT(a,b,rowsa,colsa,typea,typeb) {  \
                    typea **_AMX = (typea **)a;  \
                    typeb **_BMX = (typeb **)b;  \
                    int m_IX,_JX;  \
                    for(m_IX = 0 ; m_IX < rowsa ; m_IX++) {  \
                        for(_JX = 0 ; _JX < colsa ; _JX++) {  \
                            _BMX[_JX][m_IX] = _AMX[m_IX][_JX];  \
                        }  \
                    }  \
                }    

   /*!
      SUMA_RGBmat_2_GLCOLAR4 copies an N x 3 RGB matrix into a 4N x 1 GL color array format
      SUMA_RGBmat_2_GLCOLAR4(RGBmat, glcolar, N)   
      RGBmat (float **) N x 3 matrix of RGB values
      glcolar (GLfloat *) (4 N) x 1 vector 
      *** Mar 17 04:
      Added the SUMA_RGBvec versions using RGBvec instead of RGBmat.
   */
   
   #define SUMA_RGBmat_2_GLCOLAR4(RGBmat, glcolar, nrgb) {\
      int m_I, m_I4 = 0; \
      for (m_I=0; m_I < nrgb; ++m_I) {\
         glcolar[m_I4] = RGBmat[m_I][0]; ++m_I4;\
         glcolar[m_I4] = RGBmat[m_I][1]; ++m_I4;\
         glcolar[m_I4] = RGBmat[m_I][2]; ++m_I4;\
         ++m_I4;\
      }\
   }
   #define SUMA_RGBvec_2_GLCOLAR4(RGBvec, glcolar, nrgb) {\
      int m_I, m_I4 = 0, m_I3=0; \
      for (m_I=0; m_I < nrgb; ++m_I) {\
         glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
         glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
         glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
         ++m_I4;\
      }\
   }
   
   /*!
      SUMA_GLCOLAR4_2_RGBmat copies 4N x 1 GL color array into an N x 3 RGB matrix format
      
      SUMA_GLCOLAR4_2_RGBmat (glcolar, RGBmat, N)   
      glcolar (GLfloat *) (4 N) x 1 vector 
      RGBmat (float **) N x 3 matrix of RGB values
      *** Mar 17 04:
      Added the SUMA_RGBvec versions using RGBvec instead of RGBmat.
   */
   
   #define SUMA_GLCOLAR4_2_RGBmat(glcolar, RGBmat, nrgb) {\
      int m_I, m_I4 = 0; \
      for (m_I=0; m_I < nrgb; ++m_I) {\
         RGBmat[m_I][0] = glcolar[m_I4]; ++m_I4;\
         RGBmat[m_I][1] = glcolar[m_I4]; ++m_I4;\
         RGBmat[m_I][2] = glcolar[m_I4]; ++m_I4;\
         ++m_I4;\
      }\
   }
   #define SUMA_GLCOLAR4_2_RGBvec(glcolar, RGBvec, nrgb) {\
      int m_I, m_I4 = 0, m_I3=0; \
      for (m_I=0; m_I < nrgb; ++m_I) {\
         RGBvec[m_I3] = glcolar[m_I4]; ++m_I4; ++m_I3;\
         RGBvec[m_I3] = glcolar[m_I4]; ++m_I4; ++m_I3;\
         RGBvec[m_I3] = glcolar[m_I4]; ++m_I4; ++m_I3;\
         ++m_I4;\
      }\
   }
   
   
   /*! 
      SUMA_FillBlanks_GLCOLAR4
      fills nodes that received no color with the Nocolor Color
      SUMA_FillBlanks_GLCOLAR4(isColored, N_Nodes, R, G, B, glcolar)
      isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored.
      N_Nodes (int) total number of nodes 
      R, G, B (float 0..1) RGB values of NoColor color
      glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
   */
   
   #define SUMA_FillBlanks_GLCOLAR4(isColored, N_Nodes, R, G, B, glcolar) {\
      int m_I, m_I4; \
      for (m_I=0; m_I < N_Nodes; ++m_I) {\
         if (!isColored[m_I]) {\
            m_I4 = 4*m_I; \
            glcolar[m_I4] = R; ++m_I4;\
            glcolar[m_I4] = G; ++m_I4;\
            glcolar[m_I4] = B; ++m_I4;\
         }\
      }\
   }

   /*!
      This macro used to be called: SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4_opacity
      SUMA_RGB_FnGnL_AR4op_opacity 
      copies an N x 3 RGB matrix into a 4N x 1 GL color array format
      F (Full) means that N is equal to all the nodes in the surface
      nG (NoGlob) means no opacity is applied to the color values (fully opaque)
      nL (NoLocal) means no local gain (per node) is applied to the color values
      
      SUMA_RGB_FnGnL_AR4op(RGBmat, glcolar, N, add)   
      RGBmat (float **) N x 3 matrix of RGB values
      glcolar (GLfloat *) (4 N) x 1 vector 
      isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
      *** Mar 17 04:
      Added the SUMA_RGBv versions using RGBvec instead of RGBmat.
      
   */
   
   #define SUMA_RGB_FnGnL_AR4op(RGBmat, glcolar, nrgb, isColored) {\
      int m_I, m_I4 = 0; \
         for (m_I=0; m_I < nrgb; ++m_I) {\
            isColored[m_I] = YUP;\
            glcolar[m_I4] = RGBmat[m_I][0]; ++m_I4;\
            glcolar[m_I4] = RGBmat[m_I][1]; ++m_I4;\
            glcolar[m_I4] = RGBmat[m_I][2]; ++m_I4;\
            ++m_I4;\
         }\
   }
   
   #define SUMA_RGBv_FnGnL_AR4op(RGBvec, glcolar, nrgb, isColored) {\
      int m_I, m_I4 = 0, m_I3=0; \
         for (m_I=0; m_I < nrgb; ++m_I) {\
            isColored[m_I] = YUP;\
            glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
            glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
            glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
            ++m_I4;\
         }\
   }
   /*!
      This macro used to be called: SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4_opacity
      SUMA_RGB_FGnL_AR4op 
      copies an N x 3 RGB matrix into a 4N x 1 GL color array format
      F (Full) means that N is equal to all the nodes in the surface
      G (Glob) means an opacity is applied to the color values
      nL (NoLocal) means no local gain (per node) is applied to the color values
      
      SUMA_RGB_FGnL_AR4op(RGBmat, glcolar, N, opacity, add)   
      RGBmat (float **) N x 3 matrix of RGB values
      glcolar (GLfloat *) (4 N) x 1 vector 
      opacity (float) an opacity factor applied to each color R, G, B values in the entire list before adding it 
            to a pre-exising color opacity is not applied to the color of nodes that had not been colored thus far.
      isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
      
      *** Dec 04 03: 
      Variant SUMA_RGB_FGnL_AR4op2
      removed opacity scaling of RGBmat[m_I] and added clipping to 1.0 
      *** Mar 17 04:
      Added the SUMA_RGBv versions using RGBvec instead of RGBmat.
   */
   
   #define SUMA_RGB_FGnL_AR4op(RGBmat, glcolar, nrgb, opacity, isColored) {\
      int m_I, m_I4 = 0; \
      float m_of;\
      m_of = 1-opacity;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (isColored[m_I]) {   \
               glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBmat[m_I][0]; ++m_I4;\
               glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBmat[m_I][1]; ++m_I4;\
               glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBmat[m_I][2]; ++m_I4;\
            } else { /* not yet colored */\
               glcolar[m_I4] = RGBmat[m_I][0]; ++m_I4;\
               glcolar[m_I4] = RGBmat[m_I][1]; ++m_I4;\
               glcolar[m_I4] = RGBmat[m_I][2]; ++m_I4;\
               isColored[m_I] = YUP;\
            }  \
            ++m_I4;\
         }\
   }
   #define SUMA_RGBv_FGnL_AR4op(RGBvec, glcolar, nrgb, opacity, isColored) {\
      int m_I, m_I4 = 0, m_I3 = 0; \
      float m_of;\
      m_of = 1-opacity;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (isColored[m_I]) {   \
               glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBvec[m_I3]; ++m_I4; ++m_I3;\
            } else { /* not yet colored */\
               glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
               isColored[m_I] = YUP;\
            }  \
            ++m_I4;\
         }\
   }
   #define SUMA_RGB_FGnL_AR4op2(RGBmat, glcolar, nrgb, opacity, isColored) {\
      int m_I, m_I4 = 0; \
      float m_of;\
      m_of = 1-opacity;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (isColored[m_I]) {   \
               glcolar[m_I4] = m_of * glcolar[m_I4] + RGBmat[m_I][0]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
               glcolar[m_I4] = m_of * glcolar[m_I4] + RGBmat[m_I][1]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
               glcolar[m_I4] = m_of * glcolar[m_I4] + RGBmat[m_I][2]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
            } else { /* not yet colored */\
               glcolar[m_I4] = RGBmat[m_I][0]; ++m_I4;\
               glcolar[m_I4] = RGBmat[m_I][1]; ++m_I4;\
               glcolar[m_I4] = RGBmat[m_I][2]; ++m_I4;\
               isColored[m_I] = YUP;\
            }  \
            ++m_I4;\
         }\
   }   
   #define SUMA_RGBv_FGnL_AR4op2(RGBvec, glcolar, nrgb, opacity, isColored) {\
      int m_I, m_I4 = 0, m_I3 = 0; \
      float m_of;\
      m_of = 1-opacity;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (isColored[m_I]) {   \
               glcolar[m_I4] = m_of * glcolar[m_I4] + RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
               glcolar[m_I4] = m_of * glcolar[m_I4] + RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
               glcolar[m_I4] = m_of * glcolar[m_I4] + RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
            } else { /* not yet colored */\
               glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
               isColored[m_I] = YUP;\
            }  \
            ++m_I4;\
         }\
   }   
   /*!
    This macro used to be called: SUMA_RGBmat_FullGlobLoc2_GLCOLAR4_opacity
         but name was too long for some compilers 
      
      SUMA_RGB_FGL_AR4op 
      copies an N x 3 RGB matrix into a 4N x 1 GL color array format
      F (Full) means that N is equal to all the nodes in the surface
      G (Glob) means an opacity is applied to the color values
      L (Local) means a local gain (per node) is applied to the color values
      
      SUMA_RGB_FGL_AR4op(RGBmat, glcolar, N, opacity, locgain, add)   
      RGBmat (float **) N x 3 matrix of RGB values
      glcolar (GLfloat *) (4 N) x 1 vector 
      opacity (float) an opacity factor applied to each color R, G, B values in the entire list before adding it 
            to a pre-exising color opacity is not applied to the color of nodes that had not been colored thus far.
      locgain (float *) a N x 1 vector of gains applied to their respective nodes 
      isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
   
      *** Dec 04 03: 
      Variant SUMA_RGB_FGL_AR4op2
      removed opacity from m_of and added clipping to 1.0 
      *** Mar 17 04:
      Added the SUMA_RGBv versions using RGBvec instead of RGBmat.
   */
   
   #define SUMA_RGB_FGL_AR4op(RGBmat, glcolar, nrgb, opacity, locgain, isColored) {\
      int m_I, m_I4 = 0; \
      float m_of, m_of2;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (isColored[m_I]) {   \
               m_of = locgain[m_I] * opacity; /* Dec 04 03 changed from locgain[m_I] * opacity; */\
               m_of2 = (1-opacity);\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][0]; ++m_I4;\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][1]; ++m_I4;\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][2]; ++m_I4;\
            }  else { /* not yet colored */\
               glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][0]; ++m_I4;\
               glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][1]; ++m_I4;\
               glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][2]; ++m_I4;\
               isColored[m_I] = YUP;\
            }  \
            ++m_I4;\
         }\
   }
   #define SUMA_RGBv_FGL_AR4op(RGBvec, glcolar, nrgb, opacity, locgain, isColored) {\
      int m_I, m_I4 = 0, m_I3 = 0; \
      float m_of, m_of2;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (isColored[m_I]) {   \
               m_of = locgain[m_I] * opacity; /* Dec 04 03 changed from locgain[m_I] * opacity; */\
               m_of2 = (1-opacity);\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; ++m_I4; ++m_I3;\
            }  else { /* not yet colored */\
               glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               isColored[m_I] = YUP;\
            }  \
            ++m_I4;\
         }\
   }
   #define SUMA_RGB_FGL_AR4op2(RGBmat, glcolar, nrgb, opacity, locgain, isColored) {\
      int m_I, m_I4 = 0; \
      float m_of, m_of2;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (isColored[m_I]) {   \
               m_of = locgain[m_I]; /* Dec 04 03 changed from locgain[m_I] * opacity; */\
               m_of2 = (1-opacity);\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][0]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][1]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][2]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
            }  else { /* not yet colored */\
               glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][0]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
               glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][1]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
               glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][2]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
               isColored[m_I] = YUP;\
            }  \
            ++m_I4;\
         }\
   }   
   #define SUMA_RGBv_FGL_AR4op2(RGBvec, glcolar, nrgb, opacity, locgain, isColored) {\
      int m_I, m_I4 = 0, m_I3=0; \
      float m_of, m_of2;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (isColored[m_I]) {   \
               m_of = locgain[m_I]; /* Dec 04 03 changed from locgain[m_I] * opacity; */\
               m_of2 = (1-opacity);\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
               glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
            }  else { /* not yet colored */\
               glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
               glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
               glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
               isColored[m_I] = YUP;\
            }  \
            ++m_I4;\
         }\
   }   
   /*!
      This macro used to be called: SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4_opacity 

      SUMA_RGB_FnGL_AR4op 
      copies an N x 3 RGB matrix into a 4N x 1 GL color array format
      F (Full) means that N is equal to all the nodes in the surface
      nG (NoGlob) means no opacity is applied to the color values (fully opaque)
      L (Local) means a local gain (per node) is applied to the color values
      
      SUMA_RGB_FnGL_AR4op(RGBmat, glcolar, N, locgain, add)   
      RGBmat (float **) N x 3 matrix of RGB values
      glcolar (GLfloat *) (4 N) x 1 vector 
      locgain (float *) a N x 1 vector of gains applied to their respective nodes 
      isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
      *** Mar 17 04:
      Added the SUMA_RGBv versions using RGBvec instead of RGBmat.
   */
   
   #define SUMA_RGB_FnGL_AR4op(RGBmat, glcolar, nrgb, locgain, isColored) {\
      int m_I, m_I4 = 0; \
      float m_of;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][0]; ++m_I4;\
            glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][1]; ++m_I4;\
            glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][2]; ++m_I4;\
            isColored[m_I] = YUP;\
            ++m_I4;\
         }\
   }
   #define SUMA_RGBv_FnGL_AR4op(RGBvec, glcolar, nrgb, locgain, isColored) {\
      int m_I, m_I4 = 0, m_I3=0; \
      float m_of;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
            glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
            glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
            isColored[m_I] = YUP;\
            ++m_I4;\
         }\
   }
   /*!
      This macro used to be called: SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4_opacity 
      SUMA_RGB_PnGnL_AR4op 
      copies an N x 3 RGB matrix into a 4N x 1 GL color array format
      P (Part) means that colors are specified for some of the nodes only N < N_Nodes
      nG (NoGlob) means no opacity is applied to the color values (fully opaque)
      nL (NoLocal) means no local gain (per node) is applied to the color values
      
      SUMA_RGB_PnGnL_AR4op(RGBmat, NodeId, glcolar, N, isColored, N_Nodes)   
      RGBmat (float **) N x 3 matrix of RGB values
      NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
      glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
      isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
      when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
      *** Mar 17 04:
      Added the SUMA_RGBv versions using RGBvec instead of RGBmat.
   */
   
   #define SUMA_RGB_PnGnL_AR4op(RGBmat, NodeId, glcolar, nrgb, isColored, N_Node) {\
      int m_I, m_I4 = 0; \
         for (m_I=0; m_I < nrgb; ++m_I) {\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  glcolar[m_I4] = RGBmat[m_I][0]; ++m_I4;\
                  glcolar[m_I4] = RGBmat[m_I][1]; ++m_I4;\
                  glcolar[m_I4] = RGBmat[m_I][2]; ++m_I4;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }\
   }
   
   #define SUMA_RGBv_PnGnL_AR4op(RGBvec, NodeId, glcolar, nrgb, isColored, N_Node) {\
      int m_I, m_I4 = 0, m_I3=0; \
         for (m_I=0; m_I < nrgb; ++m_I) {\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_I3 = 3*m_I;   \
                  glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }\
   }
   /*!
      This macro used to be called: SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4_opacity
      SUMA_RGB_PGnL_AR4op 
      copies an N x 3 RGB matrix into a 4N x 1 GL color array format
      P (Part) means that colors are specified for some of the nodes only N < N_Nodes
      G (Glob) means an opacity is applied to the color values
      nL (NoLocal) means no local gain (per node) is applied to the color values
      
      SUMA_RGB_PGnL_AR4op(RGBmat, NodeId, glcolar, N, isColored, opacity, N_Nodes)   
      RGBmat (float **) N x 3 matrix of RGB values
      NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
      glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
      isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
      when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
      opacity (float) an opacity factor applied to each color R, G, B values in the entire list before adding it 
            to a pre-exising color opacity is not applied to the color of nodes that had not been colored thus far.
      
      *** Dec. 04 03: 
      Variant SUMA_RGB_PGnL_AR4op2
         Instead of mixing like this: Col_new = (1 -  opacity)*Col_1 + opacity *Col_2;
         I am using Col_new = (1 -  opacity)*Col_1 +  Col_2; if (Col_new > 1) Col_new = 1
      *** Mar 17 04:
      Added the SUMA_RGBv versions using RGBvec instead of RGBmat.
   
   */
   
   #define SUMA_RGB_PGnL_AR4op(RGBmat, NodeId, glcolar, nrgb, isColored, opacity, N_Node) {\
      int m_I, m_I4 = 0, m_II; \
      float m_of;\
         m_of = (1 - opacity); \
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is, Opacity gain should not be applied Wed Apr  2 17:31:33 EST 2003*/\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  glcolar[m_I4] = RGBmat[m_I][0]; ++m_I4;\
                  glcolar[m_I4] = RGBmat[m_I][1]; ++m_I4;\
                  glcolar[m_I4] = RGBmat[m_I][2]; ++m_I4;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }else {/* mixing to be done */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBmat[m_I][0];  ++m_I4;\
                  glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBmat[m_I][1];  ++m_I4;\
                  glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBmat[m_I][2];  ++m_I4;\
               }\
            }\
         }\
   }
   #define SUMA_RGBv_PGnL_AR4op(RGBvec, NodeId, glcolar, nrgb, isColored, opacity, N_Node) {\
      int m_I, m_I4 = 0, m_II, m_I3=0; \
      float m_of;\
         m_of = (1 - opacity); \
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is, Opacity gain should not be applied Wed Apr  2 17:31:33 EST 2003*/\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_I3 = 3*m_I; \
                  glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }else {/* mixing to be done */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_I3 = 3*m_I; \
                  glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBvec[m_I3];  ++m_I4; ++m_I3;\
                  glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBvec[m_I3];  ++m_I4; ++m_I3;\
                  glcolar[m_I4] = m_of * glcolar[m_I4] + opacity * RGBvec[m_I3];  ++m_I4; ++m_I3;\
               }\
            }\
         }\
   }
   
   #define SUMA_RGB_PGnL_AR4op2(RGBmat, NodeId, glcolar, nrgb, isColored, opacity, N_Node) {\
      int m_I, m_I4 = 0, m_II; \
      float m_of;\
         m_of = (1 - opacity); \
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is, Opacity gain should not be applied Wed Apr  2 17:31:33 EST 2003*/\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  glcolar[m_I4] = RGBmat[m_I][0]; ++m_I4;\
                  glcolar[m_I4] = RGBmat[m_I][1]; ++m_I4;\
                  glcolar[m_I4] = RGBmat[m_I][2]; ++m_I4;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }else {/* mixing to be done */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  glcolar[m_I4] = m_of * glcolar[m_I4] + RGBmat[m_I][0]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
                  glcolar[m_I4] = m_of * glcolar[m_I4] + RGBmat[m_I][1]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
                  glcolar[m_I4] = m_of * glcolar[m_I4] + RGBmat[m_I][2]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
               }\
            }\
         }\
   }
   #define SUMA_RGBv_PGnL_AR4op2(RGBvec, NodeId, glcolar, nrgb, isColored, opacity, N_Node) {\
      int m_I, m_I4 = 0, m_II, m_I3=0; \
      float m_of;\
         m_of = (1 - opacity); \
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is, Opacity gain should not be applied Wed Apr  2 17:31:33 EST 2003*/\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_I3 = 3*m_I; \
                  glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }else {/* mixing to be done */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_I3 = 3*m_I; \
                  glcolar[m_I4] = m_of * glcolar[m_I4] + RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
                  glcolar[m_I4] = m_of * glcolar[m_I4] + RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
                  glcolar[m_I4] = m_of * glcolar[m_I4] + RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
               }\
            }\
         }\
   }
   /*!
      This macro used to be called: SUMA_RGBmat_PartGlobLoc2_GLCOLAR4_opacity
      
      SUMA_RGB_PGL_AR4op 
      copies an N x 3 RGB matrix into a 4N x 1 GL color array format
      P (Part) means that colors are specified for some of the nodes only N < N_Nodes
      G (Glob) means an opacity is applied to the color values
      L (Local) means a local gain (per node) is applied to the color values
      
      SUMA_RGB_PGL_AR4op(RGBmat, NodeId, glcolar, N, isColored, opacity, locgain, add, N_Nodes)   
      RGBmat (float **) N x 3 matrix of RGB values
      NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
      glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
      isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
      when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
      opacity (float) an opacity factor applied to each color R, G, B values in the entire list before adding it 
            to a pre-exising color opacity is not applied to the color of nodes that had not been colored thus far.
      locgain (float *)  N x 1 vector of gains applied to their respective nodes 
      
      *** Dec. 04 03: 
      Variant SUMA_RGB_PGL_AR4op2
         Instead of mixing like this: Col_new = (1 -  opacity) *Col_1 + opacity * locgain *Col_2;
         I am using Col_new = (1 -  opacity)*Col_1 +  locgain * Col_2; if (Col_new > 1) Col_new = 1
       *** Mar 17 04:
      Added the SUMA_RGBv versions using RGBvec instead of RGBmat.
  
   */
   
   #define SUMA_RGB_PGL_AR4op(RGBmat, NodeId, glcolar, nrgb, isColored, opacity, locgain, N_Node) {\
      int m_I, m_I4 = 0; \
      float m_of, m_of2;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][0]; ++m_I4;\
                  glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][1]; ++m_I4;\
                  glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][2]; ++m_I4;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }else { /* mixing to be done */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_of = (locgain[m_I]  * opacity); /* Dec. 04 03, changed from locgain[m_I] * opacity */\
                  m_of2 = (1 - opacity);\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][0]; ++m_I4;\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][1]; ++m_I4;\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][2]; ++m_I4;\
               }\
            }\
         }\
   }
   #define SUMA_RGBv_PGL_AR4op(RGBvec, NodeId, glcolar, nrgb, isColored, opacity, locgain, N_Node) {\
      int m_I, m_I4 = 0, m_I3=0; \
      float m_of, m_of2;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_I3 = 3*m_I; \
                  glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }else { /* mixing to be done */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_I3 = 3*m_I; \
                  m_of = (locgain[m_I]  * opacity); /* Dec. 04 03, changed from locgain[m_I] * opacity */\
                  m_of2 = (1 - opacity);\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               }\
            }\
         }\
   }

   #define SUMA_RGB_PGL_AR4op2(RGBmat, NodeId, glcolar, nrgb, isColored, opacity, locgain, N_Node) {\
      int m_I, m_I4 = 0; \
      float m_of, m_of2;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][0]; ++m_I4;\
                  glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][1]; ++m_I4;\
                  glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][2]; ++m_I4;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }else { /* mixing to be done */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_of = (locgain[m_I]); /* Dec. 04 03, changed from locgain[m_I] * opacity */\
                  m_of2 = (1 - opacity);\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][0]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][1]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][2]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4;\
               }\
            }\
         }\
   }
   #define SUMA_RGBv_PGL_AR4op2(RGBvec, NodeId, glcolar, nrgb, isColored, opacity, locgain, N_Node) {\
      int m_I, m_I4 = 0, m_I3=0; \
      float m_of, m_of2;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_I3 = 3*m_I; \
                  glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
                  isColored[NodeId[m_I]] = YUP;\
               }\
            }else { /* mixing to be done */\
               if (NodeId[m_I] < N_Node) {\
                  m_I4 = 4*NodeId[m_I]; \
                  m_I3 = 3*m_I; \
                  m_of = (locgain[m_I]); /* Dec. 04 03, changed from locgain[m_I] * opacity */\
                  m_of2 = (1 - opacity);\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
                  glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBvec[m_I3]; SUMA_CLIP_UB(glcolar[m_I4], 1.0); ++m_I4; ++m_I3;\
               }\
            }\
         }\
   }
   
   /*!
      This macro used to be called: SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4_opacity
      SUMA_RGB_PnGL_AR4op 
      copies an N x 3 RGB matrix into a 4N x 1 GL color array format
      P (Part) means that colors are specified for some of the nodes only N < N_Nodes
      nG (NoGlob) means no opacity is applied to the color values (fully opaque)
      L (Local) means a local gain (per node) is applied to the color values
      
      SUMA_RGB_PnGL_AR4op(RGBmat, NodeId, glcolar, N, isColored, locgain, add, N_Nodes)   
      RGBmat (float **) N x 3 matrix of RGB values
      NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
      glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
      isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
      when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
      locgain (float *)  N x 1 vector of gains applied to their respective nodes 
      
      *** Mar 17 04:
      Added the SUMA_RGBv versions using RGBvec instead of RGBmat.
   */
   #define SUMA_RGB_PnGL_AR4op(RGBmat, NodeId, glcolar, nrgb, isColored,  locgain, N_Node) {\
      int m_I, m_I4 = 0; \
      float m_of;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (NodeId[m_I] < N_Node) {\
               m_I4 = 4*NodeId[m_I]; \
               glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][0]; ++m_I4;\
               glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][1]; ++m_I4;\
               glcolar[m_I4] = locgain[m_I] * RGBmat[m_I][2]; ++m_I4;\
               isColored[NodeId[m_I]] = YUP;\
            }\
         }\
   }
   #define SUMA_RGBv_PnGL_AR4op(RGBvec, NodeId, glcolar, nrgb, isColored,  locgain, N_Node) {\
      int m_I, m_I4 = 0, m_I3=0; \
      float m_of;\
         for (m_I=0; m_I < nrgb; ++m_I) {\
            if (NodeId[m_I] < N_Node) {\
               m_I4 = 4*NodeId[m_I]; \
               m_I3 = 3*m_I; \
               glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               glcolar[m_I4] = locgain[m_I] * RGBvec[m_I3]; ++m_I4; ++m_I3;\
               isColored[NodeId[m_I]] = YUP;\
            }\
         }\
   }
#endif

