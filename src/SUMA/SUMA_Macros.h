#ifndef SUMA_MACROSm_INCLUDED
#define SUMA_MACROSm_INCLUDED

#define SUMA_DBG_IN_NOTIFY(m_fname) { \
	int m_i;\
	++SUMAg_CF->InOut_Level;	\
	for (m_i=0; m_i < SUMAg_CF->InOut_Level; ++m_i) fprintf (SUMA_STDERR," ");\
	fprintf (SUMA_STDERR,"--dbg: Entered %s (lvl %d).\n", m_fname, SUMAg_CF->InOut_Level); \
}

#define SUMA_DBG_OUT_NOTIFY(m_fname) { \
	int m_i;\
	for (m_i=0; m_i < SUMAg_CF->InOut_Level; ++m_i) fprintf (SUMA_STDERR," ");\
	fprintf (SUMA_STDERR,"--dbg: Left    %s (lvl %d).\n", m_fname, SUMAg_CF->InOut_Level); \
	--SUMAg_CF->InOut_Level;	\
}

#define SUMA_RETURN(m_rvar) {\
	if (SUMAg_CF->InOut_Notify) { SUMA_DBG_OUT_NOTIFY(FuncName); }\
	return(m_rvar);\
}

#define SUMA_RETURNe  {\
	if (SUMAg_CF->InOut_Notify) { SUMA_DBG_OUT_NOTIFY(FuncName); }\
	return ;\
}


/*! \def SUMA_ANY_WIDGET2SV(m_w, m_sv, m_svi)
\brief SUMA_ANY_WIDGET2SV macro for determining the SurfaceViewer structure containing any of the following widgets: GLXAREA, TOPLEVEL, FORM, FRAME. The macro searches all the SurfaceViewer structures in SUMAg_SVv.

	m_w the widget in question
	m_sv a pointer to SUMA_SurfaceViewer structure. This pointer is NULL if no matching SurfaceViewer structure is found in SUMAg_SVv
	m_svi the index of m_sv in SUMAg_SVv vector of Surface Viewer structures. m_sv = &(SUMAg_SVv[m_svi]). -1 if no match was found 
*/

#define SUMA_ANY_WIDGET2SV(m_w, m_sv, m_svi) {\
	int m_i = 0;	\
	m_sv = NULL;	\
	m_svi = -1;	\
	while (m_i < SUMA_MAX_SURF_VIEWERS) {	\
		if (SUMAg_SVv[m_i].X->GLXAREA == m_w || SUMAg_SVv[m_i].X->TOPLEVEL == m_w || SUMAg_SVv[m_i].X->FORM == m_w || SUMAg_SVv[m_i].X->FRAME == m_w) {	\
			m_svi = m_i;	\
			m_sv = &(SUMAg_SVv[m_i]);	\
			m_i = SUMA_MAX_SURF_VIEWERS; \
		}  else {	\
			++m_i;	\
		}	\
	}	\
}

/*! \def SUMA_GLXAREA_WIDGET2SV(m_w, m_sv, m_svi)
\brief SUMA_GLXAREA_WIDGET2SV macro for determining the SurfaceViewer structure containing the widget: GLXAREA. The macro searches all the SurfaceViewer structures in SUMAg_SVv.

	m_w the widget in question
	m_sv a pointer to SUMA_SurfaceViewer structure. This pointer is NULL if no matching SurfaceViewer structure is found in SUMAg_SVv
	m_svi the index of m_sv in SUMAg_SVv vector of Surface Viewer structures. m_sv = &(SUMAg_SVv[m_svi]). -1 if no match was found 
*/

#define SUMA_GLXAREA_WIDGET2SV(m_w, m_sv, m_svi) {\
	int m_i = 0;	\
	m_sv = NULL;	\
	m_svi = -1;	\
	while (m_i < SUMA_MAX_SURF_VIEWERS) {	\
		if (SUMAg_SVv[m_i].X->GLXAREA == m_w) {	\
			m_svi = m_i;	\
			m_sv = &(SUMAg_SVv[m_i]);	\
			m_i = SUMA_MAX_SURF_VIEWERS;	\
		}  else {	\
			++m_i;	\
		}	\
	}	\
}


/* Many of these macros are taken from DSP_in_C examples in
C Language Algorithms for Digital Signal Processing 
by
Bruce Kimball, Paul Embree and Bruce Kimble 
1991, Prentice Hall
*/

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
		norm += a[m_I]*a[m_I];	 \
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
		if (a[m_I] < amin) amin = a[m_I];	 \
	} \
}

/*! \def SUMA_MIN_LOC_VEC(a,nel)
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
			amin = a[m_I];	 \
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
		if (a[m_I] > amax) amax = a[m_I];	 \
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
		if (a[m_I] > amax) { amax = a[m_I]; amaxloc = m_I; }	 \
		else { if (a[m_I] < amin) { amin = a[m_I]; aminloc = m_I; } }	 \
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

	a		pointer to input vector
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
	a 		pointer to first vector 
	b 		pointer to second vector
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
						int m_IX, _JX;	\
						for (m_IX = 0; m_IX < cols ; m_IX++) {	\
							amin[m_IX]=a[0][m_IX];	\
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
						int m_IX, _JX;	\
						for (m_IX = 0; m_IX < cols ; m_IX++) {	\
							amax[m_IX]=a[0][m_IX];	\
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
						int m_IX, _JX;	\
						for (m_IX = 0; m_IX < cols ; m_IX++) {	\
							amax[m_IX]=a[0][m_IX];	\
							amin[m_IX]=a[0][m_IX];	\
							asum[m_IX]=a[0][m_IX];	\
							for (_JX = 1 ; _JX < rows ; _JX++) { \
								if (a[_JX][m_IX] > amax[m_IX]) amax[m_IX] = a[_JX][m_IX];\
								if (a[_JX][m_IX] < amin[m_IX]) amin[m_IX] = a[_JX][m_IX];\
								asum[m_IX] += a[_JX][m_IX];	\
							}	\
						}\
					}

/*! \def SUMA_MIN_MAX_SUM_VECMAT_COL(a, rows, cols, amin, amax, asum)
\brief  SUMA_MIN_MAX_SUM_VECMAT_COL macro for minimum, maximum and sum of each column in a matrix stored in vector format
	matrix 	1 2 3 
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
						int m_IX, m_JX, m_id;	\
						for (m_IX = 0; m_IX < cols ; m_IX++) {	\
							amax[m_IX]=a[m_IX];	\
							amin[m_IX]=a[m_IX];	\
							asum[m_IX]=a[m_IX];	\
							for (m_JX = 1 ; m_JX < rows ; m_JX++) { \
								m_id = cols * m_JX + m_IX;	\
								if (a[m_id] > amax[m_IX]) amax[m_IX] = a[m_id];\
								if (a[m_id] < amin[m_IX]) amin[m_IX] = a[m_id];\
								asum[m_IX] += a[m_id];	\
							}	\
						}	\
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
SUMA_COPY_VEC macro:

copies the contents of vector a into vector b

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
	
	/*!
		SUMA_GLCOLAR4_2_RGBmat copies 4N x 1 GL color array into an N x 3 RGB matrix format
		
		SUMA_GLCOLAR4_2_RGBmat (glcolar, RGBmat, N)   
		glcolar (GLfloat *) (4 N) x 1 vector 
		RGBmat (float **) N x 3 matrix of RGB values
	*/
	
	#define SUMA_RGBmat2GLCOLAR4(glcolar, RGBmat, nrgb) {\
		int m_I, m_I4 = 0; \
		for (m_I=0; m_I < nrgb; ++m_I) {\
			RGBmat[m_I][0] = glcolar[m_I4]; ++m_I4;\
			RGBmat[m_I][1] = glcolar[m_I4]; ++m_I4;\
			RGBmat[m_I][2] = glcolar[m_I4]; ++m_I4;\
			++m_I4;\
		}\
	}
	
	/*!
		SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Full means that N is equal to all the nodes in the surface
		NoGlob means no global factor is applied to the color values
		NoLocal means no local factor (per node) is applied to the color values
		
		SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4(RGBmat, glcolar, N, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		add (SUMA_Boolean) flag indicating that new color is added to previous value in glcolar
	*/
	
	#define SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4(RGBmat, glcolar, nrgb, add) {\
		int m_I, m_I4 = 0; \
		if (!add) {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				glcolar[m_I4] = RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] = RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] = RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
		} else {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				glcolar[m_I4] += RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] += RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] += RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
		}\
	}
	
	/*!
		SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Full means that N is equal to all the nodes in the surface
		Glob means a global factor is applied to the color values
		NoLocal means no local factor (per node) is applied to the color values
		
		SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4(RGBmat, glcolar, N, fact, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		fact (float) a factor applied to each color R, G, B values in the entire list
		add (SUMA_Boolean) flag indicating that new color is added to previous value in glcolar
	*/
	
	#define SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4(RGBmat, glcolar, nrgb, fact, add) {\
		int m_I, m_I4 = 0; \
		if (!add) {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				glcolar[m_I4] = fact * RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] = fact * RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] = fact * RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
		}else {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				glcolar[m_I4] += fact * RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] += fact * RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] += fact * RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
		}\
	}
	
	/*!
		SUMA_RGBmat_FullGlobLoc2_GLCOLAR4 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Full means that N is equal to all the nodes in the surface
		Glob means a global factor is applied to the color values
		Local means a local factor (per node) is applied to the color values
		
		SUMA_RGBmat_FullGlobLoc2_GLCOLAR4(RGBmat, glcolar, N, fact, locfact, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		fact (float) a factor applied to each color R, G, B values in the entire list
		locfact (float *) a N x 1 vector of factors applied to their respective nodes 
		add (SUMA_Boolean) flag indicating that new color is added to previous value in glcolar
	*/
	
	#define SUMA_RGBmat_FullGlobLoc2_GLCOLAR4(RGBmat, glcolar, nrgb, fact, locfact, add) {\
		int m_I, m_I4 = 0; \
		if (!add) {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				glcolar[m_I4] = locfact[m_I] * fact * RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] = locfact[m_I] * fact * RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] = locfact[m_I] * fact * RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
		} else {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				glcolar[m_I4] += locfact[m_I] * fact * RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] += locfact[m_I] * fact * RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] += locfact[m_I] * fact * RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
		}\
	}
	
	/*!
		SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Full means that N is equal to all the nodes in the surface
		NoGlob means no global factor is applied to the color values
		Local means a local factor (per node) is applied to the color values
		
		SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4(RGBmat, glcolar, N, locfact, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		locfact (float *) a N x 1 vector of factors applied to their respective nodes 
		add (SUMA_Boolean) flag indicating that new color is added to previous value in glcolar
	*/
	
	#define SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4(RGBmat, glcolar, nrgb, locfact, add) {\
		int m_I, m_I4 = 0; \
		if (!add) {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				glcolar[m_I4] = locfact[m_I] * RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] = locfact[m_I] * RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] = locfact[m_I] * RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
		} else {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				glcolar[m_I4] += locfact[m_I] * RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] += locfact[m_I] * RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] += locfact[m_I] * RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
		}\
	}
	
	/*!
		SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Part means that colors are specified for some of the nodes only N < N_Nodes
		NoGlob means no global factor is applied to the color values
		NoLocal means no local factor (per node) is applied to the color values
		
		SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4(RGBmat, NodeId, glcolar, N, isColored, add, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		add (SUMA_Boolean) flag indicating that new color is added to previous value in glcolar
	*/
	
	#define SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4(RGBmat, NodeId, glcolar, nrgb, isColored, add, N_Node) {\
		int m_I, m_I4 = 0; \
		if (!add) {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (NodeId[m_I] < N_Node) {\
					m_I4 = 4*NodeId[m_I]; \
					glcolar[m_I4] = RGBmat[m_I][0]; ++m_I4;\
					glcolar[m_I4] = RGBmat[m_I][1]; ++m_I4;\
					glcolar[m_I4] = RGBmat[m_I][2]; ++m_I4;\
					isColored[NodeId[m_I]] = YUP;\
				}\
			}\
		} else {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (NodeId[m_I] < N_Node) {\
					m_I4 = 4*NodeId[m_I]; \
					glcolar[m_I4] += RGBmat[m_I][0]; ++m_I4;\
					glcolar[m_I4] += RGBmat[m_I][1]; ++m_I4;\
					glcolar[m_I4] += RGBmat[m_I][2]; ++m_I4;\
					isColored[NodeId[m_I]] = YUP;\
				}\
			}\
		}\
	}
	
	/*!
		SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Part means that colors are specified for some of the nodes only N < N_Nodes
		NoGlob means a global factor is applied to the color values
		NoLocal means no local factor (per node) is applied to the color values
		
		SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4(RGBmat, NodeId, glcolar, N, isColored, fact, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		fact (float) a factor applied to each color R, G, B values in the entire list
		add (SUMA_Boolean) flag indicating that new color is added to previous value in glcolar
	*/
	
	#define SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4(RGBmat, NodeId, glcolar, nrgb, isColored, fact, add, N_Node) {\
		int m_I, m_I4 = 0, m_II; \
		if (!add) {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (NodeId[m_I] < N_Node) {\
					m_I4 = 4*NodeId[m_I]; \
					glcolar[m_I4] = fact * RGBmat[m_I][0]; ++m_I4;\
					glcolar[m_I4] = fact * RGBmat[m_I][1]; ++m_I4;\
					glcolar[m_I4] = fact * RGBmat[m_I][2]; ++m_I4;\
					isColored[NodeId[m_I]] = YUP;\
				}\
			}\
		} else {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (NodeId[m_I] < N_Node) {\
					m_I4 = 4*NodeId[m_I]; \
					glcolar[m_I4] += fact * RGBmat[m_I][0]; ++m_I4;\
					glcolar[m_I4] += fact * RGBmat[m_I][1]; ++m_I4;\
					glcolar[m_I4] += fact * RGBmat[m_I][2]; ++m_I4;\
					isColored[NodeId[m_I]] = YUP;\
				}\
			}\
		}\
	}
	
	/*!
		SUMA_RGBmat_PartGlobLoc2_GLCOLAR4 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Part means that colors are specified for some of the nodes only N < N_Nodes
		NoGlob means a global factor is applied to the color values
		NoLocal means a local factor (per node) is applied to the color values
		
		SUMA_RGBmat_PartGlobaLoc2_GLCOLAR4(RGBmat, NodeId, glcolar, N, isColored, fact, locfact, add, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		fact (float) a factor applied to each color R, G, B values in the entire list
		locfact (float *)  N x 1 vector of factors applied to their respective nodes 
		add (SUMA_Boolean) flag indicating that new color is added to previous value in glcolar
	*/
	
	#define SUMA_RGBmat_PartGlobLoc2_GLCOLAR4(RGBmat, NodeId, glcolar, nrgb, isColored, fact, locfact, add, N_Node) {\
		int m_I, m_I4 = 0; \
		if (!add) {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (NodeId[m_I] < N_Node) {\
					m_I4 = 4*NodeId[m_I]; \
					glcolar[m_I4] = locfact[m_I] * fact * RGBmat[m_I][0]; ++m_I4;\
					glcolar[m_I4] = locfact[m_I] * fact * RGBmat[m_I][1]; ++m_I4;\
					glcolar[m_I4] = locfact[m_I] * fact * RGBmat[m_I][2]; ++m_I4;\
					isColored[NodeId[m_I]] = YUP;\
				}\
			}\
		} else {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (NodeId[m_I] < N_Node) {\
					m_I4 = 4*NodeId[m_I]; \
					glcolar[m_I4] += locfact[m_I] * fact * RGBmat[m_I][0]; ++m_I4;\
					glcolar[m_I4] += locfact[m_I] * fact * RGBmat[m_I][1]; ++m_I4;\
					glcolar[m_I4] += locfact[m_I] * fact * RGBmat[m_I][2]; ++m_I4;\
					isColored[NodeId[m_I]] = YUP;\
				}\
			}\
		}\
	}
	
	/*!
		SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Part means that colors are specified for some of the nodes only N < N_Nodes
		NoGlob means no global factor is applied to the color values
		NoLocal means a local factor (per node) is applied to the color values
		
		SUMA_RGBmat_PartNoGlobaLoc2_GLCOLAR4(RGBmat, NodeId, glcolar, N, isColored, locfact, add, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		locfact (float *)  N x 1 vector of factors applied to their respective nodes 
		add (SUMA_Boolean) flag indicating that new color is added to previous value in glcolar
		
	*/
	
	#define SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4(RGBmat, NodeId, glcolar, nrgb, isColored,  locfact, add, N_Node) {\
		int m_I, m_I4 = 0; \
		if (!add) {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (NodeId[m_I] < N_Node) {\
					m_I4 = 4*NodeId[m_I]; \
					glcolar[m_I4] = locfact[m_I] * RGBmat[m_I][0]; ++m_I4;\
					glcolar[m_I4] = locfact[m_I] * RGBmat[m_I][1]; ++m_I4;\
					glcolar[m_I4] = locfact[m_I] * RGBmat[m_I][2]; ++m_I4;\
					isColored[NodeId[m_I]] = YUP;\
				}\
			}\
		}else {\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (NodeId[m_I] < N_Node) {\
					m_I4 = 4*NodeId[m_I]; \
					glcolar[m_I4] += locfact[m_I] * RGBmat[m_I][0]; ++m_I4;\
					glcolar[m_I4] += locfact[m_I] * RGBmat[m_I][1]; ++m_I4;\
					glcolar[m_I4] += locfact[m_I] * RGBmat[m_I][2]; ++m_I4;\
					isColored[NodeId[m_I]] = YUP;\
				}\
			}\
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
		nG (NoGlob) means no global factor is applied to the color values
		nL (NoLocal) means no local factor (per node) is applied to the color values
		
		SUMA_RGB_FnGnL_AR4op(RGBmat, glcolar, N, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		
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
	
	/*!
		This macro used to be called: SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4_opacity
		SUMA_RGB_FGnL_AR4op 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		F (Full) means that N is equal to all the nodes in the surface
		G (Glob) means a global factor is applied to the color values
		nL (NoLocal) means no local factor (per node) is applied to the color values
		
		SUMA_RGB_FGnL_AR4op(RGBmat, glcolar, N, fact, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		fact (float) a factor applied to each color R, G, B values in the entire list
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		if fact is < 0 then no color mixing is done. 
	*/
	
	#define SUMA_RGB_FGnL_AR4op(RGBmat, glcolar, nrgb, fact, isColored) {\
		int m_I, m_I4 = 0; \
		float m_of;\
		m_of = 1-fact;\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				isColored[m_I] = YUP;\
				glcolar[m_I4] = m_of * glcolar[m_I4] + fact * RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] = m_of * glcolar[m_I4] + fact * RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] = m_of * glcolar[m_I4] + fact * RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
	}
	
	/*!
	 This macro used to be called: SUMA_RGBmat_FullGlobLoc2_GLCOLAR4_opacity
			but name was too long for some compilers 
		
		SUMA_RGB_FGL_AR4op 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		F (Full) means that N is equal to all the nodes in the surface
		G (Glob) means a global factor is applied to the color values
		L (Local) means a local factor (per node) is applied to the color values
		
		SUMA_RGB_FGL_AR4op(RGBmat, glcolar, N, fact, locfact, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		fact (float) a factor applied to each color R, G, B values in the entire list
		locfact (float *) a N x 1 vector of factors applied to their respective nodes 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
	*/
	
	#define SUMA_RGB_FGL_AR4op(RGBmat, glcolar, nrgb, fact, locfact, isColored) {\
		int m_I, m_I4 = 0; \
		float m_of, m_of2;\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				isColored[m_I] = YUP;\
				m_of = locfact[m_I] * fact; \
				m_of2 = (1-m_of);\
				glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
	}
	
	/*!
		This macro used to be called: SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4_opacity 

		SUMA_RGB_FnGL_AR4op 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		F (Full) means that N is equal to all the nodes in the surface
		nG (NoGlob) means no global factor is applied to the color values
		L (Local) means a local factor (per node) is applied to the color values
		
		SUMA_RGB_FnGL_AR4op(RGBmat, glcolar, N, locfact, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		locfact (float *) a N x 1 vector of factors applied to their respective nodes 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
	*/
	
	#define SUMA_RGB_FnGL_AR4op(RGBmat, glcolar, nrgb, locfact, isColored) {\
		int m_I, m_I4 = 0; \
		float m_of;\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				isColored[m_I] = YUP;\
				m_of = 1-locfact[m_I];\
				glcolar[m_I4] = m_of * glcolar[m_I4] + locfact[m_I] * RGBmat[m_I][0]; ++m_I4;\
				glcolar[m_I4] = m_of * glcolar[m_I4] + locfact[m_I] * RGBmat[m_I][1]; ++m_I4;\
				glcolar[m_I4] = m_of * glcolar[m_I4] + locfact[m_I] * RGBmat[m_I][2]; ++m_I4;\
				++m_I4;\
			}\
	}
	
	/*!
		This macro used to be called: SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4_opacity 
		SUMA_RGB_PnGnL_AR4op 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		P (Part) means that colors are specified for some of the nodes only N < N_Nodes
		nG (NoGlob) means no global factor is applied to the color values
		nL (NoLocal) means no local factor (per node) is applied to the color values
		
		SUMA_RGB_PnGnL_AR4op(RGBmat, NodeId, glcolar, N, isColored, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
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
	
	/*!
		This macro used to be called: SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4_opacity
		SUMA_RGB_PGnL_AR4op 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		P (Part) means that colors are specified for some of the nodes only N < N_Nodes
		G (Glob) means a global factor is applied to the color values
		nL (NoLocal) means no local factor (per node) is applied to the color values
		
		SUMA_RGB_PGnL_AR4op(RGBmat, NodeId, glcolar, N, isColored, fact, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		fact (float) a factor applied to each color R, G, B values in the entire list
	*/
	
	#define SUMA_RGB_PGnL_AR4op(RGBmat, NodeId, glcolar, nrgb, isColored, fact, N_Node) {\
		int m_I, m_I4 = 0, m_II; \
		float m_of;\
		m_of = 1 - fact;\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is */\
					if (NodeId[m_I] < N_Node) {\
						m_I4 = 4*NodeId[m_I]; \
						glcolar[m_I4] = fact * RGBmat[m_I][0]; ++m_I4;\
						glcolar[m_I4] = fact * RGBmat[m_I][1]; ++m_I4;\
						glcolar[m_I4] = fact * RGBmat[m_I][2]; ++m_I4;\
						isColored[NodeId[m_I]] = YUP;\
					}\
				}else {/* mixing to be done */\
					if (NodeId[m_I] < N_Node) {\
						m_I4 = 4*NodeId[m_I]; \
						glcolar[m_I4] = m_of * glcolar[m_I4] + fact * RGBmat[m_I][0]; ++m_I4;\
						glcolar[m_I4] = m_of * glcolar[m_I4] + fact * RGBmat[m_I][1]; ++m_I4;\
						glcolar[m_I4] = m_of * glcolar[m_I4] + fact * RGBmat[m_I][2]; ++m_I4;\
					}\
				}\
			}\
	}
	
	/*!
		This macro used to be called: SUMA_RGBmat_PartGlobLoc2_GLCOLAR4_opacity
		
		SUMA_RGB_PGL_AR4op 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		P (Part) means that colors are specified for some of the nodes only N < N_Nodes
		G (Glob) means a global factor is applied to the color values
		L (Local) means a local factor (per node) is applied to the color values
		
		SUMA_RGB_PGL_AR4op(RGBmat, NodeId, glcolar, N, isColored, fact, locfact, add, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		fact (float) a factor applied to each color R, G, B values in the entire list
		locfact (float *)  N x 1 vector of factors applied to their respective nodes 
	*/
	
	#define SUMA_RGB_PGL_AR4op(RGBmat, NodeId, glcolar, nrgb, isColored, fact, locfact, N_Node) {\
		int m_I, m_I4 = 0; \
		float m_of, m_of2;\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is */\
					if (NodeId[m_I] < N_Node) {\
						m_I4 = 4*NodeId[m_I]; \
						m_of = (locfact[m_I] * fact);\
						glcolar[m_I4] = m_of * RGBmat[m_I][0]; ++m_I4;\
						glcolar[m_I4] = m_of * RGBmat[m_I][1]; ++m_I4;\
						glcolar[m_I4] = m_of * RGBmat[m_I][2]; ++m_I4;\
						isColored[NodeId[m_I]] = YUP;\
					}\
				}else { /* mixing to be done */\
					if (NodeId[m_I] < N_Node) {\
						m_I4 = 4*NodeId[m_I]; \
						m_of = (locfact[m_I] * fact);\
						m_of2 = (1 - m_of);\
						glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][0]; ++m_I4;\
						glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][1]; ++m_I4;\
						glcolar[m_I4] = m_of2 * glcolar[m_I4] + m_of * RGBmat[m_I][2]; ++m_I4;\
					}\
				}\
			}\
	}
	
	/*!
		This macro used to be called: SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4_opacity
		SUMA_RGB_PnGL_AR4op 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		P (Part) means that colors are specified for some of the nodes only N < N_Nodes
		nG (NoGlob) means no global factor is applied to the color values
		L (Local) means a local factor (per node) is applied to the color values
		
		SUMA_RGB_PnGL_AR4op(RGBmat, NodeId, glcolar, N, isColored, locfact, add, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		locfact (float *)  N x 1 vector of opacities applied to their respective nodes 
		
	*/
	
	#define SUMA_RGB_PnGL_AR4op(RGBmat, NodeId, glcolar, nrgb, isColored,  locfact, N_Node) {\
		int m_I, m_I4 = 0; \
		float m_of;\
			for (m_I=0; m_I < nrgb; ++m_I) {\
				if (!isColored[NodeId[m_I]]) { /* a new color, put it down as it is */\
					if (NodeId[m_I] < N_Node) {\
						m_I4 = 4*NodeId[m_I]; \
						glcolar[m_I4] = locfact[m_I] * RGBmat[m_I][0]; ++m_I4;\
						glcolar[m_I4] = locfact[m_I] * RGBmat[m_I][1]; ++m_I4;\
						glcolar[m_I4] = locfact[m_I] * RGBmat[m_I][2]; ++m_I4;\
						isColored[NodeId[m_I]] = YUP;\
					}\
				}else { /* mixing to be done */\
					if (NodeId[m_I] < N_Node) {\
						m_I4 = 4*NodeId[m_I]; \
						m_of = 1 - locfact[m_I];\
						glcolar[m_I4] = m_of * glcolar[m_I4] + locfact[m_I] * RGBmat[m_I][0]; ++m_I4;\
						glcolar[m_I4] = m_of * glcolar[m_I4] + locfact[m_I] * RGBmat[m_I][1]; ++m_I4;\
						glcolar[m_I4] = m_of * glcolar[m_I4] + locfact[m_I] * RGBmat[m_I][2]; ++m_I4;\
					}\
				}\
			}\
	}
	
#endif
