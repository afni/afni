#ifndef SUMA_MACROS_INCLUDED
#define SUMA_MACROS_INCLUDED

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
	int _I; \
	norm = 0.0; \
	for (_I = 0; _I < nel; _I++) { \
		norm += a[_I]*a[_I];	 \
	} \
	norm = sqrtf(norm); \
}


/*! \def SUMA_MIN_VEC(a,nel)
\brief SUMA_MIN_VEC macro for minimum
	a pointer to vector
	nel number of elements in vector
	amin minimum of a (make sure types of a and amin match)
*/
#define SUMA_MIN_VEC(a,nel,amin) { \
	int _I; \
	amin = a[0]; \
	for (_I = 1; _I < nel; _I++) { \
		if (a[_I] < amin) amin = a[_I];	 \
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
	int _I; \
	amin = a[0]; \
	minloc = 0; \
	for (_I = 1; _I < nel; _I++) { \
		if (a[_I] < amin) { \
			amin = a[_I];	 \
			minloc = _I; \
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
	int _I; \
	amax = a[0]; \
	for (_I = 1; _I < nel; _I++) { \
		if (a[_I] > amax) amax = a[_I];	 \
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
	int _I; \
	amaxloc = 0; \
	amax = a[0]; \
	aminloc = 0; \
	amin = a[0];\
	for (_I = 1; _I < nel; _I++) { \
		if (a[_I] > amax) { amax = a[_I]; amaxloc = _I; }	 \
		else { if (a[_I] < amin) { amin = a[_I]; aminloc = _I; } }	 \
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
                  int _IX;  \
                      for(_IX = 0 ; _IX < (len) ; _IX++)  \
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
                  int _IX;  \
                      for(_IX = 0 ; _IX < (len) ; _IX++)  \
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
                   int _IX;  \
                       for(_IX = 0 ; _IX < (len) ; _IX++)  \
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
                       int _IX;  \
                       s = (*_PTA++);  \
                       for(_IX = 1 ; _IX < (len) ; _IX++)  \
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
                       int _IX;  \
                       for(_IX = 0 ; _IX < (len) ; _IX++)  \
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
							  int _IX;  \
                       for(_IX = 0 ; _IX < (len) ; _IX++)  \
                         _PTB[_IX] = _PTA[ind[_IX]];   \
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
                       int _IX;  \
							  _PTA = _PTA + catata; \
							  for(_IX = 0 ; _IX < (lenb) ; _IX++)  \
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
                 int _IX,_JX;  \
                 for(_IX = 0 ; _IX < rows ; _IX++) {  \
                     _PTA = _AMX[_IX ] ;  \
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
						int _IX, _JX;	\
						for (_IX = 0; _IX < cols ; _IX++) {	\
							amin[_IX]=a[0][_IX];	\
							for (_JX = 1 ; _JX < rows ; _JX++)  \
								if (a[_JX][_IX] < amin[_IX]) amin[_IX] = a[_JX][_IX];\
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
						int _IX, _JX;	\
						for (_IX = 0; _IX < cols ; _IX++) {	\
							amax[_IX]=a[0][_IX];	\
							for (_JX = 1 ; _JX < rows ; _JX++)  \
								if (a[_JX][_IX] > amax[_IX]) amax[_IX] = a[_JX][_IX];\
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
						int _IX, _JX;	\
						for (_IX = 0; _IX < cols ; _IX++) {	\
							amax[_IX]=a[0][_IX];	\
							amin[_IX]=a[0][_IX];	\
							asum[_IX]=a[0][_IX];	\
							for (_JX = 1 ; _JX < rows ; _JX++) { \
								if (a[_JX][_IX] > amax[_IX]) amax[_IX] = a[_JX][_IX];\
								if (a[_JX][_IX] < amin[_IX]) amin[_IX] = a[_JX][_IX];\
								asum[_IX] += a[_JX][_IX];	\
							}	\
						}\
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
                 int _IX,_JX;  \
                 for(_IX = 0 ; _IX < rows ; _IX++) {  \
                     _PTA = _AMX[_IX ];  \
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
                       int _IX;  \
                       for(_IX = 0 ; _IX < (len) ; _IX++)  \
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
                       int _IX;  \
                       s = (*_PTA++) * (*_PTB++);  \
                       for(_IX = 1 ; _IX < (len) ; _IX++)  \
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
               	  int _IX,_JX,_KX;  \
               	  for(_IX = 0 ; _IX < rowsa ; _IX++) {  \
                     	_PTC = _CMX[_IX];  \
                     	_PTB = _BMX[0];  \
                     	for(_JX = 0 ; _JX < colsb ; _JX++) {  \
                        	 _PTA = _AMX[_IX];  \
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
               	  int _IX,_JX;  \
               	  for(_IX = 0 ; _IX < rowsa ; _IX++) {  \
                     	for(_JX = 0 ; _JX < colsa ; _JX++) {  \
                        	 _CMX[_IX][_JX] = _AMX[_IX][_JX] + _BMX[_IX][_JX];  \
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
               	  int _IX,_JX;  \
               	  for(_IX = 0 ; _IX < rowsa ; _IX++) {  \
                     	for(_JX = 0 ; _JX < colsa ; _JX++) {  \
                        	 _CMX[_IX][_JX] = _AMX[_IX][_JX] - _BMX[_IX][_JX];  \
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
               	  int _IX,_JX;  \
               	  for(_IX = 0 ; _IX < rowsa ; _IX++) {  \
                     	for(_JX = 0 ; _JX < colsa ; _JX++) {  \
                        	 _BMX[_JX][_IX] = _AMX[_IX][_JX];  \
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
		int _I, _I4 = 0; \
		for (_I=0; _I < nrgb; ++_I) {\
			glcolar[_I4] = RGBmat[_I][0]; ++_I4;\
			glcolar[_I4] = RGBmat[_I][1]; ++_I4;\
			glcolar[_I4] = RGBmat[_I][2]; ++_I4;\
			++_I4;\
		}\
	}
	
	/*!
		SUMA_GLCOLAR4_2_RGBmat copies 4N x 1 GL color array into an N x 3 RGB matrix format
		
		SUMA_GLCOLAR4_2_RGBmat (glcolar, RGBmat, N)   
		glcolar (GLfloat *) (4 N) x 1 vector 
		RGBmat (float **) N x 3 matrix of RGB values
	*/
	
	#define SUMA_RGBmat2GLCOLAR4(glcolar, RGBmat, nrgb) {\
		int _I, _I4 = 0; \
		for (_I=0; _I < nrgb; ++_I) {\
			RGBmat[_I][0] = glcolar[_I4]; ++_I4;\
			RGBmat[_I][1] = glcolar[_I4]; ++_I4;\
			RGBmat[_I][2] = glcolar[_I4]; ++_I4;\
			++_I4;\
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
		int _I, _I4 = 0; \
		if (!add) {\
			for (_I=0; _I < nrgb; ++_I) {\
				glcolar[_I4] = RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] = RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] = RGBmat[_I][2]; ++_I4;\
				++_I4;\
			}\
		} else {\
			for (_I=0; _I < nrgb; ++_I) {\
				glcolar[_I4] += RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] += RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] += RGBmat[_I][2]; ++_I4;\
				++_I4;\
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
		int _I, _I4 = 0; \
		if (!add) {\
			for (_I=0; _I < nrgb; ++_I) {\
				glcolar[_I4] = fact * RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] = fact * RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] = fact * RGBmat[_I][2]; ++_I4;\
				++_I4;\
			}\
		}else {\
			for (_I=0; _I < nrgb; ++_I) {\
				glcolar[_I4] += fact * RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] += fact * RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] += fact * RGBmat[_I][2]; ++_I4;\
				++_I4;\
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
		int _I, _I4 = 0; \
		if (!add) {\
			for (_I=0; _I < nrgb; ++_I) {\
				glcolar[_I4] = locfact[_I] * fact * RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] = locfact[_I] * fact * RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] = locfact[_I] * fact * RGBmat[_I][2]; ++_I4;\
				++_I4;\
			}\
		} else {\
			for (_I=0; _I < nrgb; ++_I) {\
				glcolar[_I4] += locfact[_I] * fact * RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] += locfact[_I] * fact * RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] += locfact[_I] * fact * RGBmat[_I][2]; ++_I4;\
				++_I4;\
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
		int _I, _I4 = 0; \
		if (!add) {\
			for (_I=0; _I < nrgb; ++_I) {\
				glcolar[_I4] = locfact[_I] * RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] = locfact[_I] * RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] = locfact[_I] * RGBmat[_I][2]; ++_I4;\
				++_I4;\
			}\
		} else {\
			for (_I=0; _I < nrgb; ++_I) {\
				glcolar[_I4] += locfact[_I] * RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] += locfact[_I] * RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] += locfact[_I] * RGBmat[_I][2]; ++_I4;\
				++_I4;\
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
		int _I, _I4 = 0; \
		if (!add) {\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[NodeId[_I]] = YUP;\
				if (NodeId[_I] < N_Node) {\
					_I4 = 4*NodeId[_I]; \
					glcolar[_I4] = RGBmat[_I][0]; ++_I4;\
					glcolar[_I4] = RGBmat[_I][1]; ++_I4;\
					glcolar[_I4] = RGBmat[_I][2]; ++_I4;\
				}\
			}\
		} else {\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[NodeId[_I]] = YUP;\
				if (NodeId[_I] < N_Node) {\
					_I4 = 4*NodeId[_I]; \
					glcolar[_I4] += RGBmat[_I][0]; ++_I4;\
					glcolar[_I4] += RGBmat[_I][1]; ++_I4;\
					glcolar[_I4] += RGBmat[_I][2]; ++_I4;\
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
		int _I, _I4 = 0, _II; \
		if (!add) {\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[NodeId[_I]] = YUP;\
				if (NodeId[_I] < N_Node) {\
					_I4 = 4*NodeId[_I]; \
					glcolar[_I4] = fact * RGBmat[_I][0]; ++_I4;\
					glcolar[_I4] = fact * RGBmat[_I][1]; ++_I4;\
					glcolar[_I4] = fact * RGBmat[_I][2]; ++_I4;\
				}\
			}\
		} else {\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[NodeId[_I]] = YUP;\
				if (NodeId[_I] < N_Node) {\
					_I4 = 4*NodeId[_I]; \
					glcolar[_I4] += fact * RGBmat[_I][0]; ++_I4;\
					glcolar[_I4] += fact * RGBmat[_I][1]; ++_I4;\
					glcolar[_I4] += fact * RGBmat[_I][2]; ++_I4;\
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
		int _I, _I4 = 0; \
		if (!add) {\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[NodeId[_I]] = YUP;\
				if (NodeId[_I] < N_Node) {\
					_I4 = 4*NodeId[_I]; \
					glcolar[_I4] = locfact[_I] * fact * RGBmat[_I][0]; ++_I4;\
					glcolar[_I4] = locfact[_I] * fact * RGBmat[_I][1]; ++_I4;\
					glcolar[_I4] = locfact[_I] * fact * RGBmat[_I][2]; ++_I4;\
				}\
			}\
		} else {\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[NodeId[_I]] = YUP;\
				if (NodeId[_I] < N_Node) {\
					_I4 = 4*NodeId[_I]; \
					glcolar[_I4] += locfact[_I] * fact * RGBmat[_I][0]; ++_I4;\
					glcolar[_I4] += locfact[_I] * fact * RGBmat[_I][1]; ++_I4;\
					glcolar[_I4] += locfact[_I] * fact * RGBmat[_I][2]; ++_I4;\
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
		int _I, _I4 = 0; \
		if (!add) {\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[NodeId[_I]] = YUP;\
				if (NodeId[_I] < N_Node) {\
					_I4 = 4*NodeId[_I]; \
					glcolar[_I4] = locfact[_I] * RGBmat[_I][0]; ++_I4;\
					glcolar[_I4] = locfact[_I] * RGBmat[_I][1]; ++_I4;\
					glcolar[_I4] = locfact[_I] * RGBmat[_I][2]; ++_I4;\
				}\
			}\
		}else {\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[NodeId[_I]] = YUP;\
				if (NodeId[_I] < N_Node) {\
					_I4 = 4*NodeId[_I]; \
					glcolar[_I4] += locfact[_I] * RGBmat[_I][0]; ++_I4;\
					glcolar[_I4] += locfact[_I] * RGBmat[_I][1]; ++_I4;\
					glcolar[_I4] += locfact[_I] * RGBmat[_I][2]; ++_I4;\
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
		int _I, _I4; \
		for (_I=0; _I < N_Nodes; ++_I) {\
			if (!isColored[_I]) {\
				_I4 = 4*_I; \
				glcolar[_I4] = R; ++_I4;\
				glcolar[_I4] = G; ++_I4;\
				glcolar[_I4] = B; ++_I4;\
			}\
		}\
	}

	/*!
		SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4_opacity_opacity 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Full means that N is equal to all the nodes in the surface
		NoGlob means no global factor is applied to the color values
		NoLocal means no local factor (per node) is applied to the color values
		
		SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4_opacity(RGBmat, glcolar, N, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		
	*/
	
	#define SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4_opacity(RGBmat, glcolar, nrgb, isColored) {\
		int _I, _I4 = 0; \
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[_I] = YUP;\
				glcolar[_I4] = RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] = RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] = RGBmat[_I][2]; ++_I4;\
				++_I4;\
			}\
	}
	
	/*!
		SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4_opacity 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Full means that N is equal to all the nodes in the surface
		Glob means a global factor is applied to the color values
		NoLocal means no local factor (per node) is applied to the color values
		
		SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4_opacity(RGBmat, glcolar, N, fact, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		fact (float) a factor applied to each color R, G, B values in the entire list
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		if fact is < 0 then no color mixing is done. 
	*/
	
	#define SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4_opacity(RGBmat, glcolar, nrgb, fact, isColored) {\
		int _I, _I4 = 0; \
		float _of;\
		_of = 1-fact;\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[_I] = YUP;\
				glcolar[_I4] = _of * glcolar[_I4] + fact * RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] = _of * glcolar[_I4] + fact * RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] = _of * glcolar[_I4] + fact * RGBmat[_I][2]; ++_I4;\
				++_I4;\
			}\
	}
	
	/*!
		SUMA_RGBmat_FullGlobLoc2_GLCOLAR4_opacity 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Full means that N is equal to all the nodes in the surface
		Glob means a global factor is applied to the color values
		Local means a local factor (per node) is applied to the color values
		
		SUMA_RGBmat_FullGlobLoc2_GLCOLAR4_opacity(RGBmat, glcolar, N, fact, locfact, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		fact (float) a factor applied to each color R, G, B values in the entire list
		locfact (float *) a N x 1 vector of factors applied to their respective nodes 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
	*/
	
	#define SUMA_RGBmat_FullGlobLoc2_GLCOLAR4_opacity(RGBmat, glcolar, nrgb, fact, locfact, isColored) {\
		int _I, _I4 = 0; \
		float _of, _of2;\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[_I] = YUP;\
				_of = locfact[_I] * fact; \
				_of2 = (1-_of);\
				glcolar[_I4] = _of2 * glcolar[_I4] + _of * RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] = _of2 * glcolar[_I4] + _of * RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] = _of2 * glcolar[_I4] + _of * RGBmat[_I][2]; ++_I4;\
				++_I4;\
			}\
	}
	
	/*!
		SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4_opacity 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Full means that N is equal to all the nodes in the surface
		NoGlob means no global factor is applied to the color values
		Local means a local factor (per node) is applied to the color values
		
		SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4_opacity(RGBmat, glcolar, N, locfact, add)   
		RGBmat (float **) N x 3 matrix of RGB values
		glcolar (GLfloat *) (4 N) x 1 vector 
		locfact (float *) a N x 1 vector of factors applied to their respective nodes 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
	*/
	
	#define SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4_opacity(RGBmat, glcolar, nrgb, locfact, isColored) {\
		int _I, _I4 = 0; \
		float _of;\
			for (_I=0; _I < nrgb; ++_I) {\
				isColored[_I] = YUP;\
				_of = 1-locfact[_I];\
				glcolar[_I4] = _of * glcolar[_I4] + locfact[_I] * RGBmat[_I][0]; ++_I4;\
				glcolar[_I4] = _of * glcolar[_I4] + locfact[_I] * RGBmat[_I][1]; ++_I4;\
				glcolar[_I4] = _of * glcolar[_I4] + locfact[_I] * RGBmat[_I][2]; ++_I4;\
				++_I4;\
			}\
	}
	
	/*!
		SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4_opacity 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Part means that colors are specified for some of the nodes only N < N_Nodes
		NoGlob means no global factor is applied to the color values
		NoLocal means no local factor (per node) is applied to the color values
		
		SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4_opacity(RGBmat, NodeId, glcolar, N, isColored, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
	*/
	
	#define SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4_opacity(RGBmat, NodeId, glcolar, nrgb, isColored, N_Node) {\
		int _I, _I4 = 0; \
			for (_I=0; _I < nrgb; ++_I) {\
					if (NodeId[_I] < N_Node) {\
						_I4 = 4*NodeId[_I]; \
						glcolar[_I4] = RGBmat[_I][0]; ++_I4;\
						glcolar[_I4] = RGBmat[_I][1]; ++_I4;\
						glcolar[_I4] = RGBmat[_I][2]; ++_I4;\
					}\
					isColored[NodeId[_I]] = YUP;\
				}\
	}
	
	/*!
		SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4_opacity 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Part means that colors are specified for some of the nodes only N < N_Nodes
		NoGlob means a global factor is applied to the color values
		NoLocal means no local factor (per node) is applied to the color values
		
		SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4_opacity(RGBmat, NodeId, glcolar, N, isColored, fact, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		fact (float) a factor applied to each color R, G, B values in the entire list
	*/
	
	#define SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4_opacity(RGBmat, NodeId, glcolar, nrgb, isColored, fact, N_Node) {\
		int _I, _I4 = 0, _II; \
		float _of;\
		_of = 1 - fact;\
			for (_I=0; _I < nrgb; ++_I) {\
				if (!isColored[NodeId[_I]]) { /* a new color, put it down as it is */\
					if (NodeId[_I] < N_Node) {\
						_I4 = 4*NodeId[_I]; \
						glcolar[_I4] = fact * RGBmat[_I][0]; ++_I4;\
						glcolar[_I4] = fact * RGBmat[_I][1]; ++_I4;\
						glcolar[_I4] = fact * RGBmat[_I][2]; ++_I4;\
					}\
					isColored[NodeId[_I]] = YUP;\
				}else {/* mixing to be done */\
					if (NodeId[_I] < N_Node) {\
						_I4 = 4*NodeId[_I]; \
						glcolar[_I4] = _of * glcolar[_I4] + fact * RGBmat[_I][0]; ++_I4;\
						glcolar[_I4] = _of * glcolar[_I4] + fact * RGBmat[_I][1]; ++_I4;\
						glcolar[_I4] = _of * glcolar[_I4] + fact * RGBmat[_I][2]; ++_I4;\
					}\
				}\
			}\
	}
	
	/*!
		SUMA_RGBmat_PartGlobLoc2_GLCOLAR4_opacity 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Part means that colors are specified for some of the nodes only N < N_Nodes
		NoGlob means a global factor is applied to the color values
		NoLocal means a local factor (per node) is applied to the color values
		
		SUMA_RGBmat_PartGlobaLoc2_GLCOLAR4_opacity(RGBmat, NodeId, glcolar, N, isColored, fact, locfact, add, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		fact (float) a factor applied to each color R, G, B values in the entire list
		locfact (float *)  N x 1 vector of factors applied to their respective nodes 
	*/
	
	#define SUMA_RGBmat_PartGlobLoc2_GLCOLAR4_opacity(RGBmat, NodeId, glcolar, nrgb, isColored, fact, locfact, N_Node) {\
		int _I, _I4 = 0; \
		float _of, _of2;\
			for (_I=0; _I < nrgb; ++_I) {\
				if (!isColored[NodeId[_I]]) { /* a new color, put it down as it is */\
					if (NodeId[_I] < N_Node) {\
						_I4 = 4*NodeId[_I]; \
						_of = (locfact[_I] * fact);\
						glcolar[_I4] = _of * RGBmat[_I][0]; ++_I4;\
						glcolar[_I4] = _of * RGBmat[_I][1]; ++_I4;\
						glcolar[_I4] = _of * RGBmat[_I][2]; ++_I4;\
					}\
					isColored[NodeId[_I]] = YUP;\
				}else { /* mixing to be done */\
					if (NodeId[_I] < N_Node) {\
						_I4 = 4*NodeId[_I]; \
						_of = (locfact[_I] * fact);\
						_of2 = (1 - _of);\
						glcolar[_I4] = _of2 * glcolar[_I4] + _of * RGBmat[_I][0]; ++_I4;\
						glcolar[_I4] = _of2 * glcolar[_I4] + _of * RGBmat[_I][1]; ++_I4;\
						glcolar[_I4] = _of2 * glcolar[_I4] + _of * RGBmat[_I][2]; ++_I4;\
					}\
				}\
			}\
	}
	
	/*!
		SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4_opacity 
		copies an N x 3 RGB matrix into a 4N x 1 GL color array format
		Part means that colors are specified for some of the nodes only N < N_Nodes
		NoGlob means no global factor is applied to the color values
		NoLocal means a local factor (per node) is applied to the color values
		
		SUMA_RGBmat_PartNoGlobaLoc2_GLCOLAR4_opacity(RGBmat, NodeId, glcolar, N, isColored, locfact, add, N_Nodes)   
		RGBmat (float **) N x 3 matrix of RGB values
		NodeId (int *) N x 1 vector containing indices of nodes for wich color is specified in RGBmat
		glcolar (GLfloat *) (4 N_Nodes) x 1 vector 
		isColored (SUMA_Boolean) N_Nodes x 1 vector indicating that a node was colored. ONLY YUP/1 are placed
		when a node is assigned a color. Values of isColored for nodes that have not been visited remain unchanged
		locfact (float *)  N x 1 vector of opacities applied to their respective nodes 
		
	*/
	
	#define SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4_opacity(RGBmat, NodeId, glcolar, nrgb, isColored,  locfact, N_Node) {\
		int _I, _I4 = 0; \
		float _of;\
			for (_I=0; _I < nrgb; ++_I) {\
				if (!isColored[NodeId[_I]]) { /* a new color, put it down as it is */\
					if (NodeId[_I] < N_Node) {\
						_I4 = 4*NodeId[_I]; \
						glcolar[_I4] = locfact[_I] * RGBmat[_I][0]; ++_I4;\
						glcolar[_I4] = locfact[_I] * RGBmat[_I][1]; ++_I4;\
						glcolar[_I4] = locfact[_I] * RGBmat[_I][2]; ++_I4;\
					}\
					isColored[NodeId[_I]] = YUP;\
				}else { /* mixing to be done */\
					if (NodeId[_I] < N_Node) {\
						_I4 = 4*NodeId[_I]; \
						_of = 1 - locfact[_I];\
						glcolar[_I4] = _of * glcolar[_I4] + locfact[_I] * RGBmat[_I][0]; ++_I4;\
						glcolar[_I4] = _of * glcolar[_I4] + locfact[_I] * RGBmat[_I][1]; ++_I4;\
						glcolar[_I4] = _of * glcolar[_I4] + locfact[_I] * RGBmat[_I][2]; ++_I4;\
					}\
				}\
			}\
	}
	
#endif
