#include "cdflib.h"
void dzror(int *status,double *x,double *fx,double *xlo,
	   double *xhi,unsigned long *qleft,unsigned long *qhi)
/*
**********************************************************************
 
     void dzror(int *status,double *x,double *fx,double *xlo,
           double *xhi,unsigned long *qleft,unsigned long *qhi)

     Double precision ZeRo of a function -- Reverse Communication
 
 
                              Function
 
 
     Performs the zero finding.  STZROR must have been called before
     this routine in order to set its parameters.
 
 
                              Arguments
 
 
     STATUS <--> At the beginning of a zero finding problem, STATUS
                 should be set to 0 and ZROR invoked.  (The value
                 of other parameters will be ignored on this call.)
 
                 When ZROR needs the function evaluated, it will set
                 STATUS to 1 and return.  The value of the function
                 should be set in FX and ZROR again called without
                 changing any of its other parameters.
 
                 When ZROR has finished without error, it will return
                 with STATUS 0.  In that case (XLO,XHI) bound the answe
 
                 If ZROR finds an error (which implies that F(XLO)-Y an
                 F(XHI)-Y have the same sign, it returns STATUS -1.  In
                 this case, XLO and XHI are undefined.
                         INTEGER STATUS
 
     X <-- The value of X at which F(X) is to be evaluated.
                         DOUBLE PRECISION X
 
     FX --> The value of F(X) calculated when ZROR returns with
            STATUS = 1.
                         DOUBLE PRECISION FX
 
     XLO <-- When ZROR returns with STATUS = 0, XLO bounds the
             inverval in X containing the solution below.
                         DOUBLE PRECISION XLO
 
     XHI <-- When ZROR returns with STATUS = 0, XHI bounds the
             inverval in X containing the solution above.
                         DOUBLE PRECISION XHI
 
     QLEFT <-- .TRUE. if the stepping search terminated unsucessfully
                at XLO.  If it is .FALSE. the search terminated
                unsucessfully at XHI.
                    QLEFT is LOGICAL
 
     QHI <-- .TRUE. if F(X) .GT. Y at the termination of the
              search and .FALSE. if F(X) .LT. Y at the
              termination of the search.
                    QHI is LOGICAL
 
**********************************************************************
*/
{
    E0001(0,status,x,fx,xlo,xhi,qleft,qhi,NULL,NULL,NULL,NULL);
} /* END */
