#include "cdflib.h"
void dinvr(int *status,double *x,double *fx,
	   unsigned long *qleft,unsigned long *qhi)
/*
**********************************************************************
 
     void dinvr(int *status,double *x,double *fx,
           unsigned long *qleft,unsigned long *qhi)

          Double precision
          bounds the zero of the function and invokes zror
                    Reverse Communication
 
 
                              Function
 
 
     Bounds the    function  and  invokes  ZROR   to perform the   zero
     finding.  STINVR  must  have   been  called  before this   routine
     in order to set its parameters.
 
 
                              Arguments
 
 
     STATUS <--> At the beginning of a zero finding problem, STATUS
                 should be set to 0 and INVR invoked.  (The value
                 of parameters other than X will be ignored on this cal
 
                 When INVR needs the function evaluated, it will set
                 STATUS to 1 and return.  The value of the function
                 should be set in FX and INVR again called without
                 changing any of its other parameters.
 
                 When INVR has finished without error, it will return
                 with STATUS 0.  In that case X is approximately a root
                 of F(X).
 
                 If INVR cannot bound the function, it returns status
                 -1 and sets QLEFT and QHI.
                         INTEGER STATUS
 
     X <-- The value of X at which F(X) is to be evaluated.
                         DOUBLE PRECISION X
 
     FX --> The value of F(X) calculated when INVR returns with
            STATUS = 1.
                         DOUBLE PRECISION FX
 
     QLEFT <-- Defined only if QMFINV returns .FALSE.  In that
          case it is .TRUE. If the stepping search terminated
          unsucessfully at SMALL.  If it is .FALSE. the search
          terminated unsucessfully at BIG.
                    QLEFT is LOGICAL
 
     QHI <-- Defined only if QMFINV returns .FALSE.  In that
          case it is .TRUE. if F(X) .GT. Y at the termination
          of the search and .FALSE. if F(X) .LT. Y at the
          termination of the search.
                    QHI is LOGICAL
 
**********************************************************************
*/
{
    E0000(0,status,x,fx,qleft,qhi,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
} /* END */
