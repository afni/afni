/**************************************************************************
 * Parks-McClellan algorithm for FIR filter design (C version)
 *-------------------------------------------------
 *  Copyright (C) 1995  Jake Janovetz (janovetz@coewl.cen.uiuc.edu)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *************************************************************************/

/***************************************************************************
 * Test program for the remez() function.  Sends appropriate arguments to
 * remez() to generate a filter.  Then, prints the resulting coefficients
 * to stdout.
 **************************************************************************/

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "remez.c"

int main( int argc , char *argv[] )
{
   double weight[3] , desired[3] , band[6] , h[1024] ;
   int i , ntap , nband ;
   double fbot , ftop , df , fdel ;

   if( argc < 4 ){
     printf("Usage: remez_test fbot ftop ntap\n");
     printf("Where 0 <= fbot < ftop <= 0.5 (e.g., assume TR=1)\n") ;
     printf("ntap = number of taps (between 4 and 1000)\n") ;
     printf("Example:\n"
       "remez_test 0.01 0.10 180 | 1dfft -nodetrend -nfft 512 stdin: -"
       " | 1dplot -stdin -xaxis 0:0.5:10:10 -dt 0.001953\n") ;
     printf("To compile: cc -o remez_test -O2 remez_test.c -lm\n") ;
     exit(0);
   }
   fbot = strtod(argv[1],NULL) ;
   ftop = strtod(argv[2],NULL) ;
   ntap = (int)strtod(argv[3],NULL) ;
   if( fbot < 0.0 || ftop >= 0.5 || ftop <= fbot || ntap < 4 || ntap > 1024 ){
     fprintf(stderr,"Bad inputs\n") ; exit(1) ;
   }
   if( ntap%2 ){
     ntap++ ;  /* must be even */
     fprintf(stderr,"ntap increased to %d to be even",ntap) ;
   }

   df = 1.0/ntap ; fdel = df ;
   if( fbot <= fdel && fbot != 0.0 ){
     fbot = 0.0 ; fprintf(stderr,"fbot re-set to 0\n") ;
   }
   if( ftop >= 0.5-fdel && ftop != 0.5 ){
     ftop = 0.5 ; fprintf(stderr,"ftop re-set to 0.5\n") ;
   }
   if( fbot == 0.0 && ftop == 0.5 ){
     fprintf(stderr,"fbot=0 ftop=0.5 ==> I quit\n") ; exit(1) ;
   }

   nband = 0 ;

   /* reject below fbot */

   if( fbot > 0.0 ){
     weight[nband] = 1.0 ; desired[nband] = 0 ; band[2*nband] = 0.0 ; band[2*nband+1] = fbot-fdel ;
     nband++ ;
   }

   /* pass between fbot and ftop */

   weight[nband] = 1.0 ; desired[nband] = 1 ;
   if( fbot > 0.0 ){
     band[2*nband] = fbot+fdel ; band[2*nband+1] = (ftop < 0.5 ) ? ftop-fdel : ftop ;
   } else {
     band[2*nband] = 0.0       ; band[2*nband+1] = ftop-fdel ;
   }
   nband++ ;

   /* reject above ftop */

   if( ftop < 0.5 ){
     weight[nband] = 1.0 ; desired[nband] = 0 ; band[2*nband] = ftop+fdel ; band[2*nband+1] = 0.5 ;
     nband++ ;
   }

   /* compute */

   remez(h, ntap, nband, band, desired, weight, BANDPASS);

   /* print */

   for (i=0; i<ntap; i++) printf("%23.20f\n", h[i]) ;
   exit(0) ;
}

