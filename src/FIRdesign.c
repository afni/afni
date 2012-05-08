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

#include "mrilib.h"
#include "remez.c"

int main( int argc , char *argv[] )
{
   double weight[3] , desired[3] , band[6] , h[2048] ;
   int i , ntap=-666 , nband , iarg=1 ;
   double fbot=-666.9 , ftop=-999.9 , df,dff , fdel , TR=1.0 , dqq ;

   /*-- help me if you can ---*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: FIRdesign [options] fbot ftop ntap\n"
       "\n"
       "Uses the Remez algorithm to calculate the FIR filter weights\n"
       "for a bandpass filter; results are written to stdout in an\n"
       "unadorned (no header) column of numbers.\n"
       "Inputs are\n"
       "  fbot = lowest freqency in the pass band.\n"
       "  ftop = highest frequency in the pass band.\n"
       "        * 0 <= fbot < ftop <= 0.5/TR\n"
       "        * Unless the '-TR' option is given, TR=1.\n"
       "  ntap = Number of filter weights (AKA 'taps') to use.\n"
       "        * Define df = 1/(ntap*TR) = frequency resolution:\n"
       "        * Then if fbot < 1.1*df, it will be replaced by 0;\n"
       "          in other words, a pure lowpass filter.  This change\n"
       "          is necessary since the duration ntap*TR must be longer\n"
       "          than 1 full cycle of the lowest frequency (1/fbot) in\n"
       "          order to filter out slower frequency components.\n"
       "        * Similarly, if ftop > 0.5/TR-1.1*df, it will be\n"
       "          replaced by 0.5/TR; in other words, a pure\n"
       "          highpass filter.\n"
       "        * If ntap is odd, it will be replaced by ntap+1.\n"
       "        * ntap must be in the range 8..2000 (inclusive).\n"
       "\n"
       "OPTIONS:\n"
       "--------\n"
       " -TR dd          = Set time grid spacing to 'dd' [default is 1.0]\n"
       " -band fbot ftop = Alternative way to specify the passband\n"
       " -ntap nnn       = Alternative way to specify the number of taps\n"
       "\n"
       "EXAMPLES:\n"
       "---------\n"
       "  FIRdesign 0.01 0.10 180 | 1dplot -stdin\n"
       "  FIRdesign 0.01 0.10 180 | 1dfft -nodetrend -nfft 512 stdin: - \\\n"
       "            | 1dplot -stdin -xaxis 0:0.5:10:10 -dt 0.001953\n"
       "\n"
       "The first line plots the filter weights\n"
       "The second line plots the frequency response (0.001953 = 1/512)\n"
       "\n"
       "NOTES:\n"
       "------\n"
       "* http://en.wikipedia.org/wiki/Parks-McClellan_filter_design_algorithm\n"
       "* The Remez algorithm code is written and GPL-ed by Jake Janovetz\n"
       "* Multiple passbands could be designed this way; let me know if you\n"
       "  need such an option; a Hilbert transform FIR is also possible\n"
       "* Don't try to be stupidly clever when using this program\n"
       "* RWCox -- May 2012\n"
     ) ;
     exit(0);
   }

   /*-- option processing --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     /* -TR */

     if( strcasecmp(argv[iarg],"-TR")  == 0 ||
         strcasecmp(argv[iarg],"-dt")  == 0 ||
         strcasecmp(argv[iarg],"-del") == 0 ||
         strcasecmp(argv[iarg],"-dx")  == 0   ){

       if( ++iarg >= argc ) ERROR_exit("need argument after %s",argv[iarg-1]) ;
       TR = strtod(argv[iarg],NULL) ;
       if( TR <= 0.0 ) ERROR_exit("Illegal value after %s",argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     /* -ntap */

     if( strcasecmp(argv[iarg],"-ntap") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("need argument after %s",argv[iarg-1]) ;
       ntap = (int)strtod(argv[iarg],NULL) ;
       if( ntap < 8 || ntap > 2000 )
         ERROR_exit("Illegal value after %s",argv[iarg-1]) ;
       iarg++ ; continue ;
     }

     /* -band */

     if( strcasecmp(argv[iarg],"-band") == 0 ){
       if( ++iarg >= argc-1 ) ERROR_exit("need 2 arguments after %s",argv[iarg-1]) ;
       fbot = strtod(argv[iarg++],NULL) ;
       ftop = strtod(argv[iarg++],NULL) ;
       if( ftop <= fbot ) ERROR_exit("Disorderd values after %s",argv[iarg-3]) ;
       continue ;
     }

     /* ssttooppiidd */

     ERROR_exit("Unknown option '%s'",argv[iarg]) ; exit(1) ;
   }

   /*-- get fbot ftop if not already present --*/

   if( fbot < 0.0f && ftop < 0.0f ){
     if( iarg >= argc-1 ) ERROR_exit("Need 2 arguments for fbot ftop") ;
     fbot = strtod(argv[iarg++],NULL) ;
     ftop = strtod(argv[iarg++],NULL) ;
     if( ftop <= fbot ) ERROR_exit("Disorderd fbot ftop values") ;
   }

   /*-- get ntap if not already present --*/

   if( ntap < 0 ){
     if( iarg >= argc ) ERROR_exit("Need argument for ntap") ;
     ntap = (int)strtod(argv[iarg],NULL) ;
     if( ntap < 8 || ntap > 2000 )
       ERROR_exit("Illegal value after %s",argv[iarg-1]) ;
     iarg++ ;
   }

   /*-- make ntap even, if need be --*/

   if( ntap%2 ){
     ntap++ ; INFO_message("ntap increased to %d (to be even)",ntap) ;
   }

   /*-- scale frequencies by TR to get them into the TR=1 case --*/

   fbot *= TR ; ftop *= TR ; df = 1.0/ntap ; fdel = 1.1*df ; dff = 1.0444*df ;

   /*-- edit frequencies if needed --*/

   if( fbot <= fdel && fbot != 0.0 ){
     fbot = 0.0 ; INFO_message("fbot re-set to 0") ;
   }
   if( ftop >= 0.5-fdel && ftop != 0.5 ){
     ftop = 0.5 ; INFO_message("ftop re-set to Nyquist 0.5/TR=%g\n",0.5/TR) ;
   }
   if( fbot == 0.0 && ftop == 0.5 )
     ERROR_exit("fbot=0 and ftop=Nyquist=0.5/TR=%g ==> nothing to do",0.5/TR) ;

   /*-- are fbot and ftop too close for comfort? --*/

   dqq = 3.0*fdel - (ftop-fbot) ;  /* should be negative */
   if( dqq > 0.0 ){
     dqq *= 0.5 ;
     INFO_message("fbot=%g and ftop=%g are too close: adjusting",fbot/TR,ftop/TR) ;
     fbot -= dqq ; ftop += dqq ;
     ININFO_message("adjusted fbot=%g  ftop=%g",fbot/TR,ftop/TR) ;
     if( fbot <= fdel && fbot != 0.0 ){
       fbot = 0.0 ; ININFO_message("and now fbot re-set to 0") ;
     }
     if( ftop >= 0.5-fdel && ftop != 0.5 ){
       ftop = 0.5 ; ININFO_message("and now ftop re-set to Nyquist 0.5/TR=%g\n",0.5/TR) ;
     }
   }

   /*-- initialize number of bands (might end up as 2 or 3) --*/

   nband = 0 ;

   /*-- reject below fbot, if fbot > 0 --*/

   if( fbot > 0.0 ){
     weight[nband] = 1.0 ; desired[nband]  = 0 ;
     band[2*nband] = 0.0 ; band[2*nband+1] = fbot-dff ;
     nband++ ;
   }

   /*-- pass between fbot and ftop --*/

   weight[nband] = 1.0 ; desired[nband] = 1 ;
   if( fbot > 0.0 ){
     band[2*nband] = fbot+dff ; band[2*nband+1] = (ftop < 0.5) ? ftop-dff : 0.5 ;
   } else {
     band[2*nband] = 0.0      ; band[2*nband+1] = ftop-dff ;
   }
   nband++ ;

   /*-- reject above ftop, if ftop < Nyquist --*/

   if( ftop < 0.5 ){
     weight[nband] = 1.0 ;      desired[nband]  = 0   ;
     band[2*nband] = ftop+dff ; band[2*nband+1] = 0.5 ;
     nband++ ;
   }

   /*-- compute FIR weights --*/

   remez( h, ntap, nband, band, desired, weight, BANDPASS ) ;

   /*-- print --*/

   for( i=0 ; i < ntap ; i++ ) printf("%23.20f\n", h[i]) ;

   exit(0) ;
}
