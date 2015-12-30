/* 
   Functions to do expression evaluations, light.
    
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "mrilib.h"
#include "niml/niml.h"
#include "../niml/niml_private.h"
#include "xutil.h"
#include "../suma_suma.h"


/* This function is essentially sambowry's eval() as posted on 
   http://stackoverflow.com 
*/  
char *SUMA_bool_eval( char *expr, byte *res )
{
  enum { LEFT, OP1, MID, OP2, RIGHT } state = LEFT;
  enum { AND, OR } op=OR;
  int mid=0, NEG=0;
  byte tmp;

  for( ; ; expr++, state++, NEG=0 ){
    for( ;; expr++ )
         if( *expr == '!'     ) NEG = !NEG;
    else if( *expr != ' '     ) break;

         if( *expr == '0'     ){ tmp  =  NEG; }
    else if( *expr == '1'     ){ tmp  = !NEG; }
    else if( *expr == 'A'     ){ op   = AND; expr+=2; }
    else if( *expr == '&'     ){ op   = AND; expr+=1; }
    else if( *expr == 'O'     ){ op   = OR;  expr+=1; }
    else if( *expr == '|'     ){ op   = OR;  expr+=1; }
    else if( *expr == '('     ){ expr = SUMA_bool_eval( expr+1, &tmp ); 
                                 if(NEG) tmp=!tmp; }
    else if( *expr == '\0' ||
             *expr == ')'     ){ if(state == OP2) *res |= mid; return expr; }

         if( state == LEFT               ){ *res  = tmp;               }
    else if( state == MID   && op == OR  ){  mid  = tmp;               }
    else if( state == MID   && op == AND ){ *res &= tmp; state = LEFT; }
    else if( state == OP2   && op == OR  ){ *res |= mid; state = OP1;  }
    else if( state == RIGHT              ){  mid &= tmp; state = MID;  }
  }
}

#define EVAL_DBG 0
SUMA_Boolean SUMA_bool_mask_eval(int N_vals, int N_vars, byte **mask, char *expr,
                                 byte *res, byte *varcol, byte *rescol, 
                                 char **resstr)
{
   static char FuncName[]={"SUMA_bool_mask_eval"};
   char *extmp=NULL;
   int i=0, k, A, R, G, B, ivar, n;
   float sc, x;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (N_vars != 26) {
      SUMA_S_Err("mask must be an array of 26 byte * pointers");
      SUMA_RETURN(NOPE);
   }
   if (!res) {
      SUMA_S_Err("Need return pointer");
      SUMA_RETURN(NOPE);
   }
   if ((rescol && !varcol)) {
      SUMA_S_Err("Need rescol for varcol");
      SUMA_RETURN(NOPE);
   }
   if (resstr && !rescol) {
      SUMA_S_Err("resstr not good without rescol");
      SUMA_RETURN(NOPE);
   }
   extmp = SUMA_copy_string(expr);
   for (i=0; i < N_vals; ++i) {
      /* create the expression */
      k = 0; 
      while (expr[k]!='\0') {
         ivar = expr[k] - 'a';
         if (ivar >= 0 && ivar <= 26) {
            if (IN_MASK(mask[ivar], i)) extmp[k] = '1';
            else extmp[k] = '0';
         }
         ++k;
      }
      /* evaluate */
      SUMA_bool_eval(extmp, res+i);
      #if EVAL_DBG
      SUMA_LH("%s = %d", extmp, res[i]);
      #endif
   }
   SUMA_ifree(extmp);
   
   if (rescol) {
      for (i=0; i < N_vals; ++i) {
         if (res[i]) {/* We need that one, blend the colors */
            A = R = G = B = n = 0;
            k = 0;
            while (expr[k]!='\0') {
               ivar = expr[k] - 'a';
               if (ivar >= 0 && ivar <= 26) {
                  if (IN_MASK(mask[ivar], i)) {
                     R += varcol[4*ivar+0]*varcol[4*ivar+3];
                     G += varcol[4*ivar+1]*varcol[4*ivar+3];
                     B += varcol[4*ivar+2]*varcol[4*ivar+3];
                     A += varcol[4*ivar+3];
                     if (resstr) resstr[i][n]=expr[k];
                     n++;
                  }
               }
               ++k;
            }
            if (resstr) resstr[i][n]='\0';
            sc = A/(float)n; /* Consider an additional dimming factor from sv->*/
            if ((x=R/sc) > 255) rescol[4*i  ] = 255;
            else rescol[4*i  ] = (byte)x; 
            if ((x=G/sc) > 255) rescol[4*i+1] = 255;
            else rescol[4*i+1] = (byte)x;
            if ((x=B/sc) > 255) rescol[4*i+2]=255;
            else rescol[4*i+2] = (byte)x;
            if ( sc > 255) rescol[4*i+3]=255;            
            else rescol[4*i+3] = (byte)sc;
         } else {
            rescol[4*i  ] = 128;
            rescol[4*i+1] = 128;
            rescol[4*i+2] = 128;
            rescol[4*i+3] = 0;
            if (resstr) resstr[i][0]='\0';
         }
         #if EVAL_DBG
         SUMA_LH("Color at %d: %-3d %-3d %-3d %-3d\n", 
                  i, rescol[4*i  ], rescol[4*i+1], rescol[4*i+2], rescol[4*i+3]);
         #endif
      }
   }
   
   SUMA_RETURN(YUP);
}

void SUMA_set_bool_var_in_expr(char *expr, char var, byte val)
{
   int k;
   k = 0;
   while (expr[k]!='\0') {
      if (expr[k] == var) {
         if (val) expr[k] = '1';
         else expr[k] = '0';
      }
      ++k;
   }
   return;
}

SUMA_Boolean SUMA_bool_eval_truth_table(char *dexpr, byte use_orig) 
{
   static char FuncName[]={"SUMA_bool_eval_truth_table"};
   int n, ivar, k, N_used, N_rows, stp, block;
   byte used[26], ee;
   char these[26], **allexp=NULL, **allvars=NULL, 
        allv[26*2+1], expr[1024], tight[1024];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!dexpr || strlen(dexpr)>512) {
      SUMA_S_Err("NULL expression or too long a string");
      SUMA_RETURN(NOPE);
   }
   
   if (!use_orig) {
      SUMA_DispExpr_To_EvalExpr(dexpr, expr, tight);   
   } else {
      strcpy(expr, dexpr);
   }
   memset(used,0, sizeof(byte)*26);
   
   N_used = 0;
   k = 0;
   while (expr[k]!='\0') {
      ivar = expr[k] - 'a';
      if (ivar >= 0 && ivar <= 26) {
         if (!used[ivar]) {
            these[N_used] = expr[k];
            ++N_used;
            used[ivar] = 1;
         }
      }
      ++k;
   }
   
   for (k=0; k<N_used; ++k) {
      allv[2*k] = these[k];
      allv[2*k+1] = ' ';
   }
   allv[2*N_used] = '\0';
   
   /* preallocate for all outcomes */
   N_rows = pow(2,N_used);
   allexp = (char **)SUMA_calloc(N_rows,sizeof(char *));
   for (k=0; k<N_rows; ++k) allexp[k] = SUMA_copy_string(expr);
   allvars = (char **)SUMA_calloc(N_rows,sizeof(char *));
   for (k=0; k<N_rows; ++k) allvars[k] = SUMA_copy_string(allv);
   
   for (k=0; k<N_used; ++k) { /* which var to replace */
      stp = pow(2,k);
      block = 0;
      if (LocalHead) 
         fprintf(stderr,"k=%d, N_used = %d, stp=%d\n", 
                      k, N_used, stp);
      for (n=0; n<N_rows; ++n) {
         if (!(n%(2*stp))) ++block;
         if (n < 2*stp*block-stp) {
            SUMA_set_bool_var_in_expr(allexp[n], these[N_used-k-1], 0);
            SUMA_set_bool_var_in_expr(allvars[n], these[N_used-k-1], 0);
         } else {
            SUMA_set_bool_var_in_expr(allexp[n], these[N_used-k-1], 1);
            SUMA_set_bool_var_in_expr(allvars[n], these[N_used-k-1], 1);
         }
         if (LocalHead) 
            fprintf(stderr,"n=%d, block=%d (%d): %s\n", 
                    n, block, (int)(pow(2,stp)*block), allexp[n]);
      }
   }
   
   /* show expressions */
   fprintf(SUMA_STDERR,"Truth Table for %s:\n", dexpr);
   fprintf(SUMA_STDERR,"%s: %s = ?\n", allv, expr);
   fprintf(SUMA_STDERR,"----------------\n");
   for (n=0; n<N_rows; ++n) {
      SUMA_bool_eval(allexp[n], &ee);
      fprintf(SUMA_STDERR,"%s: %s = %d\n", allvars[n], allexp[n], ee);
   }
   
   /* clear */
   for (n=0; n<N_rows; ++n) SUMA_ifree(allexp[n]);
   SUMA_ifree(allexp);
   for (n=0; n<N_rows; ++n) SUMA_ifree(allvars[n]);
   SUMA_ifree(allvars);
   
   SUMA_RETURN(YUP);
}

void SUMA_bool_eval_test( char *expr, byte exprval )
{
   int i;
   byte result;
   
   if (expr) {
      SUMA_bool_eval(expr, &result);
      printf("expr: '%s' result: %i  %s\n",
             expr,result,result==exprval?"OK":"FAILED");
   } else {
      for (i=0; i<10; ++i) {
         switch (i) {
            case 0:
               expr    = "((( 1 && 0 && 0) || 1) && ((0 || 1) && 1))";
               exprval = (((( 1 && 0 && 0) || 1) && ((0 || 1) && 1)));
               break;
            case 1:
               expr    = "!(0 || (1 && 0)) || !1 && 0";
               exprval = (!(0 || (1 && 0)) || !1 && 0);
               break;
            case 2:
               expr    = "1 || (1 && 0)";
               exprval = (1 || (1 && 0));
               break;
            default:
               continue;
               break;
         }
         SUMA_bool_eval(expr, &result);
         printf("expr: '%s' result: %i  %s\n",
                expr,result,result==exprval?"OK":"FAILED");
      }
      /* Now test the vectorized form */
      {
         char *eq=NULL;
         byte **mask = NULL;
         byte res[20], rescol[20*4];
         byte varcol[26*4] = { 255,  0,  0,255, /* col of a */
                                 0,255,  0,255, /* col of b */
                               255,255,255,255, /* col of c */
                                 0,  0,255,255  /* col of d */ };
         char **resstr=NULL;
                                
         mask = (byte **)SUMA_calloc(26, sizeof(byte *));
         mask[0] = (byte *)SUMA_calloc(8, sizeof(byte)); /* fill a variable */
            mask[0][4] = mask[0][5] = mask[0][6] = mask[0][7] = 1; 
         mask[1] = (byte *)SUMA_calloc(8, sizeof(byte)); /* fill b variable */
            mask[1][2] = mask[1][3] = mask[1][6] = mask[1][7] = 1;
         mask[3] = (byte *)SUMA_calloc(8, sizeof(byte)); /* fill d variable */
            mask[3][1] = mask[3][3] = mask[3][5] = mask[3][7] = 1;
         resstr = (char **)SUMA_calloc(20, sizeof(char *));
         for(i=0; i<20; ++i) resstr[i]=(char *)SUMA_calloc(27, sizeof(char ));
         
         eq = "a | (b & d)"; /* can use ||/&& or |/& or AND/OR */
         SUMA_bool_mask_eval(8, 26, mask, eq, res, varcol, rescol, resstr);  
         
         printf("Results for eq: %s\n", eq);
         for (i=0; i<8; ++i) {
            printf("%d, col: %-3d %-3d %-3d %-3d, str: %s\n", 
                   res[i], 
                   rescol[4*i], rescol[4*i+1], rescol[4*i+2],rescol[4*i+3],
                   resstr[i]);
         }
         
         for (i=0; i<26; ++i) SUMA_ifree(mask[i]);
         SUMA_ifree(mask);
         for (i=0; i<20; ++i) SUMA_ifree(resstr[i]);
         SUMA_ifree(resstr);
      }
   }
}
