%{
/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *********************************************************************/

/* lex specification for tokens for ncgen */

/* Fill value used by ncdump from version 2.4 and later.  Should match
   definition of FILL_STRING in ../ncdump/vardata.h */
#define FILL_STRING "_"
#define XDR_INT_MIN (-2147483647-1)
#define XDR_INT_MAX 2147483647

char errstr[100];		/* for short error messages */

#include <string.h>
#include <ctype.h>
#include "genlib.h"
#include "ncgentab.h"

#define YY_BREAK                /* defining as nothing eliminates unreachable
				   statement warnings from flex output, 
                                   but make sure every action ends with
                                   "return" or "break"! */

%}

%p 6000

escaped		\\.
nonquotes	([^"\\]|{escaped})*
exp		([eE][+-]?[0-9]+)
%%
\/\/.*		        { /* comment */ 
                          break;
                        }

\"{nonquotes}\"		{
			 if(yyleng > MAXTRST) {
				yyerror("string too long, truncated\n");
			        yytext[MAXTRST-1] = '\0';
			 }
			 expand_escapes(termstring,(char *)yytext,yyleng);
		 	 return (TERMSTRING);
		        }

float|FLOAT|real|REAL	{return (FLOAT_K);}
char|CHAR		{return (CHAR_K);}
byte|BYTE		{return (BYTE_K);}
short|SHORT		{return (SHORT_K);}
long|LONG|int|INT|integer|INTEGER	{return (INT_K);}
double|DOUBLE		{return (DOUBLE_K);}
unlimited|UNLIMITED	{int_val = -1;
			 return (NC_UNLIMITED_K);}

dimensions:|DIMENSIONS:	{return (DIMENSIONS);}
variables:|VARIABLES:	{return (VARIABLES);}
data:|DATA:		{return (DATA);}
(netcdf|NETCDF|netCDF)[ \t]+[^\{]+	{
		char *s = (char*)yytext+strlen("netcdf");
		char *t = (char*)yytext+yyleng-1;
		while (isspace(*s))
			s++;
		while (isspace(*t))
			t--;
		t++;
                if (t-s+1 < 1) {
                        yyerror("netCDF name required");
                        return (DATA); /* generate syntax error */
                }
		netcdfname = (char *) emalloc(t-s+1);
		(void) strncpy(netcdfname, s, t-s);
		netcdfname[t-s] = '\0';
		return (NETCDF);
		}
DoubleInf|NaN|-?Infinity { /* missing value (pre-2.4 backward compatibility) */
                if (yytext[0] == '-') {
		    double_val = -FILL_DOUBLE;
                } else {
		    double_val = FILL_DOUBLE;
                }
		return (DOUBLE_CONST);
		}
FloatInf|-?Inff	{ /* missing value (pre-2.4 backward compatibility) */
                if (yytext[0] == '-') {
		    float_val = -FILL_FLOAT;
                } else {
		    float_val = FILL_FLOAT;
                }
		return (FLOAT_CONST);
		}
[A-Za-z_][A-Z.@#\[\]a-z_0-9-]*	{
                if (STREQ((char *)yytext, FILL_STRING))
		        return (FILLVALUE);
		if ((yylval = lookup((char *)yytext)) == NULL) {
			yylval = install((char *)yytext);
			}
		return (IDENT);
		}

\n		{
		lineno++ ;
                break;
		}

[+-]?[0-9]*[0-9][Bb]  {
                int ii;
		if (sscanf((char*)yytext, "%d", &ii) != 1) {
		    sprintf(errstr,"bad byte constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
                byte_val = ii;
		if (ii != (int)byte_val) {
		    sprintf(errstr,"byte constant out of range (-128,127): %s",(char*)yytext);
		    yyerror(errstr);
		}
		return (BYTE_CONST);
                }

[+-]?[0-9]*\.[0-9]*{exp}?[LlDd]?|[+-]?[0-9]*{exp}[LlDd]? {
		if (sscanf((char*)yytext, "%le", &double_val) != 1) {
		    sprintf(errstr,"bad long or double constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
                return (DOUBLE_CONST);
                }
[+-]?[0-9]*\.[0-9]*{exp}?[Ff]|[+-]?[0-9]*{exp}[Ff] {
		if (sscanf((char*)yytext, "%e", &float_val) != 1) {
		    sprintf(errstr,"bad float constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
                return (FLOAT_CONST);
                }
[+-]?[0-9]+[sS]|0[xX][0-9a-fA-F]+[sS] {
		if (sscanf((char*)yytext, "%hd", &short_val) != 1) {
		    sprintf(errstr,"bad short constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
		return (SHORT_CONST);
	        }
[+-]?([1-9][0-9]*|0)[lL]? {
    		char *ptr;
                errno = 0;
		double_val = strtod((char*)yytext, &ptr);
		if (errno != 0 && double_val == 0.0) {
		    sprintf(errstr,"bad numerical constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
                if (double_val < XDR_INT_MIN ||double_val > XDR_INT_MAX) {
                    return DOUBLE_CONST;
                } else {
                    int_val = (int) double_val;
                    return INT_CONST;
                }
	        }
0[xX]?[0-9a-fA-F]+[lL]? {
    		char *ptr;
                long long_val;
                errno = 0;
		long_val = strtol((char*)yytext, &ptr, 0);
		if (errno != 0) {
		    sprintf(errstr,"bad long constant: %s",(char*)yytext);
		    yyerror(errstr);
		}
                if (long_val < XDR_INT_MIN || long_val > XDR_INT_MAX) {
                    double_val = (double) long_val;
                    return DOUBLE_CONST;
                } else {
                    int_val = (int) long_val;
                    return INT_CONST;
                }
	        }
\'[^\\]\'          {
	        (void) sscanf((char*)&yytext[1],"%c",&byte_val);
		return (BYTE_CONST);
                }
\'\\[0-7][0-7]?[0-7]?\'  {
		byte_val = (char) strtol((char*)&yytext[2], (char **) 0, 8);
		return (BYTE_CONST);
                }
\'\\[xX][0-9a-fA-F][0-9a-fA-F]?\'  {
		byte_val = (char) strtol((char*)&yytext[3], (char **) 0, 16);
		return (BYTE_CONST);
                }
\'\\.\'        {
	       switch ((char)yytext[2]) {
	          case 'a': byte_val = '\007'; break; /* not everyone under-
						       * stands '\a' yet */
     	          case 'b': byte_val = '\b'; break;
		  case 'f': byte_val = '\f'; break;
		  case 'n': byte_val = '\n'; break;
		  case 'r': byte_val = '\r'; break;
		  case 't': byte_val = '\t'; break;
		  case 'v': byte_val = '\v'; break;
		  case '\\': byte_val = '\\'; break;
		  case '?': byte_val = '\177'; break;
		  case '\'': byte_val = '\''; break;
		  default: byte_val = (char)yytext[2];
	           }
		return (BYTE_CONST);
                }

[ \t\f]+	{ /* whitespace */ 
		  break;        
		}
.		return (yytext[0]) ;
