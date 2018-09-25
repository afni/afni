/**********************************************************************
 * column concatination of files
 *
 * - rickr 1/25/02
 **********************************************************************
**/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/* --- macros ------------------------------------------- */

#define MAX_FILES 1024
#define MAX_SEP   256

#define P_SUCCESS 0
#define P_EXIT    1
#define P_FAILURE 2

#define USE_SHORT 1
#define USE_FULL  2

/* --- structs ------------------------------------------ */

typedef struct
{
    int    num_files;
    int    line;
    char   separator[MAX_SEP];
    char * fnames[MAX_FILES];
    FILE * flist [MAX_FILES];
} info_s;

/* --- protos ------------------------------------------- */

int  cat_files   ( info_s * I );
int  close_files ( info_s * I );
void fix_tabs    ( char * s );
int  open_files  ( info_s * I );
int  read_args   ( int argc, char * argv [], info_s * ip );
int  usage       ( char * prog, int style );

/* --- main --------------------------------------------- */

int main ( int argc, char * argv[] )
{
    info_s I;
    int    rv;

    if ( (rv = read_args( argc, argv, &I )) != 0 ) {
	if ( rv == P_EXIT) return 0;
        else               return 1;
    }

    if ( (rv = open_files( &I )) != 0 )
	return rv;

    (void)cat_files( &I );

    (void)close_files( &I );

    return 0;
}

int cat_files( info_s * I )
{
    FILE * fp;
    int    done = 0, close_stage = 0;
    int    c, filec;
    int    linec = 0, show;
    
    while( !done )
    {
	done = 1;

        show = I->line <= 0 || I->line == (linec+1);

        for ( filec = 0; filec < I->num_files; filec++ )
	{
	    fp = I->flist[filec];

	    if ( fp == NULL )	/* skip any closed files */
		continue;

	    /* copy one line for this file */
	    c = fgetc(fp);
	    if ( !feof(fp) && (c != '\n') && !done )	/* print separator? */
	    {
		if( show ) fputs( I->separator, stdout );
	    }

	    while ( !feof(fp) && (c != '\n') )
	    {
		if ( close_stage == 1 )	/* so file lengths do not match */
		    close_stage = 2;

		/* done = 0;  rickr - move to not closed */
                if( show ) putchar(c);
		c = fgetc(fp);
            }

	    if ( feof(fp) )		/* if this file is done, close it */
	    {
		if ( close_stage == 0 )	/* note that some file has closed */
		    close_stage = 1;

		fclose(fp);
		I->flist[filec] = NULL;
	    }
	    else
		done = 0;
	}

	if ( !done ) {
	    if( show ) putchar( '\n' );
            linec++;
        }
    }

    if ( close_stage == 2 )
	fprintf( stderr, "** warning: file lengths do not match\n" );

    return P_SUCCESS;
}

int close_files( info_s * I )
{
    int filec;

    for ( filec = 0; filec < I->num_files; filec++ )
	if ( I->flist[filec] && I->flist[filec] != stdin )
	    fclose( I->flist[filec] );

    return P_SUCCESS;
}

int open_files( info_s * I )
{
    int c, filec, nstdin=0;

    for ( filec = 0; filec < I->num_files; filec++ )
    {
        /* allow for one stdin stream           15 Dec 2016 */
        if( ! strcmp(I->fnames[filec], "-") ||
            ! strcmp(I->fnames[filec],"stdin") ) {
           if( nstdin ) {
              fprintf(stderr,"** cannot read stdin more than once\n");
              return( P_FAILURE );
           }
           nstdin++;

           I->flist[filec] = stdin;
        }
	else if ( (I->flist[filec] = fopen(I->fnames[filec], "r")) == NULL )
	{
	    fprintf(stderr,"error: failed to open %s for reading, exiting...\n",
                    I->fnames[filec]);
	    for (c = 0; c < filec; c++)  /* close previously opened files */
	 	fclose(I->flist[c]);

	    return( P_FAILURE );
	}
    }

    return( P_SUCCESS );
}

int read_args( int argc, char * argv [], info_s * I )
{
    int ac, done, slen;

    /* quick check for help */
    if ( ( argc < 2 ) || !strncmp( argv[1], "-h", 2 ) )
	return( usage( argv[0], USE_FULL ) );

    /* defaults */
    I->num_files = 0;
    I->line = 0;
    I->separator[0] = ' ';
    I->separator[1] = '\0';

    for ( ac = 1, done = 0; (ac < argc) && !done; ac++ )
    {
	if ( strncmp( argv[ac], "-sep", 4 ) == 0 )
      	{
	    if ( ++ac >= argc )
	    {
		fprintf(stderr, "error: missing argument to '-sep'\n" );
		fprintf(stderr, "\n"
                                "       consider '-help' for more info\n"
                       );
		return( P_FAILURE );
	    }

            slen = strlen(argv[ac]);
            if( slen > MAX_SEP-1 ) {
                fprintf(stderr,"** limit of %d on separator length, sorry!",
                        MAX_SEP);
		return( P_FAILURE );
            }
	    strcpy( I->separator, argv[ac] );
	    fix_tabs( I->separator );
	}
	else if ( strcmp( argv[ac], "-line" ) == 0 )
        {
	    if ( ++ac >= argc ) {
		fprintf(stderr, "error: missing argument to '-line'\n" );
		return( P_FAILURE );
	    }
	    I->line = atoi(argv[ac]);
            if( I->line <= 0 ) {
                fprintf(stderr,"** -line (%s) should be > 0\n", argv[ac]);
		return( P_FAILURE );
            }
        }
	else 
        {
            if ( I->num_files >= MAX_FILES ) {
                fprintf(stderr,"** limit of %d files, sorry!", MAX_FILES);
		return( P_FAILURE );
            }
	    I->fnames[I->num_files] = argv[ac];
	    I->num_files++;
        }
    }

    return P_SUCCESS;
}

int usage ( char * prog, int style )
{
    if ( style == USE_FULL )
    {
	printf("\n");
	printf("  %s : catenate files horizontally\n", prog);
	printf("\n");
	printf("  Output is sent to stdout, so redirection of output to\n"
	       "  a file may be desirable.\n"
	       "\n"
	       "  Each line of output is the concatination of each current\n"
	       "  line from the input files, all on the same line, and\n"
	       "  separated by a space.  If different separation is desired,\n"
	       "  such as a tab, please use the -sep option.\n"
	       "\n"
	       "  ** Note that using '-' or 'stdin' for an input file means\n"
	       "     to read from stdin.  One such stream is allowed.\n"
	       "\n"
	       "  Optionos:\n"
	       "     -line LINE_NUM : print only line #LINE_NUM (1-based)\n"
	       "                      e.g. -line 1   (shows top line)\n"
	       "     -sep sep_str   : use sep_str as separation string\n"
	       "\n"
	       "  Examples:\n"
	       "\n"
	       "     %s -help\n"
	       "     %s file_a file_b\n"
	       "     %s file_a file_b file_c > output_file\n"
	       "     %s -line 17 file_a file_b file_c > output_file\n"
	       "     %s -sep : file_a file_b > output_file\n"
	       "     %s -sep '\\t' file_a file_b > output_file\n"
	       "     %s -sep ' : ' file_a file_b > output_file\n"
	       "     cat file_a | %s -line 27 stdin\n"
	       "\n"
	       "R Reynolds    Jan, 2002 (distributed Aug, 2012)\n"
	       "\n",
               prog, prog, prog, prog, prog, prog, prog, prog
              );
    }
    else
	fprintf( stderr, "usage : %s [options] file1 file2 ...\n", prog );

    return P_EXIT;
}

void fix_tabs ( char * s )
{
    char * c1, * c2;

    c1 = c2 = s;
    while ( *c2 )
    {
	if ( ( *c2 == '\\' ) && ( *(c2+1) == 't' ) )
	{
	    *c1 = '\t';
	    c2++;
	}
	else
	    *c1 = *c2;

	c1++; c2++;
    }

    *c1 = *c2;     /* be sure to get null character */
}

