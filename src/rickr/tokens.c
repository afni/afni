/*
 * tokens - a tokenizing program
 *
 * written in the mid-1990's...  Rick Reynolds
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct
{
    FILE * fpin;
    FILE * fpout;

    char * infile;
    char * outfile;

    int    num_ex;
    int  * extra;
} control_s;

control_s gcs;
 

int read_args  ( int argc, char * argv[], control_s * C );
int print_list ( control_s * C );
int show_help  ( char * prog );
int cleanup    ( control_s * C );

int is_C_word_char ( int c );


int main ( int argc, char * argv[] )
{
    control_s * C = &gcs;
    int         rv = 0;

    rv = read_args( argc, argv, C );
    if( rv == 1 ) return 0;
    if( rv <  0 ) return 1;
    /* else, continue */

    if ( print_list( C ) )
	return 1;

    if ( cleanup( C ) )
	return 1;

    return 0;
}

int cleanup( control_s * C )
{
    if( C->fpin != stdin )
       fclose( C->fpin );

    if( C->extra )
       free( C->extra );

    return 0;
}

int print_list( control_s * C )
{
    int  done = 0, wcount = 0;
    int  c;

    while ( ! done )
    {
	c = fgetc( C->fpin );

	while ( !feof( C->fpin ) && !is_C_word_char(c) )
	    c = fgetc( C->fpin );

	if ( feof( C->fpin ) )
	    break;

	/* we have a word */
	while ( !feof( C->fpin ) && is_C_word_char(c) )
	{
            putchar(c);
	    c = fgetc( C->fpin );
	}
        putchar('\n');
	
	wcount++;
    }

    return 0;
}

int read_args( int argc, char * argv[], control_s * C )
{
    int ac;

    memset( C, 0, sizeof( control_s ) );

    for ( ac = 1; ac < argc; ac++ )
    {
	if ( ! strncmp( argv[ac], "-extra", 3 ) )
	{
	    if ( ++ac >= argc )
	    {
		fprintf( stderr, "error: missing arg for extra\n" );
		return -1;
	    }

	    C->num_ex++;
	    C->extra = (int *)realloc(C->extra, C->num_ex * sizeof(int));
	    C->extra[C->num_ex-1] = argv[ac][0];
	}
	else if ( ! strncmp( argv[ac], "-infile", 3 ) )
	{
	    if ( ++ac >= argc )
	    {
		fprintf( stderr, "error: missing input file\n" );
		return -1;
	    }

	    C->infile = argv[ac];
	}
	else if ( ! strncmp( argv[ac], "-help", 2 ) )
	{
	    show_help(argv[0]);
	    return 1;
	}
	else
	{
	    fprintf( stderr, "error: invalid argument '%s'\n", argv[ac] );
	    return -1;
	}
    }

    if ( C->infile == NULL )
	C->fpin = stdin;
    else
    {
	C->fpin = fopen( C->infile, "r" );
	if ( C->fpin == NULL )
	{
	    fprintf( stderr, "failure: could not open <%s> for reading\n",
		     C->infile );
	    return -1;
	}
    }

    return 0;
}

int show_help(char * prog)
{
   printf( "usage: %s [-infile INFILE] [-extra C] [...]\n", prog );
   printf( "\n"
           "   -infile : specify input file (stdin if none)\n"
           "   -extra  : specify extra character to count as valid\n"
           "             - can use this more than once\n"
           "             - I do not remember why I added this\n");
   printf( "------------------------------\n"
           "examples:\n\n"
           "   %s -infile script.txt\n"
           "   %s -infile script.txt | grep -i anat\n",
           prog, prog);
   printf( "------------------------------\n"
           "R. Reynolds, circa 1994\n");
   printf( "version 1.1, 1 Mar 2016\n\n");

   return 0;
}

int is_C_word_char( int c )
{
    int t;

    if ( ( isalnum( c ) ) || ( c == '_' ) )
	return 1;
    for( t = 0; t < gcs.num_ex; t++ )
        if( c == gcs.extra[t] )
            return 1;

    return 0;
}

