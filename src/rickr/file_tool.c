
#define VERSION         "3.2a (March 22, 2005)"

/*----------------------------------------------------------------------
 * file_tool.c  - display or modify (binary?) info in files
 *
 * This is a pretty generic file processing tool, originally designed
 * to display and modify data at random places in files, and also to
 * deal with GEMS 5.x image files (e.g. being able to replace subject
 * names with 'x's).
 *
 * options:
 *
 *     special options:
 *
 *        -help                display help
 *        -help_ge             display help on GE info structures
 *        -hist                display history
 *        -debug    LEVEL      show extra info  (0, 1 or 2)
 *        -version             show version information
 *
 *     required 'options':
 *
 *        -infiles  file1 ...  specify files to display/alter
 *
 *     GE info options
 *
 *        -ge_all              display GE header and extras info
 *        -ge_header           display GE header info
 *        -ge_extras           display extra GE image info
 *        -ge_uv17             diplay the value of the uv17 variable
 *        -ge_run              diplay the run number - same as uv17
 *        -ge_off              diplay file offsets for various fields
 *
 *     GEMS 4.x options
 *
 *        -ge4_all             display GEMS 4.x series and image headers
 *        -ge4_series          display GEMS 4.x series header
 *        -ge4_image           display GEMS 4.x image header
 *
 *     raw ascii options:
 *
 *        -length   LENGTH     number of bytes to display/modify
 *        -mod_data DATA       specify modification data
 *        -mod_type TYPE       specify modification with a value or string
 *        -offset   OFFSET     display/modify from OFFSET bytes into files
 *        -quiet               do not display header info with output
 *
 *     numeric options:
 *
 *        -disp_int2           display data as 2-byte integers
 *        -disp_int4           display data as 4-byte integers
 *        -disp_real4          display data as 4-byte floats
 *        -swap_bytes          use byte swapping for numbers
 *
 * examples:
 * 
 *    file_tool -help
 *    file_tool -help_ge
 *    file_tool -ge_all -infiles I.100
 *    file_tool -ge_run -infiles I.?42
 *    file_tool -offset 100 -length 32 -infiles file1 file2
 *    file_tool -offset 100 -length 32 -quiet -infiles file1 file2
 *    file_tool -disp_int2 -swap -offset 1024 -length 16 -infiles file3
 *    file_tool -mod_data "hi there" -offset 2515 -length 8 -infiles I.*
 *    file_tool -debug 1 -mod_data x -mod_type val -offset 2515 \
 *              -length 21 -infiles I.*
 *----------------------------------------------------------------------
*/

static char g_history[] = 
 "----------------------------------------------------------------------\n"
 " file_tool history:\n"
 "\n"
 " 1.0  September 11, 2002\n"
 "   - initial release\n"
 "\n"
 " 1.1  February 26, 2003\n"
 "   - added -quiet option\n"
 "   - use dynamic allocation for data to read\n"
 "\n"
 " 1.2  May 06, 2003  (will go to 2.0 after more changes are made)\n"
 "   - added interface for reading GEMS 4.x formatted image files\n"
 "   - added corresponding options -ge4_all, -ge4_image, -ge4_series\n"
 "   - added options to display raw numeric data:\n"
 "       disp_int2, disp_int4, disp_real4\n"
 "   - changed local version of l_THD_filesize to THD_filesize, as\n"
 "     the ge4_ functions may get that from mrilib.\n"
 "\n"
 " 2.0  May 29, 2003\n"
 "   - added information for ge4 study header\n"
 "   - added option -ge4_study\n"
 "\n"
 " 2.1  June 02, 2003\n"
 "   - changed format of call to ge4_read_header()\n"
 "   - made swap_[24]() static\n"
 "\n"
 " 2.2  July 27, 2003\n"
 "   - wrap unknown printed strings in NULL check\n"
 "\n"
 " 3.0  March 17, 2004\n"
 "   - added binary editing (gee, that's why I wrote it 18 months ago...)\n"
 "     (see -mod_type s/uint1, s/uint2, s/uint4, float4, float8\n"
 "   - added '-ge_off' option, to display file offsets for some GEMS fields\n"
 "   - added '-hist' option, to display this history\n"
 "\n"
 " 3.1  March 17, 2004\n"
 "   - only check length against data_len for string mods\n"
 "\n"
 " 3.2  March 24, 2004\n"
 "   - only check max length when modifying data (thanks, PSFB)\n"
 "\n"
 " 3.2a March 22, 2005\n"
 "   - removed all tabs\n"
 "----------------------------------------------------------------------\n";


/* ----------------------------------------------------------------------
 * todo:
 *
 * - add option '-help_ge4'
 * ----------------------------------------------------------------------
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "file_tool.h"
#include "ge4_header.h"

char g_rep_output_data[MAX_STR_LEN];    /* in case user doesn't pass it in */

static int swap_2 ( void * ptr );
static int swap_4 ( void * ptr );

/*------------------------------------------------------------
 *  - check that the program is used correctly
 *  - fill the param_t struct, based on user inputs
 *  - process the files (display/modify data from/in each file)
 *------------------------------------------------------------
*/
int main ( int argc, char * argv[] )
{
    param_t P;
    int     rv;

    if ( (rv = set_params( &P, argc, argv ) ) < 0 )
        return rv;
    else if ( rv > 0 )
        return 0;

    if ( (rv = attack_files( &P ) ) != 0 )
        return rv;

    return 0;
}

/*------------------------------------------------------------
 *  - foreach file
 *      - if we are displaying GE info, do so
 *      - else, display/modify pertinent data
 *------------------------------------------------------------
*/
int
attack_files( param_t * p )
{
    int fc, rv;

    for ( fc = 0; fc < p->num_files; fc++ )
    {
        if ( p->ge_disp )
        {
            if ( (rv = process_ge( p->flist[fc], p )) != 0 )
                return rv;
        }
        else if ( p->ge4_disp )
        {
            if ( (rv = process_ge4( p->flist[fc], p )) != 0 )
                return rv;
        }
        else if ( ( rv = process_file( p->flist[fc], p) ) != 0 )
            return rv;
    }

    return 0;
}


/*------------------------------------------------------------
 * Run the relevant GEMS 4.x processing functions.
 *------------------------------------------------------------
*/
int
process_ge4( char * filename, param_t * p )
{
    ge4_header H;
    int        rv;

    rv = ge4_read_header( &H, filename, 0 );

    if ( rv != 0 )
    {
        if ( p->ge4_disp )      /* then display the bad result */
        {
            printf( "%s : GEMS 4.x header failure : %d\n", filename, rv );
            return 0;
        }
        else                    /* else just return it */
            return -1;
    }

    if ( (p->debug > 1) || (p->ge4_disp & GE4_DISP_STUDY) )
        idisp_ge4_study_header( filename, &H.std_h );

    if ( (p->debug > 1) || (p->ge4_disp & GE4_DISP_SERIES) )
        idisp_ge4_series_header( filename, &H.ser_h );

    if ( (p->debug > 1) || (p->ge4_disp & GE4_DISP_IMAGE) )
        idisp_ge4_image_header( filename, &H.im_h );

    return 0;
}


/*------------------------------------------------------------
 * Run the relevant GE processing functions.
 *------------------------------------------------------------
*/
int
process_ge( char * filename, param_t * p )
{
    ge_header_info H;
    ge_extras      E;
    ge_off         off;
    int            rv;

    rv = read_ge_header( filename, &H, &E, &off );

    if ( rv != 0 )
    {
        if ( p->ge_disp )  /* if we are here to display - state results */
        {
            printf( "%s : GE header failure : %d\n", filename, rv );
            return 0;  /* don't fail out */
        }
        else
            return -1;     /* else, just return the results */
    }

    if ( (p->debug > 1) || (p->ge_disp & GE_HEADER) )
        r_idisp_ge_header_info( filename, &H );

    if ( (p->debug > 1) || (p->ge_disp & GE_EXTRAS ) )
        r_idisp_ge_extras( filename, &E );

    if ( p->ge_disp & GE_OFF )
        disp_ge_offsets( filename, &off );

    if ( p->ge_disp & GE_UV17 )
        printf( "%s : run # %d\n", filename, H.uv17 );

    return 0;
}


/*------------------------------------------------------------
 * Do the processing for the given file:
 *
 *   - open the file
 *   - go to the user-specified offset (may be 0)
 *   - read the relevant number of bytes
 *   - if display info, print out data
 *   - if modifying, go back to 'offset' and write data
 *   - close file
 *------------------------------------------------------------
*/
int
process_file( char * filename, param_t * p )
{
    FILE        * fp;
    static char * fdata = NULL;
    int           nbytes;

    if ( (fp = fopen( filename, "r+" )) == NULL )
    {
        fprintf( stderr, "failure: cannot open <%s> for 'rw'\n", filename );
        return -1;
    }

    if ( fdata == NULL )
    {
        fdata = (char*) calloc( p->length, sizeof(char) );
        if ( fdata == NULL )
        {
            fprintf( stderr, "failure: cannot allocate %d bytes for data\n",
                     p->length );
            return -1;
        }
    }

    if ( fseek( fp, p->offset, SEEK_SET ) )
    {
        fprintf( stderr, "failure: cannot seek to <%ld> in file '%s'\n",
                 p->offset, filename );
        fclose( fp );
        return -1;
    }

    if ( (nbytes = fread( fdata, 1, p->length, fp )) != p->length )
    {
        fprintf( stderr, "failure: read only %d of %d bytes from file '%s'\n",
                 nbytes, p->length, filename );
        fclose( fp );
        return -1;
    }

    /* display file contents */
    if ( !p->modify || p->debug )
    {
        if ( ! p->quiet && ! p->ndisp )
            printf( "<%s> : '", filename );

        /* handle the numeric display */
        if ( p->ndisp )
        {
            if ( disp_numeric_data( fdata, p, stdout ) )
            {
                fclose( fp );
                return -1;
            }
        }
        else if ( (nbytes = fwrite(fdata, 1, p->length, stdout)) != p->length )
        {
            fprintf( stderr, "\nfailure: wrote only %d of %d bytes to '%s'\n",
                     nbytes, p->length, "stdout" );
            fclose( fp );
            return -1;
        }
        if ( ! p->quiet && ! p->ndisp )
            puts( "'" );        /* single quote plus newline */
    }

    if ( p->modify )  /* if writing back to file */
    {
        if ( fseek( fp, p->offset, SEEK_SET ) )
        {
            fprintf( stderr, "failure: cannot re-seek to <%ld> in file '%s'\n",
                     p->offset, filename );
            fclose( fp );
            return -1;
        }

        if ( (nbytes = write_data_to_file( fp, filename, p ) ) < 0 )
        {
            fclose( fp );
            return -1;
        }

        if ( p->debug > 0 )
        {
            printf( "wrote '%s' (%d bytes) to file '%s', position %ld\n",
                    p->mod_data, p->length, filename, p->offset );
        }
    }

    fclose( fp );

    return 0;
}


/*------------------------------------------------------------
 *  Actually write data into the file stream.
 *
 *  return bytes written : on success
 *                    -1 : on error
 *------------------------------------------------------------
*/
int write_data_to_file( FILE * fp, char * filename, param_t * p )
{
    double   dval;
    char   * outp, * inp, * endp;
    int      dsize, c, nbytes;

    if ( (dsize = mtype_size( p->mod_type )) < 1 )
    {
        fprintf(stderr,"** bad size %d for mod_type %d\n", dsize, p->mod_type);
        return -1;
    }

    if ( p->debug > 1 )
        fprintf(stderr,"-- dsize = %d\n", dsize);

    /* deal with characters separately */
    if ( p->mod_type == MOD_STR || p->mod_type == MOD_CHAR )
    {
        int remaining = p->length - p->data_len; /* note remainder (if any) */

        if ( (nbytes=fwrite(p->mod_data, 1, p->data_len, fp)) != p->data_len )
        {
            fprintf( stderr, "\nfailure: wrote only %d of %d bytes to '%s'\n",
                     nbytes, p->data_len, filename );
            fclose( fp );
            return -1;
        }

        /* and fill with 0 */
        for ( c = 0; c < remaining; c++ )
            fputc( '\0', fp );

        return p->length;
    }

    if ( p->debug > 1 ) fprintf(stderr,"++ values: ");

    /* now write numerical values into g_rep_output_data */
    outp = g_rep_output_data;
    inp  = p->mod_data;
    memset(outp, 0, p->length);
    for ( c = 0; (c+dsize) <= p->length; c += dsize, outp += dsize )
    {
        dval = strtod( inp, &endp );

        if ( endp && inp != endp )   /* then we read something in */
        {
            switch (p->mod_type)
            {
                default:
                    fprintf(stderr,"** wdtf: bad type %d\n", p->mod_type);
                    return -1;
                case MOD_U1:
                    *(unsigned char *)outp = (unsigned char)dval;
                    break;
                case MOD_S1:
                    *(char *)outp = (char)dval;
                    break;
                case MOD_U2:
                    *(unsigned short *)outp = (unsigned short)dval;
                    break;
                case MOD_S2:
                    *(short *)outp = (short)dval;
                    break;
                case MOD_U4:
                    *(unsigned long *)outp = (unsigned long)dval;
                    break;
                case MOD_S4:
                    *(long *)outp = (long)dval;
                    break;
                case MOD_F4:
                    *(float *)outp = (float)dval;
                    break;
                case MOD_F8:
                    *(double *)outp = dval;
                    break;
            }

            if ( p->swap )
            {
                if ( dsize == 2 )
                    swap_2( (void *)outp );
                else if ( dsize == 4 )
                    swap_4( (void *)outp );
            }

            if ( p->debug > 1 ) fprintf(stderr," %f", dval);
        }
        else
            break;      /* done */

        inp = endp;     /* prepare to go after a new number */
    }

    if ( p->debug > 1 ) fputc('\n', stderr);

    /* now just write the data out */
    nbytes = fwrite(g_rep_output_data, 1, p->length, fp);
    if ( nbytes != p->length )
    {
        fprintf(stderr,"** wrote only %d of %d bytes of binary data\n",
                nbytes, p->length);
        return nbytes;
    }

    return p->length;
}

int mtype_size( int type )
{
    if ( type == MOD_STR  ) return 1;
    if ( type == MOD_CHAR ) return 1;
    if ( type == MOD_U1   ) return 1;
    if ( type == MOD_S1   ) return 1;

    if ( type == MOD_U2   ) return 2;
    if ( type == MOD_S2   ) return 2;

    if ( type == MOD_U4   ) return 4;
    if ( type == MOD_S4   ) return 4;

    if ( type == MOD_F4   ) return 4;
    if ( type == MOD_F8   ) return 8;

    return -1;
}

/*------------------------------------------------------------
 *  Read user arguments, and fill param_t struct.
 *
 *  - if modifying, verify arguments
 *------------------------------------------------------------
*/
int
set_params( param_t * p, int argc, char * argv[] )
{
    int ac;

    if ( argc < 2 )
    {
        usage( argv[0], USE_SHORT );
        return -1;
    }

    if ( !p || !argv )
    {
        fprintf( stderr, "failure: bad params to set_params: "
                 "p = <%p>, ac = <%d>, av = <%p>\n",
                 p, argc, argv );
        return -1;
    }

    /* clear out param struct - this sets all default values */
    memset( p, 0, sizeof(*p) );

    for ( ac = 1; ac < argc; ac++ )
    {
        /* check for -help_ge before -help, to allow for only -h */
        if ( ! strncmp(argv[ac], "-help_ge", 8 ) )
        {
            usage( argv[0], USE_GE );
            return -1;
        }
        else if ( ! strncmp(argv[ac], "-help", 3 ) )
        {
            usage( argv[0], USE_LONG );
            return -1;
        }
        else if ( ! strncmp(argv[ac], "-hist", 3 ) )
        {
            usage( argv[0], USE_HISTORY );
            return -1;
        }
        else if ( ! strncmp(argv[ac], "-debug", 6 ) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "missing option parameter: LEVEL\n"
                       "option usage: -debug LEVEL\n"
                       "    where LEVEL is the debug level (0,1 or 2)\n",
                       stderr );
                return -1; 
            }

            p->debug = atoi(argv[++ac]);
            if ( (p->debug < 0) || (p->debug > 2) )
            {
                fprintf( stderr, "invalid debug level <%d>\n", p->debug );
                return -1;
            }
        }
        else if ( ! strncmp(argv[ac], "-disp_int2", 10 ) )
        {
            p->ndisp |= NDISP_INT2;
        }
        else if ( ! strncmp(argv[ac], "-disp_int4", 10 ) )
        {
            p->ndisp |= NDISP_INT4;
        }
        else if ( ! strncmp(argv[ac], "-disp_real4", 11 ) )
        {
            p->ndisp |= NDISP_REAL4;
        }
        else if ( ! strncmp(argv[ac], "-mod_data", 6 ) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "missing option parameter: DATA\n"
                       "option usage: -mod_data DATA\n"
                       "    where DATA is the replacement string or numbers\n",
                       stderr );
                return -1; 
            }

            ac++;
            p->mod_data = argv[ac];
        }
        else if ( ! strncmp(argv[ac], "-mod_type", 6 ) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "missing option parameter: TYPE\n"
                       "option usage: -mod_type TYPE\n"
                       "    where TYPE is 'val' or 'str'\n",
                       stderr );
                return -1;
            }

            ac++;
            if ( (p->mod_type = check_mod_type(argv[ac])) == MOD_INVALID )
            {
                fputs( "option usage: -mod_type TYPE\n", stderr );
                fputs( "              where TYPE is 'str' or 'val'\n", stderr );
                return -1;
            }
        }
        else if ( ! strncmp(argv[ac], "-offset", 4 ) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "missing option parameter: OFFSET\n"
                       "option usage: -offset OFFSET\n"
                       "    where OFFSET is the file offset\n",
                       stderr );
                return -1;
            }

            p->offset = atoi(argv[++ac]);
            if ( p->offset < 0 )
            {
                fprintf( stderr, "bad file OFFSET <%ld>\n", p->offset );
                return -1;
            }
        }
        else if ( ! strncmp(argv[ac], "-length", 4 ) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "missing option parameter: LENGTH\n"
                       "option usage: -length LENGTH\n"
                       "    where LENGTH is the length to display or modify\n",
                       stderr );
                return -1;
            }

            p->length = atoi(argv[++ac]);
            if ( p->length < 0 )
            {
                fprintf( stderr, "bad LENGTH <%d>\n", p->length );
                return -1;
            }
        }
        else if ( ! strncmp(argv[ac], "-quiet", 2 ) )
        {
            p->quiet = 1;
        }
        else if ( ! strncmp(argv[ac], "-swap_bytes", 3 ) )
        {
            p->swap = 1;
        }
        else if ( ! strncmp(argv[ac], "-ver", 2 ) )
        {
            usage( argv[0], USE_VERSION );
            return 1;
        }
        else if ( ! strncmp(argv[ac], "-infiles", 4 ) )
        {
            if ( (ac+1) >= argc )
            {
                fputs( "missing input files...\n"
                       "option usage: -infiles file1 file2 ...\n",
                       stderr );
                return -1;
            }

            ac++;
            p->num_files = argc - ac;
            p->flist     = argv + ac;

            break;      /* input files finish the argument list */
        }
        /* continue with GE info displays */
        else if ( ! strncmp(argv[ac], "-ge_all", 7 ) )
        {
            p->ge_disp |= GE_ALL;
        }
        else if ( ! strncmp(argv[ac], "-ge_header", 7 ) )
        {
            p->ge_disp |= GE_HEADER;
        }
        else if ( ! strncmp(argv[ac], "-ge_extras", 7 ) )
        {
            p->ge_disp |= GE_EXTRAS;
        }
        else if ( ! strncmp(argv[ac], "-ge_off", 7 ) )
        {
            p->ge_disp |= GE_OFF;
        }
        /* allow both forms - uv17 means the run number */
        else if ( ! strncmp(argv[ac], "-ge_uv17", 7 ) ||
                  ! strncmp(argv[ac], "-ge_run", 7  )    )
        {
            p->ge_disp |= GE_UV17;
        }
        /* continue with GEMS 4.x info displays */
        else if ( ! strncmp(argv[ac], "-ge4_all", 7 ) )
        {
            p->ge4_disp |= GE4_DISP_ALL;
        }
        else if ( ! strncmp(argv[ac], "-ge4_image", 7 ) )
        {
            p->ge4_disp |= GE4_DISP_IMAGE;
        }
        else if ( ! strncmp(argv[ac], "-ge4_series", 8 ) )
        {
            p->ge4_disp |= GE4_DISP_SERIES;
        }
        else if ( ! strncmp(argv[ac], "-ge4_study", 8 ) )
        {
            p->ge4_disp |= GE4_DISP_STUDY;
        }
        /* finish with bad option */
        else
        {
            fprintf( stderr, "error: unknown option, <%s>\n", argv[ac] );
            return -1;
        }
    }

    if ( p->debug > 1 )
        disp_param_data( p );

    if ( p->num_files <= 0 )
    {
        fputs( "error: missing '-infiles' option\n", stderr );
        return -1;
    }

    /* if only displaying GE data, no further check are necessary */
    if ( p->ge_disp || p->ge4_disp )
        return 0;

    /* now do all other tests for displaying/modifying generic file data */

    if ( p->mod_data )
    {
        p->modify = 1;           /* so we plan to modify the data */
        p->data_len = strlen(p->mod_data);    /* note data length */
    }
    else
        p->modify = 0;                             /* be explicit */

    if ( p->length <= 0 )
    {
        fputs( "error: missing '-length' option\n", stderr );
        return -1;
    }

    if ( p->modify )
    {
        if ( p->length > MAX_STR_LEN )
        {
            fprintf( stderr, "failure: length <%d> exceeds maximum %d\n",
                 p->length, MAX_STR_LEN );
            return -1;
        }

        if ( p->mod_type == MOD_CHAR )
        {
            /* we are writing one char length times */
            memset( g_rep_output_data, *p->mod_data, p->length );
            p->mod_data = g_rep_output_data;
            p->data_len = p->length;
        }
        else if ( p->mod_type == MOD_STR && p->length < p->data_len )
        {
            fprintf( stderr, "failure: data length <%d> exceeds length <%d>\n",
                     p->data_len, p->length );
            return -1;
        }
    }

    return 0;
}


/*------------------------------------------------------------
 *  return appropriate MOD_ value, if one exists
 *------------------------------------------------------------
*/
int check_mod_type( char * name )
{
    /* character mods */
    if ( ! strncmp(name, "str", 3) )
        return MOD_STR; /* this is default */
    if ( ! strncmp(name, "val",  3) || ! strncmp(name, "char", 4) )
        return MOD_CHAR;

    /* integral mods */
    if ( ! strncmp(name, "uint1", 5) )
        return MOD_U1;
    if ( ! strncmp(name, "sint1", 5) )
        return MOD_S1;
    if ( ! strncmp(name, "uint2", 5) )
        return MOD_U2;
    if ( ! strncmp(name, "sint2", 5) )
        return MOD_S2;
    if ( ! strncmp(name, "uint4", 5) )
        return MOD_U4;
    if ( ! strncmp(name, "sint4", 5) )
        return MOD_S4;

    /* real mods */
    if ( ! strncmp(name, "float4", 6) )
        return MOD_F4;
    if ( ! strncmp(name, "float8", 6) )
        return MOD_F8;

    return MOD_INVALID;
}


/*------------------------------------------------------------
 *  Display usage information, depending on usage level:
 *  
 *  - USE_SHORT   : most basic usage info
 *  - USE_VERSION : display the current version info
 *  - USE_GE      : describe GE struct elements
 *  - USE_LONG    : complete help on program and options
 *------------------------------------------------------------
*/
int usage( char * prog, int level )
{
    if ( level == USE_SHORT )
    {
        printf( "usage: %s -help\n"
                "usage: %s [options] file1 file2 ...\n",
                prog, prog);
        return 0;
    }
    else if ( level == USE_VERSION )
    {
        printf( "%s, version %s, compiled: %s\n",
                prog, VERSION, __DATE__ );
        return 0;
    }
    else if ( level == USE_HISTORY )
    {
        fputs( g_history, stdout );
        return 0;
    }
    else if ( level == USE_GE )
    {
        help_ge_structs( prog );
        return 0;
    }
    else if ( level != USE_LONG )
    {
        fprintf( stderr, "failure: bad usage level <%d>\n", level );
        return -1;
    }

    /* so we are in a USE_LONG case */
    help_full( prog );

    return 0;
}

/*------------------------------------------------------------
 *  Describe elements of each GE struct.
 *------------------------------------------------------------
*/
int
help_ge_structs( char * prog )
{
    printf( "------------------------------------------------------------\n"
            "These are descriptions of the elements in the GE\n"
            "data structures used by '%s'.  Most elements\n"
            "correspond to a field in the image file header.\n"
            "\n"
            "These fields are shown when running '%s'\n"
            "with any of the options:\n"
            "   '-ge_header', '-ge_extras', or '-ge_all'.\n"
            /* taken from Ifile.c */
            "----------------------------------------\n"
            "ge_header_info struct:\n"
            "\n"
            "    good     : is this a valid GE image file\n"
            "    nx,ny    : dimensions of image in voxels\n"
            "    uv17     : run number (user variable 17)\n"
            "    dx,dy,dz : directional deltas - distances between voxels\n"
            "    zoff     : location of image in z direction\n"
            "    tr,te    : TR and TE timings\n"
            "    orients  : orientation string for image\n"
            "----------------------------------------\n"
            /* taken from mri_read.c */
            "ge_extras struct:\n"
            "\n"
            "    bpp      : bytes per pixel (file_size = nx * ny * bpp)\n"
            "    cflag    : compression flag (here, 1 means NOT compressed)\n"
            "    hdroff   : offset of image header (from beginning of file)\n"
            "    skip     : offset of image data   (from beginning of file)\n"
            "    swap     : is byte swapping performed?\n"
            "    xyzX     : coordinate box containing image\n"
            "------------------------------------------------------------\n"
            "\n", prog, prog
          );

    return 0;
}

/*------------------------------------------------------------
 *  Show detailed help.
 *------------------------------------------------------------
*/
int
help_full( char * prog )
{
    printf(
        "\n"
        "%s - display or modify sections of a file\n"
        "\n"
        "    This program can be used to display or edit data in arbitrary\n"
        "    files.  If no '-mod_data' option is provided (with DATA), it\n"
        "    is assumed the user wishes only to display the specified data\n"
        "    (using both '-offset' and '-length', or using '-ge_XXX').\n"
        "\n"
        "  usage: %s [options] -infiles file1 file2 ...\n"
        "\n"
        "  examples:\n"
        "\n"
        "   ----- help examples -----\n"
        "\n"
        "   1. get detailed help:\n"
        "\n"
        "      %s -help\n"
        "\n"
        "   2. get descriptions of GE struct elements:\n"
        "\n"
        "      %s -help_ge\n"
        "\n"
        "   ----- GEMS 4.x and 5.x display examples -----\n"
        "\n"
        "   3. display GE header and extras info for file I.100:\n"
        "\n"
        "      %s -ge_all -infiles I.100\n"
        "\n"
        "   4. display GEMS 4.x series and image headers for file I.100:\n"
        "\n"
        "      %s -ge4_all -infiles I.100\n"
        "\n"
        "   5. display run numbers for every 100th I-file in this directory\n"
        "\n"
        "      %s -ge_uv17 -infiles I.?42\n"
        "      %s -ge_run  -infiles I.?42\n"
        "\n"
        "   ----- general value display examples -----\n"
        "\n"
        "   6. display the 32 characters located 100 bytes into each file:\n"
        "\n"
        "      %s -offset 100 -length 32 -infiles file1 file2\n"
        "\n"
        "   7. display the 8 4-byte reals located 100 bytes into each file:\n"
        "\n"
        "      %s -disp_real4 -offset 100 -length 32 -infiles file1 file2\n"
        "\n"
        "   ----- character modification examples -----\n"
        "\n"
        "   8. in each file, change the 8 characters at 2515 to 'hi there':\n"
        "\n"
        "      %s -mod_data \"hi there\" -offset 2515 -length 8 -infiles I.*\n"
        "\n"
        "   9. in each file, change the 21 characters at 2515 to all 'x's\n"
        "      (and print out extra debug info)\n"
        "\n"
        "      %s -debug 1 -mod_data x -mod_type val -offset 2515 \\\n"
        "                -length 21 -infiles I.*\n"
        "\n"
        "   ----- raw number modification examples -----\n"
        "\n"
        "  10. in each file, change the 3 short integers starting at position\n"
        "      2508 to '2 -419 17'\n"
        "\n"
        "      %s -mod_data '2 -419 17' -mod_type sint2 -offset 2508 \\\n"
        "                -length 6 -infiles I.*\n"
        "\n"
        "  11. in each file, change the 3 binary floats starting at position\n"
        "      2508 to '-83.4 2 17' (and set the next 8 bytes to zero by\n"
        "      setting the length to 20, instead of just 12).\n"
        "\n"
        "      %s -mod_data '-83.4 2 17' -mod_type float4 -offset 2508 \\\n"
        "                -length 20 -infiles I.*\n"
        "\n"
        "  12. in each file, change the 3 binary floats starting at position\n"
        "      2508 to '-83.4 2 17', and apply byte swapping\n"
        "\n"
        "      %s -mod_data '-83.4 2 17' -mod_type float4 -offset 2508 \\\n"
        "                -length 12 -swap_bytes -infiles I.*\n"
        "\n"
        "  notes:\n"
        "\n"
        "    o  Use of '-infiles' is required.\n"
        "    o  Use of '-length' or a GE information option is required.\n"
        "    o  As of this version, only modification with text is supported.\n"
        "       Editing binary data is coming soon to a workstation near you.\n"
        "\n"
        "  special options:\n"
        "\n"
        "    -help              : show this help information\n"
        "                       : e.g. -help\n"
        "\n"
        "    -version           : show version information\n"
        "                       : e.g. -version\n"
        "\n"
        "    -hist              : show the program's modification history\n"
        "\n"
        "    -debug LEVEL       : print extra info along the way\n"
        "                       : e.g. -debug 1\n"
        "                       : default is 0, max is 2\n"
        "\n"
        "  required 'options':\n"
        "\n"
        "    -infiles f1 f2 ... : specify input files to print from or modify\n"
        "                       : e.g. -infiles file1\n"
        "                       : e.g. -infiles I.*\n"
        "\n"
        "          Note that '-infiles' should be the final option.  This is\n"
        "          to allow the user an arbitrary number of input files.\n"
        "\n"
        "  GE info options:\n"
        "\n"
        "      -ge_all          : display GE header and extras info\n"
        "      -ge_header       : display GE header info\n"
        "      -ge_extras       : display extra GE image info\n"
        "      -ge_uv17         : display the value of uv17 (the run #)\n"
        "      -ge_run          : (same as -ge_uv17)\n"
        "      -ge_off          : display file offsets for various fields\n"
        "\n"
        "  GEMS 4.x info options:\n"
        "\n"
        "      -ge4_all         : display GEMS 4.x series and image headers\n"
        "      -ge4_image       : display GEMS 4.x image header\n"
        "      -ge4_series      : display GEMS 4.x series header\n"
        "      -ge4_study       : display GEMS 4.x study header\n"
        "\n"
        "  raw ascii options:\n"
        "\n"
        "    -length LENGTH     : specify the number of bytes to print/modify\n"
        "                       : e.g. -length 17\n"
        "\n"
        "          This includes numbers after the conversion to binary.  So\n"
        "          if -mod_data is '2 -63 186', and -mod_type is 'sint2' (or\n"
        "          signed shorts), then 6 bytes will be written (2 bytes for\n"
        "          each of 3 short integers).\n"
        "\n"
        "       ** Note that if the -length argument is MORE than what is\n"
        "          needed to write the numbers out, the remaind of the length\n"
        "          bytes will be written with zeros.  If '17' is given for\n"
        "          the length, and 3 short integers are given as data, there \n"
        "          will be 11 bytes of 0 written after the 6 bytes of data.\n"
        "\n"
        "    -mod_data DATA     : specify a string to change the data to\n"
        "                       : e.g. -mod_data hello\n"
        "                       : e.g. -mod_data '2 -17.4 649'\n"
        "                       : e.g. -mod_data \"change to this string\"\n"
        "\n"
        "          This is the data that will be writting into the modified\n"
        "          file.  If the -mod_type is 'str' or 'char', then the\n"
        "          output data will be those characters.  If the -mod_type\n"
        "          is any other (i.e. a binary numerical format), then the\n"
        "          output will be the -mod_data, converted from numerical\n"
        "          text to binary.\n"
        "\n"
        "       ** Note that a list of numbers must be contained in quotes,\n"
        "          so that it will be processed as a single parameter.\n"
        "\n"
        "    -mod_type TYPE     : specify the data type to write to the file\n"
        "                       : e.g. -mod_type string\n"
        "                       : e.g. -mod_type sint2\n"
        "                       : e.g. -mod_type float4\n"
        "                       : default is 'str'\n"
        "\n"
        "        TYPE can be one of:\n"
        "\n"
        "          str       : perform a string substitution\n"
        "          char, val : perform a (repeated?) character substitution\n"
        "          uint1     : single byte unsigned int   (binary write)\n"
        "          sint1     : single byte   signed int   (binary write)\n"
        "          uint2     : two    byte unsigned int   (binary write)\n"
        "          sint2     : two    byte   signed int   (binary write)\n"
        "          uint4     : four   byte unsigned int   (binary write)\n"
        "          sint4     : four   byte   signed int   (binary write)\n"
        "          float4    : four   byte floating point (binary write)\n"
        "          float8    : eight  byte floating point (binary write)\n"
        "\n"
        "          If 'str' is used, which is the default action, the data is\n"
        "          replaced by the contents of the string DATA (from the\n"
        "          '-mod_data' option).\n"
        "\n"
        "          If 'char' is used, then LENGTH bytes are replaced by the\n"
        "          first character of DATA, repeated LENGTH times.\n"
        "\n"
        "          For any of the others, the list of numbers found in the\n"
        "          -mod_data option will be written in the supplied binary\n"
        "          format.  LENGTH must be large enough to accomodate this\n"
        "          list.  And if LENGTH is higher, the output will be padded\n"
        "          with zeros, to fill to the requesed length.\n"
        "\n"
        "    -offset OFFSET     : use this offset into each file\n"
        "                       : e.g. -offset 100\n"
        "                       : default is 0\n"
        "\n"
        "          This is the offset into each file for the data to be\n"
        "          read or modified.\n"
        "\n"
        "    -quiet             : do not output header information\n"
        "\n"
        "  numeric options:\n"
        "\n"
        "    -disp_int2         : display 2-byte integers\n"
        "                       : e.g. -disp_int2\n"
        "\n"
        "    -disp_int4         : display 4-byte integers\n"
        "                       : e.g. -disp_int4\n"
        "\n"
        "    -disp_real4        : display 4-byte real numbers\n"
        "                       : e.g. -disp_real4\n"
        "\n"
        "    -swap_bytes        : use byte-swapping on numbers\n"
        "                       : e.g. -swap_bytes\n"
        "\n"
        "          If this option is used, then byte swapping is done on any\n"
        "          multi-byte numbers read from or written to the file.\n"
        "\n"
        "  - R Reynolds, version: %s, compiled: %s\n"
        "\n",
        prog, prog,
        prog, prog, prog, prog, prog, prog, prog, prog, prog,
        prog, prog, prog, prog,
        VERSION, __DATE__
        );

    return 0;
}

/*------------------------------------------------------------
 * Reverse the order of the 4 bytes at this address.
 *------------------------------------------------------------
*/
static int
swap_4( void * ptr )            /* destructive */
{
   unsigned char * addr = ptr;

   addr[0] ^= addr[3]; addr[3] ^= addr[0]; addr[0] ^= addr[3];
   addr[1] ^= addr[2]; addr[2] ^= addr[1]; addr[1] ^= addr[2];

   return 0;
}

/*------------------------------------------------------------
 * Reverse the order of the 2 bytes at this address.
 *------------------------------------------------------------
*/
static int
swap_2( void * ptr )            /* destructive */
{
   unsigned char * addr = ptr;

   addr[0] ^= addr[1]; addr[1] ^= addr[0]; addr[0] ^= addr[1];

   return 0;
}

/******************************************************************/
/*** Return info from a GEMS IMGF file into user-supplied struct **/

/* stolen from Ifile.c and modified ... */

int
read_ge_header( char *pathname , ge_header_info *hi, ge_extras *E, ge_off *off )
{
   FILE *imfile ;
   int  length , skip , swap=0 ;
   char orients[8] , str[8] ;
   int nx , ny , bpp , cflag , hdroff ;
        float uv17 = -1.0;
        
   if( hi == NULL ) return -1;            /* bad */
   hi->good = 0 ;                       /* not good yet */
   if( pathname    == NULL ||
       pathname[0] == '\0'   ) return -1; /* bad */

   length = THD_filesize( pathname ) ;
   if( length < 1024 ) return -1;         /* bad */

   imfile = fopen( pathname , "r" ) ;
   if( imfile == NULL ) return -1;        /* bad */

   strcpy(str,"JUNK") ;     /* initialize string */
   fread(str,1,4,imfile) ;  /* check for "IMGF" at start of file */

   if( str[0]!='I' || str[1]!='M' || str[2]!='G' || str[3]!='F' ){ /* bad */
      fclose(imfile) ; return -2;
   }

   /*-- read next 5 ints (after the "IMGF" string) --*/

   fread( &skip , 4,1, imfile ) ; /* offset into file of image data */
   fread( &nx   , 4,1, imfile ) ; /* x-size */
   fread( &ny   , 4,1, imfile ) ; /* y-size */
   fread( &bpp  , 4,1, imfile ) ; /* bits per pixel (should be 16) */
   fread( &cflag, 4,1, imfile ) ; /* compression flag (1=uncompressed)*/

        /*-- check if nx is funny --*/

   if( nx < 0 || nx > 8192 ){      /* have to byte swap these 5 ints */
     swap = 1 ;                    /* flag to swap data, too */
     swap_4(&skip); swap_4(&nx); swap_4(&ny); swap_4(&bpp); swap_4(&cflag);
   } else {
     swap = 0 ;  /* data is ordered for this CPU */
   }
   if( nx < 0 || nx > 8192 || ny < 0 || ny > 8192 ){  /* bad */
      fclose(imfile) ; return -1;
   }

   hi->nx = nx ;
   hi->ny = ny ;

   off->nx =  8;
   off->ny = 12;

   if( skip+2*nx*ny >  length ||               /* file is too short */
       skip         <= 0      ||               /* bizarre  */
       cflag        != 1      ||               /* data is compressed */
       bpp          != 16        ){
      fclose(imfile); return -1;    /* data is not shorts */
   }

   /*-- try to read image header data as well --*/

   fseek( imfile , 148L , SEEK_SET ) ; /* magic GEMS offset */
   fread( &hdroff , 4,1 , imfile ) ;   /* location of image header */
   if( swap ) swap_4(&hdroff) ;

   if( hdroff > 0 && hdroff+256 < length ){   /* can read from image header */
       float dx,dy,dz, xyz[9], zz ; int itr, ii,jj,kk ;

       /*-- get voxel grid sizes --*/

       fseek( imfile , hdroff+26 , SEEK_SET ) ;    /* dz */
       fread( &dz , 4,1 , imfile ) ;

       fseek( imfile , hdroff+50 , SEEK_SET ) ;    /* dx and dy */
       fread( &dx , 4,1 , imfile ) ;
       fread( &dy , 4,1 , imfile ) ;

       off->dx = hdroff+50;
       off->dy = hdroff+54;
       off->dz = hdroff+26;

       if( swap ){ swap_4(&dx); swap_4(&dy); swap_4(&dz); }

       hi->dx = dx ; hi->dy = dy ; hi->dz = dz ;

       /* grid orientation: from 3 sets of LPI corner coordinates: */
       /*   xyz[0..2] = top left hand corner of image     (TLHC)   */
       /*   xyz[3..5] = top right hand corner of image    (TRHC)   */
       /*   xyz[6..8] = bottom right hand corner of image (BRHC)   */
       /* GEMS coordinate orientation here is LPI                  */

       off->xyz = hdroff+154;
       fseek( imfile , hdroff+154 , SEEK_SET ) ;  /* another magic number */
       fread( xyz , 4,9 , imfile ) ;
       if( swap ){
          swap_4(xyz+0); swap_4(xyz+1); swap_4(xyz+2);
          swap_4(xyz+3); swap_4(xyz+4); swap_4(xyz+5);
          swap_4(xyz+6); swap_4(xyz+7); swap_4(xyz+8);
       }

       /* x-axis orientation */
       /* ii determines which spatial direction is x-axis  */
       /* and is the direction that has the biggest change */
       /* between the TLHC and TRHC                        */

       dx = fabs(xyz[3]-xyz[0]) ; ii = 1 ;
       dy = fabs(xyz[4]-xyz[1]) ; if( dy > dx ){ ii=2; dx=dy; }
       dz = fabs(xyz[5]-xyz[2]) ; if( dz > dx ){ ii=3;        }
       dx = xyz[ii+2]-xyz[ii-1] ; if( dx < 0. ){ ii = -ii;    }
       switch( ii ){
        case  1: orients[0]= 'L'; orients[1]= 'R'; break;
        case -1: orients[0]= 'R'; orients[1]= 'L'; break;
        case  2: orients[0]= 'P'; orients[1]= 'A'; break;
        case -2: orients[0]= 'A'; orients[1]= 'P'; break;
        case  3: orients[0]= 'I'; orients[1]= 'S'; break;
        case -3: orients[0]= 'S'; orients[1]= 'I'; break;
        default: orients[0]='\0'; orients[1]='\0'; break;
       }

       /* y-axis orientation */
       /* jj determines which spatial direction is y-axis  */
       /* and is the direction that has the biggest change */
       /* between the BRHC and TRHC                        */

       dx = fabs(xyz[6]-xyz[3]) ; jj = 1 ;
       dy = fabs(xyz[7]-xyz[4]) ; if( dy > dx ){ jj=2; dx=dy; }
       dz = fabs(xyz[8]-xyz[5]) ; if( dz > dx ){ jj=3;        }
       dx = xyz[jj+5]-xyz[jj+2] ; if( dx < 0. ){ jj = -jj;    }
       switch( jj ){
         case  1: orients[2] = 'L'; orients[3] = 'R'; break;
         case -1: orients[2] = 'R'; orients[3] = 'L'; break;
         case  2: orients[2] = 'P'; orients[3] = 'A'; break;
         case -2: orients[2] = 'A'; orients[3] = 'P'; break;
         case  3: orients[2] = 'I'; orients[3] = 'S'; break;
         case -3: orients[2] = 'S'; orients[3] = 'I'; break;
         default: orients[2] ='\0'; orients[3] ='\0'; break;
       }

       orients[4] = '\0' ;   /* terminate orientation string */

       kk = 6 - abs(ii)-abs(jj) ;   /* which spatial direction is z-axis   */
                                    /* where 1=LR, 2=PA, 3=IS               */
                                    /* (can't tell orientation from 1 slice) */

       zz = xyz[kk-1] ;             /* z-coordinate of this slice */

       hi->zoff = zz ;
       strcpy(hi->orients,orients) ;

       /*-- get TR in seconds --*/

       off->tr = hdroff+194;
       fseek( imfile , hdroff+194 , SEEK_SET ) ;
       fread( &itr , 4,1 , imfile ) ; /* note itr is an int */
       if( swap ) swap_4(&itr) ;
       hi->tr = 1.0e-6 * itr ;        /* itr is in microsec */

       /*-- get TE in milliseconds --*/

       off->te = hdroff+202;
       fseek( imfile , hdroff+202 , SEEK_SET ) ;
       fread( &itr , 4,1 , imfile ) ; /* itr is an int, in microsec */
       if( swap ) swap_4(&itr) ;
       hi->te = 1.0e-6 * itr ;

       /* zmodify: get User Variable 17, a likely indicator of a new scan,
        * info by S. Marrett, location from S. Inati's matlab function
        * GE_readHeaderImage.m
        */

        off->uv17 = hdroff+272+202;
        /* printf ("\nuv17 = \n"); */
        fseek ( imfile , hdroff+272+202, SEEK_SET ) ;
        fread( &uv17 , 4, 1 , imfile ) ;
        if( swap ) swap_4(&uv17) ;
        /* printf ("%d ", (int)uv17);  */
        hi->uv17 = (int)uv17; 
        /* printf ("\n"); */
        
        hi->good = 1 ;                  /* this is a good file */

        E->bpp    = bpp;                /* store the ge_extra info */
        E->cflag  = cflag;
        E->hdroff = hdroff;
        E->skip   = skip;
        E->swap   = swap;

        memcpy( E->xyz, xyz, sizeof(xyz) );
    } /* end of actually reading image header */

    fclose(imfile);
    return 0;
}

/*------------------------------------------------------------
 *  Return the size of the file.
 *------------------------------------------------------------
*/
unsigned long
THD_filesize ( char * pathname )
{
    struct stat buf;

    if ( pathname == NULL || *pathname == '\0' )
        return -1;

    if ( stat( pathname, &buf ) != 0 )
        return -1;

    return (unsigned long)buf.st_size;
}

/*------------------------------------------------------------
 *  Just output raw numbers from the starting location,
 *  swapping bytes (if requested).
 *------------------------------------------------------------
*/
int
disp_numeric_data( char * data, param_t * p, FILE * fp )
{
    int c;

    if ( data == NULL || fp == NULL )
    {
        fprintf( stderr, "** error: bad params to DND '%p,%p'\n", data, fp );
        return -1;
    }

    if ( p->length <= 0 || p->ndisp == 0 )
        return 0;

    /* print out shorts */
    if ( p->ndisp & NDISP_INT2 )
    {
        short * sp = (short *)data;

        fprintf( fp, "0x%4x : ", (unsigned int)p->offset );
        for ( c = 0; c < p->length/2; c++, sp++ )
        {
            if ( p->swap )
                swap_2( sp );
            fprintf( fp, "%d ", *sp );
        }
        fputc( '\n', fp );
    }

    /* print out ints */
    if ( p->ndisp & NDISP_INT4 )
    {
        int * ip = (int *)data;

        fprintf( fp, "0x%4x : ", (unsigned int)p->offset );
        for ( c = 0; c < p->length/4; c++, ip++ )
        {
            if ( p->swap )
                swap_4( ip );
            fprintf( fp, "%d ", *ip );
        }
        fputc( '\n', fp );
    }

    /* print out floats */
    if ( p->ndisp & NDISP_REAL4 )
    {
        float * rp = (float *)data;

        fprintf( fp, "0x%4x : ", (unsigned int)p->offset );
        for ( c = 0; c < p->length/4; c++, rp++ )
        {
            if ( p->swap )
                swap_4( rp );
            fprintf( fp, "%f ", *rp );
        }
        fputc( '\n', fp );
    }

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the param_t struct.
 *------------------------------------------------------------
*/
int
disp_param_data( param_t * p )
{
    if ( ! p )
        return -1;

    printf( "num_files, flist         : %d, %p\n"
            "debug, data_len          : %d, %d\n"
            "ge_disp, ge4_disp, ndisp : 0x%x, 0x%x, 0x%x\n"
            "\n"
            "swap, modify, mod_type   : %d, %d, %d\n"
            "offset, length, quiet    : %ld, %d, %d\n"
            "mod_data                 : %s\n"
            "\n",
            p->num_files, p->flist, p->debug, p->data_len,
            p->ge_disp, p->ge4_disp, p->ndisp, p->swap, p->modify,
            p->mod_type, p->offset, p->length, p->quiet,
            CHECK_NULL_STR(p->mod_data)
          );

    if ( p->debug > 1 )
    {
        int c;

        printf( "file list: " );
        for ( c = 0; c < p->num_files; c ++ )
            printf( "'%s' ", p->flist[c] );
        printf( "\n" );
    }

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the ge_extras struct.
 *------------------------------------------------------------
*/
int
r_idisp_ge_extras( char * info, ge_extras * E )
{
    if ( info )
        fputs( info, stdout );

    if ( E == NULL )
    {
        printf( "r_idisp_ge_extras: E == NULL" );
        return -1;
    }

    printf( " ge_extras at %p :\n"
            "    bpp              = %d\n"
            "    cflag            = %d\n"
            "    hdroff           = %d\n"
            "    skip             = %d\n"
            "    swap             = %d\n"
            "    (xyz0,xyz1,xyz2) = (%f,%f,%f)\n"
            "    (xyz3,xyz4,xyz5) = (%f,%f,%f)\n"
            "    (xyz6,xyz7,xyz8) = (%f,%f,%f)\n",
            E, E->bpp, E->cflag, E->hdroff, E->skip, E->swap,
            E->xyz[0], E->xyz[1], E->xyz[2],
            E->xyz[3], E->xyz[4], E->xyz[5],
            E->xyz[6], E->xyz[7], E->xyz[8]
          );
    return 0;
}

/*------------------------------------------------------------
 *  Display the contents of the ge_header_info struct.
 *------------------------------------------------------------
*/
int
r_idisp_ge_header_info( char * info, ge_header_info * I )
{
    if ( info )
        fputs( info, stdout );

    if ( I == NULL )
    {
        printf( "r_idisp_ge_header_info: I == NULL" );
        return -1;
    }

    printf( " ge_header_info at %p :\n"
            "    good        = %d\n"
            "    (nx,ny)     = (%d,%d)\n"
            "    uv17        = %d\n"
            "    (dx,dy,dz)  = (%f,%f,%f)\n"
            "    zoff        = %f\n"
            "    (tr,te)     = (%f,%f)\n"
            "    orients     = %8s\n",
            I, I->good, I->nx, I->ny, I->uv17,
            I->dx, I->dy, I->dz, I->zoff, I->tr, I->te,
            CHECK_NULL_STR(I->orients)
          );

    return 0;
}


/*------------------------------------------------------------
 *  Display the contents of the ge_header_info struct.
 *------------------------------------------------------------
*/
int
disp_ge_offsets( char * info, ge_off * D )
{
    if ( info )
        fputs( info, stdout );

    if ( D == NULL )
    {
        printf( "disp_ge_offsets: D == NULL" );
        return -1;
    }

    printf( " ge_off at %p :\n"
            "    nx, ny, uv17 = %d, %d, %d\n"
            "    dx, dy, dz   = %d, %d, %d\n"
            "    tr, te, xyz  = %d, %d, %d\n",
            D, D->nx, D->ny, D->uv17,
            D->dx, D->dy, D->dz,
            D->tr, D->te, D->xyz
          );

    return 0;
}

