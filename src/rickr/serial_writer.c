#define VERSION "1.0 (October 16, 2006)"

/*----------------------------------------------------------------------
 * serial_writer.c    - pass some data to a serial port
 *
 * - Rick Reynolds
 *
 * This program was written for Tyler Jones and Rasmus Birn.
 *----------------------------------------------------------------------
 */

static char g_history[] =
 "----------------------------------------------------------------------\n"
 " history:\n"
 "\n"
 " 0.1  October 12, 2006  [rickr] - initial program\n"
 " 1.0  October 16, 2006  [rickr] - added -ms_sleep, -nblocks and -swap\n"
 "----------------------------------------------------------------------\n";


#include <stdio.h>   /* Standard input/output definitions */
#include <string.h>  /* String function definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */

#include <stdlib.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

typedef struct{
    char  * sport_fname;    /* serial port file name */
    char  * in_fname;       /* input filename (stdin if NULL) */
    int     make_data;      /* create data */
    int     block_len;      /* length of writing block */
    int     nblocks;        /* number of blocks to write (0 for inf) */
    int     swap;           /* swap 'n' bytes (0 to do no swap) */
    int     ms_sleep;       /* sleep this number of ms betweeen blocks */
    int     debug;
} opts_t;

typedef struct{
    int     spfd;           /* serial port file descriptor */
    int     infd;           /* 0 if stdin */
} control_t;

opts_t    gopts  = { NULL, NULL, 0, 12, 0, 0, 0, 0 };
control_t gcontr = { -1, 0 };  /* uninit, stdin */

#define SW_USE_SHORT 1
#define SW_USE_LONG  2
#define SW_USE_HIST  3
#define SW_USE_VER   4
#define CHECK_NULL_STR(str) ( str ? str : "(NULL)" )

/* protos */
void cleanup    (int sig_num);
int  close_ports( void );
int  disp_hex_bytes( char * mesg, char * data, int len );
int  disp_opts_t( char * info, opts_t * O );
int  get_opts   (opts_t *opt, int argc, char *argv[]);
int  ms_sleep( int ms );
int  open_serial(opts_t *opt, control_t * contr);
int  send_serial(opts_t * opt, control_t * contr, char * data, int len);
int  set_data   ( opts_t * opts, control_t * contr, char * data );
int  swap_2( char * data, int nshort );
int  swap_4( char * data, int nint );
int  usage      ( char * prog, int level );

int main(int argc, char *argv[])
{
    int  rv, len, nblocks, done = 0;
    char * data = NULL;

    if ( (rv = get_opts(&gopts, argc, argv)) != 0 )
        return rv;

    /* register interrupt trap */
    signal(SIGTERM, cleanup);  /* so need global variables */
    signal(SIGINT, cleanup);

    /* open or set input file */
    if( gopts.in_fname )
    {
        gcontr.infd = open(gopts.in_fname, O_RDONLY);
        if( gcontr.infd < 0 )
        {
            fprintf(stderr,"** failed to open infile '%s'\n",gopts.in_fname);
            return 1;
        }
    }
    else
        gcontr.infd = 0;  /* stdin */

    /* allocate data block */
    data = (char *)malloc(gopts.block_len * sizeof(char));
    if( !data )
    {
        fprintf(stderr,"** failed to alloc %d bytes for block\n",
                gopts.block_len);
        return 1;
    }

    if( gopts.swap == 2 ) swap_2(data, gopts.block_len/2);
    else if( gopts.swap == 4 ) swap_4(data, gopts.block_len/4);

    if( (rv = open_serial(&gopts, &gcontr)) != 0 )
        return rv;

    nblocks = 0;
    while(! done && (gopts.nblocks <= 0 || nblocks < gopts.nblocks) )
    {
        len = set_data(&gopts, &gcontr, data);
        if( len > 0 ) done = send_serial(&gopts, &gcontr, data, len);
        else          done = 1;
        nblocks++;
	if( gopts.ms_sleep ) ms_sleep(gopts.ms_sleep);
    }

    free(data);
    close_ports();

    if(gopts.debug) fprintf(stderr,"-d wrote %d blocks of data\n",nblocks);

    return 0;
}


/* ----------------------------------------------------------------------
 * set_data
 * ----------------------------------------------------------------------
 */
int set_data( opts_t * opts, control_t * contr, char * data )
{
    static short   counter = 0, size = sizeof(counter);
    short        * sp;
    int            c;

    if ( opts->make_data )
    {
        int ints = opts->block_len / size;
        if( opts->block_len % size )
        {
            fprintf(stderr,"** block_len should be multiple of %d\n",size);
            return -1;
        }

        if( opts->debug )
            fprintf(stderr,"-d setting %d ints from %8d to %8d\n",
                ints, counter, counter+ints-1);

        sp = (short *)data;
        for( c = 0; c < ints; c++ )
            *sp++ = counter++;

        return opts->block_len;
    }

    c = read(contr->infd, data, opts->block_len);
    if( c < 0 ) fprintf(stderr,"** read nothing from input stream\n");

    /* maybe the user would like to verify incoming data */
    if( opts->debug > 2 )
        disp_hex_bytes( "-d all received data: ", data, c );
    else if( opts->debug > 1 )
        disp_hex_bytes( "-d first <= 16 bytes: ", data, c < 16 ? c : 16 );

    return c;
}


/* ----------------------------------------------------------------------
 * close serial port and data socket
 * ----------------------------------------------------------------------
 */
int close_ports( void )
{
    if ( gcontr.spfd >= 0 )
        close(gcontr.spfd);

    if ( gcontr.infd >= 0 )
        close(gcontr.infd);


    return 0;
}


/* close any open ports (to possibly catch an interrupt) */
void cleanup(int sig_num)
{
    if ( gopts.debug > 0 )
    {
        fprintf(stderr, "-- received signal %d: closing ports\n", sig_num);
        if ( gopts.debug > 1 )
            fprintf(stderr,"   descriptors: ser = %d, file = %d\n",
                    gcontr.spfd, gcontr.infd);
    }

    close_ports();

    exit(0);
}

#define CHECK_ARG_COUNT(ac,str)         \
        do {                            \
            if ((ac+1) >= argc) {       \
                fputs(str,stderr);      \
                return -1;              \
            }                           \
        } while (0)                     \

int get_opts(opts_t *opt, int argc, char *argv[])
{
    char * prog = argv[0];
    int    ac;

    if ( argc < 2 )
        return usage(prog, SW_USE_SHORT);

    for ( ac = 1; ac < argc; ac++ )   /* help, hist first, rest alphabetical */
    {
        if ( !strncmp(argv[ac], "-help", 5) )
            return usage(prog, SW_USE_LONG);
        else if ( !strncmp(argv[ac], "-hist", 5) )
            return usage(prog, SW_USE_HIST);
        else if ( !strncmp(argv[ac], "-ver", 4) )
            return usage(prog, SW_USE_VER);
        /* execution options */
        else if ( !strncmp(argv[ac], "-block_len", 6) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -block_len LENGTH\n");
            opt->block_len = atoi(argv[++ac]);
            if( opt->block_len <= 0 )
            {
                fprintf(stderr,"** -block_len LENGTH needs to be positive\n");
                return -1;
            }
        }
        else if ( !strncmp(argv[ac], "-debug", 6) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -debug DEBUG_LEVEL\n");
            opt->debug = atoi(argv[++ac]);
        }
        else if ( !strncmp(argv[ac], "-infile", 7) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -infile INPUT_FILENAME\n");
            opt->in_fname = argv[++ac];
        }
        else if ( !strncmp(argv[ac], "-make_data", 7) )
        {
            opt->make_data = 1;
        }
        else if ( !strncmp(argv[ac], "-ms_sleep", 9) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -ms_sleep MS_TO_SLEEP\n");
            opt->ms_sleep = atoi(argv[++ac]);
            if( opt->ms_sleep <= 0 )
            {
                fprintf(stderr,"** -ms_sleep MS_TO_SLEEP must be positive\n");
                return -1;
            }
        }
        else if ( !strncmp(argv[ac], "-nblocks", 7) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -nblocks NUM_BLOCKS\n");
            opt->nblocks = atoi(argv[++ac]);
            if( opt->nblocks <= 0 )
            {
                fprintf(stderr,"** -nblocks NUM_BLOCKS needs to be positive\n");
                return -1;
            }
        }
        else if ( !strncmp(argv[ac], "-serial_port", 7) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -serial_port SERIAL_FILENAME\n");
            opt->sport_fname = argv[++ac];
        }
        else if ( !strncmp(argv[ac], "-swap", 5) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -swap NUM_BYTES\n");
            opt->swap = atoi(argv[++ac]);
        }
        else
        {
            fprintf(stderr,"** invalid option '%s', exiting...\n", argv[ac]);
            return -1;
        }
    }

    if ( opt->debug > 1 )
        disp_opts_t( "options read: ", opt );

    if ( opt->swap != 0 && opt->swap != 2 && opt->swap != 4 )
    {
        fprintf(stderr,"** invalid -swap '%d', must be one of 0, 2 or 4\n",
                opt->swap);
        return 1;
    }

    return 0;
}


/* ----------------------------------------------------------------------
 * display usage
 *
 * SW_USE_SHORT   : most basic usage info
 * SW_USE_LONG    : display complete description of program and options
 * SW_USE_VER     : display version and compile date
 * ----------------------------------------------------------------------
 */
int usage( char * prog, int level )
{
    if ( level == SW_USE_SHORT )
        printf( "usage: %s -help\n"
                "usage: %s [options] -serial_port FILENAME\n", prog, prog );
    else if ( level == SW_USE_HIST )
        fputs(g_history, stdout);
    else if ( level == SW_USE_VER )
        printf("%s version %s, compiled %s\n", prog, VERSION, __DATE__);
    else if ( level == SW_USE_LONG )
    {
        printf(
            "------------------------------------------------------------\n"
            "%s - send data to a serial port\n"
            "\n"
            "    Write data from a file, piped to stdin, or made up to a\n"
            "    user specified serial port (given by filename).\n"
            "\n"
            "    The program terminates when input reaches end of file, or\n"
            "    when the user terminates with ctrl-c.\n"
            "\n"
            "  usage: %s [options] -serial_port FILENAME\n"
            "------------------------------------------------------------\n"
            "  examples:\n"
            "\n"
            "        %s -serial_port /dev/ttyS0\n"
            "\n"
            "        MAKE_SOME_DATA | %s -serial_port /dev/ttyS0\n"
            "\n"
            "        %s -serial_port /dev/ttyS0 -infile my_data\n"
            "        %s -serial_port /dev/ttyS0 -infile my_data -debug 2\n"
            "        %s -serial_port /dev/ttyS0 -make_data\n"
            "\n"
            "------------------------------------------------------------\n"
            "  I/O options:\n"
            "\n"
            "    -block_len LEN        : specify the length of a block\n"
            "                          : -block_len 64\n"
            "\n"
            "    -infile FILENAME      : specify name of file for input\n"
            "                          : -infile my_data\n"
            "\n"
            "        The FILENAME contains data to be sent over the serial\n"
            "        port.  The program terminates at end-of-file.\n"
            "\n"
            "    -make_data            : create data\n"
            "\n"
            "        Create data, instead of getting it from a file.\n"
            "        Currently, this will output increasing 4 byte ints.\n"
            "\n"
            "    -ms_sleep MS_TO_SLEEP : sleep between blocks\n"
            "\n"
            "        Sleep for this number of miliseconds between each\n"
            "        block of data.\n"
            "\n"
            "    -swap NUM_BYTES       : specify the number of bytes to swap\n"
            "                          : -swap 2\n"
            "\n"
            "        Swap bytes before sending.  NUM_BYTES can be either\n"
            "        2 or 4 (or 0, for no swapping).\n"
            "\n"
            "\n"
            "    -nblocks NUM_BLOCKS   : specify the number of blocks\n"
            "                          : -nblocks 1024\n"
            "------------------------------------------------------------\n"
            "  required output port:\n"
            "\n"
            "    -serial_port FILENAME : specify output serial port\n"
            "                          : -serial_port /dev/ttyS0\n"
            "\n"
            "        The FILENAME is the device file for the serial port\n"
            "        which will be used for output.\n"
            "------------------------------\n"
            "  other options:\n"
            "\n"
            "    -debug LEVEL     : set the debugging level to LEVEL\n"
            "                     : e.g. -debug 2\n"
            "                     : default is 1, max is 3\n"
            "\n"
            "    -help            : show this help information\n"
            "\n"
            "    -hist            : show modification history\n"
            "\n"
            "    -ver             : display current version\n"
            "------------------------------------------------------------\n"
            "  Author: R. Reynolds, (Oct 12, 2006)\n"
            "------------------------------------------------------------\n",
            prog, prog,
            prog, prog, prog, prog, prog
            );
    }
    else
        fprintf(stderr,"** usage error: invalid level %d\n", level);

    return 1;
}


int send_serial(opts_t * opt, control_t * contr, char * data, int len)
{
    int i = write(contr->spfd, data, len);
    if(i < len)
    {
        fprintf(stderr, "warning: wrote %d of %d bytes to serial port\n",
                i, len);
        return 1;
    }

    if( opt->debug > 1)
        fprintf(stderr,"+d wrote %d bytes to port\n", len);

    return 0;
}


int open_serial(opts_t *opt, control_t * contr)
{
    struct termios topt;

    contr->spfd = open(opt->sport_fname, O_RDWR | O_NOCTTY | O_NDELAY);
    if (contr->spfd < 0)
    {
        perror("pe: Failed to open the serial port ");
        return -1;
    }

    fcntl(contr->spfd, F_SETFL, FNDELAY);  /* nonblocking reads */

    /* get the current options for the port */
    tcgetattr(contr->spfd, &topt);

    /* set the baud rates to 9600 (fim scanner is 115200) */
    cfsetispeed(&topt, B115200);
    cfsetospeed(&topt, B115200);

    /* enable the receiver and set local mode */
    topt.c_cflag |= (CLOCAL | CREAD );

    /* set 8 bit N parity */
    topt.c_cflag &= ~PARENB;
    /* topt.c_cflag &= ~CSTOPB; */
    topt.c_cflag |= CSTOPB;		/* fim */
    topt.c_cflag &= ~CSIZE;
    topt.c_cflag |= CS8;

    /* raw data input and output */
    topt.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);
    topt.c_oflag &= ~OPOST;

    /*Set the new options for the port*/
    if (tcsetattr(contr->spfd, TCSANOW, &topt) == -1) {
        perror("pe: Failed to set attributes on the serial port ");
        close(contr->spfd);
        return -1;
    }

    if( opt->debug )
        fprintf(stderr,"+d successfully opened serial port %s\n",
            opt->sport_fname);

    return 0;
}


/* ----------------------------------------------------------------------
 * display contents of optiondata struct
 * ----------------------------------------------------------------------
 */
int disp_opts_t( char * info, opts_t * O )
{
    if ( info )
        fputs(info, stderr);

    if ( ! O )
    {
        fprintf(stderr,"** disp_opts_t: O == NULL\n");
        return -1;
    }

    fprintf(stderr,
            " opts_t at %p :\n"
            "    sport_fname     = %s\n"
            "    in_fname        = %s\n"
            "    make_data       = %d\n"
            "    debug           = %d\n",
            O, CHECK_NULL_STR(O->sport_fname), CHECK_NULL_STR(O->in_fname),
            O->make_data, O->debug);

    return 0;
}

/* ----------------------------------------------------------------------
 * display hex bytes to stderr
 * ----------------------------------------------------------------------
 */
int disp_hex_bytes( char * mesg, char * data, int len )
{
    int c1, c2;

    if( !data ) return 1;

    if( mesg ) fputs(mesg, stderr);

    for( c1 = 0; c1 < len; /* inner loop */ )
    {
        if( (c1 % 16) == 0 ) fprintf(stderr, "\n    offset 0x%08x:", c1);
        if( (c1 % 4) == 0 ) fprintf(stderr, "  ");
        for( c2 = 0; c2 < 4 && c1 < len; c1++, c2++)
            fprintf(stderr, "%02x", data[c1]);
    }

    if( mesg || len > 0 ) fputc('\n', stderr);

    return 0;
}

int ms_sleep( int ms )
{
    struct timeval tv;
    if( ms <= 0 ) return 0;
    tv.tv_sec = ms / 1000;
    tv.tv_usec = (ms%1000)*1000 ;
    select( 1 , NULL,NULL,NULL , &tv ) ;
    return 0;
}

int swap_2( char * data, int nshort )
{
    int  count;
    char c, *cp = data;

    for( count = 0; count < nshort; count++)
    {
        c = *cp; cp[0] = cp[1]; cp[1] = c;
	cp += 2;
    }
    return 0;
}


int swap_4( char * data, int nint )
{
    int  count;
    char c, *cp = data;

    for( count = 0; count < nint; count++)
    {
        c = cp[0]; cp[0] = cp[3]; cp[3] = c;
        c = cp[1]; cp[1] = cp[2]; cp[2] = c;
	cp += 4;
    }
    return 0;
}

