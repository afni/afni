
/*----------------------------------------------------------------------
 * serial_helper.c    - pass data from plug_realtime to serial port
 *
 * This program is meant to run as a tcp server.  The intention is
 * to read motion parameter data from the realtime plugin to afni,
 * and to write this data to a serial port.
 *
 * The basic outline is:
 *
 *     open tcp socket
 *     for ever
 *         wait for socket connection (listen())...
 *         open serial port
 *         while data is coming from tcp socket
 *             write data to serial port
 *         close serial port and data socket
 *
 * This program was written for Tom Ross.
 *----------------------------------------------------------------------
 */

static char g_history[] =
 "----------------------------------------------------------------------\n"
 " history:\n"
 "\n"
 " 0.1  March 25, 2004  [tross]\n"
 "    - basic outline with serial functions\n"
 "\n"
 " 1.0  March 31, 2004  [rickr]\n"
 "    - initial full release\n"
 "\n"
 " 1.1  April 1, 2004  [rickr]\n"
 "    - added a little more to the -help section\n"
 "\n"
 " 1.2  April 1, 2004  [rickr]\n"
 "    - complain about bad options\n"
 "\n"
 " 1.3  April 2, 2004  [tross/rickr]\n"
 "    - set SH_DEF_MIN_FVAL to -12.7\n"
 "    - use -128 as the special value denoting start of serial data\n"
 "\n"
 " 1.4  April 7, 2004  [rickr]\n"
 "    - added 'sys/file.h' for solaris builds (thanks, Vince)\n"
 "\n"
 " 1.4a March 22, 2005  [rickr]\n"
 "    - removed all tabs\n"
 "\n"
 " 1.5  November 11, 2006 [rickr]\n"
 "    - added -num_extras option, for processing extra floats per TR\n"
 "\n"
 " 1.6  November 15, 2006 [rickr]\n"
 "    - encode nex in handshake byte written to serial port each TR\n"
 "\n"
 " 1.7  July 16, 2008 [rickr]\n"
 "    - added -disp_all for P Kundu\n"
 "\n"
 " 1.8  July 29, 2008 [rickr]\n"
 "    - captured more exit signals\n"
 "    - enhanced failure text\n"
 "    - flushed output buffer\n"
 "\n"
 " 1.9  July 30, 2008 [rickr]\n"
 "    - added handshake interface for HELLO version 1 (old is version 0)\n"
 "    - added -show_times option\n"
 "----------------------------------------------------------------------\n";

#define VERSION "1.9 (Jul 30, 2008)"

#include <stdio.h>   /* Standard input/output definitions */
#include <string.h>  /* String function definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */

#include <stdlib.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define SH_MAX_VALS            6
#define SH_DEF_MIN_FVAL    -12.7
#define SH_DEF_MAX_FVAL     12.7
#define SH_DEF_SOCKET      53214

#define SH_USE_HIST            1
#define SH_USE_VERSION         2
#define SH_USE_SHORT           3
#define SH_USE_LONG            4

#define CHECK_NULL_STR(str) ( str ? str : "(NULL)" )

typedef struct{
    char  * serial_port;
    int     no_serial;
    float   mp_min;
    float   mp_max;
    int     sock_num;
    int     num_extra;          /* number of extra data values per TR */
    int     disp_all;           /* flag to display all results */
    int     show_times;         /* flag to diplay data times */
    int     swap;
    int     debug;
} optiondata;

typedef struct {
    int     nread;              /* number of instances processed */
    int     nvals;
    int     nex;                /* opt->num_extra */
    float   data[SH_MAX_VALS];
    float * extras;             /* extra data values received per TR */
} motparm;

typedef struct
{
    int debug;                  /* for global access in clean_n_exit() */
    int sport;
    int tdata_sd;
    int tserver_sd;
} port_list;


int   alloc_extras         ( motparm * mp, int nex );
void  clean_n_exit         ( int sig_num );
int   close_data_ports     ( port_list * plist );
int   disp_optiondata      ( char * info, optiondata * D );
int   format_output        ( optiondata * opt, motparm * mp, char ** outstr,
                                                             int * oslen);
int   get_options          ( optiondata *opt, motparm * mp, port_list * plist,
                             int argc, char *argv[] );
int   init_structs         ( optiondata *opt, motparm * mp, port_list * plist );
int   open_incoming_socket ( optiondata *opt, port_list * plist );
int   open_serial          ( optiondata *opt, port_list * plist );
int   read_socket          ( optiondata *opt, port_list * plist, motparm * mp );
void  send_serial          ( optiondata * opt, port_list * plist, motparm *mot);
int   show_time            ( char * mesg );
void  swap_4               ( void * data, int nswaps );
int   usage                ( char * prog, int level );
int   wait_for_socket      ( optiondata *opt, port_list * plist, motparm *mot );

/* global port numbers, for clean_n_exit */
static port_list g_ports;

static char g_magic_hi [] = { 0xab, 0xcd, 0xef, 0xab, 0 };  /* w/termination */
static char g_magic_bye[] = { 0xde, 0xad, 0xde, 0xad, 0 };
static int  g_magic_len   = 4;

int main(int argc, char *argv[])
{
    optiondata  opt;
    motparm     mp;  
    port_list * plist = &g_ports;
    int         rv;

    if ( (rv = get_options(&opt, &mp, plist, argc, argv)) != 0 )
        return rv;
    
    /* register interrupt trap */
    signal( SIGHUP,  clean_n_exit );
    signal( SIGINT,  clean_n_exit );
    signal( SIGQUIT, clean_n_exit );
    signal( SIGKILL, clean_n_exit );
    signal( SIGTERM, clean_n_exit );
    
    if ( (rv = open_incoming_socket(&opt, plist)) < 0 )
        return rv;

    while (1)           /* run until interrupt or error (consider restart?) */
    {
        mp.nread = 0;           /* reset our counter */

        /* wait for AFNI to talk to us */
        if ( (rv = wait_for_socket(&opt, plist, &mp)) < 0 ) {
            clean_n_exit(0);
            return rv;
        }

        if ( ! opt.no_serial )
            if ( (rv = open_serial(&opt, plist)) != 0 )
                return rv;

        /* read data while it is there */
        while ( (rv = read_socket(&opt, plist, &mp)) == 0)
            if ( ! opt.no_serial )
                send_serial(&opt, plist, &mp);

        close_data_ports(plist);
    } 

    return 0;   /* should not be reached, of course */
}

#if 0
int readnclose(int sd, int verb)
{
    struct timeval tv;
    fd_set         fd;
    char           buf[4];
    int            rv;

    if( sd <= 0 ) return 0;

    FD_ZERO(&fd);  FD_SET(sd, &fd);

    tv.tv_sec = 0; tv.tv_usec = 2;

    rv = select(sd+1, &fd, NULL, NULL, &tv);
    if( rv == -1 ) perror("socket bad on readnclose");

    if( verb ) fprintf(stderr,"-- select returns %d for fd %d\n", rv, sd);

    /* maybe we're done */
    if( !rv ) { close(sd); return 0; }

    /* otherwise, do a test read */
    rv = recv(sd, buf, 1, MSG_PEEK);

    if( verb ) fprintf(stderr,"-- recv returns %d for fd %d\n", rv, sd);

    return rv;
}
#endif

/* ----------------------------------------------------------------------
 * close serial port and data socket
 * ----------------------------------------------------------------------
 */
int close_data_ports( port_list * plist )
{
    if ( plist->sport      != 0 ) close(plist->sport);
    if ( plist->tdata_sd   != 0 ) close(plist->tdata_sd);

    plist->sport = plist->tdata_sd = 0;

    return 0;
}


/* ----------------------------------------------------------------------
 * block until data comes return the open socket
 *
 * we expect to read g_magic_hi 
 * ----------------------------------------------------------------------
 */
int wait_for_socket(optiondata *opt, port_list * plist, motparm * mp)
{
    struct sockaddr_in sin;
    char               data[8];
    int                sd, len, ver, nex;

    len = sizeof(sin);
    /* block until a connection is made */
    if ( (sd = accept(plist->tserver_sd, (struct sockaddr *)&sin, &len)) == -1 )
    {
        perror("wait for socket: accept");
        return -1;
    }

    plist->tdata_sd = sd;

    if ( opt->debug > 0 )
        fprintf(stderr,"++ accepting call from '%s'\n",inet_ntoa(sin.sin_addr));
    if ( opt->show_times )
        show_time("accepted connection");

    if ( (len = recv(sd, data, g_magic_len, 0)) == -1 )
    {
        perror("wait for socket: recv");
        return -1;
    }

    /* check the first 3 bytes of magic_hi, with the 4th determining version */
    if ( strncmp(data, g_magic_hi, g_magic_len-1) != 0 )
    {
        fprintf(stderr, "** bad data on socket: 0x%02hhx%02hhx%02hhx%02hhx\n",
                data[0], data[1], data[2], data[3] );
        return -1;
    }

    /* Hey, they said the magic word! */

    if ( opt->debug > 0 )
        fprintf(stderr,
                "++ received hello on socket: 0x%02hhx%02hhx%02hhx%02hhx\n",
                data[0], data[1], data[2], data[3]);

    /* check the hello version */
    ver = data[3] - (char)0xab;
    if( ver == 1 || ver == 2 ) {
        /* version 1: also receive num_extra over socket */
        /* version 2: also receive disp_all voxels over socket */
        if ( (len = recv(sd, (void *)&nex, sizeof(int), 0)) == -1 )
        {
            perror("wait for socket: recv");
            return -1;
        } else if ( len != sizeof(int) ) {
            fprintf(stderr,"** received only %d of 4 bytes for nextra\n",len);
            return -1;
        }
        if ( opt->show_times ) show_time("received num_extra");

        /* have num extra, apply it */
        if ( opt->swap ) swap_4(&nex, 1);

        /* modify num_extras and disp_all, depending on the version */
        if( ver == 2 ) {
            opt->disp_all = 1;
            nex *= 8;
        } else
            opt->disp_all = 0;

        fprintf(stderr,"++ hello version %d, received nextra = %d (was %d)\n",
                ver, nex, mp->nex);

        /* we may want to alloc some/more/less memory for this */
        if( mp->nex != nex && alloc_extras(mp, nex) ) return -1;

    } else if ( ver != 0 ) {
        fprintf(stderr,"** bad magic version from socket: %d\n",ver);
        return -1;
    } /* else, default hello version */

    return 0;
}

/* ----------------------------------------------------------------------
 * if nex changes (maybe from 0) allocate an appropriate extras array
 *
 * return 0 on success
 * ----------------------------------------------------------------------
 */
int alloc_extras(motparm * mp, int nex)
{
    if ( mp->nex == nex ) return 0;

    mp->nex = nex;

    if( nex <= 0 ) {    /* then free any pointer and return */
        if( mp->extras ) { free(mp->extras); mp->extras = NULL; }
        mp->nex = 0;
        return 0;
    }

    /* else update the memory */
    mp->extras = (float *)realloc(mp->extras, mp->nex*sizeof(float));
    if( !mp->extras )
    {
        fprintf(stderr,"** failed to alloc for %d extra floats\n", mp->nex);
        return 1;
    }

    return 0;
}

/* ----------------------------------------------------------------------
 * show the current time, at the ms resolution, modulo an hour
 * ----------------------------------------------------------------------
 */
int show_time( char * mesg )
{
   struct timeval  tval ;
   struct timezone tzone ;

   gettimeofday( &tval , &tzone ) ;

   if( mesg ) fprintf(stderr,"++ SH TIME (%s): ", mesg);
   else       fprintf(stderr,"++ SH TIME : ");

   fprintf(stderr,"%d seconds, %d ms\n", ((int)tval.tv_sec)%3600,
                                         ((int)tval.tv_usec)/1000);

   return 0;
}

/* ---------------------------------------------------------------------- */
/* create the socket, and announce that we are listening                  */
int open_incoming_socket( optiondata * opt, port_list * plist )
{
    struct sockaddr_in sin;
    int                sd;

    if ( opt->sock_num < 5000 || opt->sock_num > 65535 )
    {
        fprintf(stderr, "** bad socket number: %d\n", opt->sock_num);
        return -1;
    }

    if ( opt->debug > 1 )
        fprintf(stderr,"-- attempting to open port %d\n", opt->sock_num);

    /* create a comm. endpoint */
    if ( (sd = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    {
        perror("open incoming socket: socket");
        return sd;
    }

    memset( &sin, 0, sizeof(sin) );
    sin.sin_family      = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port        = htons(opt->sock_num);

    /* actually bind the port to the socket */
    if ( bind(sd, (struct sockaddr *)&sin, sizeof(sin)) == -1 )
    {
        perror("open incoming socket: bind");
        return -1;
    }

    /* announce that we are ready to accept connections */
    if ( listen(sd, 3) == -1 )
    {
        perror("open incoming socket: listen");
        return -1;
    }

    /* if we get here, all is well to start accepting communication */

    /* store the socket descriptor and return success */
    plist->tserver_sd = sd;

    if ( opt->debug > 0 )
        fprintf(stderr,"++ port %d open (sd = %d), listening...\n",
                opt->sock_num, sd);

    return 0;
}

/* ----------------------------------------------------------------------
 * initialize data structures
 * ----------------------------------------------------------------------
 */
int init_structs( optiondata *opt, motparm * mp, port_list * plist )
{
    memset(opt,   0, sizeof(*opt)  );
    memset(plist, 0, sizeof(*plist));
    memset(mp,    0, sizeof(*mp)   );

    opt->serial_port = NULL;
    opt->mp_min      = SH_DEF_MIN_FVAL;
    opt->mp_max      = SH_DEF_MAX_FVAL;
    opt->sock_num    = SH_DEF_SOCKET;

    return 0;
}


/* close any open ports (to possibly catch an interrupt) */
void clean_n_exit(int sig_num)
{
    int ssec, count;

    if ( g_ports.debug > 0 )
    {
        fputs("-- final check: closing ports\n", stderr);
        if ( g_ports.debug > 1 )
        {
            fprintf(stderr,"   descriptors: ser = %d, data = %d, serv = %d\n",
                    g_ports.sport, g_ports.tdata_sd, g_ports.tserver_sd);
            fprintf(stderr,"-- sig_num = %d\n", sig_num);
        }
        fflush(stderr);
    }

    close_data_ports(&g_ports);
    if ( g_ports.tserver_sd ) close(g_ports.tserver_sd); /* only at exit */

    exit(sig_num);
}
        
#define CHECK_ARG_COUNT(ac,str)         \
        do {                            \
            if ((ac+1) >= argc) {       \
                fputs(str,stderr);      \
                return -1;              \
            }                           \
        } while (0)                     \

int get_options(optiondata *opt, motparm * mp, port_list * plist,
                int argc, char *argv[])
{
    char * prog = argv[0];
    int    ac;

    init_structs(opt, mp, plist);

    if ( argc < 2 )
        return usage(prog, SH_USE_SHORT);

    for ( ac = 1; ac < argc; ac++ )   /* help, hist first, rest alphabetical */
    {
        if ( !strncmp(argv[ac], "-help", 5) )
            return usage(prog, SH_USE_LONG);
        if ( !strncmp(argv[ac], "-hist", 5) )
            return usage(prog, SH_USE_HIST);
        else if ( !strncmp(argv[ac], "-debug", 6) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -debug DEBUG_LEVEL\n");
            opt->debug = atoi(argv[++ac]);
        }
        else if ( !strncmp(argv[ac], "-disp_all", 9) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -disp_all NVOXELS\n");
            if( opt->num_extra > 0 ) {
                fprintf(stderr,"** cannot use both -num_extra and -disp_all\n");
                return -1;
            }
            opt->num_extra = atoi(argv[++ac]) * 8;
            opt->disp_all = 1;
        }
        else if ( !strncmp(argv[ac], "-mp_max", 6) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -mp_max MAX_MP_VAL\n");
            opt->mp_max = atof(argv[++ac]);
        }
        else if ( !strncmp(argv[ac], "-mp_min", 6) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -mp_min MIN_MP_VAL\n");
            opt->mp_min = atof(argv[++ac]);
        }
        else if ( !strncmp(argv[ac], "-no_serial", 7) )
            opt->no_serial = 1;
        else if ( !strncmp(argv[ac], "-num_extra", 7) )
        {
            if( opt->disp_all ) {
                fprintf(stderr,"** cannot use both -num_extra and -disp_all\n");
                return -1;
            }
            CHECK_ARG_COUNT(ac, "opt use: -num_extra NVALS\n");
            opt->num_extra = atoi(argv[++ac]);
        }
        else if ( !strncmp(argv[ac], "-serial_port", 7) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -serial_port SERIAL_FILENAME\n");
            opt->serial_port = argv[++ac];
        }
        else if ( !strncmp(argv[ac], "-show_times", 5) )
            opt->show_times = 1;
        else if ( !strncmp(argv[ac], "-sock_num", 7) )
        {
            CHECK_ARG_COUNT(ac, "opt use: -sock_num SOCKET_NUMBER\n");
            opt->sock_num = atoi(argv[++ac]);
        }
        else if ( !strncmp(argv[ac], "-swap", 5) )
            opt->swap = 1;
        else if ( !strncmp(argv[ac], "-ver", 4) )
            return usage(prog, SH_USE_VERSION);
        else
        {
            fprintf(stderr,"** invalid option '%s', exiting...\n", argv[ac]);
            return -1;
        }
    }

    /* check basic options */
    if ( opt->sock_num <= 0 || opt->sock_num > 65536 )
    {
        fprintf(stderr,"** socket number %d is out of range\n", opt->sock_num);
        return -1;
    }
    if ( ! opt->serial_port && opt->no_serial == 0 )
    {
        fprintf(stderr,"** missing option '-serial_port'\n");
        return -1;
    }

    if ( opt->num_extra < 0 || opt->num_extra > 1000 )
    {
        fprintf(stderr,"** -num_extra %d is out of range [0,1000]\n",
                opt->num_extra);
        return -1;
    }

    if( opt->num_extra > 0 ) alloc_extras(mp, opt->num_extra);

    if ( opt->debug > 1 )
        disp_optiondata( "options read: ", opt );

    plist->debug = opt->debug;          /* for clean_n_exit() */
    mp->nvals    = 6;

    return 0;
}


/* ----------------------------------------------------------------------
 * display usage
 *
 * SH_USE_SHORT   : most basic usage info
 * SH_USE_VERSION : display version number
 * SH_USE_LONG    : display complete description of program and options
 * ----------------------------------------------------------------------
 */
int usage( char * prog, int level )
{
    if ( level == SH_USE_SHORT )
        printf( "usage: %s -help\n"
                "usage: %s [options] -serial_port FILENAME\n", prog, prog );
    else if ( level == SH_USE_HIST )
        fputs( g_history, stdout );
    else if ( level == SH_USE_VERSION )
        printf( "%s, version %s, compiled %s\n", prog, VERSION, __DATE__ );
    else if ( level == SH_USE_LONG )
    {
        printf(
            "------------------------------------------------------------\n"
            "%s - pass motion parameters from socket to serial port\n"
            "\n"
            "    This program is meant to receive registration (motion?)\n"
            "    correction parameters from afni's realtime plugin, and to\n"
            "    pass that data on to a serial port.\n"
            "\n"
            "    The program is meant to run as a tcp server.  It listens\n"
            "    for a connection, then processes data until a termination\n"
            "    flag is received (sending data from the tcp socket to the\n"
            "    serial port), closes the new connection, and goes back\n"
            "    to a listening state.\n"
            "\n"
            "    The basic outline is:\n"
            "\n"
            "    open tcp server socket\n"
            "    repeat forever:\n"
            "        wait for a tcp client connection\n"
            "        open a serial port\n"
            "        while the client sends new data\n"
            "            write that data to the serial port\n"
            "        close the serial port and client socket\n"
            "\n"
            "    The expected client is the realtime plugin to afni,\n"
            "    plug_realtime.so.  If the afni user has their environment\n"
            "    variable AFNI_REALTIME_MP_HOST_PORT set as HOST:PORT,\n"
            "    then for EACH RUN, the realtime plugin will open a tcp\n"
            "    connection to the given HOST and PORT, pass the magic hello\n"
            "    data (0xabcdefab), pass the 6 motion parameters for each\n"
            "    time point, and signal a closure by passing the magic bye\n"
            "    data (0xdeaddead).\n"
            "\n"
            "    On this server end, the 'repeat forever' loop will do the\n"
            "    following.  First it will establish the connection by\n"
            "    checking for the magic hello data.  If that data is found,\n"
            "    the serial port will be opened.\n"
            "\n"
            "    Then it will repeatedly check the incoming data for the\n"
            "    magic bye data.  As long as that check fails, the data is\n"
            "    assumed to be valid motion parameters.  And so 6 floats at a\n"
            "    time are read from the incoming socket and passed to the\n"
            "    serial port.\n"
            "\n"
            "  usage: %s [options] -serial_port FILENAME\n"
            "------------------------------------------------------------\n"
            "  examples:\n"
            "\n"
            "    1. display this help :\n"
            "\n"
            "        %s -help\n"
            "\n"
            "    2. display the module history :\n"
            "\n"
            "        %s -hist\n"
            "\n"
            "    3. display the current version number :\n"
            "\n"
            "        %s -ver\n"
            "\n"
            "  * 4. run normally, using the serial port file /dev/ttyS0 :\n"
            "\n"
            "        %s -serial_port /dev/ttyS0\n"
            "\n"
            "  * 5. same as 4, but specify socket number 53214 :\n"
            "\n"
            "        %s -serial_port /dev/ttyS0 -sock_num 53214\n"
            "\n"
            "    6. same as 5, but specify minmum and maximum bounds on\n"
            "       the values :\n"
            "\n"
            "        %s                       \\\n"
            "            -serial_port /dev/ttyS0            \\\n"
            "            -sock_num 53214                    \\\n"
            "            -mp_min -12.7                      \\\n"
            "            -mp_max  12.7\n"
            "\n"
            "    7. run the program in socket test mode, without serial\n"
            "       communication, and printing all the incoming data\n"
            "\n"
            "        %s -no_serial -debug 3\n"
            "\n"
            "    7a.run the program in socket test mode, without serial\n"
            "       communication, and showing incoming via -disp_all\n"
            "       (assumes real-time plugin mask has 2 voxels set)\n"
            "\n"
            "        %s -no_serial -disp_all 2\n"
            "\n"
            "    8. same as 4, but use debug level 3 to see the parameters\n"
            "       that will be passed on, and duplicate all output to the\n"
            "       file, helper.output\n"
            "\n"
            "       note: this command is for the t-shell, and will not work\n"
            "             under bash (for bash do the 2>&1 thingy...)\n"
            "\n"
            "        %s -serial_port /dev/ttyS0 -debug 3 |& tee helper.out\n"
            "\n"
            "    9. same as 4, but will receive 3 extra floats per TR\n"
            "\n"
            "        %s -serial_port /dev/ttyS0 -num_extra 3\n"
            "\n"
            " * See 'example F' from 'Dimon -help' for a complete real-time\n"
            "   testing example.\n"
            "\n"
            "------------------------------------------------------------\n"
            "  program setup:\n"
            "\n"
            "    1. Start '%s' on the computer with the serial port that\n"
            "       the motion parameters should be written to.  Example 3\n"
            "       is the most likely case, though it might be useful to\n"
            "       use example 8.\n"
            "\n"
            "    2. On the computer which will be used to run 'afni -rt',\n"
            "       set the environment variable AFNI_REALTIME_MP_HOST_PORT\n"
            "       to the appropriate host:port pair.  See the '-sock_num'\n"
            "       option below for more details.\n"
            "\n"
            "       This variable can also be set in the ~/.cshrc file, or\n"
            "       as part of the AFNI environment via the ~/.afnirc file.\n"
            "\n"
            "    3. Start 'afni -rt'.  Be sure to request 'realtime' graphing\n"
            "       of the '3D: realtime' Registration parameters.\n"
            "\n"
            "    4. Start receiving data (sending it to the realtime plugin).\n"
            "\n"
            "       Note that for testing purposes, I may work well to get a\n"
            "       set of I-files (say, in directories 003, 023, etc.), and\n"
            "       to use Imon to send not-so-real-time data to afni.  An\n"
            "       example of Imon for this purpose might be:\n"
            "\n"
            "           Imon -start_dir 003 -quit -rt -host localhost\n"
            "\n"
            "       See 'Imon -help' for more information.\n"
            "\n"
            "------------------------------------------------------------\n"
            " HELLO versions:\n"
            "\n"
            "    The version number is computed by subtracting 0xab from the\n"
            "    last byte of the HELLO string (so that the default HELLO\n"
            "    string means version 0).\n"
            "\n"
            "    version 0: This is the default, which means serial_helper\n"
            "               must be told what to expect from the real-time\n"
            "               plugin via -num_extra or -disp_all.\n"
            "\n"
            "    version 1: A 4-byte int will follow the HELLO string.  This\n"
            "               number will be used as with -num_extra.\n"
            "\n"
            "    version 2: A 4-byte int will follow the HELLO string.  This\n"
            "               number will be used as with -disp_all.\n"
            "\n"
            "    These versions can change with each new HELLO string.\n"
            "\n"
            "------------------------------------------------------------\n"
            "  'required' parameter:\n"
            "\n"
            "    -serial_port FILENAME : specify output serial port\n"
            "                          : -serial_port /dev/ttyS0\n"
            "\n"
            "        If the user is not using any of the 'special' options,\n"
            "        below, then this parameter is required.\n"
            "\n"
            "        The FILENAME is the device file for the serial port\n"
            "        which will be used for output.\n"
            "------------------------------\n"
            "  special options (for information or testing):\n"
            "\n"
            "    -help            : show this help information\n"
            "\n"
            "    -hist            : show the module history\n"
            "\n"
            "    -debug LEVEL     : set the debugging level to LEVEL\n"
            "                     : e.g. -debug 2\n"
            "                     : default is 0, max is 3\n"
            "\n"
            "    -no_serial       : turn of serial port output\n"
            "\n"
            "        This option is used for testing the incoming data,\n"
            "        when output to a serial port is not desired.  The\n"
            "        program will otherwise operate normally.\n"
            "\n"
            "    -version         : show the current version number\n"
            "------------------------------\n"
            "  'normal' options:\n"
            "\n"
            "    -mp_max MAX_VAL  : limit the maximum value of the MP data\n"
            "                     : e.g. -mp_max 12.7\n"
            "                     : default is 12.7\n"
            "\n"
            "        If any incoming data is greater than this value, it will\n"
            "        be set to this value.  The default of 12.7 is used to\n"
            "        scale incoming floats to signed bytes.\n"
            "\n"
            "    -mp_min MIN_VAL  : limit the minimum value of the MP data\n"
            "                     : e.g. -mp_min -12.7\n"
            "                     : default is -12.7\n"
            "\n"
            "        If any incoming data is less than this value, it will\n"
            "        be set to this value.  The default of -12.7 is used to\n"
            "        scale incoming floats to signed bytes.\n"
            "\n"
            "    -show_times      : show communication times\n"
            "                     : e.g. -show_times\n"
            "\n"
            "        Each time data is recived, display the current time.\n"
            "        Time is at millisecond resolution, and wraps per hour.\n"
            "\n"
            "    -sock_num SOCK   : specify socket number to serve\n"
            "                     : e.g. -sock_num 53214\n"
            "                     : default is 53214\n"
            "\n"
            "        This is the socket the program will use to listen for\n"
            "        new connections.  This is the socket number that should\n"
            "        be provided to the realtime plugin via the environment\n"
            "        variable, AFNI_REALTIME_MP_HOST_PORT.\n"
            "\n"
            "        On the machine the user run afni from, that environment\n"
            "        variable should have the form HOST:PORT, where a basic\n"
            "        example might be localhost:53214.\n"
            "\n"
            "    -num_extra NVALS : will receive NVALS extra floats per TR\n"
            "                     : e.g. -num_extra 5\n"
            "                     : default is 0\n"
            "\n"
            "        Extra floats may arrive if, for instance, afni's RT\n"
            "        plugin has a mask with 3 ROIs in it (numbered 1,2,3).\n"
            "        The plugin would compute averages over each ROI per TR,\n"
            "        and send that data after the MP vals.\n"
            "\n"
            "        In such a case, specify '-num_extra 3', so the program\n"
            "        knows 3 floats will be received after the MP data.\n"
            "\n"
            "        Note that -disp_all cannot be used with -num_extra.\n"
            "\n"
            "    -disp_all NVOX   : will receive NVOX*8 extra floats per TR\n"
            "                     : e.g. -disp_all 5\n"
            "                     : default is 0\n"
            "\n"
            "        Similar to -num_extra, here the program expect data on\n"
            "        a per voxel basis, not averaged over ROIs.\n"
            "\n"
            "        Here the users specifies the number of voxels for which\n"
            "        ALL_DATA will be sent (to serial_helper).  The 8 values\n"
            "        per voxel are (still in float):\n"
            "\n"
            "            index  i  j  k  x  y  z data_value\n"
            "\n"
            "        Currently, serial_helper will output this inforamtion\n"
            "        simply as 1 row per voxel.\n"
            "\n"
            "        Note that -disp_all cannot be used with -num_extra.\n"
            "\n"
            "------------------------------------------------------------\n"
            "  Authors: R. Reynolds, T. Ross  (March, 2004)\n"
            "------------------------------------------------------------\n",
            prog, prog,
            prog, prog, prog, prog, prog, prog, prog, prog, prog, prog,
            prog
            );
    }
    else
        fprintf(stderr,"** usage error: invalid level %d\n", level);

    return 1;
}


/* ----------------------------------------------------------------------
 * check for close requeset
 *
 * return  1 : close
 *         0 : continue
 *        -1 : error
 * ----------------------------------------------------------------------
 */
int test_socket(int sd)
{
    char data[16];
    int  len;

    if ( (len = recv(sd, data, g_magic_len, MSG_PEEK)) == -1 )
    {
        fputs("** test_socket_failure\n", stderr);
        perror("test_socket: recv");
        return -1;
    }

    if ( strncmp(data, g_magic_bye, g_magic_len) == 0 )
        return 1;

    return 0;
}

/* ----------------------------------------------------------------------
 * read one set of motion parameters (extras?) and store in structure
 *
 * return 1 : finished
 *        0 : have data: continue
 *       -1 : error
 * ----------------------------------------------------------------------
 */
int read_socket(optiondata * opt, port_list * plist, motparm * mp)
{
    static char * outstring = NULL;
    static int    oslen = 0;
    int           rv, len;

    if ( (rv = test_socket(plist->tdata_sd)) < 0 )
        return -1;
    else if ( rv == 1 )
    {
        if ( opt->debug > 0 )
            fprintf(stderr,"++ found close request, mpcount = %d\n", mp->nread);
        return 1;
    }

    /* get motion params */
    len = mp->nvals * sizeof(float);
    if ( (rv = recv(plist->tdata_sd, (void *)mp->data, len, 0)) < len )
    {
        fprintf(stderr,"** read only %d of %d bytes on socket\n", rv, len);
        perror("recv mot parm");
        return -1;
    }

    if ( opt->show_times && opt->debug > 2 ) show_time("received mp data");

    if ( opt->swap ) swap_4(mp->data, mp->nvals);

    /* get extra floats */
    if( mp->nex > 0 )
    {
        len = mp->nex * sizeof(float);
        if ( (rv = recv(plist->tdata_sd, (void *)mp->extras, len, 0)) < len )
        {
            fprintf(stderr,"** read only %d of %d Ebytes on socket\n", rv, len);
            perror("recv extra floats");
            return -1;
        }

        if ( opt->swap ) swap_4(mp->extras, mp->nex);
    }

    mp->nread++;

    if ( opt->show_times ) {
        char mesg[32];
        sprintf(mesg, "received mp data #%03d", mp->nread);
        show_time(mesg);
    }

    if ( opt->debug > 2 || opt->disp_all ) {
        rv = format_output(opt, mp, &outstring, &oslen);
        if( rv ) {
            fprintf(stderr,"** failed to format output string\n");
            if( outstring ) { free(outstring); outstring = NULL; }
            return 1;
        }

        /* will probably want to send elsewhere, later */
        fputs(outstring,stderr);
        fflush(stderr);  /* may get buffered */
    }

    return 0;
}

#undef LENTEST
#define LENTEST(bytes,posn,len) \
    do { if( bytes > (len - posn) ) {                                     \
             fprintf(stderr,"** format error: bytes %d exceeds %d-%d\n",  \
                            bytes, len, posn);                            \
             return 1;                                                    \
         } else if (opt->debug > 4) {                                     \
             fprintf(stderr, "-- bytes, posn, len = %d, %d, %d\n",        \
                             bytes, posn, len);                           \
         } } while(0)

int format_output(optiondata * opt, motparm * mp, char ** outstr, int * oslen)
{
    char dhdr1[] = "++ recv floats:";
    char dhdr2[] = "++ recv %d extra floats:";
    int  ind, ind2, len, posn, bytes, nlines;

    if( !opt || !mp || !outstr || !oslen ) return 1;
    if( opt->debug > 3 ) fprintf(stderr,"-- formatting output...\n");

    if( opt->disp_all && (mp->nex % 8) ) {
        fprintf(stderr,"** num extras (%d) not multiple of 8\n",mp->nex);
        return 1;
    }

    /* first compute needed length */
    len = strlen(dhdr1) + strlen(dhdr2) + 10; /* headers and newlines */
    len += mp->nvals*12;
    len += mp->nex*12;
    if(opt->disp_all) len += (mp->nex/8 * 6); /* extra spaces/newlines */
    len += 10;                                /* a little extra padding */

    if( !*outstr || *oslen < len ) {
        /* allocate space for string */
        *outstr = (char *)realloc(*outstr, len*sizeof(char));
        if( !*outstr ) {
            fprintf(stderr,"** failed to re-alloc %d bytes for ostr\n", len);
            *oslen = 0;
            return 1;
        }
        *oslen = len;
    }

    /* motion params - common output*/
    posn = 0;
    bytes = snprintf(*outstr+posn, len-posn, dhdr1);
    posn += bytes;
    for( ind = 0; ind < mp->nvals; ind++ ) {
        bytes = snprintf(*outstr+posn, len-posn, "  %10f", mp->data[ind]);
        LENTEST(bytes,posn,len);
        posn += bytes;
    }
    strcat(*outstr, "\n");
    posn++;

    /* extras varies per debug or disp_all */
    if ( opt->disp_all ) {
        bytes = snprintf(*outstr+posn, len-posn,
                         "++ recv %dx%d extra floats:\n", mp->nex/8, 8);
        LENTEST(bytes,posn,len);
        posn += bytes;

        for( ind = 0; ind < mp->nex/8; ind++ ) {
            strcat(*outstr, "    ");
            posn += 4;
            for( ind2 = 0; ind2 < 8; ind2++ ) {
                bytes = snprintf(*outstr+posn, len-posn, " %11f",
                                 mp->extras[ind*8+ind2]);
                LENTEST(bytes,posn,len);
                posn += bytes;
            }
            strcat(*outstr, "\n");
            posn++;
        }
    } else if( opt->debug > 2 && mp->nex > 0 ) {
        bytes = snprintf(*outstr+posn, len-posn, dhdr2, mp->nex);
        LENTEST(bytes,posn,len);
        posn += bytes;

        for( ind = 0; ind < mp->nex; ind++ ) {
            bytes = snprintf(*outstr+posn, len-posn, "  %10f", mp->extras[ind]);
            LENTEST(bytes,posn,len);
            posn += bytes;
        }
        strcat(*outstr, "\n");
        posn++;
    }

    if( opt->debug > 3 ) fprintf(stderr,"++ posn, len = %d, %d\n",posn,len);

    return 0;
}

/* Here we convert each mp data value to a char after multiplying by 10.
   For now, I'm not sure what to do with the extras, send as is... */
void send_serial(optiondata * opt, port_list * plist, motparm *mot)
{
    static char outdata[7];
    int i, len;
    
    outdata[0] = (char)(-128 + mot->nex);  /* encode nex in handshake byte */
    for(i = 0; i < 6; i++)
    {
        if (mot->data[i] > opt->mp_max) 
            mot->data[i] = opt->mp_max;
        if (mot->data[i] < opt->mp_min)
            mot->data[i] = opt->mp_min;
        outdata[i+1] = (char) (mot->data[i] * 10.0);
    }
    i = write(plist->sport, outdata, 7);
    if( i < 7 )
        fprintf(stderr, "** warning: wrote %d of 7 bytes to serial port\n",i);

    if( mot->nex > 0 )
    {
        len = mot->nex * sizeof(float);
        i = write(plist->sport, mot->extras, len);
        if( i < len )
            fprintf(stderr,"** warning: wrote %d of %d Ebytes to serial port\n",
                    i, len);
    }
} 


int open_serial(optiondata *opt, port_list * plist)
{
        int sport;
        struct termios options;
        
        sport = open(opt->serial_port, O_RDWR | O_NOCTTY | O_NDELAY); 
        if (sport == -1) {
                perror("pe: Failed to open the serial port ");
                return -1;
        }
        
        /*******************
         set up the port 
         *******************/
         
        fcntl(sport, F_SETFL, FNDELAY);  /* nonblocking reads */

    /* Get the current options for the port...*/
    tcgetattr(sport, &options);

    /* Set the baud rates to 9600 */
    cfsetispeed(&options, B9600);
    cfsetospeed(&options, B9600);

    /* Enable the receiver and set local mode */
        options.c_cflag |= (CLOCAL | CREAD );
        
        /* set 8 bit N parity */
        options.c_cflag &= ~PARENB;
        options.c_cflag &= ~CSTOPB;
        options.c_cflag &= ~CSIZE;
        options.c_cflag |= CS8;

        /* raw data input and output */
    options.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);
    options.c_oflag &= ~OPOST;

    /*Set the new options for the port*/
    if (tcsetattr(sport, TCSANOW, &options) == -1) {
                perror("pe: Failed to set attributes on the serial port ");
                close(sport);
                return -1;
        }
        
    plist->sport = sport;
    return 0;
}


/* ----------------------------------------------------------------------
 * swap 4 bytes at a time
 * ----------------------------------------------------------------------
 */
void swap_4( void * data, int nswaps )
{
    char * cp, tmp;
    int    c;
    for ( c = 0, cp = (char *)data; c < nswaps; c++, cp += 4 )
    {
        tmp = cp[0];  cp[0] = cp[3];  cp[3] = tmp;
        tmp = cp[1];  cp[1] = cp[2];  cp[2] = tmp;
    }
}


/* ----------------------------------------------------------------------
 * display contents of optiondata struct
 * ----------------------------------------------------------------------
 */
int disp_optiondata( char * info, optiondata * D )
{
    if ( info )
        fputs(info, stderr);
    
    if ( ! D )
    {
        fprintf(stderr,"** disp_optiondata: D == NULL\n");
        return -1;
    }

    fprintf(stderr,
            " optiondata at %p :\n"
            "    serial_port     = %s\n"
            "    no_serial       = %d\n"
            "    mp_min, mp_max  = %f, %f\n"
            "    sock_num        = %d\n"
            "    num_extra       = %d\n"
            "    swap, debug     = %d, %d\n",
            D, CHECK_NULL_STR(D->serial_port), D->no_serial,
            D->mp_min, D->mp_max, D->sock_num, D->num_extra, D->swap, D->debug);

    return 0;
}
