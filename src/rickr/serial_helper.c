#define VERSION "1.2 (April 1, 2004)"

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
 "----------------------------------------------------------------------\n";


#include <stdio.h>   /* Standard input/output definitions */
#include <string.h>  /* String function definitions */
#include <termios.h> /* POSIX terminal control definitions */
#include <unistd.h>  /* UNIX standard function definitions */
#include <fcntl.h>   /* File control definitions */
#include <errno.h>   /* Error number definitions */

#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define SH_MAX_VALS            6
#define SH_DEF_MIN_FVAL    -12.8
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
    int     swap;
    int     debug;
} optiondata;

typedef struct {
    int   nread;		/* number of instances processed */
    int   nvals;
    float data[SH_MAX_VALS];
} motparm;

typedef struct
{
    int debug;			/* for global access in cleanup() */
    int sport;
    int tdata_sd;
    int tserver_sd;
} port_list;


void  cleanup              ( int sig_num );
int   close_data_ports     ( port_list * plist );
int   disp_optiondata      ( char * info, optiondata * D );
int   get_options          ( optiondata *opt, motparm * mp, port_list * plist,
	                     int argc, char *argv[] );
int   init_structs         ( optiondata *opt, motparm * mp, port_list * plist );
int   open_incoming_socket ( optiondata *opt, port_list * plist );
int   open_serial          ( optiondata *opt, port_list * plist );
int   read_socket          ( optiondata *opt, port_list * plist, motparm * mp );
void  send_serial          ( optiondata * opt, port_list * plist, motparm *mot);
void  swap_4               ( void * data, int nswaps );
int   usage                ( char * prog, int level );
int   wait_for_socket      ( optiondata *opt, port_list * plist );

/* global port numbers, for cleanup */
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
    signal( SIGTERM, cleanup );
    signal( SIGINT, cleanup );
    
    if ( (rv = open_incoming_socket(&opt, plist)) < 0 )
	return rv;

    while (1)		/* run until interrupt or error (consider restart?) */
    {
	mp.nread = 0;		/* reset our counter */

	/* wait for AFNI to talk to us */
	if ( (rv = wait_for_socket(&opt, plist)) < 0 )
	    return rv;

	if ( ! opt.no_serial )
	    if ( (rv = open_serial(&opt, plist)) != 0 )
		return rv;

	/* read data while it is there */
	while (read_socket(&opt, plist, &mp) == 0)
	    if ( ! opt.no_serial )
		send_serial(&opt, plist, &mp);

	close_data_ports(plist);
    } 

    return 0;	/* should not be reached, of course */
}


/* ----------------------------------------------------------------------
 * close serial port and data socket
 * ----------------------------------------------------------------------
 */
int close_data_ports( port_list * plist )
{
    if ( plist->sport    != 0 ) close(plist->sport);
    if ( plist->tdata_sd != 0 ) close(plist->tdata_sd);

    plist->sport = plist->tdata_sd = 0;

    return 0;
}


/* ----------------------------------------------------------------------
 * block until data comes return the open socket
 *
 * we expect to read g_magic_hi 
 * ----------------------------------------------------------------------
 */
int wait_for_socket(optiondata *opt, port_list * plist)
{
    struct sockaddr_in sin;
    char               data[8];
    int                sd, len;

    len = sizeof(sin);
    /* block until a connection is made */
    if ( (sd = accept(plist->tserver_sd, (struct sockaddr *)&sin, &len)) == -1 )
    {
	perror("pe: accept");
	return -1;
    }

    plist->tdata_sd = sd;

    if ( opt->debug > 0 )
	fprintf(stderr,"++ accepting call from '%s'\n",inet_ntoa(sin.sin_addr));

    if ( (len = recv(sd, data, g_magic_len, 0)) == -1 )
    {
	perror("pe: recv");
	return -1;
    }

    if ( strncmp(data, g_magic_hi, g_magic_len) != 0 )
    {
	fprintf(stderr, "** bad data on socket: 0x%x%x%x%x\n",
		data[0], data[1], data[2], data[3] );
	return -1;
    }

    /* Hey, they said the magic word! */

    if ( opt->debug > 0 )
	fprintf(stderr,"++ got hello string '%s', ready for data...\n",
		g_magic_hi);

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
	perror("pe: socket");
	return sd;
    }

    memset( &sin, 0, sizeof(sin) );
    sin.sin_family      = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port        = htons(opt->sock_num);

    /* actually bind the port to the socket */
    if ( bind(sd, (struct sockaddr *)&sin, sizeof(sin)) == -1 )
    {
	perror("pe: bind");
	return -1;
    }

    /* announce that we are ready to accept connections */
    if ( listen(sd, 3) == -1 )
    {
	perror("pe: listen");
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
void cleanup(int sig_num)
{
    if ( g_ports.debug > 0 )
    {
	fputs("-- final check: closing ports\n", stderr);
	if ( g_ports.debug > 1 )
	{
	    fprintf(stderr,"   descriptors: ser = %d, data = %d, serv = %d\n",
		    g_ports.sport, g_ports.tdata_sd, g_ports.tserver_sd);
	    fprintf(stderr,"-- sig_num = %d\n", sig_num);
	}
    }

    if ( g_ports.sport != 0)
	close(g_ports.sport);
    if (g_ports.tdata_sd != 0)
	close(g_ports.tdata_sd);
    if (g_ports.tserver_sd != 0)
	close(g_ports.tserver_sd);

    g_ports.sport = g_ports.tdata_sd = g_ports.tserver_sd = 0;
}
	
#define CHECK_ARG_COUNT(ac,str)		\
	do {				\
	    if ((ac+1) >= argc) {	\
		fputs(str,stderr);	\
		return -1;		\
	    }				\
	} while (0)			\

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
	else if ( !strncmp(argv[ac], "-serial_port", 7) )
	{
	    CHECK_ARG_COUNT(ac, "opt use: -serial_port SERIAL_FILENAME\n");
	    opt->serial_port = argv[++ac];
	}
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

    if ( opt->debug > 1 )
	disp_optiondata( "options read: ", opt );

    plist->debug = opt->debug;		/* for cleanup() */
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
	    "            -mp_min -12.8                      \\\n"
	    "            -mp_max  12.7\n"
	    "\n"
	    "    7. run the program in socket test mode, without serial\n"
	    "       communication, and printing all the incoming data\n"
	    "\n"
	    "        %s -no_serial -debug 3\n"
	    "\n"
	    "    8. same as 4, but use debug level 3 to see the parameters\n"
	    "       that will be passed on, and duplicate all output to the\n"
	    "       file, helper.output\n"
	    "\n"
	    "       note: this command is for the t-shell, and will not work\n"
	    "             under bash (for bash do the 2>&1 thingy...)\n"
	    "\n"
	    "        %s -serial_port /dev/ttyS0 -debug 3 |& tee helper.out\n"
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
	    "                     : e.g. -mp_min -12.8\n"
	    "                     : default is -12.8\n"
	    "\n"
	    "        If any incoming data is less than this value, it will\n"
	    "        be set to this value.  The default of -12.8 is used to\n"
	    "        scale incoming floats to signed bytes.\n"
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
	    "------------------------------------------------------------\n"
	    "  Authors: R. Reynolds, T. Ross  (March, 2004)\n"
	    "------------------------------------------------------------\n",
	    prog, prog,
	    prog, prog, prog, prog, prog, prog, prog, prog,
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
	perror("pe: recv");
	return -1;
    }

    if ( strncmp(data, g_magic_bye, g_magic_len) == 0 )
	return 1;

    return 0;
}

/* ----------------------------------------------------------------------
 * read one set of motion parameters and store in structure
 *
 * return 1 : finished
 *        0 : have data: continue
 *       -1 : error
 * ----------------------------------------------------------------------
 */
int read_socket(optiondata * opt, port_list * plist, motparm * mp)
{
    int    rv, len;

    if ( (rv = test_socket(plist->tdata_sd)) < 0 )
	return -1;
    else if ( rv == 1 )
    {
	if ( opt->debug > 0 )
	    fprintf(stderr,"++ found close request, mpcount = %d\n", mp->nread);
	return 1;
    }

    len = mp->nvals * sizeof(float);
    if ( (rv = recv(plist->tdata_sd, (void *)mp->data, len, 0)) < len )
    {
	fprintf(stderr,"** read only %d of %d bytes on socket\n", rv, len);
	perror("pe: recv");
	return -1;
    }

    if ( opt->swap )
	swap_4(mp->data, mp->nvals);

    mp->nread++;

    if ( opt->debug > 2 )  /* display the data */
    {
	int c;
	fprintf(stderr,"++ recv floats:");
	for ( c = 0; c < mp->nvals; c++ )
	    fprintf(stderr,"  %f", mp->data[c]);
	fputc('\n', stderr);
    }

    return 0;
}

void send_serial(optiondata * opt, port_list * plist, motparm *mot)
{
    static char outdata[7];
    int i;
    
    outdata[0] = 255;
    for (i=0; i<6; i++) {
	if (mot->data[i] > opt->mp_max) 
	    mot->data[i] = opt->mp_max;
	if (mot->data[i] < opt->mp_min)
	    mot->data[i] = opt->mp_min;
	outdata[i+1] = (char) (mot->data[i] * 10.0);
    }
    i = write(plist->sport, outdata, 7);
    if (i<7)
	fprintf(stderr, "warning: only wrote %d bytes to serial port\n", i);
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
	    "    swap, debug     = %d, %d\n",
	    D, CHECK_NULL_STR(D->serial_port), D->no_serial,
	    D->mp_min, D->mp_max, D->sock_num, D->swap, D->debug);

    return 0;
}
