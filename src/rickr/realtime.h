#ifndef _REALTIME_H_
#define _REALTIME_H_


/* AFNI realtime defines */
#define AFNI_CONTROL_PORT     7954  /* always send control data to AFNI    */
#define AFNI_TCP_PORT         7953  /* maybe send image data to AFNI       */

#define AFNI_OPEN_CONTROL_MODE   1  /* 1st time thru: open control channel */
#define AFNI_WAIT_CONTROL_MODE   2  /* waiting for AFNI to open control    */
#define AFNI_OPEN_DATA_MODE      3  /* now can open data channel to AFNI   */
#define AFNI_CATCHUP_MODE        4  /* waiting for AFNI to open data       */
#define AFNI_CONTINUE_MODE       5  /* at last! data channel is ready!     */

#define ART_STATE_NO_USE         0
#define ART_STATE_TO_OPEN        1
#define ART_STATE_TO_SEND_CTRL   2
#define ART_STATE_IN_USE         3
#define ART_STATE_TO_CLOSE       4

#define ART_COMMAND_MARKER      "Et Earello Endorenna utulien!!"
#define ART_COMMAND_MARKER_LEN  30

#define ART_NAME_LEN           128
#define ART_TBUF_LEN           512

#define ART_ADD_TO_BUF(dest,src) ( strcat(dest,src), strcat(dest,"\n") )
#define CHECK_NULL_STR(str) ( str ? str : "(nil)" )

/* ---------------------------------------------------------------------- */

typedef struct
{
    int       state;			/* state of AFNI realtime             */
    int       mode;			/* if > 0, then means AFNI is active */
    int       use_tcp;			/* if > 0, use TCP/IP to send data  */
    int       swap;			/* byte swap data before sending   */
    int       byte_order;		/* note the byte order of images  */
    char      host[ART_NAME_LEN];	/* hostname of CPU afni is on    */
    char      ioc_name[ART_NAME_LEN];	/* I/O channel name to afni     */
    char      buf[1024];		/* main buffer for afni comm   */
    IOCHAN  * ioc;			/* ptr to I/O channel struct  */
    param_t * param;			/* local pointer to images   */
} ART_comm;

/* ---------------------------------------------------------------------- */

void ART_exit              ( void );
int  ART_idisp_ART_comm    ( char * info, ART_comm * ac );
int  ART_init_AC_struct    ( ART_comm * ac );
int  ART_open_afni_link    ( ART_comm * ac, int num_tries, int again,int debug);
int  ART_send_control_info ( ART_comm * ac, vol_t * v, int debug );
int  ART_send_end_of_run   ( ART_comm * ac, int run, int seq, int debug );
int  ART_send_volume       ( ART_comm * ac, vol_t * v, int debug );
int  ART_start_io          ( ART_comm * ac, int debug );

int swap_2                 ( void * ptr, int npairs );


#endif  /* _REALTIME_H_ */
