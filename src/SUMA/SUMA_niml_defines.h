#ifndef SUMA_NIML_DEFINE_INCLUDED
#define SUMA_NIML_DEFINE_INCLUDED

#define SUMA_FLAG_WAITING    1   /*!< Waiting for connection flag */
#define SUMA_FLAG_CONNECTED  2   /*!< Connected flag */
#define SUMA_FLAG_SKIP       4   /*!< Skip flag */

#define SUMA_WriteCheckWait 400 /*!< Milliseconds to wait for each 
                                     stream_writecheck call */ 
#define SUMA_WRITECHECKWAITMAX 2000 /*!< Milliseconds to try and establish 
                                         a good WriteCheck */

typedef enum { SUMA_AFNI_STREAM_INDEX = 0, 
                     /*!< Index of SUMA<-->AFNI stream , afni listen line 1*/ 
               SUMA_AFNI_STREAM_INDEX2 ,  
                     /*!< Index of SUMA<-->AFNI 2nd stream, afni listen line 2 */
               SUMA_TO_MATLAB_STREAM_INDEX, 
                     /*!< Index of SUMA<-->MATLAB 2nd stream, matlab listen */
               SUMA_GENERIC_LISTEN_LINE, 
                     /*!< Using socket  SUMA_TCP_LISTEN_PORT0, 
                           generic suma listen line*/
               SUMA_GEOMCOMP_LINE, 
                     /*!<  Using socket  SUMA_TCP_LISTEN_PORT0 + 1*/
               SUMA_BRAINWRAP_LINE, 
                     /*!<  Using socket SUMA_TCP_LISTEN_PORT0 + 2*/
               SUMA_DRIVESUMA_LINE, 
                     /*!<  Using socket SUMA_TCP_LISTEN_PORT0 + 3*/
               SUMA_GICORR_LINE,
                     /*!<  Using socket SUMA_TCP_LISTEN_PORT0 + 4*/
               SUMA_HALLO_SUMA_LINE,
                     /*!< Communication with demo talking program */
               SUMA_INSTA_TRACT_LINE,
                     /*!< Communication with demo talking program */
               SUMA_MAX_STREAMS 
                     /*!< Maximum number of streams, KEEP AT END */
            } SUMA_STREAM_INDICES; /* If you add a new stream,
                  create a new port number for it in afni_ports.c's 
                  init_ports_list function */
            
#endif

