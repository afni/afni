#include "niml.h"
#include "SUMA_niml_defines.h"
#include <afni.h> 
#include <ptaylor/TrackIO.h>

#define NIML_DEBUG 1

/* A simple program to illustrate communication with SUMA. */

typedef struct {
   int TCP_port; /* Can get number from stdout of sys. command: 
                           afni -npb PB -port_number SUMA_HALLO_SUMA_NIML */
   NI_stream NimlStream;
   int NimlStream_flag;
   char Hostname[64]; /* default, same computer */
   char StreamName[64];
   int TrackingID;
   int Connected;
   
   int talk_suma;
   int comm_NI_mode;
   float rps;
   float nelps;  /*   number of NI elements to send per second 
                        -1 for going as fast as possible */
   int TrackID;            /*  ID of next element to be sent 
                                 NI_element StartTracking has an ID of 1 */
   int GoneBad;   /* Flag indicating that stream went bad */
   int Send;      /* Flag indicating that elements should be sent 
                                As long as GoneBad is 0 */
} COMM_STRUCT;

COMM_STRUCT * NewCommStruct(char *Hostname, int port); 
int niml_call (COMM_STRUCT *cs);
float etime (struct  timeval  *t, int Report  );
void Wait_Till_Stream_Goes_Bad(COMM_STRUCT *cs, int slp, int WaitMax, int verb); 
int show_niml(void *nel);
int SendToSuma (COMM_STRUCT *cs, NI_group *ngru, int action);
int InstaTract_niml_workproc( void *thereiselvis );
int InstaTract_process_NIML_data(NI_element *nini, COMM_STRUCT *cs);


/* initialize the communication structure 
  When port = -1, the he default port value of 1046 is used.
  It corresponds to the InstaTract port when SUMA's -npb option is set to 0.
  At the moment, you can't call a C function to get the 
  port number for a certain Niml Port Block (npb) without
  depending on the AFNI libraries (see init_ports_list() and its
  brethren). As one way to get the port value that you want for any 
  -npb value you can use the following system command:
  afni -npb 0 -port_number SUMA_HALLO_SUMA_NIML
  */
COMM_STRUCT * NewCommStruct(char *Hostname, int port) 
{
   COMM_STRUCT *cs=NULL;
   cs = (COMM_STRUCT *)calloc(1,sizeof(COMM_STRUCT));
   
   if (Hostname) sprintf(cs->Hostname,"%s",Hostname);
   else sprintf(cs->Hostname,"127.0.0.1");
   
   
   if (port < 0)  cs->TCP_port = 1046;
   else cs->TCP_port = port;
   
   sprintf(cs->StreamName,"tcp:%s:%d", cs->Hostname, cs->TCP_port);

   return(cs);
}


/*
   \brief Initiates a call on stream cs->NimlStream
   
   \return 0: Sucked
           YUP : Did not suck
   
   \sa niml_hangup
*/
int niml_call (COMM_STRUCT *cs)
{
   static char FuncName[]={"niml_call"};
   int nn=-1, Wait_tot;
   int LocalHead = 1;
   
   /* find out if the stream has been established already */
   if (cs->NimlStream) { /* stream is open, nothing to do */
      cs->NimlStream_flag = SUMA_FLAG_CONNECTED;
      if (LocalHead) 
            fprintf(stdout,"%s: Stream existed, reusing.\n", FuncName);
         fprintf(stdout,"%s: cs->Connected.\n", FuncName); fflush(stdout);
   }else {   /* must open stream */              
      /* contact afni */
      fprintf( stdout,
               "%s: Contacting on %s, maximum wait %.3f sec\n", 
               FuncName, cs->StreamName, 
               (float)SUMA_WRITECHECKWAITMAX/1000.0);
      fflush(stdout);
      cs->NimlStream =  NI_stream_open( cs->StreamName , "w" ) ;
      if (!cs->NimlStream) {
         cs->NimlStream_flag = 0;
         cs->TrackingID = 0;
         fprintf(stderr,"NI_stream_open failed (1).\n");
         cs->Connected = !cs->Connected;
         return(0) ;
      }

      if( cs->NimlStream == NULL ){
         fprintf(stderr,"NI_stream_open failed (2)\n");
         cs->Connected = !cs->Connected;
         cs->NimlStream_flag = 0;
         cs->TrackingID = 0;
         return(0) ;
      }

      Wait_tot = 0;
      while(Wait_tot < SUMA_WRITECHECKWAITMAX){
         nn = NI_stream_writecheck( cs->NimlStream , SUMA_WriteCheckWait) ;
         if( nn == 1 ){ 
            fprintf(stderr,"\n") ; 
            cs->NimlStream_flag = SUMA_FLAG_CONNECTED;
            return(1) ; 
         }
         if( nn <  0 ){ 
            fprintf(stderr,"BAD\n"); 
            cs->Connected = !cs->Connected;
            cs->NimlStream = NULL;
            cs->NimlStream_flag = 0;
            cs->TrackingID = 0;
            return(0);
         }
         Wait_tot += SUMA_WriteCheckWait;
         fprintf(stderr,".") ;
      }

      /* make sure you did not exit because of time out */
      if (nn!=1) {
         cs->Connected = !cs->Connected;
         cs->NimlStream = NULL;
         cs->NimlStream_flag = 0; 
         cs->TrackingID = 0;
         fprintf(stderr,"WriteCheck timed out (> %d ms).\n", 
                        SUMA_WRITECHECKWAITMAX);
         return(0);
      }
   } 

   /* Stream is open */
   return(1);
}

/* Utility function to track elapsed time */
float etime (struct  timeval  *t, int Report  )
{
   struct  timeval  tn;
   float Time_Fact = 1000000.0;
   float delta_t;

   /* get time */
   gettimeofday(&tn, NULL);
   
   if (Report)
      {
         delta_t = (((float)(tn.tv_sec - t->tv_sec)*Time_Fact) + 
                     (float)(tn.tv_usec - t->tv_usec)) /Time_Fact;
      }
   else
      {
         t->tv_sec = tn.tv_sec;
         t->tv_usec = tn.tv_usec;
         delta_t = 0.0;
      }
      
   return (delta_t);
   
}

/* A function to wait until open stream goes bad */
void Wait_Till_Stream_Goes_Bad(COMM_STRUCT *cs, int slp, int WaitMax, int verb) 
{  
   static char FuncName[]={"Wait_Till_Stream_Goes_Bad"};
   int good = 1;
   int WaitClose = 0;
   int LocalHead = 0;
   
   if (verb) fprintf (stderr,"\nWaiting for SUMA to close stream .\n");
   while (good && WaitClose < WaitMax) {
      if (NI_stream_goodcheck(cs->NimlStream, 1) <= 0) {
         good = 0;
      } else {
         if (LocalHead) 
            fprintf(stderr,"Good Check OK. Sleeping for %d ms...", slp);
         NI_sleep(slp);
         if (verb) fprintf (stderr,".");
         WaitClose += slp;
      }
   }

   if (WaitClose >= WaitMax) { 
      if (verb) 
         fprintf(stderr,"\nFailed to detect closed stream after %d ms.\n"
                        "Closing shop anyway...", WaitMax);  
   }else{
      if (verb) fprintf (stderr,"Done.\n");
   }

   return;
}

/* Function to write a NIML element in text mode to stdout */
int show_niml(void *nel) 
{
   static char FuncName[]={"show_niml"};
   NI_stream nstdout;
   NI_element *el=NULL;
   
   if (!nel) {
      fprintf (stdout, "\n***********NULL nel  ************\n");
      return(1); 
   }
   nstdout = NI_stream_open( "stdout:","w");
   if( nstdout == NULL ){ 
      fprintf(stderr,"%s: Can't open stdout\n", FuncName); 
      return(0); 
   }
   el = (NI_element *)nel;
   NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
   NI_stream_close(nstdout);
   return(1);
}
 
/* Function that sends to SUMA NIML formatted commands */  
int SendToSuma (COMM_STRUCT *cs, NI_group *ngru, int action)
{
   static char FuncName[]={"SendToSuma"};
   static float etm = 0.0;
   static int i_in = 0;
   char stmp[500];
   static struct  timeval tt;
   float *f=NULL;
   int n=-1, WaitClose, WaitMax, *ip = NULL;
   float wtm;
   int good = 1;
   int LocalHead = 0;
   
   /* fprintf (stderr, "%s: LocalHead = %d\n", FuncName, LocalHead); */
   
   if (action == 0) { /* initialization of connection */
      NI_element *nel = NULL;
      
      fprintf(stdout,"Setting up for communication with SUMA ...\n");
      cs->Send = 1;

      if (!niml_call (cs)) {
         fprintf (stderr,"Failed in SUMA_niml_call\n");
         /* connection flag is reset in SUMA_niml_call */
         cs->Send = 0;
         return(0);
      }

      nel = NI_new_data_element("StartTracking", 0); 
      cs->TrackID = 1; /* that's the index for StartTracking command */
      NI_set_attribute(nel,"ni_stream_name", cs->StreamName );
      sprintf(stmp, "%d", cs->TrackID);
      NI_set_attribute(nel,"Tracking_ID", stmp);
      if (NI_write_element( cs->NimlStream , 
                            nel, cs->comm_NI_mode ) < 0) {
         fprintf (stderr,"Failed to start tracking.\nContinuing...\n");
      } 
      if (nel) NI_free_element(nel); nel = NULL;
      
      /* start the workprocess for this program  */
      fprintf(stderr,"Here you can register the workprocess for listening\n"
                     "with your program's main loop. See the comments in \n"
                     "the main() function of this program regarding function\n"
                     "InstaTract_niml_workproc()\n\n");
      ++i_in;
      return(1);
   }
   
   if (action == 1) { /* action == 1,  send data mode */
      if (!i_in) {
         fprintf (stderr,"You must call SUMA_SendToSuma with action 0 "
                     "before action 1.\nNo Communcation cleanup done.\n");
         cs->Send = 0;
         return(0);
      }
      if (!ngru) {
         fprintf (stderr,"Nothing to send!\n");
         return(0);
      }
      /* make sure stream is still OK */
      if (NI_stream_goodcheck ( cs->NimlStream , 1 ) < 0) {
         cs->GoneBad = 1;
         fprintf(stdout,"Communication stream gone bad.\n"
                      "Shutting down communication.\n");
         cs->Send = 0;
         return(1);  /* returning without error since program should continue */
      }

      /* add tracking (not that important)*/
      ++cs->TrackID;
      sprintf(stmp,"%d", cs->TrackID);
      NI_set_attribute (ngru, "Tracking_ID", stmp);
         
      
      #if NIML_DEBUG /* writes every element to a 
                        text file for debugging ... */
      {
         NI_stream ns;  
         /* Test writing results in asc, 1D format */ 
         if (LocalHead) fprintf(stderr," %s:-\nWriting ascii 1D ...\n"
                        , FuncName);
         /* open the stream */
         sprintf(stmp, "file:niml_dbg_asc_TID_%d_.1D",cs->TrackID);
         ns = NI_stream_open( stmp , "w" ) ;
         if( ns == NULL ){
           fprintf (stderr,"Error  %s:\nCan't open Test_write_asc_1D!\n"
                        , FuncName); 
            return(0);
         }

         /* write out the element */
         if (NI_write_element( ns , ngru ,
                               NI_TEXT_MODE | NI_HEADERSHARP_FLAG ) < 0) {
            fprintf (stderr,"Error  %s:\nFailed in NI_write_element\n"
                           , FuncName);
            return(0);
         }

         /* close the stream */
         NI_stream_close( ns ) ;
      }
      #endif

      if (cs->nelps > 0) { /* make sure that you are not sending 
                              elements too fast */
         if (!etm) {
            etm = 100000.0; /* first pass, an eternity */
            if (LocalHead) 
               fprintf (stdout,"%s: Initializing timer\n", FuncName);
            etime(&tt, 0);
         }
         else {
            if (LocalHead) 
               fprintf (stdout,"%s: Calculating etm\n", FuncName);
            etm = etime(&tt, 1);
         }
         wtm = 1./cs->nelps - etm;
         if (wtm > 0) { /* wait */
            if (LocalHead) 
               fprintf (stdout, 
                        "%s: Sleeping by %f to meet refresh rate...\n", 
                        FuncName, wtm);
            NI_sleep((int)(wtm*1000));
         }
      }

      /* send it to SUMA */
      if (LocalHead) 
         fprintf (stdout,"Sending element %d comm_NI_mode = %d...\n", 
                              cs->TrackID, cs->comm_NI_mode);
      if (NI_write_element(   cs->NimlStream , ngru, 
                              cs->comm_NI_mode ) < 0) {
         fprintf(stderr,"Failed updating SUMA...\n");
      }
      
      if (LocalHead) {
         if (cs->nelps > 0) 
            fprintf (stdout,
                     "        element %d sent (%f sec)\n", 
                     cs->TrackID, etime(&tt, 1));
         else fprintf (stdout,"        element %d sent \n", cs->TrackID);
      }
      
      if (cs->nelps > 0) {
         if (LocalHead) 
            fprintf (stdout,"%s: Resetting time...\n", FuncName);
         etime(&tt, 0); /* start the timer */
      }
      ++i_in;
      return(1);
   }/* action == 1 */
   
   if (action == 2) {
      NI_element *nel=NULL;
      if (i_in < 2) {
         fprintf (stderr,"You must call SUMA_SendToSuma with action 0 and 1"
                     " before action 2.\nNo Communcation cleanup done.\n");
         cs->Send = 0;
         return(0);
      }
      /* reset static variables */
         i_in = 0;
         etm = 0.0;
         
      /* close the stream*/
      if (cs->Send && !cs->GoneBad) { 
         /* stop tracking */
         nel = NI_new_data_element("StopTracking", 0);
         NI_set_attribute(nel,"ni_stream_name", cs->StreamName);

         if (NI_write_element( cs->NimlStream , nel, 
                               cs->comm_NI_mode ) < 0) {
            fprintf (stderr,"Failed to stop tracking.\nContinuing...\n");
         } 
         if (nel) NI_free_element(nel); nel = NULL;

         /* tell suma you're done with that stream */
         nel = NI_new_data_element("CloseKillStream",0);
         if (!nel) {
            fprintf (stderr,"Failed to create nel");
            return(0);
         }

         NI_set_attribute (nel, "ni_stream_name", cs->StreamName);
         if (NI_write_element( cs->NimlStream , nel, 
                               cs->comm_NI_mode ) < 0) {
                        fprintf (stderr,"Failed updating SUMA...\n");
         }
         if (nel) NI_free_element(nel) ; nel = NULL;


         /* now wait till stream goes bad */
         Wait_Till_Stream_Goes_Bad(cs, 1000, 5000, 1);
          
         NI_stream_close(cs->NimlStream);
         cs->NimlStream = NULL;
         cs->NimlStream_flag = 0;
         cs->TrackingID = 0;
         cs->Send = 0;
         cs->GoneBad = 0;
         cs->nelps = -1.0;
         cs->TrackID = 0;
      }
   
      return(1);
   }

   /* should not get here */
   fprintf (stderr,"Flow error.\nThis should not be\n");
   return(0);
}

/*-----------------------------------------------------------------------*/
/* NIML workprocess.
    - Read and process any new data from open connection.
   
   The void * pointer thereiselvis is expected to point to a properly
   initialized communication structure.
   
  (If the return is True, that means don't call this workproc again.
   If the return is False, that means call this workproc again.......)
-------------------------------------------------------------------------*/
int InstaTract_niml_workproc( void *thereiselvis )
{
   static char FuncName[]={"InstaTract_niml_workproc"};
   int  nn, id;
   void *nini ;
   static int nwarn=0;
   char tmpcom[100], *nel_track;
   NI_element *nel ;
   int LocalHead = 0;
   COMM_STRUCT *cs=NULL;
   
   cs = (COMM_STRUCT *)thereiselvis;
      
  /* check if stream has gone bad */
  nn = NI_stream_goodcheck( cs->NimlStream , 1 ) ;

  if( nn < 0 ){
      /* first check in case we are dealing with the 
      intermittent AFNI disruption */
      if (1) {
         NI_stream_close( cs->NimlStream ) ;
         cs->NimlStream = NULL ;
         /* try again, a way to get around the weird disruption in 
            SHM connection*/
         fprintf(stderr,"Attempting recovery...\n");
         cs->Connected = 0;
         /* Retry calling function */ 
         if (!SendToSuma(cs, NULL, 0)) { /* buzz SUMA */
            fprintf (stderr,"Failed to re-initiate call suma\n");
         }
         nn = NI_stream_goodcheck( cs->NimlStream , 1 ) ;
      } else {
         fprintf(stderr,
                 "SUMA connection stream gone bad.\n" );
      }
  }

  if( nn < 0 ){                          /* is bad */
    if (cs->NimlStream) NI_stream_close( cs->NimlStream ) ;
    cs->NimlStream = NULL ; /* this will get checked next time */
    fprintf(stderr,"Stream gone bad. Stream closed. \n");
  } else if (nn == 0) { /* waiting, come back later */
    fprintf(stderr,"Waiting on stream\n");  
  } else {
   /* if here, stream is good;
      see if there is any data to be read */
      if (cs->NimlStream_flag & SUMA_FLAG_WAITING) {
         cs->NimlStream_flag = SUMA_FLAG_CONNECTED;
         fprintf(stderr, 
               "++ NIML connection opened from %s on port %d \n",
               NI_stream_name(cs->NimlStream), cs->TCP_port) ;
      }
      nn = NI_stream_hasinput( cs->NimlStream , 1 ) ;

      if( nn > 0 ){                                   /* has data */
         int ct = NI_clock_time() ;
         if (LocalHead)   
            fprintf(stderr,"%s: reading data stream", FuncName) ;

         nini = NI_read_element( cs->NimlStream , 1 ) ;  /* read it */

         if (LocalHead)   
            fprintf( stderr,
                     " time=%d ms\n",
                     NI_clock_time()-ct) ; ct = NI_clock_time() ;

         if( nini != NULL ) {
            nel = (NI_element *)nini ;

            if (LocalHead)   {
               fprintf( stderr,
                     "%s:     name=%s vec_len=%d vec_filled=%d, vec_num=%d\n", 
                     FuncName, nel->name, nel->vec_len, 
                     nel->vec_filled, nel->vec_num );
            }      
            if (!InstaTract_process_NIML_data( nini, cs )) {
               fprintf(stderr,
                  "Error %s: Failed in SUMA_process_NIML_data.\n", FuncName);
            }
         }

         NI_free_element( nini ) ;

         if (LocalHead)   
            fprintf(stderr,"processing time=%d ms\n",NI_clock_time()-ct) ;

      }
   } 
   
   /* Return flag for function calling the workprocess */
   if (!cs->NimlStream) {
      return(1); /* Don't call workprocess back */ 
   } else {
      return (0); /* Call workprocess back */
   }
}

/* This function should probably live somewhere under ptaylor/ */
#define RR(t) (lrand48() % t)
NI_group *MiniProbTrack(NI_element *nini)
{
   int i=0;
   TAYLOR_TRACT *tt=NULL;
   TAYLOR_BUNDLE *tb=NULL;
   TAYLOR_NETWORK *net=NULL;
   NI_group *netngr=NULL;
   float x[3]={-40, 38, 4.7};
   float y[3]={-27, -34, 14.4};
   float z[3]={26, 41, 55};
         
         /* Create a toy network */
            /* Create some dummy bundles representing edge 1-2=5, or 2-1=7*/
               tb = NULL; net = NULL; tt = NULL;
               tt = (TAYLOR_TRACT *)calloc(1, sizeof(TAYLOR_TRACT));
               tt->id=77; tt->N_pts3=12; 
               tt->pts = (float *)calloc(tt->N_pts3,sizeof(float));
               tt->pts[0]=x[1]; tt->pts[1]=y[1]; tt->pts[2]=z[1];
               tt->pts[3]=22;   tt->pts[4]=36;   tt->pts[5]=40;
               tt->pts[6]=22;   tt->pts[7]=33;   tt->pts[8]=49;
               tt->pts[9]=x[2]; tt->pts[10]=y[2];tt->pts[11]=z[2];
               for (i=0; i<12; ++i) tt->pts[i] != RR(4);
               tb = AppCreateBundle(tb, 1, tt);
               tt = Free_Tracts(tt, 1);
               /* put another track in */
               tt = (TAYLOR_TRACT *)calloc(1, sizeof(TAYLOR_TRACT));
               tt->id=78; tt->N_pts3=12; 
               tt->pts = (float *)calloc(tt->N_pts3,sizeof(float));
               tt->pts[0]=x[1]; tt->pts[1]=y[1]; tt->pts[2]=z[1];
               tt->pts[3]=23;   tt->pts[4]=35;   tt->pts[5]=42;
               tt->pts[6]=20;   tt->pts[7]=32;   tt->pts[8]=51;
               tt->pts[9]=x[2]; tt->pts[10]=y[2];tt->pts[11]=z[2];
               for (i=0; i<12; ++i) tt->pts[i] != RR(4);
               tb = AppCreateBundle(tb, 1, tt);
               tt = Free_Tracts(tt, 1);
               /* add it to network */
               net = AppAddBundleToNetwork(net, &tb, 5, 7, NULL);
               /* make another one for edge 0-1=1 and 1-0=3*/
               tt = (TAYLOR_TRACT *)calloc(1, sizeof(TAYLOR_TRACT));
               tt->id=77; tt->N_pts3=15; 
               tt->pts = (float *)calloc(tt->N_pts3,sizeof(float));
               tt->pts[0]=x[0]; tt->pts[1]=y[0];  tt->pts[2]=z[0];
               tt->pts[3]=5;    tt->pts[4]=12;    tt->pts[5]=17;
               tt->pts[6]=16;   tt->pts[7]=13;    tt->pts[8]=12;
               tt->pts[9]=20;   tt->pts[10]=16;   tt->pts[11]=16;
               tt->pts[12]=x[1];tt->pts[13]=y[1]; tt->pts[14]=z[1];
               for (i=0; i<12; ++i) tt->pts[i] != RR(4);
               tb = AppCreateBundle(tb, 1, tt);
               tt = Free_Tracts(tt, 1);
               /* add bundle to network */
               net = AppAddBundleToNetwork(net, &tb, 1, 3, NULL);
               
               /* Now turn network into a transmittable thing */
               netngr = Network_2_NIgr(net, 1);
               
               /* SUMA_ShowNel(netngr); */
                 
   return(netngr);
}

/* Process an element received from SUMA
   In this example, all we do is write 
   the element to stdout.             
   Look at the NIML API documentation to 
   learn how to unpack the data from a NIML element 
   http://afni.nimh.nih.gov/afni/doc/misc/NIML_documentation/NIML_manual/document_view
*/
int InstaTract_process_NIML_data(NI_element *nini, COMM_STRUCT *cs)
{
   static int iel = 0, iout=0;
   NI_group *ngr = NULL;
   fprintf(stderr,"Received from SUMA element #%d:\n", ++iel);
   show_niml((void*)nini);
   
   /* Now pretend you're doing something with nini and 
      send something back when done */
   fprintf(stderr,"Quiet, now working hard....\n"); NI_sleep(300);
   fprintf(stderr,"OK, GOT IT!!!\n");
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "EngineCommand");/* DriveSuma's element to boss SUMA */
   NI_set_attribute(ngr, "Command", "viewer_cont"); /* like -com view_cont */
   NI_set_attribute(ngr,"N_Key","1");              /* like -key option */
   NI_set_attribute(ngr,"Key_0","left");
   NI_set_attribute(ngr,"Key_rep_0","4");
   NI_set_attribute(ngr,"Key_pause_0","0.1");
   NI_set_attribute(ngr,"Key_redis_0","1");
   if (!SendToSuma(cs, ngr, 1)) {
      fprintf (stderr,"Failed to send item %d\n", iout);
   } else ++iout;
   NI_free(ngr); ngr=NULL; /* Done with this element, free it */

   /* A more substantial returned thing would be to return the result 
      from MiniProbTrack() */
   if ((ngr = MiniProbTrack(nini))) {
      if (!SendToSuma(cs, ngr, 1)) {
         fprintf (stderr,"Failed to send item %d\n", iout);
      } else ++iout;
      NI_free(ngr); ngr=NULL; /* Done with this element, free it */
   } else {
      fprintf (stderr,"Failed to miniprobtrackate\n");
   }
   return(1);
}

void InstaTract_usage(int detail) 
{
   printf(
"A program to illustrate how to communicate with SUMA\n"
"  with the help of AFNI's NIML API. Both the NIML API and this\n"
"  program are independent of the rest of AFNI/SUMA libraries and \n"
"  can be compiled with C or C++ compilers.\n"
"\n" 
"\n"
"  Example:\n"
"     Run the following two commands, preferably from different shells.\n"
"     suma -npb 0 -niml &\n"
"     InstaTract\n"
"\n" 
            );
   return;
}

int main( int argc , char *argv[] )
{
   COMM_STRUCT *cs = NULL;
   NI_group *ngr = NULL;
   char sss[256]={""};
   int i, nn;
   
   /* mini parsing of command line */
   nn = 1;
   while (nn < argc && argv[nn][0] == '-') {
      if (!strcmp(argv[nn], "-help") || !strcmp(argv[nn], "-h")) {
         InstaTract_usage(strlen(argv[nn]) > 3 ? 2:1);
         exit(0);
      }
      ++nn;
   }
   
   /* Initialize the communications structure */
   cs = NewCommStruct(NULL, -1);
   
   /*------------------------------------------------------*/
   /******************** Ruf SUMA an ***********************/
   /*------------------------------------------------------*/
   if (!SendToSuma(cs, NULL, 0)) { /* buzz SUMA */
      fprintf (stderr,"Failed to initiate call suma\n");
      exit(1);
   }
   
   /* Next comes two sections illustrating how to send, and how
      to receive. These two operations occur asynchronously in
      an interactive program. Sending is usually triggered by 
      a user event in the developer's program (exemplified by InstaTract).
      Receiving can happen any time SUMA sends something, so the
      function that checks for incoming elements should be added to
      the developer's program workprocess functions.                   */
   
   
   /*------------------------------------------------------*/
   /***************** Sending Example **********************/
   /*------------------------------------------------------*/
   /* This next block sends a bunch of commands to SUMA asking it 
      to jump to particular nodes. 
      Each command consists of a NIML element that is formatted a la DriveSuma. 
      I adopted this approach because DriveSuma can then serve as an
      example for how to format a large number of commands.
      For example, to see the element that DriveSuma sends to SUMA when 
      asking it to jump node 28, you can run:
         DriveSuma -echo_nel_stdout -com viewer_cont '-key:v28' j
      
      You need not send anthing at this stage, but I left these here for
      illustration. More useful back and forth is in function
            InstaTract_process_NIML_data()
   */ 
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "EngineCommand");/* DriveSuma's element to boss SUMA */
   NI_set_attribute(ngr, "Command", "viewer_cont"); /* like -com view_cont */
   NI_set_attribute(ngr,"N_Key","1");              /* like -key option */
   NI_set_attribute(ngr,"Key_0","j");
   NI_set_attribute(ngr,"Key_rep_0","1");
   NI_set_attribute(ngr,"Key_pause_0","0");
   NI_set_attribute(ngr,"Key_redis_0","1");

   for (i=0; i<10; ++i) {  /* Send a bunch of calls to SUMA, reusing element*/
      /* Make SUMA jump somewhere to nodes i*/
      sprintf(sss,"v%d",i);
      NI_set_attribute(ngr,"Key_strval_0",sss);
      if (!SendToSuma(cs, ngr, 1)) {
         fprintf (stderr,"Failed to send item %d\n", i);
      }
   }
   NI_free(ngr); ngr=NULL; /* Done with this element, free it */
   
   /*------------------------------------------------------*/
   /*********** Receiving (and sending) Example ************/
   /*------------------------------------------------------*/
   /* Here we'll go into a listening loop that will only end 
      when the connection is broken.
      Normally, InstaTract_niml_workproc() is the kind of function that
      gets registered with an application's main loop which would
      take care of calling InstaTract_niml_workproc() in each application 
      loop cycle.
      For the sake of simplicity, we'll do it the simple way here */
   while (!InstaTract_niml_workproc((void *)cs)) {
      NI_sleep(100); /* 100 msec nap - not needed in real worprocess*/   
   }
   
   
   /*------------------------------------------------------*/
   /************ Be nice and hang up nicely ****************/
   /*------------------------------------------------------*/
   if (!SendToSuma(cs, NULL, 2)) { /* terminate call to SUMA */
      fprintf (stderr,"Failed to close call suma\n");
      exit(1);
   }
   
   free(cs); cs = NULL;
   
   exit(0);
}

