#include "niml.h"
#include "SUMA_niml_defines.h"

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
   float nelps;  /*!<   number of NI elements to send per second 
                        -1 for going as fast as possible */
   int TrackID;            /*!<  ID of next element to be sent 
                                 NI_element StartTracking has an ID of 1 */
   int GoneBad;   /*!< Flag indicating that stream went bad */
   int Send;      /*!< Flag indicating that elements should be sent 
                                As long as GoneBad is 0 */
} COMM_STRUCT;


COMM_STRUCT * NewCommStruct(char *Hostname, int port) {
   COMM_STRUCT *cs=NULL;
   cs = (COMM_STRUCT *)calloc(1,sizeof(COMM_STRUCT));
   
   if (Hostname) sprintf(cs->Hostname,"%s",Hostname);
   else sprintf(cs->Hostname,"127.0.0.1");
   
   
   if (port < 0)  cs->TCP_port = 1046;
   else cs->TCP_port = port;
   
   sprintf(cs->StreamName,"tcp:%s:%d", cs->Hostname, cs->TCP_port);

   return(cs);
}




/*!
   \brief Initiates a call on stream NimlStream
   
   \return 0: Sucked
           YUP : Did not suck
   
   \sa niml_hangup
*/
int niml_call (COMM_STRUCT *cs)
{
   static char FuncName[]={"niml_call"};
   int nn=-1, Wait_tot;
   int LocalHead = 0;
   
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
            return(0) ; 
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
   
int SendToSuma (COMM_STRUCT *cs, 
                NI_group *ngru, int action)
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
      fprintf(stderr,"You need to register a workprocess at this stage\n"
                     "Otherwise there is no listening mode\n");
      ++i_in;
      return(0);
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


int main( int argc , char *argv[] )
{
   COMM_STRUCT *cs = NULL;
   NI_group *ngr = NULL;
   char *sss=NULL;
   int i;
   
   cs = NewCommStruct(NULL, -1);   /* init communication structure */
   
   if (!SendToSuma(cs, NULL, 0)) { /* buzz SUMA */
      fprintf (stderr,"Failed to initiate call suma\n");
      exit(1);
   }
   
   ngr = NI_new_group_element();
   
   NI_rename_group(ngr, "EngineCommand"); /* This is pretty much how DriveSuma 
                                             does it */
   
   NI_set_attribute(ngr, "Command", "viewer_cont"); /* like -com view_cont */

   
   NI_set_attribute(ngr,"N_Key","1");              /* like -key option */
   NI_set_attribute(ngr,"Key_0","j");
   NI_set_attribute(ngr,"Key_rep_0","1");
   NI_set_attribute(ngr,"Key_pause_0","0");
   NI_set_attribute(ngr,"Key_redis_0","1");

   for (i=0; i<10; ++i) {  /* Send some call to SUMA */
      /* Make SUMA jump somewhere to nodes i*/
      sprintf(sss,"%d",i);
      NI_set_attribute(ngr,"Key_strval_0",sss);
      if (!SendToSuma(cs, ngr, 2)) {
         fprintf (stderr,"Failed to send item %d\n", i);
      }
   }
   
   NI_free(ngr); ngr=NULL;
   
   if (!SendToSuma(cs, NULL, 2)) { /* terminate call to SUMA */
      fprintf (stderr,"Failed to close call suma\n");
      exit(1);
   }
   
   free(cs); cs = NULL;
   exit(0);
}
