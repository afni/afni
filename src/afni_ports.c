#include "mrilib.h"
#include <stdio.h>
#include <string.h>
#include "afni_environ.h"
#include <stdlib.h>

/*************************************************************************/
/************** Functions to handle port assignments *********************/
/*************************************************************************/

#define MAX_PORTS 64

typedef struct {
   int port;
   char name[64];
   char listener[64];
} PORT_ID;

typedef struct {
  PORT_ID port_id[MAX_PORTS];
  int n_ports;  
} PORTS;

static PORTS PL;

static int user_np = -1;
static int reinit = 1;
static char *user_pif = NULL;

void set_ports_list_reinit (void) { reinit = 1; }

char *get_np_help() {
   static char NP_HELP[] = {
"   -np PORT_OFFSET: Provide a port offset to allow multiple instances of\n"
"                    AFNI <--> SUMA, AFNI <--> 3dGroupIncorr, or any other\n"
"                    programs that communicate together to operate on the same\n"
"                    machine. \n"
"                    All ports are assigned numbers relative to PORT_OFFSET.\n"
"         The same PORT_OFFSET value must be used on all programs\n"
"           that are to talk together. PORT_OFFSET is an integer in\n"
"           the inclusive range [1025 to 65500]. \n"
"         When you want to use multiple instances of communicating programs, \n"
"           be sure the PORT_OFFSETS you use differ by about 50 or you may\n"
"           still have port conflicts. A BETTER approach is to use -npb below.\n"
"   -npq PORT_OFFSET: Like -np, but more quiet in the face of adversity.\n"
"   -npb PORT_OFFSET_BLOC: Similar to -np, except it is easier to use.\n"
"                          PORT_OFFSET_BLOC is an integer between 0 and\n"
"                          MAX_BLOC. MAX_BLOC is around 4000 for now, but\n"
"                          it might decrease as we use up more ports in AFNI.\n"
"                          You should be safe for the next 10 years if you \n"
"                          stay under 2000.\n"
"                          Using this function reduces your chances of causing\n"
"                          port conflicts.\n"
"\n" 
"         See also afni and suma options: -list_ports and -port_number for \n"
"            information about port number assignments.\n"
"\n"
"         You can also provide a port offset with the environment variable\n"
"            AFNI_PORT_OFFSET. Using -np overrides AFNI_PORT_OFFSET.\n"
"\n"
"   -max_port_bloc: Print the current value of MAX_BLOC and exit.\n"
"                   Remember this value can get smaller with future releases.\n"
"                   Stay under 2000.\n"
"   -max_port_bloc_quiet: Spit MAX_BLOC value only and exit.\n"
"   -num_assigned_ports: Print the number of assigned ports used by AFNI \n"
"                        then quit.\n"
"   -num_assigned_ports_quiet: Do it quietly.\n"
"\n"
"     Port Handling Examples:\n"
"     -----------------------\n"
"         Say you want to run three instances of AFNI <--> SUMA.\n"
"         For the first you just do: \n"
"            suma -niml -spec ... -sv ...  &\n"
"            afni -niml &\n"
"         Then for the second instance pick an offset bloc, say 1 and run\n"
"            suma -niml -npb 1 -spec ... -sv ...  &\n"
"            afni -niml -npb 1 &\n"
"         And for yet another instance:\n"
"            suma -niml -npb 2 -spec ... -sv ...  &\n"
"            afni -niml -npb 2 &\n"
"         etc.\n"
"\n"
"         Since you can launch many instances of communicating programs now,\n"
"            you need to know wich SUMA window, say, is talking to which AFNI.\n"
"            To sort this out, the titlebars now show the number of the bloc \n"
"            of ports they are using. When the bloc is set either via \n"
"            environment variables AFNI_PORT_OFFSET or AFNI_PORT_BLOC, or  \n"
"            with one of the -np* options, window title bars change from \n"
"            [A] to [A#] with # being the resultant bloc number.\n"
"         In the examples above, both AFNI and SUMA windows will show [A2]\n"
"            when -npb is 2.\n"
"\n"
   };
   return(NP_HELP);
}

int set_user_pif(char *s) {
   if (user_pif) {
      free(user_pif); 
   }
   user_pif = NULL;
   if (s) {
      user_pif = strdup(s);
   }
   return(1);
}

char *get_user_pif(void) {
   return(user_pif);
}

int npb_to_np(int v) {
   return(1024+v*get_num_ports());
}

int set_user_np_bloc(int v) {
   if (v > get_max_port_bloc()) {
      ERROR_message(
         "** Port bloc %d is not an integer between 0 and %d\n",
         get_max_port_bloc());
      return(0);
   }
   v = npb_to_np(v);
   return(set_user_np(v));
}

int get_user_np_bloc(void) {
   int ii = (get_user_np()-1024)/get_num_ports();
   if (ii < 0) return(-1);
   return(ii);
}

int get_max_port_bloc(void) {
   return((65535-1024)/get_num_ports()-1);
}

int set_user_np(int v) {
   int  npenv=-1;
   user_np = 0;
   
   if ( v == 0 ) return(0);   /* nothing to do. legit call. */
   
   if ( v == -1 ) { /* try initializing from environment variables */
      /* try env */
      if ((npenv = (int)AFNI_numenv_def("AFNI_PORT_BLOC", -1)) >= 0) {
         user_np = set_user_np_bloc(npenv);
      } else if ((npenv = 
                  (int)AFNI_numenv_def("AFNI_PORT_OFFSET", -1)) >= 1024) {
         user_np = set_user_np(npenv);
      }
      return(user_np);
   }
   
   if ( v >= 1024 && v<=65500) {
      user_np = v;
      reinit = 1;
      return(user_np);
   }else{
      ERROR_message("User -np, or AFNI_PORT_OFFSET environment variable\n"
                    "Outside of range 1024..65500. Have %d\n", v);
      return(0);
   }
   
   return(0);
}

int get_user_np(void) {
   static int ncall=0;
   int  npenv=-1;
   if (!ncall && user_np < 0) {
      user_np = set_user_np(-1); /* init from env. variables */
      ++ncall;
   }
   return(user_np);
}

int get_num_ports(void) {
   return(init_ports_list());
}

/* initialize port assignment given an offset np */
int init_ports_list(void) {
   int ip = 0, cc = 0, ptcpbase=0, np=0;
   static int first_call = 1;
   
   if (user_np < 0) set_user_np(-1); /* initialize */
   
     
   if (!first_call && !reinit) { /* nothing to do */
      return(PL.n_ports);
   }
   
   PL.n_ports = 0;
   
   if ((cc = AFNI_numenv("AFNI_NIML_FIRST_PORT"))) {
      if (cc >= 1025 &&  cc <= 65500) {
         if (user_np) {
            WARNING_message(
               "Cannot setenv AFNI_NIML_FIRST_PORT and use -np or \n"
               "AFNI_PORT_OFFSET environment variable.\n"
               "Ignoring AFNI_NIML_FIRST_PORT\n");
         } else {
            np = cc - 1;
         }
      } else {
         WARNING_message("AFNI_NIML_FIRST_PORT must be >= 1025 and <= 6500\n"
                        "Ingnoring value of %d\n", cc);
      }
   }
   
   
   if (user_np) np = user_np;
   else {
      np = 53211; /* old default */
   }
   
   /* Keep order of ports "AFNI_SUMA_NIML" and "AFNI_DEFAULT_LISTEN_NIML"
      in list below */
   ip = 0;
   if ((cc = AFNI_numenv("SUMA_AFNI_TCP_PORT"))) {
      if (user_np) { /* -np override */
         if (cc != 53211) { /* whine only if users changed default */
            WARNING_message(
               "ENV SUMA_AFNI_TCP_PORT superseded by -np option and\n"
               "AFNI_PORT_OFFSET environment variable.\n"
               "Set SUMA_AFNI_TCP_PORT to 0 in your .sumarc file\n"
                      "to avoid this message");
         }
         cc = np+ip;
      }  
   } else {
      cc = np+ip;
   }
   PL.port_id[ip].port = cc; /* Used to be 53211 by default */
   sprintf(PL.port_id[ip].name,"AFNI_SUMA_NIML");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;

   if ((cc = AFNI_numenv("SUMA_AFNI_TCP_PORT2"))) {
      if (user_np) { /* -np override */
         if (cc != 53212) { /* whine only if users changed default */
            WARNING_message(
               "ENV SUMA_AFNI_TCP_PORT2 superseded by -np option and \n"
               "AFNI_PORT_OFFSET environment variable.\n"
               "Set SUMA_AFNI_TCP_PORT2 to 0 in your .sumarc file\n"
               "to avoid this message");
         }
         cc = np+ip;
      }  
   } else {
      cc = np+ip;
   }
   PL.port_id[ip].port = cc; /* Used to be 53212 by default */
   sprintf(PL.port_id[ip].name,"AFNI_DEFAULT_LISTEN_NIML"); /* generic listen */
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   
   /* add new ports below */
   PL.port_id[ip].port = np+ip; /* Used to be 53212 by default */
   sprintf(PL.port_id[ip].name,"AFNI_GroupInCorr_NIML");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = np+ip; /* Used to be 53220 by default */
   sprintf(PL.port_id[ip].name,"SUMA_DEFAULT_LISTEN_NIML"); /* generic listen */
   sprintf(PL.port_id[ip].listener,"SUMA");
      ++ip;
      
   PL.port_id[ip].port = np+ip; /* Used to be 53224 by default */
   sprintf(PL.port_id[ip].name,"SUMA_GroupInCorr_NIML");
   sprintf(PL.port_id[ip].listener,"SUMA");
      ++ip;

   if ((cc = AFNI_numenv("SUMA_MATLAB_LISTEN_PORT"))) {
      if (user_np) { /* -np override */
         if (cc != 53230) { /* the old default, don't whine */
            WARNING_message(
               "ENV SUMA_MATLAB_LISTEN_PORT superseded by -np option and\n"
               "AFNI_PORT_OFFSET environment variable.\n"
               "Set SUMA_MATLAB_LISTEN_PORT to 0 in your .sumarc file\n"
               "to avoid this message");
         }
         cc = np+ip;
      }  
   } else {
      cc = np+ip;
   }
     
   PL.port_id[ip].port = cc; /* Used to be 53230 by default */
   sprintf(PL.port_id[ip].name,"MATLAB_SUMA_NIML");
   sprintf(PL.port_id[ip].listener,"MATLAB");
      ++ip;
      
   PL.port_id[ip].port = np+ip;
   sprintf(PL.port_id[ip].name,"SUMA_GEOMCOMP_NIML");
   sprintf(PL.port_id[ip].listener,"SUMA");
      ++ip;
    
   PL.port_id[ip].port = np+ip; 
   sprintf(PL.port_id[ip].name,"SUMA_BRAINWRAP_NIML");
   sprintf(PL.port_id[ip].listener,"SUMA");
      ++ip;
      
   PL.port_id[ip].port = np+ip; 
   sprintf(PL.port_id[ip].name,"SUMA_DRIVESUMA_NIML");
   sprintf(PL.port_id[ip].listener,"SUMA");
      ++ip;
      
   cc = AFNI_numenv("AFNI_PLUGOUT_TCP_BASE");
   if( cc ){
      if( cc < 1024 || cc > 65535 ){     
         fprintf(stderr,"\nPO: bad AFNI_PLUGOUT_TCP_BASE %d,"
                        " should be in [%d,%d]\n", cc, 1024, 65535);
         if (!user_np) {
            PL.port_id[ip].port = 7955; /* the old BASE_TCP_CONTROL */
         } else {
            PL.port_id[ip].port = np+ip; /* the newer default with -np */
         }
      } else { /* warn user (and use it) */
         fprintf(stderr,
            "\nPO: applying AFNI_PLUGOUT_TCP_BASE %d \n",
                 cc);
         PL.port_id[ip].port = cc;
      }
   } else {
      if (!user_np) {
         PL.port_id[ip].port = 7955; /* the old BASE_TCP_CONTROL */
      } else {
         PL.port_id[ip].port = np+ip; /* the newer default with -np */
      }   
   }
   ptcpbase = PL.port_id[ip].port;
   sprintf(PL.port_id[ip].name,"AFNI_PLUGOUT_TCP_0");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = ptcpbase+1; /* used to be 7956 */
   sprintf(PL.port_id[ip].name,"AFNI_PLUGOUT_TCP_1");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = ptcpbase+2; /* used to be 7957 */
   sprintf(PL.port_id[ip].name,"AFNI_PLUGOUT_TCP_2");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = ptcpbase+3; /* used to be 7958 */
   sprintf(PL.port_id[ip].name,"AFNI_PLUGOUT_TCP_3");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
      
   PL.port_id[ip].port = ptcpbase+4; /* used to be 7959 */
   sprintf(PL.port_id[ip].name,"AFNI_PLUGOUT_TCP_4");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = ptcpbase+5;   /* used to be 7953 */
   sprintf(PL.port_id[ip].name,"AFNI_TCP_PORT");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = ptcpbase+6;   /* used to be 7954 */
   sprintf(PL.port_id[ip].name,"AFNI_CONTROL_PORT");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   if (!user_np) PL.port_id[ip].port = 8099;  
   else PL.port_id[ip].port = np+ip;
   sprintf(PL.port_id[ip].name,"PLUGOUT_DRIVE_PORT");
   sprintf(PL.port_id[ip].listener,"PLUGOUT_DRIVE");
      ++ip;
   
   if (!user_np) PL.port_id[ip].port = 8077;  
   else PL.port_id[ip].port = np+ip;
   sprintf(PL.port_id[ip].name,"PLUGOUT_GRAPH_PORT");
   sprintf(PL.port_id[ip].listener,"PLUGOUT_GRAPH");
      ++ip;
      
   if (!user_np) PL.port_id[ip].port = 8009;  
   else PL.port_id[ip].port = np+ip;
   sprintf(PL.port_id[ip].name,"PLUGOUT_IJK_PORT");
   sprintf(PL.port_id[ip].listener,"PLUGOUT_IJK");
      ++ip;
   
   if (!user_np) PL.port_id[ip].port = 8019;  
   else PL.port_id[ip].port = np+ip;
   sprintf(PL.port_id[ip].name,"PLUGOUT_SURF_PORT");
   sprintf(PL.port_id[ip].listener,"PLUGOUT_SURF");
      ++ip;
         
   if (!user_np) PL.port_id[ip].port = 8001;  
   else PL.port_id[ip].port = np+ip;
   sprintf(PL.port_id[ip].name,"PLUGOUT_TT_PORT");
   sprintf(PL.port_id[ip].listener,"PLUGOUT_TT");
      ++ip;
   
   if (!user_np) PL.port_id[ip].port = 8005;  
   else PL.port_id[ip].port = np+ip;
   sprintf(PL.port_id[ip].name,"PLUGOUT_TTA_PORT");
   sprintf(PL.port_id[ip].listener,"PLUGOUT_TTA");
      ++ip;
    
   PL.port_id[ip].port = np+ip; 
   sprintf(PL.port_id[ip].name,"SUMA_HALLO_SUMA_NIML");
   sprintf(PL.port_id[ip].listener,"SUMA");
      ++ip;
      
   PL.port_id[ip].port = np+ip; 
   sprintf(PL.port_id[ip].name,"SUMA_INSTA_TRACT_NIML");
   sprintf(PL.port_id[ip].listener,"SUMA");
      ++ip;
      
   PL.n_ports=ip;
   
   reinit = 0;
   first_call = 0;
   return(ip);
}


int get_port_named(char *name) {
   int ip = 0;
   
   init_ports_list(); /* init, no harm if init done already */
   
   if (PL.n_ports < 1 || PL.n_ports > 100) {
      ERROR_message("Bad init.\n");
      return(0);
   }
   for (ip=0; ip<PL.n_ports; ++ip) {
      if (!strcmp(PL.port_id[ip].name,name)) return(PL.port_id[ip].port);
   }
   ERROR_message( "Port %s not found in list of %d ports.\n",
                  name, PL.n_ports);
   return(0);
}

char *get_port_numbered(int port) {
   int ip = 0;
   static char cunegonde[64];
   
   init_ports_list(); /* init, no harm if init done already */

   if (PL.n_ports < 1 || PL.n_ports > 100) {
      ERROR_message("Bad init.\n");
      return(0);
   }
   for (ip=0; ip<PL.n_ports; ++ip) {
      if (PL.port_id[ip].port==port) return(PL.port_id[ip].name);
   }
   
   if (port) sprintf(cunegonde,
         "Port numbered %d not in standard list of %d ports.\n",
         port, PL.n_ports);
   else {
      /* this happens when sending ns_listen[cc].port on a shm
         connection. No problem, don't complain */
      sprintf(cunegonde,"ZERO");
   }
   return(cunegonde);
}

void show_ports_list(void) {
   int ip = 0;
   
   init_ports_list(); /* init, no harm if init done already */
   fprintf(stdout,"\n");
   for (ip=0; ip<PL.n_ports; ++ip) {
      fprintf(stdout,"%d: %s has port %d\n", 
               ip, PL.port_id[ip].name, PL.port_id[ip].port);
   }
   return;
}

/* 
   Check if all ports in block are listenable 
*/
int is_npb_available(int npb) 
{
   int npm=0, sd = 1;
   int np = 0;
   
   np = npb_to_np(npb);
   npm = np+get_num_ports();
   set_tcp_listen_mute(1);
   sd = 1;
   while (np < npm && (sd = tcp_listen(np)) >= 0) {
      shutdown(sd,2); close(sd); /* same as CLOSEDOWN macro in niml_stream*/
      ++np; 
   }
   set_tcp_listen_mute(0);
   if (np < npm) return(0);
   return(1);
}

/* find a block of ports that is useable */
int get_available_npb(void) 
{
   int k = 0;
   
   while (k<get_max_port_bloc()) {
      if (is_npb_available(k)) return(k);
      ++k;
   }
   return(-1);
}
