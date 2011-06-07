#include <stdio.h>
#include <afni_environ.h>

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

static int user_np = 0;

char *get_np_help() {
   static char NP_HELP[] = {
"   -np PORT_OFFSET: Provide a port offset to allow multiple instances of\n"
"                    AFNI <--> SUMA, AFNI <--> 3dGroupIncorr, or any other\n"
"                    programs that communicate together to operate on the same\n"
"                    machine.\n"
"         The same PORT_OFFSET value must be used on all programs\n"
"           that are to talk together. PORT_OFFSET is an integer in\n"
"              the inclusive range [1025 to 65500]. \n"
"         For example, say you want to run two instances of AFNI <--> SUMA.\n"
"         For the first you just do: \n"
"            suma -niml -spec ... -sv ...  &\n"
"            afni -niml &\n"
"         Then for the second instance pick an offset, say 3000 and run\n"
"            suma -niml -np 3000 -spec ... -sv ...  &\n"
"            afni -niml -np 3000 &\n"
"         Each AFNI <--> SUMA instance can now communicate together although\n"
"           you may drive yourself crazy sorting out which is talking is \n"
"           which.\n"
"         If we get enough feedback we'll attempt to visually distinguish  \n"
"           communicating sets somehow."
"\n"
"         See also afni and suma options: -list_ports and -port_number for \n"
"            information about port number assignments.\n"
"\n"
"         You can also provide a port offset with the environment variable\n"
"            AFNI_PORT_OFFSET. Using -np overrides AFNI_PORT_OFFSET.\n"
"\n"
"   -npq PORT_OFFSET: Like -np, but more quiet in the face of adversity.\n"
   };
   return(NP_HELP);
}

int set_user_np(int v) {
   user_np = 0;
   
   if ( v == 0 ) return(0);   /* nothing to do. legit call. */
   
   if ( v >= 1024 && v<=65500) {
      user_np = v;
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
   if (!ncall && !user_np) {
      /* try env */
      user_np = set_user_np((int)AFNI_numenv("AFNI_PORT_OFFSET"));
      ++ncall;
   }
   return(user_np);
}

/* initialize port assignment given an offset np */
int init_ports_list(int np, int reinit) {
   int ip = 0, cc = 0, ptcpbase=0;
   static int init=0;

   if (init && !reinit) return(PL.n_ports);
   
   PL.n_ports = 0;
   
   if ((cc = AFNI_numenv("AFNI_NIML_FIRST_PORT"))) {
      if (cc >= 1025 &&  cc <= 65500) {
         if (get_user_np()) {
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
   
   
   if (np == 0) {
      if (get_user_np()) np = get_user_np();
      else {
         np = 53211; /* old default */
      }
   }
   
   /* Keep order of ports "AFNI_SUMA_NIML" and "AFNI_DEFAULT_LISTEN_NIML"
      in list below */
   ip = 0;
   if ((cc = AFNI_numenv("SUMA_AFNI_TCP_PORT"))) {
      if (get_user_np()) { /* -np override */
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
      if (get_user_np()) { /* -np override */
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
      if (get_user_np()) { /* -np override */
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
         if (!get_user_np()) {
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
      if (!get_user_np()) {
         PL.port_id[ip].port = 7955; /* the old BASE_TCP_CONTROL */
      } else {
         PL.port_id[ip].port = np+ip; /* the newer default with -np */
      }   
   }
   ptcpbase = PL.port_id[ip].port;
   sprintf(PL.port_id[ip].name,"AFNI_PLUGOUT_TCP_0");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = ptcpbase+1;
   sprintf(PL.port_id[ip].name,"AFNI_PLUGOUT_TCP_1");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = ptcpbase+2;
   sprintf(PL.port_id[ip].name,"AFNI_PLUGOUT_TCP_2");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = ptcpbase+3;
   sprintf(PL.port_id[ip].name,"AFNI_PLUGOUT_TCP_3");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
      
   PL.port_id[ip].port = ptcpbase+4;
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
   
   PL.n_ports=ip;
   
   init = 1;
   
   return(ip);
}


int get_port_named(char *name) {
   int ip = 0;
   init_ports_list(0,0); /* init, no harm if init done already */
   
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
   
   init_ports_list(0,0); /* init, no harm if init done already */

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
   
   init_ports_list(0,0); /* init, no harm if init done already */
   fprintf(stdout,"\n");
   for (ip=0; ip<PL.n_ports; ++ip) {
      fprintf(stdout,"%d: %s has port %d\n", 
               ip, PL.port_id[ip].name, PL.port_id[ip].port);
   }
   return;
}
