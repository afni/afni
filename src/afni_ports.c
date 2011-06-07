#include <stdio.h>
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

int set_user_np(int v) {
   user_np = 0;
   if ( v >= 1024 && v<=65535) {
      user_np = v;
      return(user_np);
   } 
   return(0);
}
int get_user_np(void) {
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
         if (user_np) {
            WARNING_message("Cannot setenv AFNI_NIML_FIRST_PORT and use -np\n"
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
      if (user_np) np = user_np;
      else {
         np = 53211; /* old default */
      }
   }
   
   /* Keep order of ports "AFNI_SUMA_NIML" and "AFNI_DEFAULT_LISTEN_NIML"
      in list below */
   ip = 0;
   if ((cc = AFNI_numenv("SUMA_AFNI_TCP_PORT"))) {
      if (user_np) { /* -np override */
         if (cc != 53211) { /* whine only if users changed default */
            WARNING_message("ENV SUMA_AFNI_TCP_PORT superseded by -np option\n"
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
            WARNING_message("ENV SUMA_AFNI_TCP_PORT2 superseded by -np option\n"
                      "Set SUMA_AFNI_TCP_PORT2 to 0 in your .sumarc file\n"
                      "to avoid this message");
         }
         cc = np+ip;
      }  
   } else {
      cc = np+ip;
   }
   PL.port_id[ip].port = cc; /* Used to be 53212 by default */
   sprintf(PL.port_id[ip].name,"AFNI_DEFAULT_LISTEN_NIML"); /* generic listener */
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   
   /* add new ports below */
   PL.port_id[ip].port = np+ip; /* Used to be 53212 by default */
   sprintf(PL.port_id[ip].name,"AFNI_GroupInCorr_NIML");
   sprintf(PL.port_id[ip].listener,"AFNI");
      ++ip;
   
   PL.port_id[ip].port = np+ip; /* Used to be 53220 by default */
   sprintf(PL.port_id[ip].name,"SUMA_DEFAULT_LISTEN_NIML"); /* generic listener */
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
               "ENV SUMA_MATLAB_LISTEN_PORT superseded by -np option\n"
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
   char cunegonde[64];
   
   init_ports_list(0,0); /* init, no harm if init done already */

   if (PL.n_ports < 1 || PL.n_ports > 100) {
      ERROR_message("Bad init.\n");
      return(0);
   }
   for (ip=0; ip<PL.n_ports; ++ip) {
      if (PL.port_id[ip].port==port) return(PL.port_id[ip].name);
   }
   
   sprintf(cunegonde,
         "Port numbered %d not in standard list of %d ports.\n",
         port, PL.n_ports);
   return(cunegonde);
}

void show_ports_list(void) {
   int ip = 0;
   
   init_ports_list(0,0); /* init, no harm if init done already */

   for (ip=0; ip<PL.n_ports; ++ip) {
      fprintf(stdout,"%d: %s has port %d\n", 
               ip, PL.port_id[ip].name, PL.port_id[ip].port);
   }
   return;
}
