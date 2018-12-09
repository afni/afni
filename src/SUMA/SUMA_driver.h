#ifndef SUMA_DRIVER_INCLUDED
#define SUMA_DRIVER_INCLUDED

int SUMA_ProcessCommand(char *com, SUMA_COMM_STRUCT *cs, char *EchoNel);
NI_group *SUMA_ComToNgr(char *com, char *command);
SUMA_SurfaceObject *SUMA_NodeXYZComToSO(char *com);
SUMA_SurfaceObject *SUMA_ShowSurfComToSO(char *com);
char ** SUMA_com2argv(char *com, int *argtcp);
char ** SUMA_free_com_argv(char **argt, int *argtc);
int SUMA_DriveSuma_ParseCommon(NI_group *ngr, int argtc, char ** argt);
SUMA_Boolean SUMA_ParseKeyModifiers(char *keyopt, int *Key_mult,
                                    float *Key_pause, int *Key_redis,
                                    char **strgvalp);
SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_DriveSuma_ParseInput(
                  char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps);
void usage_DriveSuma (SUMA_GENERIC_ARGV_PARSE *ps, int detail);



#endif
