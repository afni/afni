/* thd_selenium.c 
   use Google's selenium webdriver library from Python to open and drive webpages
   Allows opening webpages in the same tab of a browser as opposed to calls
   to open browser applications through system calls which generate new tabs or windows
   for each webpage request
*/

/* to compile, add -lpython2.7  to program compile option with $(LPYTHON) below and include 
   IPYTHON   = -I/System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7 \
     -I/System/Library/Frameworks/Python.framework/Versions/2.7/include/python2.7
   LPYTHON   = -lpython2.7
   IFLAGS = ... -I$(IPYTHON)
 */

#include "mrilib.h"

#ifdef SELENIUM_READY
#include <Python.h>

// extern char *THD_abindir(char withslash);
// extern char * GetAfniWebBrowser(void);

/* variable to monitor level of startup
  -1 not started
  0 closed
  1 selenium started
  2 browser started
  3 webpage has been opened
*/
static int selenium_started = -1;

/* initialize selenium */
int selenium_init()
{
   char *temppath=NULL;

   /* look for python scripts in same place as afni binary */
//   Py_SetProgramName(argv[0]); /* could set to afni/whereami/suma/... here */
   Py_Initialize();
   PyObject *sys = PyImport_ImportModule("sys");
   PyObject *path = PyObject_GetAttrString(sys, "path");

   PyRun_SimpleString("print 'Initialized Python from AFNI'");

   PyRun_SimpleString("import sys, os");

   temppath =  THD_abindir(0);
// printf("afni path is %s\n", temppath);

   PyList_Append(path, PyString_FromString(temppath));
//   PyRun_SimpleString("sys.path.extend(['/Users/dglen/abin'])");

   PyRun_SimpleString("import afni_util as U");
   PyRun_SimpleString("from selenium import webdriver");
   PyRun_SimpleString("from selenium.webdriver.common.keys import Keys");
   selenium_started = 1;
  return(0);
}

/* open selenium webdriver browser, not necessarily to a specific webpage */
int selenium_open_browser()
{
   static char *webb=NULL ; static int first=1 ;

   if( first == 1 ){ webb = GetAfniWebBrowser() ; first = 2 ; }

   if(selenium_started<=0)
      selenium_init();


   if((webb==NULL)||(strcasestr(webb,"Chrome"))||(strcasecmp(webb, "open")==0)){
      if(selenium_started<2) {
         printf("For Chrome, must install chromedriver binary available from.\n");      
         printf("https://sites.google.com/a/chromium.org/chromedriver\n");
         printf("Put the chromedriver binary in your path or update path to include\n");
         printf("the chromedriver binary\n");
      }
      PyRun_SimpleString("browser = webdriver.Chrome('chromedriver')"); 
   }
   else if (strcasestr(webb, "Firefox"))
      PyRun_SimpleString("browser = webdriver.Firefox()");
   else if (strcasestr(webb, "InternetExplorer"))
      PyRun_SimpleString("browser = webdriver.Ie()");
   else if (strcasestr(webb, "Opera")) /* opera's installation is broken now, and requires OperaAppiumDriver */
      PyRun_SimpleString("browser = webdriver.Opera()");
   else if (strcasestr(webb, "Safari")) {
      if(selenium_started<2) {
         printf("For Safari, must set environment variable first to location of jar file:\n");      
         printf("setenv SELENIUM_SERVER_JAR /Users/myusername/selenium_dev/selenium-server-standalone-2.44.0.jar\n");
         printf("You may also need to install Selenium extension for Safari.\n");
         printf("See https://github.com/SeleniumHQ/selenium/wiki/SafariDriver for more information\n");
      }

      PyRun_SimpleString("browser = webdriver.Safari()");
   }
   else {
      PyRun_SimpleString("browser = webdriver.Chrome('chromedriver')");
   }
   selenium_started = 2;
   return(0);
}

/* open a webpage in the current browser instance */
int selenium_open_webpage(char *webpage)
{
   char webstring[1024];

printf("selenium_started level = %d, webpage requested is\n   %s\n", selenium_started, webpage);
   /* legit webpage */
   if (!webpage || (strlen(webpage)==0))
      return(-1);
   /* first time through, start up python, selenium, browser instances */
   if(selenium_started<=2)
      selenium_open_browser();
   sprintf(webstring, "browser.get('%s')", webpage);
   PyRun_SimpleString(webstring);
   selenium_started = 3;
   return(0);
}

/* close browser, selenium and python */
int selenium_close()
{
   if(selenium_started<=0) return(0);
   PyRun_SimpleString("browser.quit()");
   Py_Finalize();
   selenium_started = 0;
   return(0);
}


/* use selenium browser instead of stand-alone browser */
int afni_uses_selenium()
{
   return(AFNI_yesenv("AFNI_SELENIUM")) ;
}

#else

int afni_uses_selenium() { return(0) ; }

int selenium_open_webpage(char *webpage) { return(0); }
int selenium_close() { return(0); }

#endif
