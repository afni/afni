/* Validate generated M0 designs through the production Xmat reader.
   Usage: test_insync_window_fixture FIXTURE_DIRECTORY */
#include "mrilib.h"
#include "thd_xmat.h"

#define CHECK(c,m) do { if(!(c)){ fprintf(stderr,"FAIL: %s\n",m); return 1; } } while(0)

int main(int argc, char **argv)
{
   char path[4096], err[1024];
   THD_xmat *full, *censored;
   int s, t, c, overlap=0;
   CHECK(argc==2,"need fixture directory");
   snprintf(path,sizeof(path),"%s/X.full.xmat.1D",argv[1]);
   full=THD_xmat_read(path,err,sizeof(err));
   CHECK(full!=NULL,err);
   CHECK(full->nrow==48 && full->nfull==48 && full->ncol==4,"full shape");
   CHECK(full->nrun==2 && full->runstart[1]==24 && full->tr==2.0f,"run/TR metadata");
   CHECK(full->nstim==2 && !strcmp(full->stim[0].label,"Vis") &&
         !strcmp(full->stim[1].label,"Aud"),"stimulus labels");
   CHECK(full->stim[0].bot==2 && full->stim[0].top==2 &&
         full->stim[1].bot==3 && full->stim[1].top==3,"single-column ranges");
   for(t=0;t<48;t++){
      int rt=t%24;
      float vis=(rt>=2 && rt<=9)||(rt>=14 && rt<=21);
      float aud=(rt>=7 && rt<=12)||(rt>=18 && rt<=23);
      CHECK(full->good[t]==t,"full timeline identity");
      CHECK(full->mat[2*48+t]==vis && full->mat[3*48+t]==aud,"known support");
      if(vis && aud) overlap++;
   }
   CHECK(overlap==14,"seven shared TRs in each run");
   for(s=0;s<6;s++){
      snprintf(path,sizeof(path),"%s/s%02d.X.xmat.1D",argv[1],s+1);
      censored=THD_xmat_read(path,err,sizeof(err));
      CHECK(censored!=NULL,err);
      CHECK(censored->nrow==46 && censored->nfull==48,"censored shape");
      for(t=0;t<46;t++){
         int original=censored->good[t];
         CHECK(original!=4+s && original!=28+s,"subject censor exclusions");
         for(c=0;c<4;c++)
            CHECK(censored->mat[c*46+t]==full->mat[c*48+original],"retained design rows");
      }
      THD_xmat_free(censored);
   }
   THD_xmat_free(full);
   puts("PASS: M0 generated Xmat fixtures");
   return 0;
}
