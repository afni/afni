/* This is the top-level AFNI/GIFTI interface, defining THD_read/write_gifti,
 * as well as going to/from NIML for SUMA.
 *
 * If HAVE_GIFTI is not set, these functions will just return failure.
 *
 * Add this object to libmri.
 * Do not add thd_gifti.o or anything under the gifti directory.
 */ 


#ifdef HAVE_GIFTI

    /* pretend the gifti source is here, along with thd_gifti functions */
    
    #include "mcw_malloc.h"     /* Need this to use same allocation functions */
    
    #include "gifti_io.c"       /* library */
    #include "gifti_xml.c"

    #include "thd_gifti.c"      /* afni interface */
    #include "suma_gifti.c"     /* suma interface */
#else

    /* if we do not love or even want GIFTI, include failure functions here */
    /* (these should be the same functions exported from thd_gifti.c)       */

    #include "mrilib.h"
    #include "suma_afni_surface.h"
    /* ------------------------------- AFNI ------------------------------- */

    THD_3dim_dataset * THD_open_gifti(char * fname)
    {
        fprintf(stderr,"** cannot open '%s', no compiled GIFTI support\n",
                fname ? fname : "NULL");
        return NULL;
    }

    /* presumably we've already whined, via 'open' */
    int THD_load_gifti(THD_datablock * dblk){ return 1; }

    Boolean THD_write_gifti(  THD_3dim_dataset * dset, int write_data, 
                              int forcencode)
    {
        char * prefix;

        ENTRY("THD_write_gifti");

        prefix = DSET_PREFIX(dset);
        fprintf(stderr,"** cannot write '%s', no compiled GIFTI support\n",
                prefix);
        RETURN(False);
    }


    /* ------------------------------- NIML ------------------------------- */

    NI_group * NI_read_gifti(char * fname, int read_data)
    {
        fprintf(stderr,"** cannot read '%s', no compiled GIFTI support\n",
                fname ? fname : "NULL");
        return NULL;
    }

    int NI_write_gifti(NI_group * ngr, char * fname, int forcencode)
    {
        fprintf(stderr,"** cannot write '%s', no compiled GIFTI support\n",
                fname ? fname : "NULL");
        return 1;
    }

    /* ------------------------------- SUMA ------------------------------- */
    
    NI_group * afni_open_gifti_surf(char * fname, int read_data)
    {
        fprintf(stderr,"** cannot read '%s', no compiled GIFTI support\n",
                fname ? fname : "NULL");
        return NULL;
    }
    int afni_write_gifti_surf( NI_group *aSO, char * fname, 
                              int write_data, int encoding)
    {
        fprintf(stderr,"** cannot write '%s', no compiled GIFTI support\n",
                fname ? fname : "NULL");
        return NULL;
    }


#endif  /* HAVE_GIFTI */
