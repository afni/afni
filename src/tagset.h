/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _THD_TAGLIST_HEADER_
#define _THD_TAGLIST_HEADER_

/*-----------------------------------------------------------------------
   October 1998: User defined "tags" to be placed into a dataset.
-------------------------------------------------------------------------*/

#define MAX_TAG_LABEL  40
#define MAX_TAG_NUM   100

typedef struct {
   int   set , ti ;
   float x,y,z, val ;
   char label[MAX_TAG_LABEL] ;
} THD_usertag ;

typedef struct {
   int num ;
   char label[MAX_TAG_LABEL] ;
   THD_usertag tag[MAX_TAG_NUM] ;
} THD_usertaglist ;

#define INIT_TAGLIST(tl)                     \
  do{ int iq ;                               \
      for( iq=0 ; iq < MAX_TAG_NUM ; iq++ ){ \
         (tl).tag[ii].set = 0 ;              \
         (tl).tag[ii].label[0] = '\0' ;      \
      } } while(0)

#define TAGLIST_COUNT(tl)    ((tl)->num)
#define TAGLIST_SUBTAG(tl,i) ((tl)->tag[i])
#define TAGLIST_LABEL(tl)    ((tl)->label)

#define TAG_LABEL(tt)        ((tt).label)
#define TAG_X(tt)            ((tt).x)
#define TAG_Y(tt)            ((tt).y)
#define TAG_Z(tt)            ((tt).z)
#define TAG_T(tt)            ((tt).t)
#define TAG_VAL(tt)          ((tt).val)
#define TAG_SET(tt)          ((tt).set)

#define TAG_SETLABEL(tt,str)                     \
   ( strncpy((tt).label,(str),MAX_TAG_LABEL-1) , \
     (tt).label[MAX_TAG_LABEL-1] = '\0' )

#define TAGLIST_SETLABEL(tl,str)  TAG_SETLABEL(*(tl),(str))

#define ATRNAME_TAGSET_NUM     "TAGSET_NUM"
#define ATRNAME_TAGSET_LABELS  "TAGSET_LABELS"
#define ATRNAME_TAGSET_FLOATS  "TAGSET_FLOATS"

#endif
