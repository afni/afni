/* Code in this file is taken from the book:
 "Mastering Algorithms with C"  by Kyle Loudon,  published by O'Reilly & Associates.  This
code is under copyright and cannot be included in any other book, publication,
or  educational product  without  permission  from  O'Reilly & Associates.  No
warranty is attached; we cannot take responsibility for errors or  fitness for
use.

*/

#ifndef SUMA_ALGORITHMS_INCLUDED
#define SUMA_ALGORITHMS_INCLUDED

/*****************************************************************************
*                                                                            *
*  -------------------------------- list.h --------------------------------  *
*                                                                            *
*****************************************************************************/

#ifndef LIST_H
#define LIST_H


/*****************************************************************************
*                                                                            *
*  Define a structure for linked list elements.                              *
*                                                                            *
*****************************************************************************/

typedef struct ListElmt_ {

void               *data;
struct ListElmt_   *next;

} ListElmt;

/*****************************************************************************
*                                                                            *
*  Define a structure for linked lists.                                      *
*                                                                            *
*****************************************************************************/

typedef struct List_ {

int                size;

int                (*match)(const void *key1, const void *key2);
void               (*destroy)(void *data);

ListElmt           *head;
ListElmt           *tail;

} List;

/*****************************************************************************
*                                                                            *
*  --------------------------- Public Interface ---------------------------  *
*                                                                            *
*****************************************************************************/

void list_init(List *list, void (*destroy)(void *data));

void list_destroy(List *list);

int list_ins_next(List *list, ListElmt *element, const void *data);

int list_rem_next(List *list, ListElmt *element, void **data);

#define list_size(list) ((list)->size)

#define list_head(list) ((list)->head)

#define list_tail(list) ((list)->tail)

#define list_is_head(list, element) ((element) == (list)->head ? 1 : 0)

#define list_is_tail(element) ((element)->next == NULL ? 1 : 0)

#define list_data(element) ((element)->data)

#define list_next(element) ((element)->next)

#endif

/*****************************************************************************
*                                                                            *
*  ------------------------------- clist.h --------------------------------  *
*                                                                            *
*****************************************************************************/

#ifndef CLIST_H
#define CLIST_H


/*****************************************************************************
*                                                                            *
*  Define a structure for circular list elements.                            *
*                                                                            *
*****************************************************************************/

typedef struct CListElmt_ {

void               *data;
struct CListElmt_  *next;

} CListElmt;

/*****************************************************************************
*                                                                            *
*  Define a structure for circular lists.                                    *
*                                                                            *
*****************************************************************************/

typedef struct CList_ {

int                size;

int                (*match)(const void *key1, const void *key2);
void               (*destroy)(void *data);

CListElmt          *head;

} CList;

/*****************************************************************************
*                                                                            *
*  --------------------------- Public Interface ---------------------------  *
*                                                                            *
*****************************************************************************/

void clist_init(CList *list, void (*destroy)(void *data));

void clist_destroy(CList *list);

int clist_ins_next(CList *list, CListElmt *element, const void *data);

int clist_rem_next(CList *list, CListElmt *element, void **data);

#define clist_size(list) ((list)->size)

#define clist_head(list) ((list)->head)

#define clist_data(element) ((element)->data)

#define clist_next(element) ((element)->next)

#endif

/*****************************************************************************
*                                                                            *
*  ------------------------------- dlist.h --------------------------------  *
*                                                                            *
*****************************************************************************/

#ifndef DLIST_H
#define DLIST_H


/*****************************************************************************
*                                                                            *
*  Define a structure for doubly-linked list elements.                       *
*                                                                            *
*****************************************************************************/

typedef struct DListElmt_ {

void               *data;
struct DListElmt_  *prev;
struct DListElmt_  *next;

} DListElmt;

/*****************************************************************************
*                                                                            *
*  Define a structure for doubly-linked lists.                               *
*                                                                            *
*****************************************************************************/

typedef struct DList_ {

int                size;

int                (*match)(const void *key1, const void *key2);
void               (*destroy)(void *data);

DListElmt          *head;
DListElmt          *tail;

} DList;

/*****************************************************************************
*                                                                            *
*  --------------------------- Public Interface ---------------------------  *
*                                                                            *
*****************************************************************************/

void dlist_init(DList *list, void (*destroy)(void *data));

void dlist_destroy(DList *list);

int dlist_ins_next(DList *list, DListElmt *element, const void *data);

int dlist_ins_prev(DList *list, DListElmt *element, const void *data);

int dlist_remove(DList *list, DListElmt *element, void **data);

#define dlist_size(list) ((list)->size)

#define dlist_head(list) ((list)->head)

#define dlist_tail(list) ((list)->tail)

#define dlist_is_head(element) ((element)->prev == NULL ? 1 : 0)

#define dlist_is_tail(element) ((element)->next == NULL ? 1 : 0)

#define dlist_data(element) ((element)->data)

#define dlist_next(element) ((element)->next)

#define dlist_prev(element) ((element)->prev)

#endif


#endif
