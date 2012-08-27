/* Code in this file is taken from the book:
 "Mastering Algorithms with C"  by Kyle Loudon,  published by O'Reilly & Associates.  This
code is under copyright and cannot be included in any other book, publication,
or  educational product  without  permission  from  O'Reilly & Associates.  No
warranty is attached; we cannot take responsibility for errors or  fitness for
use.
*/

#include <stdlib.h>
#include <string.h>

#include "suma_algorithms.h"


/*****************************************************************************
*                                                                            *
*  -------------------------------- list.c --------------------------------  *
*                                                                            *
*****************************************************************************/

/*****************************************************************************
*                                                                            *
*  ------------------------------- list_init ------------------------------  *
*                                                                            *
*****************************************************************************/

void list_init(List *list, void (*destroy)(void *data)) {

/*****************************************************************************
*                                                                            *
*  Initialize the list.                                                      *
*                                                                            *
*****************************************************************************/

list->size = 0;
list->destroy = destroy;
list->head = NULL;
list->tail = NULL;

return;

}

/*****************************************************************************
*                                                                            *
*  ----------------------------- list_destroy -----------------------------  *
*                                                                            *
*****************************************************************************/

void list_destroy(List *list) {

void               *data;

/*****************************************************************************
*                                                                            *
*  Remove each element.                                                      *
*                                                                            *
*****************************************************************************/

while (list_size(list) > 0) {

   if (list_rem_next(list, NULL, (void **)&data) == 0 && list->destroy !=
      NULL) {

      /***********************************************************************
      *                                                                      *
      *  Call a user-defined function to free dynamically allocated data.    *
      *                                                                      *
      ***********************************************************************/

      list->destroy(data);

   }

}

/*****************************************************************************
*                                                                            *
*  No operations are allowed now, but clear the structure as a precaution.   *
*                                                                            *
*****************************************************************************/

memset(list, 0, sizeof(List));

return;

}

/*****************************************************************************
*                                                                            *
*  ----------------------------- list_ins_next ----------------------------  *
*                                                                            *
*****************************************************************************/

int list_ins_next(List *list, ListElmt *element, const void *data) {

ListElmt           *new_element;

/*****************************************************************************
*                                                                            *
*  Allocate storage for the element.                                         *
*                                                                            *
*****************************************************************************/

if ((new_element = (ListElmt *)malloc(sizeof(ListElmt))) == NULL)
   return -1;

/*****************************************************************************
*                                                                            *
*  Insert the element into the list.                                         *
*                                                                            *
*****************************************************************************/

new_element->data = (void *)data;

if (element == NULL) {

   /**************************************************************************
   *                                                                         *
   *  Handle insertion at the head of the list.                              *
   *                                                                         *
   **************************************************************************/

   if (list_size(list) == 0)
      list->tail = new_element;

   new_element->next = list->head;
   list->head = new_element;

   }

else {

   /**************************************************************************
   *                                                                         *
   *  Handle insertion somewhere other than at the head.                     *
   *                                                                         *
   **************************************************************************/

   if (element->next == NULL)
      list->tail = new_element;

   new_element->next = element->next;
   element->next = new_element;

}

/*****************************************************************************
*                                                                            *
*  Adjust the size of the list to account for the inserted element.          *
*                                                                            *
*****************************************************************************/

list->size++;

return 0;

}

/*****************************************************************************
*                                                                            *
*  ----------------------------- list_rem_next ----------------------------  *
*                                                                            *
*****************************************************************************/

int list_rem_next(List *list, ListElmt *element, void **data) {

ListElmt           *old_element;

/*****************************************************************************
*                                                                            *
*  Do not allow removal from an empty list.                                  *
*                                                                            *
*****************************************************************************/

if (list_size(list) == 0)
   return -1;

/*****************************************************************************
*                                                                            *
*  Remove the element from the list.                                         *
*                                                                            *
*****************************************************************************/

if (element == NULL) {

   /**************************************************************************
   *                                                                         *
   *  Handle removal from the head of the list.                              *
   *                                                                         *
   **************************************************************************/

   *data = list->head->data;
   old_element = list->head;
   list->head = list->head->next;

   if (list_size(list) == 1)
      list->tail = NULL;

   }

else {

   /**************************************************************************
   *                                                                         *
   *  Handle removal from somewhere other than the head.                     *
   *                                                                         *
   **************************************************************************/

   if (element->next == NULL)
      return -1;

   *data = element->next->data;
   old_element = element->next;
   element->next = element->next->next;

   if (element->next == NULL)
      list->tail = element;

}

/*****************************************************************************
*                                                                            *
*  Free the storage allocated by the abstract data type.                     *
*                                                                            *
*****************************************************************************/

free(old_element);

/*****************************************************************************
*                                                                            *
*  Adjust the size of the list to account for the removed element.           *
*                                                                            *
*****************************************************************************/

list->size--;

return 0;

}

/*****************************************************************************
*                                                                            *
*  ------------------------------- clist.c --------------------------------  *
*                                                                            *
*****************************************************************************/


/*****************************************************************************
*                                                                            *
*  ------------------------------ clist_init ------------------------------  *
*                                                                            *
*****************************************************************************/

void clist_init(CList *list, void (*destroy)(void *data)) {

/*****************************************************************************
*                                                                            *
*  Initialize the list.                                                      *
*                                                                            *
*****************************************************************************/

list->size = 0;
list->destroy = destroy;
list->head = NULL;

return;

}

/*****************************************************************************
*                                                                            *
*  ---------------------------- clist_destroy -----------------------------  *
*                                                                            *
*****************************************************************************/

void clist_destroy(CList *list) {

void               *data;

/*****************************************************************************
*                                                                            *
*  Remove each element.                                                      *
*                                                                            *
*****************************************************************************/

while (clist_size(list) > 0) {

   if (clist_rem_next(list, list->head, (void **)&data) == 0 && list->destroy
      != NULL) {

      /***********************************************************************
      *                                                                      *
      *  Call a user-defined function to free dynamically allocated data.    *
      *                                                                      *
      ***********************************************************************/

      list->destroy(data);

   }

}

/*****************************************************************************
*                                                                            *
*  No operations are allowed now, but clear the structure as a precaution.   *
*                                                                            *
*****************************************************************************/

memset(list, 0, sizeof(CList));

return;

}

/*****************************************************************************
*                                                                            *
*  ---------------------------- clist_ins_next ----------------------------  *
*                                                                            *
*****************************************************************************/

int clist_ins_next(CList *list, CListElmt *element, const void *data) {

CListElmt          *new_element;

/*****************************************************************************
*                                                                            *
*  Allocate storage for the element.                                         *
*                                                                            *
*****************************************************************************/

if ((new_element = (CListElmt *)malloc(sizeof(CListElmt))) == NULL)
   return -1;

/*****************************************************************************
*                                                                            *
*  Insert the element into the list.                                         *
*                                                                            *
*****************************************************************************/

new_element->data = (void *)data;

if (clist_size(list) == 0) {

   /**************************************************************************
   *                                                                         *
   *  Handle insertion when the list is empty.                               *
   *                                                                         *
   **************************************************************************/

   new_element->next = new_element;
   list->head = new_element;

   }

else {

   /**************************************************************************
   *                                                                         *
   *  Handle insertion when the list is not empty.                           *
   *                                                                         *
   **************************************************************************/

   new_element->next = element->next;
   element->next = new_element;

}

/*****************************************************************************
*                                                                            *
*  Adjust the size of the list to account for the inserted element.          *
*                                                                            *
*****************************************************************************/

list->size++;

return 0;

}

/*****************************************************************************
*                                                                            *
*  ---------------------------- clist_rem_next ----------------------------  *
*                                                                            *
*****************************************************************************/

int clist_rem_next(CList *list, CListElmt *element, void **data) {

CListElmt          *old_element;

/*****************************************************************************
*                                                                            *
*  Do not allow removal from an empty list.                                  *
*                                                                            *
*****************************************************************************/

if (clist_size(list) == 0)
   return -1;

/*****************************************************************************
*                                                                            *
*  Remove the element from the list.                                         *
*                                                                            *
*****************************************************************************/

*data = element->next->data;

if (element->next == element) {

   /**************************************************************************
   *                                                                         *
   *  Handle removing the last element.                                      *
   *                                                                         *
   **************************************************************************/

   old_element = element->next;
   list->head = NULL;

   }

else {

   /**************************************************************************
   *                                                                         *
   *  Handle removing other than the last element.                           *
   *                                                                         *
   **************************************************************************/

   old_element = element->next;
   element->next = element->next->next;

}

/*****************************************************************************
*                                                                            *
*  Free the storage allocated by the abstract data type.                     *
*                                                                            *
*****************************************************************************/

free(old_element);

/*****************************************************************************
*                                                                            *
*  Adjust the size of the list to account for the removed element.           *
*                                                                            *
*****************************************************************************/

list->size--;

return 0;

}
/*****************************************************************************
*                                                                            *
*  ------------------------------- dlist.c --------------------------------  *
*                                                                            *
*****************************************************************************/

/*****************************************************************************
*                                                                            *
*  ------------------------------ dlist_init ------------------------------  *
*                                                                            *
*****************************************************************************/

void dlist_init(DList *list, void (*destroy)(void *data)) {

/*****************************************************************************
*                                                                            *
*  Initialize the list.                                                      *
*                                                                            *
*****************************************************************************/

list->size = 0;
list->destroy = destroy;
list->head = NULL;
list->tail = NULL;

return;

}

/*****************************************************************************
*                                                                            *
*  ---------------------------- dlist_destroy -----------------------------  *
*                                                                            *
*****************************************************************************/

void dlist_destroy(DList *list) {

void               *data;

/*****************************************************************************
*                                                                            *
*  Remove each element.                                                      *
*                                                                            *
*****************************************************************************/

while (dlist_size(list) > 0) {

   if (dlist_remove(list, dlist_tail(list), (void **)&data) == 0 && list->
      destroy != NULL) {

      /***********************************************************************
      *                                                                      *
      *  Call a user-defined function to free dynamically allocated data.    *
      *                                                                      *
      ***********************************************************************/

      list->destroy(data);

   }

}

/*****************************************************************************
*                                                                            *
*  No operations are allowed now, but clear the structure as a precaution.   *
*                                                                            *
*****************************************************************************/

memset(list, 0, sizeof(DList));

return;

}

/*****************************************************************************
*                                                                            *
*  ---------------------------- dlist_ins_next ----------------------------  *
*                                                                            *
*****************************************************************************/

int dlist_ins_next(DList *list, DListElmt *element, const void *data) {

DListElmt          *new_element;

/*****************************************************************************
*                                                                            *
*  Do not allow a NULL element unless the list is empty.                     *
*                                                                            *
*****************************************************************************/

if (element == NULL && dlist_size(list) != 0)
   return -1;

/*****************************************************************************
*                                                                            *
*  Allocate storage for the element.                                         *
*                                                                            *
*****************************************************************************/

if ((new_element = (DListElmt *)malloc(sizeof(DListElmt))) == NULL)
   return -1;

/*****************************************************************************
*                                                                            *
*  Insert the new element into the list.                                     *
*                                                                            *
*****************************************************************************/

new_element->data = (void *)data;

if (dlist_size(list) == 0) {

   /**************************************************************************
   *                                                                         *
   *  Handle insertion when the list is empty.                               *
   *                                                                         *
   **************************************************************************/

   list->head = new_element;
   list->head->prev = NULL;
   list->head->next = NULL;
   list->tail = new_element;

   }

else {

   /**************************************************************************
   *                                                                         *
   *  Handle insertion when the list is not empty.                           *
   *                                                                         *
   **************************************************************************/

   new_element->next = element->next;
   new_element->prev = element;

   if (element->next == NULL)
      list->tail = new_element;
   else
      element->next->prev = new_element;

   element->next = new_element;

}

/*****************************************************************************
*                                                                            *
*  Adjust the size of the list to account for the inserted element.          *
*                                                                            *
*****************************************************************************/

list->size++;

return 0;

}

/*****************************************************************************
*                                                                            *
*  ---------------------------- dlist_ins_prev ----------------------------  *
*                                                                            *
*****************************************************************************/


int dlist_ins_prev(DList *list, DListElmt *element, const void *data) {

DListElmt          *new_element;

/*****************************************************************************
*                                                                            *
*  Do not allow a NULL element unless the list is empty.                     *
*                                                                            *
*****************************************************************************/

if (element == NULL && dlist_size(list) != 0)
   return -1;

/*****************************************************************************
*                                                                            *
*  Allocate storage to be managed by the abstract data type.                 *
*                                                                            *
*****************************************************************************/

if ((new_element = (DListElmt *)malloc(sizeof(DListElmt))) == NULL)
   return -1;

/*****************************************************************************
*                                                                            *
*  Insert the new element into the list.                                     *
*                                                                            *
*****************************************************************************/

new_element->data = (void *)data;

if (dlist_size(list) == 0) {

   /**************************************************************************
   *                                                                         *
   *  Handle insertion when the list is empty.                               *
   *                                                                         *
   **************************************************************************/

   list->head = new_element;
   list->head->prev = NULL;
   list->head->next = NULL;
   list->tail = new_element;

   }


else {

   /**************************************************************************
   *                                                                         *
   *  Handle insertion when the list is not empty.                           *
   *                                                                         *
   **************************************************************************/

   new_element->next = element; 
   new_element->prev = element->prev;

   if (element->prev == NULL)
      list->head = new_element;
   else
      element->prev->next = new_element;

   element->prev = new_element;

}


/*****************************************************************************
*                                                                            *
*  Adjust the size of the list to account for the new element.               *
*                                                                            *
*****************************************************************************/

list->size++;

return 0;

}

/*****************************************************************************
*                                                                            *
*  ----------------------------- dlist_remove -----------------------------  *
*                                                                            *
*****************************************************************************/

int dlist_remove(DList *list, DListElmt *element, void **data) {

/*****************************************************************************
*                                                                            *
*  Do not allow a NULL element or removal from an empty list.                *
*                                                                            *
*****************************************************************************/

if (element == NULL || dlist_size(list) == 0)
   return -1;

/*****************************************************************************
*                                                                            *
*  Remove the element from the list.                                         *
*                                                                            *
*****************************************************************************/

*data = element->data;

if (element == list->head) {

   /**************************************************************************
   *                                                                         *
   *  Handle removal from the head of the list.                              *
   *                                                                         *
   **************************************************************************/

   list->head = element->next;

   if (list->head == NULL)
      list->tail = NULL;
   else
      element->next->prev = NULL;

   }

else {

   /**************************************************************************
   *                                                                         *
   *  Handle removal from other than the head of the list.                   *
   *                                                                         *
   **************************************************************************/

   element->prev->next = element->next;

   if (element->next == NULL)
      list->tail = element->prev;
   else
      element->next->prev = element->prev;

}

/*****************************************************************************
*                                                                            *
*  Free the storage allocated by the abstract data type.                     *
*                                                                            *
*****************************************************************************/

free(element);

/*****************************************************************************
*                                                                            *
*  Adjust the size of the list to account for the removed element.           *
*                                                                            *
*****************************************************************************/

list->size--;

return 0;

}

void *dlist_ith_elmt_data(DList *list, int index)
{
   int cnt=0;
   DListElmt *element;
   
   if (!list || index < 0 || index > list->size-1) return(NULL);

   element = list->head;
   while (index > cnt) {
      element = element->next; ++cnt;
   }
   return(dlist_data(element));
}
