/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This file contains routines for sorting numbers and determining ranks.

  File:    ranks.c
  Author:  B. Douglas Ward
  Date:    31 March 2000

*/

/*---------------------------------------------------------------------------*/
/*
  Structure to store list of values, sorted in increasing order.
*/
  
typedef struct node
{
  float fval;             /* floating point value */
  int d;                  /* count of number of occurances of this value */
  struct node * next;     /* link to next node */
} node;


/*---------------------------------------------------------------------------*/
/*
  Print contents of list, starting at smallest value. 
*/

void list_print (node * n, int * count)
{
  int i;

  for (i = 0;  i < n->d;  i++)
    {
      printf (" %6.1f", n->fval);
      *count += 1;
      if (*count % 10 == 0)
	printf ("\n");
    }

  if (n->next != NULL)
    list_print (n->next, count);
}


/*---------------------------------------------------------------------------*/
/*
  Delete the entire list.
*/

void list_delete (node ** n)
{
  if ((*n)->next != NULL)
    list_delete (&((*n)->next));
  free (*n);
  *n = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Insert one node with value r; reset pointers.
*/

void node_insert (node ** n, float r)
{
  node * ptr;

  ptr = *n;
  *n = (node *) malloc (sizeof(node));
  (*n)->fval = r;
  (*n)->d = 1;
  (*n)->next = ptr;
}


/*---------------------------------------------------------------------------*/
/*
  Add number r to list; if number is already in the list, just increment the
  counter.  Otherwise, insert new node.
*/

void node_addvalue (node ** head, float r)
{
  node ** lastptr;
  node * ptr;


  if (*head == NULL)  node_insert (head, r);
  else
    {
      lastptr = head;
      ptr = *head;

      while ( (ptr->fval < r) && (ptr->next != NULL) )
	{
	  lastptr = &(ptr->next);
	  ptr = ptr->next;
	}
      
      if (ptr->fval > r)
	node_insert (lastptr, r);
      else
	if (ptr->fval == r)
	  ptr->d += 1;
        else
	  node_insert (&(ptr->next), r);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Get rank corresponding to number r.  If ties exist, return average rank.
*/

float node_get_rank (node * head, float r)
{
  node * ptr;
  float rank;

  ptr = head;
  rank = 0.0;

  while (ptr->fval != r)
    {
      rank += ptr->d;
      ptr = ptr->next;
    }

  rank += (ptr->d + 1) / 2.0;
  return (rank);
}


/*---------------------------------------------------------------------------*/
/*
  Return value corresponding to the specified rank.
*/

float node_get_value (node * head, int rank)
{
  node * ptr;
  int k;

  ptr = head;
  k = 0;

  while (k + ptr->d < rank)
    {
      k += ptr->d;
      ptr = ptr->next;
    }

  return (ptr->fval);
}


/*---------------------------------------------------------------------------*/
/*
  Return value corresponding to the median value.
*/

float node_get_median (node * head, int n)
{
  float median;


  if (n % 2)
    median = node_get_value(head, n/2 + 1);
  else
    median = 0.5 * (node_get_value(head, n/2) + 
		    node_get_value(head, n/2 + 1));

  return (median);
}


/*---------------------------------------------------------------------------*/
/*
  Sort the input data array of floats, and return a new array containing 
  the ranks of the input data. 
*/

float * rank_array
(
  int n,                             /* number of data points */
  float * xarray                     /* array of data to be ranked */
)

{
  int i;                             /* array index */
  node * xhead = NULL;               /* pointer to list of sorted values */
  float * rarray = NULL;             /* array of ranks */


  /*----- Allocate memory for array of ranks -----*/
  rarray = (float *) malloc (sizeof(float) * n);    MTEST (rarray); 


  /*----- Enter and sort original data  -----*/
  for (i = 0;  i < n;  i++)
    node_addvalue (&xhead, xarray[i]); 


  /*----- Get ranks of data -----*/
  for (i = 0;  i < n;  i++)
    rarray[i] = node_get_rank (xhead, xarray[i]);


  /*----- Deallocate memory -----*/
  list_delete (&xhead);


  /*----- Return array of ranks -----*/
  return (rarray);
}


/*---------------------------------------------------------------------------*/
/*
  Sort the input data array of doubles, and return a new array containing 
  the ranks of the input data. 
*/

float * rank_darray
(
  int n,                             /* number of data points */
  double * darray                    /* array of data to be ranked */
)

{
  int i;                             /* array index */
  node * xhead = NULL;               /* pointer to list of sorted values */
  float * rarray = NULL;             /* array of ranks */


  /*----- Allocate memory for array of ranks -----*/
  rarray = (float *) malloc (sizeof(float) * n);    MTEST (rarray); 


  /*----- Enter and sort original data  -----*/
  for (i = 0;  i < n;  i++)
    node_addvalue (&xhead, (float) darray[i]); 


  /*----- Get ranks of data -----*/
  for (i = 0;  i < n;  i++)
    rarray[i] = node_get_rank (xhead, (float) darray[i]);


  /*----- Deallocate memory -----*/
  list_delete (&xhead);


  /*----- Return array of ranks -----*/
  return (rarray);
}


/*---------------------------------------------------------------------------*/

