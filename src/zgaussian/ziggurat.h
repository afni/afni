/****************************************************************************
*
* Code author: J. Burkhardt 
*
* This code is distributed under the MIT license.
*
* This file was downloaded from here:
*   https://people.sc.fsu.edu/~jburkardt/c_src/ziggurat/ziggurat.h
* 
* Description from here:
*   https://people.sc.fsu.edu/~jburkardt/c_src/ziggurat/ziggurat.html
* 
*   ziggurat, a C code which generates random variates from the
*   uniform, normal or exponential distributions, by Marsaglia and
*   Tsang.
* 
*   The uniform numbers are generated directly. The ziggurat method is
*   used to compute the normal and exponential values.
* 
*   In the inline version, the underlying generators are implemented
*   "inline", invoking a function call only in exceptional cases. This
*   results in very fast execution.
*
*   In this implementation, the advantages of inline code are not
*   used. All the routines and inline functions are isolated in a
*   separate file, so that a user invokes them through the familiar
*   library interface.
*
* References to read about the method:
* 
*   Philip Leong, Guanglie Zhang, Dong-U Lee, Wayne Luk, John Villasenor,
*   A comment on the implementation of the ziggurat method,
*   Journal of Statistical Software,
*   Volume 12, Number 7, February 2005.
* 
*   George Marsaglia, Wai Wan Tsang,
*   The Ziggurat Method for Generating Random Variables,
*   Journal of Statistical Software,
*   Volume 5, Number 8, October 2000, seven pages. 
*
****************************************************************************/


extern uint32_t cong_seeded ( uint32_t *jcong );
extern uint32_t kiss_seeded ( uint32_t *jcong, uint32_t *jsr, uint32_t *w, 
                              uint32_t *z );
extern uint32_t mwc_seeded ( uint32_t *w, uint32_t *z );

extern float r4_exp ( uint32_t *jsr, uint32_t ke[256], float fe[256], 
                      float we[256] );
extern void r4_exp_setup ( uint32_t ke[256], float fe[256], float we[256] );

extern float r4_nor ( uint32_t *jsr, uint32_t kn[128], float fn[128], 
                      float wn[128] );
extern void r4_nor_setup ( uint32_t kn[128], float fn[128], float wn[128] );
extern float r4_uni ( uint32_t *jsr );


extern uint32_t shr3_seeded ( uint32_t *jsr );
