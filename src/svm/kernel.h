/************************************************************************/
/*                                                                      */
/*   kernel.h                                                           */
/*                                                                      */
/*   User defined kernel function. Feel free to plug in your own.       */
/*                                                                      */
/*   Copyright: Thorsten Joachims                                       */
/*   Date: 16.12.97                                                     */
/*                                                                      */
/************************************************************************/

/* KERNEL_PARM is defined in svm_common.h The field 'custom' is reserved for */
/* parameters of the user defined kernel. You can also access and use */
/* the parameters of the other kernels. Just replace the line 
 * return((double)(1.0)); with your own kernel. */

/* Example: The following computes the polynomial kernel. sprod_ss
 * computes the inner product between two sparse vectors.
 *
 * return((CFLOAT)pow(kernel_parm->coef_lin*sprod_ss(a->words,b->words)
 * +kernel_parm->coef_const,(double)kernel_parm->poly_degree)); */

double custom_kernel(KERNEL_PARM *kernel_parm, DOC *a, DOC *b)                         
{
  if (!strcmp(kernel_parm->custom,"complex1"))
  {
    /* JL Dec. 2008:
     * Kernel to calculate inner product of two complex sparse vectors. */
    
    /* The real part has to be in the first half and the imaginary part in the
     * second half of the WORD vector within the DOCs. This kernel is basically 
     * the complex equivalent of sprod_ss.*/
    
    FNUM size_i, size_j, i, j;
    FVAL Re_a, Re_b, Im_a, Im_b, Re_sum, Im_sum;
    size_i=size_j=i=j=0;
    Re_a=Re_b=Im_a=Im_b=Re_sum=Im_sum=0.0;
    
    while ((a->words[i]).wnum) {i++;}
    while ((b->words[j]).wnum) {j++;}
    size_i=i/2;
    size_j=j/2;
    
    if (i%2 != 0 || j%2 != 0) {
      fprintf(stderr,
          "** ERROR (kernel.h): something is wrong with the complex-valued data\n"
          "                     representation in the WORD structure.\n\n");
      exit(0);
    }

    i=j=0;  
    while (i < size_i && j < size_j) {
      if ((a->words[i]).wnum > (b->words[j]).wnum) {j++;}
      else if ((a->words[i]).wnum < (b->words[j]).wnum) {i++;}
      else {
        Re_a=a->words[i].weight;
        Im_a=a->words[i+size_i].weight;
        Re_b=b->words[j].weight;
        Im_b=b->words[j+size_j].weight;
          
        Im_b=-1.0*Im_b;
        Re_sum += Re_a*Re_b-Im_a*Im_b;
        Im_sum += Re_a*Im_b+Im_a*Re_b;
        
        i++;
        j++;
      }
    }
    return((double)Re_sum);
  }
  else {
    fprintf(stderr, "** ERROR (kernel.h): Custom kernel '%s' not implemented! "
        "How did you get here? \n", kernel_parm->custom); exit(0);
  }
}
