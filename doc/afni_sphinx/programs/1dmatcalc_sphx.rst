*********
1dmatcalc
*********

.. _1dmatcalc:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: 1dmatcalc [-verb] expression
    
    Evaluate a space delimited RPN matrix-valued expression:
    
     * The operations are on a stack, each element of which is a
         real-valued matrix.
       * N.B.: This is a computer-science stack of separate matrices.
               If you want to join two matrices in separate files
               into one 'stacked' matrix, then you must use program
               1dcat to join them as columns, or the system program
               cat to join them as rows.
     * You can also save matrices by name in an internal buffer
         using the '=NAME' operation and then retrieve them later
         using just the same NAME.
     * You can read and write matrices from files stored in ASCII
         columns (.1D format) using the &read and &write operations.
     * The following 5 operations, input as a single string,
         '&read(V.1D) &read(U.1D) &transp * &write(VUT.1D)'
       - reads matrices V and U from disk (separately),
       - transposes U (on top of the stack) into U',
       - multiplies V and U' (the two matrices on top of the stack),
       - and writes matrix VU' out (the matrix left on the stack by '*').
     * Calculations are carried out in single precision ('float').
     * Operations mostly contain characters such as '&' and '*' that
       are special to Unix shells, so you'll probably need to put
       the arguments to this program in 'single quotes'.
     * You can use '%%' or '@' in place of the '&' character, if you wish.
    
     STACK OPERATIONS
     -----------------
     number     == push scalar value (1x1 matrix) on stack;
                     a number starts with a digit or a minus sign
     =NAME      == save a copy matrix on top of stack as 'NAME'
     NAME       == push a copy of NAME-ed matrix onto top of stack;
                     names start with an alphabetic character
     &clear     == erase all named matrices (to save memory);
                     does not affect the stack at all
     &read(FF)  == read ASCII (.1D) file onto top of stack from file 'FF'
     &read4x4Xform(FF)
                == Similar to &read(FF), except that it expects data
                   for a 12-parameter spatial affine transform.
                   FF can contain 12x1, 1x12, 16x1, 1x16, 3x4, or
                   4x4 values. 
                   The read operation loads the data into a 4x4 matrix
                      r11   r12   r13   r14
                      r21   r22   r23   r24
                      r31   r32   r33   r34
                      0.0   0.0   0.0   1.0
                   This option was added to simplify the combination of 
                   linear spatial transformations. However, you are better 
                   off using cat_matvec for that purpose.
    
     &write(FF) == write top matrix to ASCII file to file 'FF';
                     if 'FF' == '-', writes to stdout
     &transp    == replace top matrix with its transpose
     &ident(N)  == push square identity matrix of order N onto stack
                     N is an fixed integer, OR
                     &R to indicate the row dimension of the
                        current top matrix, OR
                     &C to indicate the column dimension of the
                        current top matrix, OR
                     =X to indicate the (1,1) element of the
                        matrix named X
     &Psinv     == replace top matrix with its pseudo-inverse
                     [computed via SVD, not via inv(A'*A)*A']
     &Sqrt      == replace top matrix with its square root
                     [computed via Denman & Beavers iteration]
                   N.B.: not all real matrices have real square
                     roots, and &Sqrt will fail if you push it
                   N.B.: the matrix must be square!
     &Pproj     == replace top matrix with the projection onto
                     its column space; Input=A; Output = A*Psinv(A)
                   N.B.: result P is symmetric and P*P=P
     &Qproj     == replace top matrix with the projection onto
                     the orthogonal complement of its column space
                     Input=A; Output=I-Pproj(A)
     *          == replace top 2 matrices with their product;
      OR               stack = [ ... C A B ] (where B = top) goes to
     &mult             stack = [ ... C AB ]
                     if either of the top matrices is a 1x1 scalar,
                     then the result is the scalar multiplication of
                     the other matrix; otherwise, matrices must conform
     + OR &add  == replace top 2 matrices with sum A+B
     - OR &sub  == replace top 2 matrices with difference A-B
     &dup       == push duplicate of top matrix onto stack
     &pop       == discard top matrix
     &swap      == swap top two matrices (A <-> B)
     &Hglue     == glue top two matrices together horizontally:
                       stack = [ ... C A B ] goes to
                       stack = [ ... C A|B ]
                     this is like what program 1dcat does.
     &Vglue     == glue top two matrices together vertically:
                       stack = [ ... C A B ] goes to
    
                                        A
                       stack = [ ... C  - ]
                                        B
    
                     this is like what program cat does.
    
    SIMPLE EXAMPLES
    ---------------
    * Multiply each element of an input 1D file
      by a constant factor and write to disk.
        1dmatcalc "&read(in.1D) 3.1416 * &write(out.1D)"
    
    * Subtract two 1D files
