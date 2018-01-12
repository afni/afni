.. contents:: 
    :depth: 4 

**********
cifti_tool
**********

.. code-block:: none

    ct : short example of reading/writing CIFTI-2 datasets
    
        This program is to demonstrate how to read a CIFTI-2 dataset.
    
        basic usage: cifti_tool -input FILE [other options]
    
        examples:
    
           cifti_tool -input FILE -disp_cext
           cifti_tool -input FILE -disp_cext -as_cext
           cifti_tool -input FILE -disp_cext -output cifti.txt
    
           cifti_tool -input FILE -eval_cext
           cifti_tool -input FILE -eval_cext -verb 2
           cifti_tool -input FILE -eval_cext -eval_type show_summary
    
           cifti_tool -input FILE -eval_cext -eval_type show_name
           cifti_tool -input FILE -eval_cext -eval_type has_data
           cifti_tool -input FILE -eval_cext -eval_type show_text_data
    
        get a list of unique element types with attached data
    
           cifti_tool -input FILE -eval_cext -eval_type has_data \
                      | sort | uniq
    
        options:
    
           -help               : show this help
    
           -input  INFILE      : specify input dataset
           -output OUTFILE     : where to write output
    
           -as_cext            : process the input as just an extension
           -disp_cext          : display the CIFTI extension
           -eval_cext          : evaluate the CIFTI extension
           -eval_type ETYPE    : method for evaluation of axml elements
    
              valid ETYPES:
                 has_data       - show elements with attached text data
                 has_bdata      - show elements with attached binary data
                 num_tokens     - show the number of tokens in such text
                 show           - like -disp_cext
                 show_names     - show element names, maybe depth indented
                 show_summary   - summarize contents of dataset
                 show_text_data - show the actual text data
    
           -verb LEVEL         : set the verbose level to LEVEL
           -verb_read LEVEL    : set verbose level when reading
           -vboth LEVEL        : apply both -verb options
