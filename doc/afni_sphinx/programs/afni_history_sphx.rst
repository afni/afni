************
afni_history
************

.. _afni_history:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    afni_history:           show AFNI updates per user, dates or levels
    
    This program is meant to display a log of updates to AFNI code, the
    website, educational material, etc.  Users can specify a level of
    importance, the author, program or how recent the changes are.
    
    The levels of importance go from 1 to 4, with meanings:
           1 - users would not care
           2 - of little importance, though some users might care
           3 - fairly important
           4 - a big change or new program
           5 - IMPORTANT: we expect users to know
    
    -----------------------------------------------------------------
    
    common examples:
    
      0. get help
    
         a. afni_history -help
    
      1. display all of the history, possibly subject to recent days/entries
    
         a. afni_history
         b. afni_history -past_days 5
         c. afni_history -past_months 6
         d. afni_history -past_entries 1
    
      2. select a specific type, level or minimum level
    
         a. afni_history -level 2
         b. afni_history -min_level 3 -type BUG_FIX
         c. afni_history -type 1 -min_level 3 -past_years 1
    
      3. select a specific author or program
    
         a. afni_history -author rickr
         b. afni_history -program afni_proc.py
    
         display the past year of updates for rickr, sorted by program name...
    
         c. afni_history -author rickr -past_days 365 -final_sort_by_prog
    
      4. select level 3+ suma updates from ziad over the past year
    
         a. afni_history -author ziad -min_level 3 -program suma
    
      5. generate a web-page, maybe from the past year at at a minimum level
    
         a. afni_history -html -reverse > afni_hist_all.html
         b. afni_history -html -reverse -min_level 2  > afni_hist_level2.html
         c. afni_history -html -reverse -min_level 3  > afni_hist_level3.html
         d. afni_history -html -reverse -min_level 4  > afni_hist_level4.html
    
      5. verify that the distribution is new enough
    
         Compare the most recent history entry against the passed date.  If
         there is a history entry as recent as the given date, it is current.
         Otherwise, the distribution is considered old.
    
         a. afni_history -check_date 1 1 2010
         b. afni_history -check_date 15 Mar 2050
    
    -----------------------------------------------------------------
    
    ------------------ informational options: -----------------------
    
      -help                    : show this help
      -hist                    : show this program's history
      -list_authors            : show the list of valid authors
      -list_types              : show the list of valid change types
      -ver                     : show this program's version
    
    ------------------ output restriction options: ------------------
    
      -author AUTHOR           : restrict output to the given AUTHOR
      -level LEVEL             : restrict output to the given LEVEL
      -min_level LEVEL         : restrict output to at least level LEVEL
      -program PROGRAM         : restrict output to the given PROGRAM
    
      -past_entries ENTRIES    : restrict output to final ENTRIES entries
      -past_days DAYS          : restrict output to the past DAYS days
      -past_months MONTHS      : restrict output to the past MONTHS months
      -past_years YEARS        : restrict output to the past YEARS years
    
      -type TYPE               : restrict output to the given TYPE
                                 (TYPE = 0..5, or strings 'NEW_PROG', etc.)
                                 e.g.  -type NEW_ENV
                                 e.g.  -type BUG_FIX
    
    ------------------ verification options: ------------------------
    
      -check_date DD MM YYYY   : check history against given date
    
         If most recent afni_history is older than the passed date, the
         distribution version might be considered out of date.  Otherwise, it
         might be considered current.
    
         If the version seems okay, afni_history returns 0, else 1.
         That way a script can check the status.
    
    ------------------ general options: -----------------------------
    
      -html                    : add html formatting
      -dline                   : put a divider line between dates
      -reverse                 : reverse the sorting order
                                 (sort is by date, author, level, program)
      -verb LEVEL              : request verbose output
                                 (LEVEL is from 0-6)
    
    
                                               Author: Rick Reynolds
                                               Thanks to: Ziad, Bob
