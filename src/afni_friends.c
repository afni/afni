#include <stdlib.h>
#include <time.h>
#include <stdio.h>

static char * afni_helptypes[] = {
   "advice and help"                ,  /* mask =   1 */
   "much encouragement"             ,  /* mask =   2 */
   "many suggestions"               ,  /* mask =   4 */
   "useful feedback"                ,  /* mask =   8 */
   "\"quick\" questions"            ,  /* mask =  16 */
   "inspiration"                    ,  /* mask =  32 */
   "great efforts"                  ,  /* mask =  64 */
   "caloric input"                  ,  /* mask = 128 */
   "awe-inspiring beer consumption" ,  /* mask = 256 */
   "awe-inspiring caffeine binging" ,  /* mask = 512 */
   "you-know-what"                     /* mask =1024 */
} ;

#define YOU_KNOW_WHAT 10
#define INSPIRATION    5
#define KLOSEK         7

#define NUM_HELPTYPES (sizeof(afni_helptypes)/sizeof(char *))

typedef struct { char * name ; int helpmask ; } AFNI_friend ;

static AFNI_friend afni_friends[] = {
  { "JR Binder"      , ( 1 |     4 | 8 | 16                             ) } ,
  { "EA DeYoe"       , ( 1 |     4 | 8                                  ) } ,
  { "JS Hyde"        , ( 1 | 2              | 32                        ) } ,
  { "SM Rao"         , ( 1 |     4 | 8 | 16           | 128             ) } ,
  { "EA Stein"       , ( 1 | 2 | 4 | 8 | 16           | 128             ) } ,
  { "A Jesmanowicz"  , (             8 |      32                        ) } ,
  { "MS Beauchamp"   , ( 1 | 2 | 4 | 8 | 16 | 32      | 128             ) } ,
  { "MM Klosek"      , ( 1 | 2              | 32                 | 1024 ) } ,
  { "JA Bobholz"     , (             8 | 16 | 32      | 128             ) } ,
  { "JA Frost"       , (             8 | 16                             ) } ,
  { "J Kummer"       , (         4 | 8      | 32                        ) } ,
  { "BD Ward"        , (         4 | 8           | 64       | 512       ) } ,
  { "KM Donahue"     , (                 16                             ) } ,
  { "PA Bandettini"  , (                 16                 | 512       ) } ,
  { "AS Bloom"       , ( 1 | 2         | 16                             ) } ,
  { "T Ross"         , (         4 | 8 | 16 | 32                        ) } ,
  { "H Garavan"      , (         4 | 8 | 16                 | 256       ) } ,
  { "SJ Li"          , (     2                                          ) } ,
  { "ZS Saad"        , (     2 | 4 | 8 | 16      | 64                   ) } ,
  { "K Ropella"      , (     2                                          ) } ,
  { "B Knutson"      , (                 16 |           128             ) } ,
  { "B Biswal"       , (                 16                             ) } ,
  { "RM Birn"        , (             8 | 16 |           128 | 512       ) } ,
  { "V Roopchansingh", (         4 | 8 | 16      | 64                   ) } ,
  { "J Ratke"        , (                 16                             ) } ,
  { "PSF Bellgowan"  , (             8 | 16                             ) } ,
  { "S Durgerian"    , (             8 | 16                             ) } ,
  { "M Belmonte"     , (             8 |           64                   ) } ,
  { "V van Gogh"     , (                      32                        ) } ,
  { "K Bove-Bettis"  , (                 16 |           128             ) } ,
  { "E Kapler"       , (                                128             ) } ,
  { "R Doucette"     , (                           64                   ) } ,
  { "RC Reynolds"    , (                           64       | 512       ) } ,
  { "PP Christidis"  , (                           64                   ) } ,
  { "G Fong"         , (                 16 |           128             ) } ,
  { "LR Frank"       , (             8                                  ) } ,
  { "R Desimone"     , (     2                                          ) }
} ;

#define NUM_FRIENDS (sizeof(afni_friends)/sizeof(AFNI_friend))

/*---------------------------------------------------------------------*/

char * AFNI_get_friend(void)
{
   static char buf[256] ; int nf , nh , hmask , qq=0 ;
   nf = lrand48() % NUM_FRIENDS ;
   do{
      nh = lrand48() % NUM_HELPTYPES ; hmask = 1 << nh ; qq++ ;
   } while( qq < 73 && (hmask & afni_friends[nf].helpmask) == 0 ) ;

   if( nh == YOU_KNOW_WHAT && nf != KLOSEK ) nh = INSPIRATION ; /* only for Gosia */

   sprintf( buf  ,
            "Thanks go to %s for %s" ,
            afni_friends[nf].name , afni_helptypes[nh] ) ;
   return buf ;
}

/*---------------------------------------------------------------------------------*/

#define JAN  1
#define FEB  2
#define MAR  3
#define APR  4
#define MAY  5
#define JUN  6
#define JUL  7
#define AUG  8
#define SEP  9
#define OCT 10
#define NOV 11
#define DEC 12

typedef struct { int mon,day; char *label; } mday ;

static mday holiday[] = {
   {JAN, 1,"New Year's Day"                                          } ,
   {JAN, 3,"John Ronald Reuel Tolkien's birthday"                    } ,
   {JAN, 4,"Isaac Newton's birthday"                                 } ,
   {JAN, 6,"Sherlock Holmes' birthday"                               } ,
   {JAN, 9,"Richard Nixon's birthday"                                } ,
   {JAN,11,"Alexander Hamilton's birthday"                           } ,
   {JAN,15,"Martin Luther King Jr's birthday"                        } ,
   {JAN,16,"David Lloyd George's birthday"                           } ,
   {JAN,17,"Benjamin Franklin's birthday"                            } ,
   {JAN,23,"David Hilbert's birthday"                                } ,
   {JAN,27,"Charles Dodgson's (Lewis Carroll) birthday"              } ,
   {JAN,30,"Franklin Roosevelt's birthday"                           } ,
   {FEB, 8,"William Tecumseh Sherman's birthday"                     } ,
   {FEB,12,"Abraham Lincoln's birthday"                              } ,
   {FEB,14,"Saint Valentine's Day"                                   } ,
   {FEB,15,"Anniversary of release of AFNI to the world!"            } ,
   {FEB,19,"Nikolaus Kopernikus' birthday"                           } ,
   {FEB,21,"Anniversary of the start of the Battle of Verdun"        } ,
   {FEB,22,"George Washington's birthday"                            } ,
   {FEB,29,"Leap Day"                                                } ,
   {MAR, 1,"Frederic Chopin's birthday"                              } ,
   {MAR, 3,"Georg Cantor's birthday"                                 } ,
   {MAR,14,"Albert Einstein's birthday"                              } ,
   {MAR,15,"The Ides of March"                                       } ,
   {MAR,17,"Saint Patrick's Day"                                     } ,
   {MAR,21,"Jean Joseph Fourier's birthday"                          } ,
   {MAR,25,"Anniversary of the Downfall of Sauron"                   } ,
   {MAR,30,"Vincent van Gogh's birthday"                             } ,
   {MAR,31,"Rene Descartes' birthday"                                } ,
   {APR, 1,"April Fool's Day"                                        } ,
   {APR, 9,"Anniversary of Lee's surrender at Appomattox"            } ,
   {APR,12,"Anniversary of attack on Fort Sumter"                    } ,
   {APR,13,"Thomas Jefferson's birthday"                             } ,
   {APR,14,"Anniversary of Lincoln's assasination"                   } ,
   {APR,21,"Queen Elizabeth II's birthday"                           } ,
   {APR,22,"Earth Day"                                               } ,
   {APR,23,"Saint George's Day"                                      } ,
   {APR,25,"Oliver Cromwell's birthday"                              } ,
   {APR,26,"William Shakespeare's birthday"                          } ,
   {APR,27,"Ulysses Grant's birthday"                                } ,
   {APR,28,"Kurt Goedel's birthday"                                  } ,
   {APR,29,"Duke Ellington's birthday"                               } ,
   {APR,30,"Karl Friedrich Gauss's birthday"                         } ,
   {MAY, 7,"Anniversary of sinking of the Lusitania"                 } ,
   {MAY, 8,"VE Day"                                                  } ,
   {MAY,18,"John Paul II's birthday"                                 } ,
   {MAY,22,"Arthur Conan Doyle's birthday"                           } ,
   {MAY,24,"Queen Victoria's birthday"                               } ,
   {MAY,27,"Wild Bill Hickock's birthday"                            } ,
   {MAY,29,"John F Kennedy's birthday"                               } ,
   {MAY,31,"Walt Whitman's birthday"                                 } ,
   {JUN, 5,"John Maynard Keynes' birthday"                           } ,
   {JUN, 8,"Frank Lloyd Wright's birthday"                           } ,
   {JUN,23,"Alan Turing's birthday"                                  } ,
   {JUN,25,"Anniversary of the Battle of the Little Big Horn"        } ,
   {JUL, 1,"Canada Day"                                              } ,
   {JUL, 2,"Anniversay of American Independence"                     } ,
   {JUL, 4,"Anniversary of the Declaration of Independence"          } ,
   {JUL, 7,"Satchel Paige's birthday"                                } ,
   {JUL,13,"Gaius Julius Caesar's birthday"                          } ,
   {JUL,14,"Bastille Day"                                            } ,
   {JUL,28,"Gerard Manley Hopkin's birthday"                         } ,
   {JUL,31,"Anniversary of the battle of Passchendaele"              } ,
   {AUG,15,"Napoleon's birthday"                                     } ,
   {AUG,17,"Pierre de Fermat's birthday"                             } ,
   {AUG,26,"Anniversary of adoption of Nineteenth Amendment"         } ,
   {SEP, 2,"VJ Day"                                                  } ,
   {SEP, 7,"Queen Elizabeth I's birthday"                            } ,
   {SEP, 8,"Anniversary of Star Trek TV premiere"                    } ,
   {SEP,17,"Anniversary of signing of American Constitution"         } ,
   {SEP,22,"Bilbo and Frodo Baggin's birthday"                       } ,
   {SEP,29,"Lech Walesa's birthday"                                  } ,
   {OCT, 6,"Anniversary of attack on Frodo at Weathertop"            } ,
   {OCT,12,"Columbus Day"                                            } ,
   {OCT,14,"Dwight Eisenhower's birthday"                            } ,
   {OCT,19,"Anniversary of Cornwallis' surrender at Yorktown"        } ,
   {OCT,24,"Anniversary of founding of United Nations"               } ,
   {OCT,25,"Saint Crispin's (or Crispian's) Day"                     } ,
   {OCT,27,"Theodore Roosevelt's birthday"                           } ,
   {OCT,31,"Halloween"                                               } ,
   {NOV, 1,"All Saint's Day"                                         } ,
   {NOV,11,"Veteran's and Remembrance Day"                           } ,
   {NOV,12,"Sun Yat Sen's birthday"                                  } ,
   {NOV,22,"Anniversary of Kennedy's assasination"                   } ,
   {NOV,25,"Joe DiMaggio's birthday"                                 } ,
   {NOV,28,"William Blake's birthday"                                } ,
   {NOV,30,"Winston Churchill's birthday"                            } ,
   {DEC, 7,"Anniversary of attack on Pearl Harbor"                   } ,
   {DEC,10,"Ada Lovelace's birthday"                                 } ,
   {DEC,14,"Tycho Brahe's birthday"                                  } ,
   {DEC,19,"William Pitt's (the younger) birthday"                   } ,
   {DEC,22,"Srinivasa Ramanujan's birthday"                          } ,
   {DEC,24,"Adam Mickiewicz's birthday"                              } ,
   {DEC,25,"Christmas Day"                                           } ,
   {DEC,26,"Boxing Day"                                              } ,
   {DEC,27,"Johannes Kepler's birthday"                              } ,
   {DEC,28,"John von Neumann's birthday"                             } ,
   {DEC,31,"New Year's Eve"                                          } ,

 {0,0,NULL} } ;  /* the last element, a flag to stop searching */

/*---------------------------------------------------------------------------------*/

char * AFNI_get_date_trivia(void)
{
   time_t tt ;
   struct tm *lt ;
   int ii ;

   tt = time(NULL) ;
   lt = localtime( &tt ) ;

   for( ii=0 ; holiday[ii].day != 0 ; ii++ )
     if( holiday[ii].mon == lt->tm_mon+1 && holiday[ii].day == lt->tm_mday )
       return holiday[ii].label ;

   return "[not yet claimed]" ;
}
