#include <stdlib.h>
#include <time.h>
#include <stdio.h>

/*-------------------------------------------------------------------------*/
/*! What we might thank people for. */

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

/*-- special codes --*/

#define YOU_KNOW_WHAT 10
#define INSPIRATION    5
#define KLOSEK         7   /* person index below */

#define NUM_HELPTYPES (sizeof(afni_helptypes)/sizeof(char *))

typedef struct { char * name ; int helpmask ; } AFNI_friend ;

/*! Who we might thank. */

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
/*! Return a "thanks" string (static storage - don't free it). */

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
/* 25 Nov 2002: this date in history! */

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

/*! The struct storing date trivia. */

typedef struct { int mon,day; char *label; } mday ;

/*! The data trivia array. */

static mday holiday[] = {
   {JAN, 1,"New Year's Day"                                          } ,
   {JAN, 3,"John Ronald Reuel Tolkien's birthday"                    } ,
   {JAN, 4,"Isaac Newton's birthday"                                 } ,
   {JAN, 5,"King Juan Carlos I's birthday"                           } ,
   {JAN, 6,"Sherlock Holmes' birthday"                               } ,
   {JAN, 7,"Millard Fillmore's birthday"                             } ,
   {JAN, 8,"Stephen Hawking's birthday"                              } ,
   {JAN, 9,"Richard Nixon's birthday"                                } ,
   {JAN,10,"Donald Knuth's birthday"                                 } ,
   {JAN,11,"Alexander Hamilton's birthday"                           } ,
   {JAN,14,"Albert Schweitzer's birthday"                            } ,
   {JAN,15,"Martin Luther King Jr's birthday"                        } ,
   {JAN,16,"David Lloyd George's birthday"                           } ,
   {JAN,17,"Benjamin Franklin's birthday"                            } ,
   {JAN,18,"Cary Grants' birthday"                                   } ,
   {JAN,19,"Edgar Allen Poe's birthday"                              } ,
   {JAN,20,"Anniversary of end of American Revolution"               } ,
   {JAN,23,"David Hilbert's birthday"                                } ,
   {JAN,24,"Anniversary of beer in cans"                             } ,
   {JAN,27,"Charles Dodgson's (Lewis Carroll) birthday"              } ,
   {JAN,28,"Anniversary of Challenger explosion"                     } ,
   {JAN,30,"Franklin Roosevelt's birthday"                           } ,
   {JAN,31,"Queen Beatrix's birthday"                                } ,
   {FEB, 2,"Charles Maurice de Talleyrand's birthday"                } ,
   {FEB, 3,"The Day the Music Died"                                  } ,
   {FEB, 4,"Charles Lindbergh's birthday"                            } ,
   {FEB, 6,"Ronald Reagan's birthday"                                } ,
   {FEB, 7,"Thomas More's birthday"                                  } ,
   {FEB, 8,"William Tecumseh Sherman's birthday"                     } ,
   {FEB,11,"Josiah Willard Gibb's birthday"                          } ,
   {FEB,12,"Abraham Lincoln's birthday"                              } ,
   {FEB,14,"Saint Valentine's Day"                                   } ,
   {FEB,15,"Anniversary of release of AFNI!"                         } ,
   {FEB,19,"Nikolaus Kopernikus' birthday"                           } ,
   {FEB,20,"Anniversary of John Glenn's spaceflight"                 } ,
   {FEB,21,"Anniversary of start of the Battle of Verdun"            } ,
   {FEB,22,"George Washington's birthday"                            } ,
   {FEB,25,"Auguste Renoir's birthday"                               } ,
   {FEB,28,"Linus Pauling's birthday"                                } ,
   {FEB,29,"Herman Hollerith's birthday"                             } ,
   {MAR, 1,"Frederic Chopin's birthday"                              } ,
   {MAR, 2,"Samuel Houston's birthday"                               } ,
   {MAR, 3,"Georg Cantor's birthday"                                 } ,
   {MAR, 5,"Anniversary of Boston Massacre"                          } ,
   {MAR, 6,"Michelangelo Buonarroti's birthday"                      } ,
   {MAR,12,"Kemal Ataturk's birthday"                                } ,
   {MAR,14,"Albert Einstein's birthday"                              } ,
   {MAR,15,"The Ides of March"                                       } ,
   {MAR,16,"James Madison's birthday"                                } ,
   {MAR,17,"Saint Patrick's Day"                                     } ,
   {MAR,20,"Anniversary of Uncle Tom's Cabin"                        } ,
   {MAR,21,"Jean Baptiste Joseph Fourier's birthday"                 } ,
   {MAR,23,"Emmy Noether's birthday"                                 } ,
   {MAR,25,"Anniversary of Downfall of Sauron"                       } ,
   {MAR,26,"Robert Frost's birthday"                                 } ,
   {MAR,28,"Pierre Simon de Laplace's birthday"                      } ,
   {MAR,30,"Vincent van Gogh's birthday"                             } ,
   {MAR,31,"Rene Descartes' birthday"                                } ,
   {APR, 1,"April Fool's Day"                                        } ,
   {APR, 2,"Charlemagne's birthday"                                  } ,
   {APR, 4,"Anniversary of founding of NATO"                         } ,
   {APR, 8,"Birthday of Siddhartha Gautama"                          } ,
   {APR, 9,"Anniversary of Lee's surrender at Appomattox"            } ,
   {APR,10,"Anniversary of loss of USS Thresher"                     } ,
   {APR,12,"Anniversary of Yuri Gagarin's spaceflight"               } ,
   {APR,13,"Thomas Jefferson's birthday"                             } ,
   {APR,14,"Anniversary of Lincoln's assasination"                   } ,
   {APR,15,"Anniversary of sinking of Titanic"                       } ,
   {APR,18,"Anniversary of Paul Revere's Ride"                       } ,
   {APR,19,"Anniversary of Warsaw Ghetto Uprising"                   } ,
   {APR,21,"Queen Elizabeth II's birthday"                           } ,
   {APR,22,"Earth Day"                                               } ,
   {APR,23,"Saint George's Day"                                      } ,
   {APR,24,"Henri Philippe Petain's birthday"                        } ,
   {APR,25,"Oliver Cromwell's birthday"                              } ,
   {APR,26,"William Shakespeare's birthday"                          } ,
   {APR,27,"Ulysses Grant's birthday"                                } ,
   {APR,28,"Kurt Goedel's birthday"                                  } ,
   {APR,29,"Duke Ellington's birthday"                               } ,
   {APR,30,"Karl Friedrich Gauss's birthday"                         } ,
   {MAY, 1,"Emperor Claudius's birthday"                             } ,
   {MAY, 3,"Anniversary of Polish Constitution"                      } ,
   {MAY, 5,"Henryk Sienkiewicz's birthday"                           } ,
   {MAY, 6,"Anniversary of Hindenburg explosion"                     } ,
   {MAY, 7,"Anniversary of sinking of Lusitania"                     } ,
   {MAY, 8,"VE Day"                                                  } ,
   {MAY,14,"Anniversary of founding of Israel"                       } ,
   {MAY,17,"National Day of Norway"                                  } ,
   {MAY,18,"Pope John Paul II's birthday"                            } ,
   {MAY,19,"Malcom X's birthday"                                     } ,
   {MAY,20,"John Stuart Mill's birthday"                             } ,
   {MAY,21,"Andrei Sakharov's birthday"                              } ,
   {MAY,22,"Arthur Conan Doyle's birthday"                           } ,
   {MAY,23,"John Bardeen's birthday"                                 } ,
   {MAY,24,"Oliver Cromwell's birthday"                              } ,
   {MAY,27,"Wild Bill Hickock's birthday"                            } ,
   {MAY,28,"Ian Fleming's birthday"                                  } ,
   {MAY,29,"John F Kennedy's birthday"                               } ,
   {MAY,31,"Walt Whitman's birthday"                                 } ,
   {JUN, 1,"Marilyn Monroe's birthday"                               } ,
   {JUN, 4,"Anniversary of Battle of Midway"                         } ,
   {JUN, 5,"John Maynard Keynes' birthday"                           } ,
   {JUN, 6,"D-Day"                                                   } ,
   {JUN, 8,"Frank Lloyd Wright's birthday"                           } ,
   {JUN, 9,"Anniversary of Donald Duck's debut"                      } ,
   {JUN,15,"Anniversary of Magna Carta"                              } ,
   {JUN,16,"Geronimo's birthday"                                     } ,
   {JUN,17,"Anniversary of Battle of Bunker's Hill"                  } ,
   {JUN,19,"Juneteenth"                                              } ,
   {JUN,20,"Anniversary of Oxford University"                        } ,
   {JUN,21,"Alexander the Great's birthday"                          } ,
   {JUN,22,"Anniversary of saxophone invention"                      } ,
   {JUN,23,"Alan Turing's birthday"                                  } ,
   {JUN,25,"Anniversary of Battle of the Little Big Horn"            } ,
   {JUL, 1,"Canada Day"                                              } ,
   {JUL, 2,"Anniversay of American Independence"                     } ,
   {JUL, 3,"Franz Kafka's birthday"                                  } ,
   {JUL, 4,"Anniversary of Declaration of Independence"              } ,
   {JUL, 5,"Anniversary of Newton's Principia"                       } ,
   {JUL, 7,"Satchel Paige's birthday"                                } ,
   {JUL,13,"Gaius Julius Caesar's birthday"                          } ,
   {JUL,14,"Bastille Day"                                            } ,
   {JUL,15,"Anniversary of Battle of Grunwald"                       } ,
   {JUL,16,"Anniversary of 1st atomic explosion"                     } ,
   {JUL,17,"Anniversary of Disneyland's opening"                     } ,
   {JUL,18,"Nelson Mandela's birthday"                               } ,
   {JUL,20,"Anniversary of first landing on Moon"                    } ,
   {JUL,22,"Friedrich Bessel's birthday"                             } ,
   {JUL,24,"Simon Bolivar's birthday"                                } ,
   {JUL,28,"Gerard Manley Hopkin's birthday"                         } ,
   {JUL,30,"Anniversary of Amistad uprising"                         } ,
   {JUL,31,"Anniversary of Battle of Passchendaele"                  } ,
   {AUG, 1,"Anniversary of Warsaw Uprising"                          } ,
   {AUG, 4,"Percy Bysshe Shelley's birthday"                         } ,
   {AUG, 9,"Anniversary of Richard Nixon's resignation"              } ,
   {AUG,12,"Erwin Schrodinger's birthday"                            } ,
   {AUG,15,"Napoleon's birthday"                                     } ,
   {AUG,17,"Pierre de Fermat's birthday"                             } ,
   {AUG,19,"Gene Roddenberry's birthday"                             } ,
   {AUG,22,"Anniversary of Battle of Bosworth Field"                 } ,
   {AUG,24,"William Wilberforce's birthday"                          } ,
   {AUG,26,"Anniversary of adoption of Nineteenth Amendment"         } ,
   {AUG,27,"Confucius' birthday"                                     } ,
   {AUG,31,"Anniversary of Solidarity's creation"                    } ,
   {SEP, 1,"Anniversary of Nazi invasion of Poland"                  } ,
   {SEP, 2,"VJ Day"                                                  } ,
   {SEP, 7,"Queen Elizabeth I's birthday"                            } ,
   {SEP, 8,"Anniversary of Star Trek TV premiere"                    } ,
   {SEP,11,"Anniversary of Day of Burning"                           } ,
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
   {NOV, 2,"Daniel Boone's birthday"                                 } ,
   {NOV,10,"Martin Luther's birthday"                                } ,
   {NOV,11,"Veteran's and Remembrance Day"                           } ,
   {NOV,12,"Sun Yat Sen's birthday"                                  } ,
   {NOV,22,"Anniversary of Kennedy's assasination"                   } ,
   {NOV,25,"Joe DiMaggio's birthday"                                 } ,
   {NOV,26,"Anniversary of premiere of Casablanca"                   } ,
   {NOV,27,"Start of First Crusade"                                  } ,
   {NOV,28,"William Blake's birthday"                                } ,
   {NOV,29,"Clive Staples Lewis's birthday"                          } ,
   {NOV,30,"Winston Churchill's birthday"                            } ,
   {DEC, 1,"Anniversary of Rosa Parks' arrest"                       } ,
   {DEC, 2,"Anniversary of Monroe Doctrine"                          } ,
   {DEC, 3,"Anniversary of Galileo's telescope"                      } ,
   {DEC, 4,"Crazy Horse's birthday"                                  } ,
   {DEC, 5,"George Armstrong Custer's birthday"                      } ,
   {DEC, 6,"Spanish Constitution Day"                                } ,
   {DEC, 7,"Anniversary of attack on Pearl Harbor"                   } ,
   {DEC,10,"Ada Lovelace's birthday"                                 } ,
   {DEC,11,"Aleksander Solzhenitsyn's birthday"                      } ,
   {DEC,12,"Frank Sinatra's birthday"                                } ,
   {DEC,14,"Tycho Brahe's birthday"                                  } ,
   {DEC,15,"Anniversary of adoption of US Bill of Rights"            } ,
   {DEC,16,"Anniversary of Boston Tea Party"                         } ,
   {DEC,17,"Anniversary of Wright brother's flight"                  } ,
   {DEC,19,"William Pitt's (younger) birthday"                       } ,
   {DEC,20,"Robert Menzies' birthday"                                } ,
   {DEC,21,"Anniversary of Pilgrim's landing"                        } ,
   {DEC,22,"Srinivasa Ramanujan's birthday"                          } ,
   {DEC,23,"Akihito's birthday"                                      } ,
   {DEC,24,"Adam Mickiewicz's birthday"                              } ,
   {DEC,25,"Christmas Day"                                           } ,
   {DEC,26,"Boxing Day"                                              } ,
   {DEC,27,"Johannes Kepler's birthday"                              } ,
   {DEC,28,"John von Neumann's birthday"                             } ,
   {DEC,30,"Tiger Woods' birthday"                                   } ,
   {DEC,31,"New Year's Eve"                                          } ,

 {0,0,NULL} } ;  /* the last element, a flag to stop searching */

/*---------------------------------------------------------------------------------*/
/*! Return today's date trivia string. */

char * AFNI_get_date_trivia(void)
{
   time_t tt ;
   struct tm *lt ;
   int ii ;

   tt = time(NULL) ;         /* seconds since 01 Jan 1970 */
   lt = localtime( &tt ) ;   /* break into pieces */

   /* find this month and day in the trivial list, if present */

   for( ii=0 ; holiday[ii].day != 0 ; ii++ )
     if( holiday[ii].mon == lt->tm_mon+1 && holiday[ii].day == lt->tm_mday )
       return holiday[ii].label ;

   /* default trivia */

   return "[not yet claimed]" ;
}
