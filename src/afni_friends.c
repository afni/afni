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


/*! max # trivia elements allowed per date */

#define NTMAX 9

/*! The date trivia array. */

static mday holiday[] = {
   {JAN, 1,"Anniversary of Emancipation Proclamation"                } ,  /* WW */
   {JAN, 1,"New Year's Day"                                          } ,
   {JAN, 1,"Lorenzo de Medici's birthday"                            } ,
   {JAN, 1,"Paul Revere's birthday"                                  } ,
   {JAN, 1,"Cameroon Independence Day"                               } ,
   {JAN, 1,"Haiti Independence Day"                                  } ,
   {JAN, 1,"Western Samoa Independence Day"                          } ,
   {JAN, 1,"Sudan Independence Day"                                  } ,
   {JAN, 2,"Isaac Asimov's birthday"                                 } ,
   {JAN, 3,"John Ronald Reuel Tolkien's birthday"                    } ,
   {JAN, 3,"Anniversary of Battle of Princeton"                      } ,
   {JAN, 4,"Burma Independence Day"                                  } ,
   {JAN, 5,"King Juan Carlos I's birthday"                           } ,
   {JAN, 5,"Konrad Adenauer's birthday"                              } ,
   {JAN, 6,"Sherlock Holmes' birthday"                               } ,
   {JAN, 6,"Jeanne d'Arc's birthday"                                 } ,
   {JAN, 7,"Millard Fillmore's birthday"                             } ,
   {JAN, 8,"Stephen Hawking's birthday"                              } ,
   {JAN, 8,"Elvis Presley's birthday"                                } ,
   {JAN, 9,"Richard Nixon's birthday"                                } ,
   {JAN,10,"Donald Knuth's birthday"                                 } ,
   {JAN,11,"Alexander Hamilton's birthday"                           } ,
   {JAN,11,"Chad Independence Day"                                   } ,
   {JAN,14,"Albert Schweitzer's birthday"                            } ,
   {JAN,15,"Martin Luther King Jr's birthday"                        } ,
   {JAN,15,"Chuck Berry's birthday"                                  } ,
   {JAN,16,"David Lloyd George's birthday"                           } ,
   {JAN,16,"Anniversary of Prohibition's start"                      } ,
   {JAN,17,"Benjamin Franklin's birthday"                            } ,
   {JAN,17,"Muhammad Ali's birthday"                                 } ,
   {JAN,18,"Cary Grants' birthday"                                   } ,
   {JAN,18,"Daniel Webster's birthday"                               } ,
   {JAN,19,"Edgar Allen Poe's birthday"                              } ,
   {JAN,20,"Anniversary of end of American Revolution"               } ,
   {JAN,20,"George Burns' birthday"                                  } ,
   {JAN,23,"David Hilbert's birthday"                                } ,
   {JAN,23,"Humphrey Bogart's birthday"                              } ,
   {JAN,24,"Anniversary of beer in cans"                             } ,
   {JAN,24,"Emperor Hadrian's birthday"                              } ,
   {JAN,25,"Somerset Maugham's birthday"                             } ,
   {JAN,26,"Anniversary of First Fleet to Botany Bay"                } ,
   {JAN,27,"Charles Dodgson's (Lewis Carroll) birthday"              } ,
   {JAN,27,"Wolfgang Amadeus Mozart's birthday"                      } ,
   {JAN,28,"Anniversary of Challenger explosion"                     } ,
   {JAN,30,"Franklin Delano Roosevelt's birthday"                    } ,
   {JAN,31,"Queen Beatrix's birthday"                                } ,
   {JAN,31,"Jackie Robinson's birthday"                              } ,
   {JAN,31,"Nauru Independence Day"                                  } ,

   {FEB, 1,"Clark Gable's birthday"                                  } ,
   {FEB, 2,"Charles Maurice de Talleyrand's birthday"                } ,
   {FEB, 3,"The Day the Music Died"                                  } ,
   {FEB, 4,"Charles Lindbergh's birthday"                            } ,
   {FEB, 4,"Sri Lanka Independence Day"                              } ,
   {FEB, 4,"Tadeusz Kosciusko's birthday"                            } ,
   {FEB, 6,"Ronald Reagan's birthday"                                } ,
   {FEB, 6,"New Zealand Day"                                         } ,
   {FEB, 7,"Thomas More's birthday"                                  } ,
   {FEB, 7,"Grenada Independence Day"                                } ,
   {FEB, 8,"William Tecumseh Sherman's birthday"                     } ,
   {FEB,11,"Josiah Willard Gibb's birthday"                          } ,
   {FEB,11,"Iran National Day"                                       } ,
   {FEB,12,"Abraham Lincoln's birthday"                              } ,
   {FEB,14,"Saint Valentine's Day"                                   } ,
   {FEB,15,"Anniversary of AFNI's release!"                          } ,
   {FEB,15,"Galileo Galilei's birthday"                              } ,
   {FEB,18,"Gambia Independence Day"                                 } ,
   {FEB,19,"Nikolaus Kopernikus' birthday"                           } ,
   {FEB,20,"Anniversary of John Glenn's spaceflight"                 } ,
   {FEB,21,"Anniversary of start of the Battle of Verdun"            } ,
   {FEB,22,"George Washington's birthday"                            } ,
   {FEB,25,"Auguste Renoir's birthday"                               } ,
   {FEB,27,"Dominican Republic Independence Day"                     } ,
   {FEB,27,"Anniversary of Lincoln's Cooper Union Speech"            } ,
   {FEB,28,"Linus Pauling's birthday"                                } ,
   {FEB,29,"Herman Hollerith's birthday"                             } ,
   {FEB,29,"W E B Dubois' birthday"                                  } ,

   {MAR, 1,"Frederic Chopin's birthday"                              } ,
   {MAR, 1,"Santiago Ramon y Cajal's birthday"                       } ,
   {MAR, 2,"Samuel Houston's birthday"                               } ,
   {MAR, 3,"Georg Cantor's birthday"                                 } ,
   {MAR, 3,"Morocco National Day"                                    } ,
   {MAR, 5,"Anniversary of Boston Massacre"                          } ,
   {MAR, 6,"Michelangelo Buonarroti's birthday"                      } ,
   {MAR, 8,"Kenneth Grahame's birthday"                              } ,
   {MAR,12,"Kemal Ataturk's birthday"                                } ,
   {MAR,12,"Mauritius  Independence Day"                             } ,
   {MAR,14,"Albert Einstein's birthday"                              } ,
   {MAR,15,"The Ides of March"                                       } ,
   {MAR,16,"James Madison's birthday"                                } ,
   {MAR,17,"Saint Patrick's Day"                                     } ,
   {MAR,19,"Saint Joseph's Day"                                      } ,
   {MAR,19,"Liechtenstein National Day"                              } ,
   {MAR,20,"Anniversary of Uncle Tom's Cabin"                        } ,
   {MAR,21,"Jean Baptiste Joseph Fourier's birthday"                 } ,
   {MAR,23,"Emmy Noether's birthday"                                 } ,
   {MAR,23,"Pakistan Day"                                            } ,
   {MAR,25,"Anniversary of Downfall of Sauron"                       } ,
   {MAR,25,"Greece Independence Day"                                 } ,
   {MAR,26,"Bangladesh Independence Day"                             } ,
   {MAR,26,"Robert Frost's birthday"                                 } ,
   {MAR,28,"Pierre Simon de Laplace's birthday"                      } ,
   {MAR,30,"Vincent van Gogh's birthday"                             } ,
   {MAR,31,"Rene Descartes' birthday"                                } ,

   {APR, 1,"April Fool's Day"                                        } ,
   {APR, 2,"Charlemagne's birthday"                                  } ,
   {APR, 4,"Anniversary of founding of NATO"                         } ,
   {APR, 4,"Hungary Liberation Day"                                  } ,
   {APR, 4,"Senegal Independence Day"                                } ,
   {APR, 4,"Yamamoto Isoroku's birthday"                             } ,
   {APR, 8,"Siddhartha Gautama's birthday"                           } ,
   {APR, 9,"Anniversary of Lee's surrender at Appomattox"            } ,
   {APR,10,"Anniversary of loss of USS Thresher"                     } ,
   {APR,12,"Anniversary of Yuri Gagarin's spaceflight"               } ,
   {APR,13,"Thomas Jefferson's birthday"                             } ,
   {APR,14,"Anniversary of Lincoln's assasination"                   } ,
   {APR,15,"Anniversary of sinking of Titanic"                       } ,
   {APR,15,"Arnold Toynbee's birthday"                               } ,
   {APR,17,"Syria Independence Day"                                  } ,
   {APR,18,"Anniversary of Paul Revere's Ride"                       } ,
   {APR,19,"Anniversary of Warsaw Ghetto Uprising"                   } ,
   {APR,21,"Queen Elizabeth II's birthday"                           } ,
   {APR,22,"Earth Day"                                               } ,
   {APR,23,"Saint George's Day"                                      } ,
   {APR,24,"Henri Philippe Petain's birthday"                        } ,
   {APR,25,"Oliver Cromwell's birthday"                              } ,
   {APR,26,"William Shakespeare's birthday"                          } ,
   {APR,26,"David Hume's birthday"                                   } ,
   {APR,27,"Ulysses Grant's birthday"                                } ,
   {APR,27,"Sierra Leone Independence Day"                           } ,
   {APR,27,"Togo Independence Day"                                   } ,
   {APR,28,"Kurt Goedel's birthday"                                  } ,
   {APR,29,"Duke Ellington's birthday"                               } ,
   {APR,30,"Karl Friedrich Gauss's birthday"                         } ,

   {MAY, 1,"Emperor Claudius's birthday"                             } ,
   {MAY, 3,"Anniversary of Polish Constitution"                      } ,
   {MAY, 5,"Henryk Sienkiewicz's birthday"                           } ,
   {MAY, 6,"Anniversary of Hindenburg explosion"                     } ,
   {MAY, 7,"Anniversary of sinking of Lusitania"                     } ,
   {MAY, 8,"VE Day"                                                  } ,
   {MAY, 8,"Harry S Truman's birthday"                               } ,
   {MAY,11,"Laos Constitution Day"                                   } ,
   {MAY,11,"Richard Feynman's birthday"                              } ,
   {MAY,14,"Paraguay Independence Day"                               } ,
   {MAY,15,"Israel Independence Day"                                 } ,
   {MAY,17,"Norway National Day"                                     } ,
   {MAY,18,"Pope John Paul II's birthday"                            } ,
   {MAY,19,"Malcom X's birthday"                                     } ,
   {MAY,20,"John Stuart Mill's birthday"                             } ,
   {MAY,20,"Cuba Independence Day"                                   } ,
   {MAY,21,"Andrei Sakharov's birthday"                              } ,
   {MAY,22,"Arthur Conan Doyle's birthday"                           } ,
   {MAY,23,"John Bardeen's birthday"                                 } ,
   {MAY,24,"Oliver Cromwell's birthday"                              } ,
   {MAY,25,"Argentina Revolution Day"                                } ,
   {MAY,25,"Jordan Independence Day"                                 } ,
   {MAY,26,"Guyana Independence Day"                                 } ,
   {MAY,27,"Wild Bill Hickock's birthday"                            } ,
   {MAY,28,"Ian Fleming's birthday"                                  } ,
   {MAY,29,"John F Kennedy's birthday"                               } ,
   {MAY,31,"Walt Whitman's birthday"                                 } ,
   {MAY,31,"South Africa Republic Day"                               } ,

   {JUN, 1,"Marilyn Monroe's birthday"                               } ,
   {JUN, 1,"Tunisia National Day"                                    } ,
   {JUN, 2,"Italy National Day"                                      } ,
   {JUN, 4,"Anniversary of Battle of Midway"                         } ,
   {JUN, 5,"John Maynard Keynes' birthday"                           } ,
   {JUN, 5,"Seychelles Independence Day"                             } ,
   {JUN, 6,"D-Day"                                                   } ,
   {JUN, 8,"Frank Lloyd Wright's birthday"                           } ,
   {JUN, 9,"Anniversary of Donald Duck's debut"                      } ,
   {JUN,10,"Portugal National Day"                                   } ,
   {JUN,12,"Philippines Independence Day"                            } ,
   {JUN,15,"Anniversary of Magna Carta"                              } ,
   {JUN,15,"Anniversary of US-British 'Pig War'"                     } ,
   {JUN,16,"Geronimo's birthday"                                     } ,
   {JUN,17,"Anniversary of Battle of Bunker's Hill"                  } ,
   {JUN,17,"Iceland Republic Day"                                    } ,
   {JUN,19,"Juneteenth"                                              } ,
   {JUN,19,"Kuwait National Day"                                     } ,
   {JUN,20,"Anniversary of Oxford University"                        } ,
   {JUN,21,"Alexander the Great's birthday"                          } ,
   {JUN,22,"Anniversary of saxophone invention"                      } ,
   {JUN,23,"Alan Turing's birthday"                                  } ,
   {JUN,23,"Luxembourg Grand Duke Day"                               } ,
   {JUN,25,"Anniversary of Battle of the Little Big Horn"            } ,
   {JUN,25,"Mozambique Independence Day"                             } ,
   {JUN,26,"Malagasy Republic Independence Day"                      } ,
   {JUN,27,"Djibouti Independence Day"                               } ,

   {JUL, 1,"Canada Day"                                              } ,  /* WW */
   {JUL, 1,"Anniversary of Battle of the Somme"                      } ,
   {JUL, 1,"Gottfried Wilhelm Leibniz's birthday"                    } ,
   {JUL, 1,"Burundi Independence Day"                                } ,
   {JUL, 1,"Ghana Republic Day"                                      } ,
   {JUL, 1,"Rwanda Independence Day"                                 } ,
   {JUL, 1,"Somalia Independence Day"                                } ,
   {JUL, 2,"Anniversay of American Independence"                     } ,
   {JUL, 2,"Thurgood Marshall's birthday"                            } ,
   {JUL, 3,"Franz Kafka's birthday"                                  } ,
   {JUL, 4,"Anniversary of Declaration of Independence"              } ,
   {JUL, 4,"Anniversary of Vickburg's surrender"                     } ,
   {JUL, 4,"Rube Goldberg's birthday"                                } ,
   {JUL, 5,"Anniversary of Newton's Principia"                       } ,
   {JUL, 5,"Anniversary of introduction of bikini"                   } ,
   {JUL, 5,"Venezuela Independence Day"                              } ,
   {JUL, 6,"Malawi Independence Day"                                 } ,
   {JUL, 7,"Satchel Paige's birthday"                                } ,
   {JUL, 7,"Robert Heinlein's birthday"                              } ,
   {JUL,10,"Bahamas Independence Day"                                } ,
   {JUL,11,"Mongolia Revolution Day"                                 } ,
   {JUL,12,"Sao Tome and Principe Independence Day"                  } ,
   {JUL,13,"Gaius Julius Caesar's birthday"                          } ,
   {JUL,14,"Bastille Day"                                            } ,
   {JUL,14,"Iraq Republic Day"                                       } ,
   {JUL,14,"Gerald Ford's birthday"                                  } ,
   {JUL,15,"Anniversary of Battle of Grunwald"                       } ,
   {JUL,16,"Anniversary of 1st atomic explosion"                     } ,
   {JUL,16,"Anniversary of Apollo 11's launch"                       } ,
   {JUL,17,"Anniversary of Disneyland's opening"                     } ,
   {JUL,17,"South Korea Constitution Day"                            } ,
   {JUL,18,"John Glenn's birthday"                                   } ,
   {JUL,18,"Nelson Mandela's birthday"                               } ,
   {JUL,18,"Spain National Day"                                      } ,
   {JUL,20,"Anniversary of Apollo 11 Moon landing"                   } ,
   {JUL,20,"Edmund Hillary's birthday"                               } ,
   {JUL,20,"Columbia Independence Day"                               } ,
   {JUL,21,"Belgium Independence Day"                                } ,
   {JUL,21,"Martyr's Day in Bolivia"                                 } ,
   {JUL,22,"Friedrich Bessel's birthday"                             } ,
   {JUL,22,"Poland Liberation Day"                                   } ,
   {JUL,22,"Gregor Mendel's birthday"                                } ,
   {JUL,23,"Raymond Chandler's birthday"                             } ,
   {JUL,23,"Egypt National Day"                                      } ,
   {JUL,24,"Simon Bolivar's birthday"                                } ,
   {JUL,24,"Anniversary of Apollo 11's return to Earth"              } ,
   {JUL,24,"Amelia Earhart's birthday"                               } ,
   {JUL,25,"Arthur Balfour's birthday"                               } ,
   {JUL,26,"Maldives Independence Day"                               } ,
   {JUL,28,"Gerard Manley Hopkin's birthday"                         } ,
   {JUL,28,"Peru Independence Day"                                   } ,
   {JUL,30,"Anniversary of Amistad uprising"                         } ,
   {JUL,30,"Vanuatu Independence Day"                                } ,
   {JUL,31,"Anniversary of Battle of Passchendaele"                  } ,
   {JUL,31,"Primo Levi's birthday"                                   } ,
   {JUL,31,"Joanne Kathleen Rowling's birthday"                      } ,

   {AUG, 1,"Anniversary of Warsaw Uprising"                          } ,
   {AUG, 1,"Dahomey Independence Day"                                } ,
   {AUG, 1,"Switzerland Confederation Day"                           } ,
   {AUG, 1,"Jerry Garcia's birthday"                                 } ,
   {AUG, 4,"Percy Bysshe Shelley's birthday"                         } ,
   {AUG, 5,"Upper Volta Independence Day"                            } ,
   {AUG, 5,"Neil Armstrong's birthday"                               } ,
   {AUG, 6,"Bolivia Independence Day"                                } ,
   {AUG, 6,"Alexander Fleming's birthday"                            } ,
   {AUG, 7,"Ivory Coast Independence Day"                            } ,
   {AUG, 9,"Anniversary of Richard Nixon's resignation"              } ,
   {AUG, 9,"Singapore National Day"                                  } ,
   {AUG,10,"Ecuador Independence Day"                                } ,
   {AUG,12,"Erwin Schrodinger's birthday"                            } ,
   {AUG,14,"Pakistan Independence Day"                               } ,
   {AUG,15,"Napoleon's birthday"                                     } ,
   {AUG,15,"India Independence Day"                                  } ,
   {AUG,16,"Cyprus Independence Day"                                 } ,
   {AUG,17,"Pierre de Fermat's birthday"                             } ,
   {AUG,17,"Gabon Independence Day"                                  } ,
   {AUG,17,"Indonesia Independence Day"                              } ,
   {AUG,18,"Afghanistan Independence Day"                            } ,
   {AUG,19,"Gene Roddenberry's birthday"                             } ,
   {AUG,22,"Anniversary of Battle of Bosworth Field"                 } ,
   {AUG,23,"Rumania Liberation Day"                                  } ,
   {AUG,23,"Gene Kelley's birthday"                                  } ,
   {AUG,24,"William Wilberforce's birthday"                          } ,
   {AUG,25,"Uruguay Independence Day"                                } ,
   {AUG,26,"Anniversary of 19th Amendment's adoption"                } ,
   {AUG,27,"Confucius' birthday"                                     } ,
   {AUG,27,"Charles Gates Dawes' birthday"                           } ,
   {AUG,31,"Anniversary of Solidarity's creation"                    } ,
   {AUG,31,"Trinidad and Tobago Independence Day"                    } ,

   {SEP, 1,"Anniversary of Nazi invasion of Poland"                  } ,
   {SEP, 2,"VJ Day"                                                  } ,
   {SEP, 2,"Queen Liliuokalani's birthday"                           } ,
   {SEP, 3,"Qatar Independence Day"                                  } ,
   {SEP, 3,"San Marino Anniversary of Founding"                      } ,
   {SEP, 6,"Swaziland Independence Day"                              } ,
   {SEP, 7,"Queen Elizabeth I's birthday"                            } ,
   {SEP, 7,"Brazil Independence Day"                                 } ,
   {SEP, 7,"Sinclair Lewis's birthday"                               } ,
   {SEP, 7,"Buddy Holly's birthday"                                  } ,
   {SEP, 7,"Anniversary of Battle of Borodino"                       } ,
   {SEP, 7,"Luigi Galvani's birthday"                                } ,
   {SEP, 8,"Anniversary of Star Trek TV debut"                       } ,
   {SEP, 8,"Andorra National Festival"                               } ,
   {SEP, 9,"Bulgaria Liberation Day"                                 } ,
   {SEP,11,"Anniversary of Day of Burning"                           } ,
   {SEP,12,"Anniversary of Battle of Vienna"                         } ,
   {SEP,14,"Jan Masaryk's birthday"                                  } ,
   {SEP,15,"Costa Rica Independence Day"                             } ,
   {SEP,15,"El Salvador Independence Day"                            } ,
   {SEP,15,"Guatemala Independence Day"                              } ,
   {SEP,15,"Honduras Independence Day"                               } ,
   {SEP,15,"Nicaragua Independence Day"                              } ,
   {SEP,16,"Malaysia Independence Day"                               } ,
   {SEP,16,"Papua New Guinea Independence Day"                       } ,
   {SEP,17,"Anniversary of signing of American Constitution"         } ,
   {SEP,18,"Mexico Independence Day"                                 } ,
   {SEP,19,"Anniversary of Battle of Saratoga"                       } ,
   {SEP,20,"Anniversary of Battle of Chalons"                        } ,
   {SEP,21,"Chile Independence Day"                                  } ,
   {SEP,21,"Belize Independence Day"                                 } ,
   {SEP,21,"Malta Independence Day"                                  } ,
   {SEP,22,"Bilbo and Frodo Baggin's birthday"                       } ,
   {SEP,22,"Mali Republic Day"                                       } ,
   {SEP,23,"Saudi Arabia National Day"                               } ,
   {SEP,23,"Emperor Augustus Caesar's birthday"                      } ,
   {SEP,26,"Yemen National Day"                                      } ,
   {SEP,27,"Anniversary of founding of Jesuit Order"                 } ,
   {SEP,29,"Lech Walesa's birthday"                                  } ,
   {SEP,30,"Botswana Independence Day"                               } ,

   {OCT, 1,"Nigeria Independence Day"                                } ,
   {OCT, 2,"Guinea Independence Day"                                 } ,
   {OCT, 2,"Anniversary of Saladin's capture of Jerusalem"           } ,
   {OCT, 2,"Groucho Marx's birthday"                                 } ,
   {OCT, 3,"Anniversary of reunification of Germany"                 } ,
   {OCT, 6,"Anniversary of attack on Frodo at Weathertop"            } ,
   {OCT, 9,"Alfred Dreyfus's birthday"                               } ,
   {OCT,10,"Anniversary of Battle of Tours"                          } ,
   {OCT,12,"Columbus Day"                                            } ,
   {OCT,14,"Dwight Eisenhower's birthday"                            } ,
   {OCT,15,"Pelham Grenville Wodehouse's birthday"                   } ,
   {OCT,16,"Oscar Wilde's birthday"                                  } ,
   {OCT,19,"Anniversary of Cornwallis' surrender at Yorktown"        } ,
   {OCT,21,"Anniversary of Battle of Trafalgar"                      } ,
   {OCT,24,"Anniversary of founding of United Nations"               } ,
   {OCT,24,"Zambia Independence Day"                                 } ,
   {OCT,25,"Saint Crispin's (or Crispian's) Day"                     } ,
   {OCT,26,"Anniversary of Gunfight at the OK Corral"                } ,
   {OCT,27,"Theodore Roosevelt's birthday"                           } ,
   {OCT,29,"Turkey Republic Day"                                     } ,
   {OCT,31,"Halloween"                                               } ,

   {NOV, 1,"All Saint's Day"                                         } ,  /* WW */
   {NOV, 1,"Anniversary of Great Lisbon Earthquake"                  } ,
   {NOV, 2,"Marie Antoinette's birthday"                             } ,
   {NOV, 2,"Anniversary of Balfour Declaration"                      } ,
   {NOV, 2,"Daniel Boone's birthday"                                 } ,
   {NOV, 3,"Panama Independence Day"                                 } ,
   {NOV, 3,"Dominica Independence Day"                               } ,
   {NOV, 5,"Guy Fawkes Day"                                          } ,
   {NOV, 6,"John Philip Sousa's birthday"                            } ,
   {NOV, 6,"Dominican Republic Constitution Day"                     } ,
   {NOV, 7,"Maria Sklodowska Curie's birthday"                       } ,
   {NOV, 7,"Lise Meitner's birthday"                                 } ,
   {NOV, 8,"Anniversary of discovery of X-rays"                      } ,
   {NOV, 8,"Anniversary of Louvre museum opening"                    } ,
   {NOV, 8,"Edmond Halley's birthday"                                } ,
   {NOV, 8,"Bram Stoker's birthday"                                  } ,
   {NOV, 8,"Felix Haussdorf's birthday"                              } ,
   {NOV, 8,"Katherine Hepburn's birthday"                            } ,
   {NOV, 9,"Benjamin Banneker's birthday"                            } ,
   {NOV, 9,"Cambodia Independence Day"                               } ,
   {NOV,10,"Martin Luther's birthday"                                } ,
   {NOV,11,"Veteran's and Remembrance Day"                           } ,
   {NOV,11,"Poland Independence Day"                                 } ,
   {NOV,12,"Sun Yat Sen's birthday"                                  } ,
   {NOV,12,"Elizabeth Cady Stanton's birthday"                       } ,
   {NOV,12,"Auguste Rodin's birthday"                                } ,
   {NOV,12,"Comoros Independence Day"                                } ,
   {NOV,13,"Saint Augustine's birthday"                              } ,
   {NOV,14,"Claude Monet's birthday"                                 } ,
   {NOV,17,"Emperor Vespasian's birthday"                            } ,
   {NOV,17,"August Ferdinand Moebius's birthday"                     } ,
   {NOV,19,"Anniversary of Gettysburg Address"                       } ,
   {NOV,19,"Monaco National Fete"                                    } ,
   {NOV,20,"Edwin Hubble's birthday"                                 } ,
   {NOV,21,"Francois-Marie Arouet's birthday"                        } ,
   {NOV,22,"Anniversary of Kennedy's assasination"                   } ,
   {NOV,22,"Lebanon Independence Day"                                } ,
   {NOV,22,"Charles de Gaulle's birthday"                            } ,
   {NOV,23,"Anniversary of Dr Who's debut"                           } ,
   {NOV,23,"Billy the Kid's birthday"                                } ,
   {NOV,23,"Boris Karloff's birthday"                                } ,
   {NOV,23,"Harpo Marx's birthday"                                   } ,
   {NOV,24,"Baruch Spinoza's birthday"                               } ,
   {NOV,24,"Anniversary of Tasmania's discovery"                     } ,
   {NOV,24,"Anniversary of Darwin's The Origin of Species"           } ,
   {NOV,25,"Pope John XXIII's birthday"                              } ,
   {NOV,25,"Joe DiMaggio's birthday"                                 } ,
   {NOV,25,"Suriname Independence Day"                               } ,
   {NOV,26,"Anniversary of Casablanca's debut"                       } ,
   {NOV,26,"Norbert Wiener's birthday"                               } ,
   {NOV,27,"Anniversary of First Crusade's start"                    } ,
   {NOV,27,"Chaim Weizmann's birthday"                               } ,
   {NOV,27,"Aleksander Dubcek's birthday"                            } ,
   {NOV,28,"Mauritania Independence Day"                             } ,
   {NOV,29,"Clive Staples Lewis's birthday"                          } ,
   {NOV,29,"Jacques Chirac's birthday"                               } ,
   {NOV,30,"Samuel Clemens' birthday"                                } ,
   {NOV,30,"Winston Churchill's birthday"                            } ,
   {NOV,30,"Barbados Independence Day"                               } ,
   {NOV,30,"Saint Andrew's Day"                                      } ,
   {NOV,30,"Barbados Independence Day"                               } ,

   {DEC, 1,"Anniversary of Rosa Parks' arrest"                       } ,  /* WW */
   {DEC, 1,"Woody Allen's birthday"                                  } ,
   {DEC, 1,"Central African Republic National Day"                   } ,
   {DEC, 2,"Anniversary of Monroe Doctrine"                          } ,
   {DEC, 2,"Georges Seurat's birthday"                               } ,
   {DEC, 2,"Anniversary of Napoleon's coronation"                    } ,
   {DEC, 2,"Anniversary of Battle of Austerlitz"                     } ,
   {DEC, 3,"Joseph Konrad's birthday"                                } ,
   {DEC, 3,"Anniversary of Galileo's telescope"                      } ,
   {DEC, 4,"Crazy Horse's birthday"                                  } ,
   {DEC, 5,"Anniversary of Prohibition's end"                        } ,
   {DEC, 5,"George Armstrong Custer's birthday"                      } ,
   {DEC, 5,"Werner Karl Heisenberg's birthday"                       } ,
   {DEC, 6,"Spanish Constitution Day"                                } ,
   {DEC, 6,"Finland Independence Day"                                } ,
   {DEC, 7,"Anniversary of attack on Pearl Harbor"                   } ,
   {DEC, 7,"Ivory Coast National Day"                                } ,
   {DEC, 9,"John Milton's birthday"                                  } ,
   {DEC, 9,"Anniversary of Lech Walesa's election"                   } ,
   {DEC, 9,"Tanzania Independence Day"                               } ,
   {DEC,10,"Ada Lovelace's birthday"                                 } ,
   {DEC,10,"Anniversary of Spanish-American War's end"               } ,
   {DEC,10,"Anniversary of Grateful Dead's first concert"            } ,
   {DEC,10,"Emily Dickinson's birthday"                              } ,
   {DEC,11,"Robert Koch's birthday"                                  } ,
   {DEC,11,"Upper Volta Republic Day"                                } ,
   {DEC,11,"Aleksander Solzhenitsyn's birthday"                      } ,
   {DEC,11,"Max Born's birthday"                                     } ,
   {DEC,12,"Frank Sinatra's birthday"                                } ,
   {DEC,12,"Kenya Independence Day"                                  } ,
   {DEC,12,"William Lloyd Garrison's birthday"                       } ,
   {DEC,12,"Edvard Munch's birthday"                                 } ,
   {DEC,14,"Tycho Brahe's birthday"                                  } ,
   {DEC,14,"Nostradamus's birthday"                                  } ,
   {DEC,14,"Jimmy Doolittle's birthday"                              } ,
   {DEC,15,"Anniversary of adoption of US Bill of Rights"            } ,
   {DEC,15,"Anniversary of Gone With the Wind debut"                 } ,
   {DEC,16,"Anniversary of Boston Tea Party"                         } ,
   {DEC,16,"Jane Austen's birthday"                                  } ,
   {DEC,16,"Arthur C Clarke's birthday"                              } ,
   {DEC,17,"Anniversary of Wright brother's flight"                  } ,
   {DEC,17,"Roman Saturnalia!"                                       } ,
   {DEC,18,"Anniversary of 13 Amendment's adoption"                  } ,
   {DEC,18,"Niger Independence Day"                                  } ,
   {DEC,19,"William Pitt's (younger) birthday"                       } ,
   {DEC,20,"Robert Menzies' birthday"                                } ,
   {DEC,20,"Anniversary of Louisiana Purchase"                       } ,
   {DEC,21,"Anniversary of Pilgrim's landing"                        } ,
   {DEC,21,"Thomas Becket's birthday"                                } ,
   {DEC,21,"Jan Lukasiewicz' birthday"                               } ,
   {DEC,21,"Nepal Independence Day"                                  } ,
   {DEC,22,"Srinivasa Ramanujan's birthday"                          } ,
   {DEC,23,"Emperoro Akihito's birthday"                             } ,
   {DEC,24,"Adam Mickiewicz's birthday"                              } ,
   {DEC,24,"Ignatius Loyola's birthday"                              } ,
   {DEC,24,"Kit Carson's birthday"                                   } ,
   {DEC,25,"Christmas Day"                                           } ,
   {DEC,25,"Isaac Newton's birthday"                                 } ,
   {DEC,25,"Clara Barton's birthday"                                 } ,
   {DEC,25,"Rod Serling's birthday"                                  } ,
   {DEC,25,"Anwar Sadat's birthday"                                  } ,
   {DEC,26,"Charles Babbage birthday"                                } ,
   {DEC,26,"Boxing Day"                                              } ,
   {DEC,27,"Johannes Kepler's birthday"                              } ,
   {DEC,27,"Louis Pasteur's birthday"                                } ,
   {DEC,28,"Arthur Eddington's birthday"                             } ,
   {DEC,28,"John von Neumann's birthday"                             } ,
   {DEC,28,"Linus Torvalds' birthday"                                } ,
   {DEC,30,"Tiger Woods' birthday"                                   } ,
   {DEC,30,"Rudyard Kipling's birthday"                              } ,
   {DEC,31,"New Year's Eve"                                          } ,
   {DEC,31,"George Marshall's birthday"                              } ,

 {0,0,NULL} } ;  /* the last element, a flag to stop searching */

/*---------------------------------------------------------------------------------*/
/*! Return today's date trivia string. */

char * AFNI_get_date_trivia(void)
{
   time_t tt ;
   struct tm *lt ;
   int ii , ntar ;
   char *tar[NTMAX] ;

   tt = time(NULL) ;         /* seconds since 01 Jan 1970 */
   lt = localtime( &tt ) ;   /* break into pieces */

   /* find this month and day in the trivial list, if present */

   for( ii=ntar=0 ; ntar < NTMAX && holiday[ii].day != 0 ; ii++ )
     if( holiday[ii].mon == lt->tm_mon+1 && holiday[ii].day == lt->tm_mday )
       tar[ntar++] = holiday[ii].label ;

   if( ntar == 1 ){
     return tar[0] ;
   } else if( ntar > 1 ){
     ii = (lrand48()>>8) % ntar ;
     return tar[ii] ;
   }

   /* default trivia */

   return "[not yet claimed]" ;
}
