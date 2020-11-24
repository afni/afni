#ifndef _RomanImperator_Header_
#define _RomanImperator_Header_

/******************************************************************************/
/*   From https://en.wikipedia.org/wiki/List_of_Roman_emperors (late 2018)    */
/******************************************************************************/

/*----------------------------------------------------------------------------*/
/* AFNI_VERSION_LABEL, defined in file AFNI_version.h, is a string of the form
     AFNI_ab.c.de
   where ab = year minus 2000 (e.g., 18 for 2018)
         c  = quarter within the year = 0, 1, 2, or 3
         de = minor number of version
   Macro AFNI_VERSION_RomanImperator (far below) uses ab and c to choose the
   cognomen (catch name) from the version; this macro is used in afni.c
   (cf. function show_AFNI_version() ) when option '-ver' is given to print
   out the AFNI version; e.g.,
     Oct 20 2020 (Version AFNI_20.3.01 'Vespasian')
*//*--------------------------------------------------------------------------*/

#ifdef AFNI_VERSION_LABEL

static char *RomanImperator[] = {  /* 175 of them, 4/year, good thru 2062 or so. */
                                   /* After that? China maybe? Turkish Sultans? */
/*2018*/    "Augustus" ,
/*2019*/    "Tiberius" , "Caligula" , "Claudius" , "Nero" ,
/*2020*/    "Galba" , "Otho" , "Aulus Vitellius" , "Vespasian" ,
/*2021*/    "Titus" , "Domitian" , "Nerva" , "Trajan" ,
/*2022*/    "Hadrian" , "Antoninus Pius" , "Marcus Aurelius" , "Lucius Verus" ,
/*2023*/    "Commodus" , "Publius Helvius Pertinax" , "Marcus Didius Severus Julianus" , "Septimius Severus" ,
/*2024*/    "Caracalla" , "Publius Septimius Geta" , "Macrinus" , "Elagabalus" ,
/*2025*/    "Severus Alexander" , "Maximinus" , "Gordian I" , "Gordian II" ,
/*2026*/    "Pupienus Maximus" , "Balbinus" , "Gordian III" , "Philip the Arab" ,
/*2027*/    "Decius" , "Hostilian" , "Gallus" , "Aemilian" ,
/*2028*/    "Valerian" , "Gallienus" , "Claudius II Gothicus" , "Quintillus" ,
/*2029*/    "Aurelian" , "Tacitus" , "Florian" , "Probus" ,
/*2030*/    "Carus" , "Numerian" , "Carinus" , "Diocletian" ,
/*2031*/    "Maximian" , "Constantius I" , "Galerius" , "Severus" ,
/*2032*/    "Maxentius" , "Constantine I" , "Galerius Valerius Maximinus" , "Licinius" ,
/*2033*/    "Constantine II" , "Constantius II" , "Constans I" , "Gallus Caesar" ,
/*2034*/    "Julian the Apostate" , "Jovian" , "Valentinian I" , "Valens" ,
/*2035*/    "Gratian" , "Valentinian II" , "Theodosius I" , "Arcadius" ,
/*2036*/    "Magnus Maximus" , "Honorius" , "Theodosius II" , "Constantius III" ,
/*2037*/    "Valentinian III" , "Marcian" , "Petronius Maximus" , "Avitus" ,
/*2038*/    "Majorian" , "Libius Severus" , "Anthemius" , "Olybrius" ,
/*2039*/    "Glycerius" , "Julius Nepos" , "Romulus Augustulus" , /* last Western Emperor */

            "Leo I the Thracian" ,  /* list of Eastern Emperors starts here */

/*2040*/    "Leo II" , "Zeno" , "Basiliscus" , "Anastasius I" ,
/*2041*/    "Justin I" , "Justinian I" , "Justin II" , "Tiberius II Constantine" ,
/*2042*/    "Maurice" , "Phocas" , "Heraclius" , "Constantine III" ,
/*2043*/    "Heraklonas" , "Constans II" , "Mezezius" , "Constantine IV" ,
/*2044*/    "Justinian II the Slit-nosed" , "Leontios" , "Tiberios III" , "Philippikos Bardanes" ,
/*2045*/    "Anastasios II" , "Theodosios III" , "Leo III the Isaurian" , "Constantine V Kopronymos" ,
/*2046*/    "Artabasdus the Icon-lover" , "Constantine V Kopronymos" , "Leo IV the Khazar" , "Constantine VI the Blinded" ,
/*2047*/    "Irene the Athenian" , "Nikephoros I" , "Staurakios" , "Michael I Rangabe" ,
/*2048*/    "Leo V the Armenian" , "Michael II the Stammerer" , "Theophilos" , "Theodora" ,
/*2049*/    "Michael III the Drunkard" , "Basil I the Macedonian" , "Leo VI the Wise" , "Alexander" ,
/*2050*/    "Constantine VII" , "Romanos I Lekapenos" , "Romanos II" , "Nikephoros II Phokas" ,
/*2051*/    "John I Tzimiskes" , "Basil II the Bulgar-slayer" , "Constantine VIII" , "Zoe" ,
/*2052*/    "Romanos III Argyros" , "Michael IV the Paphlagonian" , "Michael V the Caulker" , "Theodora" ,
/*2053*/    "Constantine IX Monomachos" , "Theodora" , "Michael VI the General" , "Isaac I Komnenos" ,

            /* Bob turns 100 in 2054 - why is there no Emperor "Bob I AFNIman"? */
/*2054*/    "Constantine X Doukas" , "Michael VII Doukas Quarter-short" , "Romanos IV Diogenes" , "Nikephoros III Botaneiates" ,

/*2055*/    "Alexios I Komnenos" , "John II Komnenos the Handsome" ,
    "Manuel I Komnenos the Great" , "Alexios II Komnenos" ,

/*2056*/   "Andronikos I Komnenos" ,
            "Isaac II Angelos" , "Alexios III Angelos" , "Isaac II Angelos" ,

/*2057*/    "Alexios IV Angelos" , "Nikolaos Kanabos" ,
            "Alexios V Doukas the Bushy-eyebrowed" , "Constantine Laskaris" ,

/*2058*/    "Theodore I Laskaris" , "John III Doukas Vatatzes" ,
            "Theodore II Doukas Laskaris" , "John IV Doukas Laskaris" ,

/*2059*/ "Michael VIII Palaiologos" ,
         "Andronikos II Palaiologos the Elder" ,
         "Andronikos III Palaiologos the Younger" ,
         "John V Palaiologos" ,

/*2060*/ "John VI Kantakouzenos" , "John V Palaiologos" ,
         "Andronikos IV Palaiologos" , "John V Palaiologos" ,

/*2061*/ "John VII Palaiologos" ,
         "John V Palaiologos" , "Manuel II Palaiologos" , "John VII Palaiologos" ,

/*2062*/  "John VIII Palaiologos" , "Constantine XI"
} ;

#define NUM_RomanImperator (sizeof(RomanImperator)/sizeof(char *))

/*----------------------------------------------------------------------------*/
/* * Gets the string for the Imperial name from the AFNI version
      label, which is of the form AFNI_19.3.26 (e.g.).
   * In this example, the subscript in the array above is computed
      from the '19.3' part of the label, using the '1', the '9', and
      the '3' ([5], [6], and [8] characters in the label string).
   * If the label is AFNI_ab.c.de, then the year 'ab'
     is munged to a*10+b-18 (since 2018 is the zero year for this)
   * This munged year is multiplied by 4 since we do one Imperator
     per quarter, and then the 'c' quarter is added in, with an
     extra minus 3 since this all started in the last quarter of 2018,
     with version AFNI_18.3.00, which should decode to the 0th Imperator
     -- Augustus!
   * Note that when the year 2100 rolls around, this macro will no
     longer work properly. At that point, AFNI will be 106 years old
     and Bob will be 146 years old. I'll let someone else worry about
     this problem, if you don't mind.
*//*--------------------------------------------------------------------------*/

#define AFNI_VERSION_RomanImperator                                   \
  RomanImperator[                                                     \
   ( 4*((AFNI_VERSION_LABEL[5]-'0')*10                                \
    +   (AFNI_VERSION_LABEL[6]-'0')-18)                               \
    +   (AFNI_VERSION_LABEL[8]-'0'-3)  ) % NUM_RomanImperator ]

#else  /*---------- the backup case == no Imperial name :( ----------*/

#define AFNI_VERSION_RomanImperator "\0"

#endif /* if AFNI_VERSION_LABEL is defined */

#endif /*_RomanImperator_Header_ */
