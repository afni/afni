#ifndef _RomanImperator_Header_
#define _RomanImperator_Header_

/**********************************************************************************/
/* Adapted from https://en.wikipedia.org/wiki/List_of_Roman_emperors in late 2018 */
/**********************************************************************************/

#ifdef AFNI_VERSION_LABEL
static char *RomanImperator[] = {      /* 175 of them, 4 per year, good thru 2062 or so */
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

    "Artabasdus the Icon-lover" , "Constantine V Kopronymos" ,
    "Leo IV the Khazar" , "Constantine VI the Blinded" , "Irene the Athenian" ,
    "Nikephoros I" , "Staurakios" , "Michael I Rangabe" ,
    "Leo V the Armenian" , "Michael II the Stammerer" , "Theophilos" ,
    "Theodora" , "Michael III the Drunkard" , "Basil I the Macedonian" ,
    "Leo VI the Wise" , "Alexander" , "Constantine VII the Purple-born" ,
    "Romanos I Lekapenos" , "Romanos II the Purple-born" , "Nikephoros II Phokas" ,
    "John I Tzimiskes" , "Basil II the Bulgar-slayer" , "Constantine VIII" ,
    "Zoe" , "Romanos III Argyros" , "Michael IV the Paphlagonian" ,
    "Michael V the Caulker" , "Theodora" , "Constantine IX Monomachos" ,
    "Theodora" , "Michael VI the General" , "Isaac I Komnenos" ,
    "Constantine X Doukas" , "Michael VII Doukas Quarter-short" , "Romanos IV Diogenes" ,
    "Nikephoros III Botaneiates" , "Alexios I Komnenos" , "John II Komnenos the Handsome" ,
    "Manuel I Komnenos the Great" , "Alexios II Komnenos" , "Andronikos I Komnenos" ,
    "Isaac II Angelos" , "Alexios III Angelos" , "Isaac II Angelos" ,
    "Alexios IV Angelos" , "Nikolaos Kanabos" ,
    "Alexios V Doukas the Bushy-eyebrowed" , "Constantine Laskaris" ,
    "Theodore I Laskaris" , "John III Doukas Vatatzes" , "Theodore II Doukas Laskaris" ,
    "John IV Doukas Laskaris" , "Michael VIII Palaiologos" ,
    "Andronikos II Palaiologos the Elder" , "Andronikos III Palaiologos the Younger" ,
    "John V Palaiologos" , "John VI Kantakouzenos" , "John V Palaiologos" ,
    "Andronikos IV Palaiologos" , "John V Palaiologos" , "John VII Palaiologos" ,
    "John V Palaiologos" , "Manuel II Palaiologos" , "John VII Palaiologos" ,
    "John VIII Palaiologos" , "Constantine XI"
} ;

#define NUM_RomanImperator (sizeof(RomanImperator)/sizeof(char *))

/*----------------------------------------------------------------------------*/
/* * Gets the string for the Imperial name from the AFNI version
      label, which is of the form AFNI_19.3.26 (e.g.).
   * In this example, the subscript in the array above is computed
      from the '19.3' part of the label, using the '1', the '9', and
      the '3' ([5], [6], and [8] characters in the label string).
*//*--------------------------------------------------------------------------*/

#define AFNI_VERSION_RomanImperator                                   \
  RomanImperator[                                                     \
   (4*((AFNI_VERSION_LABEL[5]-'0')*10+(AFNI_VERSION_LABEL[6]-'0')-18) \
    +(AFNI_VERSION_LABEL[8]-'0'-3))%NUM_RomanImperator]

#else  /*---------- the backup case ----------*/

#define AFNI_VERSION_RomanImperator "\0"

#endif /* AFNI_VERSION_LABEL */
#endif /*_RomanImperator_Header_ */
