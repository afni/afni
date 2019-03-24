#ifndef _RomanImperator_Header_
#define _RomanImperator_Header_
#ifdef AFNI_VERSION_LABEL
static char *RomanImperator[] = {      /* 175 of them, good thru 2062 or so */
    "Augustus" , "Tiberius" , "Caligula" ,
    "Claudius" , "Nero" , "Galba" ,
    "Otho" , "Aulus Vitellius" , "Vespasian" ,
    "Titus" , "Domitian" , "Nerva" ,
    "Trajan" , "Hadrian" , "Antoninus Pius" ,
    "Marcus Aurelius" , "Lucius Verus" , "Commodus" ,
    "Publius Helvius Pertinax" , "Marcus Didius Severus Julianus" ,
    "Septimius Severus" , "Caracalla" , "Publius Septimius Geta" ,
    "Macrinus" , "Elagabalus" , "Severus Alexander" ,
    "Maximinus" , "Gordian I" , "Gordian II" ,
    "Pupienus Maximus" , "Balbinus" , "Gordian III" ,
    "Philip" , "Decius" , "Hostilian" ,
    "Gallus" , "Aemilian" , "Valerian" ,
    "Gallienus" , "Claudius II Gothicus" , "Quintillus" ,
    "Aurelian" , "Tacitus" , "Florian" ,
    "Probus" , "Carus" , "Numerian" ,
    "Carinus" , "Diocletian" , "Maximian" ,
    "Constantius I" , "Galerius" , "Severus" ,
    "Maxentius" , "Constantine I" , "Galerius Valerius Maximinus" ,
    "Licinius" , "Constantine II" , "Constantius II" ,
    "Constans I" , "Gallus Caesar" , "Julian" ,
    "Jovian" , "Valentinian I" , "Valens" ,
    "Gratian" , "Valentinian II" , "Theodosius I" ,
    "Arcadius" , "Magnus Maximus" , "Honorius" ,
    "Theodosius II" , "Constantius III" , "Valentinian III" ,
    "Marcian" , "Petronius Maximus" , "Avitus" ,
    "Majorian" , "Libius Severus" , "Anthemius" ,
    "Olybrius" , "Glycerius" , "Julius Nepos" ,
    "Romulus Augustulus" , "Leo I" , "Leo II" , "Zeno"
    "Basiliscus" , "Anastasius I" , "Justin I" ,
    "Justinian I the Great" , "Justin II" , "Tiberius II Constantine" ,
    "Maurice" , "Phocas" , "Heraclius" ,
    "Constantine III" , "Heraklonas" , "Constans II" ,
    "Mezezius" , "Constantine IV" , "Justinian II the Slit-nosed" ,
    "Leontios" , "Tiberios III" , "Philippikos Bardanes" ,
    "Anastasios II" , "Theodosios III" , "Leo III the Isaurian" ,
    "Constantine V Kopronymos" , "Artabasdus the Icon-lover" , "Constantine V Kopronymos" ,
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

#define AFNI_VERSION_RomanImperator                                   \
  RomanImperator[                                                     \
   (4*((AFNI_VERSION_LABEL[5]-'0')*10+(AFNI_VERSION_LABEL[6]-'0')-18) \
    +(AFNI_VERSION_LABEL[8]-'0'-3))%NUM_RomanImperator]

#else

#define AFNI_VERSION_RomanImperator "\0"

#endif /* AFNI_VERSION_LABEL */
#endif /*_RomanImperator_Header_ */
