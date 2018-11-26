#ifndef _RomanImperator_Header_
#define _RomanImperator_Header_
#ifdef AFNI_VERSION_LABEL
static char *RomanImperator[] = {
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
