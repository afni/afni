#define AFMALL(typ,siz) \
  (typ*) calloc(1,siz)

#define AFREALL(v,typ,siz) \
  (typ*) realloc((void*)v,sizeof(typ)*siz)

#define AFFREE(v) free((void*)v)
