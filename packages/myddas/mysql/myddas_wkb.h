#ifndef MYDDAS_WKB_H_
#define MYDDAS_WKB_H_

typedef char byte;

typedef unsigned int uint32;

#define WKBXDR 0
#define WKBNDR 1

#define WKBMINTYPE 1

#define WKBPOINT 1
#define WKBLINESTRING 2
#define WKBPOLYGON 3
#define WKBMULTIPOINT 4
#define WKBMULTILINESTRING 5
#define WKBMULTIPOLYGON 6
#define WKBGEOMETRYCOLLECTION 7

#define WKBMAXTYPE 7

#define WKBGEOMETRY 0

#endif /* MYDDAS_WKB_H_ */
