#if defined MYDDAS_MYSQL

#include <stdio.h>
#include <stdlib.h>
#include "Yap.h"
#include <netinet/in.h>
#include "myddas_wkb.h"
#include "myddas_wkb2prolog.h"

static void readswap4(uint32 *buf);
static void readswap8(double *buf);

static byte get_hostbyteorder(void);
static byte get_inbyteorder(void);
static uint32 get_wkbType(void);
static Term get_point(char *functor USES_REGS);
static Term get_linestring(char *functor);
static Term get_polygon(char *functor);
static Term get_geometry(uint32 type);

static int swaporder;
static byte inbyteorder, hostbyteorder;
static byte *cursor;

Term wkb2prolog(char *wkb) {
  uint32 type;

  cursor = wkb;

  /*ignore the SRID 4 bytes*/
  cursor += 4;

  /*byteorder*/
  hostbyteorder = get_hostbyteorder();
  inbyteorder = get_inbyteorder();

  swaporder = 0;
  if ( hostbyteorder != inbyteorder )
    swaporder = 1;

  type = get_wkbType();

  return get_geometry(type);
}

static byte get_hostbyteorder(void){
  uint16_t host = 5;
  uint16_t net;

  net = htons(host);
  if ( net == host )
    return(WKBXDR);
  else
    return(WKBNDR);
}

static byte get_inbyteorder(void){
  byte b = cursor[0];

  if (b != WKBNDR && b != WKBXDR) {
    fprintf(stderr, "Unknown byteorder: %d\n",b);
    exit(0);
  }

  cursor++;

  return(b);
}

static uint32 get_wkbType(void){
  uint32 u;

  /* read the type */
  readswap4(&u);

  if (u > WKBMAXTYPE || u < WKBMINTYPE) {
    fprintf(stderr, "Unknown type: %d\n",u);
    exit(0);
  }

  return(u);
}

static void readswap4(uint32 *buf){
  ((byte *) buf)[0] = cursor[0];
  ((byte *) buf)[1] = cursor[1];
  ((byte *) buf)[2] = cursor[2];
  ((byte *) buf)[3] = cursor[3];

  if ( swaporder ) {
    if ( inbyteorder == WKBXDR ) {
      *buf = (uint32)ntohl((u_long)*buf);
    } else {
      byte u[4];

      u[0] = ((byte *) buf)[3];
      u[1] = ((byte *) buf)[2];
      u[2] = ((byte *) buf)[1];
      u[3] = ((byte *) buf)[0];
      ((byte *) buf)[0] = u[0];
      ((byte *) buf)[1] = u[1];
      ((byte *) buf)[2] = u[2];
      ((byte *) buf)[3] = u[3];
    }
  }

  cursor += 4;
}

static void readswap8(double *buf) {
  ((byte *) buf)[0] = cursor[0];
  ((byte *) buf)[1] = cursor[1];
  ((byte *) buf)[2] = cursor[2];
  ((byte *) buf)[3] = cursor[3];
  ((byte *) buf)[4] = cursor[4];
  ((byte *) buf)[5] = cursor[5];
  ((byte *) buf)[6] = cursor[6];
  ((byte *) buf)[7] = cursor[7];

  if ( swaporder ) {
    if ( inbyteorder == WKBXDR ) {
      u_long u[2];

      u[0] = ((u_long *) buf)[0];
      u[1] = ((u_long *) buf)[1];
      ((u_long *) buf)[1] = ntohl(u[0]);
      ((u_long *) buf)[0] = ntohl(u[1]);
    } else {
      byte u[8];

      u[0] = ((byte *) buf)[7];
      u[1] = ((byte *) buf)[6];
      u[2] = ((byte *) buf)[5];
      u[3] = ((byte *) buf)[4];
      u[4] = ((byte *) buf)[3];
      u[5] = ((byte *) buf)[2];
      u[6] = ((byte *) buf)[1];
      u[7] = ((byte *) buf)[0];
      ((byte *) buf)[0] = u[0];
      ((byte *) buf)[1] = u[1];
      ((byte *) buf)[2] = u[2];
      ((byte *) buf)[3] = u[3];
      ((byte *) buf)[4] = u[4];
      ((byte *) buf)[5] = u[5];
      ((byte *) buf)[6] = u[6];
      ((byte *) buf)[7] = u[7];
    }
  }

  cursor += 8;
}

static Term get_point(char *func USES_REGS){
  Term args[2];
  Functor functor;
  double d;

  if(func == NULL)
    /*functor "," => (_,_)*/
    functor = Yap_MkFunctor(Yap_LookupAtom(","), 2);
  else
    functor = Yap_MkFunctor(Yap_LookupAtom(func), 2);

  /* read the X */
  readswap8(&d);
  args[0] = MkFloatTerm(d);

  /* read the Y */
  readswap8(&d);
  args[1] = MkFloatTerm(d);

  return Yap_MkApplTerm(functor, 2, args);
}

static Term get_linestring(char *func){
  CACHE_REGS

  Term *c_list;
  Term list;
  Functor functor;
  uint32 n;
  int i;

  /* read the number of vertices */
  readswap4(&n);

  /* space for arguments */
  c_list = (Term *) calloc(sizeof(Term),n);

  for ( i = 0; i < n; i++) {
    c_list[i] = get_point(NULL PASS_REGS);
  }

  list = MkAtomTerm(Yap_LookupAtom("[]"));
  for (i = n - 1; i >= 0; i--) {
    list = MkPairTerm(c_list[i],list);
  }

  if(func == NULL)
    return list;
  else{
    functor = Yap_MkFunctor(Yap_LookupAtom(func), 1);
    return Yap_MkApplTerm(functor, 1, &list);
  }
}

static Term get_polygon(char *func){
  CACHE_REGS

  uint32 r;
  int i;
  Functor functor;
  Term *c_list;
  Term list;

  /* read the number of rings */
  readswap4(&r);

  /* space for rings */
  c_list = (Term *) calloc(sizeof(Term),r);

  for ( i = 0; i < r; i++ ) {
    c_list[i] = get_linestring(NULL);
  }

  list = MkAtomTerm(Yap_LookupAtom("[]"));
  for (i = r - 1; i >= 0; i--) {
    list = MkPairTerm(c_list[i],list);
  }

  if(func == NULL)
    return list;
  else{
    functor = Yap_MkFunctor(Yap_LookupAtom("polygon"), 1);
    return Yap_MkApplTerm(functor, 1, &list);
  }
}

static Term get_geometry(uint32 type){
  CACHE_REGS

  switch(type) {
  case WKBPOINT:
    return get_point("point" PASS_REGS);
  case WKBLINESTRING:
    return get_linestring("linestring");
  case WKBPOLYGON:
    return get_polygon("polygon");
  case WKBMULTIPOINT:
    {
      uint32 n;
      int i;
      Functor functor;
      Term *c_list;
      Term list;


      /* read the number of points */
      readswap4(&n);

      /* space for points */
      c_list = (Term *) calloc(sizeof(Term),n);

      for ( i = 0; i < n; i++ ) {
	/* read (and ignore) the byteorder and type */
	get_inbyteorder();
	get_wkbType();

	c_list[i] = get_point(NULL PASS_REGS);
      }

      list = MkAtomTerm(Yap_LookupAtom("[]"));
      for (i = n - 1; i >= 0; i--) {
	list = MkPairTerm(c_list[i],list);
      }

      functor = Yap_MkFunctor(Yap_LookupAtom("multipoint"), 1);

      return Yap_MkApplTerm(functor, 1, &list);

    }
  case WKBMULTILINESTRING:
    {
      uint32 n;
      int i;
      Functor functor;
      Term *c_list;
      Term list;


      /* read the number of polygons */
      readswap4(&n);

      /* space for polygons*/
      c_list = (Term *) calloc(sizeof(Term),n);

      for ( i = 0; i < n; i++ ) {
	/* read (and ignore) the byteorder and type */
	get_inbyteorder();
	get_wkbType();

	c_list[i] = get_linestring(NULL);
      }

      list = MkAtomTerm(Yap_LookupAtom("[]"));
      for (i = n - 1; i >= 0; i--) {
	list = MkPairTerm(c_list[i],list);
      }

      functor = Yap_MkFunctor(Yap_LookupAtom("multilinestring"), 1);

      return Yap_MkApplTerm(functor, 1, &list);

    }
  case WKBMULTIPOLYGON:
    {
      uint32 n;
      int i;
      Functor functor;
      Term *c_list;
      Term list;


      /* read the number of polygons */
      readswap4(&n);

      /* space for polygons*/
      c_list = (Term *) calloc(sizeof(Term),n);

      for ( i = 0; i < n; i++ ) {
	/* read (and ignore) the byteorder and type */
	get_inbyteorder();
	get_wkbType();

	c_list[i] = get_polygon(NULL);
      }

      list = MkAtomTerm(Yap_LookupAtom("[]"));
      for (i = n - 1; i >= 0; i--) {
        list = MkPairTerm(c_list[i],list);
      }

      functor = Yap_MkFunctor(Yap_LookupAtom("multipolygon"), 1);

      return Yap_MkApplTerm(functor, 1, &list);

    }
  case WKBGEOMETRYCOLLECTION:
    {
      uint32 n;
      int i;
      Functor functor;
      Term *c_list;
      Term list;

      /* read the number of geometries */
      readswap4(&n);

      /* space for geometries*/
      c_list = (Term *) calloc(sizeof(Term),n);


      for ( i = 0; i < n; i++ ) {
	get_inbyteorder();
	c_list[i] = get_geometry(get_wkbType());
      }

      list = MkAtomTerm(Yap_LookupAtom("[]"));
      for (i = n - 1; i >= 0; i--) {
	list = MkPairTerm(c_list[i],list);
      }

      functor = Yap_MkFunctor(Yap_LookupAtom("geometrycollection"), 1);

      return Yap_MkApplTerm(functor, 1, &list);
    }
  }

  return MkAtomTerm(Yap_LookupAtom("[]"));
}

#endif /*MYDDAS_MYSQL*/
