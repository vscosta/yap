/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		io.h							 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	control YAP from sockets.				 *
*									 *
*************************************************************************/

#include "sysbits.h"

#if HAVE_SOCKET

#if HAVE_UNISTD_H && !defined(__MINGW32__) && !_MSC_VER
#include <unistd.h>
#endif
#if STDC_HEADERS
#include <stdlib.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_TIME_H && !defined(__MINGW32__) && !_MSC_VER
#include <sys/time.h>
#endif
#ifdef _WIN32
#if HAVE_IO_H
#include <io.h>
#endif
#endif
#if _MSC_VER || defined(__MINGW32__)
#include <io.h>
#include <winsock2.h>
#else
#if HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#if HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#if HAVE_NETDB_H
#include <netdb.h>
#endif
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#if HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#if HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#endif
#ifdef _WIN32
//#include <ws2tcpip.h>
typedef int socklen_t;
#endif
/* make sure we can compile in any platform */
#ifndef AF_UNSPEC
#define AF_UNSPEC 0
#endif
#ifndef AF_LOCAL
#define AF_LOCAL AF_UNSPEC
#endif
#ifndef AF_AAL5
#define AF_AAL5 AF_UNSPEC
#endif
#ifndef AF_APPLETALK
#define AF_APPLETALK AF_UNSPEC
#endif
#ifndef AF_AX25
#define AF_AX25 AF_UNSPEC
#endif
#ifndef AF_BRIDGE
#define AF_BRIDGE AF_UNSPEC
#endif
#ifndef AF_DECnet
#define AF_DECnet AF_UNSPEC
#endif
#ifndef AF_FILE
#define AF_FILE AF_UNSPEC
#endif
#ifndef AF_INET
#define AF_INET AF_UNSPEC
#endif
#ifndef AF_INET6
#define AF_INET6 AF_UNSPEC
#endif
#ifndef AF_IPX
#define AF_IPX AF_UNSPEC
#endif
#ifndef AF_LOCAL
#define AF_LOCAL AF_UNSPEC
#endif
#ifndef AF_NETBEUI
#define AF_NETBEUI AF_UNSPEC
#endif
#ifndef AF_NETLINK
#define AF_NETLINK AF_UNSPEC
#endif
#ifndef AF_NETROM
#define AF_NETROM AF_UNSPEC
#endif
#ifndef AF_OSINET
#define AF_OSINET AF_UNSPEC
#endif
#ifndef AF_PACKET
#define AF_PACKET AF_UNSPEC
#endif
#ifndef AF_ROSE
#define AF_ROSE AF_UNSPEC
#endif
#ifndef AF_ROUTE
#define AF_ROUTE AF_UNSPEC
#endif
#ifndef AF_SECURITY
#define AF_SECURITY AF_UNSPEC
#endif
#ifndef AF_SNA
#define AF_SNA AF_UNSPEC
#endif
#ifndef AF_UNIX
#define AF_UNIX AF_UNSPEC
#endif
#ifndef AF_X25
#define AF_X25 AF_UNSPEC
#endif

#ifndef SOCK_STREAM
#define SOCK_STREAM -1
#endif
#ifndef SOCK_DGRAM
#define SOCK_DGRAM -1
#endif
#ifndef SOCK_RAW
#define SOCK_RAW -1
#endif
#ifndef SOCK_RDM
#define SOCK_RDM -1
#endif
#ifndef SOCK_SEQPACKET
#define SOCK_SEQPACKET -1
#endif
#ifndef SOCK_PACKET
#define SOCK_PACKET -1
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

#ifndef BUFSIZ
#define BUFSIZ 256
#endif

#if _MSC_VER || defined(__MINGW32__)
#define socket_errno WSAGetLastError()
#define invalid_socket_fd(fd) (fd) == INVALID_SOCKET
#else
#define socket_errno errno
#define invalid_socket_fd(fd) (fd) < 0
#endif

void Yap_init_socks(char *host, long interface_port) {
  int s;
  int r;
  struct sockaddr_in soadr;
  struct in_addr adr;
  struct hostent *he;

#if HAVE_SOCKET
  he = gethostbyname(host);
  if (he == NULL) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "can not get address for host %s: %s", host, strerror(h_errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "can not get address for host");
#endif
    return;
  }

  (void)memset((void *)&soadr, '\0', sizeof(struct sockaddr_in));
  soadr.sin_family = AF_INET;
  soadr.sin_port = htons((short)interface_port);

  if (he != NULL) {
    memmove((char *)&adr, (char *)he->h_addr_list[0], (size_t)he->h_length);
  } else {
    adr.s_addr = inet_addr(host);
  }
  soadr.sin_addr.s_addr = adr.s_addr;

  s = socket(AF_INET, SOCK_STREAM, 0);
  if (s < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "could not create socket: %s",
              strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "could not create socket");
#endif
    return;
  }

#if ENABLE_SO_LINGER
  struct linger ling; /* disables socket lingering. */
  ling.l_onoff = 1;
  ling.l_linger = 0;
  if (setsockopt(s, SOL_SOCKET, SO_LINGER, (void *)&ling, sizeof(ling)) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "socket_connect/3 (setsockopt_linger: %s)",
              strerror(socket_errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "socket_connect/3 (setsockopt_linger)");
#endif
    return;
  }
#endif

  r = connect(s, (struct sockaddr *)&soadr, sizeof(soadr));
  if (r < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "connect failed, could not connect to interface: %s",
              strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "connect failed, could not connect to interface");
#endif
    return;
  }
/* now reopen stdin stdout and stderr */
#if HAVE_DUP2 && !defined(__MINGW32__)
  if (dup2(s, 0) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "could not dup2 stdin: %s",
              strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "could not dup2 stdin");
#endif
    return;
  }
  if (dup2(s, 1) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "could not dup2 stdout: %s",
              strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "could not dup2 stdout");
#endif
    return;
  }
  if (dup2(s, 2) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "could not dup2 stderr: %s",
              strerror(errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "could not dup2 stderr");
#endif
    return;
  }
#elif _MSC_VER || defined(__MINGW32__)
  if (_dup2(s, 0) < 0) {
    fprintf(stderr, "could not dup2 stdin\n");
    return;
  }
  if (_dup2(s, 1) < 0) {
    fprintf(stderr, "could not dup2 stdout\n");
    return;
  }
  if (_dup2(s, 2) < 0) {
    fprintf(stderr, "could not dup2 stderr\n");
    return;
  }
#else
  if (dup2(s, 0) < 0) {
    fprintf(stderr, "could not dup2 stdin\n");
    return;
  }
  yp_iob[0].cnt = 0;
  yp_iob[0].flags = _YP_IO_SOCK | _YP_IO_READ;
  if (dup2(s, 1) < 0) {
    fprintf(stderr, "could not dup2 stdout\n");
    return;
  }
  yp_iob[1].cnt = 0;
  yp_iob[1].flags = _YP_IO_SOCK | _YP_IO_WRITE;
  if (dup2(s, 2) < 0) {
    fprintf(stderr, "could not dup2 stderr\n");
    return;
  }
  yp_iob[2].cnt = 0;
  yp_iob[2].flags = _YP_IO_SOCK | _YP_IO_WRITE;
#endif
//   Yap_sockets_io = 1;
#if _MSC_VER || defined(__MINGW32__)
  _close(s);
#else
  close(s);
#endif
#else  /* HAVE_SOCKET */
  Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "sockets not installed",
            strerror(errno));
#endif /* HAVE_SOCKET */
}

static Int p_socket(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Term t3 = Deref(ARG3);
  char *sdomain, *stype;
  Int domain = AF_UNSPEC, type, protocol;
  int fd;
  Term out;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "socket/4");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "socket/4");
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "socket/4");
    return (FALSE);
  }
  if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_ATOM, t2, "socket/4");
    return (FALSE);
  }
  if (IsVarTerm(t3)) {
    Yap_Error(INSTANTIATION_ERROR, t3, "socket/4");
    return (FALSE);
  }
  if (!IsIntTerm(t3)) {
    Yap_Error(TYPE_ERROR_ATOM, t3, "socket/4");
    return (FALSE);
  }
  sdomain = RepAtom(AtomOfTerm(t1))->StrOfAE;
  if (sdomain[0] != 'A' || sdomain[1] != 'F' || sdomain[2] != '_')
    return (FALSE); /* Error */
  sdomain += 3;
  switch (sdomain[0]) {
  case 'A':
    if (strcmp(sdomain, "AAL5") == 0)
      domain = AF_AAL5;
    else if (strcmp(sdomain, "APPLETALK") == 0)
      domain = AF_APPLETALK;
    else if (strcmp(sdomain, "AX25") == 0)
      domain = AF_AX25;
    break;
  case 'B':
    if (strcmp(sdomain, "BRIDGE") == 0)
      domain = AF_APPLETALK;
    break;
  case 'D':
    if (strcmp(sdomain, "DECnet") == 0)
      domain = AF_DECnet;
    break;
  case 'F':
    if (strcmp(sdomain, "FILE") == 0)
      domain = AF_FILE;
    break;
  case 'I':
    if (strcmp(sdomain, "INET") == 0)
      domain = AF_INET;
    else if (strcmp(sdomain, "INET6") == 0)
      domain = AF_INET6;
    else if (strcmp(sdomain, "IPX") == 0)
      domain = AF_IPX;
    break;
  case 'L':
    if (strcmp(sdomain, "LOCAL") == 0)
      domain = AF_LOCAL;
    break;
  case 'N':
    if (strcmp(sdomain, "NETBEUI") == 0)
      domain = AF_NETBEUI;
    else if (strcmp(sdomain, "NETLINK") == 0)
      domain = AF_NETLINK;
    else if (strcmp(sdomain, "NETROM") == 0)
      domain = AF_NETROM;
    break;
  case 'O':
    if (strcmp(sdomain, "OSINET") == 0)
      domain = AF_OSINET;
    break;
  case 'P':
    if (strcmp(sdomain, "PACKET") == 0)
      domain = AF_PACKET;
    break;
  case 'R':
    if (strcmp(sdomain, "ROSE") == 0)
      domain = AF_ROSE;
    else if (strcmp(sdomain, "ROUTE") == 0)
      domain = AF_ROUTE;
    break;
  case 'S':
    if (strcmp(sdomain, "SECURITY") == 0)
      domain = AF_SECURITY;
    else if (strcmp(sdomain, "SNA") == 0)
      domain = AF_SNA;
    break;
  case 'U':
    if (strcmp(sdomain, "UNIX") == 0)
      domain = AF_UNIX;
    break;
  case 'X':
    if (strcmp(sdomain, "X25") == 0)
      domain = AF_X25;
    break;
  }
  stype = RepAtom(AtomOfTerm(t2))->StrOfAE;
  if (stype[0] != 'S' || stype[1] != 'O' || stype[2] != 'C' ||
      stype[3] != 'K' || stype[4] != '_')
    return (FALSE); /* Error */
  stype += 5;
  if (strcmp(stype, "STREAM") == 0)
    type = SOCK_STREAM;
  else if (strcmp(stype, "DGRAM") == 0)
    type = SOCK_DGRAM;
  else if (strcmp(stype, "RAW") == 0)
    type = SOCK_RAW;
  else if (strcmp(stype, "RDM") == 0)
    type = SOCK_RDM;
  else if (strcmp(stype, "SEQPACKET") == 0)
    type = SOCK_SEQPACKET;
  else if (strcmp(stype, "PACKET") == 0)
    type = SOCK_PACKET;
  else
    return (FALSE);
  protocol = IntOfTerm(t3);
  if (protocol < 0)
    return (FALSE);
  fd = socket(domain, type, protocol);
  if (invalid_socket_fd(fd)) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket/4 (socket: %s)",
              strerror(socket_errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket/4 (socket)");
#endif
    return (FALSE);
  }
  if (domain == AF_UNIX || domain == AF_LOCAL)
    out = Yap_InitSocketStream(fd, new_socket, af_unix);
  else if (domain == AF_INET)
    out = Yap_InitSocketStream(fd, new_socket, af_inet);
  else {
/* ok, we currently don't support these sockets */
#if _MSC_VER || defined(__MINGW32__)
    _close(fd);
#else
    close(fd);
#endif
    return (FALSE);
  }
  if (out == TermNil)
    return (FALSE);
  return (Yap_unify(out, ARG4));
}

Int Yap_CloseSocket(int fd, socket_info status, socket_domain domain) {
#if _MSC_VER || defined(__MINGW32__)
  /* prevent further writing
     to the socket */
  if (status == server_session_socket || status == client_socket) {
    char bfr;

    if (shutdown(fd, 1) != 0) {
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_close/1 (close)");
      return (FALSE);
    }
    /* read all pending characters
       from the socket */
    while (recv(fd, &bfr, 1, 0) > 0)
      ;
    /* prevent further reading
       from the socket */
    if (shutdown(fd, 0) < 0) {
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_close/1 (close)");
      return (FALSE);
    }

    /* close the socket */
    if (closesocket(fd) != 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_close/1 (close: %s)",
                strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_close/1 (close)");
#endif
    }
#else
  if (status == server_session_socket || status == client_socket) {
    if (shutdown(fd, 2) < 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_close/1 (shutdown: %s)",
                strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_close/1 (shutdown)");
#endif
      return (FALSE);
    }
  }
  if (close(fd) != 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_close/1 (close: %s)",
              strerror(socket_errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_close/1 (close)");
#endif
#endif
    return (FALSE);
  }
  return (TRUE);
}

static Int p_socket_close(USES_REGS1) {
  Term t1 = Deref(ARG1);
  int sno;

  if ((sno = Yap_CheckSocketStream(t1, "socket_close/1")) < 0) {
    return (FALSE);
  }
  Yap_CloseStream(sno);
  return (TRUE);
}

static Int p_socket_bind(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  int sno;
  Functor fun;
  socket_info status;
  int fd;

  if ((sno = Yap_CheckSocketStream(t1, "socket_bind/2")) < 0) {
    return (FALSE);
  }
  status = Yap_GetSocketStatus(sno);
  fd = Yap_GetStreamFd(sno);
  if (status != new_socket) {
    /* ok, this should be an error, as you are trying to bind  */
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "socket_bind/2");
    return (FALSE);
  }
  if (!IsApplTerm(t2)) {
    Yap_Error(DOMAIN_ERROR_STREAM, t2, "socket_bind/2");
    return (FALSE);
  }
  fun = FunctorOfTerm(t2);
#if HAVE_SYS_UN_H
  if (fun == FunctorAfUnix || fun == FunctorAfLocal) {
    struct sockaddr_un sock;
    Term taddr = ArgOfTerm(1, t2);
    char *s;
    int len;

    if (IsVarTerm(taddr)) {
      Yap_Error(INSTANTIATION_ERROR, t2, "socket_bind/2");
      return (FALSE);
    }
    if (!IsAtomTerm(taddr)) {
      Yap_Error(TYPE_ERROR_ATOM, taddr, "socket_bind/2");
      return (FALSE);
    }
    s = RepAtom(AtomOfTerm(taddr))->StrOfAE;
    sock.sun_family = AF_UNIX;
    if ((len = strlen(s)) > 107) /* hit me with a broomstick */ {
      Yap_Error(DOMAIN_ERROR_STREAM, taddr, "socket_bind/2");
      return (FALSE);
    }
    sock.sun_family = AF_UNIX;
    strcpy(sock.sun_path, s);
    if (bind(fd, (struct sockaddr *)(&sock),
             ((size_t)(((struct sockaddr_un *)0)->sun_path) + len)) < 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_bind/2 (bind: %s)",
                strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_bind/2 (bind)");
#endif
      return (FALSE);
    }
    Yap_UpdateSocketStream(sno, server_socket, af_unix);
    return (TRUE);
  } else
#endif
      if (fun == FunctorAfInet) {
    Term thost = ArgOfTerm(1, t2);
    Term tport = ArgOfTerm(2, t2);
    char *shost;
    struct hostent *he;
    struct sockaddr_in saddr;
    Int port;

    memset((void *)&saddr, (int)0, sizeof(saddr));
    if (IsVarTerm(thost)) {
      saddr.sin_addr.s_addr = INADDR_ANY;
    } else if (!IsAtomTerm(thost)) {
      Yap_Error(TYPE_ERROR_ATOM, thost, "socket_bind/2");
      return (FALSE);
    } else {
      shost = RepAtom(AtomOfTerm(thost))->StrOfAE;
      if ((he = gethostbyname(shost)) == NULL) {
#if HAVE_STRERROR
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_bind/2 (gethostbyname: %s)", strerror(socket_errno));
#else
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_bind/2 (gethostbyname)");
#endif
        return (FALSE);
      }
      memmove((void *)&saddr.sin_addr, (void *)he->h_addr_list[0], he->h_length);
    }
    if (IsVarTerm(tport)) {
      port = 0;
    } else {
      port = IntOfTerm(tport);
    }
    saddr.sin_port = htons(port);
    saddr.sin_family = AF_INET;
    if (bind(fd, (struct sockaddr *)&saddr, sizeof(saddr)) == -1) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_bind/2 (bind: %s)",
                strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_bind/2 (bind)");
#endif
      return (FALSE);
    }

    if (IsVarTerm(tport)) {
/* get the port number */
#if _WIN32 || defined(__MINGW32__)
      int namelen;
#else
      socklen_t namelen;
#endif
      Term t;
      if (getsockname(fd, (struct sockaddr *)&saddr, &namelen) < 0) {
#if HAVE_STRERROR
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_bind/2 (getsockname: %s)", strerror(socket_errno));
#else
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_bind/2 (getsockname)");
#endif
        return (FALSE);
      }
      t = MkIntTerm(ntohs(saddr.sin_port));
      Yap_unify(ArgOfTermCell(2, t2), t);
    }
    Yap_UpdateSocketStream(sno, server_socket, af_inet);
    return (TRUE);
  } else
    return (FALSE);
}

static Int p_socket_connect(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Functor fun;
  int sno;
  socket_info status;
  int fd;
  int flag;
  Term out;

  if ((sno = Yap_CheckSocketStream(t1, "socket_connect/3")) < 0) {
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "socket_connect/3");
    return (FALSE);
  }
  if (!IsApplTerm(t2)) {
    Yap_Error(DOMAIN_ERROR_STREAM, t2, "socket_connect/3");
    return (FALSE);
  }
  fun = FunctorOfTerm(t2);
  fd = Yap_GetStreamFd(sno);
  status = Yap_GetSocketStatus(sno);
  if (status != new_socket) {
    /* ok, this should be an error, as you are trying to bind  */
    return (FALSE);
  }
#if HAVE_SYS_UN_H
  if (fun == FunctorAfUnix) {
    struct sockaddr_un sock;
    Term taddr = ArgOfTerm(1, t2);
    char *s;
    int len;

    if (IsVarTerm(taddr)) {
      Yap_Error(INSTANTIATION_ERROR, t2, "socket_connect/3");
      return (FALSE);
    }
    if (!IsAtomTerm(taddr)) {
      Yap_Error(TYPE_ERROR_ATOM, taddr, "socket_connect/3");
      return (FALSE);
    }
    s = RepAtom(AtomOfTerm(taddr))->StrOfAE;
    sock.sun_family = AF_UNIX;
    if ((len = strlen(s)) > 107) /* beat me with a broomstick */ {
      Yap_Error(DOMAIN_ERROR_STREAM, taddr, "socket_connect/3");
      return (FALSE);
    }
    sock.sun_family = AF_UNIX;
    strcpy(sock.sun_path, s);
    if ((flag = connect(
             fd, (struct sockaddr *)(&sock),
             ((size_t)(((struct sockaddr_un *)0)->sun_path) + len))) < 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "socket_connect/3 (connect: %s)", strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_connect/3 (connect)");
#endif
      return (FALSE);
    }
    Yap_UpdateSocketStream(sno, client_socket, af_unix);
  } else
#endif
      if (fun == FunctorAfInet) {
    Term thost = ArgOfTerm(1, t2);
    Term tport = ArgOfTerm(2, t2);
    char *shost;
    struct hostent *he;
    struct sockaddr_in saddr;
    unsigned short int port;

    memset((void *)&saddr, (int)0, sizeof(saddr));
    if (IsVarTerm(thost)) {
      Yap_Error(INSTANTIATION_ERROR, thost, "socket_connect/3");
      return (FALSE);
    } else if (!IsAtomTerm(thost)) {
      Yap_Error(TYPE_ERROR_ATOM, thost, "socket_connect/3");
      return (FALSE);
    } else {
      shost = RepAtom(AtomOfTerm(thost))->StrOfAE;
      if ((he = gethostbyname(shost)) == NULL) {
#if HAVE_STRERROR
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_connect/3 (gethostbyname: %s)",
                  strerror(socket_errno));
#else
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_connect/3 (gethostbyname)");
#endif
        return (FALSE);
      }
      memmove((void *)&saddr.sin_addr, (void *)he->h_addr_list[0], he->h_length);
    }
    if (IsVarTerm(tport)) {
      Yap_Error(INSTANTIATION_ERROR, tport, "socket_connect/3");
      return (FALSE);
    } else if (!IsIntegerTerm(tport)) {
      Yap_Error(TYPE_ERROR_INTEGER, tport, "socket_connect/3");
      return (FALSE);
    } else {
      port = (unsigned short int)IntegerOfTerm(tport);
    }
    saddr.sin_port = htons(port);
    saddr.sin_family = AF_INET;
#if ENABLE_SO_LINGER
    {
      struct linger ling; /* For making sockets linger. */
      /* disabled: I see why no reason why we should throw things away by
       * default!! */
      ling.l_onoff = 1;
      ling.l_linger = 0;
      if (setsockopt(fd, SOL_SOCKET, SO_LINGER, (void *)&ling, sizeof(ling)) <
          0) {
#if HAVE_STRERROR
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_connect/3 (setsockopt_linger: %s)",
                  strerror(socket_errno));
#else
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_connect/3 (setsockopt_linger)");
#endif
        return FALSE;
      }
    }
#endif

    {
      int one = 1; /* code by David MW Powers */

      if (setsockopt(fd, SOL_SOCKET, SO_BROADCAST, (void *)&one, sizeof(one))) {
#if HAVE_STRERROR
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_connect/3 (setsockopt_broadcast: %s)",
                  strerror(socket_errno));
#else
        Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                  "socket_connect/3 (setsockopt_broadcast)");
#endif
        return FALSE;
      }
    }

    flag = connect(fd, (struct sockaddr *)&saddr, sizeof(saddr));
    if (flag < 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "socket_connect/3 (connect: %s)", strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_connect/3 (connect)");
#endif
      return FALSE;
    }
    Yap_UpdateSocketStream(sno, client_socket, af_inet);
  } else
    return (FALSE);
  out = t1;
  return (Yap_unify(out, ARG3));
}

static Int p_socket_listen(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  int sno;
  socket_info status;
  int fd;
  Int j;

  if ((sno = Yap_CheckSocketStream(t1, "socket_listen/2")) < 0) {
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "socket_listen/2");
    return (FALSE);
  }
  if (!IsIntTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "socket_listen/2");
    return (FALSE);
  }
  j = IntOfTerm(t2);
  if (j < 0) {
    Yap_Error(DOMAIN_ERROR_STREAM, t1, "socket_listen/2");
    return (FALSE);
  }
  fd = Yap_GetStreamFd(sno);
  status = Yap_GetSocketStatus(sno);
  if (status != server_socket) {
    /* ok, this should be an error, as you are trying to bind  */
    return (FALSE);
  }
  if (listen(fd, j) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_listen/2 (listen: %s)",
              strerror(socket_errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_listen/2 (listen)");
#endif
  }
  return (TRUE);
}

static Int p_socket_accept(USES_REGS1) {
  Term t1 = Deref(ARG1);
  int sno;
  socket_info status;
  socket_domain domain;
  int ofd, fd;
  Term out;

  if ((sno = Yap_CheckSocketStream(t1, "socket_accept/3")) < 0) {
    return (FALSE);
  }
  ofd = Yap_GetStreamFd(sno);
  status = Yap_GetSocketStatus(sno);
  if (status != server_socket) {
    /* ok, this should be an error, as you are trying to bind  */
    return (FALSE);
  }
  domain = Yap_GetSocketDomain(sno);
#if HAVE_SYS_UN_H
  if (domain == af_unix) {
    struct sockaddr_un caddr;
    socklen_t len;

    memset((void *)&caddr, (int)0, sizeof(caddr));
    if ((fd = accept(ofd, (struct sockaddr *)&caddr, &len)) < 0) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_accept/3 (accept: %s)",
                strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_accept/3 (accept)");
#endif
    }
    /* ignore 2nd argument */
    out = Yap_InitSocketStream(fd, server_session_socket, af_unix);
  } else
#endif
      if (domain == af_inet) {
    struct sockaddr_in caddr;
    Term tcli;
    char *s;
    socklen_t len;

    len = sizeof(caddr);
    memset((void *)&caddr, (int)0, sizeof(caddr));
    if (invalid_socket_fd(fd = accept(ofd, (struct sockaddr *)&caddr, &len))) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_accept/3 (accept: %s)",
                strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_accept/3 (accept)");
#endif
      return (FALSE);
    }
    if ((s = inet_ntoa(caddr.sin_addr)) == NULL) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "socket_accept/3 (inet_ntoa: %s)", strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_accept/3 (inet_ntoa)");
#endif
    }
    tcli = MkAtomTerm(Yap_LookupAtom(s));
    if (!Yap_unify(ARG2, tcli))
      return (FALSE);
    out = Yap_InitSocketStream(fd, server_session_socket, af_inet);
  } else
    return (FALSE);
  if (out == TermNil)
    return (FALSE);
  return (Yap_unify(out, ARG3));
}

static Int p_socket_buffering(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Term t4 = Deref(ARG4);
  Atom mode;
  int fd;
  int writing;
#if _WIN32 || defined(__MINGW32__)
  int bufsize;
#else
  unsigned int bufsize;
#endif
  int sno;
  socklen_t len;

  if ((sno = Yap_CheckSocketStream(t1, "socket_buffering/4")) < 0) {
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "socket_buffering/4");
    return (FALSE);
  }
  if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_ATOM, t2, "socket_buffering/4");
    return (FALSE);
  }
  mode = AtomOfTerm(t2);
  if (mode == AtomRead)
    writing = FALSE;
  else if (mode == AtomWrite)
    writing = TRUE;
  else {
    Yap_Error(DOMAIN_ERROR_IO_MODE, t2, "socket_buffering/4");
    return (FALSE);
  }
  fd = Yap_GetStreamFd(sno);
  if (writing) {
    getsockopt(fd, SOL_SOCKET, SO_SNDBUF, (void *)&bufsize, &len);
  } else {
    getsockopt(fd, SOL_SOCKET, SO_RCVBUF, (void *)&bufsize, &len);
  }
  if (!Yap_unify(ARG3, MkIntegerTerm(bufsize)))
    return (FALSE);
  if (IsVarTerm(t4)) {
    bufsize = BUFSIZ;
  } else {
    Int siz;
    if (!IsIntegerTerm(t4)) {
      Yap_Error(TYPE_ERROR_INTEGER, t4, "socket_buffering/4");
      return (FALSE);
    }
    siz = IntegerOfTerm(t4);
    if (siz < 0) {
      Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t4, "socket_buffering/4");
      return (FALSE);
    }
    bufsize = siz;
  }
  if (writing) {
    setsockopt(fd, SOL_SOCKET, SO_SNDBUF, (void *)&bufsize, sizeof(bufsize));
  } else {
    setsockopt(fd, SOL_SOCKET, SO_RCVBUF, (void *)&bufsize, sizeof(bufsize));
  }
  return (TRUE);
}

static Term select_out_list(Term t1, fd_set *readfds_ptr USES_REGS) {
  if (t1 == TermNil) {
    return (TermNil);
  } else {
    int fd;
    int sno;
    Term next = select_out_list(TailOfTerm(t1), readfds_ptr PASS_REGS);
    Term Head = HeadOfTerm(t1);

    sno = Yap_CheckIOStream(Head, "stream_select/5");
    fd = Yap_GetStreamFd(sno);
    if (FD_ISSET(fd, readfds_ptr))
      return (MkPairTerm(Head, next));
    else
      return (MkPairTerm(TermNil, next));
  }
}

static Int p_socket_select(USES_REGS1) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Term t3 = Deref(ARG3);
  fd_set readfds, writefds, exceptfds;
  struct timeval timeout, *ptime;

#if _MSC_VER || defined(__MINGW32__)
  u_int fdmax = 0;
#else
  int fdmax = 0;
#endif
  Int tsec, tusec;
  Term tout = TermNil, ti, Head;

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "socket_select/5");
    return (FALSE);
  }
  if (!IsPairTerm(t1)) {
    Yap_Error(TYPE_ERROR_LIST, t1, "socket_select/5");
    return (FALSE);
  }
  if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t2, "socket_select/5");
    return (FALSE);
  }
  if (!IsIntegerTerm(t2)) {
    Yap_Error(TYPE_ERROR_INTEGER, t2, "socket_select/5");
    return (FALSE);
  }
  if (IsVarTerm(t3)) {
    Yap_Error(INSTANTIATION_ERROR, t3, "socket_select/5");
    return (FALSE);
  }
  if (!IsIntegerTerm(t3)) {
    Yap_Error(TYPE_ERROR_INTEGER, t3, "socket_select/5");
    return (FALSE);
  }
  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  FD_ZERO(&exceptfds);
  /* fetch the input streams */
  ti = t1;
  while (ti != TermNil) {
#if _MSC_VER || defined(__MINGW32__)
    u_int fd;
#else
    int fd;
#endif
    int sno;

    Head = HeadOfTerm(ti);
    sno = Yap_CheckIOStream(Head, "stream_select/5");
    if (sno < 0)
      return (FALSE);
    fd = Yap_GetStreamFd(sno);
    FD_SET(fd, &readfds);
    if (fd > fdmax)
      fdmax = fd;
    ti = TailOfTerm(ti);
  }
  /* now, check the time */
  tsec = IntegerOfTerm(t2);
  tusec = IntegerOfTerm(t3);
  if (tsec < 0) /* off */ {
    ptime = NULL;
  } else {
    timeout.tv_sec = tsec;
    timeout.tv_usec = tusec;
    ptime = &timeout;
  }
  /* do the real work */
  if (select(fdmax + 1, &readfds, &writefds, &exceptfds, ptime) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_select/5 (select: %s)",
              strerror(socket_errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "socket_select/5 (select)");
#endif
  }
  tout = select_out_list(t1, &readfds PASS_REGS);
  /* we're done, just pass the info back */
  return (Yap_unify(ARG4, tout));
}

static Int p_current_host(USES_REGS1) {
  char oname[MAXHOSTNAMELEN], *name;
  Term t1 = Deref(ARG1), out;

  if (!IsVarTerm(t1) && !IsAtomTerm(t1)) {
    Yap_Error(TYPE_ERROR_ATOM, t1, "current_host/2");
    return (FALSE);
  }
  name = oname;
  if (gethostname(name, sizeof(oname)) < 0) {
#if HAVE_STRERROR
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "current_host/2 (gethostname: %s)", strerror(socket_errno));
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "current_host/2 (gethostname)");
#endif
    return (FALSE);
  }
  if ((strrchr(name, '.') == NULL)) {
    struct hostent *he;

    /* not a fully qualified name, ask the name server */
    if ((he = gethostbyname(name)) == NULL) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "current_host/2 (gethostbyname: %s)", strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "current_host/2 (gethostbyname)");
#endif
      return (FALSE);
    }
    name = (char *)(he->h_name);
  }
  if (IsAtomTerm(t1)) {
    char *sin = RepAtom(AtomOfTerm(t1))->StrOfAE;
    int faq = (strrchr(sin, '.') != NULL);

    if (faq)
#if _MSC_VER || defined(__MINGW32__)
      return (_stricmp(name, sin) == 0);
#else
      return (strcasecmp(name, sin) == 0);
#endif
    else {
      int isize = strlen(sin);
      if (isize >= 256) {
        Yap_Error(SYSTEM_ERROR_INTERNAL, ARG1,
                  "current_host/2 (input longer than longest FAQ host name)");
        return (FALSE);
      }
      if (name[isize] != '.')
        return (FALSE);
      name[isize] = '\0';
#if _MSC_VER || defined(__MINGW32__)
      return (_stricmp(name, sin) == 0);
#else
      return (strcasecmp(name, sin) == 0);
#endif
    }
  } else {
    out = MkAtomTerm(Yap_LookupAtom(name));
    return (Yap_unify(ARG1, out));
  }
}

static Int p_hostname_address(USES_REGS1) {
  char *s;
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Term tin, out;
  struct hostent *he;

  if (!IsVarTerm(t1)) {
    if (!IsAtomTerm(t1)) {
      Yap_Error(TYPE_ERROR_ATOM, t1, "hostname_address/2");
      return (FALSE);
    } else
      tin = t1;
  } else if (IsVarTerm(t2)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "hostname_address/5");
    return (FALSE);
  } else if (!IsAtomTerm(t2)) {
    Yap_Error(TYPE_ERROR_ATOM, t2, "hostname_address/2");
    return (FALSE);
  } else
    tin = t2;
  s = RepAtom(AtomOfTerm(tin))->StrOfAE;
  if (IsVarTerm(t1)) {
    if ((he = gethostbyaddr(s, strlen(s), AF_INET)) == NULL) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "hostname_address/2 (gethostbyname: %s)",
                strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "hostname_address/2 (gethostbyname)");
#endif
    }
    out = MkAtomTerm(Yap_LookupAtom((char *)(he->h_name)));
    return (Yap_unify(out, ARG1));
  } else {
    struct in_addr adr;
    if ((he = gethostbyname(s)) == NULL) {
#if HAVE_STRERROR
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "hostname_address/2 (gethostbyname: %s)",
                strerror(socket_errno));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
                "hostname_address/2 (gethostbyname)");
#endif
	  return false;
    }
    memmove((char *)&adr, (char *)he->h_addr_list[0], (size_t)he->h_length);
    out = MkAtomTerm(Yap_LookupAtom(inet_ntoa(adr)));
    return (Yap_unify(out, ARG2));
  }
}
#endif

void Yap_InitSocketLayer(void) {
#ifdef HAVE_SOCKET
  Yap_InitCPred("socket", 4, p_socket, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("socket_close", 1, p_socket_close, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("socket_bind", 2, p_socket_bind, SafePredFlag | SyncPredFlag);
  Yap_InitCPred("socket_connect", 3, p_socket_connect,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("socket_listen", 2, p_socket_listen,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("socket_accept", 3, p_socket_accept,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("$socket_buffering", 4, p_socket_buffering,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("$socket_select", 4, p_socket_select,
                SafePredFlag | SyncPredFlag | HiddenPredFlag);
  Yap_InitCPred("current_host", 1, p_current_host, SafePredFlag);
  Yap_InitCPred("hostname_address", 2, p_hostname_address, SafePredFlag);
#if _MSC_VER || defined(__MINGW32__)
  {
    WSADATA info;
    if (WSAStartup(MAKEWORD(2, 1), &info) != 0)
      exit(1);
  }
#endif
#endif
}
