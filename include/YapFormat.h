
#ifndef YAP_FORMAT

#if defined(_WIN64)

typedef int64_t Int;
typedef uint64_t UInt;
#define Int_FORMAT "%I64d"
#define UInt_FORMAT "%I64u"
#define Int_F "I64d"
#define UInt_F "I64u"
#define UXInt_F "I64xu"
#define Sizet_F "Id"

#elif defined(_WIN32)

typedef int32_t Int;
typedef uint32_t UInt;
#define Int_FORMAT "%I32d"
#define UInt_FORMAT "%I32u"
#define Int_F "I32d"
#define UInt_F "I32u"
#define UInt_FORMAT "%I32u"
#define UXInt_FORMAT "%I32xu"
#define Sizet_F "Id"

#elif defined(PRIdPTR)
#define Int_FORMAT "%" PRIdPTR
#define Int_ANYFORMAT "%" PRIuPTR
#define UInt_FORMAT "%" PRIuPTR
#define Int_F PRIdPTR
#define Int_ANYF PRIuPTR
#define UInt_F PRIuPTR
#define UXInt_F PRIxPTR
#define Sizet_F "zd"

#elif SIZEOF_LONG_INT == SIZEOF_INT_P

typedef long int Int;
typedef unsigned long int UInt;
#define Int_FORMAT "%ld"
#define UInt_FORMAT "%uld"
#define Int_F "ld"
#define UInt_F "uld"
#define UXInt_F "ulxd"
#define Sizet_F "uld"

#elif SIZEOF_INT == SIZEOF_INT_P

typedef int Int;
typedef unsigned int UInt;
#define Int_FORMAT "%l"
#define UInt_FORMAT "%ul"
#define Int_F "l"
#define UInt_F "ul"
#define UXInt_F "uld"
#define Sizet_F "ux"

#endif

#endif
