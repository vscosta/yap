#ifndef STUFF_H
#define STUFF_H

/*--------------------------------------------------------------------*/
#ifndef bool
typedef enum { false, true } bool;
#endif
/*--------------------------------------------------------------------*/

#if   defined _MSC_VER
#define NORET void __declspec(noreturn)
#define PRINTF_LIKE_FUNC(m, n) /* empty */
#elif defined __GNUC__
#define NORET void __attribute__((noreturn))
#define PRINTF_LIKE_FUNC(m, n) __attribute__((format(printf, m, n)))
#else /* other */
#define NORET void
#define PRINTF_LIKE_FUNC(m, n) /* empty */
#endif

/*--------------------------------------------------------------------*/

#endif /* STUFF_H */
