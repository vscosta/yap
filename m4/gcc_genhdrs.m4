define(Inline,`
inline EXTERN $2 $1($3);

inline EXTERN $2 $1($3 $4)
{
	return ($2) ($5);
}
'
)
define(Inline0,`
inline EXTERN $2 $1(void);

inline EXTERN $2 $1()
{
	return ($2) ($3);
}
'
)
define(Inline2,`
inline EXTERN $2 $1($3 $4, $5 $6);

inline EXTERN $2 $1($3 $4, $5 $6)
{
	return ($2) ($7);
}
'
)
define(AbsType,`typedef struct _$1 { DUMMY_FILLER_FOR_ABS_TYPE } *$1')
define(Constructor,`
inline EXTERN $1 $2$1($3 $4);

inline EXTERN $1 $2$1($3 $4)
{
	return ($1) ($5);
}
'
)
define(Destructor,`
inline EXTERN $3 $2$1($1 $4);

inline EXTERN $3 $2$1($1 $4)
{
	return ($3) ($5);
}
'
)

