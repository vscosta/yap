define(Inline,`#define' $1($4) (($2)($5)))
define(Inline0,`#define' $1() (($2)($3)))
define(Inline2,`#define' $1($4,$6) (($2)($7)))
define(AbsType,typedef struct _$1 { int dummy; } *$1)
define(Constructor,`#define' $2$1($4) (($1)($5)))
define(Destructor,`#define' $2$1($4) (($3)($5)))




