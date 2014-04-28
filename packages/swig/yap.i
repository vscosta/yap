/* example.i */
 %module yap

 %{
 /* Put header files here or function declarations like below */
 
#include "YapInterface.h"

 %}

extern int prove_string(char *s);

%include "YapInterface.h"

