#include "core/fputil.h"

double fputil_snan(void)
{
    return +sqrt(-1);
}

double fputil_qnan(void)
{
    return -sqrt(-1);
}
