
#ifndef JIT_HPP
#define JIT_HPP

#ifdef __cplusplus

#include <vector>
#include <string>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <time.h>
#include <dlfcn.h>
#include "config.h"
#if USE_GMP
#include <gmpxx.h>
#endif
extern "C" {
#include "absmi.h"
}
using namespace std;

#else

#define LIMIT_COUNT 4096

#endif

#endif
