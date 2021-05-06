
project('yap', 'c','cpp',
  version : '0.1',
  default_options : ['warning_level=1', 'c_std=gnu11','cpp_std=c++14']
  )

# Configuratin data for config.h
cdata = configuration_data()

yap_sources = ['C/absmi.c',
		'C/adtdefs.c',
		'C/agc.c',
		'C/alloc.c',
		'C/amasm.c',
		'C/analyst.c',
		'C/args.c',
		'C/arith0.c',
		'C/arith1.c',
		'C/arith2.c',
		'C/arrays.c',
		'C/atomic.c',
		'C/attvar.c',
		'C/bb.c',
		'C/bignum.c',
		'C/blobs.c',
		'C/cdmgr.c',
		'C/c_interface.c',
		'C/clause_list.c',
		'C/cmppreds.c',
		'C/compiler.c',
		'C/computils.c',
		'C/corout.c',
		'C/cut_c.c',
		'C/dbase.c',
		'C/dbload.c',
		'C/debug.c',
		'C/depth_bound.c',
		'C/dlmalloc.c',
		'C/errors.c',
		'C/eval.c',
		'C/exec.c',
		'C/exo.c',
		'C/exo_udi.c',
		'C/flags.c',
		'C/globals.c',
		'C/gmp_support.c',
		'C/gprof.c',
		'C/grow.c',
		'C/heapgc.c',
		'C/index.c',
		'C/init.c',
		'C/inlines.c',
		'C/load_aix.c',
		'C/load_aout.c',
		'C/load_coff.c',
		'C/load_dl.c',
		'C/load_dld.c',
		'C/load_dll.c',
		'C/load_dyld.c',
		'C/load_foreign.c',
		'C/load_none.c',
		'C/load_shl.c',
		'C/mavar.c',
		'C/modules.c',
		'C/other.c',
		'C/parser.c',
		'C/qlyr.c',
		'C/qlyw.c',
		'C/range.c',
		'C/realpath.c',
		'C/save.c',
		'C/scanner.c',
		'C/signals.c',
		'C/sort.c',
		'C/stack.c',
		'C/stackinfo.c',
		'C/stdpreds.c',
		'C/terms.c',
		'C/text.c',
		'C/threads.c',
		'C/tracer.c',
		'C/udi.c',
		'C/unify.c',
		'C/userpreds.c',
		'C/utilpreds.c',
		'C/write.c',
		'C/yap-args.c',
		'os/absf.c',
		'os/alias.c',
		'os/assets.c',
		'os/charsio.c',
		'os/chartypes.c',
		'os/console.c',
		'os/cwalk/cwalk.c',
		'os/files.c',
		'os/fmem.c',
		'os/fmemopen-android.c',
		'os/fmemopen.c',
		'os/format.c',
		'os/iopreds.c',
		'os/mem.c',
		'os/open_memstream.c',
		'os/pipes.c',
		'os/random.c',
		'os/readline.c',
		'os/readterm.c',
		'os/readutil.c',
		'os/sig.c',
		'os/sockets.c',
		'os/streams.c',
		'os/sysbits.c',
		'os/time.c',
		'os/writeterm.c',
		'os/ypsocks.c',
		'os/ypstdio.c',
		'OPTYap/opt.init.c',
		'OPTYap/opt.preds.c',
		'OPTYap/or.copy_engine.c',
		'OPTYap/or.cow_engine.c',
		'OPTYap/or.cut.c',
		'OPTYap/or.memory.c',
		'OPTYap/or.sba_engine.c',
		'OPTYap/or.scheduler.c',
		'OPTYap/or.thread_engine.c',
		'OPTYap/tab.completion.c',
		'OPTYap/tab.tries.c',
		'BEAM/eamamasm.c',
		'BEAM/eam_am.c',
		'BEAM/eam_gc.c',
		'BEAM/eamindex.c',
		'BEAM/eam_showcode.c',
		'BEAM/eam_split.c',
		'BEAM/toeam.c']

#default YAP directories
yapdirs = ['BEAM','H','H/generated','JIT/HPP','os','os/cwalk','OPTYap','utf8proc','include','.']

incdirs = include_directories(yapdirs)

cc = meson.get_compiler('c')


conf_data = {}



build_options = {
		  'depth_limit' :	'DEPTH_LIMIT',
		  'tabling' : 'TABLING',
		  'threads' : 'THREADS',
		  'align_longs' : 'ALIGN_LONGS',
		  'threaded_code' :'THREADED_CODE',
		  'jit' :  'YAP_JIT',
		  'force_second_quadrant' : 'FORCE_SECOND_QUADRANT',
		  'gc_no_tags' : 'GC_NO_TAGS',
		  'ffieee': 'FFIEEE',
		  'low_level_tracer' : 'LOW_LEVEL_TRACER'
		}

if get_option('memory-allocator') == 'dl'
  conf_data += {  'USE_DL_MALLOC' : 1 }
elif get_option('memory-allocator') == 'system'
  conf_data += {  'USE_SYSTEM_MALLOC' : 1 }
endif


system_includes =  {
	'alloca.h' : 'HAVE_ALLOCA_H',
	'ctype.h' : 'HAVE_CTYPE_H',
	'direct.h' : 'HAVE_DIRECT_H',
	'errno.h' : 'HAVE_ERRNO_H',
	'fcntl.h' : 'HAVE_FCNTL_H',
	'inttypes.h' : 'HAVE_INTTYPES_H',
	'malloc.h' : 'HAVE_MALLOC_H',
	'math.h' : 'HAVE_MATH_H',
	'mpe.h' : 'HAVE_MPE_H',
	'mpi.h' : 'HAVE_MPI_H',
	'mysql/mysql.h' : 'HAVE_MYSQL_MYSQL_H',
	'noreturnstd.h' : 'HAVE_NORETURNSTD_H',
	'pthread.h' : 'HAVE_PTHREAD_H',
	'regex.h' : 'HAVE_REGEX_H',
	'setjmp.h' : 'HAVE_SETJMP_H',
	'stdarg.h' : 'HAVE_STDARG_H',
	'stdbool.h' : 'HAVE_STDBOOL_H',
	'stdint.h' : 'HAVE_STDINT_H',
	'string.h' : 'HAVE_STRING1_H',
	'strings.h' : 'HAVE_STRINGS_H',
	'stropts.h' : 'HAVE_STROPTS_H',
	'syslog.h' : 'HAVE_SYSLOG_H',
	'sys/types.h' : 'HAVE_SYS_TYPES_H',
	'sys/ucontext.h' : 'HAVE_SYS_UCONTEXT_H',
	'sys/un.h' : 'HAVE_SYS_UN_H',
	'sys/wait.h' : 'HAVE_SYS_WAIT_H',
	'termios.h' : 'HAVE_TERMIOS_H',
	'time.h' : 'HAVE_TIME_H',
	'ucontext.h' : 'HAVE_UCONTEXT_H',
	'unistd.h' : 'HAVE_UNISTD_H',
	'util.h' : 'HAVE_UTIL_H',
	'utime.h' : 'HAVE_UTIME_H',
	'wchar.h' : 'HAVE_WCHAR_H',
	'wctype.h' : 'HAVE_WCTYPE_H',
	'windef.h' : 'HAVE_WINDEF_H',
	'windows.h' : 'HAVE_WINDOWS_H',
	'Python.h' : 'HAVE_PYTHON_H' }

system_libraries = {
		     'm': 'HAVE_LIBM',
		     'judy': 'HAVE_LIBWS2_32',
		     'android': 'HAVE_LIBJUDY',
		     'log': 'HAVE_LIBLOG'
		     }

 system_functions = {
 '__builtin_ffsll' : 'HAVE___BUILTIN_FFSLL',
 'acosh' : 'HAVE_ACOSH',
 'asinh' : 'HAVE_ASINH',
 'atanh' : 'HAVE_ATANH',
 'backtrace' : 'HAVE_BACKTRACE',
 'dynarray' : 'HAVE_DYNARRAY',
 'finite' : 'HAVE_FINITE',
 'getpagesize' : 'HAVE_GETPAGESIZE',
 'isfinite' : 'HAVE_ISFINITE',
 'isinf' : 'HAVE_ISINF',
 'isnan' : 'HAVE_ISNAN',
 'iswblank' : 'HAVE_ISWBLANK',
 'iswspace' : 'HAVE_ISWSPACE',
 'labs' : 'HAVE_LABS',
 'link' : 'HAVE_LINK',
 'mbscasecoll' : 'HAVE_MBSCASECOLL',
 'mbscoll' : 'HAVE_MBSCOLL',
 'mbsnrtowcs' : 'HAVE_MBSNRTOWCS',
 'memmove' : 'HAVE_MEMCPY',
 'mtrace' : 'HAVE_MTRACE',
 'restartable_syscalls' : 'HAVE_RESTARTABLE_SYSCALLS',
 'rint' : 'HAVE_RINT',
 'sbrk' : 'HAVE_SBRK',
 'setjmp' : 'HAVE_SETJMP',
 'sigsetjmp' : 'HAVE_SIGSETJMP',
 'snprintf' : 'HAVE_SNPRINTF',
 'sqllen' : 'HAVE_SQLLEN',
 'sqlulen' : 'HAVE_SQLULEN',
 'srand' : 'HAVE_SRAND',
 'srand48' : 'HAVE_SRAND48',
 'srandom' : 'HAVE_SRANDOM',
 'stat' : 'HAVE_STAT',
 'strcasecmp' : 'HAVE_STRCASECMP',
 'strcasestr' : 'HAVE_STRCASESTR',
 'strcat' : 'HAVE_STRCAT',
 'strchr' : 'HAVE_STRCHR',
 'strcmp' : 'HAVE_STRCMP',
 'strerror' : 'HAVE_STRERROR',
 'stricmp' : 'HAVE_STRICMP',
 'strlcpy' : 'HAVE_STRLCPY',
 'strlwr' : 'HAVE_STRLWR',
 'strncasecmp' : 'HAVE_STRNCASECMP',
 'strncat' : 'HAVE_STRNCAT',
 'strncpy' : 'HAVE_STRNCPY',
 'strnlen' : 'HAVE_STRNLEN',
 'strtod' : 'HAVE_STRTOD',
 'system' : 'HAVE_SYSTEM',
 'vfork' : 'HAVE_VFORK',
 'vsnprintf' : 'HAVE_VSNPRINTF',
 'wcsnlen' : 'HAVE_WCSNLEN',
 'wordexp' : 'HAVE_WORDEXP'
		    }
system_symbols = {
 '_bool' : [ 'stdbool.h', 'HAVE__BOOL' ],
# '_chsize_s' : [ 'HAVE__CHSIZE_S' ],
 'ssize_t' : [ 'HAVE_SSIZE_T' ],
'struct_time_tm_gmtoff' : [ 'time.h', 'HAVE_STRUCT_TIME_TM_GMTOFF' ],
 'flsl' : [ 'string.h' , 'HAVE_FLSL' ],
 'ffsl' : [ 'string.h' , 'HAVE_FFSL' ],
 'ffsll' : [ 'string.h' , 'HAVE_FFSLL' ]	      }

compiler_sizes = {
	'short int' : 'SIZEOF_SHORT_INT',
	'int' : 'SIZEOF_INT',
	'long' : 'SIZEOF_LONG',
	'long int' : 'SIZEOF_LONG_INT',
	'long long' : 'SIZEOF_LONG_LONG',
	'long long int' : 'SIZEOF_LONG_LONG_INT',
	'float' : 'SIZEOF_FLOAT',
	'double' : 'SIZEOF_DOUBLE',
	'void *' : 'SIZEOF_VOID_P',
	'int *' : 'SIZEOF_INT_P',
	'ssize_t' : 'HAVE_SSIZE_T',

 	'uintptr_t' : 'CELLSIZE',
	'wchar_t' : 'SIZEOF_WCHAR_T' }



foreach o, def: build_options
	if get_option(o)
  	# header exists, do something
  		conf_data += {def:1}
	endif
endforeach
foreach file, def: system_includes
	if cc.has_header(file)
  	# header exists, do something
  		conf_data += {def:1}
	endif
endforeach

syslibs=[]
foreach file, def: system_libraries
        deplib = cc.find_library(file, required:false)
	if deplib.found()
                syslibs += [deplib]
  		# header exists, do something
  		conf_data += {def:1}
	endif
endforeach

foreach file, def: system_functions
      	if  cc.has_function(file, dependencies:syslibs,include_directories : incdirs)
  		# header exists, do something
  		conf_data += {def:1}
	endif
endforeach

foreach sy, def: system_symbols
      	if  cc.has_header_symbol(def[0], sy, dependencies:syslibs,include_directories : incdirs)
  		# header exists, do something
  		conf_data += {def[1]:1}
	endif
endforeach

foreach type, def: compiler_sizes
  # header exists, do something
  conf_data += {def : cc.sizeof(type,prefix:'#include <stdint.h>\n#include <stdbool.h>\n #include <wchar.h>\n #include <string.h>\n #include <setjmp.h>', include_directories : incdirs)}
endforeach

subdir('os')

gmp_dep = cc.find_library('gmp', required : true, has_headers: ['gmp.h','gmpxx.h'])
if gmp_dep.found()
  syslibs += gmp_dep
  conf_data += {'HAVE_GMP': 1,
		'USE_GMP': 1,
		'HAVE_GMP_H': 1,
		'HAVE_GMPXX_H': 1
	       }
endif

sed = find_program('sed')

infile = 'H/locals.h'
outfile = 'dlocals.h'
  syslibs += gmp_dep

  dlocals	 = custom_target('locals',
  output : outfile,
  input : infile,
  command :  run_command(cc,['-DLOCAL=1', '-x', 'c', '-E', '-P', '-w' , '@INPUT@', '-o', '@OUTPUT@']),

  install : true )



system_includes += {'dlocals.h': 'HAVE_DLOCALS_H'}

infile = 'H/locals.h'
outfile = 'rdlocals.h'
  syslibs += gmp_dep

  rdlocals	 = custom_target('rlocals',
  output : outfile,
  input : infile,
  command : run_command(cc,['-DREMOTE=1', '-x', 'c', '-E', '-P', '-w' , '@INPUT@', '-o', '@OUTPUT@']),

  install : true )



system_includes += {'rZZZdlocals.h': 'HAVE_DLOCALS_H'}




yapLib = shared_library('Yap', sources : yap_sources, include_directories : incdirs,
           install : true,
           link_with : [utf8Lib,osLib],
	   dependencies : [syslibs])

target = executable('yap',
           'console/yap.c',
           install : true,
           link_with : [yapLib,utf8Lib]
           )

  conf_data += { 'BITNESS': conf_data['CELLSIZE']*8 }
  conf_data += { 'C_CC': cc.cmd_array()[0] }
  conf_data += { 'EXTERN': '' }
  #conf_data += { 'C_CFLAGS': cc.cmd_array() }
  #conf_data += { 'C_LDFLAGS': target.link_args() }


configure_file(
  output : 'YapConfig.h',
  configuration : conf_data)
