
# configuratin data for config.h
os_conf_data = {}

os_sources = ['absf.c',
		'alias.c',
		'assets.c',
		'charsio.c',
		'chartypes.c',
		'console.c',
		'cwalk/cwalk.c',
		'files.c',
		'fmem.c',
		'fmemopen-android.c',
		'fmemopen.c',
		'format.c',
		'iopreds.c',
		'mem.c',
		'open_memstream.c',
		'pipes.c',
		'random.c',
		'readline.c',
		'readterm.c',
		'readutil.c',
		'sig.c',
		'sockets.c',
		'streams.c',
		'sysbits.c',
		'time.c',
		'writeterm.c',
		'ypsocks.c',
		'ypstdio.c']

#default YAP directories
os_dirs = ['.','..','../H','../H/generated','cwalk','../JIT/HPP','../OPTYap','../utf8proc','../include','.']

os_incdirs = include_directories(os_dirs)


if get_option('readline-provider') == 'readline'
	readline = cc.find_library('readline', required: get_option('readline'))
	if readline.found()
		os_conf_data += {'HAVE_READLINE': 1}
	endif

	if cc.has_function('rl_replace_line', prefix: '#include <stdio.h>\n #include <readline/readline.h>', dependencies: [readline])
		os_conf_data += { 'HAVE_READLINE_REPLACE_LINE' : 1}
	endif
else # editline
	readline = dependency('libedit', required: get_option('readline'))
	if readline.found()
		os_conf_data += {'HAVE_EDITLINE' : 1 }
	endif
endif


os_system_includes =  {
	'arpa/inet.h' : 'HAVE_ARPA_INET_H',
	'apache2/util_md5.h' : 'HAVE_APACHE2_UTIL_MD5_H',
	'apr-1/apr_md5.h' : 'HAVE_APR_1_APR_MD5_H',
	'direct.h' : 'HAVE_DIRECT_H',
	'dirent.h' : 'HAVE_DIRENT_H',
	'dlfcn.h' : 'HAVE_DLFCN_H',
	'execinfo.h' : 'HAVE_EXECINFO_H',
	'fenv.h' : 'HAVE_FENV_H',
	'fmemopen.h' : 'HAVE_FMEMOPEN_H',
	'fpu_control.h' : 'HAVE_FPU_CONTROL_H',
	'glob.h' : 'HAVE_GLOB_H',
	'ieeefp.h' : 'HAVE_IEEEFP_H',
	'io.h' : 'HAVE_IO_H',
	'libgen.h' : 'HAVE_LIBGEN_H',
	'LibLoaderAPI.h' : 'HAVE_LIBLOADERAPI_H',
	'limits.h' : 'HAVE_LIMITS_H',
	'locale.h' : 'HAVE_LOCALE_H',
	'mach-o/dyld.h' : 'HAVE_MACH_O_DYLD_H',
	'memory.h' : 'HAVE_MEMORY_H',
	'netdb.h' : 'HAVE_NETDB_H',
	'netinet/in.h' : 'HAVE_NETINET_IN_H',
	'netinet/tcp.h' : 'HAVE_NETINET_TCP_H',
	'pwd.h' : 'HAVE_PWD_H',
	'shlobj.h' : 'HAVE_SHLOBJ_H',
	'siginfo.h' : 'HAVE_SIGINFO_H',
	'signal.h' : 'HAVE_SIGNAL_H',
	'sqlite3.h' : 'HAVE_SQLITE3_H',
	'sql.h' : 'HAVE_SQL_H',
	'syslog.h' : 'HAVE_SYSLOG_H',
	'sys/resource.h' : 'HAVE_SYS_RESOURCE_H',
	'sys/stat.h' : 'HAVE_SYS_STAT_H',
	'sys/time.h' : 'HAVE_SYS_TIME_H',
	'sys/timeb.h' : 'HAVE_SYS_TIMEB_H',
	'termios.h' : 'HAVE_TERMIOS_H',
	'time.h' : 'HAVE_TIME_H',
	'utime.h' : 'HAVE_UTIME_H',
	'winsock2.h' : 'HAVE_WINSOCK2_H',
	'winsock.h' : 'HAVE_WINSOCK_H',
	'wordexp.h' : 'HAVE_WORDEXP_H' }

os_system_libraries = {
		     'dl': 'HAVE_LIBDL',
		     'comdlg32': 'HAVE_LIBCOMDLG32',
		     'msvcrt': 'HAVE_LIBMSVCRT',
		     'shell32': 'HAVE_LIBSHELL32',
		     'wsock32': 'HAVE_LIBWSOCK32',
		     'ws2_32': 'HAVE_LIBWS2_32',
		     'unicode': 'HAVE_LIBUNICODE',
		     'xnet': 'HAVE_LIBXNET',
		     }


os_system_functions = {
 '_NSGetEnviron' : 'HAVE__NSGETENVIRON',
 'access' : 'HAVE_ACCESS',
 'basename' : 'HAVE_BASENAME',
 'chdir' : 'HAVE_CHDIR',
 'clock' : 'HAVE_CLOCK',
 'clock_gettime' : 'HAVE_CLOCK_GETTIME',
 'ctime' : 'HAVE_CTIME',
 'dlopen' : 'HAVE_DLOPEN',
 'dup2' : 'HAVE_DUP2',
 'erf' : 'HAVE_ERF',
 'feclearexcept' : 'HAVE_FECLEAREXCEPT',
 'feenableexcept' : 'HAVE_FEENABLEEXCEPT',
 'fesetexceptflag' : 'HAVE_FESETEXCEPTFLAG',
 'fesetround' : 'HAVE_FESETROUND',
 'fesettrapenable' : 'HAVE_FESETTRAPENABLE',
 'fetestexcept' : 'HAVE_FETESTEXCEPT',
 'fgetpos' : 'HAVE_FGETPOS',
 'fmemopen' : 'HAVE_FMEMOPEN',
 'fpclass' : 'HAVE_FPCLASS',
 'fpurge' : 'HAVE_FPURGE',
 'ftime' : 'HAVE_FTIME',
 'ftruncate' : 'HAVE_FTRUNCATE',
 'funopen' : 'HAVE_FUNOPEN',
 'getcwd' : 'HAVE_GETCWD',
 'getenv' : 'HAVE_GETENV',
 'getexecname' : 'HAVE_GETEXECNAME',
 'gethostbyname' : 'HAVE_GETHOSTBYNAME',
 'gethostent' : 'HAVE_GETHOSTENT',
 'gethostid' : 'HAVE_GETHOSTID',
 'gethostname' : 'HAVE_GETHOSTNAME',
 'gethrtime' : 'HAVE_GETHRTIME',
  'getpagesize' : 'HAVE_GETPAGESIZE',
 'getpid' : 'HAVE_GETPID',
 'getpwnam' : 'HAVE_GETPWNAM',
 'getrlimit' : 'HAVE_GETRLIMIT',
 'getrusage' : 'HAVE_GETRUSAGE',
 'gettid' : 'HAVE_GETTID',
 'gettimeofday' : 'HAVE_GETTIMEOFDAY',
 'getwd' : 'HAVE_GETWD',
 'glob' : 'HAVE_GLOB',
 'gmtime' : 'HAVE_GMTIME',
 'h_errno' : 'HAVE_H_ERRNO',
 'isatty' : 'HAVE_ISATTY',
 'isfinite' : 'HAVE_ISFINITE',
 'isinf' : 'HAVE_ISINF',
 'isnan' : 'HAVE_ISNAN',
 'kill' : 'HAVE_KILL',
 'labs' : 'HAVE_LABS',
 'link' : 'HAVE_LINK',
 'localtime' : 'HAVE_LOCALTIME',
 'lstat' : 'HAVE_LSTAT',
 'mbscasecoll' : 'HAVE_MBSCASECOLL',
 'mbscoll' : 'HAVE_MBSCOLL',
 'mbsnrtowcs' : 'HAVE_MBSNRTOWCS',
 'memmove' : 'HAVE_MEMCPY',
 'mkstemp' : 'HAVE_MKSTEMP',
 'mktemp' : 'HAVE_MKTEMP',
 'mktime' : 'HAVE_MKTIME',
 'mtrace' : 'HAVE_MTRACE',
 'nanosleep' : 'HAVE_NANOSLEEP',
 'open_memstream' : 'HAVE_OPEN_MEMSTREAM',
 'opendir' : 'HAVE_OPENDIR',
 'putenv' : 'HAVE_PUTENV',
 'rand' : 'HAVE_RAND',
 'random' : 'HAVE_RANDOM',
 'readlink' : 'HAVE_READLINK',
 'realpath' : 'HAVE_REALPATH',
 'regexec' : 'HAVE_REGEXEC',
 'rename' : 'HAVE_RENAME',
 'restartable_syscalls' : 'HAVE_RESTARTABLE_SYSCALLS',
 'rint' : 'HAVE_RINT',
 'sbrk' : 'HAVE_SBRK',
 'select' : 'HAVE_SELECT',
 'setbuf' : 'HAVE_SETBUF',
 'setitimer' : 'HAVE_SETITIMER',
 'setlinebuf' : 'HAVE_SETLINEBUF',
 'setlocale' : 'HAVE_SETLOCALE',
 'setsid' : 'HAVE_SETSID',
 'shmat' : 'HAVE_SHMAT',
 'sigaction' : 'HAVE_SIGACTION',
 'siggetmask' : 'HAVE_SIGGETMASK',
 'siginterrupt' : 'HAVE_SIGINTERRUPT',
 'signal' : 'HAVE_SIGNAL',
 'sigprocmask' : 'HAVE_SIGPROCMASK',
 'sigsetjmp' : 'HAVE_SIGSETJMP',
 'sleep' : 'HAVE_SLEEP',
 'snprintf' : 'HAVE_SNPRINTF',
 'socket' : 'HAVE_SOCKET',
 'srand' : 'HAVE_SRAND',
 'srand48' : 'HAVE_SRAND48',
 'srandom' : 'HAVE_SRANDOM',
 'ssize_t' : 'HAVE_SSIZE_T',
 'stat' : 'HAVE_STAT',
 'stpcpy' : 'HAVE_STPCPY',
 'stpncpy' : 'HAVE_STPNCPY',
 'system' : 'HAVE_SYSTEM',
 'tcflush' : 'HAVE_TCFLUSH',
 'time' : 'HAVE_TIME',
 'timegm' : 'HAVE_TIMEGM',
 'times' : 'HAVE_TIMES',
 'tmpnam' : 'HAVE_TMPNAM',
 'ttyname' : 'HAVE_TTYNAME',
 'usleep' : 'HAVE_USLEEP',
 'utime' : 'HAVE_UTIME',
 'var_timezone' : 'HAVE_VAR_TIMEZONE',
 'vfork' : 'HAVE_VFORK',
 'waitpid' : 'HAVE_WAITPID',
 'wcsdup' : 'HAVE_WCSDUP',
 'wcsnlen' : 'HAVE_WCSNLEN',
 'wordexp' : 'HAVE_WORDEXP',
'environ' : 'HAVE_ENVIRON',
		    }

os_system_symbols = {
 'socklen_t' : 'HAVE_SOCKLEN_T',
 'SIGINFO' : [ 'signal.h' , 'HAVE_SIGINFO' ],
 'SIGFPE' : [ 'signal.h' , 'HAVE_SIGFPE' ],
 'SIGPROF' : [ 'signal.h' , 'HAVE_SIGPROF' ],
 'SIGSEGV' : ['signal.h' , 'HAVE_SIGSEGV' ],
 'timezone' : ['time.h' , 'HAVE_VAR_TIMEZONE' ],
	      }

os_compiler_sizes = {
 }



# foreach o, def: os_build_options
# 	if get_option(o).found()
  	# header exists, do something
#   		os_conf_data += {def:1}
# 	endif
# endforeach
foreach file, def: os_system_includes
	if cc.has_header(file)
  	# header exists, do something
  		os_conf_data += {def:1}
	endif
endforeach

syslibs=[]
foreach file, def: os_system_libraries
        deplib = cc.find_library(file, required:false)
	if deplib.found()
                syslibs += [deplib]
  		# header exists, do something
  		os_conf_data += {def:1}
	endif
endforeach

foreach file, def: os_system_functions
      	if  cc.has_function(file, dependencies:syslibs,include_directories : os_incdirs)
  		# header exists, do something
  		os_conf_data += {def:1}
	endif
endforeach

foreach sy, def: os_system_symbols
      	if  cc.has_header_symbol(def[0], sy, dependencies:syslibs,include_directories : os_incdirs)
  		# header exists, do something
  		os_conf_data += {def[1]:1}
	endif
endforeach

foreach type, def: os_compiler_sizes
  # header exists, do something
os_conf_data += {def : cc.sizeof(type,prefix:'#include <stdint.h>\n#include <stdbool.h>\n #include <wchar.h>\n #include <string.h>\n #include <setjmp.h>', include_directories : os_incdirs)}
endforeach

subdir("readline")

utf8Lib = shared_library('utf8proc', '../utf8proc/utf8proc.c', include_directories : incdirs)

osLib = shared_library('os', sources : os_sources, include_directories : os_incdirs,
           install : true,
           link_with : [utf8Lib,rlLib],
	   dependencies : syslibs)


