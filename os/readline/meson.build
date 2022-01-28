
# configuratin data for config.h
rl_conf_data = {}

rl_sources = [		'readline.c']
rl_build_options = {
		  'readline' : 'USE_READLINE',
		   'readline-provider' :  'READLINE_PROVIDER'
		}

#default YAP directories
rl_dirs = ['..','../../H','../../H/generated','../../JIT/HPP','../../OPTYap','../../utf8proc','../../include','.']

rl_incdirs = include_directories(orl_dirs)


if get_option('readline-provider') == 'readline'
	readline = cc.find_library('readline', required: get_option('readline'))
	if readline.found()
		rl_conf_data += {'HAVE_READLINE': 1}
	endif

	if cc.has_function('rl_replace_line', prefix: '#include <stdio.h>\n #include <readline/readline.h>', dependencies: [readline])
		rl_conf_data += { 'HAVE_READLINE_REPLACE_LINE' : 1}
	endif
else # editline
	readline = dependency('libedit', required: get_option('readline'))
	if readline.found()
		rl_conf_data += {'HAVE_EDITLINE' : 1 }
	endif
endif

rl_build_options = {
		  'readline' : 'USE_READLINE',
		   'readline-provider' :  'READLINE_PROVIDER'
		}

configure_file(
  output : 'YapIOConfig.h',
  configuration : os_conf_data+rl_conf_data  )


rlLib = shared_library('YAPrl', sources : rl_sources, include_directories : rl_incdirs,
           install : true,
           link_with : [utf8Lib],
	   dependencies : [syslibs,readline])

