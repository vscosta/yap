


#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <wchar.h>
#include <unistd.h>

static FILE *ostream;

static char *protect_class( char where[], const char *what, size_t sz, int arity)
{
  size_t i;
  char *out = where;
  for (i=0;i<sz;i++) {
    int ch=what[i];
    if (ch=='_' || (isalnum(ch)&&ch!='U')) {
      *out++=ch;
    } else {
      out += sprintf(out,"U%xU",ch);
    }
  }
  out+=sprintf(out,"_%d",arity);
  *out = '\0';
  return where;
}

static char * infixpred_doc(char *line, char *end, ssize_t sz) {
  char  *pred, *start=line;
  char buf[4096];
  if((pred = strstr(line,"@infixpred"))!=NULL && (!end || pred <end) )       {
    fprintf(ostream, "%*s",  (int)(pred-start),start);
    char *arg1 = strtok(pred+10, " \t");
    char *op = strtok(NULL, " \t");
    char *arg2 = strtok(NULL, " \t");

    line = arg2+strlen(arg2)+1;
    fprintf(ostream, " @class %s\n@brief %s/%d %s **%s** %s ",
	    protect_class(buf,op,strlen(op),2),op,2,
	    arg1, op, arg2);
  }
  return line;
}

static int commas(char *args) {
  int nargs = 1;
  if (*args != '\0')
    do {
      if (*args == ',')
	nargs++;
      args++;
    } while (*args);
  return nargs;
}

static char * pred_doc(char *line, char *end, ssize_t sz) {
  char *name, *args, *pred, *start = line;
  char buf[4096];

  if((pred = strstr(line,"@pred"))!=NULL && (!end || pred <end) )       {
    char *prefix = strtok(pred, " \t");
    name = strtok(NULL, "(");
    if (name == NULL || name+strlen(name)==end) {
      name = prefix+strlen(prefix)+1;
      name = strtok(name, " \n");
      fprintf(ostream, "%*s @class %s\n@brief %s/%d **%s**",
	      (int)(pred-start),start,
	      protect_class(buf,name,strlen(name),0),name,0,name);
    } else {
      args = strtok(NULL, ")");
       line = args+strlen(args)+1;
      int arity = commas(args);
      fprintf(ostream, "%*s @class %s\n@brief %s/%d **%s(%s)**",
	      (int)(pred-start),line,
	      protect_class(buf,name,strlen(name),arity),
	      name, arity, name, args);
    }
  }
  return line;
}

static char *process_doc(char *line, ssize_t sz) {
  char *end = line+sz;
  line = infixpred_doc(line, end, sz);
  if ( sz == end-line)
  line = pred_doc(line, end, sz);
  if (line[0]) {
    fprintf(ostream,"%s",line);
    line = NULL;
  }
  return line;
}

#if 0

char *pi;
while (
       line &&
       (pi=strchr(line,'/'))!=NULL) {
-  char *pi0 = pi;
  if (!isdigit(pi0[1]))
    break;
  pi0--;
  while (pi0 >= line && (pi0[0]=='_'|| isalnum(pi0[0])))
    pi0--;
  pi0++;
  fprintf(ostream,"%*s @ref %s @\"%*s/%c\""  ,(int)(pi0-line),line,

	  protect_class(buf,pi0,(size_t)(pi-pi0),pi[1]),
	  (int)(pi-pi0),pi0,pi[1] );
  line = pi+2;
  if (!line[0])
    line = NULL;
 }

if (line && (pi=strstr(line,"*/"))) {
  fprintf(ostream,"%*s  %*s",(int)(pi-line),line,2,pi);
 }


if (line) {
  fprintf(ostream ,"%s",line);
 }
}
#endif

static bool codecomm(char *p)
{
  bool rc;
  if (p[0] =='/' && p[1]=='/')
    rc= p[2] == '/' || p[2] == '!';
  if (p[0] =='/' && p[1]=='*')
    rc = p[2] == '*' || p[2] == '!';
  if (p[3]=='<')
    return rc && p[3]=='<' && isspace(p[4]);
  return rc && isspace(p[3]);
}

static FILE * input(char *inp) {
    if (strstr(inp,".yap" ) ||
	     strstr(inp,".ypp" ) ||
	     strstr(inp,".pl" )) {
    char s[2048];
    //      execl(YAPBIN, "-L",  PLFILTER, "--", inp, NULL);
    snprintf(s, 2047, "%s %s -L %s -- %s", YAPBIN, YAPSTARTUP, PLFILTER, inp);
    system(s);
    exit(0);
    } else if (strstr(inp,".py")) {
        char s[2048];
      snprintf(s,2047,"cat %s",inp);
      system(s);
      exit(0);
      return NULL;
    }  else {
    return fopen(inp, "r");
  }
    return NULL;
}

static FILE *output(char *inp) {
  char s[2048], *p;
  chdir("yapdocs");
  s[0] = '\0';
  p = s;
      int i=0;
      while (inp[i]) {
        if (inp[i] == '/')
          p[i] = '_';
        else
          p[i] = inp[i];
        i++;
      }
      p[i] = '\0';
    return fopen( s,"w");
}


int main(int argc, char *argv[]) {
    int current_line=1;
  size_t n;
  char *line=NULL, *p;
  FILE *f;
  if (argc == 1) {
    f= stdin;
    ostream = stdout;
  } else if (argc==2) {
    f = input(argv[1]);
    ostream = output(argv[1] );
  } else {
    f = input(argv[1]);
    ostream = output(argv[1]);
  }
  bool code=false;
    while (getline(&line,&n,f)>0) {
        current_line++;
      char *line0=line;
      if ((p=strstr(line,"/*"))) {
	/* code comment */
	if (p!=line) {
	  fprintf(ostream, "%*s", p-line, line);
         }
         line = p;
	code = codecomm(p);
	do {
	if (!line0) {
	line0=line;
        current_line++;
        }
	if ((p = strstr(line, "*/"))) {
	    p += 2;
	    if (code) {
              process_doc(line, p - line);
	      
	    } else {
              fprintf(ostream, "%s", line);
	    }
	    break;
	  } else {
	    if (code) {
	      process_doc(line, strlen(line));
            } else {
              fprintf(ostream, "%s", line);
	    }
	  }
	if (line0) {
	  free(line0);
	}
	line0 = line = NULL;
	 } while (getline(&line,&n,f));
      } else if ((p=strstr(line,"//"))) {
	code = (code || codecomm(p));

	line = p;
	if (code) {
	fprintf(ostream,"\n# %d \"%s\"\n",current_line, argv[1]);
	  process_doc(line, strlen(line));
	} else fprintf(ostream, "%s",line);
      } else {
	fprintf(ostream, "%s",line);
	code  = false;;
      }
      if (feof(f)) {
	exit(0);
      }
      if (line0) {
      free(line0);
      }
      line0=line = NULL;
    }

}




  ///
