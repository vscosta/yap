


#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <wchar.h>
#include <unistd.h>

static char *protect_class( char where[], const char *what, size_t sz, int arity)
{
  size_t i;
  where[0] = 'P';
  char *out = where+1;
  for (i=0;i<sz;i++) {
    int ch=what[i];
    if (isalnum(ch)|| ch == '_') {
	*out++=ch;
    } else 
      switch(ch) {
      case '=':
	out = stpcpy(out,"_eq");
	break;
      case '<':
	out = stpcpy(out,"_lt");
	break;
      case '>':
	out = stpcpy(out,"_gt");
	break;
      case '!':
	out = stpcpy(out,"_ct");
	break;
      case '-':
	out = stpcpy(out,"_eq");
	break;
      case '+':
	out = stpcpy(out,"_pl");
	break;
      case '*':
	out = stpcpy(out,"_st");
	break;
      case '/':
	out = stpcpy(out,"_sl");
	break;
      case '$':
	out = stpcpy(out,"_dl");
	break;
      case '[':
	out = stpcpy(out,"_os");
	break;
      case ']':
	out = stpcpy(out,"_ls");
	break;
      case '.':
	out = stpcpy(out,"_dt");
	break;
      default:
	fprintf(stderr, "ERROR:  missing suport for %c.n",ch);
	return NULL;
      }
  }
  out=stpcpy(out,"_sl");
  out[0] = '0'+ arity;
  out[1] = '\0';
  return where;
}

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

int main(int argc, char *argv[]) {
    size_t n;
    char *line=NULL;
    char buf[256];
    bool in_star = false;
    if (strstr(argv[1],".yap" ) ||
	strstr(argv[1],".ypp" ) ||
	strstr(argv[1],".pl" )) {
      char s[2048];
      //      execl(YAPBIN, "-L",  PLFILTER, "--", argv[1], NULL);
      snprintf(s, 2047, "%s %s -L %s -- %s", YAPBIN, YAPSTARTUP, PLFILTER, argv[1]);
      system(s);
      exit(0);
	}
	
    FILE *f = fopen(argv[1],"r");

    bool code_comment=false;
    while ((getline(&line,&n,f)) >0) {
      char *start, *pred, *lspace = line;
      char *line0 = start = line;

      /// ignore       lspace = line;
      while (isblank(lspace[0])) {
	  lspace++;
	  if (lspace[0] =='\n' ||
	      lspace[0] == '\0')
	fprintf(stdout,"%s",line);
	    continue;
	}
      /// check for line comments
      /// cannot be in a //* comment
      if (!in_star) {
	start=strstr(line,"//");
	///
	if ( start) {
	  char *lspace;
	  for (lspace=line;lspace <start;lspace++) {
	  };
	  /// new comment!
	  code_comment|=lspace == start && codecomm(start);
	}
      } else {
	if ((start = strstr(line, "/*"))) {
	  in_star = true;
	  code_comment=(start[2]=='*' || start[2]=='?') && isspace(start[3]);
	  fprintf(stderr,"in line=%s\n",line);
	}
      }
 

	  if (code_comment &&
	      ((pred = strstr(line,"@pred"))!=NULL) )      {
	    int arity=0, i;
	    char *start,*p0, *args;
	    start =pred;
		pred +=5;
		while(isblank(*pred++));
		p0=pred-1;
		// predicate name
		if (pred[0]=='\'') {
		  while(*pred++ != '\'');
		} else {
		  while(!isblank(*pred) && pred[0] != '(') {
		    pred++;
		  }
		  if  (isblank(pred[0])) {
				fprintf(stdout,"%.*s @class %s  ** \"%.*s\" **%s",
			(int)(start-line),line,
			protect_class(buf,p0,(pred-p0),0),
			(int)(pred-p0),p0,
			pred);
		continue;
		    }
		}
		args = pred;
		i=0;
		if (pred[i] == '(') {
		  int ch;
		  arity=1;
		  while((ch=pred[i++])!=')') {
		    if (ch==',') arity++;
			
		  }
		}
		pred +=i;
		fprintf(stdout,"%.*s @class %s ",
			(int)(start-line),line,
			protect_class(buf,p0,(int)(args-p0),arity)),
		  fprintf(stdout,"** \"%.*s\" ** %s \n",(int)(pred-p0),p0,pred);
		line=NULL;
			continue;
	      }
	  char *pi;
	  while (code_comment &&
		     line &&
		     (pi=strchr(line,'/'))!=NULL) {
		  char *pi0 = pi;
		  if (!isdigit(pi0[1]))
		    break;
		  pi0--;
		  while (pi0 >= line && (pi0[0]=='_'|| isalnum(pi0[0])))
		    pi0--;
		  pi0++;
		  fprintf(stdout,"%.*s @ref{%s}[\"%.*s/%c\"]"  ,(int)(pi0-line),line,
			  protect_class(buf,pi0,(size_t)(pi-pi0),pi[1]),
			  (int)(pi-pi0),pi0,pi[1]-'0' );
		  line = pi+2;
		  if (!line[0])
		    line = NULL;
		}
		
	      if (line && (pi=strstr(line,"*/"))) {
			  code_comment = false;
		  in_star=false;
		  fprintf(stdout,"%.*s  %.*s",(int)(pi-line),line,2,pi);
	  line = pi+2;
		}

	      
      if (line) {
	fprintf(stdout,"%s",line);
	line=NULL;
      }
      free(line0);
    }
    return 0;
    } 

///
 
