


#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <wchar.h>
#include <unistd.h>


int main(int argc, char *argv[]) {
    size_t n;
    char *line=NULL, *start, *pred;
    bool in_star = false, in_lcomm=false;
    if (strstr(argv[1],".yap" ) ||
	strstr(argv[1],".pl" )) {
      char s[2048];
      //      execl(YAPBIN, "-L",  PLFILTER, "--", argv[1], NULL);
      snprintf(s, 2047, "%s -L %s -- %s", YAPBIN, PLFILTER, argv[1]);
      system(s);
      exit(0);
	}
	
    FILE *f = fopen(argv[1],"r");

    while ((getline(&line,&n,f)) >0) {
      bool code_comment;
      char *line0 = start = line;
      if (!in_star) {
	if ((start = strstr(line, "//"))) {
	  in_lcomm=true;
	  code_comment=(start[2]=='/' ) && isblank(start[3]);
	} else  if ((start = strstr(line, "/*"))) {
	  in_star = true;
	  code_comment=(start[2]=='*' || start[2]=='?') && isspace(start[3]);
		       	}
      }  
	  else if (in_lcomm) {
	    in_lcomm = start[0]=='\n' || (start[1] && start[0]=='/' && start[1]=='/');
	  }
    
	  char *pi;
	  if (code_comment &&
	      ((pred = strstr(line,"@pred"))!=NULL) )      {
	    int arity=0, i;
	    char *p0, *args;
	    pred[0]='\0';
	fprintf(stdout,"%s",line);
	pred[0]='@';
	    
		pred +=5;
		while(isblank(*pred++));
		p0=--pred;
		// predicate name
		if (pred[0]=='\'') {
		  while(*pred++ != '\'');
		} else {
		  while(!isblank(*pred) && pred[0] != '(') {
		    pred++;
		}
		}
		args = pred;
		i=0;
		if (pred[i] == '(') {
		  int ch;
		  while((ch=pred[i++])!=')') {
		    if (ch==',') arity++;
			
		  }
		  arity++;
		}
		int ochar = pred[i],c;
		args[0]='\0';
		if (arity)
		  c='(';
		else
		  c=' ';
		fprintf(stdout,"@class P%s%d	\n	@brief **%s%c%s**\n",p0,arity,p0,c,args+1);
		args[0]=ochar;
		line=NULL;
	      }
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
		  if (pi > line) {
		    int o = pi0[0];
		    pi0[0]='\0';
		    fprintf(stdout,"%s",line);
		    pi0[0]=o;
		  }
		  pi[0]='\0';
		  fprintf(stdout,"[%s/%c](@ref classP%s%c.md) }",pi0,pi[1],pi0,pi[1] );
		  pi[0]='/';
		  line = pi+2;
		  if (!line[0])
		    line = NULL;
		}
		
	      if (line && (pi=strstr(line,"*/"))) {
			  code_comment = false;
		  in_star=false;
		  int ch=pi[2];
		  fprintf(stdout,"%s",line);
		  line[2]=ch;
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

