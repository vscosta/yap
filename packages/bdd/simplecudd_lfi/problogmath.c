/******************************************************************************\
*                                                                              *
*    SimpleCUDD library (www.cs.kuleuven.be/~theo/tools/simplecudd.html)       *
*  SimpleCUDD was developed at Katholieke Universiteit Leuven(www.kuleuven.be) *
*                                                                              *
*  Copyright T. Mantadelis, A. Kimmig, B. Gutmann                              *
*            and Katholieke Universiteit Leuven 2008                           *
*                                                                              *
*  Author:       Bernd Gutmann                                                 *
*  File:         problogmath.c                                                 *
*  $Date:: 2009-12-06 00:55:33 +0100 (Sun, 06 Dec 2009)           $            *
*  $Revision:: 2919                                               $            *
*                                                                              *
*                                                                              *
********************************************************************************
*                                                                              *
*        The "Artistic License"                                                *
*                                                                              *
*         Preamble                                                             *
*                                                                              *
* The intent of this document is to state the conditions under which a         *
* Package may be copied, such that the Copyright Holder maintains some         *
* semblance of artistic control over the development of the package,           *
* while giving the users of the package the right to use and distribute        *
* the Package in a more-or-less customary fashion, plus the right to make      *
* reasonable modifications.                                                    *
*                                                                              *
* Definitions:                                                                 *
*                                                                              *
*   "Package" refers to the collection of files distributed by the             *
*   Copyright Holder, and derivatives of that collection of files              *
*   created through textual modification.                                      *
*                                                                              *
*   "Standard Version" refers to such a Package if it has not been             *
*   modified, or has been modified in accordance with the wishes               *
*   of the Copyright Holder as specified below.                                *
*                                                                              *
*   "Copyright Holder" is whoever is named in the copyright or                 *
*   copyrights for the package.                                                *
*                                                                              *
*   "You" is you, if you're thinking about copying or distributing             *
*   this Package.                                                              *
*                                                                              *
*   "Reasonable copying fee" is whatever you can justify on the                *
*   basis of media cost, duplication charges, time of people involved,         *
*   and so on.  (You will not be required to justify it to the                 *
*   Copyright Holder, but only to the computing community at large             *
*   as a market that must bear the fee.)                                       *
*                                                                              *
*   "Freely Available" means that no fee is charged for the item               *
*   itself, though there may be fees involved in handling the item.            *
*   It also means that recipients of the item may redistribute it              *
*   under the same conditions they received it.                                *
*                                                                              *
* 1. You may make and give away verbatim copies of the source form of the      *
* Standard Version of this Package without restriction, provided that you      *
* duplicate all of the original copyright notices and associated disclaimers.  *
*                                                                              *
* 2. You may apply bug fixes, portability fixes and other modifications        *
* derived from the Public Domain or from the Copyright Holder.  A Package      *
* modified in such a way shall still be considered the Standard Version.       *
*                                                                              *
* 3. You may otherwise modify your copy of this Package in any way, provided   *
* that you insert a prominent notice in each changed file stating how and      *
* when you changed that file, and provided that you do at least ONE of the     *
* following:                                                                   *
*                                                                              *
*    a) place your modifications in the Public Domain or otherwise make them   *
*    Freely Available, such as by posting said modifications to Usenet or      *
*    an equivalent medium, or placing the modifications on a major archive     *
*    site such as uunet.uu.net, or by allowing the Copyright Holder to include *
*    your modifications in the Standard Version of the Package.                *
*                                                                              *
*    b) use the modified Package only within your corporation or organization. *
*                                                                              *
*    c) rename any non-standard executables so the names do not conflict       *
*    with standard executables, which must also be provided, and provide       *
*    a separate manual page for each non-standard executable that clearly      *
*    documents how it differs from the Standard Version.                       *
*                                                                              *
*    d) make other distribution arrangements with the Copyright Holder.        *
*                                                                              *
* 4. You may distribute the programs of this Package in object code or         *
* executable form, provided that you do at least ONE of the following:         *
*                                                                              *
*    a) distribute a Standard Version of the executables and library files,    *
*    together with instructions (in the manual page or equivalent) on where    *
*    to get the Standard Version.                                              *
*                                                                              *
*    b) accompany the distribution with the machine-readable source of         *
*    the Package with your modifications.                                      *
*                                                                              *
*    c) give non-standard executables non-standard names, and clearly          *
*    document the differences in manual pages (or equivalent), together        *
*    with instructions on where to get the Standard Version.                   *
*                                                                              *
*    d) make other distribution arrangements with the Copyright Holder.        *
*                                                                              *
* 5. You may charge a reasonable copying fee for any distribution of this      *
* Package.  You may charge any fee you choose for support of this              *
* Package.  You may not charge a fee for this Package itself.  However,        *
* you may distribute this Package in aggregate with other (possibly            *
* commercial) programs as part of a larger (possibly commercial) software      *
* distribution provided that you do not advertise this Package as a            *
* product of your own.  You may embed this Package's interpreter within        *
* an executable of yours (by linking); this shall be construed as a mere       *
* form of aggregation, provided that the complete Standard Version of the      *
* interpreter is so embedded.                                                  *
*                                                                              *
* 6. The scripts and library files supplied as input to or produced as         *
* output from the programs of this Package do not automatically fall           *
* under the copyright of this Package, but belong to whoever generated         *
* them, and may be sold commercially, and may be aggregated with this          *
* Package.  If such scripts or library files are aggregated with this          *
* Package via the so-called "undump" or "unexec" methods of producing a        *
* binary executable image, then distribution of such an image shall            *
* neither be construed as a distribution of this Package nor shall it          *
* fall under the restrictions of Paragraphs 3 and 4, provided that you do      *
* not represent such an executable image as a Standard Version of this         *
* Package.                                                                     *
*                                                                              *
* 7. C subroutines (or comparably compiled subroutines in other                *
* languages) supplied by you and linked into this Package in order to          *
* emulate subroutines and variables of the language defined by this            *
* Package shall not be considered part of this Package, but are the            *
* equivalent of input as in Paragraph 6, provided these subroutines do         *
* not change the language in any way that would cause it to fail the           *
* regression tests for the language.                                           *
*                                                                              *
* 8. Aggregation of this Package with a commercial distribution is always      *
* permitted provided that the use of this Package is embedded; that is,        *
* when no overt attempt is made to make this Package's interfaces visible      *
* to the end user of the commercial distribution.  Such use shall not be       *
* construed as a distribution of this Package.                                 *
*                                                                              *
* 9. The name of the Copyright Holder may not be used to endorse or promote    *
* products derived from this software without specific prior written           *
* permission.                                                                  *
*                                                                              *
* 10. THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR              *
* IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED               *
* WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.          *
*                                                                              *
*         The End                                                              *
*                                                                              *
\******************************************************************************/

#include "general.h"
#include "problogmath.h"


double sigmoid(double x, double slope) {
  return 1.0 / (1.0 + exp(-x * slope));
}

// This function calculates the accumulated density of the normal distribution
// For details see G. Marsaglia, Evaluating the Normal Distribution, Journal of Statistical Software, 2004:11(4).
double Phi(double x)  {
  double s=x;
  double t=0.0;
  double b=x;
  double q=x*x;
  double i=1;

  // if the value is too small or too big, return
  // 0/1 to avoid long computations
  if (x < -10.0) {
    return 0.0;
  }

  if (x > 10.0) {
    return 1.0;
  }

  // t is the value from last iteration
  // s is the value from the current iteration
  // iterate until they are equal
  while(fabs(s-t) >= DBL_MIN) {
    t=s;
    i+=2;
    b*=q/i;
    s+=b;
  }

  return 0.5+s*exp(-0.5*q-0.91893853320467274178);
}

// integrates the normal distribution over [low,high]
double cumulative_normal(double low, double high, double mu, double sigma) {
  return Phi((high-mu)/sigma) - Phi((low-mu)/sigma);
}

// evaluates the density of the normal distribution
double normal(double x, double mu,double sigma) {
  double inner=(x-mu)/sigma;
  double denom=sigma*sqrt(2*3.14159265358979323846);
  return exp(-inner*inner/2)/denom;
}

double cumulative_normal_dmu(double low, double high,double mu,double sigma) {
  return normal(low,mu,sigma) - normal(high,mu,sigma);
}

double cumulative_normal_dsigma(double low, double high,double mu,double sigma) {
  return ((mu-high)*normal(high,mu,sigma) - (mu-low)*normal(low,mu,sigma))/sigma;
}


// this function parses two strings "$a;$b" and "???_???l$ch$d" where $a-$d are (real) numbers
// it is used to parse in the parameters of continues variables from the input file
density_integral parse_density_integral_string(char *input, const char *variablename) {
  density_integral result;
  int i;
  char garbage[64], s1[64],s2[64],s3[64],s4[64];

  if(sscanf(input, "%64[^;];%64[^;]", s1,s2) != 2) {
    fprintf(stderr, "Error at parsing the string %s in the function parse_density_integral_string\n",input);
    fprintf(stderr, "The string should contain 2 fields seperated by ; characters.\n");
    exit(EXIT_FAILURE);
  }

  if (IsRealNumber(s1)) {
    result.mu=atof(s1);
  } else {
    fprintf(stderr, "Error at parsing the string %s in the function parse_density_integral_string\n",input);
    fprintf(stderr, "%s is not a number\n",s1);
    exit(EXIT_FAILURE);
  }

  if (IsRealNumber(s2)) {
    result.sigma=atof(s2);
  } else {
    fprintf(stderr, "Error at parsing the string %s in the function parse_density_integral_string\n",input);
    fprintf(stderr, "%s is not a number\n",s2);
    exit(EXIT_FAILURE);
  }

 if (result.sigma<=0) {
    fprintf(stderr, "Error at parsing the string %s in the function parse_density_integral_string",input);
    fprintf(stderr, "The value for sigma has to be larger than 0.\n");

    exit(EXIT_FAILURE);
  }

  if (sscanf(variablename,"%64[^lh]l%64[^lh]h%64[^lh]",garbage,s3,s4) != 3) {
    fprintf(stderr, "Error at parsing the string %s in the function parse_density_integral_string\n",variablename);
    fprintf(stderr, "The string should contain 2 fields seperated by ; characters.\n");
    exit(EXIT_FAILURE);
  }

  //  replace the d by . in s1 and s2
  for(i=0; s3[i]!='\0' ; i++) {
    if (s3[i]=='d') {
      s3[i]='.';
    }
    if (s3[i]=='m') {
      s3[i]='-';
    }
  }
  for(i=0; s4[i]!='\0' ; i++) {
    if (s4[i]=='d') {
      s4[i]='.';
    }
    if (s4[i]=='m') {
      s4[i]='-';
    }
  }

  if (IsRealNumber(s3)) {
    result.low=atof(s3);
  } else {
    fprintf(stderr, "Error at parsing the string %s in the function parse_density_integral_string\n",input);
    fprintf(stderr, "%s is not a number\n",s1);
    exit(EXIT_FAILURE);
  }

 if (IsRealNumber(s4)) {
    result.high=atof(s4);
  } else {
    fprintf(stderr, "Error ar parsing the string %s in the function parse_density_integral_string\n",input);
    fprintf(stderr, "%s is not a number\n",s1);
    exit(EXIT_FAILURE);
  }


  if (result.low>result.high) {
    fprintf(stderr, "Error ar parsing the string %s in the function parse_density_integral_string\n",input);
    fprintf(stderr, "The value for low has to be larger than then value for high.\n");
    exit(EXIT_FAILURE);
  }


  return result;
}
