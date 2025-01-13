/**
  * @file xml2pl.c reads an XML document
  *
  */
#include "yapi.hh"
#include "pugixml.hpp"

#include <iostream>

extern "C"  void libxml_yap_init ();
extern "C"  bool xml_load();

/**
 *
 * The class XML2YAP demonstrates  a non-validating XML to Prolog parser.
 */
class XML2YAP {
  YAPTerm inpt;

  pugi::xml_document doc;
public:
  bool ok;
  bool strings_to_strings = true;
  bool strings_to_atoms = false;
  bool interpret_strings = false;


/// This code manages the translation process.
XML2YAP(Term t)
{

  CACHE_REGS
ok = true;
  YAPTerm inpt = YAPTerm(t);
  std::string s = inpt.text();
  doc = pugi::xml_document();
//  std::cerr << s.c_str() << "\n";
    pugi::xml_parse_result result = doc.load_file(s.c_str());

  if (result.status!=pugi::status_ok) {
    std::cerr << "Error in " <<  s.c_str() << ": " << result.description() << "\n";
    ok = false;
    return;
    throw YAPError(SOURCE(),EXISTENCE_ERROR_STREAM,MkStringTerm(s.c_str()),
						   strerror(errno));
  }

};

~XML2YAP()
{
  doc.reset();
  inpt.reset();
};

  
  Term val2term(const char *s)
  {
    CACHE_REGS
      if (interpret_strings) {
	Term t = Yap_StringToNumberTerm(s, NULL,false);
	if (t!=0) return t;
	std::string st=s;
	if (st=="T" || st== "true"  || st== "yes")
	return TermTrue;
	if (st=="F" || st== "false"  || st== "no")
	return TermFalse;
    }
    if (strings_to_strings) {
      Term rc = MkStringTerm(s);
      return rc;
    }
    return MkAtomTerm(Yap_LookupAtom(s));
  }

  Term visitor(pugi::xml_node node)
  {
    Term rc  = visitor_(node);
  if (!rc) printf("ugh");
  return rc;
  }

  Term visitor_(pugi::xml_node node)
{
  // tag::code[]
CACHE_REGS
  if (ASP-HR < 32*1024) {
    YAPError *err = new YAPError();
    err->info->errorNo = RESOURCE_ERROR_STACK;
    throw err;
  }{
      switch(node.type()) {
      case pugi::node_null:
	return TermEmpty;
      case pugi::node_document:
	{
	std::vector <Term> children;
          for (pugi::xml_node n = node.first_child(); n; n = n.next_sibling()) {
	    Term t = visitor(n);
	    if (t!=TermEmpty)
	      children.push_back(t);
	  }
	  if (children.size()) {
	      return YAPListTerm( children ).pop_t();
	  } else {
	      return TermNil;
	  }
	}
       case pugi::node_element:
	{
	  Term out;
	  YAPTerm m = YAPTerm();
	  std::vector <Term> args = {};
	  std::vector <Term> atts = {};
	  std::vector <Term> children = {};
	  for (pugi::xml_attribute attr = node.first_attribute(); attr; attr = attr.next_attribute())
	  {
	    if (attr.name()[0] != '\0') {
	      if (attr.value()[0] != '\0') {
		Term el = val2term(attr.value());
	        atts.push_back(YAPApplTerm(attr.name(), el).gt());
	      } else {
		atts.push_back(YAPAtomTerm(std::string(attr.name())).gt());
	      }
	    } else {
	      if (attr.value()[0] != '\0') {
		Term el = val2term(attr.value());
		if (el!= TermNil)
		atts.push_back(el);
	      }
	    }
	  }
	for (pugi::xml_node n = node.first_child(); n; n = n.next_sibling()) {

		  if (n.type() == pugi::node_null) {
		    continue;
		  }
		  Term  u= visitor(n);
		  if (u!= TermNil)
		    children.push_back(u);
	}
	if (atts.empty() && children.empty()) {
	  m.reset();
	  return TermNil;
	}
	args.push_back(YAPListTerm(atts).gt());
		args.push_back(YAPListTerm(children).gt());
        out = YAPApplTerm(std::string(node.name()),args).gt();
	  m.reset();
	  return out;
	}
	case pugi::node_pcdata:
	  {
	  return val2term(node.value());
	  }
      case pugi::node_cdata:
	{
	  return val2term(node.value());
	}
      case pugi::node_comment:
      case pugi::node_pi:
      case pugi::node_declaration:
      case pugi::node_doctype:
	return TermEmpty;

      }
     // end::code[]
    return TermEmpty;
 }
};


Term pltree()
  {
    return visitor(doc);
  };




Term load_as_single_term()
{
  YAPTerm m;
  Term graph;
do {
CACHE_REGS
  CELL *hi = HR;
 m=YAPTerm();
 graph = 0;
    try {
     graph = pltree();
    } catch( YAPError *e) {
      if (e->getID()==RESOURCE_ERROR_STACK) {
	HR = hi;
	m.reset();
	LOCAL_ActiveError->errorNo = YAP_NO_ERROR;
	  CalculateStackGap(PASS_REGS1);
	if(!Yap_dogc(PASS_REGS1)) {
	  return 0 ;
	}
      }
    }
 }
 while(graph==0);
 m.reset();
 return graph;
  
};

};

/**
  * @pred load_xml(_XML_,_Graph_)
  */
 bool xml_load()
{
  CACHE_REGS
  Term graph = 0;
  XML2YAP *tree = new XML2YAP(ARG1);
  if (!tree->ok)
    return false;
  graph = tree->load_as_single_term();
// ~tree;
  return Yap_unify(ARG2,graph);

}


 void libxml_yap_init () {
   YAPFLIP(xml_load, "load_xml", 2);
   YAPFLIP(xml_load, "xml_load", 2);
   //YAPnewM(xml_load, "xml_load2", 2);
}



// 
 
