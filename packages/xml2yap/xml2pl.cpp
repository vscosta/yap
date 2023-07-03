
#include "yapi.hh"
#include "pugixml.hpp"

#include <iostream>

extern "C"  void libxml_yap_init ();
extern "C"  bool xml_load();

/**
 *
 * XML2YAP implements a non-validating XML to Prolog parser.
 */
class XML2YAP {
  YAPTerm inpt;
  pugi::xml_document doc;
public:
  bool ok;
  bool strings_to_strings;
  bool strings_to_atoms;
  bool interpret_strings ;

  
XML2YAP(Term t)
{

  CACHE_REGS
ok = true;
  YAPTerm inpt = YAPTerm(t);
  std::string s = inpt.text();
  doc = pugi::xml_document();
  std::cerr << s.c_str() << "\n";
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
      if (t) return t;
      std::string st=s;
      if (st=="T" || st== "true"  || st== "yes")
	return TermTrue;
      if (st=="F" || st== "false"  || st== "no")
	return TermFalse;
    }
    if (strings_to_strings)
      return MkStringTerm(s);
    return MkAtomTerm(Yap_LookupAtom(s));
  }

  Term visitor(pugi::xml_node node)
{
  // tag::code[]
CACHE_REGS
    {
      switch(node.type()) {
      case pugi::node_null:
	return TermEmptyAtom;
      case pugi::node_document:
	{
	std::vector <Term> children;
          for (pugi::xml_node n = node.first_child(); n; n = n.next_sibling()) {
	    Term t = visitor(n);
	    if (t!=TermEmptyAtom)
	      children.push_back(t);
	  }
	  if (children.size())
	    return YAPListTerm( children ).pop_t();
	  else
	    return TermNil;
	}
       case pugi::node_element:
	{
	  Term out;
	  	  	  YAPTerm m = YAPTerm();
			  std::vector <Term> args = {}
;
	  std::vector <Term> children = {};
	  if (ASP-HR < 16*1024)
	    throw YAPError(__FILE__,__FUNCTION__,__LINE__,RESOURCE_ERROR_STACK,MkStringTerm(node.name()),"xml_load to tree");
	for (pugi::xml_attribute attr = node.first_attribute(); attr; attr = attr.next_attribute())
	  {
	    if (attr.name()[0] != '\0') {
	      if (attr.value()[0] != '\0') {
		Term el = val2term(attr.value());
		args.push_back(YAPApplTerm(attr.name(), el).gt());
	      } else {
		args.push_back(YAPAtomTerm(std::string(attr.name())).gt());
	      }
	    } else {
	      if (attr.value()[0] != '\0') {
		Term el = val2term(attr.value());
		args.push_back(el);
	      }
	    }
	  }
	out = YAPListTerm(args).gt();
	 	children.push_back(out);
		for (pugi::xml_node n = node.first_child(); n; n = n.next_sibling()) {

		  if (n.type() == pugi::node_null) {
		    break;
		  }
		  Term  u= visitor(n);
	  if (ASP-HR < 16*1024)
 throw YAPError(__FILE__,__FUNCTION__,__LINE__,RESOURCE_ERROR_STACK,MkStringTerm(node.name()),"xml_load to tree");

	  children.push_back(u);
		}    
	Term o= YAPApplTerm(node.name(), YAPListTerm(children).gt(	  )).gt();
       	m.reset();
	return o;
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
	return TermEmptyAtom;

      }
    // end::code[]
      }
    return TermEmptyAtom;
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
      CELL *hi = HR;
      m=YAPTerm();
    try {
     graph = pltree();
    } catch( YAPError e) {
      if (e.getID()==RESOURCE_ERROR_STACK) {
	HR = hi;
	m.reset();
	LOCAL_ActiveError->errorNo = YAP_NO_ERROR;
	  CalculateStackGap(PASS_REGS1);
	if(!Yap_dogcl((LCL0-H0)/2 * CellSize PASS_REGS)) {
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
 
