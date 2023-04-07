
#include "yapi.hh"
#include "pugixml.hpp"

#include <iostream>

extern "C"  void libxml_yap_init ();

/**
 *
 * XML2YAP implements a non-validating XML to Prolog parser.
 */
class XML2YAP {
public:
  pugi::xml_document doc;
  bool strings_to_strings;
  bool strings_to_atoms;
  bool interpret_strings ;

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
	  std::vector <Term> args;
	  std::vector <Term> children;
	  if (ASP-HR < 16*1024)
	    throw YAPError(__FILE__,__FUNCTION__,__LINE__,RESOURCE_ERROR_STACK,MkStringTerm(node.name()),"xml_load to tree");
	for (pugi::xml_attribute attr = node.first_attribute(); attr; attr = attr.next_attribute())
	  {
	    if (attr.name()[0] != '\0') {
	      if (attr.value()[0] != '\0') {
		Term el = val2term(attr.value());
		args.push_back(YAPApplTerm(attr.name(), el).pop_t());
	      } else {
		args.push_back(YAPAtomTerm(std::string(attr.name())).pop_t());
	      }
	    } else {
	      if (attr.value()[0] != '\0') {
		Term el = val2term(attr.value());
		args.push_back(el);
	      }
	    }
	  }
	out = YAPListTerm(args).pop_t();
	 	children.push_back(out);
	for (pugi::xml_node n = node.first_child(); n; n = n.next_sibling()) {
	  YAPTerm max = YAPTerm();
	  Term v = visitor(n);
	  children.push_back(v);
	  max.reset();
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

 XML2YAP(std::string docf)
{
  CACHE_REGS

   pugi::xml_parse_result result = doc.load_file(docf.c_str());


  if (result.status!=pugi::status_ok)
     throw YAPError(SOURCE(),EXISTENCE_ERROR_STREAM,MkStringTerm(docf.c_str()),
						   strerror(errno));
}


};



extern "C" bool xml_load()
{
  CACHE_REGS
  std::string s = YAPTerm(ARG1).text();
  Term graph = 0;
  XML2YAP tree = XML2YAP(s);
  YAPTerm m;
do {
      CELL *hi = HR;
      m=YAPTerm();      
    try {
      graph = tree.pltree();
    } catch( YAPError e) {
      if (e.getID()==RESOURCE_ERROR_STACK) {
	HR = hi;
	m.reset();
	LOCAL_ActiveError->errorNo = YAP_NO_ERROR;
	  CalculateStackGap(PASS_REGS1);
	if(!Yap_dogcl((LCL0-H0)/2 * CellSize PASS_REGS)) {
	  return false;
      }
    }
    }
 }
    while(graph==0);
		m.reset();
  return Yap_unify(ARG2,graph);

}


extern "C" void libxml_yap_init () {
   YAPFLIP(xml_load, "load_xml", 2);
   YAPFLIP(xml_load, "xml_load2", 2);
   //YAPnewM(xml_load, "xml_load2", 2);
  };


// 
 
