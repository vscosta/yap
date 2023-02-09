
#include "pugixml.hpp"
#include "yapi.hh"

#include <iostream>


/**
 *
 * XML2YAP implements a non-validating XML to Prolog parser.
 */
class XML2YAP {
public:
  pugi::xml_node doc;
  bool strings_to_strings;
  bool interpret_strings ;

  Term val2term(const char *s)
  {
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
	  std::vector <Term> args;
	  std::vector <Term> children;
	  if (ASP-HR < 1024)	 
	    throw YAPError(__FILE__,__FUNCTION__,__LINE__,RESOURCE_ERROR_STACK,MkStringTerm(node.name()),"load_xml to tree");
	for (pugi::xml_attribute attr = node.first_attribute(); attr; attr = attr.next_attribute())
	  {
	    if (attr.name()[0] != '\0') {
	      if (attr.value()[0] != '\0') {
		Term el = val2term(attr.value());
		args.push_back(YAPApplTerm(attr.name(), el).pop_t());
	      } else {
		args.push_back(YAPAtomTerm(attr.name()).pop_t());
	      }
	    } else {
	      if (attr.value()[0] != '\0') {
		Term el = val2term(attr.value());
		args.push_back(el);
	      }
	    }
	  }
	if (args.size()) {
	  out = YAPApplTerm(node.name(),args).pop_t();
	} else {
	  out = YAPAtomTerm(node.name()).pop_t();
	}
	for (pugi::xml_node n = node.first_child(); n; n = n.next_sibling()) {
	      Term v = visitor(n);
	      if (v!=TermEmptyAtom)
		children.push_back(v);
	    }
	
	    if (children.size()) {
	      return YAPApplTerm("{}", children).gt();
	    } else {
	      return out;
	    }
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
  pugi::xml_document doc;

  pugi::xml_parse_result result = doc.load_file(docf.c_str());


  if (result.status!=pugi::status_ok)
     throw YAPError(SOURCE(),EXISTENCE_ERROR_STREAM,MkStringTerm(docf.c_str()),
						   strerror(errno));
}


};
      
 



extern "C" {
  
bool load_xml(USES_REGS1)
{
  std::string s = YAPTerm(ARG1).text();
  Term graph = 0;
  XML2YAP tree = XML2YAP(s);
  do {
    CELL *hi = HR;
    try {
      graph = tree.pltree();
    } catch( YAPError e) {
      if (e.getID()==RESOURCE_ERROR_STACK) {
	HR = hi;
	LOCAL_ActiveError->errorNo = YAP_NO_ERROR;
	  CalculateStackGap(PASS_REGS1);
	if(!Yap_dogcl((LCL0-H0)/2 * CellSize)) {
	  Yap_ThrowError(RESOURCE_ERROR_STACK, TermNil,
			 "No Stack Space for reading XML");
	}
      } else {
	Yap_ThrowError(e.getID(), TermNil,
		       "XML error: %s", strerror(errno));
      }

    } catch (...) {
	if (errno) {
	    fprintf(stderr,"Unknown exception %s: I am failing this goal\n", strerror(errno));
	  } else {
	    fprintf(stderr,"Unknown exception: I am failing this goal\n");
	  }
	}
      } while (graph == 0);
  return Yap_unify(ARG2,graph);
};

  void libxml_yap_init (void) {
   YAPFLIP(load_xml, "load_xml", 2);
   YAPFLIP(load_xml, "load_xml2", 2);
   //YAPnewM(load_xml, "load_xml2", 2);
  };


}


// 
