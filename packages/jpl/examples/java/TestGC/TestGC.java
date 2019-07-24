import java.util.Map;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.JPLException;
import org.jpl7.PrologException;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;
import org.jpl7.Variable;

public class TestGC
{ public static void main(String argv[])
  { int max;

   Query.hasSolution("consult('test.pl')");

   if ( argv.length > 0 )
     max = Integer.parseInt(argv[0]);
   else
     max = 10;

   System.out.println(max + " iterations");

   for(int i=0; i<max; i++) {
     Query q = new Query("testpred", new Term[] { new Atom("a"+i) });
     q.oneSolution();
   }

   Query.hasSolution("stats");
  }
}
