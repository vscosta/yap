//package pt.up.yap;
//
import java.io.* ;
import pt.up.yap.* ;
class JavaYAP {

  YAPQuery q;
  YAPEngine eng;
  InputStreamReader istream = new InputStreamReader(System.in) ;
  BufferedReader bufRead = new BufferedReader(istream) ;



  Boolean runQuery(String str)
  {
    try
      {
	q = eng.query( str );
      } catch(Exception e){
      System.out.println("Exception thrown  :" + e);
      return false;
    }
    return continueQuery();
  }

  Boolean continueQuery()
  {
    try
      {
         Boolean rc;

        rc = q.next();
        if (!rc) {
          q.close();
          return false;
        }
        YAPTerm vs = YAPTerm( q.namedVars() );
	System.out.println("Another one "+vs);
        while(!vs.nil()){
           eq = YAPTerm( vs.car() );
          //outputText.append(Integer.toString(i) + ": " + eq.text() );
          System.out.println(":\t" + eq.getArg(1).text() + " = " + eq.getArg(2).text() +"\n" );
          vs = vs.cdr();
        }
	return true;
      } catch(Exception e){
      System.out.println("Exception thrown  :" + e);
      q.close();
      return false;
    }
  }

  public void top_query()
  {
    eng = new YAPEngine(  );
    JavaCallback callback = new JavaCallback( "The man walked on the moon"  );
    // set the Java Callback
    eng.setYAPCallback(callback);
    System.out.println("Welcome To a simple Java YAP Program");
    try {
      String string;
      Boolean rc;
      System.out.println("Query? ");
      // Get Query
      while ((string = bufRead.readLine()) == null) {
	// skip empty lines
      }
      // first solution
      rc = runQuery(string);
      // no first solution, exit loop
      if (!rc) {
	System.out.println("No Dice, but Better Luck Next Time ");
	return;
      }
      // alreadyone solution
      while (true) {
	System.out.print("More Solutions enter ';' ");
	while ((string = bufRead.readLine()) == null) {
	}
	if (string.charAt(0) != ';' )
	  return;
	rc = continueQuery();
	if (!rc)
	  return;
      }
    }
    catch (IOException err) {
      System.out.println("Error reading line");
    }

  }

  public static void main(String args[])
  {
    System.loadLibrary("gmp");
    System.loadLibrary("Yap");
    System.loadLibrary("YAP++");
    System.loadLibrary("JAVAYap");

    JavaYAP y = new JavaYAP();

    while( true ) {
      y.top_query();
    }

    // System.out.println("Thanks for trying a simple Java YAP Program");

  }
}



class JavaCallback extends YAPCallback
{
  String callbacks;

  public JavaCallback( String outputText )
  {
    super();
    callbacks += outputText;
  }

  public void run(String s)
  {
    //callbacks.append(s);
  }

  private static final String TAG = "JavaCallback";

}
