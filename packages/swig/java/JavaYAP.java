//package pt.up.fc.dcc.yap;

import java.io.* ;
//import YAP.* ;
class JavaYAP {

    YAPQuery q;
    Boolean running = false, compute = true;
    YAPListTerm vs0;
    YAPEngine eng;
    String string;
    InputStreamReader istream = new InputStreamReader(System.in) ;
    BufferedReader bufRead = new BufferedReader(istream) ;



    void runQuery(String str, Boolean more)
    {
	     try
		 {
		     // check if at initial query
		     if (!running) {
			 
			 q = eng.query( str );
			 // get the uninstantiated query variables.
			 vs0 = q.namedVars();
			 running = true;
		     }
		     // start computing
		     compute = true;
		     
		     Boolean rc = true;

		     // text.setText("");
		     if (vs0.nil()) {
			 if (compute && (rc = q.next())) {
			     System.out.println( "yes\n" );
			     running = compute = more;
			 } else {
			     System.out.println( "no\n" );
			     running = false;
			     compute = false;
			 }
		     } else {
			 int i = 0;
			 while (compute  && (rc = q.next()) ) {
			     YAPListTerm vs = q.namedVars();
			     while(!vs.nil()){
				 YAPTerm eq = vs.car();
				 //outputText.append(Integer.toString(i) + ": " + eq.text() );
				 System.out.println(Integer.toString(i++));
				 System.out.println(":\t" + eq.getArg(1).text() + " = " + eq.getArg(2).text() +"\n" );
				 vs = vs.cdr();
			     }
			     compute = more;
			 }
		     }
		     if ( !rc) {
			 q.close();
			 compute = true;
			 running = false;
		     }
		 } catch(Exception e){
		 System.out.println("Exception thrown  :" + e);
		 q.close();
		 compute = true;
		 running = false;
	     }
	 }

    public void loop()
    {
	eng = new YAPEngine(  );
        JavaCallback callback = new JavaCallback( string );
        // set the Java Callback
        eng.setYAPCallback(callback);
	System.out.println("Welcome To a simple Java YAP Program");
          try {
               System.out.println("Query? ");
	       while ((string = bufRead.readLine()) != null) {
		  if (running) {
		      if (string == ";") {
			  runQuery(string, true);
		      } else
			  break;
		  } else {
		      runQuery(string, false);
		  }
		  if (running)
		      System.out.print("More Solutions enter ';' ");
		  else
		      System.out.println("Query? ");
	      }
          }
          catch (IOException err) {
               System.out.println("Error reading line");
          }

    }
    
     public static void main(String args[])
     {
	 System.loadLibrary("/Users/vsc/Yap/bins/t/packages/swig/java/libNative");

	 JavaYAP y = new JavaYAP();

	 y.loop();

	 System.out.println("Thanks for trying a simple Java YAP Program");

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
