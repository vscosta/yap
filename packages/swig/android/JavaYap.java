package pt.up.fc.dcc.yap;

/**** using sqlite
      For example,the following:

      import android.database.sqlite.SQLiteDatabase;

      should be replaced with:

      import org.sqlite.database.sqlite.SQLiteDatabase;

      As well as replacing all uses of the classes in the android.database.sqlite.* namespace, the application must also be sure to use the following two:

      org.sqlite.database.SQLException
      org.sqlite.database.DatabaseErrorHandler

      instead of:

      android.database.SQLException
      android.database.DatabaseErrorHandler

      Aside from namespace changes, there are other differences from the stock Android interface that applications need to be aware of:

      The SQLiteStatement.simpleQueryForBlobFileDescriptor() API is not available. The collation sequence "UNICODE" is not available. The collation sequence "LOCALIZED", which normally changes with the system's current locale, is always equivalent to SQLite's built in collation BINARY.

****/

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;
import android.widget.ScrollView;
import android.text.method.ScrollingMovementMethod;
import android.content.pm.PackageManager;
import android.content.pm.PackageInfo;
import  android.content.pm.PackageManager.NameNotFoundException; 
import  android.util.Log;
import android.content.res.AssetManager;
import 	android.widget.EditText;
import 	java.text.ParseException;
import org.sqlite.database.sqlite.SQLiteDatabase;
import org.sqlite.database.sqlite.SQLiteStatement;
import org.sqlite.database.sqlite.SQLiteDatabaseCorruptException;
import org.sqlite.database.sqlite.SQLiteOpenHelper;

import org.sqlite.database.SQLException;
import org.sqlite.database.DatabaseErrorHandler;
class DoNotDeleteErrorHandler implements DatabaseErrorHandler {
  private static final String TAG = "DoNotDeleteErrorHandler";
  public void onCorruption(SQLiteDatabase dbObj) {
    Log.e(TAG, "Corruption reported by sqlite on database: " + dbObj.getPath());
  }
}
public class JavaYap extends Activity
{
	TextView outputText = null;
	ScrollView scroller = null;
	YAPEngine eng = null;
	EditText text;
	String str;
	String buf;

	void runQuery(String str) 
	{
		try
		{
			YAPQuery q = eng.query( str );

			if (BuildConfig.DEBUG) {
				Log.i(TAG, "onQueryButtonClick called");
			} 	
			YAPListTerm vs0 = q.namedVars();
			Boolean rc;

			// text.setText("");
			if (vs0.nil()) {
				if (BuildConfig.DEBUG) {
					Log.i(TAG, "q0=\n");
				} 	
				if (q.next()) {
					outputText.append( "yes\n" );
				} else {
					outputText.append( "no\n" );
				}
			} else {
				int i=1;
				if (BuildConfig.DEBUG) {
				  Log.i(TAG, "q1= "+vs0.text()+"\n");

				} 	
				while (rc = q.next()) {
					if (BuildConfig.DEBUG) {
					  Log.i(TAG, "q= "+vs0.text()+"\n");

					} 	
				YAPListTerm vs = vs0;
					while(!vs.nil()){
						YAPTerm eq = vs.car();
						//outputText.append(Integer.toString(i) + ": " + eq.text() );
						outputText.append(Integer.toString(i++) + ":\t" + eq.getArg(1).text() + " = " + eq.getArg(2).text() +"\n" );
						vs = vs.cdr();
					}	
				}
			}
			q.close();
		} catch(Exception e){
			outputText.append("Exception thrown  :" + e);
			return;
		}
	}
	
	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		String s = null;
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		try {
			PackageManager m = getPackageManager();
			s = getPackageName();
			PackageInfo p = m.getPackageInfo(s, 0);
			//s = p.applicationInfo.dataDir;
			AssetManager mgr = getResources().getAssets();
			load(mgr);
		} catch(NameNotFoundException e) { 
			Log.e(TAG, "Couldn't find package  information in PackageManager", e); 
		} 
		Log.i(TAG, "mgr=" +mgr); 

		text = (EditText)findViewById(R.id.EditText01); 
		outputText = (TextView)findViewById(R.id.OutputText);
		outputText.setText("Application " + s + "\nPress 'Query' to start...\n");
		outputText.setMovementMethod(new ScrollingMovementMethod());
		scroller = (ScrollView)findViewById(R.id.Scroller);
		eng = new YAPEngine(   );  
		Log.i(TAG, "engine done");
		JavaCallback callback = new JavaCallback( outputText );
		// set the Java Callback	
		eng.setYAPCallback(callback);
		if (BuildConfig.DEBUG) {
			Log.i(TAG, "callback done");
		} 	
	}

	public void onClearButtonClick(View view)
	{
		if (BuildConfig.DEBUG) {
			Log.i(TAG, "onClearButtonClick called");
		} 	
		// Ensure scroll to end of text
		scroller.post(new Runnable() {
			public void run() {
				scroller.fullScroll(ScrollView.FOCUS_DOWN);
				text.setText("");	
			}
		});
	}
	
	public void onQueryButtonClick(View view)
	{
		if (BuildConfig.DEBUG) {
			Log.i(TAG, "onQueryButtonClick called");
		} 	
		// Ensure scroll to end of text
		scroller.post(new Runnable() {
			public void run() {
				scroller.fullScroll(ScrollView.FOCUS_DOWN);
				str = text.getText().toString();
				outputText.append("?- " + str);
				Log.i(TAG, "onQueryButtonClick "+str + "\n");
				runQuery(str);
			}
		});
	}

	public void onQuerySelectionButtonClick(View view)
	{
		if (BuildConfig.DEBUG) {
			Log.i(TAG, "onQuerySelectionButtonClick called");
		} 	
		// Ensure scroll to end of text
		scroller.post(new Runnable() {
			public void run() {
				scroller.fullScroll(ScrollView.FOCUS_DOWN);
				int startSelection = text.getSelectionStart();
				int endSelection = text.getSelectionEnd();
				str = text.getText().toString().substring( startSelection, endSelection );
				Log.i(TAG, "onQuerySelectionButtonClick "+str);
				outputText.append("?- " + str + "\n");
				runQuery(str);
			}
		});
	}


	/** static constructor */
	static {
		System.loadLibrary("android");
		System.loadLibrary("log");
		System.loadLibrary("gmp");
		System.loadLibrary("sqliteX");
		System.loadLibrary("example");
	}

	private static native void load(AssetManager mgr);

	private AssetManager mgr;

	private static final String TAG = "JavaYap";

}

class JavaCallback extends YAPCallback
{
	TextView output;
	
  public JavaCallback( TextView outputText )
  {
    super();
    output =  outputText;
    Log.i(TAG, "java callback init");
  }


  public void run()
  {
      Log.i(TAG, "java callback ");
      System.out.println("JavaCallback.run() ");
  }
  
  public void run(String s)
  {
    Log.i(TAG, "java callback ");
    output.append(s);
 }
  
  private static final String TAG = "JavaCallback";

}
