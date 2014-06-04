package org.swig.simple;

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

public class SwigSimple extends Activity
{
    TextView outputText = null;
    ScrollView scroller = null;
	YAPEngine eng = null;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
    	
        super.onCreate(savedInstanceState);
       	String s = "ugh";
                setContentView(R.layout.main);

        try {
        	PackageManager m = getPackageManager();
        	s = getPackageName();
        	PackageInfo p = m.getPackageInfo(s, 0);
        	s = p.applicationInfo.dataDir;
            mgr = getResources().getAssets();
            load(mgr);
      } catch(NameNotFoundException e) { 
            Log.e(TAG, "Couldn't find package  information in PackageManager", e); 
        } 
        
        outputText = (TextView)findViewById(R.id.OutputText);
        outputText.setText("Application " + s + "\nPress 'Run' to start...\n");
        outputText.setMovementMethod(new ScrollingMovementMethod());
        YAPParams p = new YAPParams();
        eng = new YAPEngine(  p );      
   
        scroller = (ScrollView)findViewById(R.id.Scroller);
    }

    public void onRunButtonClick(View view)
    {
      outputText.append("Started...\n");
      if (BuildConfig.DEBUG) {
    	  Log.i(TAG, "onRunButtonClick called");
    	} 
      outputText.append("Finished!\n");

      // Ensure scroll to end of text
      scroller.post(new Runnable() {
        public void run() {
          scroller.fullScroll(ScrollView.FOCUS_DOWN);
          String s = "['/assets/share/Yap/lists'].\n";
          outputText.append(s);
          YAPQuery q = eng.query(s);
          q.next();
          String s = "member(X, [1,2,3]).\n";
          outputText.append(s);
          YAPQuery q = eng.query(s);
          q.next();
          String sn = 
        }
      });
    }


    /** static constructor */
    static {
        System.loadLibrary("android");
        System.loadLibrary("log");
        System.loadLibrary("example");
    }
    
    private static native void load(AssetManager mgr);

    private AssetManager mgr;

    private static final String TAG = "SwigSimple";

}
