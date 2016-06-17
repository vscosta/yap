package pt.up.yap.app;
/****
 * using sqlite
 * For example,the following:
 * <p/>
 * import android.database.sqlite.SQLiteDatabase;
 * <p/>
 * should be replaced with:
 * <p/>
 * import org.sqlite.database.sqlite.SQLiteDatabase;
 * <p/>
 * As well as replacing all uses of the classes in the android.database.sqlite.* namespace, the application must also be sure to use the following two:
 * <p/>
 * org.sqlite.database.SQLException
 * org.sqlite.database.DatabaseErrorHandler
 * <p/>
 * instead of:
 * <p/>
 * android.database.SQLException
 * android.database.DatabaseErrorHandler
 * <p/>
 * Aside from namespace changes, there are other differences from the stock Android interface that applications need to be aware of:
 * <p/>
 * The SQLiteStatement.simpleQueryForBlobFileDescriptor() API is not available. The collation sequence "UNICODE" is not available. The collation sequence "LOCALIZED", which normally changes with the system's current locale, is always equivalent to SQLite's built in collation BINARY.
 ****/

import android.content.Context;
import android.app.Activity;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.AssetManager;
import android.net.Uri;
import android.os.Bundle;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import android.text.method.ScrollingMovementMethod;
import android.util.Log;
import android.view.View;
import android.widget.EditText;
import android.widget.ScrollView;
import android.widget.TextView;

import com.google.android.gms.appindexing.Action;
import com.google.android.gms.appindexing.AppIndex;
import com.google.android.gms.appindexing.Thing;
import com.google.android.gms.common.api.GoogleApiClient;

import org.sqlite.database.DatabaseErrorHandler;
import org.sqlite.database.sqlite.SQLiteDatabase;

import pt.up.yap.lib.*;

class DoNotDeleteErrorHandler implements DatabaseErrorHandler {
    private static final String TAG = "DoNotDeleteErrorHandler";

    public void onCorruption(SQLiteDatabase dbObj) {
        Log.e(TAG, "Corruption reported by sqlite on database: " + dbObj.getPath());
    }
}

public class YAPDroid extends Activity {

    private static final String TAG = "YAPDroid";

    TextView outputText = null;
    ScrollView scroller = null;
    YAPEngine eng = null;
    EditText text;
    String str;
    String buf;
    YAPQuery q;
    Boolean running = false, compute = true;
    int i = 1;
    YAPListTerm vs0;
    private AssetManager mgr;
    /**
     * ATTENTION: This was auto-generated to implement the App Indexing API.
     * See https://g.co/AppIndexing/AndroidStudio for more information.
     */
    private GoogleApiClient client;

    // private static native void load(AssetManager mgr);


    void runQuery(String str, Boolean more) {
        try {
            // check if at initial query
            if (running) {
                if (q != null) q.close();
            }

            if (BuildConfig.DEBUG) {
                Log.i(TAG, "query " + str);
            }

            q = eng.query(str);
            // get the uninstantiated query variables.
            vs0 = q.namedVars();
            running = true;
            // start computing
            compute = true;

            if (BuildConfig.DEBUG) {
                Log.i(TAG, "onQueryButtonClick called");
            }

            Boolean rc = true;

            text.setText("?- ");
            if (vs0.nil()) {
                if (BuildConfig.DEBUG) {
                    Log.i(TAG, "q0=\n");
                }
                if (compute && (rc = q.next())) {
                    outputText.append("yes\n");
                    running = compute = more;
                } else {
                    outputText.append("no\n");
                    running = false;
                    compute = false;
                }
            } else {
                //      if (BuildConfig.DEBUG) {
                //         Log.i(TAG, "q1= " + vs0.text() + "\n");
                //     }
                while (compute && (rc = q.next())) {
                    YAPListTerm vs = q.namedVars();
                    while (!vs.nil()) {
                        if (BuildConfig.DEBUG) {
                            Log.i(TAG, "q= " + vs.text() + "\n");
                        }
                        YAPTerm eq = vs.car();
                        //outputText.append(Integr.toString(i) + ": " + eq.text() );
                        outputText.append(Integer.toString(i));
                        outputText.append(":\t" + eq.getArg(1).text() + " = " + eq.getArg(2).text() + "\n");
                        vs = vs.cdr();
                    }
                    compute = more;
                }
            }
            if (!rc) {
                outputText.append("no\n");
                if (q != null)
                q.close();
                q = null;
                compute = false;
                running = false;
            }
        } catch (Exception e) {
            outputText.append("Exception thrown  :" + e);
            if (q != null)
            q.close();
            compute = true;
            running = false;
        }
    }

    /**
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        String s = null;
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        try {
            PackageManager
                    m = getPackageManager();
            s = getPackageName();
            PackageInfo p = m.getPackageInfo(s, 0);
            //s = p.applicationInfo.dataDir;
            mgr = this.getAssets();

            /** static constructor */
        // follow this order carefully.
            System.loadLibrary("gmp");
            System.loadLibrary("Yap");
            System.loadLibrary("Yapsqlite3");
            System.loadLibrary("Yap++");
            System.loadLibrary("YAPDroid");
            CreateFiles.setupfiles(this, mgr);

        } catch (NameNotFoundException e) {
            Log.e(TAG, "Couldn't find package  information in PackageManager", e);
        }
        Log.i(TAG, "mgr=" + mgr);

        text = (EditText) findViewById(R.id.EditText01);
        outputText = (TextView) findViewById(R.id.OutputText);
        outputText.setText("Application " + s + "\nPress 'First' or 'All' to query...\n");
        outputText.setMovementMethod(new ScrollingMovementMethod());
        scroller = (ScrollView) findViewById(R.id.Scroller);
        if (BuildConfig.DEBUG) {
            Log.i(TAG, "window making done");
        }
        eng = new YAPEngine(null, this.getExternalFilesDir("/Yap/pl/boot.yap").getAbsolutePath());
        if (BuildConfig.DEBUG) {
            Log.i(TAG, "engine done");
        }
        if (BuildConfig.DEBUG) {
            Log.i(TAG, "onClearButtonClick called");
        }
        JavaCallback callback = new JavaCallback(outputText);
        // set the Java Callback
        if (BuildConfig.DEBUG) {
            Log.i(TAG, "before setting callback");
        }
        eng.setYAPCallback(callback);
        if (BuildConfig.DEBUG) {
            Log.i(TAG, "callback done");
        }
        // ATTENTION: This was auto-generated to implement the App Indexing API.
        // See https://g.co/AppIndexing/AndroidStudio for more information.
        client = new GoogleApiClient.Builder(this).addApi(AppIndex.API).build();
    }

    public void onClearButtonClick(View view) {
        if (BuildConfig.DEBUG) {
            Log.i(TAG, "onClearButtonClick called");
        }
        // Ensure scroll to end of text
        scroller.post(new Runnable() {
            public void run() {
                scroller.fullScroll(ScrollView.FOCUS_DOWN);
                if (running) {
                    if (q != null)
                    q.close();
                    q = null;
                }
                running = false;
                text.setText("");
            }
        });
    }

    public void onFirstButtonClick(View view) {
        if (BuildConfig.DEBUG) {
            Log.i(TAG, "onQueryButtonClick called");
        }
        // Ensure scroll to end of text
        scroller.post(new Runnable() {
            public void run() {
                scroller.fullScroll(ScrollView.FOCUS_DOWN);
                str = text.getText().toString();
                //outputText.append("?- " + str+"\n\n");
                Log.i(TAG, "onQueryAnyButtonClick " + str + "\n");
                runQuery(str, false);
                scroller.fullScroll(ScrollView.FOCUS_DOWN);
            }
        });
    }

    public void onAllButtonClick(View view) {
        if (BuildConfig.DEBUG) {
            Log.i(TAG, "onQueryButtonClick called");
        }
        // Ensure scroll to end of text
        scroller.post(new Runnable() {
            public void run() {
                str = text.getText().toString();
                scroller.fullScroll(ScrollView.FOCUS_DOWN);
                outputText.append("?- " + str + "\n\n");
                Log.i(TAG, "onAllButtonClick " + str + "\n");
                runQuery(str, true);
                scroller.fullScroll(ScrollView.FOCUS_DOWN);
            }
        });
    }

    /**
     * ATTENTION: This was auto-generated to implement the App Indexing API.
     * See https://g.co/AppIndexing/AndroidStudio for more information.
     */
    public Action getIndexApiAction() {
        Thing object = new Thing.Builder()
                .setName("YAPDroid Page") // TODO: Define a title for the content shown.
                // TODO: Make sure this auto-generated URL is correct.
                .setUrl(Uri.parse("http://[ENTER-YOUR-URL-HERE]"))
                .build();
        return new Action.Builder(Action.TYPE_VIEW)
                .setObject(object)
                .setActionStatus(Action.STATUS_TYPE_COMPLETED)
                .build();
    }

    @Override
    public void onStart() {
        super.onStart();

        // ATTENTION: This was auto-generated to implement the App Indexing API.
        // See https://g.co/AppIndexing/AndroidStudio for more information.
        client.connect();
        AppIndex.AppIndexApi.start(client, getIndexApiAction());
    }

    @Override
    public void onStop() {
        super.onStop();

        // ATTENTION: This was auto-generated to implement the App Indexing API.
        // See https://g.co/AppIndexing/AndroidStudio for more information.
        AppIndex.AppIndexApi.end(client, getIndexApiAction());
        client.disconnect();
    }
}

class JavaCallback extends YAPCallback {
    private static final String TAG = "JavaCallback";
    TextView output;

    public JavaCallback(TextView outputText) {
        super();
        output = outputText;
        Log.i(TAG, "java callback init");
    }

    public void run(String s) {
        Log.i(TAG, "java callback ");
        output.append(s);
    }

}
