OBJECTS=cplint_yap.o cplint_Prob.o
.PHONY : clean
#	-lutil -lst -lepd libutil.a  /usr/local/bin/yap.dll \

cplint: $(OBJECTS)
	gcc -mno-cygwin -shared -o cplint.dll $(OBJECTS) "C:\Software\BDD\cudd-2.4.1 mingw\cudd-2.4.1\cudd\libcudd.a" \
"C:\Software\BDD\cudd-2.4.1 mingw\cudd-2.4.1\mtr\libmtr.a" "C:\Software\BDD\cudd-2.4.1 mingw\cudd-2.4.1\st\libst.a" \
"C:\Software\BDD\cudd-2.4.1 mingw\cudd-2.4.1\util\libutil.a"  "C:\Software\BDD\cudd-2.4.1 mingw\cudd-2.4.1\epd\libepd.a" \
 "C:\Software\BDD\glu-2.1\libglu.a" "C:\Yap\bin\yap.dll" \
 	"C:\zipped\Glib windows\glib-2.12.11\bin\libglib-2.0-0.dll"

cplint_yap.o: cplint_yap.c cplint.h
	gcc -mno-cygwin  -I/usr/local \
 	-I/usr/include/glib-1.2 -I/lib/glib/include -I"c:/Yap/include" \
 	-I"C:\Software\BDD\Somenzi\glu-2.1\include" \
 	-I"C:\Software\BDD\cudd-2.4.1\cudd-2.4.1\include" \
	-c -shared cplint_yap.c
	
cplint_Prob.o: cplint_Prob.c cplint.h
	gcc -mno-cygwin -I/usr/local \
 	-I/usr/include/glib-1.2 -I/lib/glib/include  -I"c:/Yap/include" \
 	-I/cygdrive/c/Software/BDD/Somenzi/glu-2.1/include \
 	-I"C:\Software\BDD\cudd-2.4.1\cudd-2.4.1\include" \
	-c -shared cplint_Prob.c

clean:
	rm $(OBJECTS) cpint.so
