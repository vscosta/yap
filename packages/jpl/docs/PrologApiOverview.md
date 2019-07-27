# Prolog API - Overview

The Prolog API comprises Prolog library predicates which support:
* creating instances (objects) of Java classes (built-in and user-defined);
* calling methods of Java objects (and static methods of classes), perhaps returning values or object references; and
* getting and setting the values of fields of Java objects and classes.


## Introduction

JPL's Prolog API is an interface which allows SWI Prolog 7.x programs to
dynamically create and manipulate Java objects.

Here are some significant features of the interface and its
implementation:

* it is completely dynamic: no precompilation is required to manipulate public Java classes which can be found at run time, and methods or fields of objects which can be instantiated from them 
* it is interoperable with the JPL 7.4 Java API (which evolved from Fred Dushin's JPL 1.0.1)
* it requires SWI Prolog 7.4+ and a recent Java SE Runtime Environment 
* it exploits the *Invocation API* of the [Java Native Interface](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/) (both are mandatory features of any compliant JVM) 
* it is implemented as a single SWI Prolog library module (`jpl.pl`), a compiled ANSI C foreign library (`jpl.dll/.so/.dylib` for Windows/Linux/MacOS), and a Java class library (`jpl.jar`)
* wherever feasible, Java data values and object references are represented within Prolog canonically and without loss of information (minor exceptions: Java *float* and *double* values are both converted to Prolog *float* values; Java *byte*, *char*, *short*, *int* and *long* values are all converted to Prolog *integer* values; the type distinctions which are lost are normally of no significance)
* references within Prolog to Java objects:
 * are opaque handles (details follow)
 * are canonical - two references are equal by `==/2` if-and-only-if they refer to the same object within the JVM
 * cooperate with SWI Prolog's garbage collection: when an object reference is garbage-collected in Prolog, the JVM garbage collector is informed, so there is sound and complete overall garbage collection of Java objects within the combined Prolog+Java system
* Java class methods can be called by name: JPL invisibly fetches (and caches) essential details of method invocation, exploiting *Java Reflection* facilities
* the Prolog API is similar to that of XPCE: the four main interface calls are `jpl_new/3`, `jpl_call/4`, `jpl_set/3` and `jpl_get/3` (there is no `jpl_free`, since Java's garbage collection is extended transparently into Prolog)
* `jpl_call/4` resolves overloaded methods automatically and dynamically, inferring the types of the call's actual parameters, and identifying the most specific of the applicable method implementations (similarly, *jpl\_new* resolves overloaded constructors)
* Prolog code which uses the API calls is responsible for passing suitably-typed values and references, since the JNI doesn't perform complete dynamic type-checking, and nor currently does JPL (although the *overloaded method resolution* mechanism could probably be adapted to do this)
* Prolog code can reason about the types of Java data values, object references, fields and methods: JPL supports a canonical representation of all Java types as structured terms (e.g. `array(array(byte))`) and also as JVM signatures (text atoms) in *descriptor* and *classname* syntax (details follow)
* the Prolog and Java APIs of JPL are largely independent; the Prolog API concentrates on representing all Java data values and objects within Prolog, and supporting manipulation of classes and objects;the Java API concentrates on representing any Prolog term within Java, and supporting the calling of goals within Prolog and the retrieving of results back into Java 
* when called from Prolog, *void* methods return an invented `@(void)` value (which is distinct from all other JPL values and references); this simplifies and regularises the API (all methods return a typed value)
* JPL uses `@/1` to construct representations of certain Java values; if `@/1` is defined as a prefix operator (as used by XPCE), then you can write `@true`, `@false`, `@null` or `@void` in your source code; otherwise (and for portability, and recommended) you should write `@(true)`, `@(false)`, `@(null)` or `@(void)`.

## JPL types

All Java values and object references which are passed between Prolog
engines and Java VMs via JPL's Prolog API are seen as instances of types
within this simplified JPL type system:

* a ***datum*** (this jargon is introduced, out of necessity, to refer to the union of *values* and *references*)
  * is a ***value*** (values are copied between Prolog and the JVM)
    * is a ***boolean***
    * or a ***char***
    * or a ***long***, ***int***, ***short*** or ***byte***
    * or a ***double*** or ***float***
    * or a ***string*** (an instance of *java.lang.String*)
    * or a ***void*** (an artificial value returned by calls to Java void methods)
  * or a ***reference***
    * is ***null***
    * or an ***object*** (held within the JVM, and represented in Prolog by a canonical reference)
      * is an ***array***
      * or a ***class instance*** (other than of *java.lang.String*)

## Java values and references

Instances of JPL types are represented within Prolog as follows:

-   ***boolean*** has two values, represented by `@(true)` and `@(false)`
-   ***char*** values are represented by corresponding Prolog *integers* in the range **0..65,535**
-   ***byte*** values are represented by corresponding Prolog *integers* in the range **-128..127**
-   ***short*** values are represented by corresponding Prolog *integers* in the range **-32,768..32,767**
-   ***int*** values are represented by corresponding Prolog *integers* in the range **-2147483648..2147483647**
-   ***long*** values are represented as Prolog *integers* in the range     **9,223,372,036,854,775,808..9,223,372,036,854,775,807**
-   ***double*** and ***float*** values are represented as Prolog floats (which are equivalent to Java doubles) (there may be minor rounding, normalisation or loss-of-precision issues when a Java float is     widened to a Prolog float then narrowed back again)
-   ***string*** values (immutable instances of *java.lang.String*) are represented as Prolog *text atoms* (in UTF-8 encoding)
-   ***null*** has only one value, represented as `@(null)`
-   ***void*** has only one value, represented as `@(void)`
-   ***array*** and ***class instance*** references are represented (since 7.4) as *[blob](http://www.swi-prolog.org/pldoc/man?predicate=blob/2)s* of type `jref`, portrayed e.g. `<jref>(0x12345678)` but (like stream handles) with no source syntax acceptable to `read/1`.

## Java types: *structured* notation 

The Prolog API allows Prolog applications to inspect, manipulate, and reason about the types of Java values, references, methods etc., and this section describes how these types themselves are represented.
Predicates which pass these type representations include (the clue is in the name):

 ```prolog
 jpl_class_to_type/2
 jpl_classname_to_type/2
 jpl_datum_to_type/2
 jpl_is_object_type/1
 jpl_is_type/1
 jpl_object_to_type/2
 jpl_primitive_type/1
 jpl_ref_to_type/2
 jpl_type_to_class/2
 jpl_type_to_classname/2
 ```

The pseudo-type ***void*** is represented by this atom:

```prolog
void
```

The pseudo-type ***null*** is represented by this atom:

```prolog
null
```

The primitive types are represented by these atoms:

```prolog
boolean
char
byte
short
int
long
float
double
```

***class*** types are represented as `class(package_parts,classname_parts)` e.g.

```prolog
class([java,util],['Date'])
```

***array*** types are represented as `array(type)` e.g.

```prolog
array(boolean)
array(class([java,lang],['String'])
```

This *structured* notation for Java types is a *term-encoding*, designed to be convenient for composition and decomposition by unification.

## Java types: *descriptor* notation 

The *descriptor* notation for Java types is one of two textual notations employed by the JVM and the Java class libraries; JPL (necessarily) supports both (and supports conversion between all three representations).

Examples of descriptor notation:

-   `'Z'` denotes ***boolean***
-   `'B'` denotes ***byte***
-   `'C'` denotes ***char***
-   `'S'` denotes ***short***
-   `'I'` denotes ***int***
-   `'J'` denotes ***long***
-   `'F'` denotes ***float***
-   `'D'` denotes ***double***
-   `'[type'` denotes an ***array*** of *type*
-   `'Ljava/util/Date;'` denotes the Java class `java.util.Date`
-   `'(argument_types)return_type'` denotes the type of a method

## Java types: *classname* notation 

The *classname* notation for Java types is the other textual notation employed by the JVM and the Java class libraries. It is a (seemingly unnecessary) variation on the *descriptor* notation, used by a few JNI
routines. It has the slight advantage that, in the case of simple class types only, it resembles the Java source text notation for classes. This representation is supported only because certain JNI functions use it; it is used within JPL's implementation of `jpl_call/4` etc. You may encounter this notation when tracing JPL activity, but otherwise you need not know about it.

Examples of classname notation:

-   `'java.util.Vector'` denotes the Java class ***java.util.Vector***
-   `'[B'` denotes an ***array*** of ***boolean***
-   `'[Ljava.lang.String;'` denotes an ***array*** of ***string***

## Creating Java objects

To create an instance of a Java class from within Prolog, call `jpl_new(+Class, +Params, -JRef)` with a classname, a list of actual parameters for the constructor, and a variable to be bound to the new reference, e.g.

```prolog
jpl_new('javax.swing.JFrame', ['frame with dialog'], JRef)
```

which binds `JRef` to a new object reference, e.g.`<jref>(0x12345678)`.

NB for convenience, this predicate is overloaded: `Class` can also be a class type in *structured* notation, e.g. `array(boolean)`.

## Calling Java methods

The object reference generated by the `jpl_new/3` call (above) can be passed to other JPL API predicates such as:

```prolog
jpl_call(+JRef, +Method, +Params, -Result)
```

e.g.

```prolog
jpl_call(JRef, setVisible, [@(true)], _)
```

which calls the `setVisible()` method of the object to which `JRef` refers, effectively passing it the Java value *true.*

(This call should display the new **JFrame** in the top left corner of the desktop.)

Note the anonymous variable passed as the fourth argument to  `jpl_call/4`. A variable in this position receives the result of the method call: either a value or a reference.

Since `SetVisible()` is a void method, the call returns the (artificial) reference `@(void)`, which can be ignored.

Some may prefer to code this call thus:

```prolog
jpl_call(F, setVisible, [@(true)], @(void))
```

which documents the programmer's understanding that this is a *void* method, and fails if it isn't.

If the `JRef` argument represents a class, then the named static method of that class is called.

## Fetching Java field values

The `jpl_get/3` API predicate has the following interface:

```prolog
jpl_get(+Class_or_Object, +Field, -Datum)
```

and can retrieve the value of an instance field e.g.

```prolog
jpl_new('java.util.GregorianCalendar', [], JRef),
jpl_get(JRef, time, Ms)
```

or of a static field, e.g.

```prolog
jpl_get('java.awt.Color', pink, Pink)
```

which binds the Prolog variable `Pink` to a reference to the predefined **java.awt.Color** "constant" which is held in the static final **.pink** field of the **java.awt.Color** class.

## Setting Java field values

Object and class fields can be set (i.e. have values or references assigned to them) by the `jpl_set/3` API procedure, which has the following interface:

```prolog
jpl_set(+Class_or_Object, +Field, +Datum)
```

where **Datum** must be a value or reference of a type suitable for assignment to the named field of the class or object.

## A slightly longer example

This code fragment

```prolog
findall(
    Ar,
    (   current_prolog_flag(N, V),
        term_to_atom(V, Va),
        jpl_new('[Ljava.lang.String;', [N,Va], Ar)
    ),
    Ars
),
jpl_new('[[Ljava.lang.String;', Ars, Ac),
jpl_datums_to_array([name,value], Ah),
jpl_new('javax.swing.JFrame', ['current_prolog_flag'], F),
jpl_call(F, getContentPane, [], CP),
jpl_new('javax.swing.JTable', [Ac,Ah], T),
jpl_new('javax.swing.JScrollPane', [T], SP),
jpl_call(CP, add, [SP,'Center'], _),
jpl_call(F, setSize, [600,400], _),
jpl_call(F, setVisible, [@(true)], _).
```

builds an array of arrays of strings containing the names and values of the current SWI-Prolog "flags", and displays it in a JTable within a ScrollPane within a JFrame:

![screendump of current prolog flags in a JTable within a ScrollPane within a JFrame](images/screendump.jpg){width="524" height="269"}

In addition to JPL API calls, this example calls `jpl_datums_to_array/2`, a utility which converts any list of valid representations of Java values (or objects) into a new Java array, whose
base type is the most specialised type of which all list members are instances, and which is defined thus:

```prolog
jpl_datums_to_array(Ds, A) :-
    ground(Ds),
    jpl_datums_to_most_specific_common_ancestor_type(Ds, T),
    jpl_new(array(T), Ds, A).
```

Having found the "most specific common ancestor type", a new array of this type is created, whose elements are initialised to the successive members of the list of datums.

This illustrates another mode of operation of `jpl_new/3`:

```prolog
jpl_new(+ArrayType, +InitialValues, -ArrayRef)
```

See [Prolog API - Reference](https://jpl7.org/PrologApiReference) for fuller details of the API procedures.

Don't overlook the possibility and advantages of writing custom Java classes to serve your Prolog applications: this interface is not designed to make Java programming redundant.

## Exceptions thrown by Java

Uncaught exceptions thrown by the JVM while handling a Prolog API call are mapped onto `error(_,_)` structures, e.g.

```prolog
?- catch(jpl_new('java.util.Date',[yesterday],_), E, true).
E = error(java_exception((0x1026D40)), 'java.lang.IllegalArgumentException').
```

because, as the exception suggests, **yesterday** is not a valid constructor argument.

Java exceptions are always returned as Prolog exceptions with this structure:

```prolog
error(java_exception(reference_to_exception_object), exception_classname)
```

## Gotchas

Here are a few things to watch out for.

### Naming static nested classes

Although, in Java source code, static nested classes are named with the *dot* notation, e.g.

```java
java.awt.geom.Path2D.Float
```

the JVM's Java Native Interface (JNI), which JPL uses, requires a special notation, hence

```prolog
jpl_new('java.awt.geom.Path2D$Float', [], P).
```

which you may have noticed in the name of the corresponding class file:

```
java/awt/geom/Path2D$Float.class
```

Unfortunately it is far from trivial to support the Java naming in JPL.  Thanks go to Timo Baumann and Sebastian Godelet for their input on this issue.

### Instantiating a non-static member class

If you need to create an instance of a non-static member class, you must explicitly pass (a reference to) an instance of its parent class as an additional (first) argument of any of its constructors. Java source syntax hides this, but reflection shows that every constructor (implicit or explicit) has this extra argument.

```java
public class Parent {
    public class Child {
    }
}
```

```prolog
jpl_new('Parent', [], P),
jpl_new('Parent$Child', [P], C).
```

### Enums are static nested classes

Given

```java
public class Foo {
    public enum Bar {
        UP, DOWN
    }
    public static String legend(Bar x) {
        switch(x) {
            case UP:
                return "upwards";
            case DOWN:
                return "downwards";
            default:
                return "?";
        }
    }
}
```

we can do

```prolog
1 ?- jpl_get('Foo$Bar', 'UP', X).
X = @('J#00000000000056425292').
```

or

```prolog
1 ?- jpl_get('Foo$Bar', 'UP', X), jpl_call('Foo', legend, [X], S).
X = @('J#00000000000056425292'),
S = upwards.
```

where X is bound to the nominated enum constant (actually an object reference),
then passed as an arg to legend/1

### Calling parameterless methods

You must pass an empty parameter list when calling Java methods which take no parameters, e.g.

```prolog
jpl_call('java.lang.System', gc, [], _)
```

There is (deliberately) no `jpl_call/3` convenience predicate which defaults parameters to `[]` (see below).

### Calling void methods

You must accept an `@(void)` result when calling void Java methods, e.g. either

```prolog
jpl_call('java.lang.System', gc, [], @(void))
```

which explicitly matches the expected result, or

```prolog
jpl_call('java.lang.System', gc, [], _)
```

which uses an anonymous variable to ignore the result.

There is (deliberately) no `jpl_call/3` convenience predicate which conceals the return value of `void` methods (see above).

### Exiting the JVM

Note that

```prolog
jpl_call('java.lang.System', exit, [0], _)
```

will terminate the SWI-Prolog process (into which the JVM library has been dynamically loaded)
*without* calling any `at_halt/1` or `PL_at_halt()` hooks, or performing system cleanup actions.
Beware also of calling Java classes which might call `java.lang.System#exit()`.

## To do

Here are a few longer-term (and tricky) aims:

-   support non-virtual method calls (i.e. explicitly call a method of some ancestor class despite there being an overriding method (i.e. of the same name etc.) in a "nearer" class). I believe this is a fairly arcane Java feature, but it is needed for completeness; I want to accommodate it without complicating the syntax of regular method calls.
-   map the JVM's `vprintf()` messages onto something in SWI-Prolog (the user\_error stream?)
-   catch the JVM's *abort* and *exit* events, and handle them appropriately (e.g. stop a Java abort from killing the SWI-Prolog process)
-   propagate SWI-Prolog's ABORT action into the JVM as appropriate, e.g. to interrupt a pending JPL call
-   reduce the (extravagant) overheads of each JPL call (without compromising functionality or safety)
