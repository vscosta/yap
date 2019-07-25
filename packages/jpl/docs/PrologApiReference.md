# Prolog API - Reference

## Core API

### jpl_new(+X, +Args, -V)

Create a new Java object.

*X* can be:
* an atomic classname, e.g. `'java.util.Date'`
* a suitable type, i.e. any `class(_,_)`, `array(_)` or primitive type (e.g. `byte`) but not `null` or `void
* an atomic descriptor, e.g. `'[I'` or `'[Ljava.lang.String;'`
* a class object, i.e. an object whose type is `class([java,lang],['Class'])`

*Args* is typically a list of datums (values or jrefs) to be passed to the appropriate constructor.

If, however, *X* denotes an array type and *Args* is a non-negative integer,
then *V* is a new array of that many elements, initialised to Java's default value for the base type.

```prolog
?- jpl_new(array(byte), 4, JRef), jpl_array_to_list(JRef, Ds).
JRef = <jref>(0x12345678),
Ds = [0, 0, 0, 0].
```

If *X* denotes an array type and *Args* is a list of datums,
each of which is (independently) castable to the array element type,
then *V* is a new array of as many elements as *Args* has members,
initialised to the results of casting the respective members of *Args*.

```prolog
?- jpl_new(array(byte), [1,1,2,3,5,8], JRef), jpl_array_to_list(JRef, Ds).
JRef = <jref>(0x12345678),
Ds = [1, 1, 2, 3, 5, 8].
```

If *X* denotes a non-array object type and *Args* is a list of datums, then *V* is the result of an invocation of that type's most specifically-typed constructor to whose respective parameters the members of *Args* are assignable (an exception is thrown if no such method is found).

### jpl_call(+X, +Method, +Args, -V)

Call an instance method of a Java object, or a static method of a Java class.

*X* can be:

* a type, class object or classname (for static methods of the denoted class, or for static or instance methods of java.lang.Class);
* a class instance or array (for static or instance methods).

*Method* can be:

* an atomic method name (if this name is ambiguous, as a result of method overloading, then it will be resolved by considering the types of *Args*, as far as they can be inferred).

*Args* must be:

* a proper list (possibly empty) of ground arguments.

Finally, an attempt will be made to unify *V* with the returned result.

### jpl_set(+X, +Field, +V)

Set the *Field* of the object or class represented by *X* to value *V*.

*X* can be:

* a class object, a classname, or an (object or array) type (for static fields, or java.lang.Class fields);
* a class instance (for non-static fields);
* an array (for indexed element or subrange assignment);
* but not a string (no fields to retrieve).

*Field* can be:

* an atomic field name (overloading will be resolved dynamically, by considering the inferred type of *V*);
* a variable (field names, or array indices, are generated);
* an array index *I* (*X* must be an array object: *X[I]* is assigned *V*);
* a pair *I-J* of integers (*J* can be a variable) (*X* must be an array object, *V* must be a list of values: *X[I-J]* will be assigned *V*).

*V* must be ground, and assignable.

### jpl_get(+X, +Field, -V)

Get the value of an instance field of an object, or of a static field of a class.

*X* can be:

* a class object, a classname, or an (object or array) type (for static fields, or java.lang.Class fields);
* a class instance (for non-static fields);
* or an array (for its length pseudo field, or for indexed element retrieval).

*Field* can be

* a field name (as a text atom)
* or a pair *I-J* of integers or variables (array subranges are generated)

Immediately before `jpl_get/3` returns, an attempt will be made to unify *V* with the internally computed result.

## Java inspection

### jpl_class_to_classname(+Class, -Classname)

*Class* must be a JPL reference to a Java class object (i.e. an instance of java.lang.Class).

*Classname* is its canonical dotted name, e.g. `'java.util.Date'`.

### jpl_class_to_type(+Class, -Type)

*Class* must be a JPL reference to a Java class object (i.e. an instance of java.lang.Class).

*Type* is its JPL type, e.g. `class([java,util],['Date'])` or `array(double)`.

### jpl_classname_to_class(+Classname, -Class)

*Classname* must be a canonical dotted name (an atom) of a Java class, e.g. `'java.util.Date'`.

*Class* is a JPL reference to a corresponding Java class object (i.e. an instance of java.lang.Class).

### jpl_classname_to_type(+Classname, -Type)

*Classname* must be a canonical dotted name (an atom) of a Java class, e.g. `'java.util.Date'`.

*Type* is its JPL type, e.g. `class([java,util],['Date'])`.

### jpl_datum_to_type(+Datum, -Type)

*Datum* must be a valid JPL representation of some Java object or value e.g. `3`, `fred`, `@(false)`.

*Type* is its JPL type, e.g. `char_byte`, `class([java,lang],['String'])`, `boolean`.

### jpl_is_class(@Term)

True if *Term* is a JPL reference to a Java class object, i.e. to an instance of java.lang.Class. No further instantiation of *Term* will take place; if *Term* is not ground, this predicate fails.

### jpl_is_false(@Term)

True if *Term* is the JPL representation of the Java boolean value `false`. No further instantiation of *Term* will take place; if *Term* is not ground, this predicate fails. You could inline this as `Term == @(false)`.

### jpl_is_null(@Term)

True if *Term* is the JPL representation of the Java value `null`. No further instantiation of *Term* will take place; if *Term* is not ground, this predicate fails. You could inline this as `Term == @(null)`.

### jpl_is_object(@Term)

True if *Term* is a JPL reference to a Java object. No further instantiation of *Term* will take place; if *Term* is not ground, this predicate fails.

### jpl_is_object_type(@Term)

True if *Term* is a JPL class or array type (but not null, void, or one of the primitive types). No further instantiation of *Term* will take place; if *Term* is not ground, this predicate fails.

### jpl_is_ref(@Term)

True if *Term* is a JPL class or array type, or is `null` (i.e. the JPL type of Java's null reference) (but not `void` or one of the primitive types). No further instantiation of *Term* will take place; if *Term* is not ground, this predicate fails.

### jpl_is_true(@Term)

True if *Term* is the JPL representation of the Java boolean value `true`. No further instantiation of *Term* will take place; if *Term* is not ground, this predicate fails. You could inline this as `Term == @(true)`.

### jpl_is_type(@Term)

True if *Term* is a JPL type, e.g. `char_byte`, `float`, `array(int)`. No further instantiation of *Term* will take place; if *Term* is not ground, this predicate fails.

### jpl_is_void(@Term)

True if *Term* is the JPL representation of the (notional but convenient) Java value `void`, i.e. `@(void)`.
No further instantiation of *Term* will take place; if *Term* is not ground, this predicate fails.
You could inline this as `Term == @(void)`.

### jpl_object_to_class(+Object, -Class)

*Object* is a JPL reference to a Java object.

*Class* is a JPL reference to a Java class object (an instance of java.lang.Class) which represents *Object*'s class.

### jpl_object_to_type(+Object, -Type)

*Object* is a JPL reference to a Java object.

*Type* is its JPL type, e.g. `array(boolean)`, `class([javax,sql],['Timestamp'])`.

### jpl_primitive_type(-Type)

*Type* is one of the JPL primitive types `boolean`, `char`, `byte`, `short`, `int`, `long`, `float`, `double`.

### jpl_ref_to_type(+Ref, -Type)

*Ref* is a JPL reference to a Java object.

*Type* is the JPL type of that object, e.g. `array(boolean)`, `class([javax,sql],['Timestamp'])`.

### jpl_type_to_class(+Type, -Class)

*Type* is a JPL class (or array) type, e.g. `class([javax,sql],['Timestamp'])` or `array(boolean)`.

*Class* is a JPL reference to a Java class object (an instance of java.lang.Class) which corresponds to *Type*.

### jpl_type_to_classname(+Type, -Classname)

*Type* is a JPL class (or array) type, e.g. `class([javax,sql],['Timestamp'])` or `array(boolean)`.

*Classname* is its canonical dotted name (an atom), e.g. `'javax.sql.Timestamp'` or `'[Z'`.

## Utilities

### jpl_array_to_length(+JRef, -Length)

*JRef* should be a JPL reference to a Java array.

*Length* is its length (an integer).

### jpl_array_to_list(+JRef, -Datums)

*JRef* should be a JPL reference to a Java array (of any base type).

*Datums* is a list of JPL references to, or values of, its respective elements.

### jpl_datums_to_array(+Datums, -JRef)

*Datums* should be a list of JPL references or values.

*JRef* is a JPL reference to a newly created Java array of corresponding objects or values.
The base type of this array is the most specific Java type of which each member of Datums
is (directly or indirectly) an instance.
If there is no such type, this predicate fails.
Values of Java primitive types are not automatically "boxed".
Lists which are mixtures of numbers, booleans and object references cannot be converted to Java arrays with this predicate.

### jpl_enumeration_element(+Enumeration, -Element) is nondet

*Enumeration* should be a JPL reference to a Java object whose class implements the java.util.Enumeration interface.

*Element* is an element of *Enumeration*.
This predicate can generate each element of an enumeration.

### jpl_enumeration_to_list(+Enumeration, -Elements)

*Enumeration* is a JPL reference to a Java object whose class implements the java.util.Enumeration interface.

*Elements* is a list of JPL references to successive elements of Enumeration.

### jpl_hashtable_pair(+Hashtable, -KeyValuePair)

*Hashtable* should be a JPL reference to a Java hashtable object (an instance of java.util.Hashtable).

*KeyValuePair* is a `-/2` compound term whose first arg is a key (text atom or JPL reference) from *Hashtable*, and whose second arg is its corresponding value (text atom or JPL reference), e.g. `fred-<jref>(0x1eb0000)`.

### jpl_iterator_element(+Iterator, -Element) is nondet

*Iterator* should be a JPL reference to a Java object whose class implements the java.util.Iterator interface.

*Element* is a JPL reference to one of its elements. This predicate can generate all elements.

### jpl_map_element(+Map, -KeyValuePair)

*Map* should be a JPL reference to a Java object whose class implements the java.util.Map interface.

*KeyValuePair* is a `-/2` compound term whose first arg is a key (text atom or JPL reference) from *Map*, and whose second arg is the corresponding value (text atom or JPL reference), e.g. `-(fred,<jref>(0x1eb0000))`, or `fred-<jref>(0x1eb0000)` using conventional operator definitions.

### jpl_set_element(+Set, -Element) is nondet

*Set* should be a JPL reference to a Java object whose class implements the java.util.Set interface.

*Element* is a JPL reference to an object (or null) within *Set*. This predicate can generate all elements of Set.

## Miscellaneous

### jpl_c_lib_version(-Version)

unifies *Version* to an atom (e.g. `'7.4.0-alpha'`) whose name is the version identifier of the 'C' library which JPL is using.

### jpl_c_lib_version(-Major, -Minor, -Patch, -Status)

unifies *Major*, *Minor*, *Patch* and *Status* to the corresponding components (e.g. `7`, `4`, `0` and `alpha`) of the version identifier of the 'C' library which JPL is using.
