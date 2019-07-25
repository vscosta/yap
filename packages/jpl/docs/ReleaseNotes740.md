# Release notes - 7.4.0

JPL 7.4 was developed for inclusion in the stable SWI Prolog release series 7.4.x
(and is included in development releases 7.5.x).

JPL 7.4 introduces one important change to JPL's public Prolog API:
the use of blobs as opaque handles for references (jrefs) to objects within the Java Virtual Machine (JVM).

## JRefs are now blobs

Prior to JPL 7.4.0, Prolog-side references to JVM objects were represented as compound terms e.g.
```prolog
@('J#00000000000443566616')
```
where the atom inside the `@/1` structure encodes the address of the object within the JVM.
JPL exploits SWI Prolog's atom garbage collection (and its associated hooks) to effectively
extend the JVM's garbage collection (GC) to treat jrefs as countable object references.

In JPL 7.4.0 we achieve the same GC cooperation using SWI Prolog's built-in support for opaque handles,
as is well established for representing (for example) streams; hence a JPL 7.4.x jref is e.g.
```prolog
<jref>(0x2193410)
```
This change will affect pre-7.4 JPL applications only if they somehow relied on the structure of jrefs,
e.g. by detecting them by unification with `@(_)`.

Note that, as with stream handles, there is no valid source text representation of a new-style jref;
the following source text (or top-level input) is not only meaningless, but is a syntax error:
```prolog
X = <jref>(0x2193410).
```
If you wish to reuse jrefs in SWI Prolog's interactive top-level interpreter, e.g.
```prolog
1 ?- jpl_new('java.util.Date', [], D).
D = @('J#00000000000443566616').

2 ?- jpl_call(@('J#00000000000443566616'), toString, [], S).
S = 'Thu Jan 19 19:56:13 GMT 2017'.
```
then you should exploit
[Reuse of top-level bindings](http://www.swi-prolog.org/pldoc/man?section=topvars),
which is also more accurate and efficient than rekeying or pasting the jref's output representation:
```prolog
1 ?- jpl_new('java.util.Date', [], D).
D = <jref>(0x2193410)

2 ?- jpl_call($D, toString, [], S).
S = 'Thu Jan 19 19:56:13 GMT 2017'.
```
## Term method orthogonality

**summary:** *all Term subclasses support all methods, reducing need for coercions*

For coding convenience, the major methods of `Term`'s concrete subclasses
(`Atom`, `Compound`, `Integer`, `Float`, `Variable` and `JRef`) are supported in JPL7 by all these classes;
this reduces the need for coercions.

method | Atom | Compound | Float | Integer | Variable | JRef
------ | ---- | -------- | ----- | ------- | -------- | ----
arg(int) | JPLException | Term | JPLException | JPLException | JPLException | JPLException
args() | Term[] {} | Term[] {...} | Term[] {} | Term[] {} | JPLException | Term[] {}
arity() | 0 | 0+ | 0 | 0 | JPLException | 0
atomType() | "text" etc. | JPLException | JPLException | JPLException | JPLException | "jref"
bigValue() | JPLException | JPLException | JPLException | BigInteger or null | JPLException | JPLException
doubleValue() | JPLException | JPLException | double | double | JPLException | JPLException
equals() | boolean | boolean | boolean | boolean | xxbooleanxx | boolean
floatValue() | JPLException | JPLException | float | float | JPLException | JPLException
hasFunctor(double, int) | false | false | boolean | false | JPLException | false
hasFunctor(String, int) | boolean | boolean | false | false | JPLException | false
hasFunctor(long, int) | boolean | boolean | false | false | JPLException | false
isAtom() | true | false | false | false | false | false
isBig() | JPLException | JPLException | JPLException | boolean | JPLException | JPLException
isBigInteger() | false | false | false | boolean | false | false
isCompound() | false | true | false | false | false | false
isFloat() | false | false | true | false | false | false
isInteger() | false | false | false | true | false | false
isJFalse() | false | boolean | false | false | false | false
isJNull() | false | boolean | false | false | false | false
isJRef() | false | false | false | false | false | true
isJTrue() | false | boolean | false | false | false | false
isJVoid() | false | boolean | false | false | false | false
isListNil() | boolean | false | false | false | false | false
isListPair() | false | boolean | false | false | false | false
isVariable() | false | false | false | false | true | false
longValue() | JPLException | JPLException | long | long | JPLException | JPLException
name() | String | String | JPLException | JPLException | String | JPLException
object() | JPLException | JPLException | JPLException | JPLException | JPLException | Object
toString() | String | String | String | String | String | String
type() | 2 | 6 | 4 | 3 | 1 | 102
typeName() | "Atom" | "Compound" | "Float" | "Integer" | "Variable" | "JRef"

The behaviours of `arg(int)` and `args()` are based on the behaviours of the corresponding SWI Prolog 7.x `arg/3` and `=../2` built-ins respectively.

The behaviours of the various `isXXX()` methods are based on SWI Prolog 7.x `==/2`.

The behaviours of the various `hasFunctor(xxx,int)` methods are based on SWI Prolog 7.x `functor/3`.
