/*  Part of JPL -- SWI-Prolog/Java interface

    Author:	   Paul Singleton, Fred Dushin and Jan Wielemaker
    E-mail:	   paul@jbgb.com
    WWW:	   http://www.swi-prolog.org
    Copyright (C): 1985-2004, Paul Singleton

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

install_t install(void);
JNIEXPORT int JNICALL Java_jpl_fli_Prolog_action_1abort( JNIEnv *env, jclass jProlog);
JNIEXPORT jstring JNICALL Java_jpl_fli_Prolog_atom_1chars( JNIEnv     *env, jclass	jProlog, jobject jatom);

JNIEXPORT int JNICALL
 Java_jpl_fli_Prolog_attach_1engine(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		 jengine
    );

JNIEXPORT jobject JNICALL
Java_jpl_fli_Prolog_attach_1pool_1engine(
    JNIEnv	*env,
    jclass	 jProlog
    );

JNIEXPORT void JNICALL
 Java_jpl_fli_Prolog_close_1query(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jqid
    );

JNIEXPORT jint JNICALL													/* returns -1, 0 or 1 (or -2 for error) */
 Java_jpl_fli_Prolog_compare(
    JNIEnv	*env,
    jclass	jProlog,
	jobject		jterm1,
	jobject		jterm2
    );

JNIEXPORT jboolean JNICALL
Java_jpl_fli_Prolog_cons_1functor_1v(
    JNIEnv	*env,
    jclass	jProlog,
	jobject		jterm,
	jobject		jfunctor,
	jobject		jterm0
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_copy_1term_1ref(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jfrom
    );

JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_current_1engine_1is_1pool(
    JNIEnv     *env,
	jclass		 jProlog
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_current_1engine(
    JNIEnv     *env,
	jclass		jProlog
    );

JNIEXPORT void JNICALL
 Java_jpl_fli_Prolog_discard_1foreign_1frame(
	JNIEnv	   *env,
	jclass		jProlog,
	jobject		jfid
	);

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_exception(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jqid
    );

JNIEXPORT jobject JNICALL
Java_jpl_fli_Prolog_get_1actual_1init_1args(
    JNIEnv     *env,
    jclass	jProlog
    );

JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1arg(
    JNIEnv     *env,
    jclass	jProlog,
	jint		jindex,
	jobject		jterm,
	jobject		jarg
    );

JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1atom_1chars(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jterm,
	jobject		jstring_holder
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_get_1c_1lib_1version(
    JNIEnv     *env,
	jclass		jProlog
    );

JNIEXPORT jobject JNICALL
Java_jpl_fli_Prolog_get_1default_1init_1args(
    JNIEnv     *env,
    jclass	jProlog
    );

JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1float(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jterm,
	jobject		jdouble_holder
    );

JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1integer(
    JNIEnv     *env,
    jclass	jProlog,
    jobject	jterm,
	jobject		jint64_holder
    );

JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1name_1arity(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jterm,
	jobject		jname_holder,								/* we trust this is a StringHolder */
	jobject		jarity_holder								/* we trust this is an IntHolder */
    );

JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_get_1string_1chars(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jterm,
	jobject		jstring_holder
    );

JNIEXPORT void JNICALL
Java_jpl_fli_Prolog_halt(
    JNIEnv     *env,
    jclass	jProlog,
    jint	jstatus
    );

JNIEXPORT jboolean JNICALL
Java_jpl_fli_Prolog_initialise(
    JNIEnv	*env,
    jclass	 jProlog
    );

JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_is_1tag(
    JNIEnv     *env,
	jclass		 jProlog,
	jstring     tag
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1atom(
    JNIEnv     *env,
    jclass	jProlog,
	jstring		jname
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1functor(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jatom,		/* read-only */
	jint		jarity
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1module(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jatom
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1term_1ref(
    JNIEnv     *env,
	jclass		jProlog
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_new_1term_1refs(
    JNIEnv     *env,
    jclass	jProlog,
	jint		jn
    );

JNIEXPORT jboolean JNICALL
 Java_jpl_fli_Prolog_next_1solution(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jqid		/* read */
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_object_1to_1tag(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jobj
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_open_1foreign_1frame(
	JNIEnv	   *env,
	jclass		jProlog
	);

JNIEXPORT jobject JNICALL
Java_jpl_fli_Prolog_open_1query(
    JNIEnv     *env,
    jclass	jProlog,
    jobject	jmodule,	/* read */
    jint	jflags,		/* read */
    jobject	jpredicate,	/* read */
    jobject	jterm0		/* read */
    );

JNIEXPORT int JNICALL
Java_jpl_fli_Prolog_pool_1engine_1id(
    JNIEnv	*env,
    jclass	 jProlog,
    jobject	 jengine
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_predicate(
    JNIEnv     *env,
    jclass	jProlog,
	jstring		jname,	/* ought not be null */
	jint		jarity,	/* oughta be >= 0 */
	jstring		jmodule	/* may be null */
    );

JNIEXPORT jboolean JNICALL
Java_jpl_fli_Prolog_put_1float(JNIEnv *env,
			       jclass jProlog,
			       jobject jterm,
			       jdouble jf);

JNIEXPORT jboolean JNICALL
Java_jpl_fli_Prolog_put_1integer(JNIEnv *env,
				 jclass	jProlog,
				 jobject jterm,
				 jlong ji);

JNIEXPORT void JNICALL
 Java_jpl_fli_Prolog_put_1jref(
	JNIEnv     *env,
	jclass      jProlog,
	jobject     jterm,
	jobject     jref
	);

JNIEXPORT void JNICALL	/* maybe oughta return jboolean (false iff given object is null) */
 Java_jpl_fli_Prolog_put_1term(
    JNIEnv     *env,
    jclass	jProlog,
    jobject	jterm1,
    jobject	jterm2
    );

JNIEXPORT void JNICALL	/* maybe oughta return jboolean (false iff given object is null) */
 Java_jpl_fli_Prolog_put_1variable(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jterm
    );

JNIEXPORT int JNICALL
Java_jpl_fli_Prolog_release_1pool_1engine(
    JNIEnv	*env,
    jclass	 jProlog
    );

JNIEXPORT jboolean JNICALL
Java_jpl_fli_Prolog_set_1default_1init_1args(
    JNIEnv     *env,
    jclass	jProlog,
    jobject	jargs	    /* oughta be proper array, perhaps zero-length */
    );

JNIEXPORT jobject JNICALL
 Java_jpl_fli_Prolog_tag_1to_1object(
	JNIEnv     *env,
	jclass      jProlog,
	jstring     tag
	);

JNIEXPORT jint JNICALL
 Java_jpl_fli_Prolog_term_1type(
    JNIEnv     *env,
    jclass	jProlog,
	jobject		jterm
    );

JNIEXPORT jint JNICALL
Java_jpl_fli_Prolog_thread_1self(
    JNIEnv	*env,
    jclass	 jProlog
    );

JNIEXPORT void JNICALL
 Java_jpl_fli_Prolog_unregister_1atom(
    JNIEnv     *env,
	jclass		jProlog,
	jobject		jatom
    );

