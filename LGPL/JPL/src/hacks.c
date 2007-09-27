/*
 %T jni_SetByteArrayElement(+term, +term, +term)
 */
static foreign_t
jni_SetByteArrayElement(
    term_t	ta1,	// +Arg1
    term_t	ta2,	// +Arg2
    term_t	ta3	// +Arg3
    )
    {
      jboolean	r;	// Prolog exit/fail outcome
      jbyteArray  p1;
      int i2;
      jbyte p4;
      JNIEnv	*env;
      atom_t	a;	/*  " */
      functor_t	fn;	/* temp for conversion macros */
      term_t	a1;	/*  " */
      int	i;	/*  " */

      if	(   !jni_ensure_jvm()	)
	{




	  return FALSE;
	}
      r = 
	JNI_term_to_byte_jarray(ta1,p1)
	&& JNI_term_to_jint(ta2,i2)
	&& JNI_term_to_jbyte(ta3,p4)
	&& ( (*env)->SetByteArrayRegion(env,p1,(jsize)i2,1,&p4) , TRUE );

      return jni_check_exception(env) && r;

    }

/*
 %T jni_SetByteArrayElement(+term, +term, +term)
 */
static foreign_t
jni_SetDoubleArrayElement(
    term_t	ta1,	// +Arg1
    term_t	ta2,	// +Arg2
    term_t	ta3	// +Arg3
    )
    {
      jboolean	r;	// Prolog exit/fail outcome
      void *p1;
      jint i2;
      jdouble p4;
      JNIEnv	*env;
      atom_t	a;	/*  " */
      functor_t	fn;	/* temp for conversion macros */
      term_t	a1;	/*  " */
      int	i;	/*  " */
      int64_t		i64;

      if	(   !jni_ensure_jvm()	)
	{
	  return FALSE;
	}
      r = 
	JNI_term_to_double_jarray(ta1,p1)
	&& JNI_term_to_jint(ta2,i2)
	&& JNI_term_to_jdouble(ta3,p4)
	&& ( (*env)->SetDoubleArrayRegion(env,(jdoubleArray)p1,(jsize)i2,1,&p4) , TRUE );

      return jni_check_exception(env) && r;

    }

