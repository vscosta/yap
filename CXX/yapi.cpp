
#define YAP_CPP_INTERFACE 1

#include "yapi.hh"

YAPAtomTerm::YAPAtomTerm(char *s) { // build string 
	BACKUP_H();

	CACHE_REGS
	seq_tv_t inp, out;
	inp.val.c = s;
	inp.type = YAP_STRING_CHARS;
	out.type = YAP_STRING_ATOM;
	if (Yap_CVT_Text(&inp, &out PASS_REGS))
		t = MkAtomTerm(out.val.a);
	else t = 0L;
	RECOVER_H();
}


YAPAtomTerm::YAPAtomTerm(char *s, size_t len) { // build string 
	BACKUP_H();

	CACHE_REGS
	seq_tv_t inp, out;
	inp.val.c = s;
	inp.type = YAP_STRING_CHARS;
	out.type = YAP_STRING_ATOM|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
	out.sz = len;
	out.max = len;
	if (Yap_CVT_Text(&inp, &out PASS_REGS))
		t = MkAtomTerm(out.val.a);
	else t = 0L;
	RECOVER_H();
}

YAPAtomTerm::YAPAtomTerm(wchar_t *s): YAPTerm() { // build string 
	BACKUP_H();

	CACHE_REGS
	seq_tv_t inp, out;
	inp.val.w = s;
	inp.type = YAP_STRING_WCHARS;
	out.type = YAP_STRING_ATOM;
	if (Yap_CVT_Text(&inp, &out PASS_REGS))
		t = MkAtomTerm(out.val.a);
	else t = 0L;
	RECOVER_H();
}


YAPAtomTerm::YAPAtomTerm(wchar_t *s, size_t len) : YAPTerm() { // build string 
	BACKUP_H();

	CACHE_REGS
	seq_tv_t inp, out;
	inp.val.w = s;
	inp.type = YAP_STRING_WCHARS;
	out.type = YAP_STRING_ATOM|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
	out.sz = len;
	out.max = len;
	if (Yap_CVT_Text(&inp, &out PASS_REGS))
		t = MkAtomTerm(out.val.a);
	else t = 0L;
	RECOVER_H();
}


YAPStringTerm::YAPStringTerm(char *s) { // build string 
	BACKUP_H();

	CACHE_REGS
	seq_tv_t inp, out;
	inp.val.c = s;
	inp.type = YAP_STRING_CHARS;
	out.type = YAP_STRING_STRING;
	if (Yap_CVT_Text(&inp, &out PASS_REGS))
		t = out.val.t;
	else t = 0L;
	RECOVER_H();
}


YAPStringTerm::YAPStringTerm(char *s, size_t len) { // build string 
	BACKUP_H();

	CACHE_REGS
	seq_tv_t inp, out;
	inp.val.c = s;
	inp.type = YAP_STRING_CHARS;
	out.type = YAP_STRING_STRING|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
	out.sz = len;
	out.max = len;
	if (Yap_CVT_Text(&inp, &out PASS_REGS))
		t = out.val.t;
	else t = 0L;
	RECOVER_H();
}

YAPStringTerm::YAPStringTerm(wchar_t *s): YAPTerm() { // build string 
	BACKUP_H();

	CACHE_REGS
	seq_tv_t inp, out;
	inp.val.w = s;
	inp.type = YAP_STRING_WCHARS;
	out.type = YAP_STRING_STRING;
	if (Yap_CVT_Text(&inp, &out PASS_REGS))
		t = out.val.t;
	else t = 0L;
	RECOVER_H();
}


YAPStringTerm::YAPStringTerm(wchar_t *s, size_t len) : YAPTerm() { // build string 
	BACKUP_H();

	CACHE_REGS
	seq_tv_t inp, out;
	inp.val.w = s;
	inp.type = YAP_STRING_WCHARS;
	out.type = YAP_STRING_STRING|YAP_STRING_NCHARS|YAP_STRING_TRUNC;
	out.sz = len;
	out.max = len;
	if (Yap_CVT_Text(&inp, &out PASS_REGS))
		t = out.val.t;
	else t = 0L;
	RECOVER_H();
}


YAPApplTerm::YAPApplTerm(YAPFunctor f, YAPTerm ts[]) : YAPTerm() {
	UInt arity = ArityOfFunctor(f.f);
	t = Yap_MkApplTerm( f.f, arity, (Term *)ts);
}

YAPApplTerm::YAPApplTerm(YAPFunctor f) : YAPTerm() {
	UInt arity = ArityOfFunctor(f.f);
	t = Yap_MkNewApplTerm( f.f, arity);
}

YAPTerm  YAPApplTerm::getArg(unsigned int arg) {
	return YAPTerm( ArgOfTerm(arg, t) );
}

YAPFunctor  YAPApplTerm::getFunctor() {
	return YAPFunctor( FunctorOfTerm(t) );
}

YAPPairTerm::YAPPairTerm(YAPTerm th, YAPTerm tl) : YAPTerm() {
	CACHE_REGS
	t = MkPairTerm( th.t, tl.t);
}

YAPPairTerm::YAPPairTerm() : YAPTerm() {
	t = Yap_MkNewPairTerm( );
}

YAP_tag_t  YAPTerm::tag() {
	if (IsVarTerm(t)) {
		CELL *pt = VarOfTerm(t);
		if (IsUnboundVar(pt)) {
			CACHE_REGS
			if (IsAttVar(pt))
				return YAP_TAG_ATT;
			return YAP_TAG_UNBOUND;
		}
		return YAP_TAG_REF;
	}
	if (IsPairTerm(t))
		return YAP_TAG_PAIR;
	if (IsAtomOrIntTerm(t)) {
		if (IsAtomTerm(t))
			return YAP_TAG_ATOM;
		return YAP_TAG_INT;
	} else {
		Functor f = FunctorOfTerm(t);

		if (IsExtensionFunctor(f)) {
			if (f == FunctorDBRef) {
				return YAP_TAG_DBREF;
			}
			if (f == FunctorLongInt) {
				return YAP_TAG_LONG_INT;
			}
			if (f == FunctorBigInt) {
				big_blob_type bt = (big_blob_type)RepAppl(t)[1];
				switch (bt) {
				case BIG_INT:
					return YAP_TAG_BIG_INT;
				case BIG_RATIONAL:
					return YAP_TAG_RATIONAL;
				default:
					return YAP_TAG_OPAQUE;
				}
			}
		}
		return YAP_TAG_APPL;
	}
}

YAPTerm  YAPTerm::deepCopy() {
	Term tn;
	BACKUP_MACHINE_REGS();

	tn = Yap_CopyTerm(t);

	RECOVER_MACHINE_REGS();
	return new YAPTerm( tn );
}

bool YAPTerm::exactlyEqual(YAPTerm t1) {
	int out;
	BACKUP_MACHINE_REGS();

	out = Yap_eq(Deref(t), Deref(t1.t));

	RECOVER_MACHINE_REGS();
	return out;
}

bool YAPTerm::unify(YAPTerm t1) {
	int out;
	BACKUP_MACHINE_REGS();

	out = Yap_unify(Deref(t), Deref(t1.t));

	RECOVER_MACHINE_REGS();
	return out;
}

bool YAPTerm::unifiable(YAPTerm t1) {
	int out;
	BACKUP_MACHINE_REGS();

	out = Yap_Unifiable(Deref(t), Deref(t1.t));

	RECOVER_MACHINE_REGS();
	return out;
}

bool YAPTerm::variant(YAPTerm t1) {
	int out;
	BACKUP_MACHINE_REGS();

	out = Yap_Variant(Deref(t), Deref(t1.t));

	RECOVER_MACHINE_REGS();
	return out;
}

intptr_t YAPTerm::hash(size_t sz, size_t depth, bool variant) {
	Int out;

	BACKUP_MACHINE_REGS();

	out = Yap_TermHash(t, sz, depth, variant);

	RECOVER_MACHINE_REGS();
	return out;
}


char *YAPAtom::name(void) {
	if (IsWideAtom(a)) {
		// return an UTF-8 version
				size_t sz = 512;
		wchar_t * ptr =  a->WStrOfAE;
		int ch = -1;
		char *s = new char[sz], *op = s;
		while (ch) {
			ch = *ptr++;
			utf8_put_char( op, ch );
		}
		sz = strlen(s)+1;
		char *os = new char[sz];
		memcpy(os, s, sz);
		delete s;
		return os;
	} else if (IsBlob(a)) {
		PL_blob_t *type = RepBlobProp(a->PropsOfAE)->blob_t;
		size_t sz = 512;

		if (type->write) {
			char *s = new char[sz];
			IOSTREAM *stream = Sopenmem(&s, &sz, "w");
			stream->encoding = ENC_UTF8;
			atom_t at = YAP_SWIAtomFromAtom(AbsAtom(a));
			type->write(stream, at, 0);
			Sclose(stream);
			popOutputContext();
			sz = strlen(s)+1;
			char *os = new char[sz];
			memcpy(os, s, sz);
			delete s;
			return os;
		} else {
			char *s = new char[sz];
#if defined(__linux__) || defined(__APPLE__)
			snprintf(s, sz, "'%s'(%p)", AtomSWIStream->StrOfAE, a);
#else
			snprintf(s, sz, "'%s'(0x%p)", AtomSWIStream->StrOfAE, a);
#endif
			char *os = new char[sz];
			memcpy(os, s, sz);
			delete s;
			return os;
		}
	} else {
		return a->StrOfAE;
	}
}


void
YAPQuery::initQuery( Term *t )
{
	CACHE_REGS

	this->oq = (YAPQuery *)LOCAL_execution;
	LOCAL_execution = (struct open_query_struct *)this;
	this->q_open=1;
	this->q_state=0;
	this->q_flags = 0;
	this->q_g = t;
}


YAPQuery::YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm t[]): YAPPredicate(f, mod)
{
	/* ignore flags  for now */
	initQuery( (Term *)t );
}

YAPQuery::YAPQuery(YAPFunctor f, YAPTerm t[]): YAPPredicate(f)
{
	/* ignore flags for now */
	initQuery( (Term *)t );
}

YAPQuery::YAPQuery(YAPPredicate p, YAPTerm t[]): YAPPredicate(p.ap)
{
	initQuery( (Term *)t );
}

YAPQuery::YAPQuery(char *s): YAPPredicate(s, &this->q_g)
{
	Term *t = this->q_g;

	initQuery( t );
}


int YAPQuery::next()
{
	CACHE_REGS
	int result;
	if (this->q_open != 1) return 0;
	if (setjmp(((YAPQuery *)LOCAL_execution)->q_env))
		return 0;
	// don't forget, on success these guys must create slots
	if (this->q_state == 0) {
		result = YAP_EnterGoal((YAP_PredEntryPtr)this->ap, this->q_g, &this->q_h);
	} else {
		LOCAL_AllowRestart = this->q_open;
		result = YAP_RetryGoal(&this->q_h);
	}
	this->q_state = 1;
	if (result == 0) {
		YAP_LeaveGoal(FALSE, &this->q_h);
		this->q_open = 0;
	}
	return result;
}

void YAPQuery::cut()
{
	CACHE_REGS

	if (this->q_open != 1 || this->q_state == 0) return;
	YAP_LeaveGoal(FALSE, &this->q_h);
	this->q_open = 0;
	LOCAL_execution = (struct open_query_struct *)this->oq;
}

void YAPQuery::close()
{
	CACHE_REGS

	if (EX && !(this->q_flags & (PL_Q_CATCH_EXCEPTION))) {
		EX = NULL;
	}
	/* need to implement backtracking here */
	if (this->q_open != 1 || this->q_state == 0) {
		return;
	}
	YAP_LeaveGoal(FALSE, &this->q_h);
	this->q_open = 0;
	LOCAL_execution = (struct open_query_struct *)this->oq;
}

int YAPPredicate::call(YAPTerm t[])
{
	YAPQuery q = YAPQuery(*this, t);
	int ret = q.next();
	q.cut();
	q.close();
	return ret;
}

YAPEngine::YAPEngine(YAPParams const& params)
{ YAP_Init( (YAP_init_args *)&params.init_args ); }


