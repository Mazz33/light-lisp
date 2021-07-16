#include <stdio.h>
#include "lisp.h"
#include "mpc.h"

#ifdef _WIN32

static char buffer[2048];

char* readline(char* prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char* cpy = malloc(strlen(buffer)+1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy)-1] = '\0';
  return cpy;
}

void add_history(char* unused) {}

#else
#include <editline/readline.h>
#include <editline/history.h>
#endif

int main(int argc, char** argv)
{
	Number = mpc_new("number");
	Symbol = mpc_new("symbol");
	String = mpc_new("string");
	Comment = mpc_new("comment");
	Sexpr = mpc_new("sexpr");
	Qexpr = mpc_new("qexpr");
	Expr = mpc_new("expr");
	Lispy = mpc_new("lispy");

	mpca_lang(MPCA_LANG_DEFAULT,
		"														\
			number	: /-?[0-9]+/ ;								\
			symbol	: /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;		\
			string  : /\"(\\\\.|[^\"])*\"/ ;					\
			comment : /;[^\\r\\n]*/ ;							\
			sexpr   : '(' <expr>* ')' ;							\
			qexpr	: '{' <expr>* '}' ;							\
			expr    : <number>  | <symbol> | <string>			\
					| <comment> | <sexpr>  | <qexpr>;			\
			lispy   : /^/ <expr>* /$/ ;							\
		", Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lispy);

	lenv* e = lenvNew();
	lenvAddBuiltins(e);

	if (argc == 1) {
		puts("Ctrl + c to exit");
		
		while (1) {
			char* input = readline("lispy> ");
			add_history(input);

			mpc_result_t r;
			if (mpc_parse("<stdin>", input, Lispy, &r)) {
				lval* x = lvalEval(e, lvalRead(r.output));
				lvalPrintln(x);
				lvalDel(x);
				mpc_ast_delete(r.output);
			} else {
				mpc_err_print(r.error);
				mpc_err_delete(r.error);
			}

			free(input);
		}
	}
	
	if (argc >= 2) {
		for (int i = 1; i < argc; i++) {
			lval* args = lvalAdd(lvalSexpr(), lvalStr(argv[i]));
			lval* x = builtinLoad(e, args);

			if ((x -> type) == LVAL_ERR)
				lvalPrintln(x);
			
			lvalDel(x);
		}
	}

	lenvDel(e);
	mpc_cleanup(8, Number, Symbol, String,
		Comment, Sexpr, Qexpr, Expr, Lispy);

	return 0;
}

lval* lvalNum(long n)
{
	lval* x = malloc(sizeof(lval));
	x -> type = LVAL_NUM;
	x -> num = n;

	return x;
}

lval* lvalStr(char* s)
{
	lval* x = malloc(sizeof(lval));
	x -> type = LVAL_STR;
	x -> str = malloc(strlen(s) + 1);
	strcpy(x -> str, s);

	return x;
}

lval* lvalSym(char* s)
{
	lval* x = malloc(sizeof(lval));
	x -> type = LVAL_SYM;
	x -> sym = malloc(strlen(s) + 1);
	strcpy(x -> sym, s);

	return x;
}

lval* lvalBuiltinFunc(lbuiltin func)
{
	lval* x = malloc(sizeof(lval));
	x -> type = LVAL_FUN;
	x -> builtin = func;

	return x;
}

lval* lvalLambda(lval* formals, lval* body)
{
	lval* x = malloc(sizeof(lval));
	x -> type = LVAL_FUN;
	x -> builtin = NULL;
	x -> formals = formals;
	x -> body = body;
	x -> env = lenvNew();

	return x;
}

lval* lvalSexpr(void)
{
	lval* x = malloc(sizeof(lval));
	x -> type = LVAL_SEXPR;
	x -> count = 0;
	x -> cell = NULL;

	return x;
}

lval* lvalQexpr(void)
{
	lval* x = malloc(sizeof(lval));
	x -> type = LVAL_QEXPR;
	x -> count = 0;
	x -> cell = NULL;

	return x;
}

lval* lvalErr(char* fmt, ...)
{
	lval* x = malloc(sizeof(lval));
	x -> type = LVAL_ERR;
	va_list va;
	va_start(va, fmt);
	x -> err = malloc(512);
	vsnprintf(x -> err, 511, fmt, va);
	x -> err = realloc(x -> err, strlen(x -> err) + 1);
	va_end(va);

	return x;
}

void lvalDel(lval* v)
{
	switch(v -> type) {
		case LVAL_NUM:
			break;
		case LVAL_STR:
			free(v -> str);
			break;
		case LVAL_SYM:
			free(v -> sym);
			break;
		case LVAL_ERR:
			free(v -> err);
			break;
		case LVAL_FUN:
			if (!(v -> builtin)) {
				lvalDel(v -> formals);
				lvalDel(v -> body);
				lenvDel(v -> env);
			}
			break;
		case LVAL_SEXPR:
		case LVAL_QEXPR:
			for (int i = 0; i < (v -> count); i++) {
				lvalDel((v -> cell)[i]);
			}
			free(v -> cell);
			break;
	}
	
	free(v);
}

lval* lvalCopy(lval* v)
{
	lval* x = malloc(sizeof(lval));
	x -> type = v -> type;
	switch (v -> type) {
		case LVAL_NUM:
			x -> num = v -> num;
			break;
		case LVAL_STR:
			x -> str = malloc(sizeof(v -> str) + 1);
			strcpy(x -> str, v -> str);
			break;
		case LVAL_SYM:
			x -> sym = malloc(sizeof(v -> sym) + 1);
			strcpy(x -> sym, v -> sym);
			break;
		case LVAL_ERR:
			x -> err = malloc(sizeof(v -> err) + 1);
			strcpy(x -> err, v -> err);
			break;
		case LVAL_FUN:
			if (!(v -> builtin)) {
				x -> builtin = NULL;
				x -> env = lenvCopy(v -> env);
				x -> body = lvalCopy(v -> body);
				x -> formals = lvalCopy(v -> formals);
			} else {
				x -> builtin = v -> builtin;
			}
			break;
		case LVAL_SEXPR:
		case LVAL_QEXPR:
			x -> count = v -> count;
			x -> cell = malloc(sizeof(lval*) * x -> count);
			for(int i = 0; i < (x -> count); i++) {
				(x -> cell)[i] = lvalCopy((v -> cell)[i]);
			}
			break;
	}

	return x;
}

lval* lvalAdd(lval* a, lval* b) 
{
	(a -> count)++;
	a -> cell = realloc(a -> cell, sizeof(lval*) * a -> count);
	a -> cell[(a -> count) - 1] = b;
	
	return a;
}

lval* lvalJoin(lval* a, lval* b)
{
	for(int i = 0; i < (b -> count); i++) {
		a = lvalAdd(a, b -> cell[i]);
	}
	lvalDel(b);
	
	return a;
}

lval* lvalPop(lval* v, int i)
{
	if (i >= (v -> count)) {
		return lvalErr("Out of bound. size is %i. Tried to access at index %i", v -> count, i);
	}

	lval* x = lvalCopy((v -> cell)[i]);
	memmove(&v->cell[i],
	&v->cell[i+1], sizeof(lval*) * (v->count-i-1));  
	v->count--;  
	v->cell = realloc(v->cell, sizeof(lval*) * v->count);

	return x;
}

lval* lvalTake(lval* v, int i)
{
	lval* x = lvalPop(v, i);
	lvalDel(v);
	
	return x;
}

void lvalPrint(lval* v)
{
	switch (v -> type) {
		case LVAL_NUM:
			printf("%li", v -> num);
			break;
		case LVAL_STR:
			lvalPrintStr(v);
			break;
		case LVAL_SYM:
			printf("%s", v -> sym);
			break;
		case LVAL_ERR:
			printf("Error: %s", v -> err);
			break;
		case LVAL_SEXPR:
			lvalPrintExpr(v, '(', ')');
			break;
		case LVAL_QEXPR:
			lvalPrintExpr(v, '{', '}');
			break;
		case LVAL_FUN:
			if (v -> builtin) {
				printf("<builtin>");
			} else {
				printf("(\\ ");
				lvalPrint(v -> formals);
				putchar(' ');
				lvalPrint(v -> body);
				putchar(')');
			}
			break;
	}
}

void lvalPrintln(lval* v)
{
	lvalPrint(v);
	putchar('\n');
}

void lvalPrintExpr(lval* v, char open, char close)
{
	putchar(open);
	for (int i = 0; i < (v -> count); i++) {
		lvalPrint((v -> cell)[i]);
		if (i != ((v -> count) -1)) {
			putchar(' ');
		}
	}
	putchar(close);
}

void lvalPrintStr(lval* v)
{
	char* escaped = malloc(strlen(v -> str) + 1);
	strcpy(escaped, v -> str);
	escaped = mpcf_escape(escaped);
	printf("\"%s\"", escaped);
	free(escaped);
}

int lvalEq(lval* a, lval* b)
{
	if ((a -> type) != (b -> type))
		return 0;
	
	switch (a -> type) {
		case LVAL_NUM:
			return (a -> num) == (b -> num);
		case LVAL_STR:
			return !strcmp(a -> str, b -> str);
		case LVAL_SYM:
			return !strcmp(a -> sym, b -> sym);
		case LVAL_ERR:
			return !strcmp(a -> err, b -> err);
		case LVAL_SEXPR:
		case LVAL_QEXPR:
			if ((a -> count) != (b -> count))
				return 0;
			for (int i = 0; i < (a -> count); i++) {
				if (!lvalEq((a -> cell)[i], (b -> cell)[i]))
					return 0;
			}
			return 1;
		case LVAL_FUN:
			if ((a -> builtin) || (b -> builtin)) {
				return (a -> builtin) == (b -> builtin);
			} else {
				return lvalEq(a -> formals, b -> formals) && lvalEq(a -> body, b -> body);
			}
		default:
			return 0; //Unreachable
	}
}

char* ltypeName(int i)
{
	switch (i) {
		case LVAL_FUN: 
			return "Function";
		case LVAL_NUM: 
			return "Number";
    	case LVAL_ERR: 
			return "Error";
    	case LVAL_SYM: 
			return "Symbol";
	    case LVAL_STR: 
			return "String";
	    case LVAL_SEXPR: 
			return "S-Expression";
	    case LVAL_QEXPR: 
			return "Q-Expression";
    	default: 
			return "Unknown";
	}
}

lenv* lenvNew(void)
{
	lenv* e = malloc(sizeof(lenv));
	e -> par = NULL;
	e -> count = 0;
	e -> syms = NULL;
	e -> vals = NULL;

	return e;
}

void lenvDel(lenv* e)
{
	for(int i = 0; i < (e -> count); i++) {
		free((e -> syms)[i]);
		lvalDel((e -> vals)[i]);
	}
	
	free(e -> syms);
	free(e -> vals);
	free(e);
}

lenv* lenvCopy(lenv* e)
{
	lenv* x = malloc(sizeof(lenv));
	x -> par = e -> par;
	x -> count = e -> count;
	x -> syms = malloc(sizeof(char*) * e -> count);
	x -> vals = malloc(sizeof(lval*) * e -> count);
	for (int i = 0; i < (e -> count); i++) {
		(x -> syms)[i] = malloc(strlen((e -> syms)[i]) + 1);
		strcpy((x -> syms)[i], (e -> syms)[i]);
		(x -> vals)[i] = lvalCopy((e -> vals)[i]);
	}

	return x;
}

lval* lenvGet(lenv* e, lval* a)
{
	for (int i = 0; i < (e -> count); i++) {
		if (!strcmp((e -> syms)[i], a -> sym))
			return lvalCopy((e -> vals)[i]);
	}
	if (e -> par) {
		return lenvGet(e -> par, a);
	} else {
		return lvalErr("Unbound Symbol '%s'", a -> sym);
	}
}

void lenvPut(lenv* e, lval* a, lval* b)
{
	for (int i = 0; i < (e -> count); i++) {
		if (!strcmp((e -> syms)[i], a -> sym)) {
			lvalDel((e -> vals)[i]);
			(e -> vals)[i] = lvalCopy(b);
			return;
		}
	}

	(e -> count)++;
	e -> vals = realloc(e -> vals, sizeof(lval*) * (e -> count));
	e -> syms = realloc(e -> syms, sizeof(char*) * (e -> count));
	(e -> vals)[(e -> count) - 1] = lvalCopy(b);
	(e -> syms)[(e -> count) - 1] = malloc(strlen((a -> sym) + 1));
	strcpy((e -> syms)[(e -> count) - 1], a -> sym);
}

void lenvDef(lenv* e, lval* k, lval* v)
{
	while (e -> par)
		e = e -> par;
	lenvPut(e, k, v);
}

lval* builtinLambda(lenv* e, lval* a)
{
	LASSERT_NUM("\\", a, 2);
	LASSERT_TYPE("\\", a, 0, LVAL_QEXPR);
	LASSERT_TYPE("\\", a, 1, LVAL_QEXPR);

	for (int i = 0; i < ((a -> cell)[0] -> count); i++) {
		LASSERT(a, ((a -> cell[0] -> cell[i] -> type) == LVAL_SYM),
			"Cannot define non-symbol. Got %s, Expected %s.",
			ltypeName(a -> cell[0] -> cell[i] -> type),
			ltypeName(LVAL_SYM));
	}

	lval* formals = lvalPop(a, 0);
	lval* body = lvalPop(a, 0);
	lvalDel(a);

	return lvalLambda(formals, body);

}

lval* builtinList(lenv* e, lval* a)
{
	a -> type = LVAL_QEXPR;
	return a;
}

lval* builtinHead(lenv* e, lval* a)
{
	LASSERT_NUM("head", a, 1);
	LASSERT_TYPE("head", a, 0, LVAL_QEXPR);
	LASSERT_NOT_EMPTY("head", a, 0);

	lval* v = lvalTake(a, 0);  
	while (v->count > 1) { lvalDel(lvalPop(v, 1)); }
	return v;
}

lval* builtinTail(lenv* e, lval* v)
{
	LASSERT_NUM("tail", v, 1);
	LASSERT_TYPE("tail", v, 0, LVAL_QEXPR);
	LASSERT_NOT_EMPTY("tail", v, 0);
		
	lval* x = lvalQexpr();
	for (int i = 1; i < (v -> count); i++) {
		lvalAdd(x, (v -> cell)[i]);
	}

	return x;
}

lval* builtinJoin(lenv* e, lval* a)
{
	for (int i = 0; i < (a -> count); i++) {
		LASSERT_TYPE("join", a, i, LVAL_QEXPR);
	}

	lval* x = lvalPop(a, 0);
	while (a -> count) {
		lval* y = lvalPop(a, 0);
		x = lvalJoin(x, y);
	}

	lvalDel(a);
	return x;
}

lval* builtinEval(lenv* e, lval* a)
{
	LASSERT_NUM("eval", a, 1);
	LASSERT_TYPE("eval", a, 0, LVAL_QEXPR);

	lval* x = lvalTake(a, 0);
	x -> type = LVAL_SEXPR;

	return lvalEval(e, x);
}

lval* builtinOp(lval* a, char* op)
{
	for (int i = 0; i < (a -> count); i++) {
		LASSERT_TYPE(op, a, i, LVAL_NUM);
	}

	lval* x = lvalPop(a, 0);
	if (!strcmp(op, "-") && a -> count == 0)
		x -> num = -(x -> num);
	
	while ((a -> count) > 0) {
		lval* y = lvalPop(a, 0);

		if (!strcmp(op, "+"))
			x -> num += y -> num;
		if (!strcmp(op, "-"))
			x -> num -= y -> num;
		if (!strcmp(op, "*"))
			x -> num *= y -> num;
		if (!strcmp(op, "/")) {
			if (y -> num == 0) {
				lvalDel(x);
				lvalDel(y);
				return lvalErr("Division by Zero.");
			}
			x -> num /= y -> num;
		}

		lvalDel(y);
	}

	lvalDel(a);
	return x;
}

lval* builtinAdd(lenv* e, lval* a)
{
	return builtinOp(a, "+");
}

lval* builtinSub(lenv* e, lval* a)
{
	return builtinOp(a, "-");
}

lval* builtinMul(lenv* e, lval* a) 
{
	return builtinOp(a, "*");
}

lval* builtinDiv(lenv* e, lval* a) 
{
	return builtinOp(a, "/");
}

lval* builtinVar(lenv* e, lval* a, char* func)
{
	LASSERT_TYPE(func, a, 0, LVAL_QEXPR);

	lval* syms = (a -> cell)[0];
	for (int i = 0; i < (a -> count); i++) {
		LASSERT(a, (((syms -> cell)[i] -> type) == LVAL_SYM),
			"Function '%s' cannot define non symbol. Got %s, Expected %s.",
			func, ltypeName((syms -> cell)[i] -> type), ltypeName(LVAL_SYM));
	}

	LASSERT(a, ((syms -> count) == ((a -> count) - 1)),
    	"Function '%s' passed too many arguments for symbols. Got %i, Expected %i.",
    	func, syms -> count, (a -> count) - 1);
	
	for (int i = 0; i < (syms -> count); i++) {
		if (!strcmp(func, "def")) {
			lenvDef(e, (syms -> cell)[i], (a -> cell)[i + 1]);
		}
		if (!strcmp(func, "=")) {
			lenvPut(e, (syms -> cell)[i], (a -> cell)[i + 1]);
		}
	}

	lvalDel(a);
	return lvalSexpr();
}

lval* builtinDef(lenv* e, lval* a)
{
	return builtinVar(e, a, "def");
}

lval* builtinPut(lenv* e, lval* a)
{
	return builtinVar(e, a, "=");
}

lval* builtinOrd(lval* a, char* op)
{
	LASSERT_NUM(op, a, 2);
	LASSERT_TYPE(op, a, 0, LVAL_NUM);
	LASSERT_TYPE(op, a, 1, LVAL_NUM);

	int r;
	if (strcmp(op, ">")  == 0) {
		r = (((a -> cell)[0] -> num) > ((a -> cell)[1] -> num)); 
	} else if (strcmp(op, "<")  == 0) {
		r = (((a -> cell)[0] -> num) < ((a -> cell)[1] -> num)); 
	} else if (strcmp(op, ">=") == 0) {
		r = (((a -> cell)[0] -> num) >= ((a -> cell)[1] -> num)); 
	} else if (strcmp(op, "<=") == 0) {
		r = (((a -> cell)[0] -> num) <= ((a -> cell)[1] -> num)); 
	}

	lvalDel(a);
	return lvalNum(r);
}

lval* builtinGt(lenv* e, lval* a)
{
	return builtinOrd(a, ">");
}
lval* builtinLt(lenv* e, lval* a)
{
	return builtinOrd(a, "<");
}
lval* builtinGe(lenv* e, lval* a)
{
	return builtinOrd(a, ">=");
}

lval* builtinLe(lenv* e, lval* a)
{
	return builtinOrd(a, "<=");
}

lval* builtinCmp(lval* a, char* op)
{
	LASSERT_NUM(op, a, 2);
	int r;

	if (!strcmp(op, "==")) {
		r = lvalEq((a -> cell)[0], (a -> cell)[1]);
	}
	if (!strcmp(op, "!=")) {
		r = !lvalEq((a -> cell)[0], (a -> cell)[1]);
	}

	return lvalNum(r);
}

lval* builtinEq(lenv* e, lval* a)
{
	return builtinCmp(a, "==");
}

lval* builtinNe(lenv* e, lval* a)
{
	return builtinCmp(a, "!=");
}

lval* builtinIf(lenv* e, lval* a)
{
	LASSERT_NUM("if", a, 3);
	LASSERT_TYPE("if", a, 0, LVAL_NUM);
	LASSERT_TYPE("if", a, 1, LVAL_QEXPR);
	LASSERT_TYPE("if", a, 2, LVAL_QEXPR);

	lval* x;
	(a -> cell)[1] -> type = LVAL_SEXPR;
	(a -> cell)[2] -> type = LVAL_SEXPR;

	if ((a -> cell)[0] -> num) {
		x = lvalEval(e, lvalPop(a, 1));
	} else {
		x = lvalEval(e, lvalPop(a, 2));
	}

	lvalDel(a);
	return x;
}

lval* builtinLoad(lenv* e, lval* a)
{
	LASSERT_NUM("load", a, 1);
	LASSERT_TYPE("load", a, 0, LVAL_STR);

	mpc_result_t r;
	if (mpc_parse_contents(a -> cell[0] -> str, Lispy, &r)) {
		lval* expr = lvalRead(r.output);
		mpc_ast_delete(r.output);

		while (expr -> count) {
			lval* x = lvalEval(e, lvalPop(expr, 0));
			if (x -> type == LVAL_ERR)
				lvalPrint(x);
			lvalDel(x);
		}

		lvalDel(expr);
		lvalDel(a);

		return lvalSexpr();
	} else {
		char* errorMsg = mpc_err_string(r.error);
		mpc_err_delete(r.error);

		lval* err = lvalErr("Couldn't load library %s", errorMsg);
		free(errorMsg);
		lvalDel(a);

		return err;
	}
}

lval* builtinPrint(lenv* e, lval* a)
{
	for (int i = 0; i < (a -> count); i++) {
		lvalPrint((a -> cell)[i]);
		putchar(' ');
	}

	putchar('\n');
	lvalDel(a);

	return lvalSexpr();
}

lval* builtinError(lenv* e, lval* a)
{
	LASSERT_NUM("error", a, 1);
	LASSERT_TYPE("error", a, 0, LVAL_STR);

	lval* err = lvalErr(a -> cell[0] -> str);
	lvalDel(a);

	return err;
}

void lenvAddBuiltin(lenv* e, char* name, lbuiltin func)
{
	lval* k = lvalSym(name);
	lval* v = lvalBuiltinFunc(func);
	lenvPut(e, k, v);
	lvalDel(k);
	lvalDel(v);
}

void lenvAddBuiltins(lenv* e)
{
	//variables functions
	lenvAddBuiltin(e, "\\", builtinLambda);
	lenvAddBuiltin(e, "def", builtinDef);
	lenvAddBuiltin(e, "=", builtinPut);
	
	//List funcs
	lenvAddBuiltin(e, "list", builtinList);
	lenvAddBuiltin(e, "head", builtinHead);
	lenvAddBuiltin(e, "tail", builtinTail);
	lenvAddBuiltin(e, "eval", builtinEval);
	lenvAddBuiltin(e, "join", builtinJoin);
	
	//Math funcs
	lenvAddBuiltin(e, "+", builtinAdd);
	lenvAddBuiltin(e, "-", builtinSub);
	lenvAddBuiltin(e, "*", builtinMul);
	lenvAddBuiltin(e, "/", builtinDiv);
	
	//comparison funcs
	lenvAddBuiltin(e, "if", builtinIf);
	lenvAddBuiltin(e, "==", builtinEq);
	lenvAddBuiltin(e, "!=", builtinNe);
	lenvAddBuiltin(e, ">", builtinGt);
	lenvAddBuiltin(e, "<", builtinLt);
	lenvAddBuiltin(e, ">=", builtinGe);
	lenvAddBuiltin(e, "<=", builtinLe);
	
	//string funcs
	lenvAddBuiltin(e, "load", builtinLoad);
	lenvAddBuiltin(e, "error", builtinError);
	lenvAddBuiltin(e, "print", builtinPrint);
}

lval* lvalCall(lenv* e, lval* f, lval* a)
{
	if (f -> builtin)
		return f -> builtin(e, a);
	
	int given = a -> count;
	int total = f -> formals -> count;

	while (a -> count) {
		if (f -> formals -> count == 0) {
			lvalDel(a);
			return lvalErr("Function passed too many arguments. Got %i, Expected %i.",
				given, total);
		}

		lval* sym = lvalPop(f -> formals, 0);
		if (!strcmp(sym -> sym, "&")) {
			if (f -> formals -> count != 1) {
				lvalDel(a);
				return lvalErr("Function format invalid. Symbol '&' not followed by single symbol");
			}

			lval* nsym = lvalPop(f -> formals, 0);
			lenvPut(f -> env, nsym, builtinList(e, a));
			break;
		}

		lval* val = lvalPop(a, 0);
		lenvPut(f -> env, sym, val);
		lvalDel(sym);
		lvalDel(val);
	}

	lvalDel(a);
	if ((f -> formals -> count > 0) &&
	  !(strcmp(f -> formals -> cell[0] -> sym, "&"))) {
			
		if (f -> formals -> count != 2) {
			return lvalErr("Function formalt invalid. Symbol '&' not followed by single sumbol.");
		}

		lvalDel(lvalPop(f -> formals, 0));
		lval* sym = lvalPop(f -> formals, 0);
		lval* val = lvalQexpr();
		lenvPut(f -> env, sym, val);
		lvalDel(sym);
		lvalDel(val);
	}

	if (f -> formals -> count == 0) {
		f -> env -> par = e;
		return builtinEval(f -> env, lvalAdd(lvalSexpr(), lvalCopy(f -> body)));
	} else {
		return lvalCopy(f);
	}
}

lval* lvalEvalSexpr(lenv* e, lval* a)
{
	for (int i = 0; i < (a -> count); i++) {
		(a -> cell)[i] = lvalEval(e, (a -> cell)[i]);
	}
	
	for (int i = 0; i < (a -> count); i++) {
		if ((a -> cell)[i] -> type == LVAL_ERR)
			return lvalTake(a, i);
	}

	if (a -> count == 0)
		return a;
	if (a -> count == 1)
		return lvalEval(e, lvalTake(a, 0));

	lval* f = lvalPop(a, 0);
	if (f -> type != LVAL_FUN) {
		lval* err = lvalErr("S-Expression starts with incorrect type. Got %s, Expected %s.",
	        ltypeName(f -> type), ltypeName(LVAL_FUN));	
		lvalDel(f);
		lvalDel(a);
		return err;
	}
	
	lval* result = lvalCall(e, f, a);
	lvalDel(f);

	return result;
}

lval* lvalEval(lenv* e, lval* a)
{
	if (a -> type == LVAL_SYM) {
		lval* x = lenvGet(e, a);
		lvalDel(a);
		return x;
	}
	if (a -> type == LVAL_SEXPR)
		return lvalEvalSexpr(e, a);
   
	return a;	
}

lval* lvalRead(mpc_ast_t* t)
{
	if (strstr(t -> tag, "number"))
		return lvalReadNum(t);
	if (strstr(t -> tag, "string"))
		return lvalReadStr(t);
	if (strstr(t -> tag, "symbol"))
		return lvalSym(t -> contents);

	lval* x = NULL;
	if (!strcmp(t -> tag, ">") || strstr(t -> tag, "sexpr"))
		x = lvalSexpr();
	if (strstr(t -> tag, "qexpr"))
		x = lvalQexpr();
	
	for (int i = 0; i < (t -> children_num); i++)
	{
		if(!strcmp(t -> children[i] -> contents, "(")) continue;
		if(!strcmp(t -> children[i] -> contents, ")")) continue;
		if(!strcmp(t -> children[i] -> contents, "}")) continue;
		if(!strcmp(t -> children[i] -> contents, "{")) continue;
		if(!strcmp(t -> children[i] -> tag, "regex")) continue;
		if(strstr(t -> children[i] -> tag, "comment")) continue;

		x = lvalAdd(x, lvalRead(t -> children[i]));
	}

	return x;
}

lval* lvalReadStr(mpc_ast_t* t)
{
	t -> contents[strlen(t -> contents) - 1] = '\0';
	char* unescaped = malloc(strlen(t -> contents + 1) + 1);
	strcpy(unescaped, t -> contents + 1);
	unescaped = mpcf_unescape(unescaped);

	lval* str = lvalStr(unescaped);
	free(unescaped);
	return str;
}

lval* lvalReadNum(mpc_ast_t* t)
{
	errno = 0;
	long x = strtol(t -> contents, NULL, 10);
	return errno != ERANGE ? lvalNum(x) : lvalErr("Invalid Number");
}
