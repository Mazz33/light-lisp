#ifndef MYLISP
#define MYLISP

#include "mpc.h"

/*** Declarations ***/
struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;
typedef lval*(*lbuiltin) (lenv*, lval*);

/*** Structures Body ***/
struct lval {
	int type;
	
	//Types Base
	long num;
	char* err;
	char* sym;
	char* str;

	//Functions
	lbuiltin builtin;
	lenv* env; //environment
	lval* formals; //function input
	lval* body; //function body

	//S/Q-Expressions
	int count;
	lval** cell; //List of lisp values
};

struct lenv {
	lenv* par; //Parent environment. Null for none
	int count; //number of variabels
	char** syms; //name
	lval** vals; //value
};

/*** Parsers ***/
mpc_parser_t* Number; 
mpc_parser_t* Symbol; 
mpc_parser_t* String; 
mpc_parser_t* Comment;
mpc_parser_t* Sexpr;  
mpc_parser_t* Qexpr;  
mpc_parser_t* Expr; 
mpc_parser_t* Lispy;

/*** Lisp Value Types ***/
enum {
	LVAL_ERR,
	LVAL_NUM,
	LVAL_SYM,
	LVAL_STR,
	LVAL_FUN,
	LVAL_SEXPR,
	LVAL_QEXPR
};


/*** Lisp Value Constructors ***/
lval* lvalNum(long); //Lisp number constructor
lval* lvalStr(char*); //Lisp String constructor
lval* lvalSym(char*); //Lisp Symbol constructor
lval* lvalBuiltinFunc(lbuiltin); //Lisp builtin functions constructor
lval* lvalLambda(lval*, lval*); //Lisp user created functions constructor
lval* lvalSexpr(void); //Lisp Symbolic Expressions constructor
lval* lvalQexpr(void); //Lisp quoted expressions constructor
lval* lvalErr(char*, ...); //Lisp error

/*** Lisp Value Functions ***/
void lvalDel(lval*); //Deletes a Lisp value
lval* lvalCopy(lval*); //Copies a Lisp value. Returns the copied lval
lval* lvalAdd(lval*, lval*); //Adds a Lisp value B into lisp value A inbuilt list
lval* lvalJoin(lval*, lval*); //Joins Lisp value B into A and clears B
lval* lvalPop(lval*, int); //Pops an element from the cells in A. Must be a Q/S-expression
lval* lvalTake(lval*, int); //Takes one element from A at index B. returns it and deletes the rest
void lvalPrint(lval*); //Print the lisp value depending on its type.
void lvalPrintln(lval*); //Calls lvalPrint and adds a new line after
void lvalPrintExpr(lval*, char, char); //Printing of Q/S-expressions
void lvalPrintStr(lval*); //Print String Lisp value
int lvalEq(lval*, lval*); //Compares 2 lisp values. Return 0 for false. Anything else for True
char* ltypeName(int); //Return a string name for the lisp value type

/*** Lisp Environment Functions ***/
lenv* lenvNew(void); //Envrionemnt constructor
void lenvDel(lenv*); //Delete an environment
lenv* lenvCopy(lenv*); //Copy an environment
lval* lenvGet(lenv*, lval*); //returns lisp value for a variable name
void lenvPut(lenv*, lval*, lval*); //Change value of a variable. Or create a new one
void lenvDef(lenv*, lval*, lval*); //Defining in global environment

/*** Builtins functions ***/
lval* builtinLambda(lenv*, lval*); //define a function
lval* builtinList(lenv*, lval*); //return a Q-expression of the argument
lval* builtinHead(lenv*, lval*); //return the first element of an expression
lval* builtinTail(lenv*, lval*); //return the list without the first element
lval* builtinJoin(lenv*, lval*); //joins one or more expressions
lval* builtinEval(lenv*, lval*); //evaluate a q-expression as an s-expression
lval* builtinOp(lval*, char*); //operations
lval* builtinAdd(lenv*, lval*);
lval* builtinSub(lenv*, lval*);
lval* builtinMul(lenv*, lval*);
lval* builtinDiv(lenv*, lval*);
lval* builtinVar(lenv*, lval*, char*); //for variables
lval* builtinDef(lenv*, lval*); //define variable
lval* builtinPut(lenv*, lval*); //set variable
lval* builtinOrd(lval*, char*); //comparison 
lval* builtinGt(lenv*, lval*);
lval* builtinLt(lenv*, lval*);
lval* builtinGe(lenv*, lval*);
lval* builtinLe(lenv*, lval*);
lval* builtinCmp(lval*, char*);
lval* builtinEq(lenv*, lval*);
lval* builtinNe(lenv*, lval*);
lval* builtinIf(lenv*, lval*);
lval* builtinLoad(lenv*, lval*);
lval* builtinPrint(lenv*, lval*);
lval* builtinError(lenv*, lval*);
void lenvAddBuiltin(lenv*, char*, lbuiltin); //register builtins in the environment
void lenvAddBuiltins(lenv*); //calls add builtin to add them to the environment

/*** Evaluations ***/
lval* lvalCall(lenv*, lval*, lval*); //Run function
lval* lvalEvalSexpr(lenv*, lval*);
lval* lvalEval(lenv*, lval*); //evaluate an expression

/*** Reading ***/
lval* lvalRead(mpc_ast_t*);
lval* lvalReadStr(mpc_ast_t*);
lval* lvalReadNum(mpc_ast_t*);

/*** Macros ***/
#define LASSERT(args, cond, fmt, ...) \
	if (!(cond)) { lval* err = lvalErr(fmt, ##__VA_ARGS__); \
		lvalDel(args); \
		return err; \
	}

#define LASSERT_TYPE(func, args, index, expect) \
	LASSERT(args, (args -> cell[index] -> type) == expect, \
		"Function '%s' passwed incorrecttype for argument %i. Got %s, Expected %s.", \
			func, index, ltypeName(args -> cell[index] -> type), \
			ltypeName(expect))

#define LASSERT_NUM(func, args, num) \
	LASSERT(args, args -> count == num, \
		"Function '%s passed incorrect numebr of arguments. Got %i, Expected %i.", \
		func, args -> count, num)

#define LASSERT_NOT_EMPTY(func, args, index) \
	LASSERT(args, (args ->  cell[index] -> count) != 0, \
		"Function '%s' passed {} for argument %i.", func, index);

#endif
