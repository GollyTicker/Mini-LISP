#include<map>

typedef map<string,AST*> Env;
typedef AST* evalProc(List*, Env);

/* perhaps we can make the evaluation
cleaner by adding a procedure type to AST? */

// procedure that evaluates an expression using the environment.
// may change the environment.
AST* eval(AST* expr, Env env);

#include "EvalPrimitives.cpp"

List* evalList(List* expr, Env env) {
  if (expr->empty) return nl;
  else {
    AST* headE = eval(expr->head, env);
    return cons(headE, evalList(expr->tail, env));
  }
}

AST* eval(AST* expr, Env env) {
  if (debug) cout << "eval: " << expr-> lisp_string() << endl;

  if (expr->is_atom()) {
    Atom* a = dynamic_cast<Atom*>(expr);
    if (env.count(a->str) >= 1) return env[a->str];
    else {
      cout << "Cannot eval atom " << a->lisp_string() << ". Missing definition in env." << endl;
      return NULL;
    }
  }
  else { // expr is list
    List* xs = dynamic_cast<List*>(expr);
    if(!xs->empty) {
      AST* hd = xs->head;
      // head = predefined atom => apply
      // head = atom => lookup atom and apply
      // head = lambda => apply lambda
      // otherwise => evaluate and repeat

      Atom* atom = dynamic_cast<Atom*>(hd);
      if (atom) {
        if (predefs.count(atom->str) >= 1) {
          // cout << "Applying primitive " << atom->str << " on env of size " << env.size() << endl;
          return predefs[atom->str](xs->tail,env); // evaluation of args based on primitives
        }
        else if (env.count(atom->str) >= 1) {
          List* eagerArgs = evalList(xs->tail, env); // eager evaluation of args
          return eval(cons(env[atom->str],eagerArgs),env);
        }
        else {
          cout << "Cannot eval undefined atom in " << xs->lisp_string() << endl;
          return NULL;
        }
      }
      List* fn = dynamic_cast<List*>(hd);
      Atom* fname = dynamic_cast<Atom*>(tryhead(fn));
      if (fname && fname->equals(at("lambda"))) { /*fn is lambda */
        AST* args = tryhead(trytail(fn));
        AST* body = tryhead(trytail(trytail(fn)));
        return prim_lambda_apply(args,body,xs->tail,env);
      }
      else { /* evaluate fn and repeat */
        AST* evaledfn = eval(fn, env);
        // defer evaluation of arguments to next eval
        return eval(cons(evaledfn,xs->tail),env);
      }
    }
    else { // empty list
      cout << "Cannot eval empty list " << xs->lisp_string() << "." << endl;
      return NULL;
    }
  }

  return NULL;
}

/* standard library definitions */
void add_standard_library(Env env) {
  eval(parse_full("(define! test-var 'test-value)"),env);
  cout << "Env of size " << env.size() << endl;
}

AST* Eval(AST* expr) {
  Env e;
  add_standard_library(e);
  return eval(expr, e);
}




/*
lambdas online:

;gnu clisp  2.49.60

(print "Hello, world!")

(print ((lambda () (cons '1 '(c d)))))

(print ((lambda (a b) (cons a b)) '1 '(c d)))

(print ((lambda (x) (cons x (cons x '()))) 'a) )

(print ((lambda (x) (cons x (cons x '()))) (lambda (f x) x)))

; (print (((lambda (x) (cons x (cons x '()))) (lambda (f x) x)) 'a) )

; (print (((lambda (x) (cons x (cons x '()))) (lambda (f x) x)) 'a) )
*/
