#include<map>

typedef map<string,AST*> Env;
typedef AST* evalProc(List*, Env);
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
      // head is atom? apply its function, otherwise evaluate head.
      AST* funcname = (hd->is_atom()) ? hd : eval(hd, env);
      Atom* at = dynamic_cast<Atom*>(funcname);
      if (!at) {
        cout << "Cannot eval head in " << xs->lisp_string() << " to atom." << endl;
        return NULL;
      }

      // the atom is either predefined or defined in the environment.
      if (predefs.count(at->str) >= 1) {
        return predefs[at->str](xs->tail,env); // eval predefined with tail
      }
      else if (env.count(at->str) >= 1) {
        List* eagerArgs = evalList(xs->tail, env);
        return eval(cons(env[at->str],eagerArgs), env);
      }
      else {
        cout << "Cannot eval atom in " << xs->lisp_string() << ". Missing definition in env." << endl;
        return NULL;
      }
    }
    else { // empty list
      cout << "Cannot eval empty list " << xs->lisp_string() << "." << endl;
      return NULL;
    }
  }

  return NULL;
}

AST* Eval(AST* expr) {
  Env e;
  return eval(expr, e);
}
