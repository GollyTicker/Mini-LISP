#include<map>
#include<set>

typedef map<string,AST*> Env;
typedef AST* evalProc(AST*, Env);
// procedure that evaluates an expression using the environment.
// may change the environment.

map<string,evalProc*> predefs;

AST* noop(AST* expr, Env env) {
  return NULL;
}

void setup_interpreter() {
  if (predefs.size() == 0){
    predefs.insert({"quote",&noop});
    /*predefs.insert("eq");
    predefs.insert("atom");
    predefs.insert("cdr");
    predefs.insert("car");
    predefs.insert("cond");*/
  }
}

AST* eval(AST* expr, Env env) {
  cout << "eval: " << expr-> lisp_string() << endl;

  if (expr->is_atom()) {
    Atom* a = (Atom*)expr;
    if (env.count(a->str) >= 1) return env[a->str];
    else {
      cout << "Cannot eval atom " << a->lisp_string() << ". Missing definition in env." << endl;
      return NULL;
    }
  }
  else { // expr is list
    List* xs = (List*)expr;
    if(!xs->empty) {
      AST* hd = xs->head;
      // head is atom? apply its function, otherwise evaluate head.
      AST* funcname = (hd->is_atom()) ? hd : eval(hd, env);
      // TODO: lookup function name. accordingly evaluate arguments or change them.
    }
    else { // empty list
      cout << "Cannot evaluate empty list " << xs->lisp_string() << "." << endl;
      return NULL;
    }
  }

  return NULL;
}

AST* Eval(AST* expr) {
  Env e;
  return eval(expr, e);
}
