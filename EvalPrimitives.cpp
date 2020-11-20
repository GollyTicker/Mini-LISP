#include<set>
#include<exception>

AST* lisp_true = (AST*) at("t");
AST* lisp_false = (AST*) nl;

map<string,evalProc*> predefs;

/* NOTE: in the implementation of substitute, we need to take care of
quote and stop substitution. */
AST* prim_quote(List* expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to quote but found " << expr->lisp_string() << endl;
    return NULL;
  }
  else return expr->head;
}

AST* prim_atom(List* expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to atom but found " << expr->lisp_string() << endl;
    return NULL;
  }
  return eval(expr->head,env)->is_atom() ? lisp_true : lisp_false;
}

AST* prim_eq(List* expr, Env& env) {
  if (!expr->head || !expr->tail->head) {
    cout << "Error: expecting 2 arguments to eq but found " << expr->lisp_string() << endl;
    return NULL;
  }
  AST* left = eval(expr->head,env);
  AST* right = eval(expr->tail->head,env);
  /* equal if both are the same atom or the empty list. */
  Atom* la = dynamic_cast<Atom*>(left);
  Atom* ra = dynamic_cast<Atom*>(right);
  if (la && ra) return la->equals(ra)? lisp_true : lisp_false;
  List* ll = dynamic_cast<List*>(left);
  List* rl = dynamic_cast<List*>(right);
  if (ll && rl) return ll->empty && rl->empty ? lisp_true : lisp_false;
  return lisp_false;
}

AST* prim_cons(List* expr, Env& env) {
  if (!expr->head || !expr->tail->head) {
    cout << "Error: expecting 2 arguments to cons but found " << expr->lisp_string() << endl;
    return NULL;
  }
  AST* hd = eval(expr->head,env);
  AST* tl = eval(expr->tail->head,env);
  List* tail = dynamic_cast<List*>(tl);
  if (!tail) {
    cout << "Cannot apply primitive cons on non-list tail " << tl->lisp_string() << endl;
    return NULL;
  }
  return cons(hd,tail);
}

AST* tryhead(AST* expr) {
  return ( expr && dynamic_cast<List*>(expr) ) ?
    dynamic_cast<List*>(expr)->head
    : NULL;
}
List* trytail(AST* expr) {
  return ( expr && dynamic_cast<List*>(expr) ) ?
    dynamic_cast<List*>(dynamic_cast<List*>(expr)->tail)
    : NULL;
}
bool tryempty(AST* expr) {
  return !expr || !dynamic_cast<List*>(expr) || dynamic_cast<List*>(expr)->empty;
}

AST* prim_cond(List* expr, Env& env) {
  if (!expr->head || tryempty(tryhead(expr)) || tryempty(trytail(tryhead(expr))) ) {
    cout << "Error: expecting (condition expr) ... to cond but found " << expr->lisp_string() << endl;
    return NULL;
  }
  List* path = dynamic_cast<List*>(expr->head);
  AST* condition = eval(path->head,env);
  Atom* b = dynamic_cast<Atom*>(condition);
  if (b && b->equals(at("t"))) { // condition true, evaluate branch
    return eval(path->tail->head,env);
  }
  else { // condition false. try rest.
    if (!expr->tail->empty) {
      return prim_cond(expr->tail,env);
    }
    else {
      cout << "Cannot apply primitive cons on non-exhaustive branches. " << endl;
      return NULL;
    }
  }
}

AST* prim_cdr(List* expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to cdr but found " << expr->lisp_string() << endl;
    return NULL;
  }
  AST* res = eval(expr->head,env);
  List* xs = dynamic_cast<List*>(res);
  if (!xs) {
    cout << "Cannot apply primitive cdr on non-list argument " << res->lisp_string() << endl;
    return NULL;
  }
  if (!xs->empty) {
    return xs->tail;
  }
  else {
    cout << "Cannot apply primitive cdr on empty list." << endl;
    return NULL;
  }
}

List* evalList(List* expr, Env& env) {
  if (expr->empty) return nl;
  else {
    AST* headE = eval(expr->head, env);
    return cons(headE, evalList(expr->tail, env));
  }
}
AST* prim_list(List* expr, Env& env) { return evalList(expr,env); }

/* NOTE: in the implementation of
substitute, we need to ignore the newly defined variable.*/
AST* prim_define(List* expr, Env& env) {
  if (!expr->head || !expr->tail->head) {
    cout << "Error: expecting 2 arguments to define! but found " << expr->lisp_string() << endl;
    return NULL;
  }
  Atom* var = dynamic_cast<Atom*>(expr->head);
  if (var) {
    // lambda definitions are given in quotes,
    // we can enable recursive definitions
    AST* value = eval(expr->tail->head,env);
    env[var->str] = value;
    return value;
  }
  else {
    cout << "Cannot apply primitive define! to non-atom " << expr->head->lisp_string() << endl;
    return NULL;
  }
}

AST* prim_car(List* expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to car but found " << expr->lisp_string() << endl;
    return NULL;
  }
  AST* res = eval(expr->head,env);
  List* xs = dynamic_cast<List*>(res);
  if (!xs) {
    cout << "Cannot apply primitive car on non-list argument " << res->lisp_string() << endl;
    return NULL;
  }
  if (!xs->empty) {
    return xs->head;
  }
  else {
    cout << "Cannot apply primitive car on empty list." << endl;
    return NULL;
  }
}

// is the head a quote?
bool is_quote(AST* e) {
  return tryhead(e)
    && tryhead(e)->is_atom()
    && dynamic_cast<Atom*>(tryhead(e))->equals(at("quote"));
}
set<string> collect_atom_strings(List* xs) {
  if (xs->empty) return set<string>();
  else if (dynamic_cast<Atom*>(xs->head)) {
    set<string> rec = collect_atom_strings(xs->tail);
    rec.insert(dynamic_cast<Atom*>(xs->head)->str);
    return rec;
  }
  else {
    string err = "Encountered mal-formed argument-list for lambda definition " + xs->lisp_string();
    cout << err << endl;
    throw logic_error(err);
  }
}
// checks if it's a lambda expression and returns the arguments in argument-list
set<string> lambda_arguments(AST* e) {
  List* fn = dynamic_cast<List*>(e);
  Atom* lambda_name = dynamic_cast<Atom*>(tryhead(fn));
  List* args = dynamic_cast<List*>(tryhead(trytail(fn)));
  if (fn && lambda_name && lambda_name->equals(at("lambda")) && args) {
    return collect_atom_strings(args);
  }
  else return set<string>();
}
set<string> define_vars(AST* e) {
  List* df = dynamic_cast<List*>(e);
  Atom* define_name = dynamic_cast<Atom*>(tryhead(df));
  Atom* var = dynamic_cast<Atom*>(tryhead(trytail(df)));
  if (df && define_name && define_name->equals(at("define!")) && var) return set<string>{var->str};
  else return set<string>();
}
AST* substitute(Atom* var, AST* value, AST* body) {
  //cout << "substitute " << var->lisp_string() << " for " << value->lisp_string() << " in " << body->lisp_string() << endl;
  if (body->is_atom()) {
    Atom* atom = dynamic_cast<Atom*>(body);
    if (atom->equals(var) && false) {
      cout << " replaced " << var->lisp_string() << " with "<< value->lisp_string() << endl;
    }
    return atom->equals(var) ? value : atom;
  }
  else {
    List* xs = dynamic_cast<List*>(body);
    if (xs->empty) return nl;
    else {
      AST* new_head;
      if (is_quote(xs->head) || /* don't continue substitution in quote */
      lambda_arguments(xs->head).count(var->str) >= 1 ||
      /* abort substitution, since it's shadowed by inner lambda. */
      define_vars(xs->head).count(var->str) >= 1
      /* abort substitution, if define with same variable */
      ) { new_head = xs->head; }
      else { new_head = substitute(var,value,xs->head); }

      List* new_tail = dynamic_cast<List*>(substitute(var,value,xs->tail));
      return cons(new_head,new_tail);
    }
  }
}

/* NOTE: in the implementation of substitute, we need to take care of
lambdas and stop substitution of the variables in the argument-list. */
AST* prim_lambda_apply(AST* args_ast, AST* body, List* is, Env& env) {
  List* args = dynamic_cast<List*>(args_ast);
  if (args) {
    if (args->empty) {
      return eval(body,env);
    }
    else {
      Atom* var = dynamic_cast<Atom*>(args->head);
      if (var) {
        if (!is->empty) {
          // substitute input for argument
          AST* new_body = substitute(var,is->head, body);
          // continue recursively
          return prim_lambda_apply(args->tail,new_body,is->tail, env);
        }
        else {
          cout << "Cannot apply lambda. Missing argument for " << args->lisp_string() << endl;
          return NULL;
        }
      }
      else {
        cout << "Error: expecting atom in argument-list for lambda definition, but got" << args->head->lisp_string() << endl;
        return NULL;
      }
    }
  }
  else {
    cout << "Error: expecting arguments-list for lambda definition, but got " << args_ast->lisp_string() << endl;
    return NULL;
  }
}

/* returns an association list representing the current environment bindings. */
AST* prim_environment(List* expr, Env& env) {
  /* (define! a '1) and (define! b '(a b c))   become   ( (a 1)  (b (a b c)) )  */
  List* assocs = nl;
  for (auto const& x : env) {
    List* pair = cons(at(x.first),cons(x.second,nl));
    assocs = cons(pair,assocs);
  }
  return assocs;
}

AST* prim_decr(List* expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to decr but found " << expr->lisp_string() << endl;
    return NULL;
  }
  AST* res = eval(expr->head,env);
  Atom* atom = dynamic_cast<Atom*>(res);
  if (!atom) {
    cout << "Cannot apply primitive decr on non-atom argument " << res->lisp_string() << endl;
    return NULL;
  }
  int n;
  try { n = stoi(atom->str); }
  catch (int e) {
    cout << "Cannot apply primitive decr with non-integer arg " << atom->lisp_string() << endl;
    return NULL;
  }
  return at(to_string(n-1));
}

AST* prim_plus(List* expr, Env& env) {
  List* xs = expr;
  int acc = 0;
  while(!xs->empty) {
    AST* evaled = eval(xs->head, env);
    Atom* at = dynamic_cast<Atom*>(evaled);
    if(!at) {
      cout << "Cannot apply primitive + with non-atom arg " << evaled->lisp_string() << endl;
      return NULL;
    }
    int n;
    try { n = stoi(at->str); }
    catch (int e) {
      cout << "Cannot apply primitive + with non-integer arg " << at->lisp_string() << endl;
      return NULL;
    }
    acc += n;
    xs = xs->tail;
  }
  return at(to_string(acc));
}

AST* prim_standalone_lambda(List* expr, Env& env) {
  cout << "Cannot evaluate standalone primitive lambda with " << expr->lisp_string() << endl;
  return NULL;
}

void setup_interpreter() {
  if (predefs.size() == 0){
    predefs.insert({"quote",&prim_quote});
    predefs.insert({"atom",&prim_atom});
    predefs.insert({"+",&prim_plus});
    predefs.insert({"decr",&prim_decr});
    predefs.insert({"eq",&prim_eq});
    predefs.insert({"car",&prim_car});
    predefs.insert({"cdr",&prim_cdr});
    predefs.insert({"cons",&prim_cons});
    predefs.insert({"cond",&prim_cond});
    predefs.insert({"lambda",&prim_standalone_lambda});
    predefs.insert({"define!",&prim_define});
    predefs.insert({"list",&prim_list});
    predefs.insert({"environment",&prim_environment});
  }
}
