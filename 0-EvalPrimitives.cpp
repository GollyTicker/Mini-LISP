#include<set>
#include<exception>
#include<memory>

pAST lisp_true = (pAST) at("t");
pAST lisp_false = (pAST) nl;

map<string,evalProc*> predefs;

/* NOTE: in the implementation of substitute, we need to take care of
quote and stop substitution. */
pAST prim_quote(pList expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to quote but found " << expr->lisp_string() << endl;
    return NULL;
  }
  else return expr->head;
}

pAST substitute(pAtom var, pAST value, pAST body);

pAST prim_atom(pList expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to atom but found " << expr->lisp_string() << endl;
    return NULL;
  }
  return eval(expr->head,env)->is_atom() ? lisp_true : lisp_false;
}

pAST prim_eq(pList expr, Env& env) {
  if (!expr->head || !expr->tail->head) {
    cout << "Error: expecting 2 arguments to eq but found " << expr->lisp_string() << endl;
    return NULL;
  }
  pAST left = eval(expr->head,env);
  pAST right = eval(expr->tail->head,env);
  /* equal if both are the same atom or the empty list. */
  pAtom la = dynamic_pointer_cast<Atom>(left);
  pAtom ra = dynamic_pointer_cast<Atom>(right);
  if (la && ra) return la->equals(ra)? lisp_true : lisp_false;
  pList ll = dynamic_pointer_cast<List>(left);
  pList rl = dynamic_pointer_cast<List>(right);
  if (ll && rl) return ll->empty && rl->empty ? lisp_true : lisp_false;
  return lisp_false;
}

pAST prim_cons(pList expr, Env& env) {
  if (!expr->head || !expr->tail->head) {
    cout << "Error: expecting 2 arguments to cons but found " << expr->lisp_string() << endl;
    return NULL;
  }
  pAST hd = eval(expr->head,env);
  pAST tl = eval(expr->tail->head,env);
  pList tail = dynamic_pointer_cast<List>(tl);
  if (!tail) {
    cout << "Cannot apply primitive cons on non-list tail " << tl->lisp_string() << endl;
    return NULL;
  }
  return cons(hd,tail);
}

pAST tryhead(pAST expr) {
  return ( expr && dynamic_pointer_cast<List>(expr) ) ?
    dynamic_pointer_cast<List>(expr)->head
    : NULL;
}
pList trytail(pAST expr) {
  return ( expr && dynamic_pointer_cast<List>(expr) ) ?
    dynamic_pointer_cast<List>(dynamic_pointer_cast<List>(expr)->tail)
    : NULL;
}
bool tryempty(pAST expr) {
  return !expr || !dynamic_pointer_cast<List>(expr) || dynamic_pointer_cast<List>(expr)->empty;
}

pAST prim_cond(pList expr, Env& env) {
  if (!expr->head || tryempty(tryhead(expr)) || tryempty(trytail(tryhead(expr))) ) {
    cout << "Error: expecting (condition expr) ... to cond but found " << expr->lisp_string() << endl;
    return NULL;
  }
  pList path = dynamic_pointer_cast<List>(expr->head);
  pAST condition = eval(path->head,env);
  pAtom b = dynamic_pointer_cast<Atom>(condition);
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

pAST prim_cdr(pList expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to cdr but found " << expr->lisp_string() << endl;
    return NULL;
  }
  pAST res = eval(expr->head,env);
  pList xs = dynamic_pointer_cast<List>(res);
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

pList evalList(pList expr, Env& env) {
  if (expr->empty) return nl;
  else {
    pAST headE = eval(expr->head, env);
    return cons(headE, evalList(expr->tail, env));
  }
}
pAST prim_list(pList expr, Env& env) { return evalList(expr,env); }

/* NOTE: in the implementation of
substitute, we need to ignore the newly defined variable.*/
pAST prim_define(pList expr, Env& env) {
  if (!expr->head || !expr->tail->head || !expr->tail->tail->head) {
    cout << "Error: expecting 3 arguments to define but found " << expr->lisp_string() << endl;
    return NULL;
  }
  pAtom var = dynamic_pointer_cast<Atom>(expr->head);
  if (var) {
    pAST new_body = substitute(var, /* new value */ expr->tail->head, /* body*/expr->tail->tail->head);
    //cout << "after substitute " << new_body->lisp_string()<< endl;
    return eval(new_body,env);
  }
  else {
    cout << "Cannot apply primitive define to non-atom " << expr->head->lisp_string() << endl;
    return NULL;
  }
}

/* NOTE: in the implementation of
substitute, we need to ignore the newly defined variable.*/
pAST prim_define_global(pList expr, Env& env) {
  if (!expr->head || !expr->tail->head) {
    cout << "Error: expecting 2 arguments to define! but found " << expr->lisp_string() << endl;
    return NULL;
  }
  pAtom var = dynamic_pointer_cast<Atom>(expr->head);
  if (var) {
    // global binding enables recursive definition
    pAST value = eval(expr->tail->head,env);
    env[var->str] = value;
    return value;
  }
  else {
    cout << "Cannot apply primitive define! to non-atom " << expr->head->lisp_string() << endl;
    return NULL;
  }
}

pAST prim_car(pList expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to car but found " << expr->lisp_string() << endl;
    return NULL;
  }
  pAST res = eval(expr->head,env);
  pList xs = dynamic_pointer_cast<List>(res);
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
bool is_quote(pAST e) {
  return tryhead(e)
    && tryhead(e)->is_atom()
    && dynamic_pointer_cast<Atom>(tryhead(e))->equals(at("quote"));
}
set<string> collect_atom_strings(pList xs) {
  if (xs->empty) return set<string>();
  else if (dynamic_pointer_cast<Atom>(xs->head)) {
    set<string> rec = collect_atom_strings(xs->tail);
    rec.insert(dynamic_pointer_cast<Atom>(xs->head)->str);
    return rec;
  }
  else {
    string err = "Encountered mal-formed argument-list for lambda definition " + xs->lisp_string();
    cout << err << endl;
    throw logic_error(err);
  }
}
// checks if it's a lambda expression and returns the arguments in argument-list
set<string> lambda_arguments(pAST e) {
  pList fn = dynamic_pointer_cast<List>(e);
  pAtom lambda_name = dynamic_pointer_cast<Atom>(tryhead(fn));
  pList args = dynamic_pointer_cast<List>(tryhead(trytail(fn)));
  if (fn && lambda_name && lambda_name->equals(at("lambda")) && args) {
    return collect_atom_strings(args);
  }
  else return set<string>();
}
set<string> define_vars(pAST e) {
  pList df = dynamic_pointer_cast<List>(e);
  pAtom define_name = dynamic_pointer_cast<Atom>(tryhead(df));
  pAtom var = dynamic_pointer_cast<Atom>(tryhead(trytail(df)));
  if (df && define_name
      && (define_name->equals(at("define!")) || define_name->equals(at("define")))
      && var) return set<string>{var->str};
  else return set<string>();
}
pAST substitute(pAtom var, pAST value, pAST body) {
  //cout << "substitute " << var->lisp_string() << " for " << value->lisp_string() << " in " << body->lisp_string() << endl;
  if (body->is_atom()) {
    pAtom atom = dynamic_pointer_cast<Atom>(body);
    return atom->equals(var) ? value : atom;
  }
  else {
    pList xs = dynamic_pointer_cast<List>(body);
    if (xs->empty) return nl;
    else {
      if (is_quote(xs) || /* don't continue substitution in quote */
        lambda_arguments(xs).count(var->str) >= 1 || /* shadowed by inner lambda. */
        define_vars(xs).count(var->str) >= 1) /* define with same variable */
      { return xs; }
      else {
        pAST new_head = substitute(var,value,xs->head);
        pList new_tail = dynamic_pointer_cast<List>(substitute(var,value,xs->tail));
        return cons(new_head,new_tail);
      }
    }
  }
}

/* NOTE: in the implementation of substitute, we need to take care of
lambdas and stop substitution of the variables in the argument-list. */
pAST prim_lambda_apply(pAST args_ast, pAST body, pList is, Env& env) {
  pList args = dynamic_pointer_cast<List>(args_ast);
  if (args) {
    if (args->empty) {
      return eval(body,env);
    }
    else {
      pAtom var = dynamic_pointer_cast<Atom>(args->head);
      if (var) {
        if (!is->empty) {
          // substitute input for argument
          pAST new_body = substitute(var,is->head, body);
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
pAST prim_environment(pList expr, Env& env) {
  /* (define! a '1) and (define! b '(a b c))   become   ( (a 1)  (b (a b c)) )  */
  pList assocs = nl;
  for (auto const& x : env) {
    pList pair = cons(at(x.first),cons(x.second,nl));
    assocs = cons(pair,assocs);
  }
  return assocs;
}

pAST prim_decr(pList expr, Env& env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to decr but found " << expr->lisp_string() << endl;
    return NULL;
  }
  pAST res = eval(expr->head,env);
  pAtom atom = dynamic_pointer_cast<Atom>(res);
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

pAST prim_plus(pList expr, Env& env) {
  pList xs = expr;
  int acc = 0;
  while(!xs->empty) {
    pAST evaled = eval(xs->head, env);
    pAtom at = dynamic_pointer_cast<Atom>(evaled);
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

pAST prim_standalone_lambda(pList expr, Env& env) {
  cout << "Cannot evaluate standalone primitive lambda with " << expr->lisp_string() << endl;
  return NULL;
}

void add_primitives() {
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
    predefs.insert({"define",&prim_define});
    predefs.insert({"define!",&prim_define_global});
    predefs.insert({"list",&prim_list});
    predefs.insert({"environment",&prim_environment});
  }
}
