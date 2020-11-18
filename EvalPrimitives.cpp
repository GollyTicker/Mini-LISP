AST* lisp_true = (AST*) at("t");
AST* lisp_false = (AST*) nl;

map<string,evalProc*> predefs;

AST* prim_quote(List* expr, Env env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to quote but found " << expr->lisp_string() << endl;
    return NULL;
  }
  else return expr->head;
}

AST* prim_atom(List* expr, Env env) {
  if (!expr->head) {
    cout << "Error: expecting 1 argument to atom but found " << expr->lisp_string() << endl;
    return NULL;
  }
  return eval(expr->head,env)->is_atom() ? (AST*)at("t") : (AST*)nl;
}

AST* prim_eq(List* expr, Env env) {
  if (!expr->head || !expr->tail->head) {
    cout << "Error: expecting 2 arguments to eq but found " << expr->lisp_string() << endl;
    return NULL;
  }
  AST* left = eval(expr->head,env);
  AST* right = eval(expr->tail->head,env);
  /* equal if both are the same atom or the empty list. */
  Atom* la = dynamic_cast<Atom*>(left);
  Atom* ra = dynamic_cast<Atom*>(right);
  if (la && ra) return la->str.compare(ra->str) == 0 ? lisp_true : lisp_false;
  List* ll = dynamic_cast<List*>(left);
  List* rl = dynamic_cast<List*>(right);
  if (ll && rl) return ll->empty && rl->empty ? lisp_true : lisp_false;
  return lisp_false;
}

AST* prim_cons(List* expr, Env env) {
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

List* tryhead(AST* expr) {
  return ( expr && dynamic_cast<List*>(expr) ) ?
    dynamic_cast<List*>(dynamic_cast<List*>(expr)->head)
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

AST* prim_cond(List* expr, Env env) {
  if (!expr->head || tryempty(tryhead(expr)) || tryempty(trytail(tryhead(expr))) ) {
    cout << "Error: expecting (condition expr) ... to cond but found " << expr->lisp_string() << endl;
    return NULL;
  }
  List* path = dynamic_cast<List*>(expr->head);
  AST* condition = eval(path->head,env);
  Atom* b = dynamic_cast<Atom*>(condition);
  if (b && b->str.compare("t") == 0) { // condition true, evaluate branch
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

AST* prim_cdr(List* expr, Env env) {
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

AST* prim_car(List* expr, Env env) {
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

AST* prim_plus(List* expr, Env env) {
  List* xs = expr;
  int acc = 0;
  while(!xs->empty) {
    Atom* at = dynamic_cast<Atom*>(xs->head);
    if(!at) {
      cout << "Cannot apply primitive + with non-atom arg " << (xs->head)->lisp_string() << endl;
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

void setup_interpreter() {
  if (predefs.size() == 0){
    predefs.insert({"quote",&prim_quote});
    predefs.insert({"atom",&prim_atom});
    predefs.insert({"+",&prim_plus});
    predefs.insert({"eq",&prim_eq});
    predefs.insert({"car",&prim_car});
    predefs.insert({"cdr",&prim_cdr});
    predefs.insert({"cons",&prim_cons});
    predefs.insert({"cond",&prim_cond});
  }
}
