/* ABSTRACT SEMANTIC TREE */
/*
AST = atom
  | list

list = nil | AST list
*/

/* Abstract Base Class*/
struct AST {
  virtual string to_string() { return "error AST::to_string"; };
  virtual string lisp_string() { return "error AST::lisp_string"; }
  virtual bool is_atom() { return "error AST:is_atom"; }
};

/* Atom class. uses string as symbol */
struct Atom: AST {
  string str = "";
  Atom(string st) { str = st; };
  virtual string to_string() { return "Atom(" + str + ")"; };
  virtual string lisp_string() { return str; }
  virtual bool is_atom() { return true; }
  bool equals(const Atom* r) { return str.compare(r->str) == 0; }
};

/* Single-Linked-List with AST* elements */
struct List: AST {
  bool empty = true;
  AST* head = NULL; // NULL if empty is true
  List* tail = NULL; // NULL if empty is true
  List() {};
  List(AST* elem, List* tail);
  virtual string to_string();
  virtual string lisp_string();
  virtual bool is_atom() { return false; }
};

List::List(AST* x, List* xs) {
  empty = false;
  head = x;
  tail = xs;
}

string List::to_string() {
  if (empty) return "[]";
  else {
    return "[" + head->to_string() + (tail->empty?"":",") + tail->to_string().substr(1);
  }
}

string List::lisp_string() {
  if (empty) return "()";
  else {
    /* MAYBE-DO: special syntax for quote? */
    return "(" + head->lisp_string() + (tail->empty?"":" ") + tail->lisp_string().substr(1);
  }
}

// convinience constructors
List* nl = new List();
List* cons(AST* head, List* tail) { return new List(head, tail); }
Atom* at(string s) { return new Atom(s); }
