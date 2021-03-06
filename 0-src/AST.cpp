#include<memory>

/* ABSTRACT SEMANTIC TREE */

/*
Codefile = vector<AST>

AST = atom
  | list

list = nil | AST list
*/

/* Abstract Base Class*/
struct AST {
  virtual string to_string() { return "error AST::to_string"; };
  virtual string lisp_string() { return "error AST::lisp_string"; }
  virtual bool is_atom() { return "error AST:is_atom"; }
  virtual ~AST() = default;
  // virtual destructor to be used with dynamic-pointer-cast on shared_ptrs
};

/* Atom class. uses string as symbol */
struct Atom: AST {
  string str = "";
  Atom(string st) { str = st; };
  virtual string to_string() { return "Atom(" + str + ")"; };
  virtual string lisp_string() { return str; }
  virtual bool is_atom() { return true; }
  bool equals(const shared_ptr<Atom> r) { return str.compare(r->str) == 0; }
  virtual ~Atom() = default;
};

/* Single-Linked-List with AST* elements */
struct List: AST {
  bool empty = true;
  shared_ptr<AST> head = NULL; // NULL if empty is true
  shared_ptr<List> tail = NULL; // NULL if empty is true
  List() {};
  List(shared_ptr<AST> elem, shared_ptr<List> tail);
  virtual string to_string();
  virtual string lisp_string();
  virtual bool is_atom() { return false; }
  virtual ~List() = default;
};

List::List(shared_ptr<AST> x, shared_ptr<List> xs) {
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
    return "(" + head->lisp_string() + (tail->empty?"":" ") + tail->lisp_string().substr(1);
  }
}

struct Codefile {
  vector<shared_ptr<AST> > statements;
  Codefile(vector<shared_ptr<AST> > stmts) {
    statements=stmts;
  }
  string lisp_string() {
    ostringstream ss;
    for (shared_ptr<AST> past : statements) {
      ss << past->lisp_string() << endl;
    }
    return ss.str();
  }
};

// shared_ptr for memory management.
// shared_ptr implements automatic reference counting and deletion when it reaches 0.
typedef shared_ptr<AST> pAST;
typedef shared_ptr<Atom> pAtom;
typedef shared_ptr<List> pList;
typedef shared_ptr<Codefile> pCodefile;

// convinience constructors
pList nl = make_shared<List>();
pList cons(pAST head, pList tail) { return make_shared<List>(head, tail); }
pAtom at(string s) { return make_shared<Atom>(s); }
pCodefile codefile(vector<pAST> stmts) { return make_shared<Codefile>(stmts); }
