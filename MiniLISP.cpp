#include<cstdlib>
#include<iostream>
#include<cstdio>
#include<vector>
#include<map>
#include<cassert>
#include<ios>
using namespace std;

// abstract semantic tree
/*
AST = atom
  | list

list = empty | AST list
*/
struct AST {
  virtual string to_string() { return "error AST::to_string"; };
  virtual string lisp_string() { return "error AST::lisp_string"; }
};

struct Atom: AST {
  string str = "";
  Atom(string st) { str = st; };
  virtual string to_string() { return "Atom(" + str + ")"; };
  virtual string lisp_string() { return str; }
};

struct List: AST {
  bool empty = true;
  AST* head = NULL; // NULL if empty is true
  List* tail = NULL; // NULL if empty is true
  List() {};
  List(AST& elem, List& tail);
  virtual string to_string();
  virtual string lisp_string();
};

List::List(AST& x, List& xs) {
  empty = false;
  head = &x;
  tail = &xs;
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

List nil;

int main(int k, char ** args){
  string s;
  getline(cin >> ws,s);

  cout << Atom("blubb").to_string() << endl;
  cout << Atom("").to_string() << endl;
  cout << nil.to_string() << endl;
  Atom a("1"), b("2"), c("3");
  List sec(b,nil), thr(a,sec);
  cout << thr.to_string() << endl;
  cout << thr.lisp_string() << endl;
  List bla(thr,thr), blubb(thr,bla);
  cout << blubb.lisp_string() << endl;

//  cout << cons(Atom("1"),cons(Atom("b"),nil)).lisp_string() << endl;

  /* parse input into abstract semantic tree: AST*/

  /* evaluate it. */

  /* print evaluated form. */

  cout << s;
}
