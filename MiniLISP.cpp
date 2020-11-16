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
  virtual string to_string();
  //virtual string lispString();
};

struct Atom: AST {
  string str = "";
  Atom(string st);
  string to_string() override;
};

Atom::Atom(string st) {
  str = st;
}

string Atom::to_string(){
  return "Atom(" + str + ")";
}

struct List: AST {
  bool empty = true;
  AST* head = NULL; // NULL if empty is true
  List* tail = NULL; // NULL if empty is true
  List();
  List(AST* elem, List* tail);
  string to_string() override;
};

List::List(AST* x, List* xs) {
  empty = false;
  head = x;
  ;tail = xs;
}

string List::to_string() {
  if (empty) return "[]";
  else {
    return "[" + head->to_string() + "," + tail->to_string().substr(1);
  }
}

int main(int k, char ** args){
  string s;
  getline(cin >> ws,s);

  /* parse input into abstract semantic tree: AST*/

  /* evaluate it. */

  /* print evaluated form. */

  cout << s;
}
