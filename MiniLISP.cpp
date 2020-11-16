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
class AST {
  // virtual string to_string();
};

class Atom: public AST {
  string str = "";
  Atom(string st);
};

Atom::Atom(string st) {
  str = st;
}

class List: public AST {
  public:
    bool empty = true;
    AST* head = NULL; // NULL if empty is true
    List* tail = NULL; // NULL if empty is true
    List();
    List(AST* elem, List* tail);
};

List::List(AST* x, List* xs) {
  empty = false;
  head = x;
  ;tail = xs;
}

int main(int k, char ** args){
  string s;
  getline(cin >> ws,s);

  /* parse input into abstract semantic tree: AST*/

  /* evaluate it. */

  /* print evaluated form. */

  cout << s;
}
