#include<cstdlib>
#include<iostream>
#include<cstdio>
#include<vector>
#include<map>
#include<cassert>
#include<ios>
using namespace std;

/* ABSTRACT SEMANTIC TREE */
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
  List(AST* elem, List* tail);
  virtual string to_string();
  virtual string lisp_string();
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
    return "(" + head->lisp_string() + (tail->empty?"":" ") + tail->lisp_string().substr(1);
  }
}

List* nl = new List();
List* cons(AST* head, List* tail) { return new List(head, tail); }
AST* at(string s) { return new Atom(s); }

/* RECURSIVE DESCENT PARSING */

// parses single character c and returns new index or fails with -1
int character(char c, string s, int i) {
  if (i < s.length() && s[i] == c) return i+1;
  else return -1; /* failure */
}

// parses and skips whitespaces
int whitespace(string s, int i) {
  while(i < s.length() && s[i] == ' ') i++;
  return i;
}

char lookahead(string s, int i) {
  return s[i];
}

pair<AST*,int> lisp_expr(string s, int i);
pair<List*,int> list_expr(string s, int i);
pair<Atom*,int> atom_expr(string s, int i);


/* parse a lisp_expression starting at index i1.
returns NULL is it fails*/
pair<AST*,int> lisp_expr(string s, int i1) {
  int i2 = whitespace(s,i1);
  if (i2 < s.length()) {
    switch (lookahead(s,i2)) {
      case '\'': { /* special syntax for quote */
          AST* quoted;
          int iNext;
          tie(quoted,iNext) = lisp_expr(s,i2+1);
          AST* res = cons(at("quote"), cons(quoted, nl));
          return make_pair(res,iNext);
          break;
        }
      case '(' : { /* list */
          return list_expr(s,i2);
          break;
        }
      default  : { /* atom */
          return atom_expr(s,i2);
          break;
        }
    }
  }
  else {
    cout << "Expecting EXPR, but found end of stream at " << i2 << endl;
    return make_pair((AST*)NULL,-1);
  }
}

pair<Atom*,int> atom_expr(string s, int i) {
  int i0 = i;
  /*read atom name until ( or end or whitespace.*/
  while (i < s.length()) {
    if (s[i] == '(') {
      if (i==i0) {
        cout << "Expecting ATOM, but found '(' at " << i << endl;
        return make_pair((Atom*)NULL,-1);
      }
    }
    else if (s[i] == ' ' || s[i] == ')') {
      break;
    }
    else i++;
  }
  /*convert from i0 until i (exlcuding) to atom_name*/
  string atomname = s.substr(i0,i-i0);
  return make_pair(new Atom(atomname),i);
}

pair<List*,int> list_expr(string s, int i1) {
  int iElem = character('(',s,i1);
  if (iElem < s.length() && iElem != -1) {
    List* head;
    List* curr = head;
    List* next = NULL;
    char la = lookahead(s,iElem);
    while (la != ')') {
      /* lisp expression & tail*/
      AST* elem;
      int iWS;
      tie(elem,iWS) = lisp_expr(s,iElem);
      iElem = whitespace(s,iWS);
      curr -> empty = false; /* TODO: needs inspection */
      curr -> head = elem;
      curr -> tail = next;
      curr = next;
      next = NULL;
      if (iElem < s.length()) {
        la = lookahead(s,iElem);
      }
      else {
        cout << "Expecting EXPR or end of list, but found end of stream at" << iElem << endl;
        return make_pair((List*)NULL,-1);
      }
    }
    /* parse end of list, since while loop ended with ')' */
    int iEnd = character(')',s,iElem); // will always succeed
    next = nl;
    return make_pair(head,iEnd);
  }
  else {
    cout << "Expecting EXPR or end of list, but found end of stream at" << iElem << endl;
    return make_pair((List*)NULL,-1);
  }
}

int main(int k, char ** args){
  string s;
  getline(cin >> ws,s);

  /*cout << Atom("blubb").to_string() << endl;
  cout << Atom("").to_string() << endl;
  cout << nil.to_string() << endl;
  Atom a("1"), b("2"), c("3");
  List sec(b,nil), thr(a,sec);
  cout << thr.to_string() << endl;
  cout << thr.lisp_string() << endl;
  List bla(thr,thr), blubb(thr,bla);
  cout << blubb.lisp_string() << endl;*/

  AST* expr;
  try {
    expr = lisp_expr(s,0).first;
  } catch (int e) {
    cout << "Error " << e << endl;
  }

  cout << expr->lisp_string() << endl;

//  cout << cons(Atom("1"),cons(Atom("b"),nil)).lisp_string() << endl;

  /* parse input into abstract semantic tree: AST*/

  /* evaluate it. */

  /* print evaluated form. */
}
