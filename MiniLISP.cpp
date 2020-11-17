#include<cstdlib>
#include<iostream>
#include<cstdio>
#include<vector>
#include<map>
#include<cassert>
#include<ios>
using namespace std;

const bool debug = false;

#include "AST.cpp"
#include "Parser.cpp"
#include "Eval.cpp"
// ^ being lazy and not using header files.


int main(int k, char ** args){
  string s;
  getline(cin >> ws,s);

  /* 1. parse input into abstract semantic tree: AST*/
  /* 2. evaluate it. */
  /* 3. print evaluated form. */

  AST* expr = parse_full(s);
  if (!expr) {
    cout << "Error parsing " << s << endl;
    return 0;
  }

  AST* result = eval(expr);
  if (!result) {
    cout << "Error evaluating " << expr->lisp_string() << endl;
    return 0;
  }

  cout << result->lisp_string() << endl;

}
