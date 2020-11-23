#include<iostream>
#include<map>
using namespace std;

const bool debug = false;

#include "Parser.cpp"
#include "Eval.cpp"
// ^ being lazy and not using header files.


int main(int k, char ** args){
  string s;
  getline(cin >> ws,s);

  /* 1. parse input into abstract semantic tree: AST*/
  pAST expr = parse_full(s);
  if (!expr) {
    cout << "Error parsing " << s << endl;
    return 0;
  }

  /* 2. evaluate it. */
  setup_interpreter();
  pAST result = Eval(expr);
  if (!result) {
    cout << "Error evaluating " << expr->lisp_string() << endl;
    return 0;
  }

  /* 3. print evaluated form. */
  cout << result->lisp_string() << endl;
}
