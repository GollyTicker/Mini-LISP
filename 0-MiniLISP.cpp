#include<iostream>
#include<map>
#include<exception>
using namespace std;

const bool debug = false;

#include "0-Parser.cpp"
#include "0-Eval.cpp"
// ^ being lazy and not using header files.

// default runs as repl via stdin and stdout.
// if first argument is "-e", then runs that expression, prints the result and terminates
int main(int k, char ** args) {

  string s;
  Env e = init_interpreter();

  bool single_expr = k >= 3 && string(args[1]) == "-e";
  bool repl = !single_expr;

  if (repl) cout << "MiniLISP REPL. type quit or exit to terminate repl." << endl;

  while (true) {
    /* 1. read input*/
    if (repl) {
      cout << "> ";
      getline(cin >> ws,s);
    }
    else {
      s = string(args[2]);
    }

    if (repl && (s.compare("quit") == 0 || s.compare("exit") == 0)) {
      break;
    }
    else {
      /* 2. evaluate it. */
      Env backup = Env(e);
      try {
        vector<pAST> results = interpret_file_string(s, e);
        // changes in environment are recorded in e.
        /* 3. print evaluated form. */
        for (pAST past : results) {
          cout << past->lisp_string() << endl;
        }
        if (single_expr) break;
      }
      catch (exception& excp) {
        cout << excp.what() << endl;
        cout << "Could not evaluate. " << endl;
        // revert changes done by expression
        e=backup;
      }
    }
  }

  return 0;
}
