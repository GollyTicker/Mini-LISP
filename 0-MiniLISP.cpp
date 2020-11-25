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
    if (repl) {
      cout << "> ";
      getline(cin >> ws,s);
    }
    else {
      s = string(args[2]); // TODO: add try catches for REPL workflow
    }

    if (repl && (s.compare("quit") == 0 || s.compare("exit") == 0)) {
      break;
    }
    else {
      /* 1. parse input into abstract semantic tree: AST*/
      pAST expr = parse_full(s);
      if (!expr) {
        cout << "Could not parse: " << s << endl;
      }
      else {
        /* 2. evaluate it. */
        Env backup = Env(e);
        pAST result = eval(expr, e);
        // changes in environment are recorded in e.
        if (!result) {
          cout << "Could not evaluate: " << expr->lisp_string() << endl;
          // revert changes done by expression
          e = backup;
        }
        else {
          /* 3. print evaluated form. */
          cout << result->lisp_string() << endl;
          if (single_expr) break;
        }
      }
    }
  }

  return 0;
}
