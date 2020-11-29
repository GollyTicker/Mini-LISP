#include<iostream>
#include<map>
#include<exception>
using namespace std;

const bool debug = false;

#include "Parser.cpp"
#include "Eval.cpp"
// ^ being lazy and not using header files.

// default runs as repl via stdin and stdout.
// if first argument is "-e", then runs that expression, prints the result and terminates
// otherwise if the first argument is "-f",
// then the file with that name is loaded and executed. each expressions result is then printed
int main(int k, char ** args) {

  string s;
  Env e = init_interpreter();

  bool single_expr = k >= 3 && string(args[1]) == "-e";
  bool file_input = k >= 3 && string(args[1]) == "-f";
  bool repl = !single_expr && ! file_input;

  if (repl) cout << "MiniLISP REPL. type quit or exit to terminate repl." << endl;

  while (true) {
    /* 1. read input*/
    if (repl) {
      cout << "> ";
      getline(cin >> ws,s);
    }
    else if (single_expr) {
      s = string(args[2]);
    }
    else {
      ifstream file(args[2]);
      if (file.is_open()) {
        s = string((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
        file.close();
      }
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
        /* 3. print evaluated forms */
        for (pAST past : results) {
          cout << past->lisp_string() << endl;
        }
      }
      catch (exception& excp) {
        cout << excp.what() << endl;
        cout << "Could not evaluate. " << endl;
        // revert changes done by expression
        e=backup;
      }
      if (single_expr || file_input) break;
    }
  }

  return 0;
}
