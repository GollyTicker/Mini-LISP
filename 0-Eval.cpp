#include<map>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <streambuf>
#include <sstream>

typedef map<string,pAST> Env;
typedef pAST evalProc(pList, Env&);

// procedure that evaluates an expression using the environment.
// may change the environment.
pAST eval(pAST expr, Env& env);

#include "0-EvalPrimitives.cpp"

map< weak_ptr<AST>, shared_ptr<AST>, owner_less< weak_ptr<AST> > > eval_memoised;

int eval_runs = 0;

pAST eval(pAST expr, Env& env) {
  /* eval memoisation. 1 */
  weak_ptr<AST> w = expr;
  if (eval_memoised.count(w) >= 1) {
    return eval_memoised[w];
  }

  // every once in a while, clean unused weak pointers from
  // memoisation. similar to garbage collection.
  eval_runs++;
  if (eval_runs % 1000 == 0) {
    // remove pointers not used anymore and free up memory.
    for (auto i = eval_memoised.begin(), e = eval_memoised.end(); i != e;) {
      auto current = i++;
      if (current->first.expired()) {
        eval_memoised.erase(current);
      }
    }
  }


  pAST result = NULL;

  if (debug) cout << "eval: " << expr-> lisp_string() << endl;

  if (expr->is_atom()) {
    pAtom a = dynamic_pointer_cast<Atom>(expr);
    if (env.count(a->str) >= 1) result = env[a->str];
    else {
      cout << "Cannot eval atom " << a->lisp_string() << ". Missing definition in env." << endl;
      result = NULL;
    }
  }
  else { // expr is list
    pList xs = dynamic_pointer_cast<List>(expr);
    if(!xs->empty) {
      pAST hd = xs->head;
      // head = predefined atom => apply
      // head = atom => lookup atom and apply
      // head = lambda => apply lambda
      // otherwise => evaluate and repeat

      pAtom atom = dynamic_pointer_cast<Atom>(hd);
      if (atom) {
        if (predefs.count(atom->str) >= 1) {
          result = predefs[atom->str](xs->tail,env); // evaluation of args based on primitives
        }
        else if (env.count(atom->str) >= 1) {
          result = eval(cons(env[atom->str],xs->tail),env);
        }
        else {
          cout << "Cannot eval undefined atom in " << xs->lisp_string() << endl;
          result = NULL;
        }
      }
      else {
        pList fn = dynamic_pointer_cast<List>(hd);
        pAtom fname = dynamic_pointer_cast<Atom>(tryhead(fn));
        if (fname && fname->equals(at("lambda"))) { /*fn is lambda */
          pAST args = tryhead(trytail(fn));
          pAST body = tryhead(trytail(trytail(fn)));
          result = prim_lambda_apply(args,body,xs->tail,env);
        }
        else { /* evaluate fn and repeat */
          /*
          Evaluating the head first enables computed head before full evaluation:
          ((cond ('() '+) ('t 'car)) '(a b c))
          This isn't possible in some other lisp implementations
          */
          pAST evaledfn = eval(fn, env);
          result = eval(cons(evaledfn,xs->tail),env);
        }
      }
    }
    else { // empty list
      cout << "Cannot eval empty list " << xs->lisp_string() << "." << endl;
      result = NULL;
    }
  }

  /* eval memoisation. 2 */
  if (result) {
    weak_ptr<AST> wres = weak_ptr<AST>(result);
    eval_memoised.insert({w, result});
  }
  else eval_memoised.insert({w, pAST(NULL)});

  return eval_memoised[w];
}

string remove_backslash_newlines(string& s) {
  string r = "";
  for (int i=0;i<s.length()-1;i++){
    if (s[i]=='\\' && s[i+1]=='\n') i+=1; // skip newline
    else r += string{s[i]};
  }
  // ignore newline at end
  return r;
}

/* standard library definitions */
void add_standard_library(Env& env) {
  string std_lib = "1-standard-library.lisp";
  ifstream file(std_lib);
  string std_defs;
  if (file.is_open()) {
    std_defs = string((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
    std_defs = remove_backslash_newlines(std_defs);
    file.close();
  }
  else {
    cout << "Error: Couldn't open std library: " << std_lib << endl;
  }

  // read std definitions into environment
  stringstream std_lines(std_defs);
  string expr;
  while (getline(std_lines, expr)) {
    eval(parse_full(expr),env);
  }
}

Env init_interpreter() {
  add_primitives();
  Env e;
  add_standard_library(e);
  return e;
}
