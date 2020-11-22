#include<map>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <streambuf>
#include <sstream>

typedef map<string,pAST> Env;
typedef pAST evalProc(pList, Env&);

/* perhaps we can make the evaluation
cleaner by adding a procedure type to AST? */

// procedure that evaluates an expression using the environment.
// may change the environment.
pAST eval(pAST expr, Env& env);

#include "EvalPrimitives.cpp"

pAST eval(pAST expr, Env& env) {
  if (debug) cout << "eval: " << expr-> lisp_string() << endl;

  if (expr->is_atom()) {
    pAtom a = dynamic_pointer_cast<Atom>(expr);
    if (env.count(a->str) >= 1) return env[a->str];
    else {
      cout << "Cannot eval atom " << a->lisp_string() << ". Missing definition in env." << endl;
      return NULL;
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
          // cout << "Applying primitive " << atom->str << " on env of size " << env.size() << endl;
          return predefs[atom->str](xs->tail,env); // evaluation of args based on primitives
        }
        else if (env.count(atom->str) >= 1) {
          //pList eagerArgs = evalList(xs->tail, env); // eager evaluation of args
          //cout << "eval "<< atom->str << " with args " << xs->tail->lisp_string() << endl;
          return eval(cons(env[atom->str],xs->tail),env);
        }
        else {
          cout << "Cannot eval undefined atom in " << xs->lisp_string() << endl;
          return NULL;
        }
      }
      pList fn = dynamic_pointer_cast<List>(hd);
      pAtom fname = dynamic_pointer_cast<Atom>(tryhead(fn));
      if (fname && fname->equals(at("lambda"))) { /*fn is lambda */
        pAST args = tryhead(trytail(fn));
        pAST body = tryhead(trytail(trytail(fn)));
        return prim_lambda_apply(args,body,xs->tail,env);
      }
      else { /* evaluate fn and repeat */
        /*
        Evaluating the head first enables
        computing head directly:
        ((cond ('() '+) ('t 'car)) '(a b c))
        This isn't possible in online lisp implementations.
        */
        pAST evaledfn = eval(fn, env);
        // defer evaluation of arguments to next eval
        return eval(cons(evaledfn,xs->tail),env);
      }
    }
    else { // empty list
      cout << "Cannot eval empty list " << xs->lisp_string() << "." << endl;
      return NULL;
    }
  }

  return NULL;
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
  /* TODO: define should be used as search and replace, but now, it's being stronger
    and it's evaluated first! What if we don't use the quotation?
    We could use non-quoted definitions for metaprogramming!*/
  string std_lib = "standard-library.lisp";
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


pAST Eval(pAST expr) {
  Env e;
  add_standard_library(e);
  return eval(expr, e);
}

/*
lambdas online:

;gnu clisp  2.49.60

(print "Hello, world!")

(print ((lambda () (cons '1 '(c d)))))

(print ((lambda (a b) (cons a b)) '1 '(c d)))

(print ((lambda (x) (cons x (cons x '()))) 'a) )

(print ((lambda (x) (cons x (cons x '()))) (lambda (f x) x)))

; (print (((lambda (x) (cons x (cons x '()))) (lambda (f x) x)) 'a) )

; (print (((lambda (x) (cons x (cons x '()))) (lambda (f x) x)) 'a) )
*/
