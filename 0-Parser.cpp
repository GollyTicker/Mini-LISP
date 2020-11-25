/* RECURSIVE DESCENT PARSING */
/* monad abstractions for these parsers would be great :) */

/*
Parsing rules:

file = file-no-comments where file-no-comments has all single-line {comment} removed
file-no-comment = "" | expr ws expr

expr = atom | list | quote expr
quote = "'"
atom = { char x where x not in "\n()'" }*
list = "(" list-rec
list-rec = ")" | expr ws list-rec

comment = ";" { char x where x != '\n' }*
ws = " " | "\n"
*/

#include<set>
#include<vector>
#include <variant>
#include <sstream>

#include "0-AST.cpp"

/* parsing types: Context, Result and Error.
A parser for type T is a function Ctx => Parsed<T> where T <= AST */
class Ctx {
  private:
    /* invariant: index i is valid in string s unless s="" */
    int I;

  public:
    string s;

    Ctx(string str) { s=str; I=0; }

    int length() { return s.length(); }
    int i() { return I; }
    char get() { return s[I]; }
    char operator[](int k) { return s[k]; }
    bool check_bounds() { return 0 <= I && I < s.length(); }
    bool notEOF() { return I < length(); }

    void operator+=(int n) { I+=n; }
    void operator++(int) { operator+=(1); }

    string format_position() {
      // count to current line and position in line.
      int pos = 1;
      int line = 1;
      for (int j = 0; j < length(); j++) {
        if (operator[](j) == '\n') { pos=1; line++; }
        else pos++;
      }
      ostringstream ss;
      string chr = I < length() ? string(1,get()) : "<end of file>";
      ss << chr << " at line " << line << ", pos " << pos;
      return ss.str();
    }
};

template<typename T>
struct Result {
  Ctx c = Ctx(""); // new context after parsing

  shared_ptr<T> val;
  Result(shared_ptr<T> v, Ctx ctx) {
    val = v;
    c = ctx;
  }
};

Result<string> make_string_result(string s, Ctx c) {
  shared_ptr<string> sp = make_shared<string>(s);
  return Result(sp,c);
}

struct ParseError {
  string msg;
  ParseError(string m, Ctx c) {
    ostringstream ss;
    ss << m << " " << c.format_position() << ".";
    msg = ss.str();
  }
};

template<typename T>
using Parsed = variant<Result<T>,ParseError>;

template<typename T>
optional<Result<T> > get_opt(Parsed<T> p) {
  return holds_alternative<Result<T> >(p) ? get<Result<T> >(p) : optional<Result<T> >();
}

template<typename T>
using Parser = Parsed<T> (*)(Ctx);

// sequence two parsers consecutively
/* PROBLEM: shared ptr does't distribute well into pair...*/
template<typename T1, typename T2>
Parser<T2> sequence2_right(Parser<T1> p1, Parser<T2> p2) {
  Parsed<T2> (*func)(Ctx) = [&p1, &p2](Ctx c) -> Parsed<T2> {
    Parsed<T1> pr1 = p1(c);
    optional<Result<T1> > r1 = get_opt<T1>(pr1);
    if (r1) {
      return p2(r1.value().c);
    }
    else {
      return (Parsed<T2>) get<ParseError>(pr1);
    }
  };

  return func;
}

// utilities for parsing. defined at bottom
int character(char c, string s, int i);
int whitespace(string s, int i);
char lookahead(string s, int i);
Parsed<string> whitespaceN(Ctx c);

// parsing functions for each expression.
pair<pAST,int> lisp_expr(string s, int i);
pair<pList,int> list_expr(string s, int i);
Parsed<Atom> atom_expr(Ctx c);

#define NO_RESULT(T) make_pair((T)NULL,-1)

// parses full string into a lisp expression or fails with a NULL pointer.
pAST parse_full(string s) {
  if (debug) cout << "parse_full("<<s<<")"<< endl;
  pAST expr;
  int iEnd;
  tie(expr,iEnd) = lisp_expr(s,0);
  if (iEnd != -1) {
    int iFin = whitespace(s,iEnd);
    if (iFin == s.length()) return expr;
    else {
      cout << "Finished parsing EXPR, but stream continues with " << s[iFin] << " at " << iFin << endl;
      return NULL;
    }
  }
  else return NULL;
}

/* parse a lisp_expression starting at index i1.
returns NULL is it fails*/
pair<pAST,int> lisp_expr(string s, int i1) {
  if (debug) cout << "lisp_expr("<<i1<<")" << endl;
  int i2 = whitespace(s,i1);
  if (i2 < s.length()) {
    switch (lookahead(s,i2)) {
      case '\'': { /* special syntax for quote */
          pAST quoted;
          int iNext;
          tie(quoted,iNext) = lisp_expr(s,i2+1);
          pAST res = cons(at("quote"), cons(quoted, nl));
          return make_pair(res,iNext);
          break;
        }
      case '(' : { /* list */
          return list_expr(s,i2);
          break;
        }
      default  : { /* atom */
          Ctx c = Ctx(s);
          c += i2;
          Parsed<Atom> p = atom_expr(c);
          optional<Result<Atom> > r = get_opt<Atom>(p);
          return r ? make_pair(r.value().val,r.value().c.i()) : NO_RESULT(pAtom); // TODO: return p here
          break;
        }
    }
  }
  else {
    cout << "Expecting EXPR, but found end of stream at " << i2 << endl;
    return NO_RESULT(pAST);
  }
}

set<char> atom_breaks{' ', ')', '(', '\''};

Parsed<Atom> atom_expr(Ctx c) {
  if (debug) cout << "atom_expr("<<c.i()<<")" << endl;
  int i0 = c.i();
  /*read atom name until a character in atom_breaks is encountered */
  while (c.notEOF()) {
    if (c.i() == i0 && atom_breaks.count(c.get())>=1) {
      return ParseError("Expecting ATOM, but found ",c);
    }
    else if (atom_breaks.count(c.get())>=1) { break; }
    else c++;
  }
  /*convert from i0 until i (excluding) to atom_name*/
  string atomname = c.s.substr(i0,c.i()-i0);
  return Result(at(atomname),c);
}

Parsed<List> list_rec_exprN(Ctx c) {
  if (debug) cout << "list_rec_expr("<<c.i()<<")" << endl;
  if (c.notEOF()) {
    char lookahead = c.get();
    if (lookahead == ')') { /* nl: end of list */
      c++;
      return Result(nl,c);
    }
    else {
      pAST elem;
      pList tail;
      int iWS,iRest,iEnd;
      tie(elem,iWS) = lisp_expr(c.s,c.i());

      Ctx c2 = Ctx(c.s);
      c2+=iWS;

      Parsed<List> sq = sequence2_right(whitespaceN,list_rec_exprN)(c2);
      // cannot convert to pointer functions
      // due to the way, lambda's are implemented
      // mabdas can only be static...
      // what now? ...
      // => to manual checking for indices and go on.
      // TODO: continue here
      return ParseError("TODO",c);
    }
  }
  else return ParseError("Expecting EXPR or end of list, but found", c);
}

pair<pList,int> list_rec_expr(string s, int i) {
  if (debug) cout << "list_rec_expr("<<i<<")" << endl;
  if (i < s.length()) {
    char la = lookahead(s,i);
    if (la == ')') { /* nl: end of list */
      return make_pair(nl,i+1);
    }
    else { /* cons: recursive case of list */
      pAST elem;
      pList tail;
      int iWS,iRest,iEnd;
      tie(elem,iWS) = lisp_expr(s,i);
      iRest = whitespace(s,iWS);
      tie(tail,iEnd) = list_rec_expr(s,iRest);
      return make_pair(cons(elem,tail),iEnd);
    }
  }
  else {
    cout << "Expecting EXPR or end of list, but found end of stream at " << i << endl;
    return NO_RESULT(pList);
  }
}

/*
Pared<List> list_expr(Ctx c) {
  if (debug) cout << "list_expr("<<c.i()<<")" << endl;
  return NULL;
}*/

pair<pList,int> list_expr(string s, int i1) {
  if (debug) cout << "list_expr("<<i1<<")" << endl;
  int iElem = character('(',s,i1);
  if (iElem < s.length() && iElem != -1) {
    int iRest = whitespace(s,iElem);
    return list_rec_expr(s,iRest);
  }
  else {
    cout << "Expecting EXPR or end of list, but found end of stream at" << iElem << endl;
    return NO_RESULT(pList);
  }
}

/* UTILITIES */

// parses single character c and returns new index or fails with -1
Parsed<string> curlyBraceOpen(Ctx c) {
  if (debug) cout << "curlyBraceOpen("<<c.i()<<")"<< endl;
  if (c.notEOF() && c.get() == '(') {
    c++;
    return make_string_result("(",c);
  }
  else {
    ostringstream ss;
    ss << "Expected '(' but got";
    return ParseError(ss.str(), c);
  }
}

Parsed<string> whitespaceN(Ctx c) {
  if (debug) cout << "whitespace("<<c.i()<<")"<< endl;
  while(c.notEOF() && (c.get() == ' ' || c.get() == '\n') ) c++;
  return make_string_result("<irrelevant>",c);
}

int character(char c, string s, int i) {
  if (debug) cout << "character("<<c<<","<<i<<")"<< endl;
  if (i < s.length() && s[i] == c) return i+1;
  else return -1; /* failure */
}

// parses and skips whitespaces
int whitespace(string s, int i) {
  if (debug) cout << "whitespace("<<i<<")"<< endl;
  while(i < s.length() && (s[i] == ' ' || s[i] == '\n') ) i++;
  return i;
}

char lookahead(string s, int i) {
  if (debug) cout << "lookahead("<<i<<") = " << s[i] << endl;
  return s[i];
}
