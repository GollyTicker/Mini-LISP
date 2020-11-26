/* RECURSIVE DESCENT PARSING */
/* monad abstractions for these parsers would be great :) */

/*
Parsing rules:

file = file-no-comments where file-no-comments has all single-line {comment} removed
file-no-comment = "" | expr ws file-no-comment

expr = atom | list | quote expr
quote = "'"
atom = { char x where x not in "\n()'" }*
list = "(" list-rec
list-rec = ")" | expr ws list-rec

comment = ";" { char x where x != '\n' }*
ws = " " | "\n"
*/

/* TODO: perhaps make contextes mutable and passed by reference?
then we can always use the same context.
TODO: perhaps pass the input string as reference instead of directly? */

#include<set>
#include<vector>
#include<variant>
#include<sstream>

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

template<typename T, typename S>
Parsed<T> result_typed(Parsed<S> p) {
  optional<Result<S> > r = get_opt(p);
  if (!r) return get<ParseError>(p);
  Result<T> converted = Result<T>(r.value().val,r.value().c);
  return converted;
}

// utilities for parsing. defined at bottom
Parsed<string> character(char chr, Ctx c);
Ctx whitespace(Ctx c);

// parsing functions for each expression.
Parsed<List> list_expr(Ctx c);
Parsed<Atom> atom_expr(Ctx c);
Parsed<AST> lisp_expr(Ctx c);

// parses full string into a lisp expression or fails with a NULL pointer.
pAST parse_full(string s) {
  if (debug) cout << "parse_full("<<s<<")"<< endl;

  Ctx c(s);
  Parsed<AST> p = lisp_expr(c);
  optional<Result<AST> > r = get_opt(p);
  if (r) {
    Result<AST> res = r.value();
    Ctx c2 = whitespace(res.c);
    if (c2.notEOF()) {
      ParseError e = ParseError("Finished parsing EXPR, but stream continues with",c2);
      cout << "Error: " << e.msg << endl;
      return NULL;
    }
    else { // reached end of stream
      return res.val;
    }
  }
  else {
    ParseError err = get<ParseError>(p);
    cout << "Error: " << err.msg << endl;
    return NULL;
  }
}

/* ignores current position in Ctx */
Ctx remove_single_line_comments(Ctx c) {
  ostringstream ss;
  for (;c.notEOF(); c++) {
    if (c.get() == ';') { // comment start
      while(c.notEOF() && c.get() != '\n') c++;
    }
    else ss << string(1,c.get());
  }
  return Ctx(ss.str());
}

Parsed<Codefile> code_file(Ctx c1) {
  if (debug) cout << "code_file("<<c1.i()<<")"<< endl;
  /* preprocess code_file to remove single line comments. */
  Ctx c2 = remove_single_line_comments(c1);
  vector<pAST> stmts(0,pAST(NULL));
  c2 = whitespace(c2);
  while (c2.notEOF()) {
    Parsed<AST> pexpr = lisp_expr(c2);
    optional<Result<AST> > rexpr = get_opt(pexpr);
    if (!rexpr) return get<ParseError>(pexpr);
    stmts.push_back(rexpr.value().val);
    c2 = rexpr.value().c;
    c2 = whitespace(c2);
  }
  return Result(codefile(vector<pAST> (stmts)),c2);
}

/* parse a lisp_expression starting at index i1.
returns NULL is it fails*/
Parsed<AST> lisp_expr(Ctx c1) {
  if (debug) cout << "lisp_expr("<<c1.i()<<")" << endl;
  Ctx c2 = whitespace(c1);
  if (c2.notEOF()) {
    /* choose option based on lookahead */
    if (c2.get() == '\'') { /* special syntax for quote */
      c2++;
      Parsed<AST> p = lisp_expr(c2);
      optional<Result<AST> > r = get_opt(p);
      if (!r) return p;
      Result<AST> res = r.value();
      return Result<AST>(cons(at("quote"),cons(res.val,nl)),res.c);
    }
    else if (c2.get() == '(') {
      return result_typed<AST>(list_expr(c2));
    }
    else {
      return result_typed<AST>(atom_expr(c2));
    }
  }
  else return ParseError("Expecting EXPR, but got",c2);
}

set<char> atom_breaks{' ', ')', '(', '\n', '\''};

Parsed<Atom> atom_expr(Ctx c) {
  if (debug) cout << "atom_expr("<<c.i()<<")" << endl;
  int i0 = c.i();
  /*read atom name until a character in atom_breaks is encountered */
  while (c.notEOF()) {
    if (c.i() == i0 && atom_breaks.count(c.get())>=1) {
      return ParseError("Expecting ATOM, but got",c);
    }
    else if (atom_breaks.count(c.get())>=1) { break; }
    else c++;
  }
  /*convert from i0 until i (excluding) to atom_name*/
  string atomname = c.s.substr(i0,c.i()-i0);
  return Result(at(atomname),c);
}

Parsed<List> list_rec_expr(Ctx c) {
  if (debug) cout << "list_rec_expr("<<c.i()<<")" << endl;
  if (c.notEOF()) {
    char lookahead = c.get();
    if (lookahead == ')') { /* nl: end of list */
      c++; return Result(nl,c);
    }
    else { /* cons: recursive case of list */
      pAST elem;
      int iWS;
      Parsed<AST> pelem = lisp_expr(c);
      optional<Result<AST> > relem = get_opt(pelem);
      if (!relem) return get<ParseError>(pelem);
      // r is non-empty
      Ctx iRest = whitespace(relem.value().c);

      Parsed<List> prec = list_rec_expr(iRest);
      optional<Result<List> > rrec = get_opt(prec);
      if (!rrec) return prec;
      // rrec is non-empty
      pList tail = rrec.value().val;
      return Result(cons(relem.value().val,tail),rrec.value().c);
    }
  }
  else return ParseError("Expected EXPR or end of list, but got",c);
}

Parsed<List> list_expr(Ctx c1) {
  if (debug) cout << "list_expr("<<c1.i()<<")" << endl;
  Parsed<string> pc = character('(',c1);
  optional<Result<string> > rc = get_opt(pc);
  if (!rc) return get<ParseError>(pc);
  Ctx c2 = rc.value().c;
  if (c2.notEOF()) {
    Ctx crest = whitespace(c2);
    return list_rec_expr(crest);
  }
  else {
    return ParseError("Expecting EXPR or end of list, but got",c2);
  }
}

/* UTILITIES */
Ctx whitespace(Ctx c) {
  if (debug) cout << "whitespace("<<c.i()<<")"<< endl;
  while(c.notEOF() && (c.get() == ' ' || c.get() == '\n') ) c++;
  return c;
}

Parsed<string> character(char chr, Ctx c) {
  if (debug) cout << "character("<<chr<<","<<c.i()<<")"<< endl;
  if (c.notEOF() && c.get() == chr) {
    c++;
    return make_string_result(string(1,chr),c);
  }
  else {
    ostringstream ss;
    ss << "Expected " << string(1,chr) << ", but got";
    return ParseError(ss.str(),c);
  }
}
