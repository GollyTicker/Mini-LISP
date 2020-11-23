/* RECURSIVE DESCENT PARSING */
/* monad abstractions for these parsers would be great :) */

#include<set>

#include "0-AST.cpp"

// utilities for parsing. defined at bottom
int character(char c, string s, int i);
int whitespace(string s, int i);
char lookahead(string s, int i);

// parsing functions for each expression.
pair<pAST,int> lisp_expr(string s, int i);
pair<pList,int> list_expr(string s, int i);
pair<pAtom,int> atom_expr(string s, int i);

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
          return atom_expr(s,i2);
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

pair<pAtom,int> atom_expr(string s, int i) {
  if (debug) cout << "atom_expr("<<i<<")" << endl;
  int i0 = i;
  /*read atom name until a character in atom_breaks is encountered */
  while (i < s.length()) {
    if (i==i0 && atom_breaks.count(s[i])>=1) {
      cout << "Expecting ATOM, but found " << s[i] << " at " << i << endl;
      return NO_RESULT(pAtom);
    }
    else if (atom_breaks.count(s[i])>=1) {
      break;
    }
    else i++;
  }
  /*convert from i0 until i (excluding) to atom_name*/
  string atomname = s.substr(i0,i-i0);
  return make_pair(at(atomname),i);
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
int character(char c, string s, int i) {
  if (debug) cout << "character("<<c<<","<<i<<")"<< endl;
  if (i < s.length() && s[i] == c) return i+1;
  else return -1; /* failure */
}

// parses and skips whitespaces
int whitespace(string s, int i) {
  if (debug) cout << "whitespace("<<i<<")"<< endl;
  while(i < s.length() && s[i] == ' ') i++;
  return i;
}

char lookahead(string s, int i) {
  if (debug) cout << "lookahead("<<i<<") = " << s[i] << endl;
  return s[i];
}
