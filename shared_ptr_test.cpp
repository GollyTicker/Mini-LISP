#include<memory>
#include<iostream>

using namespace std;

struct AST {
  bool sub = false;
  virtual ~AST() = default; // destructor for shared pointers
};

struct List:AST {
  bool empty = true;
  bool sub = true;
  shared_ptr<int> head = NULL;
  shared_ptr<List> tail = NULL;
  List() {};
  List(shared_ptr<int>, shared_ptr<List>);
  virtual ~List() = default; // destructor for shared pointers
};

typedef shared_ptr<int> pint;
typedef shared_ptr<List> plist;

plist nl = plist(new List());

List::List(pint a, plist b) {
  empty = false;
  head = a;
  tail = b;
}

plist cons(pint a, plist b) {
  return plist(new List(a, b));
}

void rewrite_node(pint five, plist a) {
  plist b = cons(five,a);
}
/*
template <class A, class B>
shared_ptr<B> cast(shared_ptr<A> a) {
  return dynamic_pointer_cast>(a);
}*/

int main(int k, char** argv) {
  cout << nl.use_count() << endl;
  pint five = make_shared<int>(5);
  plist a = cons(five,nl);
  rewrite_node(five,a);
  cout << nl.use_count() << endl;

  shared_ptr<AST> ast = nl;
  cout << "sub: " << ast->sub << endl;
  shared_ptr<List> list = dynamic_pointer_cast<List>(ast);
  cout << "sub: " << list->sub << endl;
  shared_ptr<AST> null = NULL;
  shared_ptr<List> list2 = dynamic_pointer_cast<List>(null);
  cout << "list2? " << (list2?"1":"0") << endl;
  cout << "hello" << endl;
}
