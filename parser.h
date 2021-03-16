//----------------------------------------------------------------------
// NAME: Rie Durnil
// FILE: Parser.h
// DATE: March 2021
// DESC: Implementation of the parser for MyPL.
//----------------------------------------------------------------------

#ifndef PARSER_H
#define PARSER_H

#include "token.h"
#include "mypl_exception.h"
#include "ast.h"


class Parser
{
public:

  // create a new recursive descent parser
  Parser(const Lexer& program_lexer);

  // run the parser
  void parse(Program& node);
  
private:
  Lexer lexer;
  Token curr_token;
  
  // helper functions
  void advance();
  void eat(TokenType t, std::string err_msg);
  void error(std::string err_msg);
  bool is_operator(TokenType t);
  
  // recursive descent functions
  void tdecl(TypeDecl& node);
  void fdecl(FunDecl& node);
  void vdecls(std::list<VarDeclStmt*>& list);
  void params(std::list<FunDecl::FunParam>& list);
  void dtype();
  void stmts(std::list<Stmt*>& list);
  bool stmt(std::list<Stmt*>& list);
  void vdecl_stmt(VarDeclStmt& node);
  void assign_stmt(AssignStmt& node);
  void lvalue(AssignStmt& node);
  void cond_stmt(IfStmt& node);
  void condt(IfStmt& node);
  void while_stmt(WhileStmt& node);
  void for_stmt(ForStmt& node);
  void call_expr(CallExpr& node);
  void args(CallExpr& node);
  void exit_stmt(ReturnStmt& node);
  void expr(Expr& node);
  void rvalue(SimpleTerm& node);
  void pval();
  void idrval(IDRValue & node);
};


// constructor
Parser::Parser(const Lexer& program_lexer) : lexer(program_lexer)
{
}


// Helper functions

void Parser::advance()
{
  curr_token = lexer.next_token();
}


void Parser::eat(TokenType t, std::string err_msg)
{
  if (curr_token.type() == t)
    advance();
  else
    error(err_msg);
}


void Parser::error(std::string err_msg)
{
  std::string s = err_msg + "found '" + curr_token.lexeme() + "'";
  int line = curr_token.line();
  int col = curr_token.column();
  throw MyPLException(SYNTAX, s, line, col);
}


bool Parser::is_operator(TokenType t)
{
  return t == PLUS or t == MINUS or t == DIVIDE or t == MULTIPLY or
    t == MODULO or t == AND or t == OR or t == EQUAL or t == LESS or
    t == GREATER or t == LESS_EQUAL or t == GREATER_EQUAL or t == NOT_EQUAL;
}


// Recursive-decent functions

void Parser::parse(Program& node)
{
  advance();
  while (curr_token.type() != EOS) {
    if (curr_token.type() == TYPE) {
      TypeDecl* t = new TypeDecl;
      tdecl(*t);
      node.decls.push_back(t);
    } else {
      FunDecl* f = new FunDecl;
      fdecl(*f);
      node.decls.push_back(f);
    }
  }
  eat(EOS, "expecting end-of-file ");
}


void Parser::tdecl(TypeDecl& node)
{
  advance();
  node.id = curr_token;
  eat(ID, "expecting identifier ");
  //create list for vdecls
  std::list<VarDeclStmt*> v;
  vdecls(v);
  node.vdecls = v;
  eat(END, "expecting end ");
}


void Parser::fdecl(FunDecl& node) {
  eat(FUN, "expecting fun ");
  node.return_type = curr_token;
  //will error here if not
  if (curr_token.type() != NIL)
    dtype();
  else 
    advance();
  node.id = curr_token;
  eat(ID, "expecting identifier ");
  eat(LPAREN, "expecting '(' ");
  //create list for params
  std::list<FunDecl::FunParam> f;
  params(f);
  node.params = f;
  eat(RPAREN, "expecting ')' ");
  //create list for stmts
  std::list<Stmt*> s;
  stmts(s);
  node.stmts = s;
  eat(END, "expecting end ");
}

void Parser::vdecls(std::list<VarDeclStmt*>& list) {
  if (curr_token.type() == VAR) {
    VarDeclStmt* v = new VarDeclStmt;
    vdecl_stmt(*v);
    list.push_back(v);
    vdecls(list);
  }
}

void Parser::params(std::list<FunDecl::FunParam>& list) {
  //ID COLON <dtype> ( COMMA ID COLON <dtype> )* | empty
  if (curr_token.type() == ID) {
    FunDecl::FunParam* node = new FunDecl::FunParam;
    node->id = curr_token;
    advance();
    eat(COLON, "expecting ':' ");
    node->type = curr_token;
    //will error in dtype() if not
    dtype();
    list.push_back(*node);
    while (curr_token.type() == COMMA) {
      advance();
      FunDecl::FunParam* f = new FunDecl::FunParam;
      f->id = curr_token;
      eat(ID, "expecting identifier ");
      eat(COLON, "expecting ':' ");
      f->type = curr_token;
      dtype();
      list.push_back(*f);
    }
  }
}

void Parser::dtype() {
  //INT_TYPE | DOUBLE_TYPE | BOOL_TYPE | CHAR_TYPE | STRING_TYPE | ID
  if (curr_token.type() == INT_TYPE) 
    advance();
  else if (curr_token.type() == DOUBLE_TYPE)
    advance();
  else if (curr_token.type() == BOOL_TYPE)
    advance();
  else if (curr_token.type() == CHAR_TYPE) 
    advance();
  else if (curr_token.type() == STRING_TYPE)
    advance();
  else if (curr_token.type() == ID)
    advance();
  else {
    error("incorrect type ");
  }
}

void Parser::stmts(std::list<Stmt*>& list) {
  // <stmt> <stmts> | empty
  bool more = true;
  while (more) {
    more = stmt(list);
  }
}

bool Parser::stmt(std::list<Stmt*>& list) {
  // <vdecl_stmt> | <assign_stmt> | <cond_stmt> | <while_stmt> | <for_stmt> |
  // <call_expr> | <exit_stmt>
  if (curr_token.type() == VAR) {
    VarDeclStmt* v = new VarDeclStmt;
    vdecl_stmt(*v);
    list.push_back(v);
  } else if (curr_token.type() == ID) {
    //both assign and call_expr
    Token id = curr_token;
    advance();
    if (curr_token.type() == DOT || curr_token.type() == ASSIGN) {
      AssignStmt* a = new AssignStmt;
      a->lvalue_list.push_back(id);
      assign_stmt(*a);
      list.push_back(a);
    } else if (curr_token.type() == LPAREN) {
      CallExpr* call = new CallExpr;
      call->function_id = id;
      call_expr(*call);
      list.push_back(call);
    } else 
      error("Invalid statment ");
  } else if (curr_token.type() == IF) {
    IfStmt* cond = new IfStmt;
    cond_stmt(*cond);
    list.push_back(cond);
  } else if (curr_token.type() == WHILE) {
    WhileStmt* w = new WhileStmt;
    while_stmt(*w);
    list.push_back(w);
  } else if (curr_token.type() == FOR) {
    ForStmt* f = new ForStmt;
    for_stmt(*f);
    list.push_back(f);
  } else if (curr_token.type() == RETURN) {
    ReturnStmt* exit = new ReturnStmt;
    exit_stmt(*exit);
    list.push_back(exit);
  } else 
    return false;
  return true;
}

void Parser::vdecl_stmt(VarDeclStmt& node) {
  // VAR ID ( ( COLON <dtype> ) | empty ) ASSIGN <expr>
  eat(VAR, "expecting variable ");
  node.id = curr_token;
  eat(ID, "expecting identifier ");
  if (curr_token.type() == COLON) {
    advance();
    Token* t = new Token;
    *t = curr_token;
    node.type = t;
    dtype();
  }
  eat(ASSIGN, "expecting '=' ");
  Expr* e = new Expr;
  expr(*e);
  node.expr = e;
}

void Parser::assign_stmt(AssignStmt& node) {
  // <lvalue> ASSIGN <expr>
  // already advanced from first ID
  if (curr_token.type() == DOT)
    lvalue(node);
  eat(ASSIGN, "expecting '=' ");
  Expr* e = new Expr;
  expr(*e);
  node.expr = e;
}

void Parser::lvalue(AssignStmt& node) {
  //ID ( DOT ID )*
  //skip first ID, taken care of before call
  while (curr_token.type() == DOT) {
    advance();
    node.lvalue_list.push_back(curr_token);
    eat(ID, "expecting identifier ");
  }
}

void Parser::cond_stmt(IfStmt& node) {
  // IF <expr> THEN <stmts> <condt> END
  eat(IF, "expecting if ");
  Expr* e = new Expr;
  expr(*e);
  BasicIf* b = new BasicIf;
  b->expr = e;
  eat(THEN, "expecting then ");
  std::list<Stmt*> s;
  stmts(s);
  b->stmts = s;
  node.if_part = b;
  condt(node);
  eat(END, "expecting end ");
}

void Parser::condt(IfStmt& node) {
  //ELSEIF <expr> THEN <stmts> <condt> | ELSE <stmts> | empty
  if (curr_token.type() == ELSEIF) {
    advance();
    BasicIf* b = new BasicIf;
    Expr* e = new Expr;
    expr(*e);
    b->expr = e;
    eat(THEN, "expecting then ");
    std::list<Stmt*> s;
    stmts(s);
    b->stmts = s;
    node.else_ifs.push_back(b);
    condt(node);
  } else if (curr_token.type() == ELSE) {
    advance();
    std::list<Stmt*> s2;
    stmts(s2);
    node.body_stmts = s2;
  }
}

void Parser::while_stmt(WhileStmt& node) {
  //WHILE <expr> DO <stmts> END
  eat(WHILE, "expecting while ");
  Expr* e = new Expr;
  expr(*e);
  node.expr = e;
  eat(DO, "expecting do ");
  std::list<Stmt*> s;
  stmts(s);
  node.stmts = s;
  eat(END, "expecting end ");
}

void Parser::for_stmt(ForStmt& node) {
  //FOR ID ASSIGN <expr> TO <expr> DO <stmts> END
  eat(FOR, "expecting for ");
  node.var_id = curr_token;
  eat(ID, "expecting identifier ");
  eat(ASSIGN, "expecting '=' ");
  Expr* e1 = new Expr;
  expr(*e1);
  node.start = e1;
  eat(TO, "expecting to ");
  Expr* e2 = new Expr;
  expr(*e2);
  node.end = e2;
  eat(DO, "expecting do ");
  std::list<Stmt*> s;
  stmts(s);
  node.stmts = s;
  eat(END, "expecting end ");
}

void Parser::call_expr(CallExpr& node) {
  //ID LPAREN <args> RPAREN
  //skipping first ID, taken care of in all calls
  eat(LPAREN, "expecting '(' ");
  args(node);
  eat(RPAREN, "expecting ')' ");
}

void Parser::args(CallExpr& node) {
  //<expr> ( COMMA <expr> )* | empty
  //curr_token would be RPAREN in empty case
  if (curr_token.type() != RPAREN) {
    Expr* e = new Expr;
    expr(*e);
    node.arg_list.push_back(e);
    while (curr_token.type() == COMMA) {
      advance();
      Expr* e = new Expr;
      expr(*e);
      node.arg_list.push_back(e);
    }
  }
}

void Parser::exit_stmt(ReturnStmt& node) {
  //RETURN <expr>
  eat(RETURN, "expecting return ");
  Expr* e = new Expr;
  expr(*e);
  node.expr = e;
}

void Parser::expr(Expr& node) {
  //(<rvalue> | NOT <expr> | LPAREN <expr> RPAREN ) (<operator> <expr> | empty)
  if (curr_token.type() == NOT) {
    ComplexTerm* c = new ComplexTerm;
    node.negated = true;
    advance();
    Expr* e = new Expr;
    expr(*e);
    c->expr = e;
    node.first = c;
  } else if (curr_token.type() == LPAREN) {
    ComplexTerm* c = new ComplexTerm;
    node.negated = false;
    advance();
    Expr* e = new Expr;
    expr(*e);
    c->expr = e;
    node.first = c;
    eat(RPAREN, "expecting ')' ");
  } else {
    //if it's not an rvalue, it will error in rvalue()
    SimpleTerm* t = new SimpleTerm;
    rvalue(*t);
    node.first = t;
  }
  
  if (is_operator(curr_token.type())) {
    Token* oper = new Token;
    *oper = curr_token;
    node.op = oper;
    advance();
    Expr* e = new Expr;
    expr(*e);
    node.rest = e;
  }
}

void Parser::rvalue(SimpleTerm& node) {
  //<pval> | NIL | NEW ID | <idrval> | <call_expr> | NEG <expr>
  if (curr_token.type() == NIL) {
    SimpleRValue* srv = new SimpleRValue;
    srv->value = curr_token;
    advance();
    node.rvalue = srv;
  } else if (curr_token.type() == NEW) {
    advance();
    NewRValue* n = new NewRValue;
    n->type_id = curr_token;
    eat(ID, "expecting identifier ");
    node.rvalue = n;
  } else if (curr_token.type() == ID) {
    Token curr = curr_token;
    advance();
    if (curr_token.type() == LPAREN) {
      CallExpr* call = new CallExpr;
      call->function_id = curr;
      call_expr(*call);
      node.rvalue = call;
    } else {
      IDRValue* idr = new IDRValue;
      idr->path.push_back(curr);
      idrval(*idr);
      node.rvalue = idr;
    }
  } else if (curr_token.type() == NEG) {
    NegatedRValue* nrv = new NegatedRValue;
    advance();
    Expr* e = new Expr;
    expr(*e);
    nrv->expr = e;
    node.rvalue = nrv;
  } else {
    //will error in pval() if not
    SimpleRValue* srv = new SimpleRValue;
    srv->value = curr_token;
    pval();
    node.rvalue = srv;
  }
}

void Parser::pval() {
  //INT_VAL | DOUBLE_VAL | BOOL_VAL | CHAR_VAL | STRING_VAL
  if (curr_token.type() == INT_VAL)
    advance();
  else if (curr_token.type() == DOUBLE_VAL)
    advance();
  else if (curr_token.type() == BOOL_VAL)
    advance();
  else if (curr_token.type() == CHAR_VAL)
    advance();
  else if (curr_token.type() == STRING_VAL)
    advance();
  else 
    error("Invalid value ");
} 

void Parser::idrval(IDRValue& node) {
  //only called in rvalue, so skipping first ID
  while (curr_token.type() == DOT) {
    advance();
    node.path.push_back(curr_token);
    eat(ID, "expecting identifier ");
  }
}

#endif
