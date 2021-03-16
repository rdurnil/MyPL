//----------------------------------------------------------------------
// NAME: Rie Durnil
// FILE: Printer.h
// DATE: March 2020
// DESC: Pretty-prints the AST built up in the parser using the vistor
//       pattern.
//----------------------------------------------------------------------


#ifndef PRINTER_H
#define PRINTER_H

#include <iostream>
#include "ast.h"


class Printer : public Visitor
{
public:
  // constructor
  Printer(std::ostream& output_stream) : out(output_stream) {}

  // top-level
  void visit(Program& node);
  void visit(FunDecl& node);
  void visit(TypeDecl& node);
  // statements
  void visit(VarDeclStmt& node);
  void visit(AssignStmt& node);
  void visit(ReturnStmt& node);
  void visit(IfStmt& node);
  void visit(WhileStmt& node);
  void visit(ForStmt& node);
  // expressions
  void visit(Expr& node);
  void visit(SimpleTerm& node);
  void visit(ComplexTerm& node);
  // rvalues
  void visit(SimpleRValue& node);
  void visit(NewRValue& node);
  void visit(CallExpr& node);
  void visit(IDRValue& node);
  void visit(NegatedRValue& node);

private:
  std::ostream& out;
  int indent = 0;

  void inc_indent() {indent += 3;}
  void dec_indent() {indent -= 3;}
  std::string get_indent() {return std::string(indent, ' ');}

};


void Printer::visit(Program& node) {
  //goes through each function and type declaration in program
  while (!node.decls.empty()) {
    Decl* curr = node.decls.front();
    node.decls.pop_front();
    //calls accept on each Decl
    curr->accept(*this);
    std::cout << std::endl;
  }
}

void Printer::visit(FunDecl& node) {
  std::cout << "fun " << node.return_type.lexeme() << " " << node.id.lexeme() << "(";
  if (!node.params.empty()) {
    FunDecl::FunParam f = node.params.front();
    node.params.pop_front();
    std::cout << f.id.lexeme() << ": " << f.type.lexeme();
  }
  while (!node.params.empty()) {
    FunDecl::FunParam f = node.params.front();
    node.params.pop_front();
    std::cout << ", " << f.id.lexeme() << ": " << f.type.lexeme();
  }
  std::cout << ")";
  //increase indent for statements
  inc_indent();
  while (!node.stmts.empty()) {
    std::cout << std::endl << get_indent();
    Stmt* s = node.stmts.front();
    node.stmts.pop_front();
    //calls accept on each Stmt
    s->accept(*this);
  }
  //decrease indent
  dec_indent();
  std::cout << std::endl << "end" << std::endl;
}

void Printer::visit(TypeDecl& node) {
  std::cout << "type " << node.id.lexeme();
  inc_indent();
  while (!node.vdecls.empty()) {
    std::cout << std::endl << get_indent();
    VarDeclStmt* v = node.vdecls.front();
    node.vdecls.pop_front();
    //calls accept on each VarDeclStmt
    v->accept(*this);
  }
  dec_indent();
  std::cout << std::endl << "end" << std::endl;
}

void Printer::visit(VarDeclStmt& node) {
  std::cout << "var " << node.id.lexeme();
  //optional type
  if (node.type != nullptr)
    std::cout << ": " << node.type->lexeme();
  //optional expression
  if (node.expr != nullptr) {
    std::cout << " = ";
    //calls accept on the expr
    node.expr->accept(*this);
  }
}

void Printer::visit(AssignStmt& node) {
  while (!node.lvalue_list.empty()) {
    Token t = node.lvalue_list.front();
    node.lvalue_list.pop_front();
    std::cout << t.lexeme();
    //output . until the last one
    if (!node.lvalue_list.empty())
      std::cout << ".";
  }
  std::cout << " = ";
  //call accept on expr
  node.expr->accept(*this);
}

void Printer::visit(ReturnStmt& node) {
  std::cout << "return ";
  //call accept on the expr
  node.expr->accept(*this);
}

void Printer::visit(IfStmt& node) {
  std::cout << "if ";
  //call accept on if_part
  node.if_part->expr->accept(*this);
  std::cout << " then";
  //increase indent
  inc_indent();
  while (!node.if_part->stmts.empty()) {
    std::cout << std::endl << get_indent();
    Stmt* s = node.if_part->stmts.front();
    node.if_part->stmts.pop_front();
    //call accept on stmts
    s->accept(*this);
  }
  //decrease indent for optional elseifs and elses
  dec_indent();
  while (!node.else_ifs.empty()) {
    std::cout << std::endl << get_indent() << "elseif ";
    BasicIf* b = node.else_ifs.front();
    node.else_ifs.pop_front();
    //call accept on BasicIfs expr
    b->expr->accept(*this);
    std::cout << " then";
    //increase indent
    inc_indent();
    while (!b->stmts.empty()) {
      std::cout << std::endl << get_indent();
      Stmt* s = b->stmts.front();
      b->stmts.pop_front();
      //call accept on each stmt
      s->accept(*this);
    }  
    //decrease indent for optional else
    dec_indent();
  }
  if (!node.body_stmts.empty()) {
    std::cout << std::endl << get_indent() << "else";
    //increase indent
    inc_indent();
    while (!node.body_stmts.empty()) {
      std::cout << std::endl << get_indent();
      Stmt* s = node.body_stmts.front();
      node.body_stmts.pop_front();
      //call accept on each stmt
      s->accept(*this);
    }
    //decrease indent
    dec_indent();
  }
  std::cout << std::endl << get_indent() << "end";
}

void Printer::visit(WhileStmt& node) {
  std::cout << "while ";
  //call accept on expr
  node.expr->accept(*this);
  std::cout << " do";
  //increase indent for stmts
  inc_indent();
  while (!node.stmts.empty()) {
    std::cout << std::endl << get_indent();
    Stmt* s = node.stmts.front();
    node.stmts.pop_front();
    //call accept on each stmt
    s->accept(*this);
  }
  //decrease indent
  dec_indent();
  std::cout << std::endl << get_indent() << "end";
}

void Printer::visit(ForStmt& node) {
  std::cout << "for " << node.var_id.lexeme() << " = ";
  //call accept on start expr
  node.start->accept(*this);
  std::cout << " to ";
  //call accept on end expr
  node.end->accept(*this);
  std::cout << " do";
  //increase indent
  inc_indent();
  while (!node.stmts.empty()) {
    std::cout << std::endl << get_indent();
    Stmt* s = node.stmts.front();
    node.stmts.pop_front();
    //call accept on each stmt
    s->accept(*this);
  }
  //decrease indent
  dec_indent();
  std::cout << std::endl << get_indent() << "end";
}

void Printer::visit(Expr& node) {
  if (node.op != nullptr)
    std::cout << "(";
  if (node.negated)
    std::cout << "not ";
  //call accept on first expr
  node.first->accept(*this);
  //optional operation and expression
  if (node.op != nullptr) {
    std::cout << " " << node.op->lexeme() << " ";
    //call accept on rest expr
    node.rest->accept(*this);
    std::cout << ")";
  }
}

void Printer::visit(SimpleTerm& node) {
  //call accept on rvalue
  node.rvalue->accept(*this);
}

void Printer::visit(ComplexTerm& node) {
  //call accept on expr
  node.expr->accept(*this);
}

void Printer::visit(SimpleRValue& node) {
  //output SimpleRValue
  if (node.value.type() == STRING_VAL)
    std::cout << "\"" << node.value.lexeme() << "\"";
  else if (node.value.type() == CHAR_VAL)
    std::cout << "\'" << node.value.lexeme() << "\'";
  else
    std::cout << node.value.lexeme();
}

void Printer::visit(NewRValue& node) {
  //output NewRValue
  std::cout << "new " << node.type_id.lexeme();
}

void Printer::visit(CallExpr& node) {
  std::cout << node.function_id.lexeme() << "(";
  while (!node.arg_list.empty()) {
    Expr* e = node.arg_list.front();
    node.arg_list.pop_front();
    //call accept on expr
    e->accept(*this);
    if (!node.arg_list.empty())
      std::cout << ", ";
  }
  std::cout << ")";
}

void Printer::visit(IDRValue& node) {
  //output IDRValue
  Token t = node.path.front();
  node.path.pop_front();
  std::cout << t.lexeme();
  while (!node.path.empty()) {
    t = node.path.front();
    node.path.pop_front();
    std::cout << "." << t.lexeme();
  }
}

void Printer::visit(NegatedRValue& node) {
  std::cout << "neg ";
  //call accept on expr
  node.expr->accept(*this);
}


#endif
