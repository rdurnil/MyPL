//----------------------------------------------------------------------
// NAME: Rie Durnil
// FILE: Lexer.h
// DATE: February 2021
// DESC: Implementation of the lexer for myPL. 
//----------------------------------------------------------------------

#ifndef LEXER_H
#define LEXER_H

#include <istream>
#include <string>
#include "token.h"
#include "mypl_exception.h"


class Lexer
{
public:

  // construct a new lexer from the input stream
  Lexer(std::istream& input_stream);

  // return the next available token in the input stream (including
  // EOS if at the end of the stream)
  Token next_token();
  
private:

  // input stream, current line, and current column
  std::istream& input_stream;
  int line;
  int column;

  // return a single character from the input stream and advance
  char read();

  // return a single character from the input stream without advancing
  char peek();

  // create and throw a mypl_exception (exits the lexer)
  void error(const std::string& msg, int line, int column) const;
};


Lexer::Lexer(std::istream& input_stream)
  : input_stream(input_stream), line(1), column(1)
{
}


char Lexer::read()
{
  return input_stream.get();
}


char Lexer::peek()
{
  return input_stream.peek();
}


void Lexer::error(const std::string& msg, int line, int column) const
{
  throw MyPLException(LEXER, msg, line, column);
}


Token Lexer::next_token() {
  char next;
  char p;
  int l;
  std::string lexeme = "";
  //read the next token
  next = read();
  //go through whitespace and comments
  while (std::isspace(next) || next == '#') {
    if (next == '\n' || next == '\r' || next == '#') {
      while (next != '\n' && next != '\r')
        next = read();
      ++line;
      column = 1;
    } else { 
      ++column;
    }
    next = read();
  }
  //keep track of starting column for when token spans multiple
  int start_col = column;
  //enumerate column for next token
  ++column;
  //check for end of file
  if (next == EOF) {
    return Token(EOS, "", line, start_col);
  }
  //check for symbols
  else if (!isalpha(next) && !isdigit(next)) {
    if (next == ',') 
      return Token(COMMA, ",", line, start_col);
    else if (next == '.') {
      p = peek();
      if (isdigit(p))
        error("Invalid double, no leading digit ", line, start_col);
      return Token(DOT, ".", line, start_col);
    } else if (next == '(')
      return Token(LPAREN, "(", line, start_col);
    else if (next == ')')
      return Token(RPAREN, ")", line, start_col);
    else if (next == ':')
      return Token(COLON, ":", line, start_col);
    else if (next == '+')
      return Token(PLUS, "+", line, start_col);
    else if (next == '-')
      return Token(MINUS, "-", line, start_col);
    else if (next == '*')
      return Token(MULTIPLY, "*", line, start_col);
    else if (next == '/')
      return Token(DIVIDE, "/", line, start_col);
    else if (next == '%')
      return Token(MODULO, "%", line, start_col);
    else if (next == '=') {
      p = peek();
      if (p == '=') {
        next = read();
        ++column;
        return Token(EQUAL, "==", line, start_col);
      } else {
        return Token(ASSIGN, "=", line, start_col);
      } 
    }
    else if (next == '<') {
      p = peek();
      if (p == '=') {
        next = read();
        ++column;
        return Token(LESS_EQUAL, "<=", line, start_col);
      } else {
        return Token(LESS, "<", line, start_col);
      }
    }
    else if (next == '>') {
      p = peek();
      if (p == '=') {
        next = read();
        ++column;
        return Token(GREATER_EQUAL, ">=", line, start_col);
      } else {
        return Token(GREATER, ">", line, start_col);
      }
    }
    else if (next == '!') {
      p = peek();
      if (p == '=') {
        next = read();
        ++column;
        return Token(NOT_EQUAL, "!=", line, start_col);
      } else {
        error("invalid symbol '!'", line, start_col);
      }
    }
    //char values since they begin with symbols
    else if (next == '\'') {
      next = read();
      ++column;
      lexeme += next;
      next = read();
      // if the char is not exactly 1 character
      if (next != '\'')
        error("invalid char", line, start_col);
      ++column;
      return Token(CHAR_VAL, lexeme, line, start_col);
    }
    //string values since they begin with symbols
    else if (next == '\"') {
      next = read();
      ++column;
      //build up lexeme
      while (next != '\"') {
        //making sure string only spans one line of code at most
        if (next != '\n' && next != '\r') {
          lexeme += next;
          next = read();
          ++column;
        } else {
          error ("expecting \"", line, column);
        }
      }
      return Token(STRING_VAL, lexeme, line, start_col);  
    } else {
      lexeme = "invalid symbol ";
      lexeme += next;
      error(lexeme, line, start_col);
    }
  } 
  //numeric values
  else if (isdigit(next)) {
    lexeme += next;
    p = peek();
    if (next == '0') {
      //ensure no leading zeros
      if (!isdigit(p) && p != '.' && !isalpha(p))
        return Token(INT_VAL, "0", line, start_col);
      else if (p != '.')
        error("invalid digit following 0", line, start_col);
    }
    p = peek();
    while (isdigit(p)) {
      next = read();
      ++column;
      lexeme += next;
      p = peek();
    }
    //differentiating int from double
    if (p != '.') {
      if (p == '#') {
        next = read();
        while (next != '\n' && next != '\r')
          next = read();
        ++line;
        column = 1;
        return Token(INT_VAL, lexeme, line - 1, start_col);
      }
      return Token(INT_VAL, lexeme, line, start_col);
    } else {
      next = read();
      ++column;
      lexeme += next;
      p = peek();
      if (!isdigit(p))
        error("Trailing digits needed in double ", line, start_col);
      while (isdigit(p)) {
        next = read();
        ++column;
        lexeme += next;
        p = peek();
      }
      if (!isalpha(p)) {
        if (p == '#') {
          next = read();
          while (next != '\n' && next != '\r')
            next = read();
          ++line;
          column = 1;
          return Token(DOUBLE_VAL, lexeme, line - 1, start_col);
        }
        return Token(DOUBLE_VAL, lexeme, line, start_col);
      } else {
        lexeme = "";
        lexeme += next;
        error(lexeme, line, start_col);
      }
    }
  }
  //reserved words, ids 
  else if (isalpha(next)) {
    //build up lexeme
    lexeme += next;
    p = peek();
    while (isalpha(p) || isdigit(p) || p == '_') {
      next = read();
      ++column;
      lexeme += next;
      p = peek();
    }
    //check what kind of string it is
    if (lexeme == "type") 
      return Token(TYPE, lexeme, line, start_col);
    else if (lexeme == "while")
      return Token(WHILE, lexeme, line, start_col);
    else if (lexeme == "for")
      return Token(FOR, lexeme, line, start_col);
    else if (lexeme == "to")
      return Token(TO, lexeme, line, start_col);
    else if (lexeme == "do")
      return Token(DO, lexeme, line, start_col);
    else if (lexeme == "if")
      return Token(IF, lexeme, line, start_col);
    else if (lexeme == "then")
      return Token(THEN, lexeme, line, start_col);
    else if (lexeme == "elseif")
      return Token(ELSEIF, lexeme, line, start_col);
    else if (lexeme == "else")
      return Token(ELSE, lexeme, line, start_col);
    else if (lexeme == "end")
      return Token(END, lexeme, line, start_col);
    else if (lexeme == "fun")
      return Token(FUN, lexeme, line, start_col);
    else if (lexeme == "var")
      return Token(VAR, lexeme, line, start_col);
    else if (lexeme == "return")
      return Token(RETURN, lexeme, line, start_col);
    else if (lexeme == "new")
      return Token(NEW, lexeme, line, start_col);
    else if (lexeme == "neg")
      return Token(NEG, lexeme, line, start_col);
    else if (lexeme == "and")
      return Token(AND, lexeme, line, start_col);
    else if (lexeme == "or")
      return Token(OR, lexeme, line, start_col);
    else if (lexeme == "not")
      return Token(NOT, lexeme, line, start_col);
    else if (lexeme == "bool")
      return Token(BOOL_TYPE, lexeme, line, start_col);
    else if (lexeme == "int")
      return Token(INT_TYPE, lexeme, line, start_col);
    else if (lexeme == "double")
      return Token(DOUBLE_TYPE, lexeme, line, start_col);
    else if (lexeme == "char")
      return Token(CHAR_TYPE, lexeme, line, start_col);
    else if (lexeme == "string")
      return Token(STRING_TYPE, lexeme, line, start_col);
    else if (lexeme == "while")
      return Token(WHILE, lexeme, line, start_col);
    else if (lexeme == "true" || lexeme == "false")
      return Token(BOOL_VAL, lexeme, line, start_col);
    else if (lexeme == "nil")
      return Token(NIL, lexeme, line, start_col);
    else if (isalpha(lexeme[0])) {
      return Token(ID, lexeme, line, start_col);
    }
  }
}


#endif
