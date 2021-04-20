#ifndef AST_H
#define AST_H

#include <string>

#include "Util.h"

// TODO: Write the definition of AST and ExprAST.
class AST{//represents node in abstract syntax tree 
    public:
        //get name of instruction 
        virtual const char * getTokenName() const =0;
        //outputs some information regardint AST
        virtual void print (u32 indent) const =0; 
        virtual ~AST() = default;
};

class ExprAST: public AST{ //expression in node
    public:
        //get node type
        const char* getTokenName() const override=0; 
        void print (u32 indent) const override; 
        //output information on Abstract Syntax Tree (AST) node
        virtual ~ExprAST() = default;
};

#endif  // AST_H
