#include "InstructionAST.h"

#include <algorithm>

#include "MemStack.h"
#include "Processor.h"


// //COMMENT THIS OUT LATER GG
#include <iostream>
InstructionAST::InstructionAST(Op op) : mOp(op){
  // TODO: Initialize all members of this class
}

NopInstructionAST::NopInstructionAST():InstructionAST(Op::NOP){
  // TODO: Initialize the base class and all members of this class
  //NopInstructionsAST -> InstructionAST -> ExprAST -> AST
}

void NopInstructionAST::eval(Processor& processor) const {
  // TODO: Do nothing
  static_cast<void>(processor);
}

MovInstructionAST::MovInstructionAST(RegisterOperandExprAST* dst) : InstructionAST(Op::MOV), mDst(dst){
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `dst` is transferred into this class.
  //       - `dst` is never nullptr.
  //MovInstructionAST -> InstructionAST -> ExprAST-> AST
  //let mDst point to object that dst is pointing (share)
  dst = nullptr;
}

MovInstructionAST::~MovInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mDst;
  mDst = nullptr; //deallocate pointer
}

const RegisterOperandExprAST& MovInstructionAST::getDst() const {
  // TODO: You can assume mDst is never nullptr.
  //RegisterOperandExprAST -> OperandExprAST -> ExprAST -> AST
  return *mDst;

}

RegMovInstructionAST::RegMovInstructionAST(RegisterOperandExprAST* dst, RegisterOperandExprAST* src):MovInstructionAST(dst), mSrc(src){
  // TODO: Initialize the base class and all members of this class
  //       Note that the ownership of dst and src is transferred into this class.
  dst= nullptr;
  src = nullptr;
}

RegMovInstructionAST::~RegMovInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mSrc;
  mSrc = nullptr;
}

void RegMovInstructionAST::eval(Processor& processor) const {
  // TODO: Implement register-to-register move
  //get register from processor and put into MovInstructionAST
  // u32& dst = processor.getRegister(MovInstructionAST::getDst().getRegister());
  u32 src = processor.getRegister(mSrc->getRegister());
  processor.getRegister(MovInstructionAST::getDst().getRegister())=src;
}

ImmMovInstructionAST::ImmMovInstructionAST(RegisterOperandExprAST* dst, Imm32OperandExprAST* imm): MovInstructionAST(dst),mImm(imm){
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `dst` and `imm` is transferred into this class.
  //       - `dst` and `imm` is never nullptr.
  dst = nullptr;
  imm = nullptr;
}

ImmMovInstructionAST::~ImmMovInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mImm;
  mImm = nullptr;
}

void ImmMovInstructionAST::eval(Processor& processor) const {
  // TODO: Implement immediate-to-register move
  // u32 &dst = processor.getRegister(MovInstructionAST::getDst().getRegister());
  // u32 Imm = mImm -> getUnsigned32();
  // dst = Imm;
  processor.getRegister(MovInstructionAST::getDst().getRegister())=mImm -> getUnsigned32();
}

AddInstructionAST::AddInstructionAST(RegisterOperandExprAST* dst, RegisterOperandExprAST* src1):InstructionAST(Op::ADD),mDst(dst), mSrc1(src1) {
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `dst` and `src1` is transferred into this class.
  //       - `dst` and `src1` is never nullptr.
  dst = nullptr;
  src1 = nullptr;
}

AddInstructionAST::~AddInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mDst;
  mDst = nullptr;
  delete mSrc1;
  mSrc1 = nullptr;
}

const RegisterOperandExprAST& AddInstructionAST::getDst() const {
  // TODO: You can assume mDst is never nullptr.
  return *mDst;
}

const RegisterOperandExprAST& AddInstructionAST::getSrc1() const {
  // TODO: You can assume mSrc1 is never nullptr.
  return *mSrc1;
}

RegAddInstructionAST::RegAddInstructionAST(RegisterOperandExprAST* dst,
                                           RegisterOperandExprAST* src1,
                                           RegisterOperandExprAST* src2) :
                                           AddInstructionAST(dst, src1),
                                           mSrc2(src2){
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `dst`, `src1` and `src2` is transferred into this class.
  //       - `dst`, `src1` and `src2` is never nullptr.
  dst = nullptr;
  src1= nullptr;
  src2 = nullptr;

}

RegAddInstructionAST::~RegAddInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mSrc2;
  mSrc2 = nullptr;
}

void RegAddInstructionAST::eval(Processor& processor) const {
  // TODO: Implement register-register add
  //       Note that:
  //       The addition should wraparound if it overflows. In other words, if the result of the addition does not fit 
  //       into the register, take the lower 32-bits of the result.
  //       For example, 4294967295 (i.e. maximum number represented by u32) + 1 = 0
  
  // u32 &dst = processor.getRegister(AddInstructionAST::getDst().getRegister());
  // u32 src1 = processor.getRegister(AddInstructionAST::getSrc1().getRegister());
  // u32 src2 = processor.getRegister(mSrc2->getRegister());
  // dst = src1 + src2;

  processor.getRegister(AddInstructionAST::getDst().getRegister())=processor.getRegister(AddInstructionAST::getSrc1().getRegister())+ processor.getRegister(mSrc2->getRegister());
}

ImmAddInstructionAST::ImmAddInstructionAST(RegisterOperandExprAST* dst,
                                           RegisterOperandExprAST* src,
                                           Imm32OperandExprAST* imm) :
                                           AddInstructionAST(dst, src),
                                           mImm(imm){
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `dst`, `src` and `imm` is transferred into this class.
  //       - `dst`, `src` and `imm` is never nullptr.
  dst= nullptr;
  src= nullptr;
  imm= nullptr;
}

ImmAddInstructionAST::~ImmAddInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mImm;
  mImm= nullptr;
}

void ImmAddInstructionAST::eval(Processor& processor) const {
  // TODO: Implement register-immediate add
  //       The addition should wraparound if it overflows. In other words, if the result of the addition does not fit 
  //       into the register, take the lower 32-bits of the result.
  //       For example, 4294967295 (i.e. maximum number represented by u32) + 1 = 0
  // u32 &dst = processor.getRegister(AddInstructionAST::getDst().getRegister());
  // u32 src = processor.getRegister(AddInstructionAST::getSrc1().getRegister());
  // u32 imm = mImm->getUnsigned32();
  // // u32 temp = (src+imm>4294967295)?src+imm-4294967295 : src+imm; //wraparound
  // dst= src + imm;
  
  processor.getRegister(AddInstructionAST::getDst().getRegister())=processor.getRegister(AddInstructionAST::getSrc1().getRegister())+mImm->getUnsigned32();
}

SubInstructionAST::SubInstructionAST(RegisterOperandExprAST* dst, RegisterOperandExprAST* src1) : InstructionAST(Op::SUB),mDst(dst), mSrc1(src1){
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `dst` and `src1` is transferred into this class.
  //       - `dst` and `src1` is never nullptr.
  dst=nullptr;
  src1= nullptr;
}

SubInstructionAST::~SubInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mDst;
  mDst = nullptr;
  delete mSrc1;
  mSrc1 = nullptr;
}

const RegisterOperandExprAST& SubInstructionAST::getDst() const {
  // TODO: Implement register-register subtract
  return *mDst;

}

const RegisterOperandExprAST& SubInstructionAST::getSrc1() const {
  // TODO
  return *mSrc1;
}

RegSubInstructionAST::RegSubInstructionAST(RegisterOperandExprAST* dst,
                                           RegisterOperandExprAST* src1,
                                           RegisterOperandExprAST* src2) :
                                           SubInstructionAST(dst, src1),
                                           mSrc2(src2){
  // TODO: Initialize the base class and all members of this class
  //       - The ownership of `dst`, `src1` and `src2` is transferred into this class.
  //       Note that:
  //       - `dst`, `src1` and `src2` is never nullptr.
  dst =nullptr;
  src1= nullptr;
  src2 = nullptr;
}

void RegSubInstructionAST::eval(Processor& processor) const {
  // TODO: Implement register-register subtraction
  //       The subtraction should wraparound if it overflows.
  //       For example, 0 (i.e. minimum number represented by u32) - 1 = 4294967295
  //       This will also work for signed operands.
  //GG later do signed
  // u32 &dst = processor.getRegister(SubInstructionAST::getDst().getRegister());
  // u32 src1 = processor.getRegister(SubInstructionAST::getSrc1().getRegister());
  // u32 src2 = processor.getRegister(mSrc2->getRegister());
  // dst = src1 - src2;
  processor.getRegister(SubInstructionAST::getDst().getRegister())=processor.getRegister(SubInstructionAST::getSrc1().getRegister())-processor.getRegister(mSrc2->getRegister());
}

RegSubInstructionAST::~RegSubInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mSrc2;
  mSrc2 = nullptr;

}

ImmSubInstructionAST::ImmSubInstructionAST(RegisterOperandExprAST* dst,
                                           RegisterOperandExprAST* src,
                                           Imm32OperandExprAST* imm): 
                                           SubInstructionAST(dst,src),
                                           mImm(imm){
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `dst`, `src` and `imm` is transferred into this class.
  //       - `dst`, `src` and `imm` is never nullptr.
  dst = nullptr;
  src = nullptr;
  imm = nullptr;
}

void ImmSubInstructionAST::eval(Processor& processor) const {
  // TODO: Implement register-immediate subtraction
  //       The subtraction should wraparound if it overflows.
  //       For example, 0 (i.e. minimum number represented by u32) - 1 = 4294967295
  //       This will also work for signed operands.
  // u32 &dst = processor.getRegister(SubInstructionAST::getDst().getRegister());
  // u32 src1 = processor.getRegister(SubInstructionAST::getSrc1().getRegister());
  // u32 imm = mImm->getUnsigned32();
  // dst = src1 - imm;
  processor.getRegister(SubInstructionAST::getDst().getRegister())=processor.getRegister(SubInstructionAST::getSrc1().getRegister())-mImm->getUnsigned32();
}

ImmSubInstructionAST::~ImmSubInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mImm;
  mImm = nullptr;
}

PushInstructionAST::PushInstructionAST(RegisterListOperandExprAST* regList):InstructionAST(Op::PUSH), mRegList(regList){
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `regList` is transferred into this class.
  //       - `regList` is never nullptr.
  regList= nullptr;
}

PushInstructionAST::~PushInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mRegList;
  mRegList = nullptr;
}

void PushInstructionAST::eval(Processor& processor) const {
  // TODO: Implement pushing registers into the stack
  //       Note that:
  //       - You will need to shift the stack pointer (Register::SP) to make room for the contents of the register.
  //       - The stack grows from top to bottom.
  //       - Lower-indexed registers must be stored in lower memory addresses.
  //       - Do NOT store the contents of the register beyond the stack pointer. This will be equivalent to reading 
  //         from an out-of-bounds element in an array!
  //       - The `addr` parameter of `MemStack::store` is in bytes. Registers are 32-bit, so you may need to do some
  //         calculations.
  //reglist-comma seperated registers
  
  MemStack &stack = processor.getStack();
  u32 &sp = processor.getRegister(Register::SP);
  for (u8 i=15; i>=0; --i){
    // std::cout<<mRegList->hasRegister(i)<< "   "<<   processor.getRegister(i)<<std::endl;//print out token name
    if (mRegList->hasRegister(i)){
        // std::cout<< "save" << std::endl;
        u32& value = processor.getRegister(i);
        sp-=4;
        stack.store(value,sp);
    }
    if (i==0){break;}
  }
  // stack.dump(sp);
}

PopInstructionAST::PopInstructionAST(RegisterListOperandExprAST* regList):InstructionAST(Op::POP), mRegList(regList) {
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `regList` is transferred into this class.
  //       - `regList` is never nullptr.
  regList = nullptr;
}

PopInstructionAST::~PopInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mRegList;
  mRegList = nullptr;
}

void PopInstructionAST::eval(Processor& processor) const {
  // TODO: Implement popping registers from the stack
  //       Note that:
  //       - You will need to shift the stack pointer (Register::SP) to free up room for the registers you have popped.
  //       - The stack shrinks from bottom to top.
  //       - Lower-indexed registers must be loaded from lower memory addresses.
  //       - Do NOT load the contents of the register beyond the stack pointer. This will be equivalent to reading 
  //         from an out-of-bounds element in an array!
  //       - The `addr` parameter of `MemStack::load` is in bytes. Registers are 32-bit, so you may need to do some
  //         calculations.
    MemStack &stack = processor.getStack();
    u32 &sp = processor.getRegister(Register::SP);
    for (u8 i=0; i<16; i++){
      if(mRegList->hasRegister(i)){
        processor.getRegister(i)= stack.load(sp);
        sp+=4;
      }
    }
}


StrInstructionAST::StrInstructionAST(RegisterOperandExprAST* src, IndirectOperandExprAST* dst) : InstructionAST(Op::STR), mSrc(src), mDst(dst) {
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `dst` and `src` is transferred into this class.
  //       - `dst` and `src` is never nullptr.
  dst = nullptr;
  src = nullptr;
}

StrInstructionAST::~StrInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mDst;
  mDst = nullptr;
  delete mSrc;
  mSrc = nullptr;
}


//RECHECK THIS
void StrInstructionAST::eval(Processor& processor) const {
  // TODO: Implement storing a single register into the stack
  //       Note that:
  //       - You will need to apply the offset of the indirect operand to the memory address.
  //       - You do not need to shift the stack pointer.
    u32 src= processor.getRegister(mSrc->getRegisterIndex());
    s16 offset = mDst-> getOffset();
    u32 dst = static_cast<u32>(mDst->getRegisterTarget().getRegisterIndex());//offset destination reg
    MemStack &stack = processor.getStack();
    stack.store(src, processor.getRegister(dst) + offset);//store value of src info dst after offset

}

LdrInstructionAST::LdrInstructionAST(RegisterOperandExprAST* dst, IndirectOperandExprAST* src):InstructionAST(Op::LDR), mDst(dst), mSrc(src) {
  // TODO: Initialize the base class and all members of this class
  //       Note that:
  //       - The ownership of `dst` and `src` is transferred into this class.
  //       - `dst` and `src` is never nullptr.
  dst = nullptr;
  src = nullptr;
}

LdrInstructionAST::~LdrInstructionAST() {
  // TODO: Clean up the Expression ASTs owned by this class
  delete mDst;
  mDst = nullptr;
  delete mSrc;
  mSrc = nullptr;
}

void LdrInstructionAST::eval(Processor& processor) const {
  // TODO: Implement loading a single register from the stack
  //       Note that:
  //       - You will need to apply the offset of the indirect operand to the memory address.
  //       - You do not need to shift the stack pointer.
    // processor.dump();
  MemStack& stack = processor.getStack();
  processor.getRegister(mDst->getRegisterIndex()) = stack.load(static_cast<u32>(processor.getRegister(static_cast<u32>(mSrc->getRegisterTarget().getRegisterIndex())) + mSrc->getOffset()));
    
}




