#include "AST.hpp"
#include <algorithm>
#include "output.hpp"
#include <sstream>
//#include "PrintTree.hpp"
#include "CodeGeneration.hpp"
#include <iostream>

extern int yylineno;


static constexpr std::array type_name {"INT", "BYTE", "BOOL", "STRING", "VOID"};

namespace AST {

    int switch_count = 0;
    int while_count = 0;

    static inline bool castable(Type from, Type to) {
        if (from == Type::VOID) return false;
        if (from == to || (from == Type::BYTE && to == Type::INT)) return true;
        else return false;
    }

    static inline bool numType(Type type) {
        return type == Type::INT || type == Type::BYTE;
    }

    ProgramNode::ProgramNode(unique_ptr<Node> funcs) : Node(Type::VOID, "Program") {
        bool main = false;
        for(auto &func :funcs->children){
            auto &signature = func->children[0];
            auto &ret_type = signature->children[0];
            auto &id = signature->children[1];
            auto &args = signature->children[2];
            if(id->name == "main" && ret_type->type == SymbolTable::Type::VOID && args->children.empty()){
                main = true;
                break;
            }
        }

        if(!main){
            output::errorMainMissing();
            exit(1);
        }
        children.push_back(move(funcs));
        SymbolTable::getInstance().CloseScope();
        /*auto printer = Printer{};
        printer.walk(this, "");*/
        CodeBuffer::instance().printGlobalBuffer();
        std::cout << std::endl;
        CodeBuffer::instance().printCodeBuffer();
    }

    FuncsNode::FuncsNode() : Node(Type::VOID, "Funcs") {}

    FuncsNode::FuncsNode(unique_ptr<Node> funcsImpl,
                         unique_ptr<Node> funcs) : Node(Type::VOID, "Funcs") {
        children.push_back(move(funcsImpl));
        for(auto &child : funcs->children){
            children.push_back(move(child));
        }
    }

    FuncsSignNode::FuncsSignNode(unique_ptr<Node> retType,
                                 unique_ptr<Node> id, unique_ptr<Node> formals) : Node(retType->type, "Function Signature") {
        auto &st = SymbolTable::getInstance();
        if(!st.FindID(id->name).type.empty()){
            output::errorDef(yylineno, id->name);
            exit(1);
        }
        vector<Type> types;
        types.push_back(retType->type);
        if(formals->children.empty())
            types.push_back(Type::VOID);
        else for(auto &f : formals->children)
            types.push_back(f->type);
        st.AddFunction(id->name, types);
        st.OpenScope();
        for(auto &f : formals->children) {
            if (!st.FindID(f->name).type.empty()){
                output::errorDef(yylineno, f->name);
                exit(1);
            }
            st.AddArgument(f->name, f->type);
        }
        children.push_back(move(retType));
        children.push_back(move(id));
        children.push_back(move(formals));
    }

    FuncsImplNode::FuncsImplNode(unique_ptr<Node> decl_node, unique_ptr<Node> statements)
    : Node(Type::VOID, "Function Implementation"){
        children.push_back(move(decl_node));
        children.push_back(move(statements));
        SymbolTable::getInstance().CloseScope();
    }

    RetTypeNode::RetTypeNode()
    : Node(Type::VOID, "Function Return Type") {}

    RetTypeNode::RetTypeNode(unique_ptr<Node> type_node)
    : Node(type_node->type, "Function Return Type") {}

    FormalsNode::FormalsNode() : Node(Type::VOID, "Formals") {}

    FormalsNode::FormalsNode(unique_ptr<Node> formals_list)
    : Node(Type::VOID, "Formals")
    {
        children = move(formals_list->children);
    }

    FormalsListNode::FormalsListNode(unique_ptr<Node> formal_decl)
    : Node(Type::VOID, "FormalsList")
    {
        children.push_back(move(formal_decl));
    }
    
    FormalsListNode::FormalsListNode(unique_ptr<Node> formal_decl, unique_ptr<Node> formals_list)
    : Node(Type::VOID, "FormalsList")
    {
        children.push_back(move(formal_decl));
        for(auto &ptr : formals_list->children)
            children.push_back(move(ptr));
    }

    FormalDeclNode::FormalDeclNode(unique_ptr<Node> type_node, unique_ptr<Node> id)
    : Node(type_node->type, id->name)
    {}


    StatementsNode::StatementsNode(unique_ptr<Node> statement) : Node(Type::VOID, "Statements") {
        children.push_back(move(statement));
    }

    StatementsNode::StatementsNode(unique_ptr<Node> statements,
                                   unique_ptr<Node> statement) : Node(Type::VOID, "Statements") {
        for (auto &child : statements->children) {
            children.push_back(move(child));
        }
        children.push_back(move(statement));
    }

    StatementsToStatementNode::StatementsToStatementNode(unique_ptr<Node> statements) : Node(Type::VOID, "{ statements }") {
        for (auto &child : statements->children) {
            children.push_back(move(child));
        }
    }

    StatementDeclareVariableNode::StatementDeclareVariableNode(unique_ptr<Node> type_node, unique_ptr<Node> id) : Node(Type::VOID, "declare new variable") {
        auto &st = SymbolTable::getInstance();
        vector<SymbolTable::Type> type = st.FindID(id->name).type;
        if (type.empty()) {
            string reg_name = CodeGen::create_variable("0", type_node->type);
            st.AddVariable(id->name, type_node->type, reg_name);
        }
        else {
            output::errorDef(yylineno, id->name);
            exit(1);
        }
        children.push_back(move(type_node));
        children.push_back(move(id));
    }

    StatementDeclareVariableNode::StatementDeclareVariableNode(unique_ptr<Node> type_node, unique_ptr<Node> id, unique_ptr<Node> exp) : Node(Type::VOID, "declare new variable statement + value assignment") {
        if (!castable(exp->type, type_node->type)) {
            output::errorMismatch(yylineno);
            exit(1);
        }
        auto &st = SymbolTable::getInstance();
        vector<SymbolTable::Type> type = st.FindID(id->name).type;
        if (type.empty()) {
            string value = exp->value();
            if (type_node->type == Type::INT && exp->type == Type::BYTE) {
                value = CodeGen::fromByteToInt(value);
            }
            string reg_name = CodeGen::create_variable(value, type_node->type);
            st.AddVariable(id->name, type_node->type, reg_name);
        }
        else {
            output::errorDef(yylineno, id->name);
            exit(1);
        }
        children.push_back(move(type_node));
        children.push_back(move(id));
        children.push_back(move(exp));
    }

    StatementAssignNode::StatementAssignNode(unique_ptr<Node> id, unique_ptr<Node> exp) : Node(id->type, "assign value to predeclared variable") {
        auto &st = SymbolTable::getInstance();
        auto &entry = st.FindID(id->name);
        const vector<SymbolTable::Type> &type = entry.type;
        string value = exp->value();
        if (type.empty() || type.size() > 1) {
            output::errorUndef(yylineno, id->name);
            exit(1);
        }
        else if (!castable(exp->type, type[0])) {
            output::errorMismatch(yylineno);
            exit(1);
        }
        if (entry.type[0] == Type::INT && exp->type == Type::BYTE) {
            value = CodeGen::fromByteToInt(value);
        }
        CodeGen::store_variable(entry.register_name, value, type[0]);
        children.push_back(move(id));
        children.push_back(move(exp));
    }

    StatementReturnNode::StatementReturnNode() : Node(Type::VOID, "void return statement") {
        auto &st = SymbolTable::getInstance();
        if(st.getFunctionType() != Type::VOID) {
            output::errorMismatch(yylineno);
            exit(1);
        }
    }

    StatementReturnNode::StatementReturnNode(unique_ptr<Node> exp)
    : Node(exp->type, "non void return statement") {
        auto &st = SymbolTable::getInstance();
        if( !castable(type, st.getFunctionType()) ) {
            output::errorMismatch(yylineno);
            exit(1);
        }
        children.push_back(move(exp));
    }

    StatementIfElseNode::StatementIfElseNode(unique_ptr<Node> decl, unique_ptr<Node> statement) : Node(Type::VOID, "if statement") {
        children.push_back(move(decl->children.front()));
        children.push_back(move(statement));
    }

    StatementIfElseNode::StatementIfElseNode(unique_ptr<Node> decl, unique_ptr<Node> if_statement, unique_ptr<Node> else_statement) : Node(Type::VOID, "if else statement") {
        children.push_back(move(decl->children.front()));
        children.push_back(move(if_statement));
        children.push_back(move(else_statement));
    }

    StatementWhileNode::StatementWhileNode(unique_ptr<Node> decl, unique_ptr<Node> statement) : Node(Type::VOID, "while loop") {
        children.push_back(move(decl->children.front()));
        children.push_back(move(statement));
    }

    StatementBreakContinue::StatementBreakContinue(const string &break_or_continue) : Node(Type::VOID, break_or_continue + " statement") {
        if (break_or_continue == "break" && while_count <= 0 && switch_count <= 0) {
            output::errorUnexpectedBreak(yylineno);
            exit(1);
        } else if (break_or_continue == "continue" && while_count <= 0) {
            output::errorUnexpectedContinue(yylineno);
            exit(1);
        }
    }

    StatementSwitchNode::StatementSwitchNode(unique_ptr<Node> decl, unique_ptr<Node> case_list) : Node(Type::VOID, "switch statement") {
        children.push_back(move(decl->children.front()));
        children.push_back(move(case_list));
    }

    IfDeclNode::IfDeclNode(unique_ptr<Node> exp) : Node(Type::VOID, "if declaration") {
        if (exp->type != Type::BOOL){
            output::errorMismatch(yylineno);
            exit(1);
        }
        children.push_back(move(exp));
    }

    WhileDeclNode::WhileDeclNode(unique_ptr<Node> exp) : Node(Type::VOID, "while declaration") {
        if (exp->type != Type::BOOL){
            output::errorMismatch(yylineno);
            exit(1);
        }
        children.push_back(move(exp));
    }

    SwitchDeclNode::SwitchDeclNode(unique_ptr<Node> exp) : Node(Type::VOID, "switch declaration") {
        if (!numType(exp->type)){
            output::errorMismatch(yylineno);
            exit(1);
        }
        children.push_back(move(exp));
    }

    CallNode::CallNode(unique_ptr<Node> id_node) : Node(Type::VOID, "Function Call")
    {
        auto &st = SymbolTable::getInstance();
        const vector<SymbolTable::Type> &type = st.FindID(id_node->name).type;
        if (type.empty() || type.size() < 2) {
            output::errorUndefFunc(yylineno, id_node->name);
            exit(1);
        }
        if (type[1] != Type::VOID) {
            vector<string> type_strs;
            std::transform(type.begin()+1, type.end(), std::back_inserter(type_strs),
                           [](auto &t){ return type_name[(int)t]; });
            output::errorPrototypeMismatch(yylineno, id_node->name, type_strs);
            exit(1);
        }
        children.push_back(move(id_node));
        this->type = type[0];
    }

    CallNode::CallNode(unique_ptr<Node> id_node, unique_ptr<Node> exp_list)  : Node(Type::VOID, "Function Call")
    {
        auto &st = SymbolTable::getInstance();
        const auto &type = st.FindID(id_node->name).type;
        if (type.empty() || type.size() < 2) {
            output::errorUndefFunc(yylineno, id_node->name);
            exit(1);
        }
        bool mismatch = type.size() - 1 != exp_list->children.size()
            || std::mismatch(exp_list->children.begin(),exp_list->children.end(),
                             type.begin()+1, [](auto &x, auto &y){ return castable(x->type,y); }).first
                             != exp_list->children.end();
        if(mismatch) {
            vector<string> type_strs;
            std::transform(type.begin()+1, type.end(), std::back_inserter(type_strs),
                           [](auto &t){ return type_name[(int)t]; });
            output::errorPrototypeMismatch(yylineno, id_node->name, type_strs);
            exit(1);
        }
        children.push_back(move(id_node));
        children.push_back(move(exp_list));
        this->type = type[0];
    }

    ExpListNode::ExpListNode(unique_ptr<Node> exp)  : Node(Type::VOID, "ExpList")
    {
        children.push_back(move(exp));
    }

    ExpListNode::ExpListNode(unique_ptr<Node> exp, unique_ptr<Node> exp_list)  : Node(Type::VOID, "ExpList")
    {
        children.push_back(move(exp));
        for (auto &child : exp_list->children) {
            children.push_back(move(child));
        }
    }

    TypeNode::TypeNode(Type type) : Node(type, "Type") {}

    BinOpNode::BinOpNode(unique_ptr<Node> exp1, unique_ptr<Node> op, unique_ptr<Node> exp2) : Node(Type::VOID, "Binary Operation")
    {
        string value1 = exp1->value();
        string value2 = exp2->value();

        if (exp1->type == exp2->type && (exp1->type == Type::INT || exp1->type == Type::BYTE))
        {
            type = exp1->type;
        }
        else if (exp1->type == Type::BYTE && exp2->type == Type::INT) {
            type = Type::INT;
            value1 = CodeGen::fromByteToInt(value1);
        }
        else if (exp2->type == Type::BYTE && exp1->type == Type::INT) {
            type = Type::INT;
            value2 = CodeGen::fromByteToInt(value2);
        }
        else {
            output::errorMismatch(yylineno);
            exit(1);
        }
        this->result_reg = CodeGen::arithmetic(op->name, type == Type::INT? 32: 8, value1, value2);
        children.push_back(move(exp1));
        children.push_back(move(op));
        children.push_back(move(exp2));
    }

    string BinOpNode::value() {
        return result_reg;
    }

    RelOpNode::RelOpNode(unique_ptr<Node> exp1, unique_ptr<Node> op, unique_ptr<Node> exp2)
    : Node(Type::BOOL, "Relational operator") {
        string value1 = exp1->value();
        string value2 = exp2->value();
        if (exp1->type == Type::BYTE && exp2->type == Type::INT) {
            value1 = CodeGen::fromByteToInt(value1);
        }
        else if (exp2->type == Type::BYTE && exp1->type == Type::INT) {
            value2 = CodeGen::fromByteToInt(value2);
        }
        else if (!numType(exp1->type) || !numType(exp2->type)){
            output::errorMismatch(yylineno);
            exit(1);
        }
        this->result_reg = CodeGen::relop(
                op->name,
                !(exp1->type == Type::BYTE && exp2->type == Type::BYTE),
                value1, value2
                );
        children.push_back(move(exp1));
        children.push_back(move(op));
        children.push_back(move(exp2));
    }

    string RelOpNode::value() {
        return this->result_reg;
    }

    pair<BackpatchList, BackpatchList> RelOpNode::getBackpatchLists() {
        return CodeGen::getListsFromBool(result_reg);
    }

    BoolBinOpNode::BoolBinOpNode(unique_ptr<Node> exp1, unique_ptr<Node> N, unique_ptr<Node> op, unique_ptr<Node> M, unique_ptr<Node> exp2) : Node(Type::BOOL, "Binary Boolean Operation")
    {
        if (exp1->type != Type::BOOL || exp2->type != Type::BOOL )
        {
            output::errorMismatch(yylineno);
            exit(1);
        }
        auto BPlists2 = exp2->getBackpatchLists();
        auto &buffer = CodeBuffer::instance();
        buffer.bpatch(N->getBackpatchLists().first, buffer.genLabel());
        auto BPlists1 = exp1->getBackpatchLists();
        std::tie(this->true_list, this->false_list) = CodeGen::andOr(
                op->name, M->value(),
                BPlists1, BPlists2
        );
        children.push_back(move(exp1));
        children.push_back(move(op));
        children.push_back(move(exp2));
    }

    string BoolBinOpNode::value() {
        return CodeGen::getBoolFromLists(true_list, false_list);
    }

    pair<BackpatchList, BackpatchList> BoolBinOpNode::getBackpatchLists() {
        return {true_list, false_list};
    }

    IDNode::IDNode(string name) : Node(Type::VOID, move(name)) {}

    LiteralNode::LiteralNode(string name, Type type) : Node(type, move(name)) {
        if (this->type == Type::STRING) {
            this->global_variable = CodeGen::create_global_string(this->name);
        }
    }

    string LiteralNode::value() {
        if (this->type == Type::INT || this->type == Type::BYTE) {
            return this->name;
        }
        else if (this->type == Type::STRING) {
            return this->global_variable;
        }
        throw std::logic_error("literal is not from permitted type");
    }

    NotNode::NotNode(unique_ptr<Node> exp) : Node(Type::BOOL, "Not")
    {
        if (exp->type != Type::BOOL)
        {
            output::errorMismatch(yylineno);
            exit(1);
        }
        std::tie(this->true_list, this->false_list) = pair{this->false_list, this->true_list};
        children.push_back(move(exp));
    }

    string NotNode::value() {
        return CodeGen::getBoolFromLists(true_list, false_list);
    }

    CaseListNode::CaseListNode(unique_ptr<Node> case_decl) : Node(Type::VOID, "Case List for switch block")
    {
        children.push_back(move(case_decl));
    }

    CaseListNode::CaseListNode(unique_ptr<Node> case_decl, unique_ptr<Node> case_list) : Node(Type::VOID, "Case List for switch block")
    {
        children.push_back(move(case_decl));
        for (auto &child : case_list->children)
        {
            children.push_back(move(child));
        }
    }

    DefaultCaseNode::DefaultCaseNode(unique_ptr<Node> statements) : Node(Type::VOID, "default case")
    {
        children.push_back(move(statements));
    }

    CaseDeclNode::CaseDeclNode(unique_ptr<Node> statements) : Node(Type::VOID, "case declaration") {
        for (auto &child : statements->children) {
            children.push_back(move(child));
        }
    }

    IDExp::IDExp(unique_ptr<Node> id) : Node(id->type, move(id->name)) {
        auto &st = SymbolTable::getInstance();
        auto &entry = st.FindID(name);
        auto &type = entry.type;
        if(type.empty() || type.size() > 1) {
            output::errorUndef(yylineno, name);
            exit(1);
        }
        this->type = type[0];
        // TODO : check if function argument
        this->result_reg = CodeGen::load_variable(entry.register_name, this->type);
    }

    string IDExp::value() {
        return this->result_reg;
    }

    pair<BackpatchList, BackpatchList> IDExp::getBackpatchLists() {
        if (this->type != Type::BOOL)
            throw std::logic_error("called getBackpatchLists() on non-bool value");
        return CodeGen::getListsFromBool(this->result_reg);
    }

    MNode::MNode() : Node(Type::VOID, "M") {
        this->label = CodeBuffer::instance().genLabel();
    }

    string MNode::value() {
        return this->label;
    }

    NNode::NNode() : Node(Type::VOID, "N") {
        auto &buffer = CodeBuffer::instance();
        int loc = buffer.emit("br label @");
        this->nextList = CodeBuffer::makelist({loc, FIRST});
    }

    pair<BackpatchList, BackpatchList> NNode::getBackpatchLists() {
        return { this->nextList, {} };
    }

    string Node::value() {
//        throw std::logic_error("bad call to value()");
        return {};
    }

    pair<BackpatchList, BackpatchList> Node::getBackpatchLists() {
//        throw std::logic_error("bad call to getBackpatchLists()");
        return {};
    }
}