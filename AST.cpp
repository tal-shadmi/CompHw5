#include "AST.hpp"
#include <algorithm>
#include <numeric>
#include "output.hpp"
#include <sstream>
#include "CodeGeneration.hpp"
#include <iostream>
#include <optional>

extern int yylineno;


static constexpr std::array type_name{"INT", "BYTE", "BOOL", "STRING", "VOID"};

static constexpr std::array type_llvm{"i32", "i8", "i1", "i8*", "void"};

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
        for (auto &func :funcs->children) {
            auto &signature = func->children[0];
            auto &ret_type = signature->children[0];
            auto &id = signature->children[1];
            auto &args = signature->children[2];
            if (id->name == "main" && ret_type->type == SymbolTable::Type::VOID && args->children.empty()) {
                main = true;
                break;
            }
        }

        if (!main) {
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
        for (auto &child : funcs->children) {
            children.push_back(move(child));
        }
    }

    FuncsSignNode::FuncsSignNode(unique_ptr<Node> retType,
                                 unique_ptr<Node> id, unique_ptr<Node> formals) : Node(retType->type,
                                                                                       "Function Signature") {
        auto &st = SymbolTable::getInstance();
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        cmd << "define " << type_llvm[static_cast<int>(retType->type)] << " @" << id->name << " (";
        if (!st.FindID(id->name).type.empty()) {
            output::errorDef(yylineno, id->name);
            exit(1);
        }
        vector<Type> types;
        types.push_back(retType->type);
        if (formals->children.empty())
            types.push_back(Type::VOID);
        else
            for (auto &f : formals->children)
                types.push_back(f->type);
        st.AddFunction(id->name, types);
        st.OpenScope();
        bool first = true;
        for (auto &f : formals->children) {
            if (!st.FindID(f->name).type.empty()) {
                output::errorDef(yylineno, f->name);
                exit(1);
            }
            st.AddArgument(f->name, f->type);
            if (!first)
                cmd << ", ";
            cmd << type_llvm[static_cast<int>(f->type)];
            first = false;
        }
        cmd << ") {";
        buffer.emit(cmd.str());
        children.push_back(move(retType));
        children.push_back(move(id));
        children.push_back(move(formals));
    }

    FuncsImplNode::FuncsImplNode(unique_ptr<Node> sign_node, unique_ptr<Node> statements)
            : Node(Type::VOID, "Function Implementation") {
        auto &buffer = CodeBuffer::instance();
        auto return_node_ptr = dynamic_cast<StatementReturnNode*>(statements->children.back().get());
        if (return_node_ptr == nullptr) {
            if (sign_node->type == Type::VOID) {
                buffer.emit("ret void");
            }
            else {
                stringstream cmd;
                cmd << "ret " << type_llvm[static_cast<int>(sign_node->type)] << " 0";
                buffer.emit(cmd.str());
            }
        }
        children.push_back(move(sign_node));
        children.push_back(move(statements));
        SymbolTable::getInstance().CloseScope();
        buffer.emit("}");
    }

    RetTypeNode::RetTypeNode()
            : Node(Type::VOID, "Function Return Type") {}

    RetTypeNode::RetTypeNode(unique_ptr<Node> type_node)
            : Node(type_node->type, "Function Return Type") {}

    FormalsNode::FormalsNode() : Node(Type::VOID, "Formals") {}

    FormalsNode::FormalsNode(unique_ptr<Node> formals_list)
            : Node(Type::VOID, "Formals") {
        children = move(formals_list->children);
    }

    FormalsListNode::FormalsListNode(unique_ptr<Node> formal_decl)
            : Node(Type::VOID, "FormalsList") {
        children.push_back(move(formal_decl));
    }

    FormalsListNode::FormalsListNode(unique_ptr<Node> formal_decl, unique_ptr<Node> formals_list)
            : Node(Type::VOID, "FormalsList") {
        children.push_back(move(formal_decl));
        for (auto &ptr : formals_list->children)
            children.push_back(move(ptr));
    }

    FormalDeclNode::FormalDeclNode(unique_ptr<Node> type_node, unique_ptr<Node> id)
            : Node(type_node->type, id->name) {}


    StatementsNode::StatementsNode(unique_ptr<Node> statement) : Node(Type::VOID, "Statements") {
        std::tie(this->break_list, this->continue_list) = BreakContinueMixin::getListsFromNode(statement.get());
        children.push_back(move(statement));
    }

    StatementsNode::StatementsNode(unique_ptr<Node> statements,
                                   unique_ptr<Node> statement) : Node(Type::VOID, "Statements") {
        std::tie(this->break_list, this->continue_list) = BreakContinueMixin::getListsFromNode(statements.get(), statement.get());
        for (auto &child : statements->children) {
            children.push_back(move(child));
        }
        children.push_back(move(statement));
    }

    StatementsToStatementNode::StatementsToStatementNode(unique_ptr<Node> statements) : Node(Type::VOID,
                                                                                             "{ statements }") {
        std::tie(this->break_list, this->continue_list) = BreakContinueMixin::getListsFromNode(statements.get());
        for (auto &child : statements->children) {
            children.push_back(move(child));
        }
    }

    StatementDeclareVariableNode::StatementDeclareVariableNode(unique_ptr<Node> type_node, unique_ptr<Node> id) : Node(
            Type::VOID, "declare new variable") {
        auto &st = SymbolTable::getInstance();
        vector<SymbolTable::Type> type = st.FindID(id->name).type;
        if (type.empty()) {
            string reg_name = CodeGen::create_variable("0", type_node->type);
            st.AddVariable(id->name, type_node->type, reg_name);
        } else {
            output::errorDef(yylineno, id->name);
            exit(1);
        }
        children.push_back(move(type_node));
        children.push_back(move(id));
    }

    StatementDeclareVariableNode::StatementDeclareVariableNode(unique_ptr<Node> type_node, unique_ptr<Node> id,
                                                               unique_ptr<Node> exp) : Node(Type::VOID,
                                                                                            "declare new variable statement + value assignment") {
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
        } else {
            output::errorDef(yylineno, id->name);
            exit(1);
        }
        children.push_back(move(type_node));
        children.push_back(move(id));
        children.push_back(move(exp));
    }

    StatementAssignNode::StatementAssignNode(unique_ptr<Node> id, unique_ptr<Node> exp) : Node(id->type,
                                                                                               "assign value to predeclared variable") {
        auto &st = SymbolTable::getInstance();
        auto &entry = st.FindID(id->name);
        const vector<SymbolTable::Type> &type = entry.type;
        string value = exp->value();
        if (type.empty() || type.size() > 1) {
            output::errorUndef(yylineno, id->name);
            exit(1);
        } else if (!castable(exp->type, type[0])) {
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
        if (st.getFunctionType() != Type::VOID) {
            output::errorMismatch(yylineno);
            exit(1);
        }
        CodeBuffer::instance().emit("ret void");
    }

    StatementReturnNode::StatementReturnNode(unique_ptr<Node> exp)
            : Node(exp->type, "non void return statement") {
        auto &st = SymbolTable::getInstance();
        auto function_type = st.getFunctionType();
        if (!castable(type, function_type)) {
            output::errorMismatch(yylineno);
            exit(1);
        }
        auto [ret_type, ret_value] = pair{exp->type, exp->value()};
        if (function_type == Type::INT && ret_type == Type::BYTE) {
            ret_value = CodeGen::fromByteToInt(ret_value);
            ret_type = Type::INT;
        }
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        cmd << "ret " << type_llvm[static_cast<int>(ret_type)] << " " << ret_value;
        buffer.emit(cmd.str());
        children.push_back(move(exp));
    }

    StatementIfElseNode::StatementIfElseNode(unique_ptr<Node> decl, unique_ptr<Node> m, unique_ptr<Node> statement)
            : Node(Type::VOID, "if statement") {
        std::tie(this->break_list, this->continue_list) = BreakContinueMixin::getListsFromNode(statement.get());
        auto &buffer = CodeBuffer::instance();
        auto[true_list, false_list] = decl->getBackpatchLists();
        buffer.bpatch(true_list, m->value());
        int jump = buffer.emit("br label @");
        string end_label = buffer.genLabel();
        buffer.bpatch(false_list, end_label);
        buffer.bpatch(CodeBuffer::makelist({jump, FIRST}), end_label);
        children.push_back(move(decl->children.front()));
        children.push_back(move(statement));
    }

    StatementIfElseNode::StatementIfElseNode(unique_ptr<Node> decl, unique_ptr<Node> m1, unique_ptr<Node> if_statement,
                                             unique_ptr<Node> n, unique_ptr<Node> m2, unique_ptr<Node> else_statement)
            : Node(Type::VOID, "if else statement") {
        std::tie(this->break_list, this->continue_list) = BreakContinueMixin::getListsFromNode(if_statement.get(), else_statement.get());
        auto &buffer = CodeBuffer::instance();
        auto[true_list, false_list] = decl->getBackpatchLists();
        buffer.bpatch(true_list, m1->value());
        buffer.bpatch(false_list, m2->value());
        int else_jump = buffer.emit("br label @");
        string end_label = buffer.genLabel();
        buffer.bpatch(CodeBuffer::makelist({else_jump, FIRST}), end_label);
        buffer.bpatch(n->getBackpatchLists().first, end_label);
        children.push_back(move(decl->children.front()));
        children.push_back(move(if_statement));
        children.push_back(move(else_statement));
    }

    StatementWhileNode::StatementWhileNode(unique_ptr<Node> decl, unique_ptr<Node> m, unique_ptr<Node> statement)
            : Node(Type::VOID, "while loop") {
        auto &buffer = CodeBuffer::instance();
        auto[true_list, false_list] = decl->getBackpatchLists();
        buffer.bpatch(true_list, m->value());
        auto declaration = dynamic_cast<WhileDeclNode *>(decl.get());
        stringstream cmd;
        cmd << "br label %" << declaration->label_to_bool_exp;
        buffer.emit(cmd.str());
        string end_label = buffer.genLabel();
        buffer.bpatch(false_list, end_label);
        auto break_continue = dynamic_cast<BreakContinueMixin*>(statement.get());
        if(break_continue) {
            auto [break_list, continue_list] = break_continue->getBreakContinueLists();
            buffer.bpatch(break_list, end_label);
            buffer.bpatch(continue_list, declaration->label_to_bool_exp);
        }
        children.push_back(move(decl->children.front()));
        children.push_back(move(statement));
    }

    StatementBreakContinue::StatementBreakContinue(const string &break_or_continue) : Node(Type::VOID,
                                                                                           break_or_continue +
                                                                                           " statement") {
        if (break_or_continue == "break" && while_count <= 0 && switch_count <= 0) {
            output::errorUnexpectedBreak(yylineno);
            exit(1);
        } else if (break_or_continue == "continue" && while_count <= 0) {
            output::errorUnexpectedContinue(yylineno);
            exit(1);
        }
        int loc = CodeBuffer::instance().emit("br label @");
        if(break_or_continue == "break") {
            this->break_list = CodeBuffer::makelist({loc, FIRST});
        } else {
            this->continue_list = CodeBuffer::makelist({loc, FIRST});
        }

    }

    StatementSwitchNode::StatementSwitchNode(unique_ptr<Node> decl, unique_ptr<Node> n, unique_ptr<Node> case_list) : Node(Type::VOID,
                                                                                                       "switch statement") {
        auto &buffer = CodeBuffer::instance();
        string switch_label = buffer.genLabel();
        auto &exp = decl->children[0];
        stringstream cases_str;
        cases_str << "[ ";
        std::optional<string> default_label = nullopt;
        BackpatchList last_next_list{};
        for(auto &c : case_list->children){
            auto c_ptr = dynamic_cast<CaseDeclNode*>(c.get());
            string label;
            BackpatchList next_list;
            if (c_ptr) {
                cases_str << type_llvm[static_cast<int>(exp->type)] << " " << c_ptr->num << ", label %" << c_ptr->label << " ";
                label = c_ptr->label;
                next_list = c_ptr->next;
            }
            else {
                auto d_ptr = dynamic_cast<DefaultCaseNode*>(c.get());
                if (d_ptr) {
                    default_label = d_ptr->label;
                    label = d_ptr->label;
                    next_list = d_ptr->next;
                }
            }
            buffer.bpatch(last_next_list, label);
            last_next_list.swap(next_list);
        }
        cases_str << "]";
        stringstream cmd;
        cmd << "switch " << type_llvm[static_cast<int>(exp->type)] << " " << exp->value() << ", label @ " << cases_str.str();
        int loc = buffer.emit(cmd.str());
        buffer.bpatch(n->getBackpatchLists().first, switch_label);
        string end_label = buffer.genLabel();
        auto [_break_list, _continue_list] = BreakContinueMixin::getListsFromNode(case_list.get());
        this->continue_list = _continue_list;
        buffer.bpatch(_break_list, end_label);
        buffer.bpatch({{loc, FIRST}}, default_label.value_or(end_label));
        buffer.bpatch(last_next_list, end_label);
        children.push_back(move(decl->children.front()));
        children.push_back(move(case_list));
    }

    IfDeclNode::IfDeclNode(unique_ptr<Node> exp) : Node(Type::VOID, "if declaration") {
        if (exp->type != Type::BOOL) {
            output::errorMismatch(yylineno);
            exit(1);
        }
        std::tie(true_list, false_list) = exp->getBackpatchLists();
        children.push_back(move(exp));
    }

    pair<BackpatchList, BackpatchList> IfDeclNode::getBackpatchLists() {
        return {true_list, false_list};
    }

    WhileDeclNode::WhileDeclNode(unique_ptr<Node> n, unique_ptr<Node> m, unique_ptr<Node> exp) : Node(Type::VOID,
                                                                                                      "while declaration") {
        if (exp->type != Type::BOOL) {
            output::errorMismatch(yylineno);
            exit(1);
        }
        auto &buffer = CodeBuffer::instance();
        std::tie(true_list, false_list) = exp->getBackpatchLists();
        this->label_to_bool_exp = m->value();
        buffer.bpatch(n->getBackpatchLists().first, label_to_bool_exp);
        children.push_back(move(exp));
    }

    pair<BackpatchList, BackpatchList> WhileDeclNode::getBackpatchLists() {
        return {true_list, false_list};
    }

    SwitchDeclNode::SwitchDeclNode(unique_ptr<Node> exp) : Node(Type::VOID, "switch declaration") {
        if (!numType(exp->type)) {
            output::errorMismatch(yylineno);
            exit(1);
        }
        children.push_back(move(exp));
    }

    CallNode::CallNode(unique_ptr<Node> id_node) : Node(Type::VOID, "Function Call") {
        auto &st = SymbolTable::getInstance();
        const vector<SymbolTable::Type> &type_list = st.FindID(id_node->name).type;
        if (type_list.empty() || type_list.size() < 2) {
            output::errorUndefFunc(yylineno, id_node->name);
            exit(1);
        }
        if (type_list[1] != Type::VOID) {
            vector<string> type_strs;
            std::transform(type_list.begin() + 1, type_list.end(), std::back_inserter(type_strs),
                           [](auto &t) { return type_name[(int) t]; });
            output::errorPrototypeMismatch(yylineno, id_node->name, type_strs);
            exit(1);
        }
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        if(type_list[0] != Type::VOID) {
            string new_reg = CodeGen::getRegisterName();
            cmd << new_reg << " = ";
            this->result_reg = new_reg;
        }
        cmd << "call " << type_llvm[static_cast<int>(type_list[0])] << " @" << id_node->name << "()";
        buffer.emit(cmd.str());
        children.push_back(move(id_node));
        this->type = type_list[0];
    }

    CallNode::CallNode(unique_ptr<Node> id_node, unique_ptr<Node> exp_list) : Node(Type::VOID, "Function Call") {
        auto &st = SymbolTable::getInstance();
        const auto &type_list = st.FindID(id_node->name).type;
        if (type_list.empty() || type_list.size() < 2) {
            output::errorUndefFunc(yylineno, id_node->name);
            exit(1);
        }
        bool mismatch = type_list.size() - 1 != exp_list->children.size()
                        || std::mismatch(exp_list->children.begin(), exp_list->children.end(),
                                         type_list.begin() + 1, [](auto &x, auto &y) { return castable(x->type, y); }).first
                           != exp_list->children.end();
        if (mismatch) {
            vector<string> type_strs;
            std::transform(type_list.begin() + 1, type_list.end(), std::back_inserter(type_strs),
                           [](auto &t) { return type_name[(int) t]; });
            output::errorPrototypeMismatch(yylineno, id_node->name, type_strs);
            exit(1);
        }
        auto &buffer = CodeBuffer::instance();
        string type_list_str = std::accumulate(type_list.begin() + 1, type_list.end(), string{},
                                           [&](const string &a, Type b) {
                                               return a + "," + type_llvm[static_cast<int>(b)];
                                           }).substr(1);
        stringstream arg_list;
        int i = 0;
        for(const auto & child : exp_list->children){
            if(i++ != 0) arg_list << ", ";
            auto [child_type, child_value] = pair{child->type, child->value()};
            if (type_list[i] == Type::INT && child_type == Type::BYTE) {
                child_value = CodeGen::fromByteToInt(child_value);
                child_type = Type::INT;
            }
            arg_list << type_llvm[static_cast<int>(child_type)] << " " << child_value;
        }

        stringstream cmd;
        if(type_list[0] != Type::VOID) {
            string new_reg = CodeGen::getRegisterName();
            cmd << new_reg << " = ";
            this->result_reg = new_reg;
        }
        cmd << "call " << type_llvm[static_cast<int>(type_list[0])] << " (" << type_list_str << ") @" << id_node->name << "(" << arg_list.str() << ")";
        buffer.emit(cmd.str());
        children.push_back(move(id_node));
        children.push_back(move(exp_list));
        this->type = type_list[0];
    }

    string CallNode::value() {
        return this->result_reg;
    }

    pair<BackpatchList, BackpatchList> CallNode::getBackpatchLists() {
        if(type != Type::BOOL)
            throw logic_error("called getBackpatchLists() on non-bool value");
        return CodeGen::getListsFromBool(result_reg);
    }

    ExpListNode::ExpListNode(unique_ptr<Node> exp) : Node(Type::VOID, "ExpList") {
        children.push_back(move(exp));
    }

    ExpListNode::ExpListNode(unique_ptr<Node> exp, unique_ptr<Node> exp_list) : Node(Type::VOID, "ExpList") {
        children.push_back(move(exp));
        for (auto &child : exp_list->children) {
            children.push_back(move(child));
        }
    }

    TypeNode::TypeNode(Type type) : Node(type, "Type") {}

    BinOpNode::BinOpNode(unique_ptr<Node> exp1, unique_ptr<Node> op, unique_ptr<Node> exp2) : Node(Type::VOID,
                                                                                                   "Binary Operation") {
        string value1 = exp1->value();
        string value2 = exp2->value();

        if (exp1->type == exp2->type && (exp1->type == Type::INT || exp1->type == Type::BYTE)) {
            type = exp1->type;
        } else if (exp1->type == Type::BYTE && exp2->type == Type::INT) {
            type = Type::INT;
            value1 = CodeGen::fromByteToInt(value1);
        } else if (exp2->type == Type::BYTE && exp1->type == Type::INT) {
            type = Type::INT;
            value2 = CodeGen::fromByteToInt(value2);
        } else {
            output::errorMismatch(yylineno);
            exit(1);
        }
        this->result_reg = CodeGen::arithmetic(op->name, type == Type::INT ? 32 : 8, value1, value2);
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
        } else if (exp2->type == Type::BYTE && exp1->type == Type::INT) {
            value2 = CodeGen::fromByteToInt(value2);
        } else if (!numType(exp1->type) || !numType(exp2->type)) {
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

    BoolBinOpNode::BoolBinOpNode(unique_ptr<Node> exp1, unique_ptr<Node> op, unique_ptr<Node> N, unique_ptr<Node> M,
                                 unique_ptr<Node> exp2) : Node(Type::BOOL, "Binary Boolean Operation") {
        if (exp1->type != Type::BOOL || exp2->type != Type::BOOL) {
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
        } else if (this->type == Type::STRING) {
            return this->global_variable;
        } else if(this->type == Type::BOOL ) {
            return this->name == "true" ? "1" : "0";
        }
        throw std::logic_error("literal is not from permitted type");
    }

    pair<BackpatchList, BackpatchList> LiteralNode::getBackpatchLists() {
        if(this->type != Type::BOOL )
            throw std::logic_error("called getBackpatchLists() on non-bool value");
        return CodeGen::getListsFromBool(this->value());
    }

    NotNode::NotNode(unique_ptr<Node> exp) : Node(Type::BOOL, "Not") {
        if (exp->type != Type::BOOL) {
            output::errorMismatch(yylineno);
            exit(1);
        }
        std::tie(this->false_list, this->true_list) = exp->getBackpatchLists();
        children.push_back(move(exp));
    }

    string NotNode::value() {
        return CodeGen::getBoolFromLists(true_list, false_list);
    }

    pair<BackpatchList, BackpatchList> NotNode::getBackpatchLists() {
        return { true_list, false_list};
    }

    CaseListNode::CaseListNode(unique_ptr<Node> case_decl) : Node(Type::VOID, "Case List for switch block") {
        std::tie(this->break_list, this->continue_list) = BreakContinueMixin::getListsFromNode(case_decl.get());
        children.push_back(move(case_decl));
    }

    CaseListNode::CaseListNode(unique_ptr<Node> case_decl, unique_ptr<Node> case_list) : Node(Type::VOID,
                                                                                              "Case List for switch block") {
        std::tie(this->break_list, this->continue_list) = BreakContinueMixin::getListsFromNode(case_decl.get(), case_list.get());
        children.push_back(move(case_decl));
        for (auto &child : case_list->children) {
            children.push_back(move(child));
        }
    }

    DefaultCaseNode::DefaultCaseNode(unique_ptr<Node> M, unique_ptr<Node> statements) : Node(Type::VOID, "default case") {
        std::tie(this->break_list, this->continue_list) = BreakContinueMixin::getListsFromNode(statements.get());
        auto &buffer = CodeBuffer::instance();
        this->label = M->value();
        int loc = buffer.emit("br label @");
        this->next = CodeBuffer::makelist({loc, FIRST});
        children.push_back(move(statements));
    }

    CaseDeclNode::CaseDeclNode(unique_ptr<Node> num_node, unique_ptr<Node> M, unique_ptr<Node> statements) : Node(Type::VOID, "case declaration") {
        std::tie(this->break_list, this->continue_list) = BreakContinueMixin::getListsFromNode(statements.get());
        auto &buffer = CodeBuffer::instance();
        this->label = M->value();
        this->num = num_node->value();
        int loc = buffer.emit("br label @");
        this->next = CodeBuffer::makelist({loc, FIRST});
        for (auto &child : statements->children) {
            children.push_back(move(child));
        }
    }

    IDExp::IDExp(unique_ptr<Node> id) : Node(id->type, move(id->name)) {
        auto &st = SymbolTable::getInstance();
        auto &entry = st.FindID(name);
        auto &type = entry.type;
        if (type.empty() || type.size() > 1) {
            output::errorUndef(yylineno, name);
            exit(1);
        }
        this->type = type[0];
        if(entry.offset >= 0)
            this->result_reg = CodeGen::load_variable(entry.register_name, this->type);
        else
            this->result_reg = entry.register_name;
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
        return {this->nextList, {}};
    }

    string Node::value() {
        throw std::logic_error("bad call to value()");
//        return {};
    }

    pair<BackpatchList, BackpatchList> Node::getBackpatchLists() {
        throw std::logic_error("bad call to getBackpatchLists()");
//        return {};
    }

    pair<BackpatchList, BackpatchList> BreakContinueMixin::getBreakContinueLists() {
        return { break_list, continue_list };
    }

    pair<BackpatchList, BackpatchList> BreakContinueMixin::getListsFromNode(Node *node) {
        auto break_or_continue = dynamic_cast<BreakContinueMixin*>(node);
        if(break_or_continue) {
            return break_or_continue->getBreakContinueLists();
        } else {
            return {};
        }
    }

    pair<BackpatchList, BackpatchList> BreakContinueMixin::getListsFromNode(Node *node1, Node *node2) {
        auto break_or_continue1 = dynamic_cast<BreakContinueMixin*>(node1);
        auto break_or_continue2 = dynamic_cast<BreakContinueMixin*>(node2);
        BackpatchList break_list{};
        BackpatchList continue_list{};
        if(break_or_continue1) {
            std::tie(break_list, continue_list) = break_or_continue1->getBreakContinueLists();
        }
        if(break_or_continue2) {
            auto [other_break_list, other_continue_list] = break_or_continue2->getBreakContinueLists();
            break_list = CodeBuffer::merge(break_list, other_break_list);
            continue_list = CodeBuffer::merge(continue_list, other_continue_list);
        }
        return {break_list, continue_list};
    }
}