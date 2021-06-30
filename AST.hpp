#ifndef _AST_H
#define _AST_H

#include <string>
#include <vector>
#include <memory>
#include <utility>
#include "SymbolTable.hpp"
#include "bp.hpp"

using std::unique_ptr;
using std::move;
using std::vector;
using std::string;



namespace AST {

    using Type = SymbolTable::Type;

    class Node {
    protected:
        Node(Type type, string name): type(type), name(move(name)) {}
    public:
        virtual ~Node() = default;
        Type type;
        string name;
        vector<unique_ptr<Node>> children;
        virtual string value();
        virtual pair<BackpatchList, BackpatchList> getBackpatchLists();
    };

    class BreakContinueMixin {
    protected:
        BackpatchList break_list;
        BackpatchList continue_list;
    public:
        pair<BackpatchList, BackpatchList> getBreakContinueLists();
        static pair<BackpatchList, BackpatchList> getListsFromNode(Node *node);
        static pair<BackpatchList, BackpatchList> getListsFromNode(Node *node1, Node *node2);
    };

    class ProgramNode : public Node {
        public:
        explicit ProgramNode(unique_ptr<Node> funcs);
        ~ProgramNode() override = default;
    };

    class FuncsNode : public Node {
        public:
        FuncsNode();
        FuncsNode(unique_ptr<Node> funcsImpl, unique_ptr<Node> funcs);
        ~FuncsNode() override = default;
    };

    class FuncsSignNode : public Node {
    public:
        FuncsSignNode(unique_ptr<Node> retType, unique_ptr<Node> id, unique_ptr<Node> formals);
        ~FuncsSignNode() override = default;
    };

    class FuncsImplNode : public Node {
        public:
        FuncsImplNode(unique_ptr<Node> sign_node, unique_ptr<Node> statements);
        ~FuncsImplNode() override = default;
    };

    class RetTypeNode : public Node {
        public:
        RetTypeNode();
        explicit RetTypeNode(unique_ptr<Node> type_node);
        ~RetTypeNode() override = default;
    };

    class FormalsNode : public Node {
        public:
        FormalsNode();
        explicit FormalsNode(unique_ptr<Node> formals_list);
        ~FormalsNode() override = default;
    };
    
    class FormalsListNode : public Node {
        public:
        explicit FormalsListNode(unique_ptr<Node> formal_decl);
        FormalsListNode(unique_ptr<Node> formal_decl, unique_ptr<Node> formals_list);
        ~FormalsListNode() override = default;
    };
    
    class FormalDeclNode : public Node {
        public:
        FormalDeclNode(unique_ptr<Node> type_node, unique_ptr<Node> id);
        ~FormalDeclNode() override = default;
    };

    class StatementsNode : public Node, public BreakContinueMixin {
        public:
        explicit StatementsNode(unique_ptr<Node> Statement);
        StatementsNode(unique_ptr<Node> Statements, unique_ptr<Node> Statement);
        ~StatementsNode() override = default;
    };

    class StatementsToStatementNode : public Node, public BreakContinueMixin {
        public:
        explicit StatementsToStatementNode(unique_ptr<Node> Statements);
        ~StatementsToStatementNode() override = default;
    };

    class StatementDeclareVariableNode : public Node {
        public:
        StatementDeclareVariableNode(unique_ptr<Node> type_node, unique_ptr<Node> id);
        StatementDeclareVariableNode(unique_ptr<Node> type_node, unique_ptr<Node> id, unique_ptr<Node> exp);
        ~StatementDeclareVariableNode() override = default;
    };

    class StatementAssignNode : public Node {
        public:
        StatementAssignNode(unique_ptr<Node> id, unique_ptr<Node> exp);
        ~StatementAssignNode() override = default;
    };

    class StatementReturnNode : public Node {
        public:
        StatementReturnNode();
        explicit StatementReturnNode(unique_ptr<Node> exp);
        ~StatementReturnNode() override = default;
    };

    class StatementIfElseNode : public Node, public BreakContinueMixin {
        public:
        StatementIfElseNode(unique_ptr<Node> decl, unique_ptr<Node> m, unique_ptr<Node> statement);
        StatementIfElseNode(unique_ptr<Node> decl, unique_ptr<Node> m1, unique_ptr<Node> if_statement, unique_ptr<Node> n, unique_ptr<Node> m2, unique_ptr<Node> else_statement);
        ~StatementIfElseNode() override = default;
    };

    class StatementWhileNode : public Node {
        public:
        StatementWhileNode(unique_ptr<Node> decl, unique_ptr<Node> m, unique_ptr<Node> statement);
        ~StatementWhileNode() override = default;
    };

    class StatementBreakContinue : public Node, public BreakContinueMixin {
        public:
        explicit StatementBreakContinue(const string &break_or_continue);
        ~StatementBreakContinue() override = default;
    };

    class StatementSwitchNode : public Node, public BreakContinueMixin {
        public:
        StatementSwitchNode(unique_ptr<Node> decl, unique_ptr<Node> n, unique_ptr<Node> case_list);
        ~StatementSwitchNode() override = default;
    };

    class IfDeclNode : public Node {
        BackpatchList true_list;
        BackpatchList false_list;
    public:
        explicit IfDeclNode(unique_ptr<Node> exp);
        ~IfDeclNode() override = default;
        pair<BackpatchList, BackpatchList> getBackpatchLists() override;
    };

    class WhileDeclNode : public Node {
        BackpatchList true_list;
        BackpatchList false_list;
    public:
        string label_to_bool_exp;
        explicit WhileDeclNode(unique_ptr<Node> n, unique_ptr<Node> m, unique_ptr<Node> exp);
        ~WhileDeclNode() override = default;
        pair<BackpatchList, BackpatchList> getBackpatchLists() override;
    };

    class SwitchDeclNode : public Node {
    public:
        explicit SwitchDeclNode(unique_ptr<Node> exp);
        ~SwitchDeclNode() override = default;
    };

    class CallNode : public Node {
        string result_reg;
        public:
        explicit CallNode(unique_ptr<Node> id_node);
        CallNode(unique_ptr<Node> id_node, unique_ptr<Node> exp_list);
        ~CallNode() override = default;
        string value() override;
        pair<BackpatchList, BackpatchList> getBackpatchLists() override;
    };

    class ExpListNode : public Node {
        public:
        vector<pair<string, Type>> arguments;
        explicit ExpListNode(unique_ptr<Node> exp);
        ExpListNode(unique_ptr<Node> exp_list, unique_ptr<Node> exp);
        ~ExpListNode() override = default;
    };

    class TypeNode : public Node {
        public:
        explicit TypeNode(Type type);
        ~TypeNode() override = default;
    };

    class BinOpNode : public Node {
        string result_reg;
        public:
        BinOpNode(unique_ptr<Node> exp1, unique_ptr<Node> op, unique_ptr<Node> exp2);
        ~BinOpNode() override = default;
        string value() override;
    };

    class BoolBinOpNode : public Node {
        BackpatchList true_list;
        BackpatchList false_list;
        public:
        BoolBinOpNode(unique_ptr<Node> exp1, unique_ptr<Node> op, unique_ptr<Node> N, unique_ptr<Node> M, unique_ptr<Node> exp2);
        ~BoolBinOpNode() override = default;
        string value() override;
        pair<BackpatchList, BackpatchList> getBackpatchLists() override;
    };

    class RelOpNode : public Node {
        string result_reg;
    public:
        RelOpNode(unique_ptr<Node> exp1, unique_ptr<Node> op, unique_ptr<Node> exp2);
        ~RelOpNode() override = default;
        string value() override;
        pair<BackpatchList, BackpatchList> getBackpatchLists() override;
    };

    class IDNode : public Node {
        public:
        explicit IDNode(string name);
        ~IDNode() override = default;
    };

    class IDExp : public Node {
        string result_reg;
    public:
        explicit IDExp(unique_ptr<Node> id);
        ~IDExp() override = default;
        string value() override;
        pair<BackpatchList, BackpatchList> getBackpatchLists() override;
    };

    class LiteralNode : public Node {
        string global_variable;
        public:
        LiteralNode(string name, Type type);
        ~LiteralNode() override = default;
        string value() override;
        pair<BackpatchList, BackpatchList> getBackpatchLists() override;
    };

    class NotNode : public Node {
        BackpatchList true_list;
        BackpatchList false_list;
        public:
        explicit NotNode(unique_ptr<Node> exp);
        ~NotNode() override = default;
        string value() override;
        pair<BackpatchList, BackpatchList> getBackpatchLists() override;
    };

    class CaseListNode : public Node, public BreakContinueMixin {
    public:
        explicit CaseListNode(unique_ptr<Node> case_decl);
        CaseListNode(unique_ptr<Node> case_decl, unique_ptr<Node> case_list);
        ~CaseListNode() override = default;
    };

    class DefaultCaseNode : public Node, public BreakContinueMixin {
    public:
        string label;
        BackpatchList next;
        explicit DefaultCaseNode(unique_ptr<Node> M, unique_ptr<Node> statements);
        ~DefaultCaseNode() override = default;
    };

    class CaseDeclNode : public Node, public BreakContinueMixin {
    public:
        string num;
        string label;
        BackpatchList next;
        explicit CaseDeclNode(unique_ptr<Node> num_node, unique_ptr<Node> M, unique_ptr<Node> statements);
        ~CaseDeclNode() override = default;
    };

    class MNode : public Node {
        string label;
    public:
        explicit MNode();
        ~MNode() override = default;
        string value() override;
    };

    class NNode : public Node {
        BackpatchList nextList;
    public:
        explicit NNode();
        ~NNode() override = default;
        pair<BackpatchList, BackpatchList> getBackpatchLists() override;
    };

    extern int switch_count;
    extern int while_count;
}
#endif