//
// Created by yuval on 13/06/2021.
//

#include "CodeGeneration.hpp"
#include <sstream>
#include <unordered_map>


using namespace std;

static int register_num = 0;

static const unordered_map<string, string> RelOpMapUnsigned = {
        {"==", "eq"},
        {"!=", "ne"},
        {">", "ugt"},
        {">=", "uge"},
        {"<", "ult"},
        {"<=", "ule"},
};

static const unordered_map<string, string> RelOpMapSigned = {
        {"==", "eq"},
        {"!=", "ne"},
        {">", "sgt"},
        {">=", "sge"},
        {"<", "slt"},
        {"<=", "sle"}
};

static const unordered_map<string, string> BinOpMap = {
        {"+", "add"},
        {"-", "sub"},
        {"/", "div"},
        {"*", "mul"}

};


namespace CodeGen {

    string getRegisterName() {
        return "%r" + to_string(register_num++);
    }

    string getGlobalName() {
        return "@.r" + to_string(register_num++);
    }

    string create_global_string(const string &value) {
        string new_global = getGlobalName();
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        cmd << new_global << " = constant [4 x i8] " << value.substr(0, value.size() - 1) << "\\00\"";
        buffer.emitGlobal(cmd.str());
        return new_global;
    }

    string create_variable(const string &value, SymbolTable::Type type) {
        string new_reg = getRegisterName();
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        cmd << new_reg << " = alloca i32";
        buffer.emit(cmd.str());
        store_variable(new_reg, value, type);
        return new_reg;
    }

    string fromByteToInt(const string &value) {
        string new_reg = getRegisterName();
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        cmd << new_reg <<" = zext i8 " << value << " to i32";
        buffer.emit(cmd.str());
        return new_reg;
    }

    static int typeToNum(SymbolTable::Type type) {
        switch(type){
            case SymbolTable::Type::INT:
                return 32;
            case SymbolTable::Type::BYTE:
                return 8;
            case SymbolTable::Type::BOOL:
                return 1;
            default:
                throw std::logic_error("Bad type for variable");
        }
    }

    string load_variable(const string &register_name, SymbolTable::Type type) {
        string new_reg = getRegisterName();
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        cmd << new_reg << " = " << "load i32, i32* " << register_name;
        buffer.emit(cmd.str());
        cmd.str("");
        cmd.clear();
        int i = typeToNum(type);
        if (i == 1 || i == 8) {
            string mid_reg = getRegisterName();
            cmd << mid_reg << " = trunc i32 " << new_reg << " to i" << i;
            buffer.emit(cmd.str());
            new_reg = mid_reg;
        }
        return new_reg;
    }

    void store_variable(const string &register_name, const string &value, SymbolTable::Type type) {
        auto &buffer = CodeBuffer::instance();
        string temp_value = value;
        stringstream cmd;
        int i = typeToNum(type);
        if (i == 1 || i == 8) {
            string mid_reg = getRegisterName();
            cmd << mid_reg <<" = zext i" << i << " " << value << " to i32";
            buffer.emit(cmd.str());
            cmd.str("");
            cmd.clear();
            temp_value = mid_reg;
        }
        cmd << "store i32 " << temp_value << ", i32* " << register_name;
        buffer.emit(cmd.str());
    }

    string relop(const string &action, bool isSigned, const string &x, const string &y) {
        string new_reg = getRegisterName();
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        if (isSigned) {
            cmd << new_reg << " = " << "icmp " << RelOpMapSigned.at(action) << " i32 " << x << ", " << y;
        }
        else {
            cmd << new_reg << " = " << "icmp " << RelOpMapUnsigned.at(action) << " i8 " << x << ", " << y;
        }
        buffer.emit(cmd.str());
        return new_reg;
    }

    pair<BackpatchList, BackpatchList> andOr(const string &action, const string &midlabel, const pair<BackpatchList, BackpatchList> &x, const pair<BackpatchList, BackpatchList> &y) {
        auto &buffer = CodeBuffer::instance();
        BackpatchList true_list{};
        BackpatchList false_list{};
        if (action == "or") {
            buffer.bpatch(x.second, midlabel);
            true_list = CodeBuffer::merge(x.first, y.first);
            false_list = y.second;
        }
        else {
            buffer.bpatch(x.first, midlabel);
            true_list = y.first;
            false_list = CodeBuffer::merge(x.second, y.second);
        }
        return { true_list, false_list };
    }

    string arithmetic(const string &action, int type, const string &x, const string &y) {
        string new_reg = getRegisterName();
        stringstream cmd;
        cmd << new_reg << " = " << BinOpMap.at(action) << " i" << type << " " << x << ", " << y;
        auto &buffer = CodeBuffer::instance();
        buffer.emit(cmd.str());
        return new_reg;
    }

    string getBoolFromLists(BackpatchList &true_list, BackpatchList &false_list) {
        auto &buffer = CodeBuffer::instance();
        string true_label = buffer.genLabel();
        vector<BackpatchListItem> address_list{};
        address_list.emplace_back(buffer.emit("br label @"), FIRST);
        string false_label = buffer.genLabel();
        address_list.emplace_back(buffer.emit("br label @"), FIRST);
        string end_label = buffer.genLabel();
        buffer.bpatch(address_list, end_label);
        buffer.bpatch(true_list, true_label);
        buffer.bpatch(false_list, false_label);
        true_list.clear();
        false_list.clear();
        string new_reg = getRegisterName();
        stringstream cmd;
        cmd << new_reg << " =  phi i1 [0, %" << false_label << "], [1, %" << true_label << "]";
        buffer.emit(cmd.str());
        return new_reg;
    }

    pair<BackpatchList, BackpatchList> getListsFromBool (const string &value) {
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        cmd.str("");
        cmd.clear();
        cmd << "br i1 " << value << ", label " << "@" << ", " << ", label " << "@";
        int loc = buffer.emit(cmd.str());
        return {CodeBuffer::makelist({loc, FIRST}), CodeBuffer::makelist({loc, SECOND})};
    }

}
