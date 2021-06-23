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

    string create_variable(const string &value) {
        string new_reg = getRegisterName();
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        cmd << new_reg << " = alloca i32";
        buffer.emit(cmd.str());
        cmd.str("");
        cmd.clear();
        cmd << "store i32 " << value << ", i32* " << new_reg;
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

    string arithmetic(const string &action, int type, const string &x, const string &y) {
        string new_reg = getRegisterName();
        stringstream cmd;
        cmd << new_reg << " = " << BinOpMap.at(action) << " i" << type << " " << x << ", " << y;
        auto &buffer = CodeBuffer::instance();
        buffer.emit(cmd.str());
        return new_reg;
    }


}
