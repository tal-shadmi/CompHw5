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

static string getRegisterName() {
    return "%r" + to_string(register_num++);
}

namespace CodeGen {

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

    string load_variable(const string &register_name) {
        string new_reg = getRegisterName();
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        cmd << new_reg << " = " << "load i32, i32* " << register_name;
        buffer.emit(cmd.str());
        return new_reg;
    }

    pair<BackpatchList, BackpatchList> relop(const string &action, bool isSigned, const string &x, const string &y) {
        string new_reg = getRegisterName();
        auto &buffer = CodeBuffer::instance();
        stringstream cmd;
        if (isSigned) {
            cmd << new_reg << " = " << "icmp " << RelOpMapSigned.at(action) << " i32 " << x << ", " << y;
        }
        else {
            cmd << new_reg << " = " << "icmp " << RelOpMapUnsigned.at(action) << " i32 " << x << ", " << y;
        }
        buffer.emit(cmd.str());
        cmd.str("");
        cmd.clear();
        cmd << "br i1 " << new_reg << ", label " << "@" << ", " << ", label " << "@";
        int loc = buffer.emit(cmd.str());
        return {CodeBuffer::makelist({loc, FIRST}), CodeBuffer::makelist({loc, SECOND})};
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
