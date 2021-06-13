//
// Created by yuval on 13/06/2021.
//

#include "CodeGeneration.hpp"
#include <sstream>
#include "bp.hpp"

using namespace std;

static int register_num = 0;


static string getRegisterName() {
    return "%" + to_string(register_num++);
}
namespace CodeGen {
    string arithmetic(const string &action, int type, const string &x, const string &y) {
        string new_reg = getRegisterName();
        stringstream cmd;
        cmd << new_reg << " = " << action << " i" << type << " " << x << ", " << y;
        auto &buffer = CodeBuffer::instance();
        buffer.emit(cmd.str());
        return new_reg;
    }

}
