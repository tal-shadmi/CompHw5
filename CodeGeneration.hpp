#ifndef CODEGENERATION_H
#define CODEGENERATION_H

#include <string>
#include "bp.hpp"

using std::string;

namespace CodeGen {

    string create_variable(const string &value);

    string load_variable(const string &register_name);

    pair<BackpatchList, BackpatchList> relop(const string &action, bool isSigned, const string &x, const string &y);

//    pair<BackpatchList, BackpatchList> AND(const string &x, const string &y);
//
//    pair<BackpatchList, BackpatchList> OR(const string &x, const string &y);
//
//    pair<BackpatchList, BackpatchList> NOT(const string &x);

    string arithmetic(const string &action, int type, const string &x, const string &y);

}

#endif //CODEGENERATION_H
