#ifndef CODEGENERATION_H
#define CODEGENERATION_H

#include <string>
#include "bp.hpp"
#include "SymbolTable.hpp"

using std::string;

namespace CodeGen {
    string getRegisterName();

    string getGlobalName();

    string create_variable(const string &value, SymbolTable::Type type);

    string create_global_string(const string &value);

    void store_variable(const string &register_name, const string &value, SymbolTable::Type type);

    string load_variable(const string &register_name, SymbolTable::Type type);

    string relop(const string &action, bool isSigned, const string &x, const string &y);

    string arithmetic(const string &action, int type, const string &x, const string &y);

    string getBoolFromLists(BackpatchList &true_list, BackpatchList &false_list);

    pair<BackpatchList, BackpatchList> getListsFromBool(const string &value);

    pair<BackpatchList, BackpatchList> andOr(const string &action, const string &midlabel, const pair<BackpatchList, BackpatchList> &x, const pair<BackpatchList, BackpatchList> &y);

    string fromByteToInt(const string &value);
}

#endif //CODEGENERATION_H
