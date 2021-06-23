#ifndef _SYMBOL_H
#define _SYMBOL_H

#include <stack>
#include <vector>
#include <string>

using std::string;
using std::vector;

class SymbolTable {

public:

    enum class Type {INT = 0, BYTE, BOOL, STRING, VOID};

    struct Entry {
        string name;
        vector<Type> type;
        int offset;
        string register_name;
    };

    using Table=vector<Entry>;

private:

    vector<Table> tables_stack;
    vector<int> offsets_stack;
    SymbolTable();

public:

    void OpenScope();

    void CloseScope();

    void AddFunction(string name, vector<Type> types);

    void AddVariable(string name, Type type, string register_name);

    void AddArgument(string name, Type type);

    const Entry &FindID(const string &name);

    Type getFunctionType();

    static SymbolTable& getInstance()
    {
        static SymbolTable instance;
        return instance;
    }

    SymbolTable(SymbolTable&) = delete;

    SymbolTable& operator=(const SymbolTable&) = delete;
};

#endif