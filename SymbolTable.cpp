#include <iostream>
#include "SymbolTable.hpp"
#include "algorithm"
#include <array>
#include "output.hpp"
#include <exception>

using std::cout;
using std::endl;

static const vector<SymbolTable::Type> no_result {};

static constexpr std::array type_name {"INT", "BYTE", "BOOL", "STRING", "VOID"};

class ArgumentAfterVariable: public exception
{
    [[nodiscard]] const char* what() const noexcept override
    {
        return "Argument added to Symbol Table after variable in local scope";
    }
};



SymbolTable::SymbolTable() {
    tables_stack.emplace_back(
        Table {
            Entry{ "print" , vector{ Type::VOID, Type::STRING }, 0},
            Entry{ "printi" , vector{ Type::VOID, Type::INT }, 0}
        }
    );
    offsets_stack.push_back(0);
}

void SymbolTable::OpenScope() {
    tables_stack.emplace_back();
    offsets_stack.push_back(offsets_stack.back());
}

void SymbolTable::CloseScope() {
    output::endScope();
    for (auto &entry : tables_stack.back()) {
        if (entry.type.size() == 1) {
            output::printID(entry.name, static_cast<int>(entry.offset), type_name[static_cast<int>(entry.type[0])]);
        }
        else {
            auto ret_type = type_name[static_cast<int>(entry.type[0])];
            vector<string> arguments_types;
            if(entry.type[1] != Type::VOID)
                std::transform(entry.type.begin()+1, entry.type.end(),
                           std::back_inserter(arguments_types),
                           [](Type t){ return type_name[static_cast<int>(t)];});
            output::printID(entry.name, static_cast<int>(entry.offset), output::makeFunctionType(ret_type, arguments_types));
        }
    }
    tables_stack.pop_back();
    offsets_stack.pop_back();
}

void SymbolTable::AddFunction(string name, vector<Type> types) {
    tables_stack.back().push_back( Entry{ move(name), move(types), 0 } );
}


void SymbolTable::AddVariable(string name, Type type, string register_name) {
    auto offset = offsets_stack.back()++;
    tables_stack.back().push_back( Entry{ move(name), { type }, offset, move(register_name) } );
}

void SymbolTable::AddArgument(string name, Type type) {
    auto &table = tables_stack.back();
    int offset;
    if(table.empty()){
        offset = -1;
    } else if (table.back().offset < 0) {
        offset = table.back().offset - 1;
    } else {
        throw ArgumentAfterVariable();
    }
    table.push_back( Entry{ move(name), { type }, offset } );
}

const vector<SymbolTable::Type> &SymbolTable::FindID(const string &name) {
    for(Table &table : tables_stack) {
        auto it = std::find_if(table.begin(),table.end(),
                     [&name](Entry &entry){ return entry.name == name;} );
        if(it != table.end()){
            return it->type;
        }
    }
    return no_result;
}

SymbolTable::Type SymbolTable::getFunctionType() {
    return tables_stack.front().back().type.front();
}
