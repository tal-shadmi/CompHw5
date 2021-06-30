#include "bp.hpp"
#include <vector>
#include <iostream>
#include <sstream>

using namespace std;

bool replace(string &str, const string &from, const string &to, BranchLabelIndex index);

CodeBuffer::CodeBuffer()
        : buffer({
                         "define void @printi(i32) {",
                         "%spec_ptr = getelementptr [4 x i8], [4 x i8]* @.int_specifier, i32 0, i32 0",
                         "call i32 (i8*, ...) @printf(i8* %spec_ptr, i32 %0)",
                         "ret void",
                         "}",
                         "",
                         "define void @print(i8*) {",
                         "%spec_ptr = getelementptr [4 x i8], [4 x i8]* @.str_specifier, i32 0, i32 0",
                         "call i32 (i8*, ...) @printf(i8* %spec_ptr, i8* %0)",
                         "ret void",
                         "}",
                         ""
                 }

),
          globalDefs({
                             "declare i32 @printf(i8*, ...)",
                             "declare void @exit(i32)",
                             R"(@.int_specifier = constant [4 x i8] c"%d\0A\00")",
                             R"(@.str_specifier = constant [4 x i8] c"%s\0A\00")",
                             R"(@.division_error = constant [23 x i8] c"Error division by zero\00")"
                     }) {}

CodeBuffer &CodeBuffer::instance() {
    static CodeBuffer inst;//only instance
    return inst;
}

string CodeBuffer::genLabel() {
    if(this->buffer.back().substr(0,6) == "label_"){
        string &s = this->buffer.back();
        return s.substr(0,s.size()-1);
    }
    std::stringstream label;
    label << "label_";
    label << buffer.size();
    std::string ret(label.str());
    label << ":";
    emit(label.str());
    return ret;
}

int CodeBuffer::emit(const string &s) {
    buffer.push_back(s);
    return buffer.size() - 1;
}

void CodeBuffer::bpatch(const vector<pair<int, BranchLabelIndex>> &address_list, const std::string &label) {
    for (vector<pair<int, BranchLabelIndex>>::const_iterator i = address_list.begin(); i != address_list.end(); i++) {
        int address = (*i).first;
        BranchLabelIndex labelIndex = (*i).second;
        replace(buffer[address], "@", "%" + label, labelIndex);
    }
}

void CodeBuffer::printCodeBuffer() {
    for (std::vector<string>::const_iterator it = buffer.begin(); it != buffer.end(); ++it) {
        cout << *it << endl;
    }
}

vector<pair<int, BranchLabelIndex>> CodeBuffer::makelist(pair<int, BranchLabelIndex> item) {
    vector<pair<int, BranchLabelIndex>> newList;
    newList.push_back(item);
    return newList;
}

vector<pair<int, BranchLabelIndex>>
CodeBuffer::merge(const vector<pair<int, BranchLabelIndex>> &l1, const vector<pair<int, BranchLabelIndex>> &l2) {
    vector<pair<int, BranchLabelIndex>> newList(l1.begin(), l1.end());
    newList.insert(newList.end(), l2.begin(), l2.end());
    return newList;
}

// ******** Methods to handle the global section ********** //
void CodeBuffer::emitGlobal(const std::string &dataLine) {
    globalDefs.push_back(dataLine);
}

void CodeBuffer::printGlobalBuffer() {
    for (vector<string>::const_iterator it = globalDefs.begin(); it != globalDefs.end(); ++it) {
        cout << *it << endl;
    }
}

// ******** Helper Methods ********** //
bool replace(string &str, const string &from, const string &to, BranchLabelIndex index) {
    size_t pos;
    if (index == SECOND) {
        pos = str.find_last_of(from);
    } else { //FIRST
        pos = str.find_first_of(from);
    }
    if (pos == string::npos)
        return false;
    str.replace(pos, from.length(), to);
    return true;
}

