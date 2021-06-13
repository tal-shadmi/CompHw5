#ifndef COMPHW3_PRINTTREE_H
#define COMPHW3_PRINTTREE_H

#include "AST.hpp"
#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;


namespace AST {
    class Printer {
    private:
        size_t dirs = 0;
        size_t files = 0;
        static constexpr std::array type {"INT", "BYTE", "BOOL", "STRING", "VOID"};

        vector<string> inner_pointers = {"├── ", "│   "};
        vector<string> final_pointers = {"└── ", "    "};

    public:
        void walk(AST::Node *node, string prefix) {
            for (size_t index = 0; index < node->children.size(); index++) {
                auto entry = node->children[index].get();
                vector<string> pointers = index == node->children.size() - 1 ? final_pointers : inner_pointers;

                cout << prefix << pointers[0] << type[(int)entry->type] << " : " << entry->name << endl;

                if (node->children.empty()) {
                    files++;
                } else {
                    dirs++;
                    walk(entry, prefix + pointers[1]);
                }
            }
        }

    };

}
#endif //COMPHW3_PRINTTREE_H
