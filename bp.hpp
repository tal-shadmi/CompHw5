#ifndef EX5_CODE_GEN
#define EX5_CODE_GEN

#include <vector>
#include <string>

using std::string;
using std::vector;
using std::pair;

//this enum is used to distinguish between the two possible missing labels of a conditional branch in LLVM during backpatching.
//for an unconditional branch (which contains only a single label) use FIRST.
enum BranchLabelIndex {FIRST, SECOND};

using BackpatchListItem = pair<int,BranchLabelIndex>;
using BackpatchList = vector<BackpatchListItem>;

class CodeBuffer{
	CodeBuffer();
	CodeBuffer(CodeBuffer const&);
    void operator=(CodeBuffer const&);
	vector<string> buffer;
	vector<string> globalDefs;
public:
	static CodeBuffer &instance();

	// ******** Methods to handle the code section ******** //

	//generates a jump location label for the next command, writes it to the buffer and returns it
	std::string genLabel();

	//writes command to the buffer, returns its location in the buffer
	int emit(const string &command);

	//gets a pair<int,BranchLabelIndex> item of the form {buffer_location, branch_label_index} and creates a list for it
	static BackpatchList makelist(BackpatchListItem item);

	//merges two lists of {buffer_location, branch_label_index} items
	static BackpatchList merge(const BackpatchList &l1, const BackpatchList &l2);

	/* accepts a list of {buffer_location, branch_label_index} items and a label.
	For each {buffer_location, branch_label_index} item in address_list, backpatches the branch command 
	at buffer_location, at index branch_label_index (FIRST or SECOND), with the label.
	note - the function expects to find a '@' char in place of the missing label.
	note - for unconditional branches (which contain only a single label) use FIRST as the branch_label_index.
	example #1:
	int loc1 = emit("br label @");  - unconditional branch missing a label. ~ Note the '@' ~
	bpatch(makelist({loc1,FIRST}),"my_label"); - location loc1 in the buffer will now contain the command "br label %my_label"
	note that index FIRST referes to the one and only label in the line.
	example #2:
	int loc2 = emit("br i1 %cond, label @, label @"); - conditional branch missing two labels.
	bpatch(makelist({loc2,SECOND}),"my_false_label"); - location loc2 in the buffer will now contain the command "br i1 %cond, label @, label %my_false_label"
	bpatch(makelist({loc2,FIRST}),"my_true_label"); - location loc2 in the buffer will now contain the command "br i1 %cond, label @my_true_label, label %my_false_label"
	*/
	void bpatch(const BackpatchList& address_list, const string &label);
	
	//prints the content of the code buffer to stdout
	void printCodeBuffer();

	// ******** Methods to handle the data section ******** //
	//write a line to the global section
	void emitGlobal(const string& dataLine);
	//print the content of the global buffer to stdout
	void printGlobalBuffer();

};


#endif

