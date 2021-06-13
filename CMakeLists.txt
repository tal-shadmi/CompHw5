cmake_minimum_required(VERSION 3.10)
project(CompHw5)

set(CMAKE_CXX_STANDARD 17)

add_compile_options(-Wall -pedantic)

add_executable(${CMAKE_PROJECT_NAME} output.cpp parser.tab.cpp lex.yy.c AST.cpp SymbolTable.cpp bp.cpp)

add_custom_command(
        # Generates Flex file
        OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/lex.yy.c
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND flex scanner.lex
        DEPENDS scanner.lex
)

add_custom_command(
        # Generates Bison files
        OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/parser.tab.cpp
        BYPRODUCTS ${CMAKE_CURRENT_SOURCE_DIR}/parser.tab.hpp
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND bison -d parser.ypp
        DEPENDS parser.ypp
)

#add_custom_command(
#        # Copy executable to source folder
#        OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/hw3
#        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
#        COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_PROJECT_NAME} ./hw5
#        DEPENDS ${CMAKE_PROJECT_NAME}
#)
#
#add_custom_target(
#        # Build this target to run tests
#        tests1
#        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests
#        COMMAND ./run.bash
#        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/hw3
#)


#add_custom_target(
#        all-tests
#)

#add_dependencies(all-tests tests1 tests2 tests3 oy-tests whatsapp-tests)

add_custom_target(
        Clean
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND rm -f lex.yy.c parser.tab.*pp hw3
)

# compile lex.yy.c as c++
set_source_files_properties(lex.yy.c PROPERTIES LANGUAGE CXX )

