cmake_minimum_required(VERSION 3.10)
project(CompHw5)

set(CMAKE_CXX_STANDARD 17)

add_compile_options(-Wall -pedantic)

add_executable(${CMAKE_PROJECT_NAME} output.cpp parser.tab.cpp lex.yy.c AST.cpp SymbolTable.cpp bp.cpp CodeGeneration.cpp)

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

  # TESTS

set(TEST_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/hw5-tests)

add_custom_target(
        exe-test
        # Copy executable to test directory
        WORKING_DIRECTORY ${TEST_DIRECTORY}
        COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_PROJECT_NAME} ./hw5
        DEPENDS ${CMAKE_PROJECT_NAME}
)

add_custom_target(
        # Build this target to run tests (only build don't run)
        TESTS
        WORKING_DIRECTORY ${TEST_DIRECTORY}
        COMMAND ./run.sh
        DEPENDS exe-test
)

add_custom_target(
        # Build this target to clean flex and bison output (only build don't run)
        CleanFlexBisonFiles
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND rm -f lex.yy.c parser.tab.*pp
)

add_custom_target(
        # Build this target to clean test directory (only build don't run)
        CleanTestsDir
        WORKING_DIRECTORY ${TEST_DIRECTORY}
        COMMAND rm -f *.res *.llvm hw5
)

# compile lex.yy.c as c++
set_source_files_properties(lex.yy.c PROPERTIES LANGUAGE CXX )
