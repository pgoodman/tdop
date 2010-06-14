
ROOT_DIR = ./
CXX = /Users/petergoodman/Code/llvm/Release-Asserts/bin/clang++
CXX_WARN_FLAGS = -pedantic -pedantic-errors -Wall -Werror -Wextra
CXX_FLAGS = -O0 -g -ansi -I${ROOT_DIR}
CXX_FLAGS += ${CXX_WARN_FLAGS}

# if using g++ then we can use some extra flags
ifeq (${CXX}, g++)
	CXX_FLAGS += -Wshadow -Wpointer-arith -Wcast-qual 
	CXX_FLAGS += -Wwrite-strings -Wfloat-equal -Wconversion 
	CXX_FLAGS += -Wredundant-decls -Wvolatile-register-var 
endif

all: calc

bin/%.o: %.cpp
	${CXX} ${CXX_FLAGS} -c $< -o $@

calc: bin/calc.o
	${CXX} $^ -o $@

clean:
	-rm bin/*.o
