/*
 * tdop.cpp
 *
 *  Created on: Jun 10, 2010
 *      Author: Peter Goodman
 *     Version: $Id$
 */

#include <iostream>
#include <cstdlib>
#include <cmath>

#include "tdop/Grammar.h"


typedef enum {
    PLUS = '+',
    INTEGER = 'i',
    POW = '^',
    POPEN = '(',
    PCLOSE = ')',
    MULT = '*',
    DIV = '/',
    MINUS = '-'
} Terminal;

class Token {
public:
    Terminal term;
    const char *lexeme;

    Token(Terminal t, const char *lex)
     : term(t), lexeme(lex) { }

    Token(Terminal t)
     : term(t), lexeme("") { }

    Token(void) { }

    Token &operator=(const Token &other) {
        term = other.term;
        lexeme = other.lexeme;
        return *this;
    }
};

static int add_node(int x, int y) {
    std::cout << "calling ADD(" << x << "," << y <<")\n";
    return x + y;
}

static int pos_node(int x) {
    std::cout << "calling POS(" << x <<")\n";
    return x;
}

static int pow_node(int x, int y) {
    std::cout << "calling POW(" << x <<"," << y << ")\n";
    return (int) pow((double) x, (double) y);
}

static int tok_to_int(Token tok) {
    std::cout << "calling TOK_TO_INT(\"" << tok.lexeme << "\")\n";
    return atoi(tok.lexeme);
}

static int paren_node(int x) {
    std::cout << "calling PAREN(" << x << ")\n";
    return x;
}

static Terminal tok_to_term(Token &tok) {
    return tok.term;
}

static void lex(std::vector<Token> &tokens, char *line) {
    tokens.clear();

    for(; *line != '\0'; ++line) {
        switch(*line) {
        case '(': tokens.push_back(Token(POPEN)); break;
        case ')': tokens.push_back(Token(PCLOSE)); break;
        case '+': tokens.push_back(Token(PLUS)); break;
        case '^': tokens.push_back(Token(POW)); break;
        case '*': tokens.push_back(Token(MULT)); break;
        case '/': tokens.push_back(Token(DIV)); break;
        case '-': tokens.push_back(Token(MINUS)); break;
        default:
            if('0' <= *line && '9' >= *line) {
                tokens.push_back(Token(INTEGER, line));
                for(; *line != '\0'; ++line) {
                    if('0' > *line || '9' < *line) {
                        --line;
                        break;
                    }
                }
            }
        }
    }
}

int main(void) {

    tdop::Grammar<int, Terminal, Token> g(&tok_to_term);
    tdop::Variable<int, Terminal, Token> left_expr(g);
    tdop::Variable<int, Terminal, Token> right_expr(g);
    tdop::Token<int, Terminal, Token> token_expr(g);

    // deal with converting a string to an integer
    g --> token_expr(INTEGER)
      >>= g.action(tok_to_int, token_expr);

    // addition operator, left associative
    g --> left_expr(10) | PLUS | right_expr(10)
      >>= g.action(add_node, left_expr, right_expr);

    // positive operator (essentially identity for integers)
    g --> PLUS | right_expr(9)
      >>= g.action(pos_node, right_expr);

    // exponentiation, right associative
    g --> left_expr(20) | POW | right_expr(19)
      >>= g.action(pow_node, left_expr, right_expr);

    // parentheses to change order of operations
    g --> POPEN | left_expr(0) | PCLOSE
      >>= g.action(paren_node, left_expr);

    std::vector<Token> str;

    char line[100];
    while(std::cin.good()) {

        // read in a line
        std::cout << ">>> ";
        std::cin.unsetf(std::ios_base::skipws);
        std::cin.getline(line, 100);
        lex(str, line);

        tdop::ParseResult<int, Terminal, Token> result(g.parse(str));

        if(!result.isError()) {
            std::cout << "result=" << result.getResult() << '\n';
        } else {
            std::cout << "error!\n";
            tdop::Error<int, Terminal, Token> err(result.getError());

            for(size_t i(0); i < err.partial_envs.size(); ++i) {
                std::cout << "partial result=" << err.partial_envs[i] << '\n';
            }

            if(err.type == tdop::NO_TOKENS_TO_PARSE) {
                std::cout << "no tokens to parse\n";
            } else if(err.type == tdop::MISSING_NULL_DENOTATION) {
                std::cout << "missing null denotation\n";
            } else if(err.type == tdop::MISSING_LEFT_DENOTATION) {
                std::cout << "missing left denotation\n";
            } else if(err.type == tdop::UNEXPECTED_END_OF_INPUT) {
                std::cout << "unexpect eoi\n";
            } else if(err.type == tdop::UNEXPECTED_TOKEN) {
                std::cout << "unexpected token\n";
            } else if(err.type == tdop::INCOMPLETE_PARSE) {
                std::cout << "incomplete parse.\n";
            } else {
                std::cout << "??\n";
            }
        }
    }

    return 0;
}
