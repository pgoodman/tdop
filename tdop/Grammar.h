/*
 * Grammar.h
 *
 *  Created on: Jun 10, 2010
 *      Author: Peter Goodman
 *     Version: $Id$
 */

#ifndef TDOP_GRAMMAR_H_
#define TDOP_GRAMMAR_H_

#include <map>
#include <set>
#include <vector>
#include <cassert>
#include <cstdarg>
#include <limits>

#include "Null.h"
#include "Variant.h"
#include "Function.h"

#define TDOP_BP_BASE 10

namespace tdop {

    /// classes
    template <typename EnvT, typename TermT, typename TokT=TermT>
    class Token;

    template <typename EnvT, typename TermT, typename TokT=TermT>
    class Variable;

    template <typename EnvT, typename TermT, typename TokT=TermT>
    class Denotation;

    template <typename EnvT, typename TermT, typename TokT=TermT>
    class Grammar;

    template <typename EnvT, typename TermT, typename TokT=TermT>
    class Error;

    typedef enum {
        NO_ERROR,
        NO_TOKENS_TO_PARSE,
        MISSING_NULL_DENOTATION,
        MISSING_LEFT_DENOTATION,
        UNEXPECTED_END_OF_INPUT,
        UNEXPECTED_TOKEN
    } ErrorType;

    namespace {

        /// represents a symbol within a denotation. symbols can either be
        /// terminals, variables representing terminals (Tokens), variables
        /// representing a binding environment, or variables representing
        /// a grammar.
        template <typename EnvT, typename TermT, typename TokT>
        class Symbol {

            typedef Variable<EnvT, TermT, TokT> VarT;
            typedef Grammar<EnvT, TermT, TokT> GrammarT;
            typedef Symbol<EnvT, TermT, TokT> SelfT;

        public:

            TermT terminal;
            unsigned variable_id;
            GrammarT *sub_grammar;
            unsigned binding_power;

            enum {
                Variable,
                Term,
                TermVariable,
                Uninitialized
            } state;

            Symbol(void)
             : variable_id(0)
             , sub_grammar(0)
             , binding_power(TDOP_BP_BASE)
             , state(Uninitialized) { }

            Symbol(const TermT term)
             : terminal(term)
             , variable_id(0)
             , sub_grammar(0)
             , binding_power(TDOP_BP_BASE)
             , state(Term) { }

            Symbol(const TermT term, const unsigned id)
             : terminal(term)
             , variable_id(id)
             , sub_grammar(0)
             , binding_power(TDOP_BP_BASE)
             , state(TermVariable) { }

            Symbol(const VarT &var)
             : variable_id(var.id)
             , sub_grammar(var.sub_grammar)
             , binding_power(var.binding_power + TDOP_BP_BASE)
             , state(Variable) { }

            ~Symbol(void) { }
        };

        /// the denotation builder handles most of the operator magic that
        /// makes building grammars easier.
        template <typename EnvT, typename TermT, typename TokT>
        class DenotationBuilder {

            typedef Grammar<EnvT, TermT, TokT> GrammarT;
            typedef Denotation<EnvT, TermT, TokT> DenotationT;
            typedef DenotationBuilder<EnvT, TermT, TokT> SelfT;
            typedef Token<EnvT, TermT, TokT> TokenT;
            typedef Variable<EnvT, TermT, TokT> VarT;

            friend class Grammar<EnvT, TermT, TokT>;

        private:

            GrammarT &grammar;
            DenotationT *temp_denotation;

            DenotationBuilder(GrammarT &g)
             : grammar(g)
             , temp_denotation(0) { }

        public:

            ~DenotationBuilder(void) {
                if(0 != temp_denotation) {
                    delete temp_denotation;
                    temp_denotation = 0;
                }
            }

            /// define the beginning of a left denotation
            SelfT &operator>(VarT &prefix_env) {
                assert(0 == temp_denotation);
                temp_denotation = new DenotationT(grammar, prefix_env);
                temp_denotation->operator|(prefix_env);
                return *this;
            }

            /// define a null denotation for a terminal
            DenotationT &operator>(const TermT term) {
                assert(0 == temp_denotation);
                return (grammar.assignDenotation(
                    term,
                    new DenotationT(grammar),
                    grammar.null_denotations
                ) | term);
            }

            /// define a null denotation for a terminal
            DenotationT &operator>(const TokenT &tok) {
                assert(0 == temp_denotation);
                assert(tok.is_initialized);
                return (grammar.assignDenotation(
                    tok.terminal,
                    new DenotationT(grammar),
                    grammar.null_denotations
                ) | tok);
            }

            /// finish off a left denotation by assigning it into the
            /// grammar now that we know the term
            DenotationT &operator|(const TermT term) {
                assert(0 != temp_denotation);
                DenotationT *d(temp_denotation);
                temp_denotation = 0;
                return (grammar.assignDenotation(
                    term, d, grammar.left_denotations
                ) | term);
            }
        };

        /// represents a stored function to be called when a denotation
        /// is matched. actions contain the variable assignments and orderings
        /// to curry to stored functions.
        template <typename EnvT, typename TermT, typename TokT>
        class Action /* Lawsuit */ {

            friend class Denotation<EnvT, TermT, TokT>;
            friend class Grammar<EnvT, TermT, TokT>;

            typedef Function<EnvT> FunctionT;
            typedef Action<EnvT, TermT, TokT> SelfT;

        public:
            mutable FunctionT *delegate;
            mutable std::vector<unsigned> parameter_ids;

            Action(FunctionT *d, std::vector<unsigned> &param_ids)
             : delegate(d) {
                parameter_ids.swap(param_ids);
            }

            ~Action(void) {
                if(0 != delegate) {
                    delete delegate;
                    delegate = 0;
                }
            }

            Action(const SelfT &other) {
                delegate = other.delegate;
                parameter_ids.swap(other.parameter_ids);
                other.delegate = 0;
            }

            SelfT &operator=(const SelfT &other) {
                delegate = other.delegate;
                parameter_ids.swap(other.parameter_ids);
                other.delegate = 0;
                return *this;
            }
        };
    }

    /// the token class is used to represent a specific token
    template <typename EnvT, typename TermT, typename TokT>
    class Token {

        typedef Token<EnvT,TermT,TokT> SelfT;

        friend class Denotation<EnvT,TermT,TokT>;
        friend class Grammar<EnvT,TermT,TokT>;
        friend class DenotationBuilder<EnvT,TermT,TokT>;

    private:

        unsigned id;
        TermT terminal;
        bool is_initialized;

    public:

        Token(Grammar<EnvT,TermT,TokT> &g)
         : id(g.getNextVariableId())
         , is_initialized(false) { }

        SelfT &operator()(const TermT term) {
            is_initialized = true;
            terminal = term;
            return *this;
        }
    };

    /// the variable class is used to represent the environment
    template <typename EnvT, typename TermT, typename TokT>
    class Variable {

        friend class Grammar<EnvT, TermT, TokT>;
        friend class Symbol<EnvT, TermT, TokT>;
        friend class Denotation<EnvT,TermT,TokT>;

        typedef Grammar<EnvT, TermT, TokT> GrammarT;
        typedef Variable<EnvT,TermT,TokT> SelfT;

    private:
        unsigned binding_power;
        GrammarT *sub_grammar;

        unsigned id;
        bool is_initialized;

    public:

        /// make a new variable
        Variable(GrammarT &g)
         : binding_power(0)
         , sub_grammar(0)
         , id(g.getNextVariableId())
         , is_initialized(false) { }

        /// assign a new binding power to this variable
        SelfT &operator()(const unsigned bp) {
            is_initialized = true;
            binding_power = bp;
            sub_grammar = 0;
            return *this;
        }

        /// assign a binding power but also a grammar to this variable
        SelfT &operator()(GrammarT &grammar) {
            is_initialized = true;
            sub_grammar = &grammar;
            binding_power = 0;
            return *this;
        }

        /// equivalence between variables
        bool operator==(const SelfT &other) {
            return id == other.id;
        }
    };

    /// represents the actions that can be taken for a particular terminal
    template <typename EnvT, typename TermT, typename TokT>
    class Denotation {

        friend class Grammar<EnvT,TermT,TokT>;
        friend class DenotationBuilder<EnvT,TermT,TokT>;

        typedef Denotation<EnvT,TermT,TokT> SelfT;
        typedef Grammar<EnvT,TermT,TokT> GrammarT;
        typedef Symbol<EnvT,TermT,TokT> SymbolT;
        typedef Variable<EnvT,TermT,TokT> VarT;
        typedef Token<EnvT,TermT,TokT> TokenT;
        typedef Action<EnvT, TermT, TokT> ActionT;
        typedef Error<EnvT, TermT, TokT> ErrorT;
        typedef typename std::vector<TokT>::iterator TokTIterator;
        typedef Variant<EnvT,TokT> EnvOrTokT;

    private:

        unsigned prefix_env_var_id;

        // sequence of terminals and binding powers and variables
        std::vector<SymbolT> symbols;

        // the action to perform when this denotation is matched
        size_t action_id;

        GrammarT &grammar;

        Denotation(GrammarT &g, VarT var)
         : prefix_env_var_id(var.id)
         , action_id(0)
         , grammar(g) { }

        Denotation(GrammarT &g)
         : prefix_env_var_id(0)
         , action_id(0)
         , grammar(g) { }

        /// don't allow these to be used usefully
        Denotation(const SelfT &) { }
        SelfT &operator=(const SelfT &) { return *this; }

        /// evaluate this denotation as a null denotation
        Variant<EnvT,ErrorT> eval(
            std::vector<Variant<Null, EnvOrTokT> > &params,
            Function<EnvT> *delegate,
            TokTIterator &it,
            TokTIterator &end,
            const size_t offset=0
        ) const {

            TermT term;

            // go through the symbols and try to match terminals against
            // the input tokens or call a sub parser. the purpose of this
            // loop is to collect collect the values to assign to the
            // variables into the params array so that they can be extracted
            // and curried into the action function is the appropriate order.
            for(size_t i(offset); i < symbols.size(); ++i) {

                const SymbolT &symbol(symbols[i]);

                if(it == end) {
                    return ErrorT(UNEXPECTED_END_OF_INPUT);
                }

                term = (grammar.tok_to_term)(*it);

                // std::cout << "looking at term=" << ((char) term) << ", tok=" << (it->lexeme) << '\n';

                // we need to match a terminal, and possibly pass the token
                // in to the action function
                if(SymbolT::Term == symbol.state
                || SymbolT::TermVariable == symbol.state) {

                    if(term != symbol.terminal) {
                        return ErrorT(
                            UNEXPECTED_TOKEN,
                            *it,
                            symbol.terminal
                        );
                    }

                    // add in the parameter
                    if(SymbolT::TermVariable == symbol.state) {
                        params[symbol.variable_id] = EnvOrTokT(*it);
                    }

                    ++it;

                // we need to match some binding environment either directly
                // or indirectly through another grammar.
                } else if(SymbolT::Variable == symbol.state) {

                    // which grammar should we use for the parsing?
                    GrammarT &g(
                        0 == symbol.sub_grammar
                        ? grammar
                        : *(symbol.sub_grammar)
                    );

                    // get the result of parsing
                    Variant<EnvT,ErrorT> env_or_error(g.parseImpl(
                        it, end, symbol.binding_power
                    ));

                    // the parse was successful
                    if(env_or_error.isFirst()) {
                        params[symbol.variable_id] = EnvOrTokT(
                            env_or_error.getFirst()
                        );

                    // an error occurred while parsing, propagate it upward
                    } else {
                        // std::cout << "* params.size()=" << params.size() << "\n";
                        return env_or_error;
                    }
                } else {
                    assert(false);
                }
            }

            // now curry the action variables, this makes sure we do them in
            // the right order

            std::vector<unsigned> &param_ids(
                grammar.actions[action_id].parameter_ids
            );

            for(size_t i(0); i < param_ids.size(); ++i) {
                const unsigned param_id(param_ids[i]);
                EnvOrTokT &env_or_tok(params[param_id].getSecond());

                if(env_or_tok.isFirst()) {
                    delegate->curry(env_or_tok.getFirst());
                } else {
                    delegate->curry(env_or_tok.getSecond());
                }
            }

            return delegate->apply();
        }

        /// make sure that the variables referenced in the symbols are
        /// disjoint
        bool symbolVarsAreDisjoint(const unsigned int var_id) const {
            for(size_t i(0); i < symbols.size(); ++i) {
                const SymbolT &symbol(symbols[i]);
                if(SymbolT::Variable == symbol.state
                || SymbolT::TermVariable == symbol.state) {
                    if(symbol.variable_id == var_id) {
                        return false;
                    }
                }
            }
            return true;
        }

        /// make sure that the intersection between the variables referenced
        /// in the action and the variables referenced in the symbols list is
        /// the same as the variables referenced in the action
        bool symbolsCoverActionVars(const ActionT &action) const {

            // make the set of ids used in the symbol
            std::set<unsigned> symbol_var_ids;
            for(size_t i(0); i < symbols.size(); ++i) {
                const SymbolT &symbol(symbols[i]);
                if(SymbolT::Variable == symbol.state
                || SymbolT::TermVariable == symbol.state) {
                    symbol_var_ids.insert(symbol.variable_id);
                }
            }

            const size_t size(symbol_var_ids.size());
            symbol_var_ids.insert(
                action.parameter_ids.begin(),
                action.parameter_ids.end()
            );

            return size == symbol_var_ids.size();
        }

    public:

        /// assign in a variable with a binding power
        SelfT &operator|(const VarT &var) {
            assert(var.is_initialized);
            assert(symbolVarsAreDisjoint(var.id));
            symbols.push_back(SymbolT(var));
            return *this;
        }

        SelfT &operator|(const TermT term) {
            symbols.push_back(SymbolT(term));
            return *this;
        }

        SelfT &operator|(const TokenT &tok) {
            assert(tok.is_initialized);
            assert(symbolVarsAreDisjoint(tok.id));
            symbols.push_back(SymbolT(tok.terminal, tok.id));
            return *this;
        }

        void operator>>=(const size_t id) {
            assert(symbolsCoverActionVars(grammar.actions[id]));
            action_id = id;
        }
    };

    /// represents the grammar type
    template <typename EnvT, typename TermT, typename TokT>
    class Grammar {

        typedef Denotation<EnvT,TermT,TokT> DenotationT;
        typedef Variable<EnvT,TermT,TokT> VarT;
        typedef Token<EnvT,TermT,TokT> TokenT;
        typedef Symbol<EnvT,TermT,TokT> SymbolT;
        typedef Grammar<EnvT,TermT,TokT> SelfT;
        typedef Action<EnvT,TermT,TokT> ActionT;
        typedef DenotationBuilder<EnvT,TermT,TokT> DBuilderT;
        typedef typename std::vector<TokT>::iterator TokTIterator;
        typedef TermT (TokToTermFuncT)(TokT &);
        typedef Variant<EnvT,TokT> EnvOrTokT;
        typedef Error<EnvT,TermT,TokT> ErrorT;
        typedef Variant<Null, EnvOrTokT> ParamT;

        friend class Token<EnvT,TermT,TokT>;
        friend class Variable<EnvT,TermT,TokT>;
        friend class Denotation<EnvT,TermT,TokT>;
        friend class DenotationBuilder<EnvT,TermT,TokT>;

    private:

        unsigned num_variables;

        std::map<TermT, DenotationT *> null_denotations;
        std::map<TermT, DenotationT *> left_denotations;

        // used to build denotations. obviosuly, it is a bad idea to start
        // one denotation and then start another without finishing the first
        // one.
        DBuilderT builder;

        // the set of actions. they are stored so that their destructors may
        // be called whenever the grammar is destroyed, thus destroying their
        // heap-allocated variadic functions.
        std::vector<ActionT> actions;

        TokToTermFuncT *tok_to_term;

        /// get the next variable id used by this grammar
        unsigned getNextVariableId(void) {
            return num_variables++;
        }

        /// assign and return a reference to a denotation
        DenotationT &assignDenotation(
            const TermT term,
            DenotationT *d,
            std::map<TermT, DenotationT *> &denotations
        ) {
            assert(denotations.find(term) == denotations.end());
            denotations[term] = d;
            return *d;
        }

        /// clean up the memory for the denotations
        void freeDenotations(std::map<TermT, DenotationT *> &ds) {
            typename std::map<TermT, DenotationT *>::iterator it(ds.begin());
            typename std::map<TermT, DenotationT *>::iterator it_end(ds.end());

            for(; it != it_end; ++it) {
                delete it->second;
            }

            ds.clear();
        }

        /// add an action to this grammar and return it. this is a convenience
        // method.
        size_t addAction(
            Function<EnvT> *func_ptr,
            unsigned count,
            ...
        ) {
            va_list id_list;
            std::vector<unsigned> ids;

            va_start(id_list, count);
            for(unsigned int i(0); i < count; ++i) {
                ids.push_back(va_arg(id_list, unsigned));
            }
            va_end(id_list);

            actions.push_back(ActionT(func_ptr, ids));
            return actions.size() - 1;
        }

        /// check to see if a particular denotation exists
        bool hasDenotation(const TermT term, std::map<TermT, DenotationT *> &ds) {
            return ds.find(term) != ds.end();
        }

        /// get the left binding power for a given terminal, or return
        /// TDOP_BP_BASE if there is no left denotation for the given, and
        /// return TDOP_BP_BASE-1 in the event that there is no denotation for
        /// this temrinal (and so it should never be allowed to be sucked up
        /// except by a denotation that explicitly mentions it).
        unsigned leftBindingPower(const TermT term) {
            if(!hasDenotation(term, left_denotations)) {
                if(hasDenotation(term, null_denotations)) {
                    return TDOP_BP_BASE;
                }
                return TDOP_BP_BASE - 1;
            }
            DenotationT *denotation(left_denotations[term]);

            assert(0 < denotation->symbols.size());
            return denotation->symbols[0].binding_power;
        }

    public:

        /// constructor
        Grammar(TokToTermFuncT *ttt)
         : num_variables(0)
         , builder(*this)
         , tok_to_term(ttt) { }

        /// destructor, clean up the denotations
        ~Grammar(void) {
            freeDenotations(null_denotations);
            freeDenotations(left_denotations);
            actions.clear();
        }

        /// used as a part of a fake "-->" operator for defining denotations.
        DBuilderT &operator--(int) {
            return builder;
        }

#include "Action.src"

    private:

        /// pull out all of the partial environments for error handling
        void addPartialEnvsToError(ErrorT &err, std::vector<ParamT> &params) {

            typename std::vector<ParamT>::iterator it(params.begin());
            typename std::vector<ParamT>::iterator end(params.end());

            for(; it != end; ++it) {
                ParamT &param(*it);
                if(param.isFirst()) {
                    continue;
                }

                EnvOrTokT &env_or_tok(param.getSecond());
                if(!env_or_tok.isFirst()) {
                    continue;
                }
                // std::cout << "**partial result=" << env_or_tok.getFirst() << "\n";
                err.partial_envs.push_back(env_or_tok.getFirst());
            }

            params.clear();
        }

        /// parse a sequence of tokens
        Variant<EnvT, ErrorT> parseImpl(
            TokTIterator &it,
            TokTIterator &end,
            unsigned int rbp
        ) {

            TermT terminal(tok_to_term(*it));

            if(!hasDenotation(terminal, null_denotations)) {
                return ErrorT(MISSING_NULL_DENOTATION, *it);
            }

            // std::cout << "push parseImpl(" << rbp << "), start is " << ((char) terminal) << '\n';
            std::vector<ParamT> params;
            params.resize(num_variables, Null());

            // evaluate the null denotation starting at the terminal pointed
            // to by the token iterator
            // std::cout << "push nud(" << ((char) terminal) << " " << it->lexeme << ")\n";

            DenotationT *denotation(null_denotations[terminal]);

            const ActionT &nud_action(actions[denotation->action_id]);

            // create a copy of the action function to call
            Function<EnvT> *delegate(nud_action.delegate->copy());
            Variant<EnvT, ErrorT> env_or_error(
                denotation->eval(params, delegate, it, end)
            );
            delete delegate;

            // an error occurred, propagate upward.
            if(!env_or_error.isFirst()) {
                addPartialEnvsToError(env_or_error.getSecond(), params);
                return env_or_error;
            }

            // std::cout << "pop nud(" << ((char) terminal) << ")\n";

            // be greedy and try to capture as much as possible in this
            // environment by looking for operators with leds that
            // can take over the nud.
            for(; it != end; ) {

                terminal = tok_to_term(*it);
                // std::cout << "rbp=" << rbp << "; lbp(" << ((char) terminal) << ")=" << leftBindingPower(terminal) << "\n";
                if(rbp >= leftBindingPower(terminal)) {
                    // std::cout << "** stopping\n";
                    break;
                }


                if(!hasDenotation(terminal, left_denotations)) {
                    ErrorT err(ErrorT(
                        MISSING_LEFT_DENOTATION,
                        env_or_error.getFirst(),
                        *it
                    ));
                    addPartialEnvsToError(err, params);
                    return err;
                }

                denotation = left_denotations[terminal];

                // std::cout << "push led(" << ((char) terminal) << " " << it->lexeme << ")\n";

                // get the action function to call, copy it, pass it in, and
                // then tell the evaluation function to skip the first symbol
                // of its symbols array because we pass that in by setting
                // it directly to the params array.
                const ActionT &led_action(actions[denotation->action_id]);
                delegate = led_action.delegate->copy();
                params[denotation->symbols[0].variable_id] = EnvOrTokT(
                    env_or_error.getFirst()
                );
                env_or_error = denotation->eval(params, delegate, ++it, end, 2);
                delete delegate;

                // an error occurred, propagate upward
                if(!env_or_error.isFirst()) {
                    addPartialEnvsToError(env_or_error.getSecond(), params);
                    return env_or_error;
                }

                // std::cout << "pop led(" << ((char) terminal) << ")\n";
            }

            delegate = 0;

            // std::cout << "pop parseImpl\n";

            return env_or_error;
        }

    public:

        /// parse a string
        Variant<EnvT, ErrorT> parse(std::vector<TokT> &str) {

            ErrorT error(NO_TOKENS_TO_PARSE);
            if(0 == str.size()) {
                return error;
            }

            TokTIterator it(str.begin());
            TokTIterator it_end(str.end());

            return parseImpl(it, it_end, TDOP_BP_BASE);
        }
    };

    template <typename EnvT, typename TermT, typename TokT>
    class Error {

        typedef Error<EnvT,TermT,TokT> SelfT;

    public:

        std::vector<EnvT> partial_envs;
        TokT error_tok;
        TermT error_term;
        ErrorType type;

        Error(void)
         : type(NO_ERROR) { }

        Error(const ErrorType t)
         : type(t) { }

        Error(const ErrorType t, EnvT e, TokT et)
         : error_tok(et)
         , type(t) {
            partial_envs.push_back(e);
        }

        Error(const ErrorType t, TokT et)
         : error_tok(et)
         , type(t) { }

        Error(const ErrorType t, TokT et, TermT tt)
         : error_tok(et)
         , error_term(tt)
         , type(t) { }

        // make sure to keep track of any parsed environments
        SelfT &operator=(const SelfT &other) {
            partial_envs.insert(
                partial_envs.end(),
                other.partial_envs.begin(),
                other.partial_envs.end()
            );

            error_tok = other.error_tok;
            error_term = other.error_term;
            type = other.type;
            return *this;
        }

        Error(const SelfT &other) {
            operator=(other);
        }
    };
}

#endif /* TDOP_GRAMMAR_H_ */
