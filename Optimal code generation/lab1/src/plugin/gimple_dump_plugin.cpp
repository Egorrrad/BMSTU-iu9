#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <gcc-plugin.h>
#include <plugin-version.h>
#include <coretypes.h>
#include <tree-pass.h>
#include <context.h>
#include <basic-block.h>
#include <tree.h>
#include <tree-ssa-alias.h>
#include <gimple-expr.h>
#include <gimple.h>
#include <gimple-ssa.h>
#include <tree-phinodes.h>
#include <tree-ssa-operands.h>
#include <ssa-iterators.h>
#include <gimple-iterator.h>

using namespace std;

int plugin_is_GPL_compatible = 1;

void on_gimple_phi(gimple* stmt);

void get_tree(tree t) {
    if (!t) {
        cout << "UNDEFINED_TREE_CODE (0)";
        return;
    }

    switch (TREE_CODE(t)) {
    case INTEGER_CST:
        cout << "INTEGER_CST: " << TREE_INT_CST_LOW(t);
        break;
    case STRING_CST:
        cout << "\"" << TREE_STRING_POINTER(t) << "\"";
        break;
    case LABEL_DECL: {
        const char* name = DECL_NAME(t) ? IDENTIFIER_POINTER(DECL_NAME(t)) : "L";
        cout << name << ":";
        break;
    }
    case VAR_DECL:
    case CONST_DECL:
    case PARM_DECL: {
        const char* name = DECL_NAME(t) ? IDENTIFIER_POINTER(DECL_NAME(t)) : "unnamed";
        cout << name;
        break;
    }
    case ARRAY_REF:
        get_tree(TREE_OPERAND(t, 0));
        cout << "[";
        get_tree(TREE_OPERAND(t, 1));
        cout << "]";
        break;
    case COMPONENT_REF:
        get_tree(TREE_OPERAND(t, 0));
        cout << ".";
        get_tree(TREE_OPERAND(t, 1));
        break;
    case MEM_REF:
        cout << "MEM_REF ((";
        get_tree(TREE_TYPE(t));
        cout << ")";
        get_tree(TREE_OPERAND(t, 0));
        cout << ")";
        break;
    case SSA_NAME: {
        gimple* st = SSA_NAME_DEF_STMT(t);
        if (gimple_code(st) == GIMPLE_PHI) {
            gphi* phi_stmt = as_a<gphi*>(st);
            cout << "PHI(";
            for (unsigned i = 0; i < gimple_phi_num_args(phi_stmt); ++i) {
                if (i > 0) cout << ", ";
                cout << "BB" << gimple_phi_arg_edge(phi_stmt, i)->src->index << ":";
                get_tree(gimple_phi_arg_def(phi_stmt, i));
            }
            cout << ")";
        } else {
            const char* name = SSA_NAME_IDENTIFIER(t) ? 
                IDENTIFIER_POINTER(SSA_NAME_IDENTIFIER(t)) : "SSA_NAME";
            cout << name << "__v" << SSA_NAME_VERSION(t);
        }
        break;
    }
    case POINTER_PLUS_EXPR:
        cout << "PTR_ADD(";
        get_tree(TREE_OPERAND(t, 0));
        cout << ", ";
        get_tree(TREE_OPERAND(t, 1));
        cout << ")";
        break;
    default:
        cout << get_tree_code_name(TREE_CODE(t)) << " (" << TREE_CODE(t) << ")";
        break;
    }
}

void op(enum tree_code code) {
    const char* name = get_tree_code_name(code);
    cout << (name ? name : "UNKNOWN_OP");
}

void bb_info(basic_block bb) {
    cout << "\nBB " << bb->index << ":\n";
    cout << "        predecessors: { ";
    edge e;
    edge_iterator it;
    vector<int> preds;
    FOR_EACH_EDGE(e, it, bb->preds) {
        preds.push_back(e->src->index);
    }
    if (preds.empty()) cout << "none";
    else for (size_t i = 0; i < preds.size(); ++i) {
        cout << preds[i] << (i < preds.size()-1 ? ", " : "");
    }
    cout << " }\n";
    
    cout << "        successors: { ";
    vector<int> succs;
    FOR_EACH_EDGE(e, it, bb->succs) {
        succs.push_back(e->dest->index);
    }
    if (succs.empty()) cout << "none";
    else for (size_t i = 0; i < succs.size(); ++i) {
        cout << succs[i] << (i < succs.size()-1 ? ", " : "");
    }
    cout << " }\n";
}

void on_gimple_assign(gimple* stmt) {
    cout << "                GIMPLE_ASSIGN:  { ";
    get_tree(gimple_assign_lhs(stmt));
    cout << " = ";

    const unsigned num_ops = gimple_num_ops(stmt);
    for (unsigned i = 0; i < num_ops; ++i) {
        if (i == 1) {
            cout << " ";
            op(gimple_assign_rhs_code(stmt));
            cout << " ";
        }
        if (i > 1) cout << ", ";
        get_tree(gimple_op(stmt, i));
    }
    cout << " }\n";
}

void on_gimple_call(gimple* stmt) {
    cout << "                GIMPLE_CALL:  { ";
    if (tree lhs = gimple_call_lhs(stmt)) {
        get_tree(lhs);
        cout << " = ";
    }
    
    tree fndecl = gimple_call_fndecl(stmt);
    if (fndecl) {
        cout << IDENTIFIER_POINTER(DECL_NAME(fndecl));
    } else {
        cout << "INDIRECT_CALL";
    }
    
    cout << "(";
    for (unsigned i = 0; i < gimple_call_num_args(stmt); ++i) {
        if (i > 0) cout << ", ";
        get_tree(gimple_call_arg(stmt, i));
    }
    cout << ") }\n";
}

void on_gimple_cond(gimple* stmt) {
    cout << "                GIMPLE_COND:  { ";
    get_tree(gimple_cond_lhs(stmt));
    cout << " ";
    op(gimple_cond_code(stmt));
    cout << " ";
    get_tree(gimple_cond_rhs(stmt));
    cout << " }\n";
}

void on_gimple_label(gimple* stmt) {
    glabel* label_stmt = as_a<glabel*>(stmt);
    cout << "                GIMPLE_LABEL:  { ";
    get_tree(gimple_label_label(label_stmt));
    cout << " }\n";
}

void on_gimple_return(gimple* stmt) {
    greturn* return_stmt = as_a<greturn*>(stmt);
    cout << "                GIMPLE_RETURN:  { ";
    if (tree retval = gimple_return_retval(return_stmt)) {
        get_tree(retval);
    } else {
        cout << "void";
    }
    cout << " }\n";
}

void process_statements(basic_block bb) {
    cout << "        statements:";
    for (gimple_stmt_iterator gsi = gsi_start_bb(bb); !gsi_end_p(gsi); gsi_next(&gsi)) {
        gimple* stmt = gsi_stmt(gsi);
        
        switch (gimple_code(stmt)) {
            case GIMPLE_ASSIGN:  on_gimple_assign(stmt); break;
            case GIMPLE_CALL:    on_gimple_call(stmt); break;
            case GIMPLE_COND:    on_gimple_cond(stmt); break;
            case GIMPLE_LABEL:   on_gimple_label(stmt); break;
            case GIMPLE_RETURN:  on_gimple_return(stmt); break;
            case GIMPLE_PHI:     on_gimple_phi(stmt); break;
            default:
                cout << "\n                UNKNOWN: " 
                    << gimple_code_name[gimple_code(stmt)] 
                    << "\n";
        }
    }
}

void on_gimple_phi(gimple* stmt) {
    gphi* phi = as_a<gphi*>(stmt);
    cout << "                GIMPLE_PHI:  { ";
    get_tree(gimple_phi_result(phi));
    cout << " = [";
    
    for (unsigned i = 0; i < gimple_phi_num_args(phi); ++i) {
        if (i > 0) cout << ", ";
        cout << "BB" << gimple_phi_arg_edge(phi, i)->src->index << ":";
        get_tree(gimple_phi_arg_def(phi, i));
    }
    cout << "] }\n";
}

int process_function(function* fn) {
    cout << "\nfunc " << function_name(fn) << ":";
    basic_block bb;
    
    FOR_EACH_BB_FN(bb, fn) {
        bb_info(bb);
        process_statements(bb);
    }
    return 0;
}

struct GimpleDumpPass : public gimple_opt_pass {
    GimpleDumpPass(gcc::context* ctx) 
        : gimple_opt_pass(pass_data, ctx) {}
        
    unsigned int execute(function* fn) override {
        return process_function(fn);
    }
    
    static const pass_data pass_data;
};

const pass_data GimpleDumpPass::pass_data = {
    .type = GIMPLE_PASS,
    .name = "gimple-dump",
    .optinfo_flags = OPTGROUP_NONE,
    .tv_id = TV_NONE,
    .properties_required = PROP_gimple_any,
    .properties_provided = 0,
    .properties_destroyed = 0,
    .todo_flags_start = 0,
    .todo_flags_finish = 0
};

int plugin_init(struct plugin_name_args* args, 
                struct plugin_gcc_version* version) {
    if (!plugin_default_version_check(version, &gcc_version))
        return 1;

    register_pass_info pass_info = {
        new GimpleDumpPass(g),
        "ssa",
        1,
        PASS_POS_INSERT_AFTER
    };

    register_callback(args->base_name, 
                     PLUGIN_PASS_MANAGER_SETUP, 
                     NULL, 
                     &pass_info);
    return 0;
}