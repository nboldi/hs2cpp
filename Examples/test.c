#include <boost/preprocessor/library.hpp>

// UNCOMMENT THESE FOR MEASURING HANDWRITTEN IMPLEMENTATION
// #define ALLOCATE_FUNCTION malloc
// #define DEALLOCATE_FUNCTION free
// #include "macro_config.h"

// UNCOMMENT THESE FOR MEASURING GENERATED IMPLEMENTATOIN
#include "Config.h"
#define DECLARE declare
#define VALUE_OF(id,c) valueOf(id,c)
#define SCALAR Scalar
#define PTR Pointer
#define ARRAY Array


// Test configurations
#define C_S SCALAR
#define C_PS PTR(SCALAR)
#define C_PPS PTR(PTR(SCALAR))
#define C_PPPS PTR(PTR(PTR(SCALAR)))
#define C_PPPPS PTR(PTR(PTR(PTR(SCALAR))))
#define C_PPPPPS PTR(PTR(PTR(PTR(PTR(SCALAR)))))
#define C_PPPPPPS PTR(PTR(PTR(PTR(PTR(PTR(SCALAR))))))
#define C_PPPPPPPS PTR(PTR(PTR(PTR(PTR(PTR(PTR(SCALAR)))))))
#define C_PPPPPPPPS PTR(PTR(PTR(PTR(PTR(PTR(PTR(PTR(SCALAR))))))))
#define C_PPPPPPPPPS PTR(PTR(PTR(PTR(PTR(PTR(PTR(PTR(PTR(SCALAR)))))))))

#define SCALAR_CONFIGURATIONS (C_S)(C_PS)(C_PPS)(C_PPPS)(C_PPPPS)(C_PPPPPS)(C_PPPPPPS)(C_PPPPPPPS)(C_PPPPPPPPS)(C_PPPPPPPPPS)

// UNCOMMENT FOR 1-2-4-8-16 TIMES OF WORK DONE
// #define ALL_CONFIGURATIONS    SCALAR_CONFIGURATIONS
// #define ALL_CONFIGURATIONS    SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS
// #define ALL_CONFIGURATIONS    SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS
// #define ALL_CONFIGURATIONS    SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS
#define ALL_CONFIGURATIONS    SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS SCALAR_CONFIGURATIONS

// Convert a test configuration into a label
#define MAKE_LABEL(prefix, config) BOOST_PP_CAT(prefix, BOOST_PP_CAT(__, BOOST_PP_TUPLE_ELEM(0, BOOST_PP_SEQ_FOLD_LEFT(MAKE_LABEL_, (BOOST_PP_EMPTY(), 0), config)))):
#define MAKE_LABEL_(s, state, item) \
    (BOOST_PP_CAT( \
        BOOST_PP_TUPLE_ELEM(0, state), \
        BOOST_PP_IIF( \
            BOOST_PP_GREATER(BOOST_PP_TUPLE_ELEM(1, state), 0), \
            BOOST_PP_CAT(_, item), \
            item \
        ) \
    ), BOOST_PP_INC(BOOST_PP_TUPLE_ELEM(1, state)))

    
void test()
{   
// Declaration
    #define DECLARE_WRAPPER(r, data, index, elem) \
        DECLARE( \
            BOOST_PP_TUPLE_ELEM(0, data), \
            BOOST_PP_CAT(BOOST_PP_TUPLE_ELEM(1, data), index), \
            elem \
        );
    BOOST_PP_SEQ_FOR_EACH_I(DECLARE_WRAPPER, (int, a), ALL_CONFIGURATIONS)
    #undef DECLARE_WRAPPER
    
// Value of
    #define VALUE_OF_WRAPPER(r, data, index, elem) \
        VALUE_OF(BOOST_PP_CAT(data, index), elem);
    BOOST_PP_SEQ_FOR_EACH_I(VALUE_OF_WRAPPER, a, ALL_CONFIGURATIONS)
    #undef VALUE_OF_WRAPPER
}