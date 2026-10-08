#include "SemanticAnalyser.hpp"

#include <cstdlib>

#include "frontend/type/TypeManager.hpp"

using namespace Sema;

void SemanticAnalyser::analyse() {
    typeSolver_.prepareForConstraints();

    constraintGenerator_.traverse();
    emitErrorsAndQuitIfNeeded();

    typeSolver_.solve();
    emitErrorsAndQuitIfNeeded();

    validator_.traverse();
    emitErrorsAndQuitIfNeeded();
}

void SemanticAnalyser::emitErrorsAndQuit() const {
    diagnosticsEngine_.emit();
    exit(EXIT_FAILURE);
}
