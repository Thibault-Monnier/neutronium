#pragma once

#include <string>
#include <vector>

namespace AST {

struct ASTNode {
    explicit ASTNode(std::string type) : type_(std::move(type)) {}
    virtual ~ASTNode() = default;

    const std::string type_;
};

struct Assignment : ASTNode {
    Assignment(std::string identifier, std::string value)
        : ASTNode{"Assignment"}, identifier_(std::move(identifier)), value_(std::move(value)) {}

    std::string identifier_;
    std::string value_;
};

struct Expression : ASTNode {
    Expression(std::string value) : ASTNode{"Expression"}, value_(std::move(value)) {}

    std::string value_;
};

using Statement = std::variant<Assignment, Expression>;

struct Program : ASTNode {
    Program() : ASTNode{"Program"} {}

    std::vector<Statement> statements_;
};

}  // namespace AST