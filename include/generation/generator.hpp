#pragma once

#include <string>
#include <utility>

#include "parsing/AST.hpp"
#include "unordered_map"
#include "utils/log.hpp"

class Generator {
   private:
    AST::Program program_;
    std::stringstream output_;

    int currentStackOffset_ = 0;
    int currentStackSize_ = 0;

    std::unordered_map<std::string, int> variableStackOffset_;

    void allocate_stack(const int bytes) {
        output_ << "    sub rsp, " << bytes << "\n";
        currentStackSize_ += bytes;
    }

    int allocate_variable(const std::string& name, const int value, const int size = 8) {
        if (currentStackOffset_ + size > currentStackSize_) {
            allocate_stack(size * 8);  // Allocate a little more to do less runtime allocations
        }

        std::string movDirective;
        if (size == 1)
            movDirective = "byte";
        else if (size == 2)
            movDirective = "word";
        else if (size == 4)
            movDirective = "dword";
        else if (size == 8)
            movDirective = "qword";
        else {
            throw std::invalid_argument(std::format("Cannot allocate a {} byte variable", size));
        }

        currentStackOffset_ += size;
        output_ << "    mov " << movDirective << " [rbp - " << currentStackOffset_ << "], " << value
                << "\n";

        if (!variableStackOffset_.emplace(name, currentStackOffset_).second) {
            const std::string errorMessage =
                std::format("The following variable cannot be redeclared: a", name);
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }

        return currentStackOffset_;
    }

    int get_variable_stack_offset(const std::string& name) {
        const auto itr = variableStackOffset_.find(name);
        if (itr == variableStackOffset_.end()) {
            const std::string errorMessage = std::format("Use of undeclared variable: {}", name);
            print_error(errorMessage);
            exit(EXIT_FAILURE);
        }

        return itr->second;
    }

    void generate_exit(const AST::Exit& exitStmt) {
        output_ << "    mov rax, 60\n";  // syscall: exit
        output_ << "    mov rdi, ";      // exit code
        const int exitCode =
            get_variable_stack_offset(static_cast<AST::Identifier*>(exitStmt.exitCode_.get())->name_);
        output_ << "[rbp - " << exitCode << "]\n";
        output_ << "    syscall\n";
    }

    void generate_assignment(const AST::Assignment& assignmentStmt) {
        const AST::NumberLiteral value =
            *static_cast<AST::NumberLiteral*>(assignmentStmt.value_.get());
        const int pos = allocate_variable(assignmentStmt.identifier_, value.value_, 8);
    }

   public:
    explicit Generator(AST::Program ast) : program_(std::move(ast)) {}

    std::stringstream generate() {
        // Write the header
        output_ << "section .text\n";
        output_ << "global _start\n";
        output_ << "_start:\n";

        output_ << "    push rbp\n";
        output_ << "    mov rbp, rsp\n\n";

        for (const auto& stmt : program_.statements_) {
            if (stmt->kind_ == AST::NodeKind::EXIT) {
                const auto exitStmt = static_cast<AST::Exit*>(stmt.get());
                generate_exit(*exitStmt);
            } else if (stmt->kind_ == AST::NodeKind::ASSIGNMENT) {
                const auto assignmentStmt = static_cast<AST::Assignment*>(stmt.get());
                generate_assignment(*assignmentStmt);
            } else {
                print_error("Unknown statement kind");
                print_hint("Only exit statements are currently supported");
                exit(EXIT_FAILURE);
            }
        }

        return std::move(output_);
    }
};