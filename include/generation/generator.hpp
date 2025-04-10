#pragma once

#include <string>
#include <utility>

#include "parsing/AST.hpp"
#include "utils/log.hpp"

class Generator {
   private:
    AST::Program program_;
    std::stringstream output_;

    int currentStackOffset_ = 0;
    int currentStackSize_ = 0;

    void allocate_stack(const int bytes) {
        output_ << "    sub rsp, " << bytes << "\n";
        currentStackSize_ += bytes;
    }

    int allocate_variable(const int value, const int size = 8) {
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
        output_ << "    mov " << movDirective << " [rbp - " << currentStackOffset_ << "], "
                << value << "\n";

        return currentStackOffset_ - size;
    }

    void generate_exit(const AST::Exit& exitStmt) {
        output_ << "    mov rax, 60\n";  // syscall: exit
        output_ << "    mov rdi, ";      // exit code
        const int exitCode = static_cast<AST::NumberLiteral*>(exitStmt.exitCode_.get())->value_;
        output_ << "[rbp - 8]" << "\n";
        output_ << "    syscall\n";
    }

    void generate_assignment(const AST::Assignment& assignmentStmt) {
        const AST::NumberLiteral value =
            *static_cast<AST::NumberLiteral*>(assignmentStmt.value_.get());
        const int pos = allocate_variable(value.value_, 8);
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