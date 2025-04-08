# CS320 Final Project: MiniML Interpreter

This repository consists of multiple assignments I completed over the duration of the course **CS320**.  
The final project (within assigns/interp03) is an interpreter for a functional programming language with the following features:

---

## Overview

The project is divided into three main parts:

### 1. Parser Combinator Library
- Provides reusable combinators for building parsers.
- Inspired by monadic parser combinators from functional languages like Haskell and OCaml.
- Supports:
  - Sequencing (`>>=`)
  - Choice (`<|>`)
  - Repetition (`many`, `many1`)
  - Optional parsing and more.

---

### 2. High-Level Language Parser

Parses a small ML-style language with support for:

1. Variables, integers, booleans, and unit `()`
2. Arithmetic and logical expressions
3. Function definitions and applications
4. Let bindings and nested scopes
5. Control flow: `if ... then ... else`
6. Unary and binary operators
7. Tracing (`trace`) for debugging

---

### 3. Stack-Based Intermediate Representation

Defines a minimal stack-based language with:

1. Constants, arithmetic/logical operations  
2. Function closures and applications  
3. Variable lookup and assignment  
4. Conditional branching  
5. Stack manipulation operations (e.g. `Swap`, `Return`, `Trace`)

---

### How to Use

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/cs320-final-project.git
   cd cs320-final-project
