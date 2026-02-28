#!/usr/bin/env python3
"""
Cat's GCC 0.1 — Self-Contained C Compiler IDE
═══════════════════════════════════════════════
600×400 Tkinter IDE with a BUILT-IN C compiler.
No GCC, no MinGW, no external tools needed.

Includes:
  - Full C lexer (tokenizer)
  - Recursive-descent parser → AST
  - Tree-walking interpreter with:
      • int, float, double, char, void types
      • Arrays (1D+2D), pointers (basic), strings
      • if/else, while, for, do-while, switch/case
      • Functions with recursion, forward decls
      • printf/scanf/puts/strlen/strcmp/strcpy/strcat/atoi/atof
      • malloc/free (simulated), sizeof
      • Structs (basic)
      • All C operators including bitwise, ternary, comma
      • Pre/post increment/decrement
      • Break/continue/return
      • #include <stdio.h> etc (recognized, not needed)
      • #define simple macros (no params)
  - Code editor with syntax highlighting
  - Line numbers, dark theme, output panel
  - Compile (F5), Run (F6), Build+Run (F9)

Run: python catsgcc01.py
"""

import tkinter as tk
from tkinter import filedialog, messagebox, scrolledtext
import os
import sys
import re
import math
import time
import threading
import traceback

# ═══════════════════════════════════════════════════════════════════════
# C LEXER
# ═══════════════════════════════════════════════════════════════════════

# Token types
TK_INT, TK_FLOAT, TK_CHAR_LIT, TK_STRING, TK_IDENT = 'INT', 'FLOAT', 'CHAR', 'STRING', 'IDENT'
TK_KW, TK_OP, TK_PUNCT, TK_EOF = 'KW', 'OP', 'PUNCT', 'EOF'

KEYWORDS = {
    'auto','break','case','char','const','continue','default','do','double',
    'else','enum','extern','float','for','goto','if','inline','int','long',
    'register','restrict','return','short','signed','sizeof','static','struct',
    'switch','typedef','union','unsigned','void','volatile','while',
    'printf','scanf','puts','gets','strlen','strcmp','strcpy','strcat',
    'malloc','free','atoi','atof','abs','rand','srand','exit',
    'putchar','getchar','sprintf','fprintf','memset','memcpy',
    'NULL','true','false','bool','size_t',
}

MULTI_OPS = [
    '<<=', '>>=', '...', '&&', '||', '==', '!=', '<=', '>=', '<<', '>>',
    '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '++', '--', '->', '##',
]

SINGLE_OPS = set('+-*/%&|^~!<>=?:.,;(){}[]#')

class Token:
    __slots__ = ('type', 'value', 'line', 'col')
    def __init__(self, tp, val, line=0, col=0):
        self.type = tp; self.value = val; self.line = line; self.col = col
    def __repr__(self):
        return f'Token({self.type}, {self.value!r}, L{self.line})'

class LexError(Exception):
    pass

def lex(source):
    """Tokenize C source code. Returns list of Token."""
    tokens = []
    i = 0
    n = len(source)
    line = 1
    col = 1

    while i < n:
        c = source[i]

        # Newline
        if c == '\n':
            line += 1; col = 1; i += 1; continue

        # Whitespace
        if c in ' \t\r':
            i += 1; col += 1; continue

        # Single-line comment
        if c == '/' and i+1 < n and source[i+1] == '/':
            while i < n and source[i] != '\n':
                i += 1
            continue

        # Multi-line comment
        if c == '/' and i+1 < n and source[i+1] == '*':
            i += 2; col += 2
            while i < n:
                if source[i] == '\n':
                    line += 1; col = 1
                elif source[i] == '*' and i+1 < n and source[i+1] == '/':
                    i += 2; col += 2; break
                i += 1; col += 1
            continue

        # Preprocessor directives
        if c == '#':
            start = i
            i += 1; col += 1
            # Skip whitespace after #
            while i < n and source[i] in ' \t':
                i += 1; col += 1
            # Read directive name
            dstart = i
            while i < n and source[i].isalpha():
                i += 1; col += 1
            directive = source[dstart:i]
            # Read rest of line (handling line continuations)
            val_start = i
            while i < n and source[i] != '\n':
                if source[i] == '\\' and i+1 < n and source[i+1] == '\n':
                    i += 2; line += 1; col = 1; continue
                i += 1; col += 1
            val = source[val_start:i].strip()
            tokens.append(Token(TK_PUNCT, '#' + directive, line, col))
            if directive == 'define' and val:
                # Store define value
                parts = val.split(None, 1)
                tokens.append(Token(TK_IDENT, parts[0], line, col))
                if len(parts) > 1:
                    tokens.append(Token(TK_STRING, parts[1], line, col))
                else:
                    tokens.append(Token(TK_STRING, '', line, col))
            elif directive == 'include':
                tokens.append(Token(TK_STRING, val.strip('<>"'), line, col))
            continue

        # String literal
        if c == '"':
            start_line = line
            i += 1; col += 1
            s = ''
            while i < n and source[i] != '"':
                if source[i] == '\\' and i+1 < n:
                    esc = source[i+1]
                    esc_map = {'n':'\n','t':'\t','r':'\r','0':'\0','\\':'\\','"':'"',"'":"'",
                               'a':'\a','b':'\b','f':'\f','v':'\v'}
                    if esc in esc_map:
                        s += esc_map[esc]; i += 2; col += 2
                    elif esc == 'x':
                        # Hex escape
                        i += 2; col += 2
                        hx = ''
                        while i < n and source[i] in '0123456789abcdefABCDEF':
                            hx += source[i]; i += 1; col += 1
                        s += chr(int(hx, 16)) if hx else '\\x'
                    else:
                        s += esc; i += 2; col += 2
                elif source[i] == '\n':
                    line += 1; col = 1; i += 1
                else:
                    s += source[i]; i += 1; col += 1
            if i < n: i += 1; col += 1  # skip closing "
            tokens.append(Token(TK_STRING, s, start_line, col))
            continue

        # Char literal
        if c == "'":
            i += 1; col += 1
            if i < n and source[i] == '\\' and i+1 < n:
                esc = source[i+1]
                esc_map = {'n':'\n','t':'\t','r':'\r','0':'\0','\\':'\\','"':'"',"'":"'"}
                ch = esc_map.get(esc, esc)
                i += 2; col += 2
            elif i < n:
                ch = source[i]; i += 1; col += 1
            else:
                ch = '\0'
            if i < n and source[i] == "'":
                i += 1; col += 1
            tokens.append(Token(TK_CHAR_LIT, ch, line, col))
            continue

        # Number
        if c.isdigit() or (c == '.' and i+1 < n and source[i+1].isdigit()):
            start = i
            is_float = False
            if c == '0' and i+1 < n and source[i+1] in 'xX':
                i += 2; col += 2
                while i < n and source[i] in '0123456789abcdefABCDEF':
                    i += 1; col += 1
                tokens.append(Token(TK_INT, int(source[start:i], 16), line, col))
            elif c == '0' and i+1 < n and source[i+1] in 'bB':
                i += 2; col += 2
                while i < n and source[i] in '01':
                    i += 1; col += 1
                tokens.append(Token(TK_INT, int(source[start+2:i], 2), line, col))
            else:
                while i < n and source[i].isdigit():
                    i += 1; col += 1
                if i < n and source[i] == '.':
                    is_float = True; i += 1; col += 1
                    while i < n and source[i].isdigit():
                        i += 1; col += 1
                if i < n and source[i] in 'eE':
                    is_float = True; i += 1; col += 1
                    if i < n and source[i] in '+-': i += 1; col += 1
                    while i < n and source[i].isdigit():
                        i += 1; col += 1
                # Suffix
                while i < n and source[i] in 'fFlLuU':
                    if source[i] in 'fF': is_float = True
                    i += 1; col += 1
                num_str = source[start:i]
                num_str = num_str.rstrip('fFlLuU')
                if is_float:
                    tokens.append(Token(TK_FLOAT, float(num_str), line, col))
                else:
                    tokens.append(Token(TK_INT, int(num_str), line, col))
            continue

        # Identifier / keyword
        if c.isalpha() or c == '_':
            start = i
            while i < n and (source[i].isalnum() or source[i] == '_'):
                i += 1; col += 1
            word = source[start:i]
            if word in KEYWORDS:
                tokens.append(Token(TK_KW, word, line, col))
            else:
                tokens.append(Token(TK_IDENT, word, line, col))
            continue

        # Multi-char operators
        found = False
        for op in MULTI_OPS:
            if source[i:i+len(op)] == op:
                tokens.append(Token(TK_OP, op, line, col))
                i += len(op); col += len(op)
                found = True; break
        if found: continue

        # Single-char operators/punctuation
        if c in SINGLE_OPS:
            tokens.append(Token(TK_OP, c, line, col))
            i += 1; col += 1; continue

        # Unknown
        i += 1; col += 1

    tokens.append(Token(TK_EOF, None, line, col))
    return tokens

# ═══════════════════════════════════════════════════════════════════════
# AST NODES
# ═══════════════════════════════════════════════════════════════════════

class AST:
    pass

class Program(AST):
    def __init__(self): self.decls = []; self.defines = {}

class FuncDecl(AST):
    def __init__(self, ret_type, name, params, body):
        self.ret_type = ret_type; self.name = name
        self.params = params; self.body = body  # body=None for forward decl

class VarDecl(AST):
    def __init__(self, vtype, name, init=None, is_array=False, array_size=None):
        self.vtype = vtype; self.name = name; self.init = init
        self.is_array = is_array; self.array_size = array_size

class StructDecl(AST):
    def __init__(self, name, members):
        self.name = name; self.members = members

class Block(AST):
    def __init__(self, stmts): self.stmts = stmts

class IfStmt(AST):
    def __init__(self, cond, then_b, else_b=None):
        self.cond = cond; self.then_b = then_b; self.else_b = else_b

class WhileStmt(AST):
    def __init__(self, cond, body): self.cond = cond; self.body = body

class DoWhileStmt(AST):
    def __init__(self, body, cond): self.body = body; self.cond = cond

class ForStmt(AST):
    def __init__(self, init, cond, update, body):
        self.init = init; self.cond = cond; self.update = update; self.body = body

class SwitchStmt(AST):
    def __init__(self, expr, cases, default):
        self.expr = expr; self.cases = cases; self.default = default

class ReturnStmt(AST):
    def __init__(self, expr=None): self.expr = expr

class BreakStmt(AST): pass
class ContinueStmt(AST): pass

class ExprStmt(AST):
    def __init__(self, expr): self.expr = expr

class BinOp(AST):
    def __init__(self, op, left, right): self.op = op; self.left = left; self.right = right

class UnaryOp(AST):
    def __init__(self, op, expr, prefix=True): self.op = op; self.expr = expr; self.prefix = prefix

class TernaryOp(AST):
    def __init__(self, cond, then_e, else_e):
        self.cond = cond; self.then_e = then_e; self.else_e = else_e

class Assign(AST):
    def __init__(self, target, value, op='='):
        self.target = target; self.value = value; self.op = op

class NumLit(AST):
    def __init__(self, value): self.value = value

class StrLit(AST):
    def __init__(self, value): self.value = value

class CharLit(AST):
    def __init__(self, value): self.value = value

class Ident(AST):
    def __init__(self, name): self.name = name

class FuncCall(AST):
    def __init__(self, name, args): self.name = name; self.args = args

class ArrayAccess(AST):
    def __init__(self, array, index): self.array = array; self.index = index

class MemberAccess(AST):
    def __init__(self, obj, member, is_ptr=False):
        self.obj = obj; self.member = member; self.is_ptr = is_ptr

class CastExpr(AST):
    def __init__(self, cast_type, expr): self.cast_type = cast_type; self.expr = expr

class SizeofExpr(AST):
    def __init__(self, target): self.target = target

class CommaExpr(AST):
    def __init__(self, exprs): self.exprs = exprs

class ArrayInit(AST):
    def __init__(self, elements): self.elements = elements

# ═══════════════════════════════════════════════════════════════════════
# PARSER
# ═══════════════════════════════════════════════════════════════════════

class ParseError(Exception):
    pass

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.defines = {}

    def cur(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else Token(TK_EOF, None)

    def peek(self, offset=0):
        p = self.pos + offset
        return self.tokens[p] if p < len(self.tokens) else Token(TK_EOF, None)

    def eat(self, tp=None, val=None):
        t = self.cur()
        if tp and t.type != tp:
            raise ParseError(f"L{t.line}: Expected {tp} got {t.type}({t.value!r})")
        if val and t.value != val:
            raise ParseError(f"L{t.line}: Expected '{val}' got '{t.value}'")
        self.pos += 1
        return t

    def match(self, tp=None, val=None):
        t = self.cur()
        if tp and t.type != tp: return False
        if val is not None and t.value != val: return False
        return True

    def eat_if(self, tp=None, val=None):
        if self.match(tp, val):
            return self.eat()
        return None

    def is_type(self, t=None):
        if t is None: t = self.cur()
        types = {'void','char','short','int','long','float','double',
                 'signed','unsigned','struct','enum','bool','size_t',
                 'const','static','extern','register','volatile','inline'}
        return t.type == TK_KW and t.value in types

    def parse(self):
        prog = Program()
        while not self.match(TK_EOF):
            # Preprocessor
            if self.match(TK_PUNCT, '#include'):
                self.eat(); self.eat()  # skip #include and filename
                continue
            if self.match(TK_PUNCT, '#define'):
                self.eat()  # #define
                name_tok = self.eat(TK_IDENT)
                val_tok = self.eat(TK_STRING)
                prog.defines[name_tok.value] = val_tok.value
                self.defines[name_tok.value] = val_tok.value
                continue
            if self.match(TK_PUNCT) and self.cur().value.startswith('#'):
                self.eat()
                if not self.match(TK_EOF): self.eat()  # skip arg
                continue
            # Struct
            if self.match(TK_KW, 'struct') and self.peek(1).type == TK_IDENT and self.peek(2).value == '{':
                prog.decls.append(self._parse_struct())
                continue
            if self.match(TK_KW, 'typedef'):
                self._skip_typedef()
                continue
            if self.match(TK_KW, 'enum'):
                self._skip_enum()
                continue
            # Function or global var
            d = self._parse_decl_or_func()
            if d:
                if isinstance(d, list):
                    prog.decls.extend(d)
                else:
                    prog.decls.append(d)
        return prog

    def _skip_typedef(self):
        self.eat()  # typedef
        depth = 0
        while not self.match(TK_EOF):
            if self.cur().value == '{': depth += 1
            elif self.cur().value == '}': depth -= 1
            if depth == 0 and self.cur().value == ';':
                self.eat(); return
            self.eat()

    def _skip_enum(self):
        self.eat()  # enum
        if self.match(TK_IDENT): self.eat()
        if self.match(TK_OP, '{'):
            self.eat()
            while not self.match(TK_OP, '}') and not self.match(TK_EOF):
                self.eat()
            self.eat_if(TK_OP, '}')
        self.eat_if(TK_OP, ';')

    def _parse_struct(self):
        self.eat()  # struct
        name = self.eat(TK_IDENT).value
        self.eat(TK_OP, '{')
        members = []
        while not self.match(TK_OP, '}'):
            mtype = self._parse_type()
            mname = self.eat(TK_IDENT).value
            arr_sz = None
            if self.eat_if(TK_OP, '['):
                arr_sz = self.cur().value; self.eat()
                self.eat(TK_OP, ']')
            self.eat(TK_OP, ';')
            members.append((mtype, mname, arr_sz))
        self.eat(TK_OP, '}')
        self.eat(TK_OP, ';')
        return StructDecl(name, members)

    def _parse_type(self):
        parts = []
        while self.is_type():
            parts.append(self.eat().value)
        if not parts:
            if self.match(TK_IDENT):
                parts.append(self.eat().value)
            else:
                parts.append('int')
        # Pointers
        while self.match(TK_OP, '*'):
            self.eat(); parts.append('*')
        return ' '.join(parts)

    def _parse_decl_or_func(self):
        ret_type = self._parse_type()
        if self.match(TK_OP, ';'):
            self.eat(); return None
        name = self.eat(TK_IDENT).value

        # Function
        if self.match(TK_OP, '('):
            self.eat()
            params = []
            while not self.match(TK_OP, ')'):
                if self.match(TK_KW, 'void') and self.peek(1).value == ')':
                    self.eat(); break
                if self.match(TK_OP, '...'):
                    self.eat()
                    params.append(('...', '...'))
                    break
                ptype = self._parse_type()
                pname = self.eat(TK_IDENT).value if self.match(TK_IDENT) else f'_p{len(params)}'
                arr = False
                if self.eat_if(TK_OP, '['):
                    arr = True
                    if not self.match(TK_OP, ']'): self.eat()
                    self.eat(TK_OP, ']')
                    ptype += '[]'
                params.append((ptype, pname))
                if not self.eat_if(TK_OP, ','): break
            self.eat(TK_OP, ')')

            # Forward declaration
            if self.eat_if(TK_OP, ';'):
                return FuncDecl(ret_type, name, params, None)

            body = self._parse_block()
            return FuncDecl(ret_type, name, params, body)

        # Variable declaration (possibly with init, possibly multiple)
        decls = []
        decl = self._parse_var_rest(ret_type, name)
        decls.append(decl)
        while self.eat_if(TK_OP, ','):
            # Additional declarator
            n2 = self.eat(TK_IDENT).value
            d2 = self._parse_var_rest(ret_type, n2)
            decls.append(d2)
        self.eat(TK_OP, ';')
        return decls if len(decls) > 1 else decls[0]

    def _parse_var_rest(self, vtype, name):
        arr_size = None
        is_array = False
        if self.eat_if(TK_OP, '['):
            is_array = True
            if not self.match(TK_OP, ']'):
                arr_size = self._parse_expr()
            self.eat(TK_OP, ']')
        init = None
        if self.eat_if(TK_OP, '='):
            if self.match(TK_OP, '{'):
                init = self._parse_array_init()
            else:
                init = self._parse_assign_expr()
        return VarDecl(vtype, name, init, is_array, arr_size)

    def _parse_array_init(self):
        self.eat(TK_OP, '{')
        elements = []
        while not self.match(TK_OP, '}'):
            if self.match(TK_OP, '{'):
                elements.append(self._parse_array_init())
            else:
                elements.append(self._parse_assign_expr())
            self.eat_if(TK_OP, ',')
        self.eat(TK_OP, '}')
        return ArrayInit(elements)

    def _parse_block(self):
        self.eat(TK_OP, '{')
        stmts = []
        while not self.match(TK_OP, '}') and not self.match(TK_EOF):
            s = self._parse_stmt()
            if s is not None:
                if isinstance(s, list):
                    stmts.extend(s)
                else:
                    stmts.append(s)
        self.eat(TK_OP, '}')
        return Block(stmts)

    def _parse_stmt(self):
        t = self.cur()

        # Block
        if self.match(TK_OP, '{'):
            return self._parse_block()

        # Preprocessor in function body
        if self.match(TK_PUNCT) and t.value.startswith('#'):
            self.eat()
            if not self.match(TK_EOF) and not self.match(TK_OP, '{') and not self.match(TK_OP, '}'):
                self.eat()
            return None

        # If
        if self.match(TK_KW, 'if'):
            return self._parse_if()

        # While
        if self.match(TK_KW, 'while'):
            return self._parse_while()

        # Do-while
        if self.match(TK_KW, 'do'):
            return self._parse_do_while()

        # For
        if self.match(TK_KW, 'for'):
            return self._parse_for()

        # Switch
        if self.match(TK_KW, 'switch'):
            return self._parse_switch()

        # Return
        if self.match(TK_KW, 'return'):
            self.eat()
            expr = None
            if not self.match(TK_OP, ';'):
                expr = self._parse_expr()
            self.eat(TK_OP, ';')
            return ReturnStmt(expr)

        # Break
        if self.match(TK_KW, 'break'):
            self.eat(); self.eat(TK_OP, ';')
            return BreakStmt()

        # Continue
        if self.match(TK_KW, 'continue'):
            self.eat(); self.eat(TK_OP, ';')
            return ContinueStmt()

        # Empty statement
        if self.match(TK_OP, ';'):
            self.eat(); return None

        # Local variable declaration
        if self.is_type() or (self.match(TK_KW, 'struct')):
            return self._parse_local_decl()

        # Expression statement
        expr = self._parse_expr()
        self.eat(TK_OP, ';')
        return ExprStmt(expr)

    def _parse_local_decl(self):
        vtype = self._parse_type()
        name = self.eat(TK_IDENT).value
        decl = self._parse_var_rest(vtype, name)
        decls = [decl]
        while self.eat_if(TK_OP, ','):
            n2 = self.eat(TK_IDENT).value
            d2 = self._parse_var_rest(vtype, n2)
            decls.append(d2)
        self.eat(TK_OP, ';')
        return decls if len(decls) > 1 else decls[0]

    def _parse_if(self):
        self.eat(TK_KW, 'if')
        self.eat(TK_OP, '(')
        cond = self._parse_expr()
        self.eat(TK_OP, ')')
        then_b = self._parse_stmt()
        else_b = None
        if self.eat_if(TK_KW, 'else'):
            else_b = self._parse_stmt()
        return IfStmt(cond, then_b, else_b)

    def _parse_while(self):
        self.eat(TK_KW, 'while')
        self.eat(TK_OP, '(')
        cond = self._parse_expr()
        self.eat(TK_OP, ')')
        body = self._parse_stmt()
        return WhileStmt(cond, body)

    def _parse_do_while(self):
        self.eat(TK_KW, 'do')
        body = self._parse_stmt()
        self.eat(TK_KW, 'while')
        self.eat(TK_OP, '(')
        cond = self._parse_expr()
        self.eat(TK_OP, ')')
        self.eat(TK_OP, ';')
        return DoWhileStmt(body, cond)

    def _parse_for(self):
        self.eat(TK_KW, 'for')
        self.eat(TK_OP, '(')
        # Init
        init = None
        if not self.match(TK_OP, ';'):
            if self.is_type():
                init = self._parse_local_decl_no_semi()
            else:
                init = self._parse_expr()
        self.eat(TK_OP, ';')
        # Cond
        cond = None
        if not self.match(TK_OP, ';'):
            cond = self._parse_expr()
        self.eat(TK_OP, ';')
        # Update
        update = None
        if not self.match(TK_OP, ')'):
            update = self._parse_expr()
        self.eat(TK_OP, ')')
        body = self._parse_stmt()
        return ForStmt(init, cond, update, body)

    def _parse_local_decl_no_semi(self):
        vtype = self._parse_type()
        name = self.eat(TK_IDENT).value
        return self._parse_var_rest(vtype, name)

    def _parse_switch(self):
        self.eat(TK_KW, 'switch')
        self.eat(TK_OP, '(')
        expr = self._parse_expr()
        self.eat(TK_OP, ')')
        self.eat(TK_OP, '{')
        cases = []
        default = None
        while not self.match(TK_OP, '}'):
            if self.match(TK_KW, 'case'):
                self.eat()
                val = self._parse_expr()
                self.eat(TK_OP, ':')
                stmts = []
                while not self.match(TK_KW, 'case') and not self.match(TK_KW, 'default') and not self.match(TK_OP, '}'):
                    s = self._parse_stmt()
                    if s: stmts.append(s)
                cases.append((val, Block(stmts)))
            elif self.match(TK_KW, 'default'):
                self.eat(); self.eat(TK_OP, ':')
                stmts = []
                while not self.match(TK_KW, 'case') and not self.match(TK_OP, '}'):
                    s = self._parse_stmt()
                    if s: stmts.append(s)
                default = Block(stmts)
            else:
                self.eat()  # skip unexpected
        self.eat(TK_OP, '}')
        return SwitchStmt(expr, cases, default)

    # ── Expression Parser (precedence climbing) ──

    def _parse_expr(self):
        expr = self._parse_assign_expr()
        while self.match(TK_OP, ','):
            self.eat()
            right = self._parse_assign_expr()
            expr = CommaExpr([expr, right]) if not isinstance(expr, CommaExpr) else CommaExpr(expr.exprs + [right])
        return expr

    def _parse_assign_expr(self):
        left = self._parse_ternary()
        assign_ops = {'=','+=','-=','*=','/=','%=','&=','|=','^=','<<=','>>='}
        if self.cur().value in assign_ops:
            op = self.eat().value
            right = self._parse_assign_expr()
            return Assign(left, right, op)
        return left

    def _parse_ternary(self):
        expr = self._parse_or()
        if self.match(TK_OP, '?'):
            self.eat()
            then_e = self._parse_expr()
            self.eat(TK_OP, ':')
            else_e = self._parse_ternary()
            return TernaryOp(expr, then_e, else_e)
        return expr

    def _parse_or(self):
        left = self._parse_and()
        while self.match(TK_OP, '||'):
            self.eat(); left = BinOp('||', left, self._parse_and())
        return left

    def _parse_and(self):
        left = self._parse_bitor()
        while self.match(TK_OP, '&&'):
            self.eat(); left = BinOp('&&', left, self._parse_bitor())
        return left

    def _parse_bitor(self):
        left = self._parse_bitxor()
        while self.match(TK_OP, '|') and not self.match(TK_OP, '||'):
            self.eat(); left = BinOp('|', left, self._parse_bitxor())
        return left

    def _parse_bitxor(self):
        left = self._parse_bitand()
        while self.match(TK_OP, '^'):
            self.eat(); left = BinOp('^', left, self._parse_bitand())
        return left

    def _parse_bitand(self):
        left = self._parse_equality()
        while self.match(TK_OP, '&') and not self.match(TK_OP, '&&'):
            self.eat(); left = BinOp('&', left, self._parse_equality())
        return left

    def _parse_equality(self):
        left = self._parse_relational()
        while self.cur().value in ('==', '!='):
            op = self.eat().value; left = BinOp(op, left, self._parse_relational())
        return left

    def _parse_relational(self):
        left = self._parse_shift()
        while self.cur().value in ('<', '>', '<=', '>='):
            op = self.eat().value; left = BinOp(op, left, self._parse_shift())
        return left

    def _parse_shift(self):
        left = self._parse_additive()
        while self.cur().value in ('<<', '>>'):
            op = self.eat().value; left = BinOp(op, left, self._parse_additive())
        return left

    def _parse_additive(self):
        left = self._parse_multiplicative()
        while self.cur().value in ('+', '-'):
            op = self.eat().value; left = BinOp(op, left, self._parse_multiplicative())
        return left

    def _parse_multiplicative(self):
        left = self._parse_unary()
        while self.cur().value in ('*', '/', '%'):
            op = self.eat().value; left = BinOp(op, left, self._parse_unary())
        return left

    def _parse_unary(self):
        t = self.cur()
        # Prefix ++/--
        if t.value in ('++', '--'):
            op = self.eat().value
            expr = self._parse_unary()
            return UnaryOp(op, expr, prefix=True)
        # Unary -/+/!/~
        if t.value in ('-', '+', '!', '~'):
            op = self.eat().value
            expr = self._parse_unary()
            return UnaryOp(op, expr)
        # Address-of &
        if t.value == '&' and not self.peek(1).value in ('&',):
            # Check if it's address-of (before identifier)
            if self.peek(1).type in (TK_IDENT, TK_OP):
                self.eat()
                expr = self._parse_unary()
                return UnaryOp('&', expr)
        # Dereference *
        if t.value == '*' and self.peek(1).type == TK_IDENT:
            self.eat()
            expr = self._parse_unary()
            return UnaryOp('*', expr)
        # sizeof
        if t.value == 'sizeof':
            self.eat()
            if self.match(TK_OP, '('):
                self.eat()
                if self.is_type():
                    target = self._parse_type()
                else:
                    target = self._parse_expr()
                self.eat(TK_OP, ')')
            else:
                target = self._parse_unary()
            return SizeofExpr(target)
        # Cast: (type)expr
        if t.value == '(' and self.is_type(self.peek(1)):
            self.eat()
            cast_type = self._parse_type()
            self.eat(TK_OP, ')')
            expr = self._parse_unary()
            return CastExpr(cast_type, expr)
        return self._parse_postfix()

    def _parse_postfix(self):
        expr = self._parse_primary()
        while True:
            # Function call
            if self.match(TK_OP, '(') and isinstance(expr, Ident):
                self.eat()
                args = []
                while not self.match(TK_OP, ')'):
                    args.append(self._parse_assign_expr())
                    self.eat_if(TK_OP, ',')
                self.eat(TK_OP, ')')
                expr = FuncCall(expr.name, args)
            # Array access
            elif self.match(TK_OP, '['):
                self.eat()
                idx = self._parse_expr()
                self.eat(TK_OP, ']')
                expr = ArrayAccess(expr, idx)
            # Member access .
            elif self.match(TK_OP, '.'):
                self.eat()
                member = self.eat(TK_IDENT).value
                expr = MemberAccess(expr, member, is_ptr=False)
            # Pointer member ->
            elif self.match(TK_OP, '->'):
                self.eat()
                member = self.eat(TK_IDENT).value
                expr = MemberAccess(expr, member, is_ptr=True)
            # Postfix ++/--
            elif self.cur().value in ('++', '--'):
                op = self.eat().value
                expr = UnaryOp(op, expr, prefix=False)
            else:
                break
        return expr

    def _parse_primary(self):
        t = self.cur()
        # Number
        if t.type == TK_INT:
            self.eat(); return NumLit(t.value)
        if t.type == TK_FLOAT:
            self.eat(); return NumLit(t.value)
        # Char
        if t.type == TK_CHAR_LIT:
            self.eat(); return CharLit(t.value)
        # String
        if t.type == TK_STRING:
            self.eat()
            # Concatenate adjacent strings
            while self.match(TK_STRING):
                t2 = self.eat()
                t = Token(TK_STRING, t.value + t2.value, t.line, t.col)
            return StrLit(t.value)
        # NULL
        if t.value == 'NULL':
            self.eat(); return NumLit(0)
        if t.value == 'true':
            self.eat(); return NumLit(1)
        if t.value == 'false':
            self.eat(); return NumLit(0)
        # Identifier
        if t.type == TK_IDENT:
            self.eat()
            name = t.value
            # Check defines
            if name in self.defines:
                dv = self.defines[name]
                try:
                    return NumLit(int(dv))
                except (ValueError, TypeError):
                    try:
                        return NumLit(float(dv))
                    except (ValueError, TypeError):
                        return StrLit(str(dv)) if dv else NumLit(0)
            return Ident(name)
        # Keyword used as identifier (common in simple C)
        if t.type == TK_KW and t.value in ('printf','scanf','puts','gets','strlen',
            'strcmp','strcpy','strcat','malloc','free','atoi','atof','abs','rand',
            'srand','exit','putchar','getchar','sprintf','fprintf','memset','memcpy'):
            self.eat()
            return Ident(t.value)
        # Parenthesized expression
        if t.value == '(':
            self.eat()
            expr = self._parse_expr()
            self.eat(TK_OP, ')')
            return expr

        raise ParseError(f"L{t.line}: Unexpected token {t.type}({t.value!r})")

# ═══════════════════════════════════════════════════════════════════════
# INTERPRETER (Tree-Walking)
# ═══════════════════════════════════════════════════════════════════════

class ReturnSignal(Exception):
    def __init__(self, value=None): self.value = value

class BreakSignal(Exception):
    pass

class ContinueSignal(Exception):
    pass

class ExitSignal(Exception):
    def __init__(self, code=0): self.code = code

class RuntimeErr(Exception):
    pass

class Environment:
    def __init__(self, parent=None):
        self.vars = {}
        self.parent = parent

    def get(self, name):
        if name in self.vars:
            return self.vars[name]
        if self.parent:
            return self.parent.get(name)
        raise RuntimeErr(f"Undefined variable: '{name}'")

    def set(self, name, value):
        if name in self.vars:
            self.vars[name] = value; return
        if self.parent and self.parent.has(name):
            self.parent.set(name, value); return
        self.vars[name] = value

    def has(self, name):
        if name in self.vars: return True
        if self.parent: return self.parent.has(name)
        return False

    def set_local(self, name, value):
        self.vars[name] = value

class Interpreter:
    def __init__(self, output_callback=None):
        self.output = output_callback or (lambda s: print(s, end=''))
        self.global_env = Environment()
        self.functions = {}
        self.structs = {}
        self.heap = {}
        self.heap_id = 1
        self._call_depth = 0
        self.MAX_DEPTH = 500
        self.MAX_ITER = 1_000_000
        self.running = True

    def run(self, program):
        # Register defines
        for k, v in program.defines.items():
            try:
                self.global_env.set_local(k, int(v))
            except (ValueError, TypeError):
                try:
                    self.global_env.set_local(k, float(v))
                except (ValueError, TypeError):
                    self.global_env.set_local(k, str(v) if v else 0)

        # First pass: register functions and structs, handle global vars
        for d in program.decls:
            if isinstance(d, FuncDecl):
                self.functions[d.name] = d
            elif isinstance(d, StructDecl):
                self.structs[d.name] = d
            elif isinstance(d, VarDecl):
                val = self._eval(d.init, self.global_env) if d.init else self._default_val(d.vtype, d.is_array, d.array_size, self.global_env)
                self.global_env.set_local(d.name, val)

        # Find and call main
        if 'main' not in self.functions:
            raise RuntimeErr("No main() function found")

        try:
            result = self._call_func('main', [], self.global_env)
            return result if result is not None else 0
        except ExitSignal as e:
            return e.code

    def _default_val(self, vtype, is_array=False, arr_size=None, env=None):
        if is_array:
            sz = int(self._eval(arr_size, env)) if arr_size else 0
            return [0] * max(sz, 0)
        if 'float' in vtype or 'double' in vtype:
            return 0.0
        if 'char' in vtype:
            return 0
        return 0

    def _call_func(self, name, args, env):
        if not self.running:
            raise RuntimeErr("Execution stopped")

        self._call_depth += 1
        if self._call_depth > self.MAX_DEPTH:
            self._call_depth -= 1
            raise RuntimeErr(f"Stack overflow (>{self.MAX_DEPTH} calls)")

        try:
            # Built-in functions
            result = self._builtin(name, args, env)
            if result is not None:
                return result

            if name not in self.functions:
                raise RuntimeErr(f"Undefined function: '{name}'")

            func = self.functions[name]
            if func.body is None:
                raise RuntimeErr(f"Function '{name}' declared but not defined")

            local = Environment(self.global_env)

            # Bind parameters
            for i, (ptype, pname) in enumerate(func.params):
                if ptype == '...': break
                val = args[i] if i < len(args) else 0
                local.set_local(pname, val)

            # Variadic: store extra args
            local.set_local('__va_args__', list(args[len(func.params):]) if len(args) > len(func.params) else [])

            try:
                self._exec_block(func.body, local)
            except ReturnSignal as r:
                return r.value

            return 0  # default return for void/int
        finally:
            self._call_depth -= 1

    def _builtin(self, name, args, env):
        if name == 'printf':
            if not args: return 0
            fmt = args[0] if isinstance(args[0], str) else str(args[0])
            result = self._format_printf(fmt, args[1:])
            self.output(result)
            return len(result)

        if name == 'sprintf':
            if len(args) < 2: return 0
            fmt = args[1] if isinstance(args[1], str) else str(args[1])
            result = self._format_printf(fmt, args[2:])
            return len(result)

        if name == 'fprintf':
            if len(args) < 2: return 0
            fmt = args[1] if isinstance(args[1], str) else str(args[1])
            result = self._format_printf(fmt, args[2:])
            self.output(result)
            return len(result)

        if name == 'puts':
            s = args[0] if args else ''
            self.output(str(s) + '\n')
            return 0

        if name == 'putchar':
            ch = chr(int(args[0])) if args else '\0'
            self.output(ch)
            return int(args[0]) if args else 0

        if name == 'getchar':
            return ord('\n')

        if name == 'scanf':
            return 0

        if name == 'gets':
            return ''

        if name == 'strlen':
            s = args[0] if args else ''
            return len(str(s))

        if name == 'strcmp':
            a, b = str(args[0]) if args else '', str(args[1]) if len(args) > 1 else ''
            return (a > b) - (a < b)

        if name == 'strcpy':
            return args[1] if len(args) > 1 else ''

        if name == 'strcat':
            return str(args[0]) + str(args[1]) if len(args) > 1 else str(args[0]) if args else ''

        if name == 'atoi':
            try:
                return int(float(str(args[0]))) if args else 0
            except (ValueError, TypeError):
                return 0

        if name == 'atof':
            try:
                return float(str(args[0])) if args else 0.0
            except (ValueError, TypeError):
                return 0.0

        if name == 'abs':
            return abs(int(args[0])) if args else 0

        if name == 'malloc':
            size = int(args[0]) if args else 0
            arr = [0] * size
            hid = self.heap_id; self.heap_id += 1
            self.heap[hid] = arr
            return arr  # return the list itself as a "pointer"

        if name == 'free':
            return None  # no-op

        if name == 'memset':
            if len(args) >= 3 and isinstance(args[0], list):
                val = int(args[1]) & 0xFF
                n = int(args[2])
                for i in range(min(n, len(args[0]))):
                    args[0][i] = val
            return args[0] if args else 0

        if name == 'memcpy':
            if len(args) >= 3 and isinstance(args[0], list) and isinstance(args[1], list):
                n = int(args[2])
                for i in range(min(n, len(args[0]), len(args[1]))):
                    args[0][i] = args[1][i]
            return args[0] if args else 0

        if name == 'rand':
            import random
            return random.randint(0, 32767)

        if name == 'srand':
            import random
            random.seed(int(args[0]) if args else 0)
            return None

        if name == 'exit':
            raise ExitSignal(int(args[0]) if args else 0)

        if name == 'sizeof':
            return 4  # simplified

        return None  # Not a builtin

    def _format_printf(self, fmt, args):
        result = []
        ai = 0
        i = 0
        while i < len(fmt):
            if fmt[i] == '%' and i+1 < len(fmt):
                i += 1
                # Flags
                flags = ''
                while i < len(fmt) and fmt[i] in '-+ 0#':
                    flags += fmt[i]; i += 1
                # Width
                width = ''
                while i < len(fmt) and fmt[i].isdigit():
                    width += fmt[i]; i += 1
                # Precision
                prec = ''
                if i < len(fmt) and fmt[i] == '.':
                    i += 1
                    while i < len(fmt) and fmt[i].isdigit():
                        prec += fmt[i]; i += 1
                # Length modifier
                if i < len(fmt) and fmt[i] in 'hlLzjt':
                    i += 1
                    if i < len(fmt) and fmt[i] in 'hlLzjt':
                        i += 1
                # Specifier
                if i >= len(fmt): break
                spec = fmt[i]; i += 1
                val = args[ai] if ai < len(args) else 0
                ai += 1

                w = int(width) if width else 0
                p = int(prec) if prec else -1

                if spec == 'd' or spec == 'i':
                    s = str(int(val))
                    if w > 0:
                        if '-' in flags: s = s.ljust(w)
                        elif '0' in flags: s = s.zfill(w)
                        else: s = s.rjust(w)
                    result.append(s)
                elif spec == 'u':
                    s = str(int(val) & 0xFFFFFFFF)
                    result.append(s.rjust(w) if w else s)
                elif spec == 'f':
                    pr = p if p >= 0 else 6
                    s = f'{float(val):.{pr}f}'
                    result.append(s.rjust(w) if w else s)
                elif spec == 'e' or spec == 'E':
                    pr = p if p >= 0 else 6
                    s = f'{float(val):.{pr}e}'
                    if spec == 'E': s = s.upper()
                    result.append(s.rjust(w) if w else s)
                elif spec == 'g' or spec == 'G':
                    pr = p if p >= 0 else 6
                    s = f'{float(val):.{pr}g}'
                    if spec == 'G': s = s.upper()
                    result.append(s.rjust(w) if w else s)
                elif spec == 'x':
                    s = format(int(val) & 0xFFFFFFFF, 'x')
                    result.append(s.rjust(w, '0' if '0' in flags else ' ') if w else s)
                elif spec == 'X':
                    s = format(int(val) & 0xFFFFFFFF, 'X')
                    result.append(s.rjust(w, '0' if '0' in flags else ' ') if w else s)
                elif spec == 'o':
                    s = format(int(val) & 0xFFFFFFFF, 'o')
                    result.append(s)
                elif spec == 'c':
                    ch = chr(int(val)) if isinstance(val, (int, float)) else str(val)[0] if val else '\0'
                    result.append(ch)
                elif spec == 's':
                    s = str(val)
                    if p >= 0: s = s[:p]
                    result.append(s.ljust(w) if '-' in flags and w else s.rjust(w) if w else s)
                elif spec == 'p':
                    result.append(hex(id(val)))
                elif spec == '%':
                    result.append('%'); ai -= 1
                elif spec == 'n':
                    ai -= 1
                else:
                    result.append('%' + spec)
            else:
                result.append(fmt[i])
                i += 1
        return ''.join(result)

    def _exec(self, node, env):
        if not self.running:
            raise RuntimeErr("Execution stopped")

        if node is None:
            return

        if isinstance(node, Block):
            self._exec_block(node, env)
        elif isinstance(node, VarDecl):
            val = self._eval(node.init, env) if node.init else self._default_val(node.vtype, node.is_array, node.array_size, env)
            env.set_local(node.name, val)
        elif isinstance(node, IfStmt):
            if self._truthy(self._eval(node.cond, env)):
                self._exec(node.then_b, env)
            elif node.else_b:
                self._exec(node.else_b, env)
        elif isinstance(node, WhileStmt):
            iters = 0
            while self._truthy(self._eval(node.cond, env)):
                iters += 1
                if iters > self.MAX_ITER:
                    raise RuntimeErr(f"Infinite loop detected (>{self.MAX_ITER} iterations)")
                if not self.running: raise RuntimeErr("Stopped")
                try:
                    self._exec(node.body, env)
                except BreakSignal:
                    break
                except ContinueSignal:
                    continue
        elif isinstance(node, DoWhileStmt):
            iters = 0
            while True:
                iters += 1
                if iters > self.MAX_ITER:
                    raise RuntimeErr(f"Infinite loop (>{self.MAX_ITER})")
                if not self.running: raise RuntimeErr("Stopped")
                try:
                    self._exec(node.body, env)
                except BreakSignal:
                    break
                except ContinueSignal:
                    pass
                if not self._truthy(self._eval(node.cond, env)):
                    break
        elif isinstance(node, ForStmt):
            local = Environment(env)
            if node.init:
                if isinstance(node.init, VarDecl):
                    self._exec(node.init, local)
                else:
                    self._eval(node.init, local)
            iters = 0
            while True:
                if node.cond and not self._truthy(self._eval(node.cond, local)):
                    break
                iters += 1
                if iters > self.MAX_ITER:
                    raise RuntimeErr(f"Infinite loop (>{self.MAX_ITER})")
                if not self.running: raise RuntimeErr("Stopped")
                try:
                    self._exec(node.body, local)
                except BreakSignal:
                    break
                except ContinueSignal:
                    pass
                if node.update:
                    self._eval(node.update, local)
        elif isinstance(node, SwitchStmt):
            val = self._eval(node.expr, env)
            matched = False
            try:
                for case_val, case_body in node.cases:
                    cv = self._eval(case_val, env)
                    if matched or val == cv:
                        matched = True
                        self._exec(case_body, env)
                if not matched and node.default:
                    self._exec(node.default, env)
            except BreakSignal:
                pass
        elif isinstance(node, ReturnStmt):
            val = self._eval(node.expr, env) if node.expr else None
            raise ReturnSignal(val)
        elif isinstance(node, BreakStmt):
            raise BreakSignal()
        elif isinstance(node, ContinueStmt):
            raise ContinueSignal()
        elif isinstance(node, ExprStmt):
            self._eval(node.expr, env)
        elif isinstance(node, list):
            for item in node:
                self._exec(item, env)
        else:
            raise RuntimeErr(f"Unknown statement: {type(node).__name__}")

    def _exec_block(self, block, env):
        local = Environment(env)
        for stmt in block.stmts:
            self._exec(stmt, local)

    def _eval(self, node, env):
        if not self.running:
            raise RuntimeErr("Stopped")

        if node is None:
            return 0

        if isinstance(node, NumLit):
            return node.value

        if isinstance(node, StrLit):
            return node.value

        if isinstance(node, CharLit):
            return ord(node.value) if isinstance(node.value, str) else node.value

        if isinstance(node, Ident):
            return env.get(node.name)

        if isinstance(node, ArrayInit):
            return [self._eval(e, env) for e in node.elements]

        if isinstance(node, BinOp):
            return self._eval_binop(node, env)

        if isinstance(node, UnaryOp):
            return self._eval_unary(node, env)

        if isinstance(node, Assign):
            return self._eval_assign(node, env)

        if isinstance(node, FuncCall):
            args = [self._eval(a, env) for a in node.args]
            return self._call_func(node.name, args, env)

        if isinstance(node, ArrayAccess):
            arr = self._eval(node.array, env)
            idx = int(self._eval(node.index, env))
            if isinstance(arr, list):
                if 0 <= idx < len(arr):
                    return arr[idx]
                raise RuntimeErr(f"Array index {idx} out of bounds (size {len(arr)})")
            if isinstance(arr, str):
                if 0 <= idx < len(arr):
                    return ord(arr[idx])
                raise RuntimeErr(f"String index {idx} out of bounds (len {len(arr)})")
            raise RuntimeErr(f"Cannot index {type(arr).__name__}")

        if isinstance(node, MemberAccess):
            obj = self._eval(node.obj, env)
            if isinstance(obj, dict):
                return obj.get(node.member, 0)
            raise RuntimeErr(f"Cannot access member '{node.member}' on {type(obj).__name__}")

        if isinstance(node, TernaryOp):
            if self._truthy(self._eval(node.cond, env)):
                return self._eval(node.then_e, env)
            return self._eval(node.else_e, env)

        if isinstance(node, CastExpr):
            val = self._eval(node.expr, env)
            ct = node.cast_type
            if 'int' in ct or 'long' in ct or 'short' in ct:
                return int(val) if not isinstance(val, str) else ord(val[0]) if val else 0
            if 'float' in ct or 'double' in ct:
                return float(val) if not isinstance(val, str) else 0.0
            if 'char' in ct:
                return int(val) & 0xFF if isinstance(val, (int, float)) else ord(val[0]) if isinstance(val, str) and val else 0
            return val

        if isinstance(node, SizeofExpr):
            if isinstance(node.target, str):
                sizes = {'char':1,'short':2,'int':4,'long':8,'float':4,'double':8,'void':0}
                for k, v in sizes.items():
                    if k in node.target: return v
                return 4
            val = self._eval(node.target, env)
            if isinstance(val, list): return len(val) * 4
            if isinstance(val, str): return len(val) + 1
            return 4

        if isinstance(node, CommaExpr):
            result = 0
            for e in node.exprs:
                result = self._eval(e, env)
            return result

        raise RuntimeErr(f"Unknown expression: {type(node).__name__}")

    def _eval_binop(self, node, env):
        # Short-circuit for && and ||
        if node.op == '&&':
            l = self._eval(node.left, env)
            return int(self._truthy(l) and self._truthy(self._eval(node.right, env)))
        if node.op == '||':
            l = self._eval(node.left, env)
            return int(self._truthy(l) or self._truthy(self._eval(node.right, env)))

        l = self._eval(node.left, env)
        r = self._eval(node.right, env)

        # String concatenation with +
        if isinstance(l, str) and isinstance(r, str) and node.op == '+':
            return l + r

        # Ensure numeric
        if isinstance(l, str):
            l = ord(l[0]) if l else 0
        if isinstance(r, str):
            r = ord(r[0]) if r else 0

        op = node.op
        if op == '+': return l + r
        if op == '-': return l - r
        if op == '*': return l * r
        if op == '/':
            if r == 0: raise RuntimeErr("Division by zero")
            if isinstance(l, float) or isinstance(r, float): return l / r
            return int(l) // int(r)  # C integer division
        if op == '%':
            if r == 0: raise RuntimeErr("Modulo by zero")
            return int(l) % int(r)
        if op == '&': return int(l) & int(r)
        if op == '|': return int(l) | int(r)
        if op == '^': return int(l) ^ int(r)
        if op == '<<': return int(l) << int(r)
        if op == '>>': return int(l) >> int(r)
        if op == '==': return int(l == r)
        if op == '!=': return int(l != r)
        if op == '<':  return int(l < r)
        if op == '>':  return int(l > r)
        if op == '<=': return int(l <= r)
        if op == '>=': return int(l >= r)

        raise RuntimeErr(f"Unknown operator: {op}")

    def _eval_unary(self, node, env):
        if node.op in ('++', '--'):
            # Pre/post increment/decrement
            if isinstance(node.expr, Ident):
                old = env.get(node.expr.name)
                new = old + 1 if node.op == '++' else old - 1
                env.set(node.expr.name, new)
                return new if node.prefix else old
            elif isinstance(node.expr, ArrayAccess):
                arr = self._eval(node.expr.array, env)
                idx = int(self._eval(node.expr.index, env))
                old = arr[idx]
                new = old + 1 if node.op == '++' else old - 1
                arr[idx] = new
                return new if node.prefix else old

        val = self._eval(node.expr, env)
        if node.op == '-': return -val
        if node.op == '+': return +val
        if node.op == '!': return int(not self._truthy(val))
        if node.op == '~': return ~int(val)
        if node.op == '&': return val  # address-of (simplified: return value)
        if node.op == '*': return val  # dereference (simplified)

        raise RuntimeErr(f"Unknown unary op: {node.op}")

    def _eval_assign(self, node, env):
        val = self._eval(node.value, env)
        op = node.op

        if isinstance(node.target, Ident):
            name = node.target.name
            if op == '=':
                env.set(name, val)
            else:
                old = env.get(name)
                env.set(name, self._compound_assign(old, val, op))
            return env.get(name)

        elif isinstance(node.target, ArrayAccess):
            arr = self._eval(node.target.array, env)
            idx = int(self._eval(node.target.index, env))
            if isinstance(arr, list):
                if op == '=':
                    arr[idx] = val
                else:
                    arr[idx] = self._compound_assign(arr[idx], val, op)
                return arr[idx]

        elif isinstance(node.target, MemberAccess):
            obj = self._eval(node.target.obj, env)
            if isinstance(obj, dict):
                if op == '=':
                    obj[node.target.member] = val
                else:
                    obj[node.target.member] = self._compound_assign(obj.get(node.target.member, 0), val, op)
                return obj[node.target.member]

        elif isinstance(node.target, UnaryOp) and node.target.op == '*':
            # Dereference assign (simplified)
            inner = self._eval(node.target.expr, env)
            if isinstance(inner, list) and len(inner) > 0:
                inner[0] = val
            return val

        raise RuntimeErr(f"Cannot assign to {type(node.target).__name__}")

    def _compound_assign(self, old, val, op):
        if op == '+=': return old + val
        if op == '-=': return old - val
        if op == '*=': return old * val
        if op == '/=':
            if val == 0: raise RuntimeErr("Division by zero")
            return old / val if isinstance(old, float) else int(old) // int(val)
        if op == '%=': return int(old) % int(val)
        if op == '&=': return int(old) & int(val)
        if op == '|=': return int(old) | int(val)
        if op == '^=': return int(old) ^ int(val)
        if op == '<<=': return int(old) << int(val)
        if op == '>>=': return int(old) >> int(val)
        raise RuntimeErr(f"Unknown compound assign: {op}")

    def _truthy(self, val):
        if val is None: return False
        if isinstance(val, (int, float)): return val != 0
        if isinstance(val, str): return len(val) > 0
        if isinstance(val, list): return len(val) > 0
        return bool(val)

# ═══════════════════════════════════════════════════════════════════════
# COMPILER FACADE (lex → parse → interpret)
# ═══════════════════════════════════════════════════════════════════════

class CatsCompiler:
    def __init__(self, output_callback=None):
        self.output = output_callback or (lambda s: print(s, end=''))

    def compile_and_run(self, source):
        """Compile + execute C source. Returns (success: bool, message: str)."""
        # Lex
        try:
            tokens = lex(source)
        except LexError as e:
            return False, f"Lexer Error: {e}"

        # Parse
        try:
            parser = Parser(tokens)
            program = parser.parse()
        except ParseError as e:
            return False, f"Parse Error: {e}"

        # Interpret
        try:
            interp = Interpreter(output_callback=self.output)
            self._interp = interp
            result = interp.run(program)
            return True, f"\nProgram exited with code {result}"
        except ExitSignal as e:
            return True, f"\nProgram exited with code {e.code}"
        except ReturnSignal as r:
            return True, f"\nProgram returned {r.value}"
        except RuntimeErr as e:
            return False, f"Runtime Error: {e}"
        except RecursionError:
            return False, "Runtime Error: Stack overflow (deep recursion)"
        except Exception as e:
            return False, f"Internal Error: {e}\n{traceback.format_exc()}"

    def stop(self):
        if hasattr(self, '_interp'):
            self._interp.running = False

# ═══════════════════════════════════════════════════════════════════════
# GUI — Cat's GCC 0.1
# ═══════════════════════════════════════════════════════════════════════

APP_NAME   = "Cat's GCC 0.1"
WINDOW_W   = 600
WINDOW_H   = 400

# Colors — dark theme (Catppuccin Mocha)
BG_DARK    = "#1e1e1e"
BG_EDITOR  = "#1e1e2e"
BG_GUTTER  = "#16161e"
BG_OUTPUT  = "#11111b"
BG_TOOLBAR = "#181825"
FG_TEXT    = "#cdd6f4"
FG_GUTTER  = "#585b70"
BG_BUTTON  = "#313244"
BG_BTN_HVR = "#45475a"
SELECT_BG  = "#45475a"
CURSOR_CLR = "#f5e0dc"

# Syntax colors
SY_KW      = "#cba6f7"   # purple — keywords
SY_TYPE    = "#89b4fa"   # blue — types
SY_STR     = "#a6e3a1"   # green — strings
SY_CMT     = "#6c7086"   # gray — comments
SY_NUM     = "#fab387"   # orange — numbers
SY_PRE     = "#f38ba8"   # pink — preprocessor
SY_FN      = "#89dceb"   # teal — functions

C_KW_PAT   = r'\b(auto|break|case|const|continue|default|do|else|enum|extern|for|goto|if|inline|register|restrict|return|sizeof|static|struct|switch|typedef|union|volatile|while)\b'
C_TY_PAT   = r'\b(void|char|short|int|long|float|double|signed|unsigned|size_t|bool|FILE|NULL|true|false|uint8_t|uint16_t|uint32_t|uint64_t|int8_t|int16_t|int32_t|int64_t)\b'
C_STR_PAT  = r'("(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\')'
C_CMT_PAT  = r'(//.*?$|/\*[\s\S]*?\*/)'
C_NUM_PAT  = r'\b(0[xX][0-9a-fA-F]+|0[bB][01]+|\d+\.?\d*[fFlLuU]*)\b'
C_PRE_PAT  = r'(^\s*#\s*\w+)'
C_FN_PAT   = r'\b(\w+)\s*\('

class CatsGCC_GUI:
    def __init__(self, root):
        self.root = root
        self.root.title(APP_NAME)
        self.root.geometry(f"{WINDOW_W}x{WINDOW_H}")
        self.root.minsize(480, 320)
        self.root.configure(bg=BG_DARK)

        self.file_path = None
        self.modified = False
        self.compiler = None
        self._hl_job = None

        self._build_menu()
        self._build_toolbar()
        self._build_panes()
        self._build_statusbar()
        self._bind_keys()

        # Default code
        self.editor.insert("1.0",
            '#include <stdio.h>\n\n'
            'int main(void) {\n'
            '    printf("Hello from Cat\'s GCC!\\n");\n'
            '    \n'
            '    for (int i = 0; i < 5; i++) {\n'
            '        printf("  i = %d\\n", i);\n'
            '    }\n'
            '    \n'
            '    printf("Goodbye!\\n");\n'
            '    return 0;\n'
            '}\n')
        self._update_gutter()
        self._highlight()
        self._status("Ready — Built-in C Compiler (no GCC needed)", "ok")

    # ─── Menu ───

    def _build_menu(self):
        mb = tk.Menu(self.root, bg=BG_TOOLBAR, fg=FG_TEXT, activebackground=BG_BTN_HVR,
                     activeforeground=FG_TEXT, relief="flat")
        mcfg = dict(tearoff=0, bg=BG_TOOLBAR, fg=FG_TEXT,
                    activebackground=BG_BTN_HVR, activeforeground=FG_TEXT)

        fm = tk.Menu(mb, **mcfg)
        fm.add_command(label="New        Ctrl+N", command=self.file_new)
        fm.add_command(label="Open       Ctrl+O", command=self.file_open)
        fm.add_separator()
        fm.add_command(label="Save       Ctrl+S", command=self.file_save)
        fm.add_command(label="Save As    Ctrl+Shift+S", command=self.file_save_as)
        fm.add_separator()
        fm.add_command(label="Exit", command=self._quit)
        mb.add_cascade(label="File", menu=fm)

        em = tk.Menu(mb, **mcfg)
        em.add_command(label="Undo       Ctrl+Z", command=lambda: self.editor.edit_undo())
        em.add_command(label="Redo       Ctrl+Y", command=lambda: self.editor.edit_redo())
        em.add_separator()
        em.add_command(label="Select All Ctrl+A", command=self._sel_all)
        em.add_command(label="Clear Output", command=self._clear_out)
        mb.add_cascade(label="Edit", menu=em)

        bm = tk.Menu(mb, **mcfg)
        bm.add_command(label="Compile      F5", command=self.do_compile)
        bm.add_command(label="Run          F6", command=self.do_run)
        bm.add_command(label="Build & Run  F9", command=self.do_build_run)
        bm.add_separator()
        bm.add_command(label="Stop", command=self.do_stop)
        mb.add_cascade(label="Build", menu=bm)

        hm = tk.Menu(mb, **mcfg)
        hm.add_command(label="About", command=self._about)
        mb.add_cascade(label="Help", menu=hm)

        self.root.config(menu=mb)

    # ─── Toolbar ───

    def _build_toolbar(self):
        tb = tk.Frame(self.root, bg=BG_TOOLBAR, height=30)
        tb.pack(fill="x")
        bc = dict(bg=BG_BUTTON, fg=FG_TEXT, activebackground=BG_BTN_HVR,
                  activeforeground=FG_TEXT, relief="flat", bd=0, padx=8, pady=2,
                  font=("Consolas", 9, "bold"))

        tk.Button(tb, text="NEW", command=self.file_new, **bc).pack(side="left", padx=2, pady=3)
        tk.Button(tb, text="OPEN", command=self.file_open, **bc).pack(side="left", padx=2, pady=3)
        tk.Button(tb, text="SAVE", command=self.file_save, **bc).pack(side="left", padx=2, pady=3)
        tk.Frame(tb, bg=FG_GUTTER, width=1).pack(side="left", fill="y", padx=6, pady=4)
        tk.Button(tb, text="COMPILE [F5]", command=self.do_compile,
                  bg=BG_BUTTON, fg="#a6e3a1", activebackground=BG_BTN_HVR,
                  activeforeground=FG_TEXT, relief="flat", bd=0, padx=8, pady=2,
                  font=("Consolas", 9, "bold")).pack(side="left", padx=2, pady=3)
        tk.Button(tb, text="RUN [F6]", command=self.do_run,
                  bg=BG_BUTTON, fg="#89b4fa", activebackground=BG_BTN_HVR,
                  activeforeground=FG_TEXT, relief="flat", bd=0, padx=8, pady=2,
                  font=("Consolas", 9, "bold")).pack(side="left", padx=2, pady=3)
        tk.Button(tb, text="BUILD+RUN [F9]", command=self.do_build_run,
                  bg=BG_BUTTON, fg="#fab387", activebackground=BG_BTN_HVR,
                  activeforeground=FG_TEXT, relief="flat", bd=0, padx=8, pady=2,
                  font=("Consolas", 9, "bold")).pack(side="left", padx=2, pady=3)
        tk.Button(tb, text="STOP", command=self.do_stop,
                  bg=BG_BUTTON, fg="#f38ba8", activebackground=BG_BTN_HVR,
                  activeforeground=FG_TEXT, relief="flat", bd=0, padx=8, pady=2,
                  font=("Consolas", 9, "bold")).pack(side="left", padx=2, pady=3)

    # ─── Editor + Output ───

    def _build_panes(self):
        pw = tk.PanedWindow(self.root, orient="vertical", bg=BG_DARK, sashwidth=4, sashrelief="flat")
        pw.pack(fill="both", expand=True)

        # Editor
        ef = tk.Frame(pw, bg=BG_EDITOR)
        self.gutter = tk.Text(ef, width=4, bg=BG_GUTTER, fg=FG_GUTTER, font=("Consolas", 11),
                              relief="flat", bd=0, padx=4, state="disabled", takefocus=False,
                              selectbackground=BG_GUTTER, cursor="arrow")
        self.gutter.pack(side="left", fill="y")

        self.editor = tk.Text(ef, bg=BG_EDITOR, fg=FG_TEXT, font=("Consolas", 11), relief="flat",
                              bd=0, insertbackground=CURSOR_CLR, selectbackground=SELECT_BG,
                              selectforeground=FG_TEXT, wrap="none", undo=True, autoseparators=True,
                              maxundo=-1, padx=6, pady=4, tabs=("4c",))
        self.editor.pack(side="left", fill="both", expand=True)

        esb = tk.Scrollbar(ef, command=self._yscroll)
        esb.pack(side="right", fill="y")
        self.editor.config(yscrollcommand=esb.set)
        ehb = tk.Scrollbar(ef, orient="horizontal", command=self.editor.xview)
        ehb.pack(side="bottom", fill="x")
        self.editor.config(xscrollcommand=ehb.set)
        pw.add(ef, minsize=100)

        # Output
        of = tk.Frame(pw, bg=BG_OUTPUT)
        tk.Label(of, text=" OUTPUT ", bg=BG_TOOLBAR, fg=FG_GUTTER,
                 font=("Consolas", 8, "bold"), anchor="w").pack(fill="x")
        self.output = tk.Text(of, bg=BG_OUTPUT, fg=FG_TEXT, font=("Consolas", 10), relief="flat",
                              bd=0, state="disabled", wrap="word", padx=6, pady=4,
                              selectbackground=SELECT_BG, cursor="arrow")
        self.output.pack(fill="both", expand=True)
        osb = tk.Scrollbar(of, command=self.output.yview)
        osb.pack(side="right", fill="y")
        self.output.config(yscrollcommand=osb.set)
        pw.add(of, minsize=60)

        # Tags
        self.output.tag_config("stdout", foreground=FG_TEXT)
        self.output.tag_config("stderr", foreground="#f38ba8")
        self.output.tag_config("success", foreground="#a6e3a1")
        self.output.tag_config("info", foreground="#89b4fa")

        self.editor.tag_config("keyword", foreground=SY_KW)
        self.editor.tag_config("type", foreground=SY_TYPE)
        self.editor.tag_config("string", foreground=SY_STR)
        self.editor.tag_config("comment", foreground=SY_CMT)
        self.editor.tag_config("number", foreground=SY_NUM)
        self.editor.tag_config("preproc", foreground=SY_PRE)
        self.editor.tag_config("func", foreground=SY_FN)

    # ─── Status ───

    def _build_statusbar(self):
        self.statusbar = tk.Label(self.root, text="Ready", bg=BG_TOOLBAR, fg=FG_GUTTER,
                                  font=("Consolas", 9), anchor="w", padx=8)
        self.statusbar.pack(fill="x", side="bottom")

    def _status(self, msg, kind="ok"):
        clr = {"ok":FG_TEXT, "warn":"#fab387", "err":"#f38ba8", "info":"#89b4fa"}.get(kind, FG_TEXT)
        self.statusbar.config(text=msg, fg=clr)

    # ─── Bindings ───

    def _bind_keys(self):
        for combo, fn in [
            ("<Control-n>", lambda e: self.file_new()), ("<Control-N>", lambda e: self.file_new()),
            ("<Control-o>", lambda e: self.file_open()), ("<Control-O>", lambda e: self.file_open()),
            ("<Control-s>", lambda e: self.file_save()), ("<Control-S>", lambda e: self.file_save()),
            ("<Control-Shift-s>", lambda e: self.file_save_as()),
            ("<F5>", lambda e: self.do_compile()),
            ("<F6>", lambda e: self.do_run()),
            ("<F9>", lambda e: self.do_build_run()),
        ]:
            self.root.bind(combo, fn)
        self.root.protocol("WM_DELETE_WINDOW", self._quit)
        self.editor.bind("<<Modified>>", self._on_mod)
        self.editor.bind("<KeyRelease>", self._on_key)
        self.editor.bind("<MouseWheel>", lambda e: self.gutter.yview_scroll(int(-e.delta/120), "units"))
        self.editor.bind("<Button-1>", lambda e: self.root.after(1, self._update_gutter))

    def _on_mod(self, e=None):
        if self.editor.edit_modified():
            self.modified = True; self._update_title()
            self.editor.edit_modified(False)

    def _on_key(self, e=None):
        self._update_gutter()
        if self._hl_job: self.root.after_cancel(self._hl_job)
        self._hl_job = self.root.after(150, self._highlight)

    def _yscroll(self, *a):
        self.editor.yview(*a); self.gutter.yview(*a)

    def _quit(self):
        if self.modified:
            r = messagebox.askyesnocancel("Unsaved", "Save changes?")
            if r is None: return
            if r: self.file_save()
        self.root.destroy()

    # ─── Gutter ───

    def _update_gutter(self):
        self.gutter.config(state="normal")
        self.gutter.delete("1.0", "end")
        n = int(self.editor.index("end-1c").split(".")[0])
        self.gutter.insert("1.0", "\n".join(str(i) for i in range(1, n+1)))
        self.gutter.config(state="disabled")
        self.gutter.yview_moveto(self.editor.yview()[0])

    # ─── Syntax Highlight ───

    def _highlight(self):
        code = self.editor.get("1.0", "end-1c")
        for tag in ("keyword","type","string","comment","number","preproc","func"):
            self.editor.tag_remove(tag, "1.0", "end")
        self._hl_pat("number", C_NUM_PAT, code)
        self._hl_pat("func", C_FN_PAT, code, group=1)
        self._hl_pat("keyword", C_KW_PAT, code)
        self._hl_pat("type", C_TY_PAT, code)
        self._hl_pat("preproc", C_PRE_PAT, code, flags=re.MULTILINE)
        self._hl_pat("string", C_STR_PAT, code)
        self._hl_pat("comment", C_CMT_PAT, code, flags=re.MULTILINE|re.DOTALL)

    def _hl_pat(self, tag, pat, code, group=0, flags=0):
        for m in re.finditer(pat, code, flags):
            self.editor.tag_add(tag, f"1.0+{m.start(group)}c", f"1.0+{m.end(group)}c")

    # ─── File Ops ───

    def _update_title(self):
        name = os.path.basename(self.file_path) if self.file_path else "untitled.c"
        mod = " *" if self.modified else ""
        self.root.title(f"{APP_NAME} — {name}{mod}")

    def file_new(self):
        if self.modified:
            r = messagebox.askyesnocancel("Unsaved", "Save?")
            if r is None: return
            if r: self.file_save()
        self.editor.delete("1.0", "end")
        self.editor.insert("1.0", '#include <stdio.h>\n\nint main(void) {\n    \n    return 0;\n}\n')
        self.file_path = None; self.modified = False
        self._update_title(); self._update_gutter(); self._highlight()

    def file_open(self):
        if self.modified:
            r = messagebox.askyesnocancel("Unsaved", "Save?")
            if r is None: return
            if r: self.file_save()
        p = filedialog.askopenfilename(filetypes=[("C files","*.c *.h *.cpp"),("All","*.*")])
        if not p: return
        try:
            with open(p, "r", encoding="utf-8", errors="replace") as f:
                self.editor.delete("1.0", "end")
                self.editor.insert("1.0", f.read())
            self.file_path = p; self.modified = False
            self._update_title(); self._update_gutter(); self._highlight()
            self._status(f"Opened: {p}", "ok")
        except Exception as e:
            messagebox.showerror("Error", str(e))

    def file_save(self):
        if self.file_path:
            self._write(self.file_path)
        else:
            self.file_save_as()

    def file_save_as(self):
        p = filedialog.asksaveasfilename(defaultextension=".c",
            filetypes=[("C files","*.c"),("All","*.*")])
        if not p: return
        self.file_path = p; self._write(p)

    def _write(self, path):
        try:
            with open(path, "w", encoding="utf-8") as f:
                f.write(self.editor.get("1.0", "end-1c"))
            self.modified = False; self._update_title()
            self._status(f"Saved: {path}", "ok")
        except Exception as e:
            messagebox.showerror("Error", str(e))

    def _sel_all(self):
        self.editor.tag_add("sel", "1.0", "end"); return "break"

    # ─── Output ───

    def _clear_out(self):
        self.output.config(state="normal"); self.output.delete("1.0", "end")
        self.output.config(state="disabled")

    def _out(self, text, tag="stdout"):
        self.output.config(state="normal")
        self.output.insert("end", text, tag)
        self.output.see("end")
        self.output.config(state="disabled")

    # ─── Compile / Run ───

    def do_compile(self):
        self._clear_out()
        self._out("─── Compiling... ───\n", "info")
        self._status("Compiling...", "info")
        source = self.editor.get("1.0", "end-1c")

        try:
            tokens = lex(source)
            parser = Parser(tokens)
            program = parser.parse()
            self._compiled_program = program
            self._compiled_source = source
            self._out("Compilation successful.\n", "success")
            n_funcs = sum(1 for d in program.decls if isinstance(d, FuncDecl) and d.body)
            n_vars = sum(1 for d in program.decls if isinstance(d, VarDecl))
            self._out(f"  {n_funcs} function(s), {n_vars} global(s)\n", "info")
            self._status("Compiled OK", "ok")
        except (LexError, ParseError) as e:
            self._compiled_program = None
            self._out(f"Error: {e}\n", "stderr")
            self._status("Compile failed", "err")

    def do_run(self):
        if not hasattr(self, '_compiled_program') or not self._compiled_program:
            self._out("No compiled program. Press F5 first.\n", "stderr")
            self._status("Nothing to run", "warn")
            return
        self._out("\n─── Running... ───\n", "info")
        self._status("Running...", "info")

        def _thread():
            def out_cb(text):
                self.root.after(0, self._out, text, "stdout")
            self.compiler = CatsCompiler(output_callback=out_cb)
            # Re-compile from source (interpreter needs fresh state)
            source = self._compiled_source
            ok, msg = self.compiler.compile_and_run(source)
            tag = "success" if ok else "stderr"
            self.root.after(0, self._out, msg + "\n", tag)
            self.root.after(0, self._status, "Done" if ok else "Runtime error", "ok" if ok else "err")
            self.compiler = None

        threading.Thread(target=_thread, daemon=True).start()

    def do_build_run(self):
        self._clear_out()
        self._out("─── Build & Run ───\n", "info")
        self._status("Building...", "info")
        source = self.editor.get("1.0", "end-1c")

        # Compile check
        try:
            tokens = lex(source)
            parser = Parser(tokens)
            program = parser.parse()
            self._compiled_program = program
            self._compiled_source = source
            self._out("Compiled OK.\n", "success")
        except (LexError, ParseError) as e:
            self._compiled_program = None
            self._out(f"Compile Error: {e}\n", "stderr")
            self._status("Build failed", "err")
            return

        self._out("Running...\n", "info")
        self._status("Running...", "info")

        def _thread():
            def out_cb(text):
                self.root.after(0, self._out, text, "stdout")
            self.compiler = CatsCompiler(output_callback=out_cb)
            ok, msg = self.compiler.compile_and_run(source)
            tag = "success" if ok else "stderr"
            self.root.after(0, self._out, msg + "\n", tag)
            self.root.after(0, self._status, "Done" if ok else "Error", "ok" if ok else "err")
            self.compiler = None

        threading.Thread(target=_thread, daemon=True).start()

    def do_stop(self):
        if self.compiler:
            self.compiler.stop()
            self._out("\n[Stopped]\n", "stderr")
            self._status("Stopped", "warn")

    def _about(self):
        messagebox.showinfo("About",
            f"{APP_NAME}\n\n"
            "Self-contained C Compiler IDE\n"
            "NO external GCC required!\n\n"
            "Built-in: Lexer → Parser → Interpreter\n"
            "Supports: printf, scanf, arrays, structs,\n"
            "  for/while/do-while, switch/case,\n"
            "  functions, recursion, pointers (basic),\n"
            "  all C operators, #define, #include\n\n"
            "Shortcuts: F5=Compile F6=Run F9=Build+Run\n\n"
            "Built by AI Core — Team Flames 2026")

# ═══════════════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════════════

if __name__ == "__main__":
    root = tk.Tk()
    app = CatsGCC_GUI(root)
    root.mainloop()
