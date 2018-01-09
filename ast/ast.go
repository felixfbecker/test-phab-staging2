// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package ast declares the types used to represent syntax trees for Go
// packages.
//
package ast

import (
	"strings"
	"unicode"
	"unicode/utf8"

	"gitolite.sgdev.org/testing/token"
)

// ----------------------------------------------------------------------------
// Interfaces
//
// There are 3 main classes of nodes: Expressions and type nodes,
// statement nodes, and declaration nodes. The node names usually
// match the corresponding Go spec production names to which they
// correspond. The node fields correspond to the individual parts
// of the respective productions.
//
// All nodes contain position information marking the beginning of
// the corresponding source text segment; it is accessible via the
// Pos accessor method. Nodes may contain additional position info
// for language constructs where comments may be found between parts
// of the construct (typically any larger, parenthesized subpart).
// That position information is needed to properly position comments
// when printing the construct.

// All node types implement the Node interface.
type Node interface {
	Pos() token1.Pos // position of first character belonging to the node
	End() token1.Pos // position of first character immediately after the node
}

// All expression nodes implement the Expr interface.
type Expr interface {
	Node
	exprNode()
}

// All statement nodes implement the Stmt interface.
type Stmt interface {
	Node
	stmtNode()
}

// All declaration nodes implement the Decl interface.
type Decl interface {
	Node
	declNode()
}

// ----------------------------------------------------------------------------
// Comments

// A Comment node represents a single //-style or /*-style comment.
type Comment struct {
	Slash token1.Pos // position of "/" starting the comment
	Text  string     // comment text (excluding '\n' for //-style comments)
}

func (c *Comment) Pos() token1.Pos { return c.Slash }
func (c *Comment) End() token1.Pos { return token1.Pos(int(c.Slash) + len(c.Text)) }

// A CommentGroup represents a sequence of comments
// with no other token1s and no empty lines between.
//
type CommentGroup struct {
	List []*Comment // len(List) > 0
}

func (g *CommentGroup) Pos() token1.Pos { return g.List[0].Pos() }
func (g *CommentGroup) End() token1.Pos { return g.List[len(g.List)-1].End() }

func isWhitespace(ch byte) bool { return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' }

func stripTrailingWhitespace(s string) string {
	i := len(s)
	for i > 0 && isWhitespace(s[i-1]) {
		i--
	}
	return s[0:i]
}

// Text returns the text of the comment.
// Comment markers (//, /*, and */), the first space of a line comment, and
// leading and trailing empty lines are removed. Multiple empty lines are
// reduced to one, and trailing space on lines is trimmed. Unless the result
// is empty, it is newline-terminated.
//
func (g *CommentGroup) Text() string {
	if g == nil {
		return ""
	}
	comments := make([]string, len(g.List))
	for i, c := range g.List {
		comments[i] = c.Text
	}

	lines := make([]string, 0, 10) // most comments are less than 10 lines
	for _, c := range comments {
		// Remove comment markers.
		// The parser has given us exactly the comment text.
		switch c[1] {
		case '/':
			//-style comment (no newline at the end)
			c = c[2:]
			// strip first space - required for Example tests
			if len(c) > 0 && c[0] == ' ' {
				c = c[1:]
			}
		case '*':
			/*-style comment */
			c = c[2 : len(c)-2]
		}

		// Split on newlines.
		cl := strings.Split(c, "\n")

		// Walk lines, stripping trailing white space and adding to list.
		for _, l := range cl {
			lines = append(lines, stripTrailingWhitespace(l))
		}
	}

	// Remove leading blank lines; convert runs of
	// interior blank lines to a single blank line.
	n := 0
	for _, line := range lines {
		if line != "" || n > 0 && lines[n-1] != "" {
			lines[n] = line
			n++
		}
	}
	lines = lines[0:n]

	// Add final "" entry to get trailing newline from Join.
	if n > 0 && lines[n-1] != "" {
		lines = append(lines, "")
	}

	return strings.Join(lines, "\n")
}

// ----------------------------------------------------------------------------
// Expressions and types

// A Field represents a Field declaration list in a struct type,
// a method list in an interface type, or a parameter/result declaration
// in a signature.
//
type Field struct {
	Doc     *CommentGroup // associated documentation; or nil
	Names   []*Ident      // field/method/parameter names; or nil if anonymous field
	Type    Expr          // field/method/parameter type
	Tag     *BasicLit     // field tag; or nil
	Comment *CommentGroup // line comments; or nil
}

func (f *Field) Pos() token1.Pos {
	if len(f.Names) > 0 {
		return f.Names[0].Pos()
	}
	return f.Type.Pos()
}

func (f *Field) End() token1.Pos {
	if f.Tag != nil {
		return f.Tag.End()
	}
	return f.Type.End()
}

// A FieldList represents a list of Fields, enclosed by parentheses or braces.
type FieldList struct {
	Opening token1.Pos // position of opening parenthesis/brace, if any
	List    []*Field   // field list; or nil
	Closing token1.Pos // position of closing parenthesis/brace, if any
}

func (f *FieldList) Pos() token1.Pos {
	if f.Opening.IsValid() {
		return f.Opening
	}
	// the list should not be empty in this case;
	// be conservative and guard against bad ASTs
	if len(f.List) > 0 {
		return f.List[0].Pos()
	}
	return token1.NoPos
}

func (f *FieldList) End() token1.Pos {
	if f.Closing.IsValid() {
		return f.Closing + 1
	}
	// the list should not be empty in this case;
	// be conservative and guard against bad ASTs
	if n := len(f.List); n > 0 {
		return f.List[n-1].End()
	}
	return token1.NoPos
}

// NumFields returns the number of (named and anonymous fields) in a FieldList.
func (f *FieldList) NumFields() int {
	n := 0
	if f != nil {
		for _, g := range f.List {
			m := len(g.Names)
			if m == 0 {
				m = 1 // anonymous field
			}
			n += m
		}
	}
	return n
}

// An expression is represented by a tree consisting of one
// or more of the following concrete expression nodes.
//
type (
	// A BadExpr node is a placeholder for expressions containing
	// syntax errors for which no correct expression nodes can be
	// created.
	//
	BadExpr struct {
		From, To token1.Pos // position range of bad expression
	}

	// An Ident node represents an identifier.
	Ident struct {
		NamePos token1.Pos // identifier position
		Name    string     // identifier name
		Obj     *Object    // denoted object; or nil
	}

	// An Ellipsis node stands for the "..." type in a
	// parameter list or the "..." length in an array type.
	//
	Ellipsis struct {
		Ellipsis token1.Pos // position of "..."
		Elt      Expr       // ellipsis element type (parameter lists only); or nil
	}

	// A BasicLit node represents a literal of basic type.
	BasicLit struct {
		ValuePos token1.Pos   // literal position
		Kind     token1.Token // token1.INT, token1.FLOAT, token1.IMAG, token1.CHAR, or token1.STRING
		Value    string       // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
	}

	// A FuncLit node represents a function literal.
	FuncLit struct {
		Type *FuncType  // function type
		Body *BlockStmt // function body
	}

	// A CompositeLit node represents a composite literal.
	CompositeLit struct {
		Type   Expr       // literal type; or nil
		Lbrace token1.Pos // position of "{"
		Elts   []Expr     // list of composite elements; or nil
		Rbrace token1.Pos // position of "}"
	}

	// A ParenExpr node represents a parenthesized expression.
	ParenExpr struct {
		Lparen token1.Pos // position of "("
		X      Expr       // parenthesized expression
		Rparen token1.Pos // position of ")"
	}

	// A SelectorExpr node represents an expression followed by a selector.
	SelectorExpr struct {
		X   Expr   // expression
		Sel *Ident // field selector
	}

	// An IndexExpr node represents an expression followed by an index.
	IndexExpr struct {
		X      Expr       // expression
		Lbrack token1.Pos // position of "["
		Index  Expr       // index expression
		Rbrack token1.Pos // position of "]"
	}

	// An SliceExpr node represents an expression followed by slice indices.
	SliceExpr struct {
		X      Expr       // expression
		Lbrack token1.Pos // position of "["
		Low    Expr       // begin of slice range; or nil
		High   Expr       // end of slice range; or nil
		Max    Expr       // maximum capacity of slice; or nil
		Slice3 bool       // true if 3-index slice (2 colons present)
		Rbrack token1.Pos // position of "]"
	}

	// A TypeAssertExpr node represents an expression followed by a
	// type assertion.
	//
	TypeAssertExpr struct {
		X      Expr       // expression
		Lparen token1.Pos // position of "("
		Type   Expr       // asserted type; nil means type switch X.(type)
		Rparen token1.Pos // position of ")"
	}

	// A CallExpr node represents an expression followed by an argument list.
	CallExpr struct {
		Fun      Expr       // function expression
		Lparen   token1.Pos // position of "("
		Args     []Expr     // function arguments; or nil
		Ellipsis token1.Pos // position of "..." (token1.NoPos if there is no "...")
		Rparen   token1.Pos // position of ")"
	}

	// A StarExpr node represents an expression of the form "*" Expression.
	// Semantically it could be a unary "*" expression, or a pointer type.
	//
	StarExpr struct {
		Star token1.Pos // position of "*"
		X    Expr       // operand
	}

	// A UnaryExpr node represents a unary expression.
	// Unary "*" expressions are represented via StarExpr nodes.
	//
	UnaryExpr struct {
		OpPos token1.Pos   // position of Op
		Op    token1.Token // operator
		X     Expr         // operand
	}

	// A BinaryExpr node represents a binary expression.
	BinaryExpr struct {
		X     Expr         // left operand
		OpPos token1.Pos   // position of Op
		Op    token1.Token // operator
		Y     Expr         // right operand
	}

	// A KeyValueExpr node represents (key : value) pairs
	// in composite literals.
	//
	KeyValueExpr struct {
		Key   Expr
		Colon token1.Pos // position of ":"
		Value Expr
	}
)

// The direction of a channel type is indicated by one
// of the following constants.
//
type ChanDir int

const (
	SEND ChanDir = 1 << iota
	RECV
)

// A type is represented by a tree consisting of one
// or more of the following type-specific expression
// nodes.
//
type (
	// An ArrayType node represents an array or slice type.
	ArrayType struct {
		Lbrack token1.Pos // position of "["
		Len    Expr       // Ellipsis node for [...]T array types, nil for slice types
		Elt    Expr       // element type
	}

	// A StructType node represents a struct type.
	StructType struct {
		Struct     token1.Pos // position of "struct" keyword
		Fields     *FieldList // list of field declarations
		Incomplete bool       // true if (source) fields are missing in the Fields list
	}

	// Pointer types are represented via StarExpr nodes.

	// A FuncType node represents a function type.
	FuncType struct {
		Func    token1.Pos // position of "func" keyword (token1.NoPos if there is no "func")
		Params  *FieldList // (incoming) parameters; non-nil
		Results *FieldList // (outgoing) results; or nil
	}

	// An InterfaceType node represents an interface type.
	InterfaceType struct {
		Interface  token1.Pos // position of "interface" keyword
		Methods    *FieldList // list of methods
		Incomplete bool       // true if (source) methods are missing in the Methods list
	}

	// A MapType node represents a map type.
	MapType struct {
		Map   token1.Pos // position of "map" keyword
		Key   Expr
		Value Expr
	}

	// A ChanType node represents a channel type.
	ChanType struct {
		Begin token1.Pos // position of "chan" keyword or "<-" (whichever comes first)
		Arrow token1.Pos // position of "<-" (token1.NoPos if there is no "<-")
		Dir   ChanDir    // channel direction
		Value Expr       // value type
	}
)

// Pos and End implementations for expression/type nodes.

func (x *BadExpr) Pos() token1.Pos  { return x.From }
func (x *Ident) Pos() token1.Pos    { return x.NamePos }
func (x *Ellipsis) Pos() token1.Pos { return x.Ellipsis }
func (x *BasicLit) Pos() token1.Pos { return x.ValuePos }
func (x *FuncLit) Pos() token1.Pos  { return x.Type.Pos() }
func (x *CompositeLit) Pos() token1.Pos {
	if x.Type != nil {
		return x.Type.Pos()
	}
	return x.Lbrace
}
func (x *ParenExpr) Pos() token1.Pos      { return x.Lparen }
func (x *SelectorExpr) Pos() token1.Pos   { return x.X.Pos() }
func (x *IndexExpr) Pos() token1.Pos      { return x.X.Pos() }
func (x *SliceExpr) Pos() token1.Pos      { return x.X.Pos() }
func (x *TypeAssertExpr) Pos() token1.Pos { return x.X.Pos() }
func (x *CallExpr) Pos() token1.Pos       { return x.Fun.Pos() }
func (x *StarExpr) Pos() token1.Pos       { return x.Star }
func (x *UnaryExpr) Pos() token1.Pos      { return x.OpPos }
func (x *BinaryExpr) Pos() token1.Pos     { return x.X.Pos() }
func (x *KeyValueExpr) Pos() token1.Pos   { return x.Key.Pos() }
func (x *ArrayType) Pos() token1.Pos      { return x.Lbrack }
func (x *StructType) Pos() token1.Pos     { return x.Struct }
func (x *FuncType) Pos() token1.Pos {
	if x.Func.IsValid() || x.Params == nil { // see issue 3870
		return x.Func
	}
	return x.Params.Pos() // interface method declarations have no "func" keyword
}
func (x *InterfaceType) Pos() token1.Pos { return x.Interface }
func (x *MapType) Pos() token1.Pos       { return x.Map }
func (x *ChanType) Pos() token1.Pos      { return x.Begin }

func (x *BadExpr) End() token1.Pos { return x.To }
func (x *Ident) End() token1.Pos   { return token1.Pos(int(x.NamePos) + len(x.Name)) }
func (x *Ellipsis) End() token1.Pos {
	if x.Elt != nil {
		return x.Elt.End()
	}
	return x.Ellipsis + 3 // len("...")
}
func (x *BasicLit) End() token1.Pos       { return token1.Pos(int(x.ValuePos) + len(x.Value)) }
func (x *FuncLit) End() token1.Pos        { return x.Body.End() }
func (x *CompositeLit) End() token1.Pos   { return x.Rbrace + 1 }
func (x *ParenExpr) End() token1.Pos      { return x.Rparen + 1 }
func (x *SelectorExpr) End() token1.Pos   { return x.Sel.End() }
func (x *IndexExpr) End() token1.Pos      { return x.Rbrack + 1 }
func (x *SliceExpr) End() token1.Pos      { return x.Rbrack + 1 }
func (x *TypeAssertExpr) End() token1.Pos { return x.Rparen + 1 }
func (x *CallExpr) End() token1.Pos       { return x.Rparen + 1 }
func (x *StarExpr) End() token1.Pos       { return x.X.End() }
func (x *UnaryExpr) End() token1.Pos      { return x.X.End() }
func (x *BinaryExpr) End() token1.Pos     { return x.Y.End() }
func (x *KeyValueExpr) End() token1.Pos   { return x.Value.End() }
func (x *ArrayType) End() token1.Pos      { return x.Elt.End() }
func (x *StructType) End() token1.Pos     { return x.Fields.End() }
func (x *FuncType) End() token1.Pos {
	if x.Results != nil {
		return x.Results.End()
	}
	return x.Params.End()
}
func (x *InterfaceType) End() token1.Pos { return x.Methods.End() }
func (x *MapType) End() token1.Pos       { return x.Value.End() }
func (x *ChanType) End() token1.Pos      { return x.Value.End() }

// exprNode() ensures that only expression/type nodes can be
// assigned to an Expr.
//
func (*BadExpr) exprNode()        {}
func (*Ident) exprNode()          {}
func (*Ellipsis) exprNode()       {}
func (*BasicLit) exprNode()       {}
func (*FuncLit) exprNode()        {}
func (*CompositeLit) exprNode()   {}
func (*ParenExpr) exprNode()      {}
func (*SelectorExpr) exprNode()   {}
func (*IndexExpr) exprNode()      {}
func (*SliceExpr) exprNode()      {}
func (*TypeAssertExpr) exprNode() {}
func (*CallExpr) exprNode()       {}
func (*StarExpr) exprNode()       {}
func (*UnaryExpr) exprNode()      {}
func (*BinaryExpr) exprNode()     {}
func (*KeyValueExpr) exprNode()   {}

func (*ArrayType) exprNode()     {}
func (*StructType) exprNode()    {}
func (*FuncType) exprNode()      {}
func (*InterfaceType) exprNode() {}
func (*MapType) exprNode()       {}
func (*ChanType) exprNode()      {}

// ----------------------------------------------------------------------------
// Convenience functions for Idents

// NewIdent creates a new Ident without position.
// Useful for ASTs generated by code other than the Go parser.
//
func NewIdent(name string) *Ident { return &Ident{token1.NoPos, name, nil} }

// IsExported reports whether name is an exported Go symbol
// (that is, whether it begins with an upper-case letter).
//
func IsExported(name string) bool {
	ch, _ := utf8.DecodeRuneInString(name)
	return unicode.IsUpper(ch)
}

// IsExported reports whether id is an exported Go symbol
// (that is, whether it begins with an uppercase letter).
//
func (id *Ident) IsExported() bool { return IsExported(id.Name) }

func (id *Ident) String() string {
	if id != nil {
		return id.Name
	}
	return "<nil>"
}

// ----------------------------------------------------------------------------
// Statements

// A statement is represented by a tree consisting of one
// or more of the following concrete statement nodes.
//
type (
	// A BadStmt node is a placeholder for statements containing
	// syntax errors for which no correct statement nodes can be
	// created.
	//
	BadStmt struct {
		From, To token1.Pos // position range of bad statement
	}

	// A DeclStmt node represents a declaration in a statement list.
	DeclStmt struct {
		Decl Decl // *GenDecl with CONST, TYPE, or VAR token1
	}

	// An EmptyStmt node represents an empty statement.
	// The "position" of the empty statement is the position
	// of the immediately following (explicit or implicit) semicolon.
	//
	EmptyStmt struct {
		Semicolon token1.Pos // position of following ";"
		Implicit  bool       // if set, ";" was omitted in the source
	}

	// A LabeledStmt node represents a labeled statement.
	LabeledStmt struct {
		Label *Ident
		Colon token1.Pos // position of ":"
		Stmt  Stmt
	}

	// An ExprStmt node represents a (stand-alone) expression
	// in a statement list.
	//
	ExprStmt struct {
		X Expr // expression
	}

	// A SendStmt node represents a send statement.
	SendStmt struct {
		Chan  Expr
		Arrow token1.Pos // position of "<-"
		Value Expr
	}

	// An IncDecStmt node represents an increment or decrement statement.
	IncDecStmt struct {
		X      Expr
		TokPos token1.Pos   // position of Tok
		Tok    token1.Token // INC or DEC
	}

	// An AssignStmt node represents an assignment or
	// a short variable declaration.
	//
	AssignStmt struct {
		Lhs    []Expr
		TokPos token1.Pos   // position of Tok
		Tok    token1.Token // assignment token1, DEFINE
		Rhs    []Expr
	}

	// A GoStmt node represents a go statement.
	GoStmt struct {
		Go   token1.Pos // position of "go" keyword
		Call *CallExpr
	}

	// A DeferStmt node represents a defer statement.
	DeferStmt struct {
		Defer token1.Pos // position of "defer" keyword
		Call  *CallExpr
	}

	// A ReturnStmt node represents a return statement.
	ReturnStmt struct {
		Return  token1.Pos // position of "return" keyword
		Results []Expr     // result expressions; or nil
	}

	// A BranchStmt node represents a break, continue, goto,
	// or fallthrough statement.
	//
	BranchStmt struct {
		TokPos token1.Pos   // position of Tok
		Tok    token1.Token // keyword token1 (BREAK, CONTINUE, GOTO, FALLTHROUGH)
		Label  *Ident       // label name; or nil
	}

	// A BlockStmt node represents a braced statement list.
	BlockStmt struct {
		Lbrace token1.Pos // position of "{"
		List   []Stmt
		Rbrace token1.Pos // position of "}"
	}

	// An IfStmt node represents an if statement.
	IfStmt struct {
		If   token1.Pos // position of "if" keyword
		Init Stmt       // initialization statement; or nil
		Cond Expr       // condition
		Body *BlockStmt
		Else Stmt // else branch; or nil
	}

	// A CaseClause represents a case of an expression or type switch statement.
	CaseClause struct {
		Case  token1.Pos // position of "case" or "default" keyword
		List  []Expr     // list of expressions or types; nil means default case
		Colon token1.Pos // position of ":"
		Body  []Stmt     // statement list; or nil
	}

	// A SwitchStmt node represents an expression switch statement.
	SwitchStmt struct {
		Switch token1.Pos // position of "switch" keyword
		Init   Stmt       // initialization statement; or nil
		Tag    Expr       // tag expression; or nil
		Body   *BlockStmt // CaseClauses only
	}

	// An TypeSwitchStmt node represents a type switch statement.
	TypeSwitchStmt struct {
		Switch token1.Pos // position of "switch" keyword
		Init   Stmt       // initialization statement; or nil
		Assign Stmt       // x := y.(type) or y.(type)
		Body   *BlockStmt // CaseClauses only
	}

	// A CommClause node represents a case of a select statement.
	CommClause struct {
		Case  token1.Pos // position of "case" or "default" keyword
		Comm  Stmt       // send or receive statement; nil means default case
		Colon token1.Pos // position of ":"
		Body  []Stmt     // statement list; or nil
	}

	// An SelectStmt node represents a select statement.
	SelectStmt struct {
		Select token1.Pos // position of "select" keyword
		Body   *BlockStmt // CommClauses only
	}

	// A ForStmt represents a for statement.
	ForStmt struct {
		For  token1.Pos // position of "for" keyword
		Init Stmt       // initialization statement; or nil
		Cond Expr       // condition; or nil
		Post Stmt       // post iteration statement; or nil
		Body *BlockStmt
	}

	// A RangeStmt represents a for statement with a range clause.
	RangeStmt struct {
		For        token1.Pos   // position of "for" keyword
		Key, Value Expr         // Key, Value may be nil
		TokPos     token1.Pos   // position of Tok; invalid if Key == nil
		Tok        token1.Token // ILLEGAL if Key == nil, ASSIGN, DEFINE
		X          Expr         // value to range over
		Body       *BlockStmt
	}
)

// Pos and End implementations for statement nodes.

func (s *BadStmt) Pos() token1.Pos        { return s.From }
func (s *DeclStmt) Pos() token1.Pos       { return s.Decl.Pos() }
func (s *EmptyStmt) Pos() token1.Pos      { return s.Semicolon }
func (s *LabeledStmt) Pos() token1.Pos    { return s.Label.Pos() }
func (s *ExprStmt) Pos() token1.Pos       { return s.X.Pos() }
func (s *SendStmt) Pos() token1.Pos       { return s.Chan.Pos() }
func (s *IncDecStmt) Pos() token1.Pos     { return s.X.Pos() }
func (s *AssignStmt) Pos() token1.Pos     { return s.Lhs[0].Pos() }
func (s *GoStmt) Pos() token1.Pos         { return s.Go }
func (s *DeferStmt) Pos() token1.Pos      { return s.Defer }
func (s *ReturnStmt) Pos() token1.Pos     { return s.Return }
func (s *BranchStmt) Pos() token1.Pos     { return s.TokPos }
func (s *BlockStmt) Pos() token1.Pos      { return s.Lbrace }
func (s *IfStmt) Pos() token1.Pos         { return s.If }
func (s *CaseClause) Pos() token1.Pos     { return s.Case }
func (s *SwitchStmt) Pos() token1.Pos     { return s.Switch }
func (s *TypeSwitchStmt) Pos() token1.Pos { return s.Switch }
func (s *CommClause) Pos() token1.Pos     { return s.Case }
func (s *SelectStmt) Pos() token1.Pos     { return s.Select }
func (s *ForStmt) Pos() token1.Pos        { return s.For }
func (s *RangeStmt) Pos() token1.Pos      { return s.For }

func (s *BadStmt) End() token1.Pos  { return s.To }
func (s *DeclStmt) End() token1.Pos { return s.Decl.End() }
func (s *EmptyStmt) End() token1.Pos {
	if s.Implicit {
		return s.Semicolon
	}
	return s.Semicolon + 1 /* len(";") */
}
func (s *LabeledStmt) End() token1.Pos { return s.Stmt.End() }
func (s *ExprStmt) End() token1.Pos    { return s.X.End() }
func (s *SendStmt) End() token1.Pos    { return s.Value.End() }
func (s *IncDecStmt) End() token1.Pos {
	return s.TokPos + 2 /* len("++") */
}
func (s *AssignStmt) End() token1.Pos { return s.Rhs[len(s.Rhs)-1].End() }
func (s *GoStmt) End() token1.Pos     { return s.Call.End() }
func (s *DeferStmt) End() token1.Pos  { return s.Call.End() }
func (s *ReturnStmt) End() token1.Pos {
	if n := len(s.Results); n > 0 {
		return s.Results[n-1].End()
	}
	return s.Return + 6 // len("return")
}
func (s *BranchStmt) End() token1.Pos {
	if s.Label != nil {
		return s.Label.End()
	}
	return token1.Pos(int(s.TokPos) + len(s.Tok.String()))
}
func (s *BlockStmt) End() token1.Pos { return s.Rbrace + 1 }
func (s *IfStmt) End() token1.Pos {
	if s.Else != nil {
		return s.Else.End()
	}
	return s.Body.End()
}
func (s *CaseClause) End() token1.Pos {
	if n := len(s.Body); n > 0 {
		return s.Body[n-1].End()
	}
	return s.Colon + 1
}
func (s *SwitchStmt) End() token1.Pos     { return s.Body.End() }
func (s *TypeSwitchStmt) End() token1.Pos { return s.Body.End() }
func (s *CommClause) End() token1.Pos {
	if n := len(s.Body); n > 0 {
		return s.Body[n-1].End()
	}
	return s.Colon + 1
}
func (s *SelectStmt) End() token1.Pos { return s.Body.End() }
func (s *ForStmt) End() token1.Pos    { return s.Body.End() }
func (s *RangeStmt) End() token1.Pos  { return s.Body.End() }

// stmtNode() ensures that only statement nodes can be
// assigned to a Stmt.
//
func (*BadStmt) stmtNode()        {}
func (*DeclStmt) stmtNode()       {}
func (*EmptyStmt) stmtNode()      {}
func (*LabeledStmt) stmtNode()    {}
func (*ExprStmt) stmtNode()       {}
func (*SendStmt) stmtNode()       {}
func (*IncDecStmt) stmtNode()     {}
func (*AssignStmt) stmtNode()     {}
func (*GoStmt) stmtNode()         {}
func (*DeferStmt) stmtNode()      {}
func (*ReturnStmt) stmtNode()     {}
func (*BranchStmt) stmtNode()     {}
func (*BlockStmt) stmtNode()      {}
func (*IfStmt) stmtNode()         {}
func (*CaseClause) stmtNode()     {}
func (*SwitchStmt) stmtNode()     {}
func (*TypeSwitchStmt) stmtNode() {}
func (*CommClause) stmtNode()     {}
func (*SelectStmt) stmtNode()     {}
func (*ForStmt) stmtNode()        {}
func (*RangeStmt) stmtNode()      {}

// ----------------------------------------------------------------------------
// Declarations

// A Spec node represents a single (non-parenthesized) import,
// constant, type, or variable declaration.
//
type (
	// The Spec type stands for any of *ImportSpec, *ValueSpec, and *TypeSpec.
	Spec interface {
		Node
		specNode()
	}

	// An ImportSpec node represents a single package import.
	ImportSpec struct {
		Doc     *CommentGroup // associated documentation; or nil
		Name    *Ident        // local package name (including "."); or nil
		Path    *BasicLit     // import path
		Comment *CommentGroup // line comments; or nil
		EndPos  token1.Pos    // end of spec (overrides Path.Pos if nonzero)
	}

	// A ValueSpec node represents a constant or variable declaration
	// (ConstSpec or VarSpec production).
	//
	ValueSpec struct {
		Doc     *CommentGroup // associated documentation; or nil
		Names   []*Ident      // value names (len(Names) > 0)
		Type    Expr          // value type; or nil
		Values  []Expr        // initial values; or nil
		Comment *CommentGroup // line comments; or nil
	}

	// A TypeSpec node represents a type declaration (TypeSpec production).
	TypeSpec struct {
		Doc     *CommentGroup // associated documentation; or nil
		Name    *Ident        // type name
		Assign  token1.Pos    // position of '=', if any
		Type    Expr          // *Ident, *ParenExpr, *SelectorExpr, *StarExpr, or any of the *XxxTypes
		Comment *CommentGroup // line comments; or nil
	}
)

// Pos and End implementations for spec nodes.

func (s *ImportSpec) Pos() token1.Pos {
	if s.Name != nil {
		return s.Name.Pos()
	}
	return s.Path.Pos()
}
func (s *ValueSpec) Pos() token1.Pos { return s.Names[0].Pos() }
func (s *TypeSpec) Pos() token1.Pos  { return s.Name.Pos() }

func (s *ImportSpec) End() token1.Pos {
	if s.EndPos != 0 {
		return s.EndPos
	}
	return s.Path.End()
}

func (s *ValueSpec) End() token1.Pos {
	if n := len(s.Values); n > 0 {
		return s.Values[n-1].End()
	}
	if s.Type != nil {
		return s.Type.End()
	}
	return s.Names[len(s.Names)-1].End()
}
func (s *TypeSpec) End() token1.Pos { return s.Type.End() }

// specNode() ensures that only spec nodes can be
// assigned to a Spec.
//
func (*ImportSpec) specNode() {}
func (*ValueSpec) specNode()  {}
func (*TypeSpec) specNode()   {}

// A declaration is represented by one of the following declaration nodes.
//
type (
	// A BadDecl node is a placeholder for declarations containing
	// syntax errors for which no correct declaration nodes can be
	// created.
	//
	BadDecl struct {
		From, To token1.Pos // position range of bad declaration
	}

	// A GenDecl node (generic declaration node) represents an import,
	// constant, type or variable declaration. A valid Lparen position
	// (Lparen.IsValid()) indicates a parenthesized declaration.
	//
	// Relationship between Tok value and Specs element type:
	//
	//	token1.IMPORT  *ImportSpec
	//	token1.CONST   *ValueSpec
	//	token1.TYPE    *TypeSpec
	//	token1.VAR     *ValueSpec
	//
	GenDecl struct {
		Doc    *CommentGroup // associated documentation; or nil
		TokPos token1.Pos    // position of Tok
		Tok    token1.Token  // IMPORT, CONST, TYPE, VAR
		Lparen token1.Pos    // position of '(', if any
		Specs  []Spec
		Rparen token1.Pos // position of ')', if any
	}

	// A FuncDecl node represents a function declaration.
	FuncDecl struct {
		Doc  *CommentGroup // associated documentation; or nil
		Recv *FieldList    // receiver (methods); or nil (functions)
		Name *Ident        // function/method name
		Type *FuncType     // function signature: parameters, results, and position of "func" keyword
		Body *BlockStmt    // function body; or nil for external (non-Go) function
	}
)

// Pos and End implementations for declaration nodes.

func (d *BadDecl) Pos() token1.Pos  { return d.From }
func (d *GenDecl) Pos() token1.Pos  { return d.TokPos }
func (d *FuncDecl) Pos() token1.Pos { return d.Type.Pos() }

func (d *BadDecl) End() token1.Pos { return d.To }
func (d *GenDecl) End() token1.Pos {
	if d.Rparen.IsValid() {
		return d.Rparen + 1
	}
	return d.Specs[0].End()
}
func (d *FuncDecl) End() token1.Pos {
	if d.Body != nil {
		return d.Body.End()
	}
	return d.Type.End()
}

// declNode() ensures that only declaration nodes can be
// assigned to a Decl.
//
func (*BadDecl) declNode()  {}
func (*GenDecl) declNode()  {}
func (*FuncDecl) declNode() {}

// ----------------------------------------------------------------------------
// Files and packages

// A File node represents a Go source file.
//
// The Comments list contains all comments in the source file in order of
// appearance, including the comments that are pointed to from other nodes
// via Doc and Comment fields.
//
// For correct printing of source code containing comments (using packages
// go/format and go/printer), special care must be taken to update comments
// when a File's syntax tree is modified: For printing, comments are interspersed
// between token1s based on their position. If syntax tree nodes are
// removed or moved, relevant comments in their vicinity must also be removed
// (from the File.Comments list) or moved accordingly (by updating their
// positions). A CommentMap may be used to facilitate some of these operations.
//
// Whether and how a comment is associated with a node depends on the
// interpretation of the syntax tree by the manipulating program: Except for Doc
// and Comment comments directly associated with nodes, the remaining comments
// are "free-floating" (see also issues #18593, #20744).
//
type File struct {
	Doc        *CommentGroup   // associated documentation; or nil
	Package    token1.Pos      // position of "package" keyword
	Name       *Ident          // package name
	Decls      []Decl          // top-level declarations; or nil
	Scope      *Scope          // package scope (this file only)
	Imports    []*ImportSpec   // imports in this file
	Unresolved []*Ident        // unresolved identifiers in this file
	Comments   []*CommentGroup // list of all comments in the source file
}

func (f *File) Pos() token1.Pos { return f.Package }
func (f *File) End() token1.Pos {
	if n := len(f.Decls); n > 0 {
		return f.Decls[n-1].End()
	}
	return f.Name.End()
}

// A Package node represents a set of source files
// collectively building a Go package.
//
type Package struct {
	Name    string             // package name
	Scope   *Scope             // package scope across all files
	Imports map[string]*Object // map of package id -> package object
	Files   map[string]*File   // Go source files by filename
}

func (p *Package) Pos() token1.Pos { return token1.NoPos }
func (p *Package) End() token1.Pos { return token1.NoPos }
