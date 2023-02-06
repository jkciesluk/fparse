package fparse.example

case class Identifier(id: String)

enum Binop:
  case BINOP_And
  case BINOP_Or
  case BINOP_Add
  case BINOP_Sub
  case BINOP_Mult
  case BINOP_Div
  case BINOP_Rem

enum Relop:
  case RELOP_Eq, RELOP_Ne, RELOP_Lt, RELOP_Gt, RELOP_Le, RELOP_Ge


enum Unop:
  case UNOP_Not, UNOP_Neg


enum TypeExpression:
  case TEXPR_Id(id: Identifier)
  case TEXPR_Int
  case TEXPR_Bool
  case TEXPR_Array(tpe: TypeExpression)
  case TEXPR_Record(fields: List[FieldType])

case class FieldType(name: Identifier, tpe: TypeExpression)


enum Expression:
  case EXPR_Id(id: Identifier)
  case EXPR_Int(i: Int)
  case EXPR_Char(c: Char)
  case EXPR_String(str: String)
  case EXPR_Bool(b: Boolean)
  case EXPR_Length(e: Expression)
  case EXPR_Relation(op: Relop, lhs: Expression, rhs: Expression)
  case EXPR_Binop(op: Binop, lhs: Expression, rhs: Expression)
  case EXPR_Unop(op: Unop, expr: Expression)
  case EXPR_Call(callee: Identifier, arguments: List[Expression])
  case EXPR_Index(expr: Expression, index: Expression)
  case EXPR_Field(expr: Expression, field: Identifier)
  case EXPR_Unfold(expr: Expression)
  case EXPR_Array(exprs: List[Expression])
  case EXPR_EmptyStruct


case class FieldExpr(name: Identifier, value: Expression)



case class VarDecl(id: Identifier, tpe: TypeExpression)

enum LValue:
  case LVALUE_Id(id: Identifier)
  case LVALUE_Index(expr: Expression, index: Expression)
  case LVALUE_Field(expr: Expression, field: Identifier)

enum Statement:
  case STMT_Call(callee: Identifier, arguments: List[Expression])
  case STMT_Assign(lhs: LValue, rhs: Expression)
  case STMT_VarDecl(variable: VarDecl, init: Option[Expression])
  case STMT_ArrayDecl(
      id: Identifier,
      tp: TypeExpression,
      sizes: List[Expression],
  )
  case STMT_If(
      cond: Expression,
      then_branch: Statement,
      else_branch: Option[Statement],
  )
  case STMT_While(cond: Expression, body: Statement)
  case STMT_Return(expr: Option[Expression])
  case STMT_Block(stats: List[Statement])


enum GlobalDefinition: 
  case GDEF_Use(id: Identifier)
  case GDEF_Type(id: Identifier, body: TypeExpression)
  case GDEF_Function(
      id: Identifier,
      formal_parameters: List[VarDecl],
      return_type: Option[TypeExpression],
      body: Statement,
  )


enum GlobalDeclaration:
  case GDECL_Type(id: Identifier, body: TypeExpression)
  case GDECL_TypeDecl(id: Identifier)
  case GDECL_Function(
      id: Identifier,
      formal_parameters: List[VarDecl],
      return_type: Option[TypeExpression],
  )

case class ModuleDefinition(global_definitions: List[GlobalDefinition])

case class ModuleInterface(global_declarations: List[GlobalDefinition])