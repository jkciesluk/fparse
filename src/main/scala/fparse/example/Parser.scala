package fparse.example

import fparse.Parsers
import fparse.reader.InputReader
import fparse.Scanners

// NOTE: This Parser doesn't work for now, because of left recursive parseExpr and parseLValueExpr
// Also, parseExpr doesn't support operators priorities
object Parser extends Scanners[Token, List] {

  private val parseIdent =
    accept { case id: Ident => Identifier(id.id) }

  private def parseFieldType = for {
    id <- parseIdent
    _ <- accept(Tk.COLON)
    tpe <- parseTypeExpr
  } yield FieldType(id, tpe)

  private def parseArrayTpe: Parser[TypeExpression.TEXPR_Array] = for {
    tpe <- parseTypeExpr
    _ <- accept(Tk.LSBRACKET)
    _ <- accept(Tk.RSBRACKET)
  } yield TypeExpression.TEXPR_Array(tpe)

  private def parseRecordTpe: Parser[TypeExpression.TEXPR_Record] = for {
    _ <- accept(Tk.LCBRACE)
    tpe <- parseFieldType.separatedOne(accept(Tk.COMMA)) <* accept(
      Tk.COMMA
    ).?
    _ <- accept(Tk.RCBRACE)
  } yield TypeExpression.TEXPR_Record(tpe)

  private def parseTypeExpr: Parser[TypeExpression] =
    oneOf(
      List(
        accept(Keyword.BOOL).map(_ => TypeExpression.TEXPR_Bool),
        accept(Keyword.INT).map(_ => TypeExpression.TEXPR_Int),
        parseIdent.map(TypeExpression.TEXPR_Id(_)),
        parseArrayTpe,
        parseRecordTpe
      )
    )

  private val parseBinop: Parser[Binop] =
    oneOf(
      accept(Tk.PLUS).map(_ => Binop.BINOP_Add),
      accept(Tk.MINUS).map(_ => Binop.BINOP_Sub),
      accept(Tk.DIV).map(_ => Binop.BINOP_Div),
      accept(Tk.TIMES).map(_ => Binop.BINOP_Mult),
      accept(Tk.REM).map(_ => Binop.BINOP_Rem),
      accept(Keyword.AND).map(_ => Binop.BINOP_Add),
      accept(Keyword.OR).map(_ => Binop.BINOP_Or)
    )

  private val parseRelop: Parser[Relop] =
    oneOf(
      accept(Tk.LE).map(_ => Relop.RELOP_Le),
      accept(Tk.GE).map(_ => Relop.RELOP_Ge),
      accept(Tk.EQ).map(_ => Relop.RELOP_Eq),
      accept(Tk.NEQ).map(_ => Relop.RELOP_Ne),
      accept(Tk.LT).map(_ => Relop.RELOP_Lt),
      accept(Tk.GT).map(_ => Relop.RELOP_Gt)
    )

  private val parseUnop: Parser[Unop] =
    oneOf(
      accept(Tk.MINUS).map(_ => Unop.UNOP_Neg),
      accept(Keyword.NOT).map(_ => Unop.UNOP_Not)
    )

  private def parseExpr: Parser[Expression] =
    oneOf(
      (accept(Keyword.LENGTH) >> accept(Tk.LPAREN) >> parseExpr <* accept(
        Tk.RPAREN
      )),
      (parseExpr ~ parseBinop ~ parseExpr).map { case ((e1, bop), e2) =>
        Expression.EXPR_Binop(bop, e1, e2)
      },
      (parseExpr ~ parseRelop ~ parseExpr).map { case ((e1, rop), e2) =>
        Expression.EXPR_Relation(rop, e1, e2)
      },
      (parseUnop ~ parseExpr).map { case (uop, e1) =>
        Expression.EXPR_Unop(uop, e1)
      },
      between(accept(Tk.LPAREN), parseExpr, accept(Tk.RPAREN)),
      parseCall.map(Expression.EXPR_Call(_, _)),
      parseExprIndex(parseExpr),
      parseExprField(parseExpr),
      (parseExpr <* accept(Tk.UNFOLD)).map(Expression.EXPR_Unfold(_)),
      parseIdent.map(Expression.EXPR_Id(_)),
      accept { case Number(n) => Expression.EXPR_Int(n) },
      accept { case Char(c) => Expression.EXPR_Char(c) },
      accept { case Quote(s) => Expression.EXPR_String(s) },
      accept(Keyword.FALSE).map(_ => Expression.EXPR_Bool(false)),
      accept(Keyword.TRUE).map(_ => Expression.EXPR_Bool(true))
    )
  private def parseFieldExpr: Parser[FieldExpr] = for {
    id <- parseIdent
    _ <- accept(Tk.ASSIGN)
    e1 <- parseExpr
  } yield FieldExpr(id, e1)

  private def parseCall: Parser[(Identifier, List[Expression])] = for {
    callee <- parseIdent
    _ <- accept(Tk.LPAREN)
    args <- (parseExpr.separatedOne(accept(Tk.COMMA)) <* accept(
      Tk.COMMA
    ).?).?
    _ <- accept(Tk.RPAREN)
  } yield (callee, args.getOrElse(List.empty))

  private def parseExprIndex(firstExpr: Parser[Expression]) = for {
    e1 <- firstExpr
    _ <- accept(Tk.LSBRACKET)
    idx <- parseExpr
    _ <- accept(Tk.RSBRACKET)
  } yield Expression.EXPR_Index(e1, idx)

  private def parseExprField(firstExpr: Parser[Expression]) = for {
    e1 <- firstExpr
    _ <- accept(Tk.DOT)
    id <- parseIdent
  } yield Expression.EXPR_Field(e1, id)

  private def parseLValueExpr: Parser[Expression] =
    oneOf(
      (accept(Keyword.LENGTH) >> accept(Tk.LPAREN) >> parseExpr <* accept(
        Tk.RPAREN
      )),
      parseCall.map(Expression.EXPR_Call(_, _)),
      parseExprIndex(parseLValueExpr),
      parseExprField(parseLValueExpr),
      (parseLValueExpr <* accept(Tk.UNFOLD)).map(Expression.EXPR_Unfold(_)),
      parseIdent.map(Expression.EXPR_Id(_)),
      accept { case Number(n) => Expression.EXPR_Int(n) },
      accept { case Char(c) => Expression.EXPR_Char(c) },
      accept { case Quote(s) => Expression.EXPR_String(s) },
      accept(Keyword.FALSE).map(_ => Expression.EXPR_Bool(false)),
      accept(Keyword.TRUE).map(_ => Expression.EXPR_Bool(true))
    )

  private val parseVarDecl: Parser[VarDecl] =
    ((parseIdent <* accept(Tk.COLON)) ~ parseTypeExpr).map((id, tpe) =>
      VarDecl(id, tpe)
    )

  private val parseLValue: Parser[LValue] =
    oneOf(
      parseIdent.map(LValue.LVALUE_Id(_)),
      ((parseLValueExpr <* accept(Tk.LSBRACKET)) ~ parseExpr).map((e1, idx) =>
        LValue.LVALUE_Index(e1, idx)
      ) <* accept(Tk.RSBRACKET),
      ((parseLValueExpr <* accept(Tk.DOT)) ~ parseIdent).map((e1, id) =>
        LValue.LVALUE_Field(e1, id)
      )
    )

  private val parseSize: Parser[Expression] =
    between(accept(Tk.LSBRACKET), parseExpr, accept(Tk.RSBRACKET))

  private val parseArrayDecl = for {
    id <- parseIdent
    _ <- accept(Tk.COLON)
    tpe <- parseTypeExpr
    sizes <- parseSize.repeatedOne
  } yield Statement.STMT_ArrayDecl(id, tpe, sizes)

  private def parseSimpleStatement: Parser[Statement] =
    (oneOf(
      parseCall.map(Statement.STMT_Call(_, _)),
      (parseLValue <* accept(Tk.ASSIGN))
        .andThen(parseExpr)
        .map(Statement.STMT_Assign(_, _)),
      (parseVarDecl <* accept(Tk.ASSIGN))
        .andThen(parseExpr)
        .map((lhs, rhs) => Statement.STMT_VarDecl(lhs, Some(rhs))),
      parseVarDecl.map(Statement.STMT_VarDecl(_, None)),
      parseArrayDecl,
      (accept(Keyword.RETURN) *> parseExpr).map(r =>
        Statement.STMT_Return(Some(r))
      ),
      between(accept(Tk.LCBRACE), parseStatement.repeated, accept(Tk.RCBRACE))
        .map(Statement.STMT_Block(_))
    ) <* accept(Tk.SEMICOLON).?) <|>
      (accept(Keyword.RETURN) >> accept(Tk.SEMICOLON).map(_ =>
        Statement.STMT_Return(None)
      ))

  private def parseFullIf(parseStmt: Parser[Statement]) = for {
    _ <- accept(Keyword.IF)
    eb <- parseExpr
    then_branch <- parseClosedStatement
    _ <- accept(Keyword.ELSE)
    else_branch <- parseStmt
  } yield Statement.STMT_If(eb, then_branch, Some(else_branch))

  private def parsePartialIf: Parser[Statement] = for {
    _ <- accept(Keyword.IF)
    eb <- parseExpr
    then_branch <- parseClosedStatement
  } yield Statement.STMT_If(eb, then_branch, None)

  private def parseWhile(parseStmt: Parser[Statement]): Parser[Statement] =
    for {
      _ <- accept(Keyword.WHILE)
      eb <- parseExpr
      body <- parseStmt
    } yield Statement.STMT_While(eb, body)

  private def parseClosedStatement: Parser[Statement] =
    parseFullIf(parseClosedStatement) <|> parseWhile(
      parseClosedStatement
    ) <|> parseSimpleStatement

  private def parseOpenStatement: Parser[Statement] =
    parseFullIf(parseOpenStatement) <|> parsePartialIf <|> parseWhile(
      parseOpenStatement
    )

  private def parseStatement =
    parseClosedStatement <|> parseOpenStatement

  private val parseFunctioDefinition = for {
    id <- parseIdent
    _ <- accept(Tk.LPAREN)
    params <- parseVarDecl.separated(accept(Tk.COMMA))
    _ <- accept(Tk.RPAREN)
    tpe <- (accept(Tk.COLON) *> parseTypeExpr).?
    body <- parseStatement
  } yield GlobalDefinition.GDEF_Function(id, params, tpe, body)

  private val parseGlobalDefinition =
    oneOf(
      parseFunctioDefinition,
      (accept(Keyword.USE) *> parseIdent).map(GlobalDefinition.GDEF_Use(_)),
      ((accept(Keyword.TYPE) *> parseIdent <* accept(
        Tk.ASSIGN
      )) ~ parseTypeExpr).map((id, tpe) => GlobalDefinition.GDEF_Type(id, tpe))
    )

  private val parseFunctioDeclaration = for {
    id <- parseIdent
    _ <- accept(Tk.LPAREN)
    params <- parseVarDecl.separated(accept(Tk.COMMA))
    _ <- accept(Tk.RPAREN)
    tpe <- (accept(Tk.COLON) *> parseTypeExpr).?
  } yield GlobalDeclaration.GDECL_Function(id, params, tpe)

  private val parseGlobalDeclaration =
    oneOf(
      parseFunctioDeclaration,
      ((accept(Keyword.TYPE) *> parseIdent <* accept(
        Tk.ASSIGN
      )) ~ parseTypeExpr).map((id, tpe) =>
        GlobalDeclaration.GDECL_Type(id, tpe)
      ),
      (accept(Keyword.TYPE) *> parseIdent)
        .map(GlobalDeclaration.GDECL_TypeDecl(_))
    )

  private val parseSourceFile0 =
    (parseGlobalDefinition.repeated <* accept(EOF)).map(ModuleDefinition(_))
  private val parseInterfaceFile0 =
    (parseGlobalDeclaration.repeated <* accept(EOF)).map(ModuleInterface(_))

  def tokenize(str: String): List[Token] =
    Lexer.getTokens(str)

  def parseSourceFile(str: String) = parseSourceFile0(makeInput(tokenize(str)))
  def parseInterfaceFile(str: String) = parseInterfaceFile0(
    makeInput(tokenize(str))
  )

}
