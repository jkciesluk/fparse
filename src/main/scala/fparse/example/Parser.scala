package fparse.example

import fparse.Parsers
import fparse.reader.InputReader

object Parser extends Parsers[List] {
  type Elem = Token

  private val parseIdent = 
    accept{case id: Ident => Identifier(id.id)}

  private val parseTypeExpr = 
    accept(Keyword.BOOL).map(_ => TypeExpression.TEXPR_Bool)
    accept(Keyword.INT).map(_ => TypeExpression.TEXPR_Int)
    parseIdent.map(TypeExpression.TEXPR_Id(_))
    accept(Keyword.BOOL).map(_ => TypeExpression.TEXPR_Bool)


}