//===--- ParseExprCXX.cpp - C++ Expression Parsing ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Expression parsing implementation for C++.
//
//===----------------------------------------------------------------------===//

#include "clang/Parse/ParseDiagnostic.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/DeclSpec.h"
using namespace clang;

/// ParseOptionalCXXScopeSpecifier - Parse global scope or
/// nested-name-specifier if present.  Returns true if a nested-name-specifier
/// was parsed from the token stream.  Note that this routine will not parse
/// ::new or ::delete, it will just leave them in the token stream.
///
///       '::'[opt] nested-name-specifier
///       '::'
///
///       nested-name-specifier:
///         type-name '::'
///         namespace-name '::'
///         nested-name-specifier identifier '::'
///         nested-name-specifier 'template'[opt] simple-template-id '::' [TODO]
///
bool Parser::ParseOptionalCXXScopeSpecifier(CXXScopeSpec &SS) {
  assert(getLang().CPlusPlus &&
         "Call sites of this function should be guarded by checking for C++");

  if (Tok.is(tok::annot_cxxscope)) {
    SS.setScopeRep(Tok.getAnnotationValue());
    SS.setRange(Tok.getAnnotationRange());
    ConsumeToken();
    return true;
  }

  bool HasScopeSpecifier = false;

  if (Tok.is(tok::coloncolon)) {
    // ::new and ::delete aren't nested-name-specifiers.
    tok::TokenKind NextKind = NextToken().getKind();
    if (NextKind == tok::kw_new || NextKind == tok::kw_delete)
      return false;
    
    // '::' - Global scope qualifier.
    SourceLocation CCLoc = ConsumeToken();
    SS.setBeginLoc(CCLoc);
    SS.setScopeRep(Actions.ActOnCXXGlobalScopeSpecifier(CurScope, CCLoc));
    SS.setEndLoc(CCLoc);
    HasScopeSpecifier = true;
  }

  while (true) {
    // nested-name-specifier:
    //   type-name '::'
    //   namespace-name '::'
    //   nested-name-specifier identifier '::'
    if (Tok.is(tok::identifier) && NextToken().is(tok::coloncolon)) {
      // We have an identifier followed by a '::'. Lookup this name
      // as the name in a nested-name-specifier.
      IdentifierInfo *II = Tok.getIdentifierInfo();
      SourceLocation IdLoc = ConsumeToken();
      assert(Tok.is(tok::coloncolon) && "NextToken() not working properly!");
      SourceLocation CCLoc = ConsumeToken();
      
      if (!HasScopeSpecifier) {
        SS.setBeginLoc(IdLoc);
        HasScopeSpecifier = true;
      }
      
      if (SS.isInvalid())
        continue;
      
      SS.setScopeRep(
        Actions.ActOnCXXNestedNameSpecifier(CurScope, SS, IdLoc, CCLoc, *II));
      SS.setEndLoc(CCLoc);
      continue;
    }

    // nested-name-specifier:
    //   type-name '::'
    //   nested-name-specifier 'template'[opt] simple-template-id '::'
    if ((Tok.is(tok::identifier) && NextToken().is(tok::less)) ||
        Tok.is(tok::kw_template)) {
      // Parse the optional 'template' keyword, then make sure we have
      // 'identifier <' after it.
      if (Tok.is(tok::kw_template)) {
        SourceLocation TemplateKWLoc = ConsumeToken();
        
        if (Tok.isNot(tok::identifier)) {
          Diag(Tok.getLocation(), 
               diag::err_id_after_template_in_nested_name_spec)
            << SourceRange(TemplateKWLoc);
          break;
        }

        if (NextToken().isNot(tok::less)) {
          Diag(NextToken().getLocation(),
               diag::err_less_after_template_name_in_nested_name_spec)
            << Tok.getIdentifierInfo()->getName()
            << SourceRange(TemplateKWLoc, Tok.getLocation());
          break;
        }

        TemplateTy Template 
          = Actions.ActOnDependentTemplateName(TemplateKWLoc,
                                               *Tok.getIdentifierInfo(),
                                               Tok.getLocation(),
                                               SS);
        AnnotateTemplateIdToken(Template, TNK_Dependent_template_name,
                                &SS, TemplateKWLoc, false);
        continue;
      }

      TemplateTy Template;
      TemplateNameKind TNK = Actions.isTemplateName(*Tok.getIdentifierInfo(),
                                                    CurScope, Template, &SS);
      if (TNK) {
        // We have found a template name, so annotate this this token
        // with a template-id annotation. We do not permit the
        // template-id to be translated into a type annotation,
        // because some clients (e.g., the parsing of class template
        // specializations) still want to see the original template-id
        // token.
        AnnotateTemplateIdToken(Template, TNK, &SS, SourceLocation(), false);
        continue;
      }
    }

    if (Tok.is(tok::annot_template_id) && NextToken().is(tok::coloncolon)) {
      // We have 
      //
      //   simple-template-id '::'
      //
      // So we need to check whether the simple-template-id is of the
      // right kind (it should name a type or be dependent), and then
      // convert it into a type within the nested-name-specifier.
      TemplateIdAnnotation *TemplateId 
        = static_cast<TemplateIdAnnotation *>(Tok.getAnnotationValue());

      if (TemplateId->Kind == TNK_Type_template || 
          TemplateId->Kind == TNK_Dependent_template_name) {
        AnnotateTemplateIdTokenAsType(&SS);
        SS.setScopeRep(0);

        assert(Tok.is(tok::annot_typename) && 
               "AnnotateTemplateIdTokenAsType isn't working");
        Token TypeToken = Tok;
        ConsumeToken();
        assert(Tok.is(tok::coloncolon) && "NextToken() not working properly!");
        SourceLocation CCLoc = ConsumeToken();
        
        if (!HasScopeSpecifier) {
          SS.setBeginLoc(TypeToken.getLocation());
          HasScopeSpecifier = true;
        }
        
        if (TypeToken.getAnnotationValue())
          SS.setScopeRep(
            Actions.ActOnCXXNestedNameSpecifier(CurScope, SS, 
                                                TypeToken.getAnnotationValue(),
                                                TypeToken.getAnnotationRange(),
                                                CCLoc));
        else
          SS.setScopeRep(0);
        SS.setEndLoc(CCLoc);
        continue;
      } else
        assert(false && "FIXME: Only type template names supported here");
    }

    // We don't have any tokens that form the beginning of a
    // nested-name-specifier, so we're done.
    break;
  }
    
  return HasScopeSpecifier;
}

/// ParseCXXIdExpression - Handle id-expression.
///
///       id-expression:
///         unqualified-id
///         qualified-id
///
///       unqualified-id:
///         identifier
///         operator-function-id
///         conversion-function-id                [TODO]
///         '~' class-name                        [TODO]
///         template-id                           [TODO]
///
///       qualified-id:
///         '::'[opt] nested-name-specifier 'template'[opt] unqualified-id
///         '::' identifier
///         '::' operator-function-id
///         '::' template-id                      [TODO]
///
///       nested-name-specifier:
///         type-name '::'
///         namespace-name '::'
///         nested-name-specifier identifier '::'
///         nested-name-specifier 'template'[opt] simple-template-id '::' [TODO]
///
/// NOTE: The standard specifies that, for qualified-id, the parser does not
/// expect:
///
///   '::' conversion-function-id
///   '::' '~' class-name
///
/// This may cause a slight inconsistency on diagnostics:
///
/// class C {};
/// namespace A {}
/// void f() {
///   :: A :: ~ C(); // Some Sema error about using destructor with a
///                  // namespace.
///   :: ~ C(); // Some Parser error like 'unexpected ~'.
/// }
///
/// We simplify the parser a bit and make it work like:
///
///       qualified-id:
///         '::'[opt] nested-name-specifier 'template'[opt] unqualified-id
///         '::' unqualified-id
///
/// That way Sema can handle and report similar errors for namespaces and the
/// global scope.
///
/// The isAddressOfOperand parameter indicates that this id-expression is a
/// direct operand of the address-of operator. This is, besides member contexts,
/// the only place where a qualified-id naming a non-static class member may
/// appear.
///
Parser::OwningExprResult Parser::ParseCXXIdExpression(bool isAddressOfOperand) {
  // qualified-id:
  //   '::'[opt] nested-name-specifier 'template'[opt] unqualified-id
  //   '::' unqualified-id
  //
  CXXScopeSpec SS;
  ParseOptionalCXXScopeSpecifier(SS);

  // unqualified-id:
  //   identifier
  //   operator-function-id
  //   conversion-function-id
  //   '~' class-name                        [TODO]
  //   template-id                           [TODO]
  //
  switch (Tok.getKind()) {
  default:
    return ExprError(Diag(Tok, diag::err_expected_unqualified_id));

  case tok::identifier: {
    // Consume the identifier so that we can see if it is followed by a '('.
    IdentifierInfo &II = *Tok.getIdentifierInfo();
    SourceLocation L = ConsumeToken();
    return Actions.ActOnIdentifierExpr(CurScope, L, II, Tok.is(tok::l_paren),
                                       &SS, isAddressOfOperand);
  }

  case tok::kw_operator: {
    SourceLocation OperatorLoc = Tok.getLocation();
    if (OverloadedOperatorKind Op = TryParseOperatorFunctionId())
      return Actions.ActOnCXXOperatorFunctionIdExpr(
                       CurScope, OperatorLoc, Op, Tok.is(tok::l_paren), SS,
                       isAddressOfOperand);
    if (TypeTy *Type = ParseConversionFunctionId())
      return Actions.ActOnCXXConversionFunctionExpr(CurScope, OperatorLoc, Type,
                                                    Tok.is(tok::l_paren), SS,
                                                    isAddressOfOperand);

    // We already complained about a bad conversion-function-id,
    // above.
    return ExprError();
  }

  } // switch.

  assert(0 && "The switch was supposed to take care everything.");
}

/// ParseCXXCasts - This handles the various ways to cast expressions to another
/// type.
///
///       postfix-expression: [C++ 5.2p1]
///         'dynamic_cast' '<' type-name '>' '(' expression ')'
///         'static_cast' '<' type-name '>' '(' expression ')'
///         'reinterpret_cast' '<' type-name '>' '(' expression ')'
///         'const_cast' '<' type-name '>' '(' expression ')'
///
Parser::OwningExprResult Parser::ParseCXXCasts() {
  tok::TokenKind Kind = Tok.getKind();
  const char *CastName = 0;     // For error messages

  switch (Kind) {
  default: assert(0 && "Unknown C++ cast!"); abort();
  case tok::kw_const_cast:       CastName = "const_cast";       break;
  case tok::kw_dynamic_cast:     CastName = "dynamic_cast";     break;
  case tok::kw_reinterpret_cast: CastName = "reinterpret_cast"; break;
  case tok::kw_static_cast:      CastName = "static_cast";      break;
  }

  SourceLocation OpLoc = ConsumeToken();
  SourceLocation LAngleBracketLoc = Tok.getLocation();

  if (ExpectAndConsume(tok::less, diag::err_expected_less_after, CastName))
    return ExprError();

  TypeResult CastTy = ParseTypeName();
  SourceLocation RAngleBracketLoc = Tok.getLocation();

  if (ExpectAndConsume(tok::greater, diag::err_expected_greater))
    return ExprError(Diag(LAngleBracketLoc, diag::note_matching) << "<");

  SourceLocation LParenLoc = Tok.getLocation(), RParenLoc;

  if (ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after, CastName))
    return ExprError();

  OwningExprResult Result = ParseExpression();
  
  // Match the ')'.
  if (Result.isInvalid())
    SkipUntil(tok::r_paren);
  
  if (Tok.is(tok::r_paren))
    RParenLoc = ConsumeParen();
  else
    MatchRHSPunctuation(tok::r_paren, LParenLoc);

  if (!Result.isInvalid() && !CastTy.isInvalid())
    Result = Actions.ActOnCXXNamedCast(OpLoc, Kind,
                                       LAngleBracketLoc, CastTy.get(),
                                       RAngleBracketLoc,
                                       LParenLoc, move(Result), RParenLoc);

  return move(Result);
}

/// ParseCXXTypeid - This handles the C++ typeid expression.
///
///       postfix-expression: [C++ 5.2p1]
///         'typeid' '(' expression ')'
///         'typeid' '(' type-id ')'
///
Parser::OwningExprResult Parser::ParseCXXTypeid() {
  assert(Tok.is(tok::kw_typeid) && "Not 'typeid'!");

  SourceLocation OpLoc = ConsumeToken();
  SourceLocation LParenLoc = Tok.getLocation();
  SourceLocation RParenLoc;

  // typeid expressions are always parenthesized.
  if (ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after,
      "typeid"))
    return ExprError();

  OwningExprResult Result(Actions);

  if (isTypeIdInParens()) {
    TypeResult Ty = ParseTypeName();

    // Match the ')'.
    MatchRHSPunctuation(tok::r_paren, LParenLoc);

    if (Ty.isInvalid())
      return ExprError();

    Result = Actions.ActOnCXXTypeid(OpLoc, LParenLoc, /*isType=*/true,
                                    Ty.get(), RParenLoc);
  } else {
    Result = ParseExpression();

    // Match the ')'.
    if (Result.isInvalid())
      SkipUntil(tok::r_paren);
    else {
      MatchRHSPunctuation(tok::r_paren, LParenLoc);

      Result = Actions.ActOnCXXTypeid(OpLoc, LParenLoc, /*isType=*/false,
                                      Result.release(), RParenLoc);
    }
  }

  return move(Result);
}

/// ParseCXXBoolLiteral - This handles the C++ Boolean literals.
///
///       boolean-literal: [C++ 2.13.5]
///         'true'
///         'false'
Parser::OwningExprResult Parser::ParseCXXBoolLiteral() {
  tok::TokenKind Kind = Tok.getKind();
  return Actions.ActOnCXXBoolLiteral(ConsumeToken(), Kind);
}

/// ParseThrowExpression - This handles the C++ throw expression.
///
///       throw-expression: [C++ 15]
///         'throw' assignment-expression[opt]
Parser::OwningExprResult Parser::ParseThrowExpression() {
  assert(Tok.is(tok::kw_throw) && "Not throw!");
  SourceLocation ThrowLoc = ConsumeToken();           // Eat the throw token.

  // If the current token isn't the start of an assignment-expression,
  // then the expression is not present.  This handles things like:
  //   "C ? throw : (void)42", which is crazy but legal.
  switch (Tok.getKind()) {  // FIXME: move this predicate somewhere common.
  case tok::semi:
  case tok::r_paren:
  case tok::r_square:
  case tok::r_brace:
  case tok::colon:
  case tok::comma:
    return Actions.ActOnCXXThrow(ThrowLoc, ExprArg(Actions));

  default:
    OwningExprResult Expr(ParseAssignmentExpression());
    if (Expr.isInvalid()) return move(Expr);
    return Actions.ActOnCXXThrow(ThrowLoc, move(Expr));
  }
}

/// ParseCXXThis - This handles the C++ 'this' pointer.
///
/// C++ 9.3.2: In the body of a non-static member function, the keyword this is
/// a non-lvalue expression whose value is the address of the object for which
/// the function is called.
Parser::OwningExprResult Parser::ParseCXXThis() {
  assert(Tok.is(tok::kw_this) && "Not 'this'!");
  SourceLocation ThisLoc = ConsumeToken();
  return Actions.ActOnCXXThis(ThisLoc);
}

/// ParseCXXTypeConstructExpression - Parse construction of a specified type.
/// Can be interpreted either as function-style casting ("int(x)")
/// or class type construction ("ClassType(x,y,z)")
/// or creation of a value-initialized type ("int()").
///
///       postfix-expression: [C++ 5.2p1]
///         simple-type-specifier '(' expression-list[opt] ')'      [C++ 5.2.3]
///         typename-specifier '(' expression-list[opt] ')'         [TODO]
///
Parser::OwningExprResult
Parser::ParseCXXTypeConstructExpression(const DeclSpec &DS) {
  Declarator DeclaratorInfo(DS, Declarator::TypeNameContext);
  TypeTy *TypeRep = Actions.ActOnTypeName(CurScope, DeclaratorInfo).get();

  assert(Tok.is(tok::l_paren) && "Expected '('!");
  SourceLocation LParenLoc = ConsumeParen();

  ExprVector Exprs(Actions);
  CommaLocsTy CommaLocs;

  if (Tok.isNot(tok::r_paren)) {
    if (ParseExpressionList(Exprs, CommaLocs)) {
      SkipUntil(tok::r_paren);
      return ExprError();
    }
  }

  // Match the ')'.
  SourceLocation RParenLoc = MatchRHSPunctuation(tok::r_paren, LParenLoc);

  assert((Exprs.size() == 0 || Exprs.size()-1 == CommaLocs.size())&&
         "Unexpected number of commas!");
  return Actions.ActOnCXXTypeConstructExpr(DS.getSourceRange(), TypeRep,
                                           LParenLoc, move_arg(Exprs),
                                           CommaLocs.data(), RParenLoc);
}

/// ParseCXXCondition - if/switch/while/for condition expression.
///
///       condition:
///         expression
///         type-specifier-seq declarator '=' assignment-expression
/// [GNU]   type-specifier-seq declarator simple-asm-expr[opt] attributes[opt]
///             '=' assignment-expression
///
Parser::OwningExprResult Parser::ParseCXXCondition() {
  if (!isCXXConditionDeclaration())
    return ParseExpression(); // expression

  SourceLocation StartLoc = Tok.getLocation();

  // type-specifier-seq
  DeclSpec DS;
  ParseSpecifierQualifierList(DS);

  // declarator
  Declarator DeclaratorInfo(DS, Declarator::ConditionContext);
  ParseDeclarator(DeclaratorInfo);

  // simple-asm-expr[opt]
  if (Tok.is(tok::kw_asm)) {
    SourceLocation Loc;
    OwningExprResult AsmLabel(ParseSimpleAsm(&Loc));
    if (AsmLabel.isInvalid()) {
      SkipUntil(tok::semi);
      return ExprError();
    }
    DeclaratorInfo.setAsmLabel(AsmLabel.release());
    DeclaratorInfo.SetRangeEnd(Loc);
  }

  // If attributes are present, parse them.
  if (Tok.is(tok::kw___attribute)) {
    SourceLocation Loc;
    AttributeList *AttrList = ParseAttributes(&Loc);
    DeclaratorInfo.AddAttributes(AttrList, Loc);
  }

  // '=' assignment-expression
  if (Tok.isNot(tok::equal))
    return ExprError(Diag(Tok, diag::err_expected_equal_after_declarator));
  SourceLocation EqualLoc = ConsumeToken();
  OwningExprResult AssignExpr(ParseAssignmentExpression());
  if (AssignExpr.isInvalid())
    return ExprError();

  return Actions.ActOnCXXConditionDeclarationExpr(CurScope, StartLoc,
                                                  DeclaratorInfo,EqualLoc,
                                                  move(AssignExpr));
}

/// ParseCXXSimpleTypeSpecifier - [C++ 7.1.5.2] Simple type specifiers.
/// This should only be called when the current token is known to be part of
/// simple-type-specifier.
///
///       simple-type-specifier:
///         '::'[opt] nested-name-specifier[opt] type-name
///         '::'[opt] nested-name-specifier 'template' simple-template-id [TODO]
///         char
///         wchar_t
///         bool
///         short
///         int
///         long
///         signed
///         unsigned
///         float
///         double
///         void
/// [GNU]   typeof-specifier
/// [C++0x] auto               [TODO]
///
///       type-name:
///         class-name
///         enum-name
///         typedef-name
///
void Parser::ParseCXXSimpleTypeSpecifier(DeclSpec &DS) {
  DS.SetRangeStart(Tok.getLocation());
  const char *PrevSpec;
  SourceLocation Loc = Tok.getLocation();
  
  switch (Tok.getKind()) {
  case tok::identifier:   // foo::bar
  case tok::coloncolon:   // ::foo::bar
    assert(0 && "Annotation token should already be formed!");
  default: 
    assert(0 && "Not a simple-type-specifier token!");
    abort();

  // type-name
  case tok::annot_typename: {
    DS.SetTypeSpecType(DeclSpec::TST_typename, Loc, PrevSpec,
                       Tok.getAnnotationValue());
    break;
  }
    
  // builtin types
  case tok::kw_short:
    DS.SetTypeSpecWidth(DeclSpec::TSW_short, Loc, PrevSpec);
    break;
  case tok::kw_long:
    DS.SetTypeSpecWidth(DeclSpec::TSW_long, Loc, PrevSpec);
    break;
  case tok::kw_signed:
    DS.SetTypeSpecSign(DeclSpec::TSS_signed, Loc, PrevSpec);
    break;
  case tok::kw_unsigned:
    DS.SetTypeSpecSign(DeclSpec::TSS_unsigned, Loc, PrevSpec);
    break;
  case tok::kw_void:
    DS.SetTypeSpecType(DeclSpec::TST_void, Loc, PrevSpec);
    break;
  case tok::kw_char:
    DS.SetTypeSpecType(DeclSpec::TST_char, Loc, PrevSpec);
    break;
  case tok::kw_int:
    DS.SetTypeSpecType(DeclSpec::TST_int, Loc, PrevSpec);
    break;
  case tok::kw_float:
    DS.SetTypeSpecType(DeclSpec::TST_float, Loc, PrevSpec);
    break;
  case tok::kw_double:
    DS.SetTypeSpecType(DeclSpec::TST_double, Loc, PrevSpec);
    break;
  case tok::kw_wchar_t:
    DS.SetTypeSpecType(DeclSpec::TST_wchar, Loc, PrevSpec);
    break;
  case tok::kw_bool:
    DS.SetTypeSpecType(DeclSpec::TST_bool, Loc, PrevSpec);
    break;
  
  // GNU typeof support.
  case tok::kw_typeof:
    ParseTypeofSpecifier(DS);
    DS.Finish(Diags, PP);
    return;
  }
  if (Tok.is(tok::annot_typename))
    DS.SetRangeEnd(Tok.getAnnotationEndLoc());
  else
    DS.SetRangeEnd(Tok.getLocation());
  ConsumeToken();
  DS.Finish(Diags, PP);
}

/// ParseCXXTypeSpecifierSeq - Parse a C++ type-specifier-seq (C++
/// [dcl.name]), which is a non-empty sequence of type-specifiers,
/// e.g., "const short int". Note that the DeclSpec is *not* finished
/// by parsing the type-specifier-seq, because these sequences are
/// typically followed by some form of declarator. Returns true and
/// emits diagnostics if this is not a type-specifier-seq, false
/// otherwise.
///
///   type-specifier-seq: [C++ 8.1]
///     type-specifier type-specifier-seq[opt]
///
bool Parser::ParseCXXTypeSpecifierSeq(DeclSpec &DS) {
  DS.SetRangeStart(Tok.getLocation());
  const char *PrevSpec = 0;
  int isInvalid = 0;

  // Parse one or more of the type specifiers.
  if (!ParseOptionalTypeSpecifier(DS, isInvalid, PrevSpec)) {
    Diag(Tok, diag::err_operator_missing_type_specifier);
    return true;
  }
  
  while (ParseOptionalTypeSpecifier(DS, isInvalid, PrevSpec)) ;

  return false;
}

/// TryParseOperatorFunctionId - Attempts to parse a C++ overloaded
/// operator name (C++ [over.oper]). If successful, returns the
/// predefined identifier that corresponds to that overloaded
/// operator. Otherwise, returns NULL and does not consume any tokens.
///
///       operator-function-id: [C++ 13.5]
///         'operator' operator
///
/// operator: one of
///            new   delete  new[]   delete[]
///            +     -    *  /    %  ^    &   |   ~
///            !     =    <  >    += -=   *=  /=  %=
///            ^=    &=   |= <<   >> >>= <<=  ==  !=
///            <=    >=   && ||   ++ --   ,   ->* ->
///            ()    []
OverloadedOperatorKind
Parser::TryParseOperatorFunctionId(SourceLocation *EndLoc) {
  assert(Tok.is(tok::kw_operator) && "Expected 'operator' keyword");
  SourceLocation Loc;

  OverloadedOperatorKind Op = OO_None;
  switch (NextToken().getKind()) {
  case tok::kw_new:
    ConsumeToken(); // 'operator'
    Loc = ConsumeToken(); // 'new'
    if (Tok.is(tok::l_square)) {
      ConsumeBracket(); // '['
      Loc = Tok.getLocation();
      ExpectAndConsume(tok::r_square, diag::err_expected_rsquare); // ']'
      Op = OO_Array_New;
    } else {
      Op = OO_New;
    }
    if (EndLoc)
      *EndLoc = Loc;
    return Op;

  case tok::kw_delete:
    ConsumeToken(); // 'operator'
    Loc = ConsumeToken(); // 'delete'
    if (Tok.is(tok::l_square)) {
      ConsumeBracket(); // '['
      Loc = Tok.getLocation();
      ExpectAndConsume(tok::r_square, diag::err_expected_rsquare); // ']'
      Op = OO_Array_Delete;
    } else {
      Op = OO_Delete;
    }
    if (EndLoc)
      *EndLoc = Loc;
    return Op;

#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly)  \
    case tok::Token:  Op = OO_##Name; break;
#define OVERLOADED_OPERATOR_MULTI(Name,Spelling,Unary,Binary,MemberOnly)
#include "clang/Basic/OperatorKinds.def"

  case tok::l_paren:
    ConsumeToken(); // 'operator'
    ConsumeParen(); // '('
    Loc = Tok.getLocation();
    ExpectAndConsume(tok::r_paren, diag::err_expected_rparen); // ')'
    if (EndLoc)
      *EndLoc = Loc;
    return OO_Call;

  case tok::l_square:
    ConsumeToken(); // 'operator'
    ConsumeBracket(); // '['
    Loc = Tok.getLocation();
    ExpectAndConsume(tok::r_square, diag::err_expected_rsquare); // ']'
    if (EndLoc)
      *EndLoc = Loc;
    return OO_Subscript;

  default:
    return OO_None;
  }

  ConsumeToken(); // 'operator'
  Loc = ConsumeAnyToken(); // the operator itself
  if (EndLoc)
    *EndLoc = Loc;
  return Op;
}

/// ParseConversionFunctionId - Parse a C++ conversion-function-id,
/// which expresses the name of a user-defined conversion operator
/// (C++ [class.conv.fct]p1). Returns the type that this operator is
/// specifying a conversion for, or NULL if there was an error.
///
///        conversion-function-id: [C++ 12.3.2]
///                   operator conversion-type-id
///
///        conversion-type-id:
///                   type-specifier-seq conversion-declarator[opt]
///
///        conversion-declarator:
///                   ptr-operator conversion-declarator[opt]
Parser::TypeTy *Parser::ParseConversionFunctionId(SourceLocation *EndLoc) {
  assert(Tok.is(tok::kw_operator) && "Expected 'operator' keyword");
  ConsumeToken(); // 'operator'

  // Parse the type-specifier-seq.
  DeclSpec DS;
  if (ParseCXXTypeSpecifierSeq(DS))
    return 0;

  // Parse the conversion-declarator, which is merely a sequence of
  // ptr-operators.
  Declarator D(DS, Declarator::TypeNameContext);
  ParseDeclaratorInternal(D, /*DirectDeclParser=*/0);
  if (EndLoc)
    *EndLoc = D.getSourceRange().getEnd();

  // Finish up the type.
  Action::TypeResult Result = Actions.ActOnTypeName(CurScope, D);
  if (Result.isInvalid())
    return 0;
  else
    return Result.get();
}

/// ParseCXXNewExpression - Parse a C++ new-expression. New is used to allocate
/// memory in a typesafe manner and call constructors.
/// 
/// This method is called to parse the new expression after the optional :: has
/// been already parsed.  If the :: was present, "UseGlobal" is true and "Start"
/// is its location.  Otherwise, "Start" is the location of the 'new' token.
///
///        new-expression:
///                   '::'[opt] 'new' new-placement[opt] new-type-id
///                                     new-initializer[opt]
///                   '::'[opt] 'new' new-placement[opt] '(' type-id ')'
///                                     new-initializer[opt]
///
///        new-placement:
///                   '(' expression-list ')'
///
///        new-type-id:
///                   type-specifier-seq new-declarator[opt]
///
///        new-declarator:
///                   ptr-operator new-declarator[opt]
///                   direct-new-declarator
///
///        new-initializer:
///                   '(' expression-list[opt] ')'
/// [C++0x]           braced-init-list                                   [TODO]
///
Parser::OwningExprResult
Parser::ParseCXXNewExpression(bool UseGlobal, SourceLocation Start) {
  assert(Tok.is(tok::kw_new) && "expected 'new' token");
  ConsumeToken();   // Consume 'new'

  // A '(' now can be a new-placement or the '(' wrapping the type-id in the
  // second form of new-expression. It can't be a new-type-id.

  ExprVector PlacementArgs(Actions);
  SourceLocation PlacementLParen, PlacementRParen;

  bool ParenTypeId;
  DeclSpec DS;
  Declarator DeclaratorInfo(DS, Declarator::TypeNameContext);
  if (Tok.is(tok::l_paren)) {
    // If it turns out to be a placement, we change the type location.
    PlacementLParen = ConsumeParen();
    if (ParseExpressionListOrTypeId(PlacementArgs, DeclaratorInfo)) {
      SkipUntil(tok::semi, /*StopAtSemi=*/true, /*DontConsume=*/true);
      return ExprError();
    }

    PlacementRParen = MatchRHSPunctuation(tok::r_paren, PlacementLParen);
    if (PlacementRParen.isInvalid()) {
      SkipUntil(tok::semi, /*StopAtSemi=*/true, /*DontConsume=*/true);
      return ExprError();
    }

    if (PlacementArgs.empty()) {
      // Reset the placement locations. There was no placement.
      PlacementLParen = PlacementRParen = SourceLocation();
      ParenTypeId = true;
    } else {
      // We still need the type.
      if (Tok.is(tok::l_paren)) {
        SourceLocation LParen = ConsumeParen();
        ParseSpecifierQualifierList(DS);
        DeclaratorInfo.SetSourceRange(DS.getSourceRange());
        ParseDeclarator(DeclaratorInfo);
        MatchRHSPunctuation(tok::r_paren, LParen);
        ParenTypeId = true;
      } else {
        if (ParseCXXTypeSpecifierSeq(DS))
          DeclaratorInfo.setInvalidType(true);
        else {
          DeclaratorInfo.SetSourceRange(DS.getSourceRange());
          ParseDeclaratorInternal(DeclaratorInfo,
                                  &Parser::ParseDirectNewDeclarator);
        }
        ParenTypeId = false;
      }
    }
  } else {
    // A new-type-id is a simplified type-id, where essentially the
    // direct-declarator is replaced by a direct-new-declarator.
    if (ParseCXXTypeSpecifierSeq(DS))
      DeclaratorInfo.setInvalidType(true);
    else {
      DeclaratorInfo.SetSourceRange(DS.getSourceRange());
      ParseDeclaratorInternal(DeclaratorInfo,
                              &Parser::ParseDirectNewDeclarator);
    }
    ParenTypeId = false;
  }
  if (DeclaratorInfo.isInvalidType()) {
    SkipUntil(tok::semi, /*StopAtSemi=*/true, /*DontConsume=*/true);
    return ExprError();
  }

  ExprVector ConstructorArgs(Actions);
  SourceLocation ConstructorLParen, ConstructorRParen;

  if (Tok.is(tok::l_paren)) {
    ConstructorLParen = ConsumeParen();
    if (Tok.isNot(tok::r_paren)) {
      CommaLocsTy CommaLocs;
      if (ParseExpressionList(ConstructorArgs, CommaLocs)) {
        SkipUntil(tok::semi, /*StopAtSemi=*/true, /*DontConsume=*/true);
        return ExprError();
      }
    }
    ConstructorRParen = MatchRHSPunctuation(tok::r_paren, ConstructorLParen);
    if (ConstructorRParen.isInvalid()) {
      SkipUntil(tok::semi, /*StopAtSemi=*/true, /*DontConsume=*/true);
      return ExprError();
    }
  }

  return Actions.ActOnCXXNew(Start, UseGlobal, PlacementLParen,
                             move_arg(PlacementArgs), PlacementRParen,
                             ParenTypeId, DeclaratorInfo, ConstructorLParen,
                             move_arg(ConstructorArgs), ConstructorRParen);
}

/// ParseDirectNewDeclarator - Parses a direct-new-declarator. Intended to be
/// passed to ParseDeclaratorInternal.
///
///        direct-new-declarator:
///                   '[' expression ']'
///                   direct-new-declarator '[' constant-expression ']'
///
void Parser::ParseDirectNewDeclarator(Declarator &D) {
  // Parse the array dimensions.
  bool first = true;
  while (Tok.is(tok::l_square)) {
    SourceLocation LLoc = ConsumeBracket();
    OwningExprResult Size(first ? ParseExpression()
                                : ParseConstantExpression());
    if (Size.isInvalid()) {
      // Recover
      SkipUntil(tok::r_square);
      return;
    }
    first = false;

    SourceLocation RLoc = MatchRHSPunctuation(tok::r_square, LLoc);
    D.AddTypeInfo(DeclaratorChunk::getArray(0, /*static=*/false, /*star=*/false,
                                            Size.release(), LLoc),
                  RLoc);

    if (RLoc.isInvalid())
      return;
  }
}

/// ParseExpressionListOrTypeId - Parse either an expression-list or a type-id.
/// This ambiguity appears in the syntax of the C++ new operator.
///
///        new-expression:
///                   '::'[opt] 'new' new-placement[opt] '(' type-id ')'
///                                     new-initializer[opt]
///
///        new-placement:
///                   '(' expression-list ')'
///
bool Parser::ParseExpressionListOrTypeId(ExprListTy &PlacementArgs,
                                         Declarator &D) {
  // The '(' was already consumed.
  if (isTypeIdInParens()) {
    ParseSpecifierQualifierList(D.getMutableDeclSpec());
    D.SetSourceRange(D.getDeclSpec().getSourceRange());
    ParseDeclarator(D);
    return D.isInvalidType();
  }

  // It's not a type, it has to be an expression list.
  // Discard the comma locations - ActOnCXXNew has enough parameters.
  CommaLocsTy CommaLocs;
  return ParseExpressionList(PlacementArgs, CommaLocs);
}

/// ParseCXXDeleteExpression - Parse a C++ delete-expression. Delete is used
/// to free memory allocated by new.
///
/// This method is called to parse the 'delete' expression after the optional
/// '::' has been already parsed.  If the '::' was present, "UseGlobal" is true
/// and "Start" is its location.  Otherwise, "Start" is the location of the
/// 'delete' token.
///
///        delete-expression:
///                   '::'[opt] 'delete' cast-expression
///                   '::'[opt] 'delete' '[' ']' cast-expression
Parser::OwningExprResult
Parser::ParseCXXDeleteExpression(bool UseGlobal, SourceLocation Start) {
  assert(Tok.is(tok::kw_delete) && "Expected 'delete' keyword");
  ConsumeToken(); // Consume 'delete'

  // Array delete?
  bool ArrayDelete = false;
  if (Tok.is(tok::l_square)) {
    ArrayDelete = true;
    SourceLocation LHS = ConsumeBracket();
    SourceLocation RHS = MatchRHSPunctuation(tok::r_square, LHS);
    if (RHS.isInvalid())
      return ExprError();
  }

  OwningExprResult Operand(ParseCastExpression(false));
  if (Operand.isInvalid())
    return move(Operand);

  return Actions.ActOnCXXDelete(Start, UseGlobal, ArrayDelete, move(Operand));
}

static UnaryTypeTrait UnaryTypeTraitFromTokKind(tok::TokenKind kind)
{
  switch(kind) {
  default: assert(false && "Not a known unary type trait.");
  case tok::kw___has_nothrow_assign:      return UTT_HasNothrowAssign;
  case tok::kw___has_nothrow_copy:        return UTT_HasNothrowCopy;
  case tok::kw___has_nothrow_constructor: return UTT_HasNothrowConstructor;
  case tok::kw___has_trivial_assign:      return UTT_HasTrivialAssign;
  case tok::kw___has_trivial_copy:        return UTT_HasTrivialCopy;
  case tok::kw___has_trivial_constructor: return UTT_HasTrivialConstructor;
  case tok::kw___has_trivial_destructor:  return UTT_HasTrivialDestructor;
  case tok::kw___has_virtual_destructor:  return UTT_HasVirtualDestructor;
  case tok::kw___is_abstract:             return UTT_IsAbstract;
  case tok::kw___is_class:                return UTT_IsClass;
  case tok::kw___is_empty:                return UTT_IsEmpty;
  case tok::kw___is_enum:                 return UTT_IsEnum;
  case tok::kw___is_pod:                  return UTT_IsPOD;
  case tok::kw___is_polymorphic:          return UTT_IsPolymorphic;
  case tok::kw___is_union:                return UTT_IsUnion;
  }
}

/// ParseUnaryTypeTrait - Parse the built-in unary type-trait
/// pseudo-functions that allow implementation of the TR1/C++0x type traits
/// templates.
///
///       primary-expression:
/// [GNU]             unary-type-trait '(' type-id ')'
///
Parser::OwningExprResult Parser::ParseUnaryTypeTrait()
{
  UnaryTypeTrait UTT = UnaryTypeTraitFromTokKind(Tok.getKind());
  SourceLocation Loc = ConsumeToken();

  SourceLocation LParen = Tok.getLocation();
  if (ExpectAndConsume(tok::l_paren, diag::err_expected_lparen))
    return ExprError();

  // FIXME: Error reporting absolutely sucks! If the this fails to parse a type
  // there will be cryptic errors about mismatched parentheses and missing
  // specifiers.
  TypeResult Ty = ParseTypeName();

  SourceLocation RParen = MatchRHSPunctuation(tok::r_paren, LParen);

  if (Ty.isInvalid())
    return ExprError();

  return Actions.ActOnUnaryTypeTrait(UTT, Loc, LParen, Ty.get(), RParen);
}

/// ParseCXXAmbiguousParenExpression - We have parsed the left paren of a
/// parenthesized ambiguous type-id. This uses tentative parsing to disambiguate
/// based on the context past the parens.
Parser::OwningExprResult
Parser::ParseCXXAmbiguousParenExpression(ParenParseOption &ExprType,
                                         TypeTy *&CastTy,
                                         SourceLocation LParenLoc,
                                         SourceLocation &RParenLoc) {
  assert(getLang().CPlusPlus && "Should only be called for C++!");
  assert(ExprType == CastExpr && "Compound literals are not ambiguous!");
  assert(isTypeIdInParens() && "Not a type-id!");

  OwningExprResult Result(Actions, true);
  CastTy = 0;

  // We need to disambiguate a very ugly part of the C++ syntax:
  //
  // (T())x;  - type-id
  // (T())*x; - type-id
  // (T())/x; - expression
  // (T());   - expression
  //
  // The bad news is that we cannot use the specialized tentative parser, since
  // it can only verify that the thing inside the parens can be parsed as
  // type-id, it is not useful for determining the context past the parens.
  //
  // The good news is that the parser can disambiguate this part without
  // making any unnecessary Action calls.
  //
  // It uses a scheme similar to parsing inline methods. The parenthesized
  // tokens are cached, the context that follows is determined (possibly by
  // parsing a cast-expression), and then we re-introduce the cached tokens
  // into the token stream and parse them appropriately.

  ParenParseOption ParseAs;  
  CachedTokens Toks;

  // Store the tokens of the parentheses. We will parse them after we determine
  // the context that follows them.
  if (!ConsumeAndStoreUntil(tok::r_paren, tok::unknown, Toks, tok::semi)) {
    // We didn't find the ')' we expected.
    MatchRHSPunctuation(tok::r_paren, LParenLoc);
    return ExprError();
  }

  if (Tok.is(tok::l_brace)) {
    ParseAs = CompoundLiteral;
  } else {
    bool NotCastExpr;
    // FIXME: Special-case ++ and --: "(S())++;" is not a cast-expression
    if (Tok.is(tok::l_paren) && NextToken().is(tok::r_paren)) {
      NotCastExpr = true;
    } else {
      // Try parsing the cast-expression that may follow.
      // If it is not a cast-expression, NotCastExpr will be true and no token
      // will be consumed.
      Result = ParseCastExpression(false/*isUnaryExpression*/,
                                   false/*isAddressofOperand*/,
                                   NotCastExpr);
    }

    // If we parsed a cast-expression, it's really a type-id, otherwise it's
    // an expression.
    ParseAs = NotCastExpr ? SimpleExpr : CastExpr;
  }

  // The current token should go after the cached tokens. 
  Toks.push_back(Tok);
  // Re-enter the stored parenthesized tokens into the token stream, so we may
  // parse them now.
  PP.EnterTokenStream(Toks.data(), Toks.size(),
                      true/*DisableMacroExpansion*/, false/*OwnsTokens*/);
  // Drop the current token and bring the first cached one. It's the same token
  // as when we entered this function.
  ConsumeAnyToken();

  if (ParseAs >= CompoundLiteral) {
    TypeResult Ty = ParseTypeName();

    // Match the ')'.
    if (Tok.is(tok::r_paren))
      RParenLoc = ConsumeParen();
    else
      MatchRHSPunctuation(tok::r_paren, LParenLoc);

    if (ParseAs == CompoundLiteral) {
      ExprType = CompoundLiteral;
      return ParseCompoundLiteralExpression(Ty.get(), LParenLoc, RParenLoc);
    }
    
    // We parsed '(' type-id ')' and the thing after it wasn't a '{'.
    assert(ParseAs == CastExpr);

    if (Ty.isInvalid())
      return ExprError();

    CastTy = Ty.get();

    // Result is what ParseCastExpression returned earlier.
    if (!Result.isInvalid())
      Result = Actions.ActOnCastExpr(LParenLoc, CastTy, RParenLoc,move(Result));
    return move(Result);
  }
  
  // Not a compound literal, and not followed by a cast-expression.
  assert(ParseAs == SimpleExpr);

  ExprType = SimpleExpr;
  Result = ParseExpression();
  if (!Result.isInvalid() && Tok.is(tok::r_paren))
    Result = Actions.ActOnParenExpr(LParenLoc, Tok.getLocation(), move(Result));

  // Match the ')'.
  if (Result.isInvalid()) {
    SkipUntil(tok::r_paren);
    return ExprError();
  }
  
  if (Tok.is(tok::r_paren))
    RParenLoc = ConsumeParen();
  else
    MatchRHSPunctuation(tok::r_paren, LParenLoc);

  return move(Result);
}
