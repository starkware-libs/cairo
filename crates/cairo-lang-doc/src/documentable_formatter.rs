use std::fmt;
use std::fmt::Write;

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, ImplAliasId,
    ImplConstantDefId, ImplDefId, ImplFunctionId, ImplTypeDefId, LanguageElementId, LookupItemId,
    MemberId, ModuleTypeAliasId, NamedLanguageElementId, StructId, TopLevelLanguageElementId,
    TraitConstantId, TraitFunctionId, TraitId, TraitTypeId, VariantId,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::items::generics::GenericArgumentId;
use cairo_lang_semantic::items::modifiers::get_relevant_modifier;
use cairo_lang_semantic::items::visibility::Visibility;
use cairo_lang_semantic::{Expr, GenericParam, Signature};
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedStablePtr, TypedSyntaxNode, green};
use cairo_lang_utils::{LookupIntern, Upcast};
use smol_str::SmolStr;

use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;

const INDENT: &str = "    ";
const MISSING: &str = "<missing>";

pub trait HirDisplay {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error>;

    fn write_module_item_visibility(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        if let DocumentableItemId::LookupItem(LookupItemId::ModuleItem(module_item_id)) = item_id {
            let parent_module = module_item_id.parent_module(f.db.upcast());
            let item_name = module_item_id.name(f.db.upcast());
            let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
                .module_item_info_by_name(parent_module, item_name.clone())
                .unwrap()
                .unwrap();
            let visibility = match module_item_info.visibility {
                Visibility::Public => "pub ",
                Visibility::PublicInCrate => "pub(crate) ",
                Visibility::Private => "",
            };
            write!(f, "{}", visibility)
        } else {
            panic!("Expected a ModuleItem, found a different type");
        }
    }

    fn get_signature(&self, f: &mut HirFormatter<'_>, item_id: DocumentableItemId) -> String {
        self.hir_fmt(f, item_id).unwrap();
        f.buf.clone()
    }
}

pub struct HirFormatter<'a> {
    /// The database handle
    pub db: &'a dyn DocGroup,
    /// A buffer to intercept writes with
    buf: String,
}

impl fmt::Write for HirFormatter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buf.push_str(s);
        Ok(())
    }
}

impl<'a> HirFormatter<'a> {
    pub fn new(db: &'a dyn DocGroup) -> Self {
        Self { db: db.upcast(), buf: String::new() }
    }
}

impl HirDisplay for VariantId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        let name = item_id.name(f.db.upcast());
        let variant_semantic = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .variant_semantic(self.enum_id(f.db.upcast()), *self)
            .unwrap();
        let variant_type = variant_semantic.ty.format(f.db.upcast());
        if !variant_semantic.ty.is_unit(f.db.upcast()) {
            let variant_type = extract_and_format(&variant_type);
            let variant_signature =
                format!("{name}{} {variant_type}", syntax_kind_to_text(SyntaxKind::TerminalColon),);
            write!(
                f,
                "{}{INDENT}{variant_signature}",
                syntax_kind_to_text(SyntaxKind::TokenNewline),
            )
        } else {
            write!(f, " {}{}", INDENT, name)
        }
    }
}

impl HirDisplay for EnumId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        self.write_module_item_visibility(f, item_id)?;
        let item_name = item_id.name(f.db.upcast());
        write!(
            f,
            "{} {item_name} {}",
            syntax_kind_to_text(SyntaxKind::TerminalEnum),
            syntax_kind_to_text(SyntaxKind::TerminalLBrace)
        )?;
        let variants =
            <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db).enum_variants(*self).unwrap();
        variants.iter().for_each(|(name, variant_id)| {
            let variant_semantic = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
                .variant_semantic(*self, *variant_id)
                .unwrap();
            let variant_type = variant_semantic.ty.format(f.db.upcast());
            if !variant_semantic.ty.is_unit(f.db.upcast()) {
                let variant_type = extract_and_format(&variant_type);
                let variant_signature = format!(
                    "{name}{} {variant_type}",
                    syntax_kind_to_text(SyntaxKind::TerminalColon),
                );
                write!(
                    f,
                    "{}{INDENT}{variant_signature}{}",
                    syntax_kind_to_text(SyntaxKind::TokenNewline),
                    syntax_kind_to_text(SyntaxKind::TerminalComma),
                )
                .unwrap();
            } else {
                write!(
                    f,
                    "{}{INDENT}{name}{}",
                    syntax_kind_to_text(SyntaxKind::TokenNewline),
                    syntax_kind_to_text(SyntaxKind::TerminalComma),
                )
                .unwrap();
            }
        });
        write!(
            f,
            "{}{}",
            syntax_kind_to_text(SyntaxKind::TokenNewline),
            syntax_kind_to_text(SyntaxKind::TerminalRBrace)
        )?;
        Ok(())
    }
}

impl HirDisplay for MemberId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        let name = item_id.name(f.db.upcast());
        let struct_id = self.struct_id(f.db.upcast());
        let semantic_members = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .struct_members(struct_id)
            .unwrap();
        let member = semantic_members.get(&name).unwrap();
        if member.ty.is_unit(f.db.upcast()) {
            write!(f, "{}{}", get_syntactic_visibility(&member.visibility), name)
        } else {
            let member_type = extract_and_format(&member.ty.format(f.db.upcast()));
            write!(
                f,
                "{}{}{} {}",
                get_syntactic_visibility(&member.visibility),
                name,
                syntax_kind_to_text(SyntaxKind::TerminalColon),
                member_type
            )
        }
    }
}

impl HirDisplay for StructId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        let struct_attributes = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .struct_attributes(*self)
            .unwrap();
        HirFormatter::write_struct_attributes_syntax(struct_attributes, f)?;
        self.write_module_item_visibility(f, item_id)?;
        let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .struct_generic_params(*self)
            .unwrap();
        let generics_formatted = HirFormatter::format_generic_params(generic_params, f);
        write!(
            f,
            "{} {}{} {}",
            syntax_kind_to_text(SyntaxKind::TerminalStruct),
            item_id.name(f.db.upcast()),
            generics_formatted,
            syntax_kind_to_text(SyntaxKind::TerminalLBrace),
        )?;
        let semantic_members = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .struct_members(*self)
            .unwrap();
        semantic_members.iter().for_each(|(_, m)| {
            let member = format!(
                "{}{}{} {}",
                get_syntactic_visibility(&m.visibility),
                m.id.name(f.db.upcast()),
                syntax_kind_to_text(SyntaxKind::TerminalColon),
                extract_and_format(&m.ty.format(f.db.upcast())),
            );
            write!(f, "{}{INDENT}{member},", syntax_kind_to_text(SyntaxKind::TokenNewline),)
                .unwrap();
        });
        write!(
            f,
            "{}{}",
            if semantic_members.len() > 0 {
                syntax_kind_to_text(SyntaxKind::TokenNewline)
            } else {
                "".to_string()
            },
            syntax_kind_to_text(SyntaxKind::TerminalRBrace)
        )?;
        Ok(())
    }
}

trait FunctionSignatureWriter {
    fn write_function_signature(
        &self,
        f: &mut HirFormatter<'_>,
        signature: Signature,
        item_id: DocumentableItemId,
        syntactic_kind: String,
        generic_args_formatted: String,
    ) -> Result<(), fmt::Error> {
        write!(
            f,
            "{} {}{}{}",
            syntactic_kind,
            item_id.name(f.db.upcast()),
            generic_args_formatted,
            syntax_kind_to_text(SyntaxKind::TerminalLParen)
        )?;

        let mut count = signature.params.len();
        let mut postfix = format!("{} ", syntax_kind_to_text(SyntaxKind::TerminalComma),);
        signature.params.iter().for_each(|param| {
            if count == 1 {
                postfix = "".to_string();
            }
            let syntax_node = param.id.stable_location(f.db.upcast()).syntax_node(f.db.upcast());
            let type_definition = Self::get_type_clause(syntax_node, f).unwrap();
            let modifier = get_relevant_modifier(&param.mutability);
            let modifier_postfix = if modifier.len() == 0 { "" } else { " " };
            write!(
                f,
                "{}{}{}{}{}",
                modifier, modifier_postfix, param.name, type_definition, postfix
            )
            .unwrap();
            count -= 1;
        });

        let return_postfix = {
            if signature.return_type.is_unit(f.db.upcast()) {
                ""
            } else {
                &format!(
                    " {} {}",
                    syntax_kind_to_text(SyntaxKind::TerminalArrow),
                    extract_and_format(&signature.return_type.format(f.db.upcast()))
                )
            }
        };
        write!(f, "{}{}", syntax_kind_to_text(SyntaxKind::TerminalRParen), return_postfix)?;
        Ok(())
    }

    fn get_type_clause(syntax_node: SyntaxNode, f: &mut HirFormatter<'_>) -> Option<String> {
        let children = f.db.get_children(syntax_node);
        for child in children.iter() {
            if child.kind(f.db.upcast()) == SyntaxKind::TypeClause {
                return Some(child.clone().get_text_without_all_comment_trivia(f.db.upcast()));
            }
        }
        Some(String::from(MISSING))
    }
}

impl FunctionSignatureWriter for FreeFunctionId {}

impl HirDisplay for FreeFunctionId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        self.write_module_item_visibility(f, item_id)?;
        let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .free_function_generic_params(*self)
            .unwrap();
        let generics_formatted = HirFormatter::format_generic_params(generic_params, f);

        let signature = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .free_function_signature(*self)
            .unwrap();
        Self::write_function_signature(
            self,
            f,
            signature,
            item_id,
            syntax_kind_to_text(SyntaxKind::TerminalFunction),
            generics_formatted,
        )
    }
}

trait SyntacticWriter {
    fn write_syntactic_evaluation(
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        let syntax_node =
            item_id.stable_location(f.db.upcast()).unwrap().syntax_node(f.db.upcast());

        if matches!(
            &syntax_node.green_node(f.db.upcast()).details,
            green::GreenNodeDetails::Node { .. }
        ) {
            let mut is_after_evaluation_value = false;
            for child in f.db.get_children(syntax_node.clone()).iter() {
                let kind = child.kind(f.db.upcast());
                if !matches!(kind, SyntaxKind::Trivia) {
                    if matches!(kind, SyntaxKind::TerminalSemicolon) {
                        write!(f.buf, "{}", syntax_kind_to_text(SyntaxKind::TerminalSemicolon))?;
                        return Ok(());
                    }
                    if is_after_evaluation_value {
                        f.buf.write_str(&SyntaxNode::get_text_without_all_comment_trivia(
                            child,
                            f.db.upcast(),
                        ))?;
                    };
                    if matches!(kind, SyntaxKind::TerminalEq) {
                        is_after_evaluation_value = true;
                    }
                }
            }
        };

        Ok(())
    }
}

impl SyntacticWriter for ConstantId {}

impl HirDisplay for ConstantId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        self.write_module_item_visibility(f, item_id)?;
        write!(
            f,
            "{} {}{} {} {} ",
            syntax_kind_to_text(SyntaxKind::TerminalConst),
            item_id.name(f.db.upcast()),
            syntax_kind_to_text(SyntaxKind::TerminalColon),
            extract_and_format(
                &<dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
                    .constant_const_type(*self)
                    .unwrap()
                    .format(f.db.upcast())
            ),
            syntax_kind_to_text(SyntaxKind::TerminalEq),
        )?;
        let constant = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .constant_semantic_data(*self)
            .unwrap();
        let constant_value = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .lookup_intern_const_value(
                <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
                    .constant_const_value(*self)
                    .unwrap(),
            );
        let expression = &constant.arenas.exprs[constant.value];

        match expression {
            Expr::Literal(v) => {
                write!(f, "{}{}", v.value, syntax_kind_to_text(SyntaxKind::TerminalSemicolon))?;
            }
            Expr::FunctionCall(_) => {
                if let ConstValue::Int(value, _) = constant_value {
                    Self::write_syntactic_evaluation(f, item_id)?;
                    write!(
                        f.buf,
                        " {} {} {}",
                        syntax_kind_to_text(SyntaxKind::TokenSingleLineInnerComment),
                        syntax_kind_to_text(SyntaxKind::TokenEq),
                        value
                    )?;
                }
            }
            _ => {
                Self::write_syntactic_evaluation(f, item_id)?;
            }
        };
        Ok(())
    }
}

impl SyntacticWriter for ImplConstantDefId {}

impl HirDisplay for ImplConstantDefId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        let def_value_id = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .impl_constant_def_value(*self)
            .unwrap();
        write!(
            f,
            "{} {}{} {} {} ",
            syntax_kind_to_text(SyntaxKind::TerminalConst),
            item_id.name(f.db.upcast()),
            syntax_kind_to_text(SyntaxKind::TerminalColon),
            extract_and_format(&def_value_id.ty(f.db.upcast()).unwrap().format(f.db.upcast())),
            syntax_kind_to_text(SyntaxKind::TerminalEq),
        )?;

        Self::write_syntactic_evaluation(f, item_id)?;
        Ok(())
    }
}

impl FunctionSignatureWriter for TraitFunctionId {}

impl HirDisplay for TraitFunctionId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        let signature = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .trait_function_signature(*self)
            .unwrap();
        let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .trait_function_generic_params(*self)
            .unwrap();
        let generic_params_formatted = HirFormatter::format_generic_params(generic_params, f);
        Self::write_function_signature(
            self,
            f,
            signature,
            item_id,
            syntax_kind_to_text(SyntaxKind::TerminalFunction),
            generic_params_formatted,
        )
    }
}

impl FunctionSignatureWriter for ImplFunctionId {}

impl HirDisplay for ImplFunctionId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        let signature = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .impl_function_signature(*self)
            .unwrap();
        let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .impl_function_generic_params(*self)
            .unwrap();
        let generic_params_formatted = HirFormatter::format_generic_params(generic_params, f);
        Self::write_function_signature(
            self,
            f,
            signature,
            item_id,
            syntax_kind_to_text(SyntaxKind::TerminalFunction),
            generic_params_formatted,
        )
    }
}

impl HirDisplay for TraitId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        self.write_module_item_visibility(f, item_id)?;
        write!(
            f,
            "{} {}",
            syntax_kind_to_text(SyntaxKind::TerminalTrait),
            item_id.name(f.db.upcast())
        )?;

        let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .trait_generic_params(*self)
            .unwrap();
        let generic_params_formatted = HirFormatter::format_generic_params(generic_params, f);
        write!(f, "{}", generic_params_formatted)?;
        Ok(())
    }
}

impl HirDisplay for TraitConstantId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        write!(
            f,
            "{} {}{} {}{}",
            syntax_kind_to_text(SyntaxKind::TerminalConst),
            item_id.name(f.db.upcast()),
            syntax_kind_to_text(SyntaxKind::TerminalColon),
            extract_and_format(
                &<dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
                    .trait_constant_type(*self)
                    .unwrap()
                    .format(f.db.upcast())
            ),
            syntax_kind_to_text(SyntaxKind::TerminalSemicolon),
        )?;
        Ok(())
    }
}

impl HirDisplay for ImplDefId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        self.write_module_item_visibility(f, item_id)?;

        let trait_id = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .impl_def_trait(*self)
            .unwrap();
        let concrete_trait_id = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .impl_def_concrete_trait(*self)
            .unwrap();
        let intern = concrete_trait_id
            .lookup_intern(<dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db));
        let path = HirFormatter::format_intern_path(
            intern
                .trait_id
                .parent_module(f.db.upcast())
                .owning_crate(f.db.upcast())
                .name(f.db.upcast()),
            trait_id.name(f.db.upcast()),
            trait_id.full_path(f.db.upcast()),
        );
        write!(
            f,
            "{} {} {} {}",
            syntax_kind_to_text(SyntaxKind::TerminalImpl),
            item_id.name(f.db.upcast()),
            syntax_kind_to_text(SyntaxKind::TerminalOf),
            path
        )?;

        let formated_generic_args =
            HirFormatter::format_generic_args(intern.generic_args.clone(), f);
        write!(f, "{}", formated_generic_args)?;
        Ok(())
    }
}

impl SyntacticWriter for ImplAliasId {}

impl HirDisplay for ImplAliasId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        self.write_module_item_visibility(f, item_id)?;
        write!(
            f,
            "{} {} {} ",
            syntax_kind_to_text(SyntaxKind::TerminalImpl),
            item_id.name(f.db.upcast()),
            syntax_kind_to_text(SyntaxKind::TerminalEq),
        )?;
        Self::write_syntactic_evaluation(f, item_id)?;
        Ok(())
    }
}

impl SyntacticWriter for ModuleTypeAliasId {}

impl HirDisplay for ModuleTypeAliasId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        self.write_module_item_visibility(f, item_id)?;
        write!(
            f,
            "{} {} {} ",
            syntax_kind_to_text(SyntaxKind::TerminalImpl),
            item_id.name(f.db.upcast()),
            syntax_kind_to_text(SyntaxKind::TerminalEq),
        )?;
        Self::write_syntactic_evaluation(f, item_id)?;
        Ok(())
    }
}

impl HirDisplay for TraitTypeId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        write!(
            f,
            "{} {}{}",
            syntax_kind_to_text(SyntaxKind::TerminalType),
            item_id.name(f.db.upcast()),
            syntax_kind_to_text(SyntaxKind::TerminalSemicolon),
        )?;
        Ok(())
    }
}

impl HirDisplay for ImplTypeDefId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        let resolved_type = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .impl_type_def_resolved_type(*self)
            .unwrap();
        write!(
            f,
            "{} {} {} {};",
            syntax_kind_to_text(SyntaxKind::TerminalType),
            item_id.name(f.db.upcast()),
            syntax_kind_to_text(SyntaxKind::TerminalEq),
            extract_and_format(&resolved_type.format(f.db.upcast())),
        )?;
        Ok(())
    }
}

impl HirDisplay for ExternTypeId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        self.write_module_item_visibility(f, item_id)?;

        write!(
            f,
            "{} {} {}",
            syntax_kind_to_text(SyntaxKind::TerminalExtern),
            syntax_kind_to_text(SyntaxKind::TerminalType),
            item_id.name(f.db.upcast()),
        )?;
        let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .extern_type_declaration_generic_params(*self)
            .unwrap();
        if !generic_params.is_empty() {
            let mut count = generic_params.len();
            f.write_str(&syntax_kind_to_text(SyntaxKind::TerminalLT))?;
            generic_params.iter().for_each(|param| {
                if count == 1 {
                    write!(
                        f,
                        "{}{}",
                        param.id().name(f.db.upcast()).unwrap(),
                        syntax_kind_to_text(SyntaxKind::TerminalGT),
                    )
                    .unwrap();
                } else {
                    write!(
                        f,
                        "{}{} ",
                        param.id().name(f.db.upcast()).unwrap(),
                        syntax_kind_to_text(SyntaxKind::TerminalColon),
                    )
                    .unwrap();
                    count -= 1;
                }
            })
        };
        Ok(())
    }
}

impl FunctionSignatureWriter for ExternFunctionId {}

impl HirDisplay for ExternFunctionId {
    fn hir_fmt(
        &self,
        f: &mut HirFormatter<'_>,
        item_id: DocumentableItemId,
    ) -> Result<(), fmt::Error> {
        self.write_module_item_visibility(f, item_id)?;
        let signature = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .extern_function_signature(*self)
            .unwrap();
        let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .extern_function_declaration_generic_params(*self)
            .unwrap();
        let generics_formatted = HirFormatter::format_generic_params(generic_params, f);
        Self::write_function_signature(
            self,
            f,
            signature.clone(),
            item_id,
            format!(
                "{} {}",
                syntax_kind_to_text(SyntaxKind::TerminalExtern),
                syntax_kind_to_text(SyntaxKind::TerminalFunction),
            ),
            generics_formatted,
        )?;
        if !signature.implicits.is_empty() {
            write!(f, " implicits{}", syntax_kind_to_text(SyntaxKind::TerminalLParen))?;

            let mut count = signature.implicits.len();
            signature.implicits.iter().for_each(|type_id| {
                write!(
                    f,
                    "{}{}",
                    type_id.format(f.db.upcast()),
                    if count == 1 {
                        syntax_kind_to_text(SyntaxKind::TerminalLParen)
                    } else {
                        format!("{} ", syntax_kind_to_text(SyntaxKind::TerminalComma),)
                    }
                )
                .unwrap();
                count -= 1;
            })
        }
        if !signature.panicable {
            write!(f, " {}", syntax_kind_to_text(SyntaxKind::TerminalNoPanic),)?;
        };
        write!(f, "{}", syntax_kind_to_text(SyntaxKind::TerminalSemicolon),)?;
        Ok(())
    }
}

impl<'a> HirFormatter<'a> {
    fn format_intern_path(
        intern_name: SmolStr,
        trait_name: SmolStr,
        trait_full_path: String,
    ) -> String {
        if format!(
            "{}{}{}",
            intern_name,
            syntax_kind_to_text(SyntaxKind::TerminalColonColon),
            trait_name
        ) == trait_full_path
        {
            String::from(trait_name)
        } else {
            trait_full_path
        }
    }

    fn format_generic_params(
        generic_params: Vec<GenericParam>,
        f: &mut HirFormatter<'_>,
    ) -> String {
        if !generic_params.is_empty() {
            let generics_formatted = generic_params
                .iter()
                .map(|param| match param {
                    GenericParam::Type(param_type) => {
                        param_type.id.format(f.db.upcast()).to_string()
                    }
                    GenericParam::Const(param_const) => {
                        format!(
                            "{} {} {}{}",
                            syntax_kind_to_text(SyntaxKind::TerminalConst),
                            syntax_kind_to_text(SyntaxKind::TerminalColon),
                            param_const.id.format(f.db.upcast()),
                            extract_and_format(&param_const.ty.format(f.db.upcast()))
                        )
                    }
                    GenericParam::Impl(param_impl) => {
                        format!("{}", param_impl.id.format(f.db.upcast()),)
                    }
                    GenericParam::NegImpl(_) => String::from(MISSING),
                })
                .collect::<Vec<String>>()
                .join(&format!("{} ", syntax_kind_to_text(SyntaxKind::TerminalComma),));
            format!(
                "{}{}{}",
                syntax_kind_to_text(SyntaxKind::TerminalLT),
                generics_formatted,
                syntax_kind_to_text(SyntaxKind::TerminalGT)
            )
        } else {
            String::new()
        }
    }

    fn format_generic_args(
        generic_args: Vec<GenericArgumentId>,
        f: &mut HirFormatter<'_>,
    ) -> String {
        let mut buf = String::new();
        let mut count = generic_args.len();

        generic_args.iter().for_each(|arg| {
            let gt = extract_and_format(&arg.format(f.db.upcast()));
            write!(
                buf,
                "{}{}{}",
                if count == generic_args.len() { "<" } else { "" },
                gt,
                if count == 1 { ">;" } else { ";" }
            )
            .unwrap();
            count -= 1;
        });
        buf
    }

    fn write_struct_attributes_syntax(
        attributes: Vec<Attribute>,
        f: &mut HirFormatter<'_>,
    ) -> Result<(), fmt::Error> {
        attributes.iter().for_each(|a| {
            let syntax_node = a.stable_ptr.lookup(f.db.upcast()).as_syntax_node();
            let children =
                <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db).get_children(syntax_node);
            for child in children.iter() {
                let to_text = child.clone().get_text_without_all_comment_trivia(f.db.upcast());
                let cleaned_text = to_text.replace("\n", "").replace(" ", "");
                write!(f.buf, "{}", cleaned_text).unwrap();
            }
            write!(f.buf, "{}", syntax_kind_to_text(SyntaxKind::TokenNewline)).unwrap();
        });
        Ok(())
    }
}

pub fn get_syntactic_visibility(semantic_visibility: &Visibility) -> &str {
    match semantic_visibility {
        Visibility::Public => "pub ",
        Visibility::PublicInCrate => "pub(crate) ",
        Visibility::Private => "",
    }
}

fn syntax_kind_to_text(kind: SyntaxKind) -> String {
    let s = match kind {
        SyntaxKind::TerminalExtern => "extern",
        SyntaxKind::TerminalType => "type",
        SyntaxKind::TerminalFunction => "fn",
        SyntaxKind::TerminalTrait => "trait",
        SyntaxKind::TerminalImpl => "impl",
        SyntaxKind::TerminalOf => "of",
        SyntaxKind::TerminalNoPanic => "nopanic",
        SyntaxKind::TerminalStruct => "struct",
        SyntaxKind::TerminalEnum => "enum",
        SyntaxKind::TerminalConst => "const",
        SyntaxKind::TerminalColon => ":",
        SyntaxKind::TerminalColonColon => "::",
        SyntaxKind::TerminalComma => ",",
        SyntaxKind::TerminalEq => "=",
        SyntaxKind::TerminalLT => "<",
        SyntaxKind::TerminalGT => ">",
        SyntaxKind::TerminalSemicolon => ";",
        SyntaxKind::TerminalLBrace => "{",
        SyntaxKind::TerminalRBrace => "}",
        SyntaxKind::TerminalLBrack => "[",
        SyntaxKind::TerminalRBrack => "]",
        SyntaxKind::TerminalLParen => "(",
        SyntaxKind::TerminalRParen => ")",
        SyntaxKind::TerminalArrow => "->",
        SyntaxKind::TokenNewline => "\n",
        SyntaxKind::TokenSingleLineInnerComment => "//",
        _ => "",
    };
    s.to_string()
}

pub fn extract_and_format(input: &str) -> String {
    fn inner(input: &str) -> String {
        input
            .split(',')
            .map(|part| {
                let mut parts = part.split("::").filter(|s| !s.is_empty()).collect::<Vec<_>>();

                if parts.len() >= 2 && parts.last().unwrap().contains("<") {
                    let last = parts.pop().unwrap();
                    let generic_parts = last
                        .split::<&[_]>(&['<', '>', ':'])
                        .filter(|s| !s.is_empty())
                        .collect::<Vec<_>>();
                    if generic_parts.len() >= 2 {
                        let l = generic_parts.len();
                        parts.push(&generic_parts[l - 2]);
                        format!("{}<{}>", parts.join("::"), generic_parts[l - 1])
                    } else {
                        last.to_owned()
                    }
                } else {
                    parts.pop().unwrap_or(part).trim().to_owned()
                }
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    let mut result = String::new();
    let mut temp = String::new();
    let mut nest_level = 0;

    for c in input.chars() {
        match c {
            '(' | '[' | '<' => {
                if nest_level == 0 && !temp.is_empty() {
                    result.push_str(&inner(&temp));
                    temp.clear();
                }
                nest_level += 1;
                result.push(c);
            }
            ')' | ']' | '>' => {
                if nest_level == 1 && !temp.is_empty() {
                    result.push_str(&inner(&temp));
                    temp.clear();
                }
                nest_level -= 1;
                result.push(c);
            }
            ',' if nest_level > 0 => {
                if !temp.is_empty() {
                    result.push_str(&inner(&temp));
                    temp.clear();
                }
                result.push(c);
                result.push(' ');
            }
            ',' => temp.push(c),
            _ => temp.push(c),
        }
    }

    if !temp.is_empty() {
        result.push_str(&inner(&temp));
    }

    result
}
