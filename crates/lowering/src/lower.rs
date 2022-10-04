use defs::ids::{FreeFunctionId, LanguageElementId};
use diagnostics::Diagnostics;
use id_arena::Arena;
use semantic::db::SemanticGroup;
use semantic::FreeFunctionDefinition;

use crate::diagnostic::{LoweringDiagnostic, LoweringDiagnostics};
use crate::objects::{Block, BlockEnd, BlockId, Variable};

// TODO(spapini): Remove.
#[allow(dead_code)]
pub struct Lowerer<'db> {
    db: &'db dyn SemanticGroup,
    function_def: &'db FreeFunctionDefinition,
    diagnostics: LoweringDiagnostics,
    variables: Arena<Variable>,
    blocks: Arena<Block>,
}
pub struct Lowered {
    pub diagnostics: Diagnostics<LoweringDiagnostic>,
    pub root: BlockId,
    pub variables: Arena<Variable>,
    pub blocks: Arena<Block>,
}

impl<'db> Lowerer<'db> {
    pub fn lower(db: &dyn SemanticGroup, free_function_id: FreeFunctionId) -> Option<Lowered> {
        let function_def = db.free_function_definition(free_function_id)?;
        let mut lowerer = Lowerer {
            db,
            function_def: &*function_def,
            diagnostics: LoweringDiagnostics::new(free_function_id.module(db.upcast())),
            variables: Arena::default(),
            blocks: Arena::default(),
        };
        let root = lowerer.lower_block(function_def.body);
        let Lowerer { diagnostics, variables, blocks, .. } = lowerer;
        Some(Lowered { diagnostics: diagnostics.build(), root, variables, blocks })
    }

    fn lower_block(&mut self, _block: id_arena::Id<semantic::Expr>) -> id_arena::Id<Block> {
        let block = Block {
            inputs: vec![],
            statements: vec![],
            drops: vec![],
            end: BlockEnd::Callsite(vec![]),
        };
        self.blocks.alloc(block)
    }
}
