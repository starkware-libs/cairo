//! Keys of inference-free semantic objects that are consistent across builds: unlike their ids,
//! they depend only on the definitions they are built from and the values of their constants.

use cairo_lang_defs::ids::{FunctionTitleId, LanguageElementId};
use cairo_lang_defs::stable_order::{ElementKey, element_key, node_key};
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use num_bigint::BigInt;
use salsa::Database;

use crate::items::constant::{ConstValue, ConstValueId};
use crate::items::enm::ConcreteVariant;
use crate::items::functions::{
    ConcreteFunctionWithBodyId, GenericFunctionId, GenericFunctionWithBodyId,
};
use crate::items::imp::{
    GeneratedImplItems, ImplId, ImplLongId, NegativeImplId, NegativeImplLongId, UninferredImpl,
};
use crate::items::trt::ConcreteTraitId;
use crate::types::{TypeId, TypeLongId};
use crate::{FunctionId, GenericArgumentId};

/// A build-independent key of a semantic object, as the sequence of atoms written by
/// [StableOrd::write_key]; distinct inference-free objects have distinct keys.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::SalsaValue)]
pub struct StableKey<'db>(Vec<Atom<'db>>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::SalsaValue)]
enum Atom<'db> {
    /// A definition or a syntax node, by its position.
    Element(ElementKey<'db>),
    /// An enum variant, distinguished from the other variants by the line it is keyed at.
    Variant(u32),
    /// The length of a sequence, an inference variable index, or a flag.
    Index(usize),
    /// A constant value.
    Value(BigInt),
}

impl<'db> StableKey<'db> {
    /// Writes an enum variant, identified by the `line!()` of the call, so a `match` keys its arms
    /// distinctly.
    pub fn variant(&mut self, line: u32) -> &mut Self {
        self.0.push(Atom::Variant(line));
        self
    }
    /// Writes an index: the length of a sequence, an inference variable index, or a flag.
    pub fn index(&mut self, index: usize) -> &mut Self {
        self.0.push(Atom::Index(index));
        self
    }
    /// Writes a constant value.
    pub fn value(&mut self, value: &BigInt) -> &mut Self {
        self.0.push(Atom::Value(value.clone()));
        self
    }
    /// Writes a definition.
    pub fn element(
        &mut self,
        db: &'db dyn Database,
        element: &impl LanguageElementId<'db>,
    ) -> &mut Self {
        self.0.push(Atom::Element(element_key(db, element)));
        self
    }
    /// Writes a syntax node.
    pub fn node(&mut self, db: &'db dyn Database, stable_ptr: SyntaxStablePtrId<'db>) -> &mut Self {
        self.0.push(Atom::Element(node_key(db, stable_ptr)));
        self
    }
    /// Writes the key of a semantic object.
    pub fn item(&mut self, db: &'db dyn Database, item: &impl StableOrd<'db>) -> &mut Self {
        item.write_key(db, self);
        self
    }
    /// Writes a sequence, prefixed by its length.
    pub fn items<T: StableOrd<'db>>(&mut self, db: &'db dyn Database, items: &[T]) -> &mut Self {
        self.index(items.len());
        for item in items {
            item.write_key(db, self);
        }
        self
    }
    /// Writes a generic definition applied to generic arguments.
    pub fn concrete(
        &mut self,
        db: &'db dyn Database,
        element: &impl LanguageElementId<'db>,
        args: &[GenericArgumentId<'db>],
    ) -> &mut Self {
        self.element(db, element).items(db, args)
    }
}

/// A semantic object with a build-independent key.
pub trait StableOrd<'db> {
    /// Appends the key of the object to `key`.
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>);

    /// The key of the object.
    fn stable_key(&self, db: &'db dyn Database) -> StableKey<'db> {
        let mut key = StableKey::default();
        self.write_key(db, &mut key);
        key
    }
}

/// `Ok` values are distinct from errors, which are all equal.
impl<'db, T: StableOrd<'db>, E> StableOrd<'db> for Result<T, E> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        match self {
            Ok(value) => key.variant(line!()).item(db, value),
            Err(_) => key.variant(line!()),
        };
    }
}

impl<'db> StableOrd<'db> for GenericArgumentId<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        match self {
            Self::Type(ty) => key.variant(line!()).item(db, ty),
            Self::Constant(value) => key.variant(line!()).item(db, value),
            Self::Impl(imp) => key.variant(line!()).item(db, imp),
            Self::NegImpl(imp) => key.variant(line!()).item(db, imp),
        };
    }
}

impl<'db> StableOrd<'db> for TypeId<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use TypeLongId::*;
        match self.long(db) {
            Concrete(ty) => {
                key.variant(line!()).concrete(db, &ty.generic_type(db), &ty.generic_args(db))
            }
            Tuple(types) => key.variant(line!()).items(db, types),
            Snapshot(ty) => key.variant(line!()).item(db, ty),
            GenericParameter(param) => key.variant(line!()).element(db, param),
            Var(var) => key.variant(line!()).index(var.id.0),
            NumericLiteral(var) => key.variant(line!()).index(var.id.0),
            Coupon(function) => key.variant(line!()).item(db, function),
            FixedSizeArray { type_id, size } => {
                key.variant(line!()).item(db, type_id).item(db, size)
            }
            ImplType(ty) => key.variant(line!()).item(db, &ty.impl_id()).element(db, &ty.ty()),
            Closure(closure) => key
                .variant(line!())
                .node(db, closure.params_location.stable_ptr())
                .item(db, &closure.parent_function),
            Missing(_) => key.variant(line!()),
        };
    }
}

impl<'db> StableOrd<'db> for ConstValueId<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use ConstValue::*;
        match self.long(db) {
            Int(value, ty) => key.variant(line!()).value(value).item(db, ty),
            Struct(members, ty) => key.variant(line!()).item(db, ty).items(db, members),
            Enum(variant, payload) => key.variant(line!()).item(db, variant).item(db, payload),
            NonZero(value) => key.variant(line!()).item(db, value),
            Generic(param) => key.variant(line!()).element(db, param),
            ImplConstant(constant) => key
                .variant(line!())
                .item(db, &constant.impl_id())
                .element(db, &constant.trait_constant_id()),
            Var(var, ty) => key.variant(line!()).index(var.id.0).item(db, ty),
            Missing(_) => key.variant(line!()),
        };
    }
}

impl<'db> StableOrd<'db> for ConcreteVariant<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        let concrete_enum = self.concrete_enum_id.long(db);
        key.concrete(db, &concrete_enum.enum_id, &concrete_enum.generic_args).element(db, &self.id);
    }
}

impl<'db> StableOrd<'db> for ImplId<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use ImplLongId::*;
        match self.long(db) {
            Concrete(imp) => {
                let imp = imp.long(db);
                key.variant(line!()).concrete(db, &imp.impl_def_id, &imp.generic_args)
            }
            GenericParameter(param) => key.variant(line!()).element(db, param),
            ImplVar(var) => key.variant(line!()).index(var.long(db).id.0),
            ImplImpl(imp) => {
                key.variant(line!()).item(db, &imp.impl_id()).element(db, &imp.trait_impl_id())
            }
            SelfImpl(concrete_trait) => key.variant(line!()).item(db, concrete_trait),
            GeneratedImpl(imp) => {
                let imp = imp.long(db);
                key.variant(line!()).item(db, &imp.concrete_trait).item(db, &imp.impl_items)
            }
        };
    }
}

impl<'db> StableOrd<'db> for NegativeImplId<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use NegativeImplLongId::*;
        match self.long(db) {
            Solved(imp) => key.variant(line!()).item(db, imp),
            GenericParameter(param) => key.variant(line!()).element(db, param),
            NegativeImplVar(var) => key.variant(line!()).index(var.long(db).id.0),
        };
    }
}

/// Keyed as a sequence of trait type and value pairs.
impl<'db> StableOrd<'db> for GeneratedImplItems<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        key.index(self.0.len());
        for (trait_type, ty) in self.0.iter() {
            key.element(db, trait_type).item(db, ty);
        }
    }
}

impl<'db> StableOrd<'db> for ConcreteTraitId<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        let concrete_trait = self.long(db);
        key.concrete(db, &concrete_trait.trait_id, &concrete_trait.generic_args);
    }
}

/// Keyed by the definition of the function (the trait function for impl functions), then by kind.
impl<'db> StableOrd<'db> for FunctionId<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use GenericFunctionId::*;
        let function = &self.long(db).function;
        match &function.generic_function {
            Free(id) => key.element(db, &FunctionTitleId::Free(*id)).variant(line!()),
            Extern(id) => key.element(db, &FunctionTitleId::Extern(*id)).variant(line!()),
            Impl(id) => key
                .element(db, &FunctionTitleId::Trait(id.function))
                .variant(line!())
                .item(db, &id.impl_id),
        };
        key.items(db, &function.generic_args);
    }
}

/// Keyed like `FunctionId`, by the impl function's definition where it has one.
impl<'db> StableOrd<'db> for ConcreteFunctionWithBodyId<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use GenericFunctionWithBodyId::*;
        let function = self.long(db);
        key.element(db, &function.function_with_body_id(db));
        match &function.generic_function {
            Free(_) => key.variant(line!()),
            Impl(imp) => {
                let concrete_impl = imp.concrete_impl_id.long(db);
                key.variant(line!()).concrete(
                    db,
                    &concrete_impl.impl_def_id,
                    &concrete_impl.generic_args,
                )
            }
            Trait(trait_function) => {
                key.variant(line!()).item(db, &trait_function.concrete_trait(db))
            }
        };
        key.items(db, &function.generic_args);
    }
}

impl<'db> StableOrd<'db> for UninferredImpl<'db> {
    fn write_key(&self, db: &'db dyn Database, key: &mut StableKey<'db>) {
        use UninferredImpl::*;
        match self {
            Def(imp) => key.variant(line!()).element(db, imp),
            ImplAlias(alias) => key.variant(line!()).element(db, alias),
            GenericParam(param) => key.variant(line!()).element(db, param),
            ImplImpl(imp) => {
                key.variant(line!()).item(db, &imp.impl_id()).element(db, &imp.trait_impl_id())
            }
            GeneratedImpl(imp) => {
                let imp = imp.long(db);
                key.variant(line!()).item(db, &imp.concrete_trait).item(db, &imp.impl_items)
            }
        };
    }
}
