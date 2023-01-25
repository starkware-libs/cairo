use super::args_as_single_type;
use super::error::{ExtensionError, SpecializationError};
use super::type_specialization_context::TypeSpecializationContext;
use crate::ids::{id_from_string, ConcreteTypeId, FunctionId, GenericLibfuncId, GenericTypeId};
use crate::program::{Function, FunctionSignature, GenericArg};

/// Trait for the specialization of libfunc signatures.
pub trait SignatureSpecializationContext: TypeSpecializationContext {
    /// Returns concrete type id given a generic type and the generic arguments.
    fn try_get_concrete_type(
        &self,
        id: GenericTypeId,
        generic_args: &[GenericArg],
    ) -> Option<ConcreteTypeId>;

    /// Wraps [Self::try_get_concrete_type] with a result object.
    fn get_concrete_type(
        &self,
        id: GenericTypeId,
        generic_args: &[GenericArg],
    ) -> Result<ConcreteTypeId, SpecializationError> {
        self.try_get_concrete_type(id.clone(), generic_args)
            .ok_or_else(|| SpecializationError::TypeWasNotDeclared(id, generic_args.to_vec()))
    }

    /// Returns the function's signature object associated with the given [FunctionId].
    fn try_get_function_signature(&self, function_id: &FunctionId) -> Option<FunctionSignature>;

    /// Wraps [Self::try_get_function_signature] with a result object.
    fn get_function_signature(
        &self,
        function_id: &FunctionId,
    ) -> Result<FunctionSignature, SpecializationError> {
        self.try_get_function_signature(function_id)
            .ok_or_else(|| SpecializationError::MissingFunction(function_id.clone()))
    }

    /// Returns the ap-change of the given function.
    fn try_get_function_ap_change(&self, function_id: &FunctionId) -> Option<SierraApChange>;

    /// Wraps [Self::try_get_function_ap_change] with a result object.
    fn get_function_ap_change(
        &self,
        function_id: &FunctionId,
    ) -> Result<SierraApChange, SpecializationError> {
        self.try_get_function_ap_change(function_id)
            .ok_or_else(|| SpecializationError::MissingFunction(function_id.clone()))
    }

    /// Returns the concrete id of `T<S>` given generic type T and concrete type S.
    fn get_wrapped_concrete_type(
        &self,
        id: GenericTypeId,
        wrapped: ConcreteTypeId,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        self.get_concrete_type(id, &[GenericArg::Type(wrapped)])
    }

    /// Upcasting to the [TypeSpecializationContext], since trait upcasting is still experimental.
    fn as_type_specialization_context(&self) -> &dyn TypeSpecializationContext;
}

/// Trait for the specialization of full libfuncs.
pub trait SpecializationContext: SignatureSpecializationContext {
    /// Upcasting to the [SignatureSpecializationContext], since trait upcasting is still
    /// experimental.
    fn upcast(&self) -> &dyn SignatureSpecializationContext;

    /// Returns the function object associated with the given [FunctionId].
    fn try_get_function(&self, function_id: &FunctionId) -> Option<Function>;

    /// Wraps [Self::try_get_function] with a result object.
    fn get_function(&self, function_id: &FunctionId) -> Result<Function, SpecializationError> {
        self.try_get_function(function_id)
            .ok_or_else(|| SpecializationError::MissingFunction(function_id.clone()))
    }
}

/// Trait for implementing a libfunc specialization generator.
pub trait GenericLibfunc: Sized {
    type Concrete: ConcreteLibfunc;

    /// Instantiates the libfunc by id.
    fn by_id(id: &GenericLibfuncId) -> Option<Self>;

    /// Creates the specialization of the libfunc's signature with the template arguments.
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError>;

    /// Creates the specialization with the template arguments.
    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError>;
}

/// Trait for introducing helper methods on [GenericLibfunc].
pub trait GenericLibfuncEx: GenericLibfunc {
    fn specialize_signature_by_id(
        context: &dyn SignatureSpecializationContext,
        libfunc_id: &GenericLibfuncId,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, ExtensionError>;

    fn specialize_by_id(
        context: &dyn SpecializationContext,
        libfunc_id: &GenericLibfuncId,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, ExtensionError>;
}
impl<TGenericLibfunc: GenericLibfunc> GenericLibfuncEx for TGenericLibfunc {
    fn specialize_signature_by_id(
        context: &dyn SignatureSpecializationContext,
        libfunc_id: &GenericLibfuncId,
        generic_args: &[GenericArg],
    ) -> Result<LibfuncSignature, ExtensionError> {
        Self::by_id(libfunc_id)
            .ok_or_else(move || ExtensionError::LibfuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .specialize_signature(context, generic_args)
            .map_err(move |error| ExtensionError::LibfuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error,
            })
    }

    fn specialize_by_id(
        context: &dyn SpecializationContext,
        libfunc_id: &GenericLibfuncId,
        generic_args: &[GenericArg],
    ) -> Result<TGenericLibfunc::Concrete, ExtensionError> {
        Self::by_id(libfunc_id)
            .ok_or_else(move || ExtensionError::LibfuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .specialize(context, generic_args)
            .map_err(move |error| ExtensionError::LibfuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error,
            })
    }
}

/// Trait for implementing a specialization generator with a simple id.
pub trait NamedLibfunc: Default {
    type Concrete: ConcreteLibfunc;
    const STR_ID: &'static str;

    /// Creates the specialization of the libfunc's signature with the template arguments.
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError>;

    /// Creates the specialization with the template arguments.
    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError>;
}
impl<TNamedLibfunc: NamedLibfunc> GenericLibfunc for TNamedLibfunc {
    type Concrete = <Self as NamedLibfunc>::Concrete;

    fn by_id(id: &GenericLibfuncId) -> Option<Self> {
        if id_from_string(Self::STR_ID) == id.id { Some(Self::default()) } else { None }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        self.specialize_signature(context, args)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        self.specialize(context, args)
    }
}

/// Trait for implementing a specialization generator not holding anything more than a signature.
pub trait SignatureOnlyGenericLibfunc: Default {
    const STR_ID: &'static str;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError>;
}

impl<T: SignatureOnlyGenericLibfunc> NamedLibfunc for T {
    type Concrete = SignatureOnlyConcreteLibfunc;
    const STR_ID: &'static str = <Self as SignatureOnlyGenericLibfunc>::STR_ID;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        self.specialize_signature(context, args)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(SignatureOnlyConcreteLibfunc {
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

/// Trait for implementing a specialization generator expecting a single generic param type, and
/// creating a concrete libfunc containing that type as well.
pub trait SignatureAndTypeGenericLibfunc: Default {
    const STR_ID: &'static str;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<LibfuncSignature, SpecializationError>;
}

/// Wrapper to prevent implementation collisions for [NamedLibfunc].
#[derive(Default)]
pub struct WrapSignatureAndTypeGenericLibfunc<T: SignatureAndTypeGenericLibfunc>(T);

impl<T: SignatureAndTypeGenericLibfunc> NamedLibfunc for WrapSignatureAndTypeGenericLibfunc<T> {
    type Concrete = SignatureAndTypeConcreteLibfunc;
    const STR_ID: &'static str = <T as SignatureAndTypeGenericLibfunc>::STR_ID;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        self.0.specialize_signature(context, args_as_single_type(args)?)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = args_as_single_type(args)?;
        Ok(SignatureAndTypeConcreteLibfunc {
            ty: ty.clone(),
            signature: self.0.specialize_signature(context.upcast(), ty)?,
        })
    }
}

/// Trait for implementing a specialization generator with no generic arguments.
pub trait NoGenericArgsGenericLibfunc: Default {
    const STR_ID: &'static str;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError>;
}
impl<T: NoGenericArgsGenericLibfunc> SignatureOnlyGenericLibfunc for T {
    const STR_ID: &'static str = <Self as NoGenericArgsGenericLibfunc>::STR_ID;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        if args.is_empty() {
            self.specialize_signature(context)
        } else {
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }
}

/// Information regarding a parameter of the libfunc.
pub struct ParamSignature {
    /// The type of the parameter.
    pub ty: ConcreteTypeId,
    /// Whether the libfunc argument can be an expression of the form `[ap/fp + i] + [ap/fp + j]`.
    /// For example, `store_temp()` and `store_local()`.
    pub allow_deferred: bool,
    /// Whether the libfunc argument can be an expression of the form `[ap + i] + const`.
    pub allow_add_const: bool,
    /// Whether the libfunc argument can be a constant.
    pub allow_const: bool,
}
impl ParamSignature {
    /// Returns a [ParamSignature] with default attributes.
    pub fn new(ty: ConcreteTypeId) -> Self {
        Self { ty, allow_add_const: false, allow_deferred: false, allow_const: false }
    }
}
impl From<ConcreteTypeId> for ParamSignature {
    fn from(ty: ConcreteTypeId) -> Self {
        Self::new(ty)
    }
}

/// Information regarding the reference created as an output of a library function.
/// For example, whether the reference is equal to one of the parameters (as in the dup() function),
/// or whether it's newly allocated local variable.
#[derive(Debug)]
pub enum OutputVarReferenceInfo {
    /// The output value is exactly the same as one of the parameters.
    SameAsParam { param_idx: usize },
    /// The output value is a part of one of the parameters.
    /// For example, it may be the first element of a struct.
    ///
    /// Information, such as whether the parameter was a temporary or local variable, will be
    /// copied to the output variable.
    PartialParam { param_idx: usize },
    /// The output was allocated as a temporary variable.
    /// For the outputs that are at the top of the stack (contiguously), contains the index of the
    /// temporary variable in the stack (0 is the lowest variable).
    NewTempVar { idx: Option<usize> },
    /// The output was allocated as a local variable.
    NewLocalVar,
    /// The output is the result of a computation. For example `[ap] + [fp]`,
    /// `[ap + 1] * [fp - 3]`, `[ap] + 3`, `7`.
    Deferred(DeferredOutputKind),
}

/// The type of a deferred output.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DeferredOutputKind {
    /// The output is a constant. For example, `7`.
    Const,
    /// The output is the addition of a constant to one of the parameters. For example, `x + 3`.
    AddConst { param_idx: usize },
    /// The output is not one of the above (e.g., `[ap] + [fp]`, `[ap + 1] * [fp - 3]`,
    /// `[ap] * 3`).
    Generic,
}

/// Contains information regarding an output variable in a single branch.
#[derive(Debug)]
pub struct OutputVarInfo {
    pub ty: ConcreteTypeId,
    pub ref_info: OutputVarReferenceInfo,
}

/// Contains information on the variables returned in a single libfunc branch
/// for all the output variables in an output branch.
///
/// See [OutputVarInfo].
#[derive(Debug)]
pub struct BranchSignature {
    /// Information about the new variables created in the branch.
    pub vars: Vec<OutputVarInfo>,
    /// Information about the change in the `ap` register in the branch.
    pub ap_change: SierraApChange,
}

/// Describes the effect on the `ap` register in a given libfunc branch.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SierraApChange {
    /// The libfunc changes `ap` in an unknown way.
    Unknown,
    /// The libfunc changes `ap` in a known (during compilation) way.
    Known {
        /// `true` if all the new stack cells created by the libfunc are its output
        /// variables (as described in [OutputVarReferenceInfo::NewTempVar] in
        /// [`BranchSignature::vars`]).
        new_vars_only: bool,
    },
    /// The lib func is `branch_align`.
    /// The `ap` change is know during compilation.
    BranchAlign,
    /// Indicates that the value of ApChange was not assigned properly yet. Behaves as `Unknown`.
    /// This will be removed, once all places using it are fixed.
    // TODO(lior): Remove this value once it is no longer used.
    NotImplemented,
}
/// Trait for a specialized library function.
pub trait ConcreteLibfunc {
    /// The parameter types and other information for the parameters for calling a library
    /// function.
    fn param_signatures(&self) -> &[ParamSignature];
    /// The output types and other information returning from a library function per branch.
    fn branch_signatures(&self) -> &[BranchSignature];
    /// The index of the fallthrough branch of the library function if any.
    fn fallthrough(&self) -> Option<usize>;

    /// Returns the output types returning from a library function per branch.
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>> {
        self.branch_signatures()
            .iter()
            .map(|branch_info| {
                branch_info.vars.iter().map(|var_info| var_info.ty.clone()).collect()
            })
            .collect()
    }
}

/// Represents the signature of a library function.
pub struct LibfuncSignature {
    /// The parameter types and other information for the parameters for calling a library
    /// function.
    pub param_signatures: Vec<ParamSignature>,
    /// The output types and other information for the return values of a library function per
    /// branch.
    pub branch_signatures: Vec<BranchSignature>,
    /// The index of the fallthrough branch of the library function if any.
    pub fallthrough: Option<usize>,
}
impl LibfuncSignature {
    /// Creates a non branch signature.
    pub fn new_non_branch(
        input_types: Vec<ConcreteTypeId>,
        output_info: Vec<OutputVarInfo>,
        ap_change: SierraApChange,
    ) -> Self {
        Self::new_non_branch_ex(
            input_types.into_iter().map(ParamSignature::new).collect(),
            output_info,
            ap_change,
        )
    }

    /// Same as [LibfuncSignature::new_non_branch], except that more complicated [ParamSignature]
    /// are supported.
    pub fn new_non_branch_ex(
        param_signatures: Vec<ParamSignature>,
        output_info: Vec<OutputVarInfo>,
        ap_change: SierraApChange,
    ) -> LibfuncSignature {
        Self {
            param_signatures,
            branch_signatures: vec![BranchSignature { vars: output_info, ap_change }],
            fallthrough: Some(0),
        }
    }
}

/// Trait for implementing a [ConcreteLibfunc] that returns a reference to the full signature of the
/// library function.
pub trait SignatureBasedConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature;
}

impl<TSignatureBasedConcreteLibfunc: SignatureBasedConcreteLibfunc> ConcreteLibfunc
    for TSignatureBasedConcreteLibfunc
{
    fn param_signatures(&self) -> &[ParamSignature] {
        &self.signature().param_signatures
    }
    fn branch_signatures(&self) -> &[BranchSignature] {
        &self.signature().branch_signatures
    }
    fn fallthrough(&self) -> Option<usize> {
        self.signature().fallthrough
    }
}

/// Struct providing a [ConcreteLibfunc] only with a signature and a type.
pub struct SignatureAndTypeConcreteLibfunc {
    pub ty: ConcreteTypeId,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for SignatureAndTypeConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Struct providing a [ConcreteLibfunc] only with a signature - should not be implemented for
/// concrete libfuncs that require any extra data.
pub struct SignatureOnlyConcreteLibfunc {
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for SignatureOnlyConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Forms a concrete library function type from an enum of library calls.
/// The new enum implements [ConcreteLibfunc].
/// All the variant types must also implement [ConcreteLibfunc].
/// Usage example:
/// ```ignore
/// define_concrete_libfunc_hierarchy! {
///     pub enum MyLibfunc {
///       LF0(Libfunc0),
///       LF1(Libfunc1),
///     }
/// }
/// ```
#[macro_export]
macro_rules! define_concrete_libfunc_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty),)* }) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant),)*
        }
        impl $crate::extensions::ConcreteLibfunc for $name {
            $crate::extensions::lib_func::concrete_method_impl! {
                fn param_signatures(&self) -> &[$crate::extensions::lib_func::ParamSignature] {
                    $($variant_name => $variant,)*
                }
            }
            $crate::extensions::lib_func::concrete_method_impl!{
                fn branch_signatures(&self) -> &[$crate::extensions::lib_func::BranchSignature] {
                    $($variant_name => $variant,)*
                }
            }
            $crate::extensions::lib_func::concrete_method_impl!{
                fn fallthrough(&self) -> Option<usize> {
                    $($variant_name => $variant,)*
                }
            }
        }
    }
}

/// Implements a method for an enum of library calls by recursively calling the enum option existing
/// implementation.
macro_rules! concrete_method_impl {
    (fn $method_name:ident(&self $(,$var_name:ident : $var:ty)*) -> $ret_type:ty {
        $($variant_name:ident => $variant:ty,)*
    }) => {
        fn $method_name(&self $(,$var_name:ident : $var:ty)*) -> $ret_type {
            match self {
                $(Self::$variant_name(value) => value.$method_name()),*
            }
        }
    }
}
pub(crate) use concrete_method_impl;

/// Forms a libfunc type from an enum of libfuncs.
/// The new enum implements [GenericLibfunc].
/// All the variant types must also implement [GenericLibfunc].
/// Usage example:
/// ```ignore
/// define_libfunc_hierarchy! {
///     pub enum MyLibfunc {
///       LF0(Libfunc0),
///       LF1(Libfunc1),
///     }, MyLibfuncConcrete
/// }
/// ```
#[macro_export]
macro_rules! define_libfunc_hierarchy {
    (pub enum $name:ident { $($variant_name:ident ($variant:ty),)* },
    $concrete_name:ident) => {
        #[allow(clippy::enum_variant_names)]
        pub enum $name {
            $($variant_name ($variant)),*
        }

        impl $crate::extensions::GenericLibfunc for $name {
            type Concrete = $concrete_name;
            fn by_id(id: &$crate::ids::GenericLibfuncId) -> Option<Self> {
                $(
                    if let Some(res) = <$variant>::by_id(id){
                        return Some(Self::$variant_name(res));
                    }
                )*
                None
            }
            fn specialize_signature(
                    &self,
                    context: &dyn $crate::extensions::lib_func::SignatureSpecializationContext,
                    args: &[$crate::program::GenericArg],
            ) -> Result<
                    $crate::extensions::lib_func::LibfuncSignature,
                    $crate::extensions::SpecializationError
                >{
                match self {
                    $(
                        Self::$variant_name(value) => {
                            <$variant as $crate::extensions::GenericLibfunc>::specialize_signature(
                                value, context, args,
                            )
                        }
                    ),*
                }
            }
            fn specialize(
                    &self,
                    context: &dyn $crate::extensions::lib_func::SpecializationContext,
                    args: &[$crate::program::GenericArg],
            ) -> Result<Self::Concrete, $crate::extensions::SpecializationError>{
                match self {
                    $(
                        Self::$variant_name(value) => {
                            Ok(Self::Concrete::$variant_name(
                                <$variant as $crate::extensions::GenericLibfunc>::specialize(
                                    value, context, args,
                                )?
                                .into(),
                            ))
                        }
                    ),*
                }
            }
        }

        $crate::define_concrete_libfunc_hierarchy! {
            pub enum $concrete_name {
                $($variant_name (<$variant as $crate::extensions::GenericLibfunc> ::Concrete),)*
            }
        }
    }
}
