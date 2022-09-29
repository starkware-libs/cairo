use super::error::{ExtensionError, SpecializationError};
use crate::ids::{ConcreteTypeId, FunctionId, GenericLibFuncId, GenericTypeId};
use crate::program::{Function, FunctionSignature, GenericArg};

/// Trait for the specialization of libfunc signatures.
pub trait SignatureSpecializationContext {
    /// Returns concrete type id given a generic type and the generic arguments.
    fn get_concrete_type(
        &self,
        id: GenericTypeId,
        generic_args: &[GenericArg],
    ) -> Option<ConcreteTypeId>;

    /// Returns the function's signature object associated with the given [FunctionId].
    fn get_function_signature(&self, function_id: &FunctionId) -> Option<FunctionSignature>;
}

/// Trait for the specialization of full libfuncs.
pub trait SpecializationContext: SignatureSpecializationContext {
    /// Upcasting to the SignatureSpecializationContext, since trait upcasting is still
    /// experimental.
    fn upcast(&self) -> &dyn SignatureSpecializationContext;

    /// Returns the function object associated with the given [FunctionId].
    fn get_function(&self, function_id: &FunctionId) -> Option<Function>;
}

/// Extension of SignatureSpecializationContext for easier usage.
pub trait SignatureSpecializationContextEx: SignatureSpecializationContext {
    /// Wraps `get_concrete_type` with a result object.
    fn get_concrete_type_as_result(
        &self,
        id: GenericTypeId,
        generic_args: &[GenericArg],
    ) -> Result<ConcreteTypeId, SpecializationError>;
    /// Returns the concrete id of a generic-type-id wrapping the type of a concrete-type-id.
    fn get_wrapped_concrete_type(
        &self,
        id: GenericTypeId,
        wrapped: ConcreteTypeId,
    ) -> Result<ConcreteTypeId, SpecializationError>;
    /// Wraps `get_function_signature` with a result object.
    fn get_function_signature_as_result(
        &self,
        function_id: &FunctionId,
    ) -> Result<FunctionSignature, SpecializationError>;
}
impl<TSignatureSpecializationContext: SignatureSpecializationContext + ?Sized>
    SignatureSpecializationContextEx for TSignatureSpecializationContext
{
    fn get_concrete_type_as_result(
        &self,
        id: GenericTypeId,
        generic_args: &[GenericArg],
    ) -> Result<ConcreteTypeId, SpecializationError> {
        <Self as SignatureSpecializationContext>::get_concrete_type(self, id.clone(), generic_args)
            .ok_or_else(|| SpecializationError::TypeWasNotDeclared(id, generic_args.to_vec()))
    }
    fn get_wrapped_concrete_type(
        &self,
        id: GenericTypeId,
        wrapped: ConcreteTypeId,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        self.get_concrete_type_as_result(id, &[GenericArg::Type(wrapped)])
    }
    fn get_function_signature_as_result(
        &self,
        function_id: &FunctionId,
    ) -> Result<FunctionSignature, SpecializationError> {
        self.get_function_signature(function_id)
            .ok_or_else(|| SpecializationError::MissingFunction(function_id.clone()))
    }
}

/// Extension of SpecializationContext for easier usage.
pub trait SpecializationContextEx: SpecializationContext {
    /// Wraps `get_function` with a result object.
    fn get_function_as_result(
        &self,
        function_id: &FunctionId,
    ) -> Result<Function, SpecializationError>;
}
impl<TSpecializationContext: SpecializationContext + ?Sized> SpecializationContextEx
    for TSpecializationContext
{
    fn get_function_as_result(
        &self,
        function_id: &FunctionId,
    ) -> Result<Function, SpecializationError> {
        self.get_function(function_id)
            .ok_or_else(|| SpecializationError::MissingFunction(function_id.clone()))
    }
}

/// Trait for implementing a libfunc specialization generator.
pub trait GenericLibFunc: Sized {
    type Concrete: ConcreteLibFunc;

    /// Instantiates the libfunc by id.
    fn by_id(id: &GenericLibFuncId) -> Option<Self>;

    /// Creates the specialization of the libfunc's signature with the template arguments.
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError>;

    /// Creates the specialization with the template arguments.
    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError>;
}

/// Trait for introducing helper methods on GenericLibFunc.
pub trait GenericLibFuncEx: GenericLibFunc {
    fn specialize_signature_by_id(
        context: &dyn SignatureSpecializationContext,
        libfunc_id: &GenericLibFuncId,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, ExtensionError>;

    fn specialize_by_id(
        context: &dyn SpecializationContext,
        libfunc_id: &GenericLibFuncId,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, ExtensionError>;
}
impl<TGenericLibFunc: GenericLibFunc> GenericLibFuncEx for TGenericLibFunc {
    fn specialize_signature_by_id(
        context: &dyn SignatureSpecializationContext,
        libfunc_id: &GenericLibFuncId,
        generic_args: &[GenericArg],
    ) -> Result<LibFuncSignature, ExtensionError> {
        Self::by_id(libfunc_id)
            .ok_or_else(move || ExtensionError::LibFuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .specialize_signature(context, generic_args)
            .map_err(move |error| ExtensionError::LibFuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error,
            })
    }

    fn specialize_by_id(
        context: &dyn SpecializationContext,
        libfunc_id: &GenericLibFuncId,
        generic_args: &[GenericArg],
    ) -> Result<TGenericLibFunc::Concrete, ExtensionError> {
        Self::by_id(libfunc_id)
            .ok_or_else(move || ExtensionError::LibFuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error: SpecializationError::UnsupportedId,
            })?
            .specialize(context, generic_args)
            .map_err(move |error| ExtensionError::LibFuncSpecialization {
                libfunc_id: libfunc_id.clone(),
                error,
            })
    }
}

/// Trait for implementing a specialization generator with with a simple id.
pub trait NamedLibFunc: Default {
    type Concrete: ConcreteLibFunc;
    const ID: GenericLibFuncId;

    /// Creates the specialization of the libfunc's signature with the template arguments.
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError>;

    /// Creates the specialization with the template arguments.
    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError>;
}
impl<TNamedLibFunc: NamedLibFunc> GenericLibFunc for TNamedLibFunc {
    type Concrete = <Self as NamedLibFunc>::Concrete;

    fn by_id(id: &GenericLibFuncId) -> Option<Self> {
        if &Self::ID == id { Some(Self::default()) } else { None }
    }

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        <Self as NamedLibFunc>::specialize_signature(self, context, args)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        <Self as NamedLibFunc>::specialize(self, context, args)
    }
}

/// Trait for implementing a specialization generator with no generic arguments.
pub trait NoGenericArgsGenericLibFunc: Default {
    type Concrete: ConcreteLibFunc;
    const ID: GenericLibFuncId;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError>;

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
    ) -> Result<Self::Concrete, SpecializationError>;
}
impl<T: NoGenericArgsGenericLibFunc> NamedLibFunc for T {
    type Concrete = <Self as NoGenericArgsGenericLibFunc>::Concrete;
    const ID: GenericLibFuncId = <Self as NoGenericArgsGenericLibFunc>::ID;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        if args.is_empty() {
            self.specialize_signature(context)
        } else {
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        if args.is_empty() {
            self.specialize(context)
        } else {
            Err(SpecializationError::WrongNumberOfGenericArgs)
        }
    }
}

/// Information regarding the reference created as an output of a library function.
/// For example, whether the reference is equal to one of the parameters (as in the dup() function),
/// or whether it's newly allocated local variable.
pub enum OutputVarReferenceInfo {
    /// The output value is exactly the same as one of the parameters.
    SameAsParam { param_idx: usize },
    /// The output was allocated as a temporary variable. Contains the index of the temporary
    /// variable in case that more than one temporary variable was allocated by the libfunc.
    NewTempVar { idx: usize },
    /// The output was allocated as a local variable.
    NewLocalVar,
    /// The output is the result of a computation. For example `[ap] + [fp]`,
    /// `[ap + 1] * [fp - 3]`.
    Deferred,
    /// The output is a constant.
    Const,
}

/// Contains information regarding an output variable in a single branch.
pub struct OutputVarInfo {
    pub ty: ConcreteTypeId,
    pub ref_info: OutputVarReferenceInfo,
}

/// Contains information on the variables returned in a single libfunc branch
/// for all the output variables in an output branch.
///
/// See [OutputVarInfo].
pub struct OutputBranchInfo {
    pub vars: Vec<OutputVarInfo>,
}

/// Trait for a specialized library function.
pub trait ConcreteLibFunc {
    /// The input types for calling the library function.
    fn input_types(&self) -> &[ConcreteTypeId];
    /// The output types and other information returning from a library function per branch.
    fn output_info(&self) -> &[OutputBranchInfo];
    /// The index of the fallthrough branch of the library function if any.
    fn fallthrough(&self) -> Option<usize>;

    /// Returns the output types returning from a library function per branch.
    fn output_types(&self) -> Vec<Vec<ConcreteTypeId>> {
        self.output_info()
            .iter()
            .map(|branch_info| {
                branch_info.vars.iter().map(|var_info| var_info.ty.clone()).collect()
            })
            .collect()
    }
}

/// Represents the signature of a library function.
pub struct LibFuncSignature {
    /// The input types for calling a library function.
    pub input_types: Vec<ConcreteTypeId>,
    /// The output types and other information for the return values of a library function per
    /// branch.
    pub output_info: Vec<OutputBranchInfo>,
    /// The index of the fallthrough branch of the library function if any.
    pub fallthrough: Option<usize>,
}
impl LibFuncSignature {
    /// Creates a non branch signature.
    pub fn new_non_branch(
        input_types: Vec<ConcreteTypeId>,
        output_info: Vec<OutputVarInfo>,
    ) -> Self {
        Self {
            input_types,
            output_info: vec![OutputBranchInfo { vars: output_info }],
            fallthrough: Some(0),
        }
    }
}

/// Trait for implementing a ConcreteLibFunc that returns a reference to the full signature of the
/// library function.
pub trait SignatureBasedConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature;
}

/// Struct providing a ConcreteLibFunc only with a signature - should not be implemented for
/// concrete libfuncs that require any extra data.
pub struct SignatureOnlyConcreteLibFunc {
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for SignatureOnlyConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

impl<TSignatureBasedConcreteLibFunc: SignatureBasedConcreteLibFunc> ConcreteLibFunc
    for TSignatureBasedConcreteLibFunc
{
    fn input_types(&self) -> &[ConcreteTypeId] {
        &self.signature().input_types
    }
    fn output_info(&self) -> &[OutputBranchInfo] {
        &self.signature().output_info
    }
    fn fallthrough(&self) -> Option<usize> {
        self.signature().fallthrough
    }
}

/// Forms a concrete library function type from an enum of library calls.
/// The new enum implements ConcreteLibFunc.
/// All the variant types must also implement ConcreteLibFunc.
/// Usage example:
/// ```ignore
/// define_concrete_libfunc_hierarchy! {
///     pub enum MyLibFunc {
///       LF0(LibFunc0),
///       LF1(LibFunc1),
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
        impl $crate::extensions::ConcreteLibFunc for $name {
            $crate::extensions::lib_func::concrete_method_impl! {
                fn input_types(&self) -> &[$crate::ids::ConcreteTypeId] {
                    $($variant_name => $variant,)*
                }
            }
            $crate::extensions::lib_func::concrete_method_impl!{
                fn output_info(&self) -> &[$crate::extensions::lib_func::OutputBranchInfo] {
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
                $(Self::$variant_name(value) =>
                    <$variant as $crate::extensions::ConcreteLibFunc>::$method_name(value)),*
            }
        }
    }
}
pub(crate) use concrete_method_impl;

/// Forms a libfunc type from an enum of libfuncs.
/// The new enum implements GenericLibFunc.
/// All the variant types must also implement GenericLibFunc.
/// Usage example:
/// ```ignore
/// define_libfunc_hierarchy! {
///     pub enum MyLibFunc {
///       LF0(LibFunc0),
///       LF1(LibFunc1),
///     }, MyLibFuncConcrete
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

        impl $crate::extensions::GenericLibFunc for $name {
            type Concrete = $concrete_name;
            fn by_id(id: &$crate::ids::GenericLibFuncId) -> Option<Self> {
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
                    $crate::extensions::lib_func::LibFuncSignature,
                    $crate::extensions::SpecializationError
                >{
                match self {
                    $(
                        Self::$variant_name(value) => {
                            <$variant as $crate::extensions::GenericLibFunc>::specialize_signature(
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
                                <$variant as $crate::extensions::GenericLibFunc>::specialize(
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
                $($variant_name (<$variant as $crate::extensions::GenericLibFunc> ::Concrete),)*
            }
        }
    }
}
