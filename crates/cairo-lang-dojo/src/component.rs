use std::collections::HashMap;

use std::fs::File;
use std::io::{Read, Write};
use std::ops::Add;
use std::path::PathBuf;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::patcher::{ModifiedNode, PatchBuilder, RewriteNode};
use cairo_lang_semantic::plugin::DynDiagnosticMapper;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use indoc::formatdoc;
use smol_str::SmolStr;

use cairo_lang_filesystem::detect::detect_corelib;





use crate::plugin::DiagnosticRemapper;

pub struct Component {
    pub name: SmolStr,
    pub rewrite_nodes: Vec<RewriteNode>,
    pub diagnostics: Vec<PluginDiagnostic>,
}

pub struct ComponentImplementation {
    name:String,
    storage_impl: String,
    serde_impl: String,
    component_struct:String,
}

impl Component {
    pub fn from_module_body(db: &dyn SyntaxGroup, name: SmolStr, body: ast::ModuleBody) -> Self {
        let diagnostics = vec![];
        let rewrite_nodes: Vec<RewriteNode> = vec![];
        let mut component = Component { rewrite_nodes, name, diagnostics };

        let mut matched_struct = false;
        for item in body.items(db).elements(db) {
            match &item {
                ast::Item::Struct(item_struct) => {
                    if matched_struct {
                        component.diagnostics.push(PluginDiagnostic {
                            message: "Only one struct per module is supported.".to_string(),
                            stable_ptr: item_struct.stable_ptr().untyped(),
                        });
                        continue;
                    }
                    component.handle_component_struct(db, item_struct.clone());
                    matched_struct = true;
                }
                ast::Item::FreeFunction(item_function) => {
                    component.handle_component_functions(db, item_function.clone());
                }
                _ => (),
            }
        }

        component
    }

    pub fn extend_corelib(db: &dyn SyntaxGroup, name:SmolStr, body: ast::ModuleBody) {
        let diagnostics = vec![];
        let rewrite_nodes: Vec<RewriteNode> = vec![];
        let mut component = Component { rewrite_nodes, name, diagnostics };

        let mut matched_struct = false;
        for item in body.items(db).elements(db) {
            match &item {
                ast::Item::Struct(item_struct) => {
                    if matched_struct {
                        component.diagnostics.push(PluginDiagnostic {
                            message: "Only one struct per module is supported.".to_string(),
                            stable_ptr: item_struct.stable_ptr().untyped(),
                        });
                        continue;
                    }

                    let component_impl = component.handle_component_struct(db, item_struct.clone());
                    let corelib_path = detect_corelib().unwrap();
                    modify_starknet_lib(&corelib_path, component_impl.storage_impl);
                    modify_serde_lib(&corelib_path, component_impl.serde_impl);
                    export_dojo_struct(&corelib_path, component_impl.name,component_impl.component_struct);

                    matched_struct = true;
                }
                _ => (),
            }
        }
    }

    pub fn result(self, db: &dyn SyntaxGroup) -> PluginResult {
        let name = self.name;
        let mut builder = PatchBuilder::new(db);
        builder.add_modified(RewriteNode::interpolate_patched(
            &formatdoc!(
                "
                #[contract]
                mod {name} {{
                    $body$
                }}
                ",
            ),
            HashMap::from([(
                "body".to_string(),
                RewriteNode::Modified(ModifiedNode { children: self.rewrite_nodes }),
            )]),
        ));

        PluginResult {
            code: Some(PluginGeneratedFile {
                name,
                content: builder.code,
                aux_data: DynGeneratedFileAuxData::new(DynDiagnosticMapper::new(
                    DiagnosticRemapper { patches: builder.patches },
                )),
            }),
            diagnostics: self.diagnostics,
            remove_original_item: true,
        }
    }

    fn handle_component_struct(&mut self, db: &dyn SyntaxGroup, struct_ast: ast::ItemStruct) -> ComponentImplementation {
        // We remove the struct definition from the contract file as it will be defined in corelib/dojo.cairo

        // Generate Serde / StorageAccess implementations
        let mut serialize = vec![];
        let mut deserialize = vec![];
        let mut read = vec![];
        let mut write = vec![];
        struct_ast.members(db).elements(db).iter().enumerate().for_each(|(i, member)| {
            let key = member.name(db).as_syntax_node().get_text_without_trivia(db);
            let offset = i;
            serialize.push(
                format!("Serde::<felt>::serialize(ref serialized, input.{key});"),
            );

            deserialize.push(
                format!("{key}: Serde::<felt>::deserialize(ref serialized)?,"),
            );

            read.push(
                format!("{key}: storage_read_syscall(
                    address_domain, storage_address_from_base_and_offset(base, {offset}_u8)
                )?,"));

            let final_token = if i != struct_ast.members(db).elements(db).len() - 1 { ";" } else { "" };
            write.push(
                format!("storage_write_syscall(
                    address_domain, storage_address_from_base_and_offset(base, {offset}_u8), \
                 value.{key}){final_token}"));
        });

        let serialize_string = serialize.join("\n");
        let deserialize_string = deserialize.join("\n");
        let read_string = read.join("\n");
        let write_string = write.join("\n");

        let type_name = struct_ast.name(db).as_syntax_node().get_text_without_trivia(db);

        let serde_impl = format!(
"\n
impl {type_name}Serde of Serde::<{type_name}> {{
        fn serialize(ref serialized: Array::<felt>, input: {type_name}) {{
            {serialize_string}
        }}
        fn deserialize(ref serialized: Array::<felt>) -> Option::<{type_name}> {{
            Option::Some(
                {type_name} {{
                    {deserialize_string}
                }}
            )
        }}
    }}
            ");

        let storage_impl =
            format!("\n
    impl StorageAccess{type_name} of StorageAccess::<{type_name}> {{
        fn read(address_domain: felt, base: StorageBaseAddress) -> SyscallResult::<{type_name}> {{
            Result::Ok(
                {type_name} {{
                    {read_string}
                }}
            )
        }}
        fn write(
            address_domain: felt, base: StorageBaseAddress, value: {type_name}
        ) -> SyscallResult::<()> {{
            {write_string}
        }}
    }}
            ");

        // Generate the component struct as a string
        let struct_definition = struct_ast.as_syntax_node().get_text_without_trivia(db);
        let full_struct = "\n#[derive(Copy, Drop)]\n".to_string().add(&struct_definition);

        self.rewrite_nodes.push(RewriteNode::interpolate_patched(
            "
                    struct Storage {
                        world_address: felt,
                        state: Map::<felt, $type_name$>,
                    }
    
                    // Initialize $type_name$Component.
                    #[external]
                    fn initialize(world_addr: felt) {
                        let world = world_address::read();
                        assert(world == 0, '$type_name$Component: Already initialized.');
                        world_address::write(world_addr);
                    }
    
                    // Set the state of an entity.
                    #[external]
                    fn set(entity_id: felt, value: $type_name$) {
                        state::write(entity_id, value);
                    }
    
                    // Get the state of an entity.
                    #[view]
                    fn get(entity_id: felt) -> $type_name$ {
                        return state::read(entity_id);
                    }
                    ",
            HashMap::from([(
                "type_name".to_string(),
                RewriteNode::Trimmed(struct_ast.name(db).as_syntax_node()),
            )]),
        ));

        ComponentImplementation {
            name: struct_ast.name(db).as_syntax_node().get_text_without_trivia(db),
            storage_impl,
            serde_impl,
            component_struct:full_struct,
        }
    }

    fn handle_component_functions(&mut self, db: &dyn SyntaxGroup, func: ast::FunctionWithBody) {
        let declaration = func.declaration(db);

        let mut func_declaration = RewriteNode::from_ast(&declaration);
        func_declaration
            .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
            .modify_child(db, ast::FunctionSignature::INDEX_PARAMETERS)
            .modify(db)
            .children
            .splice(0..1, vec![RewriteNode::Text("entity_id: felt".to_string())]);

        self.rewrite_nodes.push(RewriteNode::interpolate_patched(
            "
                            #[view]
                            $func_decl$ {
                                let self = state::read(entity_id);
                                $body$
                            }
                            ",
            HashMap::from([
                ("func_decl".to_string(), func_declaration),
                (
                    "body".to_string(),
                    RewriteNode::Trimmed(func.body(db).statements(db).as_syntax_node()),
                ),
            ]),
        ))
    }
}


fn modify_starknet_lib(path: &PathBuf, implementation: String) {
    let path_starknet = path.join("starknet.cairo");
    let mut file = File::open(path_starknet.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents.push_str(&implementation);
    // write to file
    let mut file = File::create(path_starknet).unwrap();
    file.write_all(contents.as_bytes()).unwrap();
}

fn modify_serde_lib(path: &PathBuf, implementation: String) {
    // open file
    // path_starknet as path+/starknet.cairo
    let path_serde = path.join("serde.cairo");
    let mut file = File::open(path_serde.clone()).unwrap();
    // update file contents by appending node
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents.push_str(&implementation);
    // write to file
    let mut file = File::create(path_serde).unwrap();
    file.write_all(contents.as_bytes()).unwrap();
}

fn export_dojo_struct(path: &PathBuf, component_name: String, component_struct: String) {
    let path_dojo = path.join("dojo.cairo");
    let mut file = File::open(path_dojo.clone()).unwrap();
    // update file contents by appending node
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents.push_str(&component_struct);
    // write to file
    let mut file = File::create(path_dojo).unwrap();
    file.write_all(contents.as_bytes()).unwrap();

    let path_lib = path.join("lib.cairo");
    let mut file = File::open(path_lib.clone()).unwrap();
    // update file contents by appending node
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents.push_str(format!("\nuse dojo::{component_name};").as_str());
    // write to file
    let mut file = File::create(path_lib).unwrap();
    file.write_all(contents.as_bytes()).unwrap();

}