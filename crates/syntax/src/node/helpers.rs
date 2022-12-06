use smol_str::SmolStr;

use super::ast::{self, Modifier, TerminalIdentifierGreen, TokenIdentifierGreen};
use super::db::SyntaxGroup;
use super::kind::SyntaxKind;
use super::Terminal;
use crate::node::green::GreenNodeDetails;

pub trait GetIdentifier {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr;
}
impl GetIdentifier for ast::ExprPathGreen {
    /// Retrieves the text of the last identifier in the path.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let children = match db.lookup_intern_green(self.0).details {
            GreenNodeDetails::Node { children, width: _ } => children,
            _ => panic!("Unexpected token"),
        };
        assert_eq!(children.len() & 1, 1, "Expected an odd number of elements in the path.");
        ast::TerminalIdentifierGreen(*children.last().unwrap()).identifier(db)
    }
}
impl GetIdentifier for ast::TerminalIdentifierGreen {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match db.lookup_intern_green(self.0).details {
            GreenNodeDetails::Token(_) => "Unexpected token".into(),
            GreenNodeDetails::Node { children, width: _ } => {
                TokenIdentifierGreen(children[1]).text(db)
            }
        }
    }
}
impl GetIdentifier for ast::ExprPath {
    /// Retrieves the identifier of the last segment of the path.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.elements(db).last().cloned().unwrap().identifier(db)
    }
}
impl GetIdentifier for ast::ParamNameGreen {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let green_node = db.lookup_intern_green(self.0);

        match green_node.details {
            GreenNodeDetails::Token(_) => "Unexpected token".into(),
            GreenNodeDetails::Node { .. } => match green_node.kind {
                SyntaxKind::TerminalIdentifier => TerminalIdentifierGreen(self.0).identifier(db),
                // All '_' params will be named with the same name...
                SyntaxKind::TerminalUnderscore => "_".into(),
                _ => "Unexpected identifier for param name".into(),
            },
        }
    }
}

// Helper trait for ast::PathSegment.
pub trait PathSegmentEx {
    fn identifier_ast(&self, db: &dyn SyntaxGroup) -> ast::TerminalIdentifier;
}
impl PathSegmentEx for ast::PathSegment {
    /// Retrieves the identifier ast of a path segment.
    fn identifier_ast(&self, db: &dyn SyntaxGroup) -> ast::TerminalIdentifier {
        match self {
            ast::PathSegment::Simple(segment) => segment.ident(db),
            ast::PathSegment::WithGenericArgs(segment) => segment.ident(db),
        }
    }
}
impl GetIdentifier for ast::PathSegment {
    /// Retrieves the text of the segment (without the generic args).
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.identifier_ast(db).text(db)
    }
}
impl GetIdentifier for ast::ParamName {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match self {
            ast::ParamName::Underscore(_) => "_".into(),
            ast::ParamName::Name(name) => name.text(db),
        }
    }
}
impl GetIdentifier for ast::Modifier {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match self {
            Modifier::Ref(r) => r.text(db),
            Modifier::Mut(m) => m.text(db),
        }
    }
}
