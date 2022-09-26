use smol_str::SmolStr;

use super::ast::{self, TokenIdentifierGreen};
use super::db::SyntaxGroup;
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
