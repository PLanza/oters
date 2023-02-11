#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Spanned<T> {
    pub span: (usize, usize),
    pub term: Box<T>,
}

pub type SpPExpr = Spanned<super::ast::PExpr>;
pub type SpTypeExpr = Spanned<super::ast::TypeExpr>;
pub type SpPattern = Spanned<super::ast::Pattern>;
