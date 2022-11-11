mod tests;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Float,
    String,
    Bool,
    Tuple(Vec<Box<Type>>),
    List(Box<Type>),
    Function(Box<Type>, Box<Type>),
    Delay(Box<Type>),       
    Stable(Box<Type>),     
    Fix(String, Box<Type>),
    Var(String),
    Struct(Vec<Box<Type>>),
    Enum(Vec<Option<Box<Type>>>),
}

#[derive(Debug, PartialEq)]
enum TypeTerm {
    Tick,
    Var(String, Type)
}

#[derive(Debug, PartialEq)]
struct TypeContext{
    context: Vec<TypeTerm>,
    ticks: Vec<usize>, // Locations of ticks within the context
}

impl Type {
    pub fn is_stable(&self) -> bool {
        use self::Type::*;
        match self {
            Unit | Int | Float | String | Bool => true, // All primitive types are Stable
            Tuple(v) => v.iter().fold(true, |mut acc, t| {
                acc = acc && t.is_stable();
                acc
            }),
            List(t) => t.is_stable(),
            Function(..) => false, // Functions can have temporal values in their closure
            Delay(..) => false, // Delayed values are inherently temporal
            Stable(..) => true, // Stable values wrap any type, making it atemporal
            Fix(..) => false, // The fix point type argument is implictly a delay type
            Var(..) => false, 
            Struct(v) => v.iter().fold(true, |mut acc, t| {
                acc = acc && t.is_stable();
                acc
            }),
            Enum(v) => v.iter().fold(true, |mut acc, o| {
                let t_stable = match o {
                    Some(t) => t.is_stable(),
                    None => true,
                };
                acc = acc && t_stable;
                acc
            }),
        }
    }
}

impl TypeContext {
    pub fn stable(&self) -> Self {
        let context = self.context.iter().fold(Vec::new(), |mut context, term| {
            match term {
                TypeTerm::Tick => context,
                TypeTerm::Var(x, t) => if t.is_stable() {
                    context.push(TypeTerm::Var(x.clone(), t.clone()));
                    context
                } else {
                    context
                }
            }
        });

        TypeContext { context, ticks: Vec::with_capacity(0) }
    }
}

