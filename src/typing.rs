use crate::{intern::InternedString, parser::Expr};
use std::{
    collections::{BTreeSet, HashMap},
    fmt::{Debug, Display},
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Var(TyVar),
    Lambda(Box<Self>, Box<Self>),
}

impl Type {
    fn lower(&self, vars: &mut HashMap<TyVar, TyVar>) -> Self {
        match self.clone() {
            Self::Int => Self::Int,
            Self::Bool => Self::Bool,
            Self::Var(name) => {
                if let Some(n) = vars.get(&name) {
                    Self::Var(*n)
                } else {
                    let n = vars.len();
                    vars.insert(name, TyVar(n));
                    Self::Var(TyVar(n))
                }
            }
            Self::Lambda(param, body) => {
                Self::Lambda(Box::new(param.lower(vars)), Box::new(body.lower(vars)))
            }
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Var(n) => write!(f, "{:?}", n),
            Self::Lambda(param, body) => match param.as_ref() {
                Self::Lambda(_, _) => write!(f, "({:?}) -> {:?}", param, body),
                _ => write!(f, "{:?} -> {:?}", param, body),
            },
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Int => write!(f, "Int"),
            Self::Bool => write!(f, "Bool"),
            Self::Var(n) => write!(f, "{}", n),
            Self::Lambda(param, body) => match param.as_ref() {
                Self::Lambda(_, _) => write!(f, "({}) -> {}", param, body),
                _ => write!(f, "{} -> {}", param, body),
            },
        }
    }
}

type Substitution = HashMap<TyVar, Type>;

#[derive(Debug, Clone, PartialEq)]
struct Scheme {
    vars: Vec<TyVar>,
    ty: Type,
}
impl Scheme {
    fn new(vars: Vec<TyVar>, ty: Type) -> Self {
        Self { vars, ty }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVar(pub usize);

static COUNTER: AtomicUsize = AtomicUsize::new(0);

impl TyVar {
    fn fresh() -> Self {
        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

impl Debug for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TyVar({})", self)
    }
}

const ALPHABET: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z',
];

impl Display for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 < ALPHABET.len() {
            write!(f, "'{}", ALPHABET[self.0])
        } else {
            write!(f, "t{}", self.0)
        }
    }
}

fn apply_subst(subst: Substitution, ty: Type) -> Type {
    match ty {
        Type::Int | Type::Bool => ty.clone(),
        Type::Var(n) => subst.get(&n).cloned().unwrap_or(ty.clone()),
        Type::Lambda(param, body) => Type::Lambda(
            Box::new(apply_subst(subst.clone(), *param)),
            Box::new(apply_subst(subst.clone(), *body)),
        ),
    }
}

fn apply_subst_scheme(mut subst: Substitution, scheme: Scheme) -> Scheme {
    for var in &scheme.vars {
        subst.remove(var);
    }
    Scheme {
        vars: scheme.vars.clone(),
        ty: apply_subst(subst.clone(), scheme.ty.clone()),
    }
}

// left-biased union
fn map_union(m1: Substitution, m2: Substitution) -> Substitution {
    m2.into_iter().chain(m1.into_iter()).collect()
}

fn compose_subst(s1: Substitution, s2: Substitution) -> Substitution {
    map_union(
        s2.iter()
            .map(|(var, ty)| (var.clone(), apply_subst(s1.clone(), ty.clone())))
            .collect(),
        s1,
    )
}

fn free_vars(ty: Type) -> BTreeSet<TyVar> {
    match ty {
        Type::Var(n) => vec![n.clone()].into_iter().collect(),
        Type::Lambda(param, body) => free_vars(*param.clone())
            .union(&free_vars(*body.clone()))
            .cloned()
            .collect(),
        _ => BTreeSet::new(),
    }
}

fn free_vars_scheme(scheme: Scheme) -> BTreeSet<TyVar> {
    free_vars(scheme.ty.clone())
        .difference(&scheme.vars.iter().cloned().collect())
        .cloned()
        .collect()
}

fn var_bind(var: TyVar, ty: Type) -> Result<Substitution, String> {
    if ty.clone() == Type::Var(var.clone()) {
        Ok(HashMap::new())
    } else if free_vars(ty.clone()).contains(&var) {
        Err(format!("occurs check failed: {} occurs in {:?}", var, ty))
    } else {
        let mut subst = HashMap::new();
        subst.insert(var, ty.clone());
        Ok(subst)
    }
}

pub fn unify(t1: Type, t2: Type) -> Result<Substitution, String> {
    match (t1.clone(), t2.clone()) {
        (Type::Int, Type::Int) => Ok(HashMap::new()),
        (Type::Bool, Type::Bool) => Ok(HashMap::new()),
        (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
            let s1 = unify(*p1.clone(), *p2.clone())?;
            let s2 = unify(
                apply_subst(s1.clone(), *b1.clone()),
                apply_subst(s1.clone(), *b2.clone()),
            )?;
            Ok(compose_subst(s1.clone(), s2.clone()))
        }
        (Type::Var(n), t) | (t, Type::Var(n)) => var_bind(n.clone(), t.clone()),
        _ => Err(format!(
            "cannot unify {:?} and {:?}",
            t1.lower(&mut HashMap::new()),
            t2.lower(&mut HashMap::new())
        )),
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Context {
    vars: HashMap<InternedString, Scheme>,
}

fn apply_subst_ctx(subst: Substitution, ctx: Context) -> Context {
    Context {
        vars: ctx
            .vars
            .into_iter()
            .map(|(name, scheme)| (name, apply_subst_scheme(subst.clone(), scheme)))
            .collect(),
    }
}

fn free_vars_ctx(ctx: Context) -> BTreeSet<TyVar> {
    ctx.vars
        .into_iter()
        .map(|(_, scheme)| free_vars_scheme(scheme))
        .fold(BTreeSet::new(), |acc, set| {
            acc.union(&set).cloned().collect()
        })
}

fn generalize(ctx: Context, ty: Type) -> Scheme {
    Scheme {
        vars: free_vars(ty.clone())
            .difference(&free_vars_ctx(ctx))
            .cloned()
            .collect(),
        ty,
    }
}

fn instantiate(scheme: Scheme) -> Type {
    let mut subst = HashMap::new();
    for var in &scheme.vars {
        subst.insert(*var, Type::Var(TyVar::fresh()));
    }
    apply_subst(subst, scheme.ty)
}

fn infer(ctx: Context, expr: Expr) -> Result<(Substitution, Type), String> {
    match expr.clone() {
        Expr::Int(_) => Ok((HashMap::new(), Type::Int)),
        Expr::Bool(_) => Ok((HashMap::new(), Type::Bool)),
        Expr::Var(name) => match ctx.clone().vars.get(&name) {
            Some(scheme) => Ok((HashMap::new(), instantiate(scheme.clone()))),
            None => Err(format!("unbound variable: {:?}", expr)),
        },
        Expr::Lambda(name, body) => {
            let ty_binder = Type::Var(TyVar::fresh());
            let tmp_ctx = Context {
                vars: ctx
                    .clone()
                    .vars
                    .into_iter()
                    .chain(vec![(name, Scheme::new(vec![], ty_binder.clone()))].into_iter())
                    .collect(),
            };
            let (s1, t1) = infer(tmp_ctx, *body.clone())?;
            Ok((
                s1.clone(),
                Type::Lambda(
                    Box::new(apply_subst(s1.clone(), ty_binder.clone())),
                    Box::new(t1),
                ),
            ))
        }
        Expr::Apply(fun, arg) => {
            let ty_ret = Type::Var(TyVar::fresh());
            let (s1, ty_fun) = infer(ctx.clone(), *fun.clone())?;
            let (s2, ty_arg) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *arg.clone())?;
            let s3 = unify(
                apply_subst(s2.clone(), ty_fun.clone()),
                Type::Lambda(Box::new(ty_arg.clone()), Box::new(ty_ret.clone())),
            )?;
            let sf = compose_subst(s3.clone(), compose_subst(s2.clone(), s1.clone()));
            Ok((sf, apply_subst(s3, ty_ret.clone())))
        }
        Expr::Let(name, binding, body) => {
            let (s1, t1) = infer(ctx.clone(), *binding.clone())?;
            let scheme = Scheme {
                vars: vec![],
                ty: apply_subst(s1.clone(), t1.clone()),
            };
            let tmp_ctx = Context {
                vars: ctx
                    .vars
                    .clone()
                    .into_iter()
                    .chain(vec![(name.clone(), scheme)].into_iter())
                    .collect(),
            };
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), tmp_ctx), *body.clone())?;
            Ok((compose_subst(s2.clone(), s1.clone()), t2.clone()))
        }
        Expr::Add(l, r) => {
            let (s1, t1) = infer(ctx.clone(), *l.clone())?;
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
            let s3 = unify(t1, Type::Int)?;
            let s4 = unify(t2, Type::Int)?;
            let sf = compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone());
            Ok((sf, Type::Int))
        }
        Expr::Sub(l, r) => {
            let (s1, t1) = infer(ctx.clone(), *l.clone())?;
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
            let s3 = unify(t1, Type::Int)?;
            let s4 = unify(t2, Type::Int)?;
            Ok((
                compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
                Type::Int,
            ))
        }
        Expr::Mul(l, r) => {
            let (s1, t1) = infer(ctx.clone(), *l.clone())?;
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
            let s3 = unify(t1, Type::Int)?;
            let s4 = unify(t2, Type::Int)?;
            Ok((
                compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
                Type::Int,
            ))
        }
        Expr::Div(l, r) => {
            let (s1, t1) = infer(ctx.clone(), *l.clone())?;
            let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
            let s3 = unify(t1, Type::Int)?;
            let s4 = unify(t2, Type::Int)?;
            Ok((
                compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
                Type::Int,
            ))
        }
    }
}

fn type_inference(ctx: Context, expr: Expr) -> Result<Type, String> {
    let (subst, ty) = infer(ctx, expr)?;
    Ok(apply_subst(subst, ty).lower(&mut HashMap::new()))
}

fn default_ctx() -> Context {
    let mut ctx = Context {
        vars: HashMap::new(),
    };
    ctx.vars.insert(InternedString::from("id"), {
        let a = TyVar::fresh();
        Scheme::new(
            vec![a.clone()],
            Type::Lambda(
                Box::new(Type::Var(a.clone())),
                Box::new(Type::Var(a.clone())),
            ),
        )
    });
    ctx.vars.insert(InternedString::from("const"), {
        let a = TyVar::fresh();
        let b = TyVar::fresh();
        Scheme::new(
            vec![a.clone(), b.clone()],
            Type::Lambda(
                Box::new(Type::Var(a.clone())),
                Box::new(Type::Lambda(
                    Box::new(Type::Var(b.clone())),
                    Box::new(Type::Var(a.clone())),
                )),
            ),
        )
    });
    ctx
}
