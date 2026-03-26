#![allow(dead_code)]

use std::{collections::BTreeSet, ops::BitOr};

use smallvec::SmallVec;

use crate::{
    Assembly, Exec, HasStack, ImplPrimitive, Node, Primitive, SigNode, UiuaError, Value,
    invert::InversionError,
};

pub struct TypeEnv<'a> {
    asm: &'a Assembly,
    stack: Vec<TypeSet>,
}

impl<'a> HasStack for TypeEnv<'a> {
    type Item = TypeSet;
    type Error = TypeError;
    fn stack(&self) -> &Vec<Self::Item> {
        &self.stack
    }
    fn stack_mut(&mut self) -> &mut Vec<Self::Item> {
        &mut self.stack
    }
    fn underflow_error<A: uiua_stack::StackArg>(&self, arg: A) -> Self::Error {
        TypeError::Underflow(arg.underflow_message())
    }
}

#[derive(Debug)]
pub enum TypeError {
    Unsupported,
    Underflow(String),
    Inversion(InversionError),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum TypeSet {
    #[default]
    Any,
    Set(BTreeSet<TypeVal>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeVal {
    Val(Value),
    Type(Type),
}

impl From<TypeVal> for TypeSet {
    fn from(tv: TypeVal) -> Self {
        TypeSet::Set([tv].into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Type {
    pub scalar: ScalarSet,
    pub shape: DynShape,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DynShape(pub SmallVec<[DimSet; 1]>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Scalar {
    Num,
    Char,
    Box(Option<Box<Type>>),
    Complex,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum ScalarSet {
    #[default]
    Any,
    Set(BTreeSet<Scalar>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DimSet {
    Static(usize),
    Set(BTreeSet<usize>),
    Dyn,
}

impl BitOr for ScalarSet {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        use ScalarSet::*;
        match (self, rhs) {
            (Any, _) | (_, Any) => Any,
            (Set(mut a), Set(b)) => {
                a.extend(b);
                Set(a)
            }
        }
    }
}

impl BitOr for DimSet {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        use DimSet::*;
        match (self, rhs) {
            (Dyn, _) | (_, Dyn) => Dyn,
            (Set(mut a), Set(b)) => {
                a.extend(b);
                Set(a)
            }
            (Set(mut set), Static(d)) | (Static(d), Set(mut set)) => {
                set.insert(d);
                Set(set)
            }
            (Static(a), Static(b)) => Set([a, b].into()),
        }
    }
}

impl DimSet {
    pub fn superset_of(&self, other: &Self) -> bool {
        use DimSet::*;
        match (self, other) {
            (Dyn, _) => true,
            (_, Dyn) => false,
            (Set(a), Set(b)) => a.is_superset(b),
            (Set(a), Static(b)) => a.contains(b),
            (Static(a), Set(b)) => b.len() == 1 && b.contains(a),
            (Static(a), Static(b)) => a == b,
        }
    }
    pub fn subset_of(&self, other: &Self) -> bool {
        other.superset_of(self)
    }
}

impl BitOr for TypeVal {
    type Output = Result<Self, [Self; 2]>;
    fn bitor(self, rhs: Self) -> Self::Output {
        let a = match self {
            TypeVal::Val(value) => todo!(),
            TypeVal::Type(_) => todo!(),
        };
        todo!()
    }
}

impl From<&Value> for Type {
    fn from(value: &Value) -> Self {
        let scalar = match value {
            Value::Num(_) | Value::Byte(_) => Scalar::Num,
            Value::Char(_) => Scalar::Char,
            Value::Complex(_) => Scalar::Complex,
            Value::Box(arr) => {
                // Scalar::Box(
                //     arr.data
                //         .iter()
                //         .map(Type::from)
                //         .reduce(BitOr::bitor)
                //         .map(Box::new),
                // );
                todo!()
            }
        };
        todo!()
    }
}

impl BitOr for TypeSet {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        use TypeSet::*;
        match (self, rhs) {
            (Any, _) | (_, Any) => Any,
            (Set(a), Set(b)) => {
                let mut new = BTreeSet::new();
                for a in a {
                    for b in &b {
                        match a.clone() | b.clone() {
                            Ok(ty) => {
                                new.insert(ty);
                            }
                            Err(tys) => new.extend(tys),
                        }
                    }
                }
                Set(new)
            }
        }
    }
}

impl<'a> Exec<SigNode> for TypeEnv<'a> {
    type Output = ();
    fn exec(&mut self, f: SigNode) -> Result<Self::Output, Self::Error> {
        self.node(&f.node)
    }
}

impl<'a> Exec<&SigNode> for TypeEnv<'a> {
    type Output = ();
    fn exec(&mut self, f: &SigNode) -> Result<Self::Output, Self::Error> {
        self.node(&f.node)
    }
}

impl<'a> TypeEnv<'a> {
    fn node(&mut self, node: &Node) -> Result<(), TypeError> {
        use {ImplPrimitive::*, Node::*, Primitive::*};
        match node {
            Run(nodes) => {
                for node in nodes {
                    self.node(node)?;
                }
            }
            Call(f, _) => self.node(&self.asm[f])?,
            Push(val) => self.push(TypeVal::Val(val.clone())),
            Prim(prim, _) => match prim {
                Identity => _ = self.require_height(1)?,
                Pop => {
                    self.pop(1)?;
                }
                Dup => self.dup()?,
                Flip => self.flip()?,
                _ => return Err(TypeError::Unsupported),
            },
            ImplPrim(prim, _) => match prim {
                Over => self.over()?,
                _ => return Err(TypeError::Unsupported),
            },
            Mod(prim, ops, _) => match prim {
                Fork => self.fork(ops.clone())?,
                Bracket => self.bracket(ops.clone())?,
                Dip => self.dip(monad(ops))?,
                _ => return Err(TypeError::Unsupported),
            },
            CustomInverse(cust, _) => match &cust.normal {
                Ok(node) => self.exec(node)?,
                Err(e) => return Err(TypeError::Inversion(e.clone())),
            },
            _ => return Err(TypeError::Unsupported),
        }
        Ok(())
    }
}

fn monad(ops: &[SigNode]) -> &SigNode {
    let [f] = get_ops(ops);
    f
}

fn get_ops<const N: usize>(ops: &[SigNode]) -> &[SigNode; N] {
    ops.try_into().unwrap()
}
