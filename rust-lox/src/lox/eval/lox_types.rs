use crate::lox::ast::{Declaration, Reference};
use crate::lox::types::Identifier;
use alloc::rc::Rc;
use core::fmt::Display;
use std::cell::RefCell;
use std::collections::HashMap;

pub type LoxScope = Vec<(Identifier, Rc<RefCell<LoxValue>>)>;

#[derive(PartialEq, Clone, Debug)]
pub enum LoxValue {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Object(Rc<RefCell<LoxObject>>),
    Class(Rc<RefCell<LoxClass>>),
    Function(LoxFunction),
}

impl LoxValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxValue::Nil => false,
            LoxValue::Boolean(b) => *b,
            // Everything else is truthy
            _ => true,
        }
    }
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Boolean(b) => write!(f, "{}", b),
            LoxValue::Number(n) => {
                let repr = format!("{}", n);
                if let Some(repr) = repr.strip_suffix(".0") {
                    write!(f, "{}", repr)
                } else {
                    write!(f, "{}", repr)
                }
            }
            LoxValue::String(s) => write!(f, "{:?}", s),
            LoxValue::Object(o) => write!(f, "{}", o.borrow()),
            LoxValue::Class(_c) => write!(f, "TODO: Class"),
            LoxValue::Function(_f) => write!(f, "TODO: Function"),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct LoxObject {
    pub class: Rc<RefCell<LoxClass>>,
    pub fields: HashMap<Identifier, LoxValue>,
}

impl Display for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{ ", self.class.borrow().name.id().0.0)?;
        let mut first = true;
        for (key, value) in &self.fields {
            if !first {
                write!(f, ", ")?;
            }
            first = false;

            write!(f, "{:?}: {}", key.0, value)?;
        }

        write!(f, " }}")
    }
}

impl LoxObject {
    pub fn new(class: Rc<RefCell<LoxClass>>) -> LoxObject {
        Self {
            class,
            fields: Default::default(),
        }
    }

    pub fn get(&self, name: &Identifier) -> Option<LoxValue> {
        self.fields
            .get(name)
            .cloned()
            .or_else(|| self.class.borrow().get_method(name).map(LoxValue::Function))
    }

    pub fn set(&mut self, name: &Identifier, value: &LoxValue) {
        self.fields.insert(name.clone(), value.clone());
    }
}

#[derive(PartialEq, Debug)]
pub struct LoxClass {
    pub name: Reference,
    pub superclass: Option<Rc<RefCell<LoxClass>>>,
    pub functions: HashMap<Identifier, LoxFunction>,
}

impl LoxClass {
    pub fn get_method(&self, name: &Identifier) -> Option<LoxFunction> {
        self.functions.get(name).cloned().or_else(|| {
            if let Some(superclass) = &self.superclass {
                superclass.borrow().get_method(name)
            } else {
                None
            }
        })
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LoxFunction {
    pub params: Vec<Reference>,
    pub body: Vec<Declaration>,
    /// Needs to be a fresh value upon construction, will be cloned upon calls.
    pub scopes: Vec<LoxScope>,
    /// Should never actually be mutated after construction.
    pub declaring_class: Option<Rc<RefCell<LoxClass>>>,
    /// Is this an initializer method?
    pub is_initializer: bool,
}
