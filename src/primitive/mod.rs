mod primitive_defs;
pub use primitive_defs::*;

use std::{
    borrow::Cow,
    cell::RefCell,
    f64::{
        consts::{PI, TAU},
        INFINITY,
    },
    fmt,
    sync::OnceLock,
};

use enum_iterator::{all, Sequence};
use rand::prelude::*;

use crate::{
    algorithm::loops, function::FunctionId, lex::Simple, sys::*, value::*, Uiua, UiuaError,
    UiuaResult,
};

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(c) = self.unicode() {
            write!(f, "{}", c)
        } else if let Some(s) = self.ascii() {
            write!(f, "{}", s)
        } else if let Some(s) = self.name() {
            write!(f, "{}", s)
        } else if let Primitive::FillValue = self {
            write!(f, "_")
        } else {
            write!(f, "{:?}", self)
        }
    }
}

impl Primitive {
    pub fn name(&self) -> Option<&'static str> {
        self.names().map(|n| n.text)
    }
    pub fn ascii(&self) -> Option<Simple> {
        self.names().and_then(|n| n.ascii)
    }
    pub fn unicode(&self) -> Option<char> {
        self.names().and_then(|n| n.unicode)
    }
    /// Find a primitive by its text name
    pub fn from_name(name: &str) -> Option<Self> {
        Self::all().find(|p| p.names().is_some_and(|n| n.text.eq_ignore_ascii_case(name)))
    }
    pub fn from_simple(s: Simple) -> Option<Self> {
        Self::all().find(|p| p.ascii() == Some(s))
    }
    pub fn from_unicode(c: char) -> Option<Self> {
        Self::all().find(|p| p.unicode() == Some(c))
    }
    pub fn is_modifier(&self) -> bool {
        self.modifier_args().is_some()
    }
    pub fn inverse(&self) -> Option<Self> {
        use Primitive::*;
        Some(match self {
            Noop => Noop,
            Flip => Flip,
            Neg => Neg,
            Not => Not,
            Sin => Asin,
            Cos => Acos,
            Asin => Sin,
            Acos => Cos,
            Reverse => Reverse,
            Transpose => InvTranspose,
            InvTranspose => Transpose,
            Bits => InverseBits,
            InverseBits => Bits,
            Debug => Debug,
            _ => return None,
        })
    }
    /// Try to parse a primitive from a name prefix
    pub fn from_format_name(name: &str) -> Option<Self> {
        if name.chars().any(char::is_uppercase) {
            return None;
        }
        if name == "pi" || name == "π" {
            return Some(Primitive::Pi);
        }
        if name == "tau" || name == "τ" {
            return Some(Primitive::Tau);
        }
        if name == "eta" || name == "η" {
            return Some(Primitive::Eta);
        }
        if name.len() < 3 {
            return None;
        }
        let mut matching = Primitive::all().filter(|p| {
            p.names()
                .is_some_and(|n| n.is_formattable() && n.text.starts_with(name))
        });
        let res = matching.next()?;
        let exact_match = res.names().unwrap().text == name;
        (exact_match || matching.next().is_none()).then_some(res)
    }
    /// Try to parse multiple primitives from the concatentation of their name prefixes
    pub fn from_format_name_multi(name: &str) -> Option<Vec<(Self, &str)>> {
        if name == "pi" || name == "π" {
            return Some(vec![(Primitive::Pi, name)]);
        }
        if name == "tau" || name == "τ" {
            return Some(vec![(Primitive::Tau, name)]);
        }
        if name == "eta" || name == "η" {
            return Some(vec![(Primitive::Eta, name)]);
        }
        let indices: Vec<usize> = name.char_indices().map(|(i, _)| i).collect();
        if indices.len() < 3 {
            return None;
        }
        let mut prims = Vec::new();
        let mut start = 0;
        'outer: loop {
            if start == indices.len() {
                break Some(prims);
            }
            for len in (2..=indices.len() - start).rev() {
                let start_index = indices[start];
                let end_index = indices[start + len - 1];
                let sub_name = &name[start_index..=end_index];
                if let Some(p) = Primitive::from_format_name(sub_name) {
                    if len >= 3 || p == Primitive::Pi {
                        prims.push((p, sub_name));
                        start += len;
                        continue 'outer;
                    }
                }
            }
            break None;
        }
    }
    pub(crate) fn run(&self, env: &mut Uiua) -> UiuaResult {
        match self {
            Primitive::Eta => env.push(PI / 2.0),
            Primitive::Pi => env.push(PI),
            Primitive::Tau => env.push(TAU),
            Primitive::Infinity => env.push(INFINITY),
            Primitive::Noop | Primitive::FillValue => {}
            Primitive::Not => env.monadic_env(Value::not)?,
            Primitive::Neg => env.monadic_env(Value::neg)?,
            Primitive::Abs => env.monadic_env(Value::abs)?,
            Primitive::Sign => env.monadic_env(Value::sign)?,
            Primitive::Sqrt => env.monadic_env(Value::sqrt)?,
            Primitive::Sin => env.monadic_env(Value::sin)?,
            Primitive::Cos => env.monadic_env(Value::cos)?,
            Primitive::Asin => env.monadic_env(Value::asin)?,
            Primitive::Acos => env.monadic_env(Value::acos)?,
            Primitive::Floor => env.monadic_env(Value::floor)?,
            Primitive::Ceil => env.monadic_env(Value::ceil)?,
            Primitive::Round => env.monadic_env(Value::round)?,
            Primitive::Eq => env.dyadic_ref_env(Value::is_eq)?,
            Primitive::Ne => env.dyadic_ref_env(Value::is_ne)?,
            Primitive::Lt => env.dyadic_ref_env(Value::is_lt)?,
            Primitive::Le => env.dyadic_ref_env(Value::is_le)?,
            Primitive::Gt => env.dyadic_ref_env(Value::is_gt)?,
            Primitive::Ge => env.dyadic_ref_env(Value::is_ge)?,
            Primitive::Add => env.dyadic_ref_env(Value::add)?,
            Primitive::Sub => env.dyadic_ref_env(Value::sub)?,
            Primitive::Mul => env.dyadic_ref_env(Value::mul)?,
            Primitive::Div => env.dyadic_ref_env(Value::div)?,
            Primitive::Mod => env.dyadic_ref_env(Value::modulus)?,
            Primitive::Pow => env.dyadic_ref_env(Value::pow)?,
            Primitive::Log => env.dyadic_ref_env(Value::log)?,
            Primitive::Min => env.dyadic_ref_env(Value::min)?,
            Primitive::Max => env.dyadic_ref_env(Value::max)?,
            Primitive::Atan => env.dyadic_ref_env(Value::atan2)?,
            Primitive::Match => env.dyadic_ref(|a, b| a == b)?,
            Primitive::Join => env.dyadic_env(Value::join)?,
            Primitive::Transpose => env.monadic_mut(Value::transpose)?,
            Primitive::InvTranspose => env.monadic_mut(Value::inv_transpose)?,
            Primitive::Pick => env.dyadic_env(Value::pick)?,
            Primitive::Replicate => env.dyadic_ref_own_env(Value::replicate)?,
            Primitive::Take => env.dyadic_env(Value::take)?,
            Primitive::Drop => env.dyadic_env(Value::drop)?,
            Primitive::Rotate => env.dyadic_ref_own_env(Value::rotate)?,
            Primitive::Couple => env.dyadic_env(Value::couple)?,
            Primitive::Fill => {
                let fill = env.pop(1)?;
                let mut array = env.pop(2)?;
                array.fill(fill, env)?;
                env.push(array);
            }
            Primitive::Sort => env.monadic_mut(Value::sort)?,
            Primitive::Grade => env.monadic_ref_env(|v, env| v.grade(env))?,
            Primitive::Indices => env.monadic_ref_env(|v, env| v.indices(env))?,
            Primitive::Select => env.dyadic_ref_env(Value::select)?,
            Primitive::Windows => env.dyadic_ref_env(Value::windows)?,
            Primitive::Classify => env.monadic_ref_env(Value::classify)?,
            Primitive::Deduplicate => env.monadic_mut(Value::deduplicate)?,
            Primitive::Member => env.dyadic_ref_env(Value::member)?,
            Primitive::Find => env.dyadic_ref_env(Value::find)?,
            Primitive::IndexOf => env.dyadic_ref_env(Value::index_of)?,
            Primitive::Group => env.dyadic_ref_env(Value::group)?,
            Primitive::Partition => env.dyadic_ref_env(Value::partition)?,
            Primitive::Call => env.call()?,
            Primitive::Parse => env.monadic_env(|v, env| v.parse_num(env))?,
            Primitive::Range => env.monadic_ref_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::First => env.monadic_env(Value::first)?,
            Primitive::Last => env.monadic_env(Value::last)?,
            Primitive::Len => env.monadic_ref(Value::len)?,
            Primitive::Rank => env.monadic_ref(Value::rank)?,
            Primitive::Truncate => env.monadic_mut(Value::truncate)?,
            Primitive::Bits => env.monadic_ref_env(Value::bits)?,
            Primitive::InverseBits => env.monadic_ref_env(Value::inverse_bits)?,
            Primitive::Fold => loops::fold(env)?,
            Primitive::Reduce => loops::reduce(env)?,
            Primitive::Each => loops::each(env)?,
            Primitive::Zip => loops::zip(env)?,
            Primitive::Rows => loops::rows(env)?,
            Primitive::Bridge => loops::bridge(env)?,
            Primitive::Distribute => loops::distribute(env)?,
            Primitive::Plow => loops::plow(env)?,
            Primitive::Table => loops::table(env)?,
            Primitive::Cross => loops::cross(env)?,
            Primitive::Scan => loops::scan(env)?,
            Primitive::Repeat => loops::repeat(env)?,
            Primitive::Level => loops::level(env)?,
            Primitive::Reshape => {
                let shape = env.pop(1)?;
                let mut array = env.pop(2)?;
                array.reshape(&shape, env)?;
                env.push(array);
            }
            Primitive::Break => {
                let n = env.pop(1)?.as_nat(env, "break expects a natural number")?;
                if n > 0 {
                    return Err(UiuaError::Break(n - 1, env.span().clone()));
                }
            }
            Primitive::Recur => env.recur()?,
            Primitive::Debug => {
                let value = env.pop(1)?;
                env.sys.print_str(&value.show()).map_err(|e| env.error(e))?;
                env.sys.print_str("\n").map_err(|e| env.error(e))?;
                env.push(value);
            }
            Primitive::Dup => {
                let x = env.pop(1)?;
                env.push(x.clone());
                env.push(x);
            }
            Primitive::Flip => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                env.push(a);
                env.push(b);
            }
            Primitive::Over => {
                let a = env.pop(1)?;
                let b = env.pop(2)?;
                env.push(b.clone());
                env.push(a);
                env.push(b);
            }
            Primitive::Pop => {
                env.pop(1)?;
            }
            Primitive::Try => {
                let f = env.pop(1)?;
                let handler = env.pop(2)?;
                let size = env.stack_size();
                env.push(f);
                if let Err(e) = env.call() {
                    env.truncate_stack(size);
                    env.push(e.message());
                    env.push(handler);
                    env.call()?;
                }
            }
            Primitive::Invert => {
                let f = env.pop(1)?;
                let inv_f = f.invert(env)?;
                env.push(inv_f);
                env.call()?;
            }
            Primitive::Under => {
                let f = env.pop(1)?;
                let g = env.pop(2)?;
                let inv_f = f.invert(env)?;
                env.push(f);
                env.call()?;
                env.push(g);
                env.call()?;
                env.push(inv_f);
                env.call()?;
            }
            Primitive::Throw => {
                let msg = env.pop(1)?;
                let cond = env.pop(2)?;
                if cond.as_nat(env, "").map_or(true, |n| n == 0) {
                    return Err(UiuaError::Throw(msg.into(), env.span().clone()));
                }
            }
            Primitive::Shape => {
                env.monadic_ref(|v| v.shape().iter().copied().collect::<Value>())?
            }
            Primitive::Rand => {
                thread_local! {
                    static RNG: RefCell<SmallRng> = RefCell::new(SmallRng::seed_from_u64(instant::now().to_bits()));
                }
                env.push(RNG.with(|rng| rng.borrow_mut().gen::<f64>()));
            }
            Primitive::Gen => {
                let seed = env.pop(1)?;
                let mut rng =
                    SmallRng::seed_from_u64(seed.as_num(env, "gen expects a number")?.to_bits());
                let val: f64 = rng.gen();
                let next_seed = f64::from_bits(rng.gen::<u64>());
                env.push(val);
                env.push(next_seed);
            }
            Primitive::Use => {
                let name = env.pop(1)?.as_string(env, "Use name must be a string")?;
                let lib = env.pop(2)?;
                let f = match lib {
                    Value::Func(fs) => fs.data.iter().find_map(|f| {
                        matches!(&f.id, FunctionId::Named(n) if n.as_str().eq_ignore_ascii_case(&name))
                            .then(|| f.clone())
                    }),
                    _ => None,
                }
                .ok_or_else(|| env.error(format!("No function found for {name:?}")))?;
                env.push(f);
            }
            Primitive::Sys(io) => io.run(env)?,
        }
        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct PrimDoc {
    pub short: Vec<PrimDocFragment>,
    pub lines: Vec<PrimDocLine>,
}

impl PrimDoc {
    pub fn short_text(&self) -> Cow<str> {
        if self.short.len() == 1 {
            match &self.short[0] {
                PrimDocFragment::Text(t) => return Cow::Borrowed(t),
                PrimDocFragment::Code(c) => return Cow::Borrowed(c),
                PrimDocFragment::Emphasis(e) => return Cow::Borrowed(e),
                PrimDocFragment::Primitive { prim, named: true } => {
                    if let Some(s) = prim.name() {
                        return Cow::Owned(s.to_owned());
                    }
                }
                PrimDocFragment::Primitive { .. } => {}
            }
        }
        let mut s = String::new();
        for frag in &self.short {
            match frag {
                PrimDocFragment::Text(t) => s.push_str(t),
                PrimDocFragment::Code(c) => s.push_str(c),
                PrimDocFragment::Emphasis(e) => s.push_str(e),
                PrimDocFragment::Primitive { prim, named } => {
                    let mut name = String::new();
                    if *named {
                        s.push_str(prim.name().unwrap_or_else(|| {
                            name = format!("{prim:?}");
                            &name
                        }));
                    } else if let Some(c) = prim.unicode() {
                        s.push(c);
                    } else {
                        s.push_str(prim.name().unwrap_or_else(|| {
                            name = format!("{prim:?}");
                            &name
                        }));
                    }
                }
            }
        }
        Cow::Owned(s)
    }
    pub fn from_lines(s: &str) -> Self {
        let mut short = Vec::new();
        let mut lines = Vec::new();
        for line in s.lines() {
            let line = line.trim();
            if let Some(mut ex) = line.strip_prefix("ex:") {
                // Example
                if ex.starts_with(' ') {
                    ex = &ex[1..]
                }
                lines.push(PrimDocLine::Example(PrimExample {
                    input: ex.into(),
                    output: OnceLock::new(),
                }));
            } else if let Some(mut ex) = line.strip_prefix(':') {
                // Continue example
                if ex.starts_with(' ') {
                    ex = &ex[1..]
                }
                if let Some(PrimDocLine::Example(example)) = lines.last_mut() {
                    example.input.push('\n');
                    example.input.push_str(ex);
                } else {
                    lines.push(PrimDocLine::Text(parse_doc_line_fragments(line)));
                }
            } else if short.is_empty() {
                // Set short
                short = parse_doc_line_fragments(line);
            } else {
                // Add line
                lines.push(PrimDocLine::Text(parse_doc_line_fragments(line)));
            }
        }
        while let Some(PrimDocLine::Text(frags)) = lines.first() {
            if frags.is_empty() {
                lines.remove(0);
            } else {
                break;
            }
        }
        while let Some(PrimDocLine::Text(frags)) = lines.last() {
            if frags.is_empty() {
                lines.pop();
            } else {
                break;
            }
        }
        Self { short, lines }
    }
}

#[derive(Debug)]
pub struct PrimExample {
    input: String,
    output: OnceLock<Result<Vec<String>, String>>,
}

impl PrimExample {
    pub fn input(&self) -> &str {
        &self.input
    }
    pub fn output(&self) -> &Result<Vec<String>, String> {
        self.output.get_or_init(|| {
            Uiua::with_backend(&NativeSys)
                .load_str(&self.input)
                .map(|env| env.take_stack().into_iter().map(|val| val.show()).collect())
                .map_err(|e| {
                    e.to_string()
                        .lines()
                        .next()
                        .unwrap_or_default()
                        .split_once(' ')
                        .unwrap_or_default()
                        .1
                        .into()
                })
        })
    }
}

#[derive(Debug)]
pub enum PrimDocLine {
    Text(Vec<PrimDocFragment>),
    Example(PrimExample),
}

#[derive(Debug, Clone)]
pub enum PrimDocFragment {
    Text(String),
    Code(String),
    Emphasis(String),
    Primitive { prim: Primitive, named: bool },
}

fn parse_doc_line_fragments(line: &str) -> Vec<PrimDocFragment> {
    let mut frags = Vec::new();
    #[derive(PartialEq, Eq)]
    enum FragKind {
        Text,
        Code,
        Emphasis,
        Primitive,
    }
    impl FragKind {
        fn open(&self) -> &str {
            match self {
                FragKind::Text => "",
                FragKind::Code => "`",
                FragKind::Emphasis => "*",
                FragKind::Primitive => "[",
            }
        }
    }
    let mut curr = String::new();
    let mut kind = FragKind::Text;
    for c in line.chars() {
        match c {
            '`' if kind == FragKind::Code => {
                if let Some(prim) = Primitive::from_name(&curr) {
                    frags.push(PrimDocFragment::Primitive { prim, named: false });
                } else {
                    frags.push(PrimDocFragment::Code(curr));
                }
                curr = String::new();
                kind = FragKind::Text;
            }
            '`' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
                kind = FragKind::Code;
            }
            '*' if kind == FragKind::Emphasis => {
                frags.push(PrimDocFragment::Emphasis(curr));
                curr = String::new();
                kind = FragKind::Text;
            }
            '*' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
                kind = FragKind::Emphasis;
            }
            '[' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
                kind = FragKind::Primitive;
            }
            ']' if kind == FragKind::Primitive => {
                if let Some(prim) = Primitive::from_name(&curr) {
                    frags.push(PrimDocFragment::Primitive { prim, named: true });
                } else {
                    frags.push(PrimDocFragment::Text(curr));
                }
                curr = String::new();
                kind = FragKind::Text;
            }
            ']' if kind == FragKind::Text => {
                frags.push(PrimDocFragment::Text(curr));
                curr = String::new();
            }
            c => curr.push(c),
        }
    }
    curr.insert_str(0, kind.open());
    if !curr.is_empty() {
        frags.push(PrimDocFragment::Text(curr));
    }
    frags
}

#[test]
fn primitive_from_name() {
    assert_eq!(Primitive::from_format_name("rev"), Some(Primitive::Reverse));
    assert_eq!(Primitive::from_format_name("re"), None);
    assert_eq!(
        Primitive::from_format_name("resh"),
        Some(Primitive::Reshape)
    );
}

#[cfg(test)]
#[test]
fn from_multiname() {
    assert!(matches!(
        &*Primitive::from_format_name_multi("rev").expect("rev"),
        [(Primitive::Reverse, _)]
    ));
    assert!(matches!(
        &*Primitive::from_format_name_multi("revrev").expect("revrev"),
        [(Primitive::Reverse, _), (Primitive::Reverse, _)]
    ));
    assert!(matches!(
        &*Primitive::from_format_name_multi("tabrepl").unwrap(),
        [(Primitive::Table, _), (Primitive::Replicate, _)]
    ));
    assert_eq!(Primitive::from_format_name_multi("foo"), None);
}

#[cfg(test)]
#[test]
fn gen_grammar_file() {
    let noadic_functions: String = Primitive::all()
        .filter(|p| p.modifier_args().is_none() && p.args() == Some(0))
        .filter_map(|p| p.unicode())
        .collect();
    let monadic_functions: String = Primitive::all()
        .filter(|p| p.modifier_args().is_none() && p.args() == Some(1))
        .filter_map(|p| p.unicode())
        .collect();
    let dyadic_functions = Primitive::all()
        .filter(|p| p.modifier_args().is_none() && p.args() == Some(2))
        .filter_map(|p| p.unicode())
        .collect::<String>()
        .replace('-', "\\\\-");
    let monadic_modifiers = Primitive::all()
        .filter(|p| matches!(p.modifier_args(), Some((1, _))))
        .filter_map(|p| p.unicode())
        .collect::<String>()
        .replace('\\', "\\\\\\\\");
    let dyadic_modifiers: String = Primitive::all()
        .filter(|p| matches!(p.modifier_args(), Some((2, _))))
        .filter_map(|p| p.unicode())
        .collect();

    let text = format!(
        r##"{{
	"$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json",
	"name": "Uiua",
	"patterns": [
		{{
			"include": "#comments"
		}},
		{{
			"include": "#keywords"
		}},
		{{
			"include": "#strings"
		}},
        {{
            "include": "#characters"
        }},
		{{
			"include": "#numbers"
		}},
        {{
            "include": "#strand"
        }},
		{{
			"include": "#noadic"
		}},
		{{
			"include": "#monadic"
		}},
		{{
			"include": "#dyadic"
		}},
		{{
			"include": "#mod1"
		}},
		{{
			"include": "#mod2"
		}}
	],
	"repository": {{
		"comments": {{
			"name": "comment.line.uiua",
			"match": "#.*$"
		}},
		"strings": {{
			"name": "constant.character.escape",
			"match": "(\".*\"|\\$.*$)"
		}},
        "characters": {{
            "name": "constant.character.escape",
            "match": "'\\\\?.'"
        }},
		"numbers": {{
			"name": "constant.numeric.uiua",
			"match": "\\d+(\\.\\d+(e[+-]?\\d+)?)?"
		}},
		"strand": {{
			"name": "comment.line",
			"match": "_"
		}},
		"noadic": {{
			"name": "entity.name.tag.uiua",
            "match": "[{noadic_functions}]"
        }},
		"monadic": {{
			"name": "string.quoted",
            "match": "[{monadic_functions}]"
        }},
		"dyadic": {{
			"name": "entity.name.function.uiua",
            "match": "[{dyadic_functions}]"
        }},
		"mod1": {{
			"name": "entity.name.type.uiua",
            "match": "[{monadic_modifiers}]"
        }},
		"mod2": {{
			"name": "keyword.control.uiua",
            "match": "[{dyadic_modifiers}]"
        }}
    }},
	"scopeName": "source.uiua"
}}"##
    );

    std::fs::write("uiua.tmLanguage.json", text).expect("Failed to write grammar file");
}
