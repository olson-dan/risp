// Based on http://norvig.com/lispy.html

extern crate itertools;

use itertools::Itertools;
use std::collections::HashMap;
use std::io::Write;

#[derive(Debug, Clone)]
enum Value {
    Float(f32),
    Int(i32),
    List(Vec<Value>),
    Bool(bool),
}

impl std::str::FromStr for Value {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "#t" => Ok(Value::Bool(true)),
            "#f" => Ok(Value::Bool(false)),
            _ => {
                if let Ok(i) = i32::from_str(s) {
                    Ok(Value::Int(i))
                } else {
                    if let Ok(f) = f32::from_str(s) {
                        Ok(Value::Float(f))
                    } else {
                        Err(format!("Couldn't parse number from '{}'", s))
                    }
                }
            }
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &Value::Float(x) => write!(f, "{}", x),
            &Value::Int(x) => write!(f, "{}", x),
            &Value::Bool(b) => if b { write!(f, "#t") } else { write!(f, "#f") },
            &Value::List(ref x) => write!(f, "[{}]", x.iter().join(", ")),
        }
    }
}

impl Value {
    fn mul(&self, rhs: &Value) -> Result<Value, String> {
        match (self, rhs) {
            (&Value::Float(x), &Value::Float(y)) => Ok(Value::Float(x * y)),
            (&Value::Float(x), &Value::Int(y)) => Ok(Value::Float(x * y as f32)),
            (&Value::Int(x), &Value::Float(y)) => Ok(Value::Float(x as f32 * y)),
            (&Value::Int(x), &Value::Int(y)) => Ok(Value::Int(x * y)),
            _ => Err(format!("Invalid arguments to '*': {}, {}", self, rhs)),
        }
    }
}

struct Env<'a> {
    functions: HashMap<&'a str, Box<Fn(&[Value]) -> Result<Value, String>>>,
    variables: HashMap<String, Value>,
}

impl<'a> Env<'a> {
    fn new() -> Env<'a> {
        let mut functions: HashMap<&'a str, Box<Fn(&[Value]) -> Result<Value, String>>> =
            HashMap::new();
        let mut variables: HashMap<String, Value> = HashMap::new();
        let begin = Box::new(|args: &[Value]| {
            args.last().map(|x| x.clone()).ok_or(format!("begin function missing arguments"))
        });
        functions.insert("begin", begin);
        let mul = Box::new(|args: &[Value]| {
            let mut result = args.first().unwrap_or(&Value::Int(0)).clone();
            for x in args.iter().skip(1) {
                result = result.mul(&x)?;
            }
            Ok(result)
        });
        functions.insert("*", mul);
        let list = Box::new(|args: &[Value]| {
            let l: Vec<Value> = Vec::from(args);
            Ok(Value::List(l))
        });
        functions.insert("list", list);
        variables.insert("pi".into(), Value::Float(3.141592));
        Env {
            variables: variables,
            functions: functions,
        }
    }
}

fn tokenize<'a>(text: &'a str) -> Vec<&'a str> {
    text.split_whitespace()
        .flat_map(|x| x.split("(").intersperse("("))
        .flat_map(|x| x.split(")").intersperse(")"))
        .filter(|x| x.len() > 0)
        .collect()
}

#[test]
fn test_tokenize() {
    assert_eq!(tokenize("(begin (define r 10) (* pi (* r r)))"),
               vec!["(", "begin", "(", "define", "r", "10", ")", "(", "*", "pi", "(", "*", "r",
                    "r", ")", ")", ")"]);
}

type Expression<'a> = Vec<Ast<'a>>;

#[derive(Debug)]
enum Ast<'a> {
    Variable(&'a str),
    Literal(Value),
    Conditional(Expression<'a>, Expression<'a>, Expression<'a>),
    Definition(&'a str, Expression<'a>),
    Procedure(&'a str, Expression<'a>),
}

fn parse<'a>(tokens: &[&'a str], mut in_arg_list: bool) -> Result<(usize, Expression<'a>), String> {
    let mut expr: Expression<'a> = Vec::new();
    let mut index = 0;
    while let Some(t) = tokens.get(index) {
        match *t {
            "(" => {
                let name = tokens.get(index + 1)
                    .ok_or(format!("expected name at token {}", index + 1))?;
                expr.push(match *name {
                    "if" => {
                        index += 2;
                        let (x, test) = parse(&tokens[index..], false)?;
                        index += x;
                        let (y, conseq) = parse(&tokens[index..], false)?;
                        index += y;
                        let (z, alt) = parse(&tokens[index..], false)?;
                        index += z;
                        in_arg_list = true; // ensure we eat the )
                        Ast::Conditional(test, conseq, alt)
                    }
                    "define" => {
                        index += 2;
                        let var = tokens.get(index)
                            .ok_or(format!("expected variable name at token {}", index))?;
                        index += 1;
                        let (x, val) = parse(&tokens[index..], true)?;
                        index += x;
                        Ast::Definition(var, val)
                    }
                    _ => {
                        index += 2;
                        let (x, args) = parse(&tokens[index..], true)?;
                        index += x;
                        Ast::Procedure(name, args)
                    }
                });
                if !in_arg_list {
                    break;
                }
            }
            ")" => {
                index += 1;
                break;
            }
            _ => {
                index += 1;
                expr.push(if let Ok(v) = t.parse::<Value>() {
                    Ast::Literal(v)
                } else {
                    Ast::Variable(*t)
                });
                if !in_arg_list {
                    break;
                }
            }
        }
    }
    Ok((index, expr))
}

fn eval<'a>(expr: &[Ast<'a>], env: &mut Env) -> Result<Value, String> {
    if let Some(x) = expr.first() {
        match x {
            &Ast::Conditional(ref test, ref conseq, ref alt) => {
                let result = eval(&test, env)?;
                if match result {
                    Value::Bool(b) => b,
                    _ => return Err(format!("conditional is not a bool type")),
                } {
                    Ok(eval(&conseq, env)?)
                } else {
                    Ok(eval(&alt, env)?)
                }
            }
            &Ast::Definition(ref var, ref exp) => {
                let val = eval(&exp, env)?;
                env.variables.insert(String::from(*var), val.clone());
                Ok(val)
            }
            &Ast::Procedure(ref name, ref exp) => {
                let res: Result<Vec<_>, _> = exp.iter()
                    .enumerate()
                    .map(|(i, _)| eval(&exp[i..], env))
                    .collect();
                let args: Vec<_> = res?;
                let f = env.functions
                    .get(name)
                    .ok_or(format!("Unknown procedure name '{}'", name))?;
                Ok(f(&args)?)
            }
            &Ast::Literal(ref val) => Ok(val.clone()),
            &Ast::Variable(name) => {
                Ok(env.variables
                    .get(name)
                    .ok_or(format!("Unknown value for variable '{}'", name))?
                    .clone())
            }
        }
    } else {
        Err(String::new())
    }
}

fn main() {
    let mut env = Env::new();
    let stdin = std::io::stdin();
    loop {
        print!("> ");
        if let Err(e) = std::io::stdout().flush() {
            println!("{}", e);
            std::process::exit(1);
        }
        let mut input = String::new();
        match stdin.read_line(&mut input)
            .map_err(|e| format!("{}", e))
            .map(|_| tokenize(&input))
            .and_then(|x| parse(&x, false))
            .and_then(|(_, x)| eval(&x, &mut env)) {
            Ok(result) => println!("{}", result),
            Err(e) => {
                if e != "" {
                    println!("Error: {}", e)
                }
            }
        }
    }
}