// Based on http://norvig.com/lispy.html

extern crate itertools;

use itertools::Itertools;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
enum Value {
    Float(f32),
    Int(i32),
}

impl std::str::FromStr for Value {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(f) = f32::from_str(s) {
            Ok(Value::Float(f))
        } else {
            if let Ok(i) = i32::from_str(s) {
                Ok(Value::Int(i))
            } else {
                Err(format!("Couldn't parse number from '{}'", s))
            }
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            &Value::Float(x) => write!(f, "{}", x),
            &Value::Int(x) => write!(f, "{}", x),
        }
    }
}

impl Value {
    fn mul(&self, rhs: Value) -> Value {
        match (self, rhs) {
            (&Value::Float(x), Value::Float(y)) => Value::Float(x * y),
            (&Value::Float(x), Value::Int(y)) => Value::Float(x * y as f32),
            (&Value::Int(x), Value::Float(y)) => Value::Float(x as f32 * y),
            (&Value::Int(x), Value::Int(y)) => Value::Int(x * y),
        }
    }
}

struct Env<'a> {
    functions: HashMap<&'a str, Box<Fn(&[Value]) -> Result<Value, String>>>,
    variables: HashMap<&'a str, Value>,
}

impl<'a> Env<'a> {
    fn new() -> Env<'a> {
        let mut functions: HashMap<&'a str, Box<Fn(&[Value]) -> Result<Value, String>>> =
            HashMap::new();
        let mut variables: HashMap<&'a str, Value> = HashMap::new();
        functions.insert("begin",
                         Box::new(|args: &[Value]| {
                             args.last()
                                 .map(|x| *x)
                                 .ok_or(format!("begin function missing arguments"))
                         }));
        functions.insert("*",
                         Box::new(|args: &[Value]| {
                             Ok(args.iter().fold(Value::Int(1), |product, i| product.mul(*i)))
                         }));
        variables.insert("pi", Value::Float(3.141592));
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

fn parse<'a>(tokens: &[&'a str]) -> Result<(usize, Expression<'a>), String> {
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
                        let (x, test) = parse(&tokens[index..])?;
                        index += x;
                        let (y, conseq) = parse(&tokens[index..])?;
                        index += y;
                        let (z, alt) = parse(&tokens[index..])?;
                        index += z;
                        Ast::Conditional(test, conseq, alt)
                    }
                    "define" => {
                        index += 2;
                        let var = tokens.get(index)
                            .ok_or(format!("expected variable name at token {}", index))?;
                        index += 1;
                        let (x, y) = parse(&tokens[index..])?;
                        index += x;
                        Ast::Definition(var, y)
                    }
                    _ => {
                        index += 2;
                        let (x, y) = parse(&tokens[index..])?;
                        index += x;
                        Ast::Procedure(name, y)
                    }
                });
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
            }
        }
    }
    Ok((index, expr))
}

fn eval<'a>(expr: &[Ast<'a>], env: &mut Env<'a>) -> Result<Value, String> {
    if let Some(x) = expr.first() {
        Ok(match x {
            &Ast::Conditional(ref test, ref conseq, ref alt) => {
                let result = eval(&test, env)?;
                if match result {
                    Value::Float(f) => f != 0.0,
                    Value::Int(i) => i != 0,
                } {
                    eval(&conseq, env)?
                } else {
                    eval(&alt, env)?
                }
            }
            &Ast::Definition(ref var, ref exp) => {
                let val = eval(&exp, env)?;
                env.variables.insert(var, val);
                val
            }
            &Ast::Procedure(ref name, ref exp) => {
                let res: Result<Vec<_>, _> =
                    exp.iter().enumerate().map(|(i, _)| eval(&exp[i..], env)).collect();
                let args: Vec<_> = res?;
                let f = env.functions
                    .get(name)
                    .ok_or(format!("Unknown procedure name '{}'", name))?;
                f(&args)?
            }
            &Ast::Literal(val) => val,
            &Ast::Variable(name) => {
                *env.variables
                    .get(name)
                    .ok_or(format!("Unknown value for variable '{}'", name))?
            }
        })
    } else {
        Err(format!("Couldn't execute {:?}", expr))
    }
}

fn main() {
    match Ok("(begin (define r 10) (* pi (* r r)))")
        .map(tokenize)
        .and_then(|x| parse(&x))
        .and_then(|(_, x)| {
            let mut env = Env::new();
            eval(&x, &mut env)
        }) {
        Ok(result) => println!("{}", result),
        Err(e) => println!("Error in REPL: {}", e),
    }
}
