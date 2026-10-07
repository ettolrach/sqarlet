use std::cell::RefCell;
use std::ops::Add;
use std::rc::Rc;
use std::str::FromStr;

struct NumOrString(String);

impl From<NumOrString> for i64 {
    fn from(value: NumOrString) -> Self {
        value.0.parse::<i64>().unwrap()
    }
}

impl From<NumOrString> for f64 {
    fn from(value: NumOrString) -> Self {
        value.0.parse::<f64>().unwrap()
    }
}

impl From<NumOrString> for Rc<RefCell<Vec<char>>> {
    fn from(value: NumOrString) -> Self {
        Rc::new(RefCell::new(value.0.chars().collect()))
    }
}

impl From<String> for NumOrString {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl<A> Add<A> for NumOrString
where
    A: FromStr + Add<A, Output = A>,
    <A as FromStr>::Err: std::fmt::Debug,
{
    type Output = A;
    fn add(self, rhs: A) -> Self::Output {
        self.0
            .parse::<A>()
            .expect("Runtime Error: Incorrect type entered by user. Expected INTEGER.")
            + rhs
    }
}

impl Add<NumOrString> for u64 {
    type Output = u64;
    fn add(self, rhs: NumOrString) -> Self::Output {
        rhs.0
            .parse::<u64>()
            .expect("Runtime Error: Incorrect type entered by user. Expected INTEGER.")
            + self
    }
}

impl Add<NumOrString> for f64 {
    type Output = f64;
    fn add(self, rhs: NumOrString) -> Self::Output {
        rhs.0
            .parse::<f64>()
            .expect("Runtime Error: Incorrect type entered by user. Expected INTEGER.")
            + self
    }
}

fn read_from_keyboard() -> String {
    let mut to_return = String::new();
    std::io::stdin()
        .read_line(&mut to_return)
        .expect("Unable to read from STDIN. Is the terminal still working?");
    to_return
}
