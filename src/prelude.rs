use std::cell::RefCell;
use std::rc::Rc;

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

fn read_from_keyboard() -> String {
    let mut to_return = String::new();
    std::io::stdin()
        .read_line(&mut to_return)
        .expect("Unable to read from STDIN. Is the terminal still working?");
    to_return
}
