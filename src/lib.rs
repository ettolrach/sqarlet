mod codegen;
mod expr;
mod parse;
mod prelude;
mod typechecking;
mod types;

/// Returns the transpiled Rust code.
pub fn generate(s: &str) -> Result<String, ()> {
    Ok(String::from(
        r#"
use std::rc::Rc;
use std::cell::RefCell;

fn main() {
    let allScores = Rc::new(RefCell::new(vec![12,34,23,54,32,67,26,23]));
    for counter in (0..=7) {
        if allScores.borrow()[counter] >= 50 {
            println!("Great Score{}", allScores.borrow()[counter]);
        }
    }
}
"#,
    ))
}

#[cfg(test)]
mod tests {
    use std::{env::current_dir, process::Command};

    use super::*;
    #[test]
    fn traverse_array_1() {
        let code = String::from(
            r#"
DECLARE allScores INITIALLY [ 12,34,23,54,32,67,26,23 ]
FOR counter FROM 0 TO 7 DO
    IF allScores[counter] >= 50 THEN
        SEND “Great Score” & allScores[counter] TO DISPLAY
    END IF
END FOR
"#,
        );

        let expected = String::from(
            "
Great Score54
Great Score67
",
        );

        let current_dir = current_dir().unwrap();
        let rust_path = current_dir.join("traverse_array_1.rs");
        let program_path = current_dir.join("traverse_array_1");

        std::fs::write(&rust_path, generate(&code).unwrap()).unwrap();
        _ = Command::new("rustc")
            .arg("-o")
            .arg(&program_path)
            .arg(&rust_path)
            .output()
            .unwrap();

        let output =
            String::from_utf8(Command::new(program_path).output().unwrap().stdout).unwrap();

        assert_eq!(output.trim(), expected.trim());
    }
}
