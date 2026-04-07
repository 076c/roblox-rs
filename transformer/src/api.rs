use transformer::parse_source;
use transformer::transpiler::transpile_ir;

pub fn main() {
    let file = std::env::args().nth(1).expect("expected exactly one file");

    let bytes = std::fs::read(file).unwrap_or_else(|_| panic!("failed to read file"));
    let source = String::from_utf8_lossy(&bytes).to_string();

    let file = parse_source(source);

    let lifted: Vec<transformer::ir::IRStmt> =
        transformer::lift_syn(&file.as_ref().expect("Failed to parse file!")).unwrap();

    println!("{:#?}", file);
    println!("{:#?}", lifted);

    println!("{:#?}", transpile_ir(lifted).unwrap());
}
