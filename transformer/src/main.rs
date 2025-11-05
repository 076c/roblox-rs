use transformer::transform_source;

fn main() {
    let file = std::env::args().nth(1).expect("expected exactly one file");

    let bytes = std::fs::read(file).unwrap_or_else(|_| panic!("failed to read file"));
    let source = String::from_utf8_lossy(&bytes).to_string();

    println!("{:#?}", transform_source(source))
}
