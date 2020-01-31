use criterion::{criterion_group, criterion_main, Benchmark, BenchmarkId, Criterion, Throughput};

// new implementation
use lib_maniascript::parser::parse;

// old implementation
use lib_maniascript::{lexer::Lexer, parser::Parser};

fn new_parse(input: &str) {
    parse(input);
}

fn old_parse(input: &str) {
    let lexer = Lexer::new(&input);
    let tree = Parser::new(lexer).parse();
}

fn compare_implementation(c: &mut Criterion) {
    let input = include_str!("Obstacle.Script.txt");

    let mut group = c.benchmark_group("Parser");
    group
        .sample_size(20)
        .bench_function("Old", |b| b.iter(|| old_parse(input)));
    group
        .sample_size(20)
        .bench_function("New", |b| b.iter(|| new_parse(input)));
}

criterion_group!(benches, compare_implementation);
criterion_main!(benches);
