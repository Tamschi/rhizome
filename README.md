# rhizome

[![Lib.rs](https://img.shields.io/badge/Lib.rs-*-84f)](https://lib.rs/crates/rhizome)
[![Crates.io](https://img.shields.io/crates/v/rhizome)](https://crates.io/crates/rhizome)
[![Docs.rs](https://docs.rs/rhizome/badge.svg)](https://docs.rs/rhizome)

![Rust 1.55](https://img.shields.io/static/v1?logo=Rust&label=&message=1.55&color=grey)
[![CI](https://github.com/Tamschi/rhizome/workflows/CI/badge.svg?branch=develop)](https://github.com/Tamschi/rhizome/actions?query=workflow%3ACI+branch%3Adevelop)
![Crates.io - License](https://img.shields.io/crates/l/rhizome/0.0.1)

[![GitHub](https://img.shields.io/static/v1?logo=GitHub&label=&message=%20&color=grey)](https://github.com/Tamschi/rhizome)
[![open issues](https://img.shields.io/github/issues-raw/Tamschi/rhizome)](https://github.com/Tamschi/rhizome/issues)
[![open pull requests](https://img.shields.io/github/issues-pr-raw/Tamschi/rhizome)](https://github.com/Tamschi/rhizome/pulls)
[![good first issues](https://img.shields.io/github/issues-raw/Tamschi/rhizome/good%20first%20issue?label=good+first+issues)](https://github.com/Tamschi/rhizome/contribute)

[![crev reviews](https://web.crev.dev/rust-reviews/badge/crev_count/rhizome.svg)](https://web.crev.dev/rust-reviews/crate/rhizome/)
[![Zulip Chat](https://img.shields.io/endpoint?label=chat&url=https%3A%2F%2Fiteration-square-automation.schichler.dev%2F.netlify%2Ffunctions%2Fstream_subscribers_shield%3Fstream%3Dproject%252Frhizome)](https://iteration-square.schichler.dev/#narrow/stream/project.2Frhizome)

A convenient hierarchical dependency-extraction container.

`rhizome` supports lazy provision, shadowing and testing/configuration use cases.

## Installation

Please use [cargo-edit](https://crates.io/crates/cargo-edit) to always add the latest version of this library:

```cmd
cargo add rhizome
```

## Example

```rust
// TODO_EXAMPLE
```

## License

Licensed under either of

- Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license
   ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

See [CONTRIBUTING](CONTRIBUTING.md) for more information.

## [Code of Conduct](CODE_OF_CONDUCT.md)

## [Changelog](CHANGELOG.md)

## Versioning

`rhizome` strictly follows [Semantic Versioning 2.0.0](https://semver.org/spec/v2.0.0.html) with the following exceptions:

- The minor version will not reset to 0 on major version changes (except for v1).  
Consider it the global feature level.
- The patch version will not reset to 0 on major or minor version changes (except for v0.1 and v1).  
Consider it the global patch level.

This includes the Rust version requirement specified above.  
Earlier Rust versions may be compatible, but this can change with minor or patch releases.

Which versions are affected by features and patches can be determined from the respective headings in [CHANGELOG.md](CHANGELOG.md).

Note that dependencies of this crate may have a more lenient MSRV policy!
Please use `cargo +nightly update -Z minimal-versions` in your automation if you don't generate Cargo.lock manually (or as necessary) and require support for a compiler older than current stable.
