# droptest



[![Checks](https://img.shields.io/github/checks-status/regexident/droptest/main?style=flat-square)](https://github.com/regexident/droptest/)
[![Downloads](https://img.shields.io/crates/d/droptest.svg?style=flat-square)](https://crates.io/crates/droptest/)
[![Version](https://img.shields.io/crates/v/droptest.svg?style=flat-square)](https://crates.io/crates/droptest/)
[![License](https://img.shields.io/crates/l/droptest.svg?style=flat-square)](https://crates.io/crates/droptest/)

## Synopsis

A Rust helper crate for testing drop-semantics.

## Motivation

When implementing one's own smart pointers or collections in Rust one tends to end up having to resort to manual memory management using `std::ptr`, including manual drop management. At which point one will want to write unit tests for making sure the implementation works as one expects it to.

This is where `droptest` comes in handy!

## Usage

```rust
use droptest::prelude::*;

let registry = DropRegistry::default();
let guard = registry.new_guard();
let guard_id = guard.id();

assert_no_drop!(registry, guard_id);

std::mem::drop(guard);

assert_drop!(registry, guard_id);
```

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our [code of conduct](https://www.rust-lang.org/conduct.html),  
and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/regexident/droptest/tags).

## License

This project is licensed under the [**MPL-2.0**](https://www.tldrlegal.com/l/mpl-2.0) – see the [LICENSE.md](LICENSE.md) file for details.
