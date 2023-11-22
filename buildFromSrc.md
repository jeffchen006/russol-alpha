# Build from source:

```bash
cargo build --release
```

# Run one test

```bash
cargo run --release --bin ruslic ./demo.rs
```




# Step-by-Step Instructions

The artifact includes the tools required to verify all of our claims. We describe each in turn, and provide instructions for reproducing the results.

## Section 1

The StackOverflow example from Fig. 1 can be found at `russol-alpha/ruslic/tests/synth/paper/rust/b-stackoverflow/reborrow.rs`. It can be synthesized by running the tool on a single file as described above

```bash
cargo run --release --bin ruslic ./ruslic/tests/synth/paper/rust/b-stackoverflow/reborrow.rs
```

## Section 2

The running example from this section can be found at `russol-alpha/ruslic/tests/synth/paper/rust/c-custom/list_ex/list_paper.rs`. All of the functions can be synthesized in one go with

```bash
cargo run --release --bin ruslic ./list_paper.rs 
```

The order of the functions is the same as presented in the paper, though the names of some of the functions are changed to avoid clashes. There is also an additional `pop` function, which is not mentioned in the paper.

The functions can also be synthesized one by one, by annotating the one to be synthesized with `#[synth]` in the source file.

## Section 4

There are two main points to be checked for the evaluation; the results from Table 1, and that synthesis results can be verified using the Creusot verification tool.

### Table 1

**Note on parallelism**: The execution times of the commands in this section are drastically reduced by synthesizing all functions within a file/crate in parallel. The tool enables this by default, but it may lead to slightly longer recorded synthesis times per function, especially for systems with fewer cores (or few cores allocated to the docker engine). If one needs precise permormance data or runs into out-of-memory issues, this parallelism can be disabled by adding the `-e RUSLIC_THREAD_COUNT=1` flag to the docker commands (i.e. `docker run -e RUSLIC_THREAD_COUNT=1 ...`), the default value is `8`.

#### Rust, SuSLik and Verifier categories

The categories "Rust", "SuSLik" and "Verifier" are contained in respective directories inside `russol-alpha/ruslic/tests/synth/paper`. They can all be synthesized by running the test harness `.../tests/ci.rs` with

```bash
docker run --rm -it -v ${PWD}/russol-alpha/ruslic/tests:/home/sbtuser/russol-alpha/ruslic/tests jonasalaif/russol test --release --test ci -- all_tests --nocapture
```

```bash
cargo test --release --test ci -- all_tests --nocapture
```

**Overall, it takes 262.90s to finish all the tests on my desktop.**

The command outputs the results' table both to stdout and `${PWD}/russol-alpha/ruslic/ruslic/tests/ci-results.txt`. The `-v ${PWD}/russol-alpha/ruslic/tests:/home/sbtuser/russol-alpha/ruslic/tests` part ensures that this file is also saved to the local file system (at `.../tests/ci-results.txt`). It also ensures that we are running on the same tests as the ones in the artifact.

The resulting table should look similar (time results will vary) to the one found at `.../tests/ci-expected.txt` and can be compared to the one in the paper. The outputted table contains a summary of the results for each category, for example

```text
rust                   Solved 50 	 Time 1.3s 	 SOL rules 32.98 	 Rust LOC 6.16 	 Code/Spec 1.27 | Overhead data: Sln nodes 19.20 	 Ann nodes 14.42 	 Non-exec pure fn nodes 0.72
```

The annotation overhead (Code/Spec) is reported as the ratio of the number of synthesized solution AST nodes to the *total* number of annotation AST nodes. The pure function AST nodes are included in the annotation count only if they return a type not usable outside of specification (e.g. `Set<T>`).

The two failing cases can be found under `.../tests/synth/paper/rust/b-stackoverflow/swap_enum.rs` and at the end of `.../tests/synth/paper/suslik/d-list_of_lists/multi-list.rs`. They are not tested by the command above, but can be uncommented and synthesized individually (as described above).

#### Full output

The test above printed only a reduced output for easier comparison to the table. To print the full output add `-e RUSLIC_EVAL=false` to the command (i.e. `docker run -e RUSLIC_EVAL=false ...`). This flag only affects the formatting of the benchmarking results. The expected full output can be found at `.../tests/ci-expected-full.txt`.

The full output includes all categories, even some sub-categories which are not mentioned/included in the paper. The results for each category now also list the used pure functions along with their AST nodes and whether they are counted as executable.

Additionally, stats per function are also reported, for example

```text
stack.rs::List::<T>::new - 0_124ms [1/3/3/5] | spec_ast: 4, pfn_ast: {"List::<T>::len": (true, 14), "Node::<T>::len_gt": (true, 16)}
```

means that the function `List::<T>::new` was synthesized in 0.124 seconds, with 1 LOC, 3 synthesized AST nodes, 3 unsimplified synthesized AST nodes (not reported in the paper), 5 rule applications and a specification AST of size 4. Pure functions used for the specification along with if they are executable and their size in AST nodes are also reported.

#### 100-Crates category

The test harness `.../tests/top_crates.rs` runs the tool on the top 100 crates cached in the directory `.../tests/top_100_crates/`, we use the latest version of the crates as of 31.03.2023. It can be run with

> This command can take multiple hours to complete! On computers with better specs one can (significantly) reduce execution time by giving the Docker image access to >10 CPUs and `X`gb Memory and running the command with `-e RUSLIC_THREAD_COUNT=X` (where `X` is e.g. `32`).

```bash
docker run --rm -it -v ${PWD}/russol-alpha/ruslic/tests:/home/sbtuser/russol-alpha/ruslic/tests jonasalaif/russol test --release --test top_crates -- top_crates_cached --nocapture
```


```bash
cargo test --release --test top_crates -- top_crates_cached --nocapture
```





At the end it will print a summary of the results (and write them to `.../tests/crates-results.txt`), which should look similar to that included in `.../tests/crates-expected.txt`.

The tool can also be run on the current latest version of the top 100 crates from crates.io by replacing `top_crates_cached` with `top_crates_all` in the above command.

#### Full output

Again the full output is not shown, but can be enabled by running with `-e RUSLIC_EVAL=false`. Along with printing average statistics for all crates combined, the full output also breaks them down per crate. The expected full output can be found at `.../tests/crates-expected-full.txt`.

### Creusot

There are a few subtle differences between Creusot specifications and RusSOL ones which prevent us from directly running the former tool on the same file synthesized by the latter. Therefore, we have compiled and slightly updated the specification style of all the annotated tests into a single file at `.../tests/all/creusot.rs`. This file can be compared with the compiled version of all the tests in `.../tests/all/russol.rs`.

> Some functions are commented out as they are either not supported by Creusot or require helper functions which the synthesis tool does not annotate with specifications.

The file can be verified by running the following three commands

```bash
docker load -i ../creusot-arch.tar.gz
docker run --rm -it -v ${PWD}/russol-alpha/ruslic/tests/all:/all jonasalaif/creusot ./mlcfg /all/creusot.rs
docker run --rm -it -v ${PWD}/russol-alpha/ruslic/tests/all:/all jonasalaif/creusot ./prove /all/creusot.mlcfg
```

> All proof goals should be verified successfully, except the last one ("clone'_refn") which was not synthesized but rather automatically generated by `#[derive(Clone)]`.

The first command loads the Creusot image, the second command runs Creusot to generate a `.mlcfg` proof goal file, and the third command runs `why3` to try and automatically prove all goals in this file.

We include the expected proof goal file in the artifact, at `.../tests/all/creusot-expected.mlcfg` which can be compared to the generated one (`creusot.mlcfg`) if there are any issues.

The main changes required to adapt a file for Creusot are:

- Replace `use russol_contracts::*;` at the top of the file with `extern crate creusot_contracts; use creusot_contracts::*;`

- Replace all `#[pure]` annotations with `#[logic]`, any integer return type for these "logic" functions with `logic::Int` and `Set`s also need to be adapted (see the changes made between the `russol.rs` and `creusot.rs` files).

- In Creusot, "logic" functions need to be shown to terminate - to avoid having to prove this one can annotate the function with `#[trusted]` and add `#[ensures(result == (fn_body))]` where `fn_body` is the body of the function.

- Integer literals within specifications may need to be written as `5_u32` instead of `5`, if the Rust type is needed, to avoid Creusot's automatic conversion to `logic::Int`.