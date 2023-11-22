#!/bin/bash


# for 15 times
for i in {1..15}
do
    cargo test --release --test top_crates -- top_crates_cached --nocapture
    sleep 900
done


