#!/bin/bash
# Benchmark runner with timing for each test

export DYLD_LIBRARY_PATH=./gc/.libs

echo "=== uts benchmarks ==="
echo ""

run_bench() {
    local name="$1"
    local expr="$2"
    printf "%-20s " "$name:"

    local tmpfile=$(mktemp)
    local start=$(python3 -c 'import time; print(time.time())')
    echo "$expr" | ./uts uts.fasl > "$tmpfile" 2>&1
    local end=$(python3 -c 'import time; print(time.time())')
    local elapsed=$(python3 -c "print(f'{$end - $start:.3f}')")

    # Get last result (-> followed by non-space)
    local val=$(grep -E "^-> [^ ]" "$tmpfile" | tail -1 | sed 's/^-> //')
    rm -f "$tmpfile"

    printf "%8ss  result=%s\n" "$elapsed" "$val"
}

# Tak - triply recursive, tests function calls + arithmetic
run_bench "tak(24,16,8)" "(define (tak x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)))) (tak 24 16 8)"

# Fib - exponential recursion
run_bench "fib(35)" "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 35)"

# Ack - deep recursion
run_bench "ack(3,7)" "(define (ack m n) (cond ((= m 0) (+ n 1)) ((= n 0) (ack (- m 1) 1)) (else (ack (- m 1) (ack m (- n 1)))))) (ack 3 7)"

# Sum - tight loop, fixnum add (tail recursive)
run_bench "sum(10000000)" "(define (sum-to n) (let loop ((i 0) (acc 0)) (if (> i n) acc (loop (+ i 1) (+ acc i))))) (sum-to 10000000)"

# Sum-fp - floating point arithmetic
run_bench "sum-fp(10000000)" "(define (sum-fp n) (let loop ((i 0) (acc 0.0)) (if (> i n) acc (loop (+ i 1) (+ acc (* i 1.0)))))) (sum-fp 10000000)"

# Large integer multiply - factorial
run_bench "fac(19)x10000" "(define (fac n) (if (<= n 1) 1 (* n (fac (- n 1))))) (do ((i 0 (+ i 1)) (r 0)) ((>= i 10000) r) (set! r (fac 19)))"

echo ""
echo "=== done ==="
