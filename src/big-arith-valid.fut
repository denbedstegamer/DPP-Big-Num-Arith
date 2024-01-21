import "./big-arith-fut/bigadd"
import "./big-arith-fut/bigmul"

entry big_add_validation [m][n] (as : [m][n]u64) (bs : [m][n]u64) (exp_res : [m][n]u64) : [m]bool =
    let x = n/4
    in map3 (\a b r -> bigadd (a :> [4*x]u64) (b :> [4*x]u64) == (r :> [4*x]u64)) as bs exp_res

entry big_mul_validation [m][n] (as : [m][n]u64) (bs : [m][n]u64) (exp_res : [m][n]u64) : [m]bool =
    let x = n/4
    in map3 (\a b r -> bmul0 (a :> [4*x]u64) (b :> [4*x]u64) == (r :> [4*x]u64)) as bs exp_res


-- ==
-- entry: big_add_bench
-- compiled random input { [2048][128]u64 [2048][128]u64 } auto output
-- compiled random input { [4096][128]u64 [4096][128]u64 } auto output
-- compiled random input { [8192][128]u64 [8192][128]u64 } auto output
-- compiled random input { [16384][128]u64 [16384][128]u64 } auto output
-- compiled random input { [32768][128]u64 [32768][128]u64 } auto output
-- compiled random input { [65536][128]u64 [65536][128]u64 } auto output
entry big_add_bench [m][n] (as : [m][n]u64) (bs : [m][n]u64) :  [m][n]u64 =
    let x = n/4
    in map2 (\a b -> badd0 (a :> [4*x]u64) (b :> [4*x]u64)) as bs :> [m][n]u64

-- ==
-- entry: big_mul_bench
-- compiled random input { [2048][128]u64 [2048][128]u64 } auto output
-- compiled random input { [4096][128]u64 [4096][128]u64 } auto output
-- compiled random input { [8192][128]u64 [8192][128]u64 } auto output
-- compiled random input { [16384][128]u64 [16384][128]u64 } auto output
-- compiled random input { [32768][128]u64 [32768][128]u64 } auto output
-- compiled random input { [65536][128]u64 [65536][128]u64 } auto output
entry big_mul_bench [m][n] (as : [m][n]u64) (bs : [m][n]u64) :  [m][n]u64 =
    let x = n/4
    in map2 (\a b -> bmul0 (a :> [4*x]u64) (b :> [4*x]u64)) as bs :> [m][n]u64

-- deprecated due to batching.
-- entry big_add_debug [m] (as : [m]u64) (bs : [m]u64) (exp_res : [m]u64) : ([]u64, []u64, []i64) =
--     let n = m/4
--     let c = bigadd (as :> [4*n]u64) (bs :> [4*n]u64)
--     in filter (\(r, c, i) -> c != r) (zip3 (exp_res :> [4*n]u64) c (iota (4*n))) |> unzip3

-- deprecated due to batching.
-- entry big_mul_debug [m] (as : [m]u64) (bs : [m]u64) (exp_res : [m]u64) : ([]u64, []u64, []i64) =
--     let n = m/4
--     let c = bmul0 (as :> [4*n]u64) (bs :> [4*n]u64)
--     in filter (\(r, c, i) -> c != r) (zip3 (exp_res :> [4*n]u64) c (iota (4*n))) |> unzip3