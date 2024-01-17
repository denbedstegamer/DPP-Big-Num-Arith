import "./big-arith-fut/bigadd"
import "./big-arith-fut/bigmul"

entry big_add_validation [n] (as : [n]u64) (bs : [n]u64) (exp_res : [n]u64) : bool =
    badd0 as bs == exp_res

entry big_add_debug [n] (as : [n]u64) (bs : [n]u64) (exp_res : [n]u64) : ([]u64, []u64, []i64) =
    let c = badd0 as bs
    in filter (\(r, c, i) -> c != r) (zip3 exp_res c (iota n)) |> unzip3

entry big_mul_validation [n] (as : [n]u64) (bs : [n]u64) (res : []u64) : bool =
    bmul0 as bs == res

-- entry big_mul_debug [n] (as : [n]u64) (bs : [n]u64) (res : []u64) : bool =
--     let c = bmul0 as bs
--     in filter (\(r, c, i) -> c != r) (zip3 exp_res c (iota n)) |> unzip3