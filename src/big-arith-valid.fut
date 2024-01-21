import "./big-arith-fut/bigadd"
import "./big-arith-fut/bigmul"

entry big_add_validation [m] (as : [m]u64) (bs : [m]u64) (exp_res : [m]u64) : bool =
    let n = m/4
    in bigadd (as :> [4*n]u64) (bs :> [4*n]u64) == (exp_res :> [4*n]u64)

entry big_add_debug [m] (as : [m]u64) (bs : [m]u64) (exp_res : [m]u64) : ([]u64, []u64, []i64) =
    let n = m/4
    let c = bigadd (as :> [4*n]u64) (bs :> [4*n]u64)
    in filter (\(r, c, i) -> c != r) (zip3 (exp_res :> [4*n]u64) c (iota (4*n))) |> unzip3

entry big_mul_validation [m] (as : [m]u64) (bs : [m]u64) (exp_res : [m]u64) : bool =
    let n = m/4
    in bmul0 (as :> [4*n]u64) (bs :> [4*n]u64) == (exp_res :> [4*n]u64)

entry big_mul_debug [m] (as : [m]u64) (bs : [m]u64) (exp_res : [m]u64) : ([]u64, []u64, []i64) =
    let n = m/4
    let c = bmul0 (as :> [4*n]u64) (bs :> [4*n]u64)
    in filter (\(r, c, i) -> c != r) (zip3 (exp_res :> [4*n]u64) c (iota (4*n))) |> unzip3