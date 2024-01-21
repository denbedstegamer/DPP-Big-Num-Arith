import "./bigadd"

-- let combine2 (l0: u64, h0: u64, c0: u32) (l1: u64, h1: u64, c1: u32): (u64, u64, u64, u64) =
--   let combined_low = l0 + (l1 << 32)
--   let combined_high = h0 + h1 + (u64.u32 c0) + ((u64.u32 c1) << 32)
--   in (l0, l1, combined_low, combined_high)

def combine2 (l0: u64, h0: u64, c0: u32) (l1: u64, h1: u64, c1: u32): (u64, u64, u64, u64) =
  let n_l = l1 + h0
  let c_l = n_l < h0 |> u64.bool
  let n_h = (u64.u32 c0) + h1 + c_l
  let c_h = n_h < h1 |> u64.bool
  let n_c = (u64.u32 c1) + c_h
  in (l0, n_l, n_h, n_c)

def computeIter64 (ai : u64) (bj : u64) (l : u64, h : u64, c : u32)
                  : (u64, u64, u32) =
    let ck_l = ai* bj
    let n_l = l + ck_l
    let c_l = (u32.u64 (n_l >> 32)) < (u32.u64 (ck_l >> 32))
              |> u64.bool
    let n_h = h + c_l
    let ck_h = u64.mul_hi ai bj
    let n_h = n_h + ck_h
    let c_h = (u32.u64 (n_h >> 32)) < (u32.u64 (ck_h >> 32))
              |> u32.bool
    let n_c = c + c_h
    in (n_l, n_h, n_c)

def convulution4 (n : i32) (ash : []u64) (bsh : []u64) (tid : i32)
                 : ( (u64, u64, u64, u64), (u64, u64, u64, u64) ) =
    -- let (instance, vtid) = (tid / n, tid % n)
    -- let off = instance * (4*n)
    -- instance and offset are, in our case, always 0 (we always use a single "block"). Could be omitted.
    -- ... And now, is omitted.

    let k1 = 2*tid
    let (lhc0, lhc1) =
        loop (lhc0, lhc1) = ((0u64, 0u64, 0u32), (0u64, 0u64, 0u32))
        for kk < (k1+1) do
            let i = kk
            let j = k1 - i
            let lhc0 = computeIter64 ash[i] bsh[j] lhc0
            let lhc1 = computeIter64 ash[i] bsh[j+1] lhc1
            in (lhc0, lhc1)
    let lhc1 = computeIter64 ash[k1+1] bsh[0] lhc1
    let (l0, l1, h2, c3) = combine2 lhc0 lhc1

    let k2 = 4*n - k1 - 2
    let (lhc2, lhc3) =
        loop (lhc2, lhc3) = ((0u64,0u64,0u32), (0u64,0u64,0u32))
        for kk < (k2+1) do
            let i = kk
            let j = k2 - i
            let lhc2 = computeIter64 ash[i] bsh[j] lhc2
            let lhc3 = computeIter64 ash[i] bsh[j+1] lhc3
            in (lhc2, lhc3)
    let lhc3 = computeIter64 ash[k2+1] bsh[0] lhc3
    let (l_nm2, l_nm1, h_n, c_np1) = combine2 lhc2 lhc3
    in ( (l0, l1, h2, c3), (l_nm2, l_nm1, h_n, c_np1) )

def intertwine2 [n] (as : [n]u64) (bs : [n]u64) : [2*n]u64 =
  let asc = scatter (replicate (2*n) 0) (map (\i -> 2*i) (iota n)) (map i64.u64 as)
  let bsc = scatter asc (map (\i -> 2*i+1) (iota n)) (map i64.u64 bs)
  in map u64.i64 bsc

def bmul0 [n] (as : [4*n]u64) (bs : [4*n]u64) : [4*n]u64 =
  let (p1, p2) = map (\tid -> convulution4 (i32.i64 n) as bs (i32.i64 tid)) (iota n)
                 |> unzip
  let ( (l0, l1, h2, c3), (l_nm2, l_nm1, h_n, c_np1) ) = (unzip4 p1, unzip4 p2)
  let v = intertwine2 l0 l1 ++ intertwine2 (reverse l_nm2) (reverse l_nm1) :> [4*n]u64
  let c = intertwine2 h2 c3 ++ intertwine2 (reverse h_n) (reverse c_np1)
          |> rotate (-2)
          |> drop 2
          |> concat [0, 0]
          :> [4*n]u64
  in badd0 v c -- change to bigadd for shared memory

  -- Do a map over convolutions (as to use the threads)
  -- For debugging, use a smaller test size, print the failing as and bs and run by hand
  -- ghci