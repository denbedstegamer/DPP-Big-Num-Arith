-- evt. undersøg Schönhage–Strassen algorithmen

def combine2 = ???

def computeIter64 (ai : u64) (bj : u64) (l : u64, h : u64, c : u32)
                  : (u64, u64, u32) =
    let ck_l = ai* bj
    let n_l = l + ck_l
    let c_l = (u32.u64 (n_l >> 32)) < (u32.u64 (ck_l >> 32))
              |> u64.bool
    let n_h = h + c_l
    let ck_h = u64.mulhigh ai bj
    let n_h = n_h + ck_h
    let c_h = ((n_h >> 32)) < (u32.u64 (ck_h >> 32))
              |> u32.bool
    let n_c = c + c_h
    in (n_l, n_h, n_c)

def convulution4 (n : i32) (ash : []u64) (bsh : []u64) (tid : i32)
                 : ( (u64, u64, u64, u64), (u64, u64, u64, u64) ) =
    let (instance, vtid) = (tid / n, tid % n)
    let off = instance * (4*n)

    let k1 = 2*vtid
    let (lhc0, lhc1) =
        loop (lhc0, lhc1) = ((0u64, 0u64, 0u64), (0u64,0u64,0u32))
        for kk < k1 do
            let i = kk
            let j = k1 - i
            let lhc0 = computeIter64 ash[off+i] bsh[off+j] lhc0
            let lhc1 = computeIter64 ash[off+i] bsh[off+j+1] lhc1
            in (lhc0, lhc1)
    let lhc1 = computeIter64 ash[off+k1+1] bsh[off+0] lhc1
    let (l0, l1, h2, c3) = combine2 lhc0 lhc1

    let k2 = 4*n - k1 - 2
    let (lhc2, lhc3) =
        loop (lhc2, lhc3) = ((0u64, 0u64, 0u64), (0u64,0u64,0u32))
        for kk < k2 do
            let i = kk
            let j = k2 - i
            let lhc2 = computeIter64 ash[off+i] bsh[off+j] lhc2
            let lhc3 = computeIter64 ash[off+i] bsh[off+j+1] lhc3
            in (lhc2, lhc3)
    let (l_nm2, l_nm1, h_n, c_np1) = combine2 lhc2 lhc3
    in ( (l0, l1, h2, c3), (l_nm2, l_nm1, h_n, c_np1) )

def bmul0 [m] (ash : [m]u64) (bsh : [m]u64)
          : [m]u64 =
    ???
    
